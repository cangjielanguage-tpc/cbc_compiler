/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.codeemitter.BranchOp
import com.huawei.excelsior.jet.compiler.Env.tailRegister
import com.huawei.excelsior.jet.compiler.abi.ABI.AltLocation
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.{Frame, SlotBase}
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.BulldozerGCM
import com.huawei.excelsior.jet.compiler.opt.backend.codegen.{CodeGenerator, Code, CodeMach, DataGenerator}
import com.huawei.excelsior.jet.compiler.opt.backend.fast.{FastCodeOrdering, FastRegAlloc}
import com.huawei.excelsior.jet.compiler.opt.backend.global.GlobalGenerator
import com.huawei.excelsior.jet.compiler.opt.backend.local.LocalGenerator
import com.huawei.excelsior.jet.compiler.opt.backend.post.PostProcessComponent
import com.huawei.excelsior.jet.compiler.opt.backend.preparation.Preparation
import com.huawei.excelsior.jet.compiler.opt.backend.util.BackendGraphs
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Resources, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.LivenessAnalysis
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.NumOption.CodegenLogsLevel
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.Void as V
import com.huawei.excelsior.jet.compiler.symlevel.{MethodSignature, MethodType, SignatureType}
import com.huawei.excelsior.jet.util.ScalaCollections.singleton
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.compiler.{CompilerEnvironment, RTConst, Stage, StatsKind}
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait InitResources extends CompilerEnvironment { self: Universe with BackEnd =>
  if (!Resources.cacheInitialized) { // TODO: initialize cache only once before Universe creation
    val abi = platform.abi(MethodType(MethodSignature()(V)))
    Resources.initializeCache(abi.availableIRegs ++ abi.availableFRegs)
  }
}

/** BackEnd is the final stage in compiler, that generates result object code. */
trait BackEnd extends InitResources with GlobalGenerator with LocalGenerator with BackendGraphs with LivenessAnalysis
  with FrameComponent with CodeGenerator with RegFiles with MachineDescription with PostProcessComponent
  with IRTransformationsCollection with BulldozerGCM with Preparation with FastCodeOrdering with FastRegAlloc { self: Universe =>


  /////////////////////////////////////////////////////////////////////////////////////
  // Arch-specific constructor parts

  /** Returns arch-specific [[Frame]]. */
  protected def makeFrame(): FRAME

  /** Returns arch-specific [[CodeGeneratorImpl]]. */
  protected def makeCodeGeneratorImpl(): CodeGeneratorImpl

  /** Returns arch-specific set of all available [[IReg]]. */
  protected def makeAllIRegsSet() = setOf(frame.availableIRegs)

  /** Returns arch-specific set of all available [[FReg]]. */
  protected def makeAllFRegsSet() = setOf(frame.availableFRegs)


  /////////////////////////////////////////////////////////////////////////////////////
  // Common used components, sets & flags

  /** Compiled method frame. */
  protected val frame = makeFrame()

  /** Returns set of resources, contains only eeIReg. */
  protected val eeIRegSet = if (frame.EER == null) emptySet else setOf(frame.EER)

  /** Immutable set of all integral registers, available to register allocation. */
  protected val allIRegsSet = makeAllIRegsSet()
  protected val allParamIRegsSet = allIRegsSet | eeIRegSet

  /** Returns immutable set of all floating-point registers. */
  protected val allFRegsSet = makeAllFRegsSet()

  /** Returns set of resources, contains only `Tail Register`. */
  protected val tailRegSet = setOf(tailRegister)

  /** Returns set of all alt locations. */
  protected val allAltLocationsSet = setOf(AltLocation(slot = 0), AltLocation(slot = 1))

  /** Returns set of alt locations that aren't used to store the result. */
  protected val nonResultAltLocationsSet = setOf(AltLocation(slot = 1))

  // TODO: refactor this
  protected val tracedStackAllocSlots = ArrayBuffer.empty[FrameSlot]

  protected var bGCMHints: BGCMHints = _
  protected var liveness: CFGLiveness = _


  /////////////////////////////////////////////////////////////////////////////////////
  // Common used utilities

  protected def indexInValueArgs(e: Edge) = {
    val targetProto = e.target.proto
    e.targetArgIndex - (if (targetProto.hasControlArg) 1 else 0) - (if (targetProto.hasMemoryArg) 1 else 0)
  }

  /** Returns the required method alignment for optimization reasons (e.g. see JET-9062). */
  protected def requiredMethodAlignment: Int = RTConst.MethodInfoFrameDescriptor.CODE_ALIGNMENT.intValue

  /** Creates constraints for all blocks. Constraints are nodes, that represents global output of block. */
  protected def createConstraints(beforeRegAlloc: Boolean): Unit = {
    liveness = calcCFGLiveness()

    /** Let phi-function F(X, Y) be in block A that have incoming blocks B and C. Let X be live out of block B.
      * Value associated with X should be alive in the end of block B on two different resources. This function
      * creates for such cases special copy instruction from node X, that replace arguments in phi-functions.
      */
    def insertPhiArgCopies(edge: Edge): Unit = {
      val Edge(src, dst: BBlock) = edge
      var phiArgs: Set[Node] = Sets[Node].newImmSet
      for (phi <- dst.phies) {
        val dataEdge = phi.phiInput(edge)
        val x = dataEdge.source

        if (nodeOnReadOnlyResource(x) || phiArgs.contains(x) || liveness.in(dst).contains(x)) {
          val blockEnd = src.block.blockEnd
          def insertCopy(copy: Copy): Copy = { CodeOrder.insertBefore(blockEnd, copy); copy atLowerPoint blockEnd }

          dataEdge.source = temporaryResourcesForIntermediateCopy(x) match {
            case None => insertCopy(Copy.withOwnValue(x))
            case Some(temporals) => insertCopy(Copy.withOwnValue(insertCopy(Copy.withoutValue(x, temporals))))
          }

        } else {
          phiArgs += x
        }
      }
    }

    for (block <- all[Block]) {
      ScalaCollections.singleton(block.succBlockEdges) match {
        case Some(edge) if edge.target.block.phies.nonEmpty =>
          // After registers allocation there may be phi-functions formally needed to be processed by phi-arg copies
          // (alive in the same point with one of its arguments), but only for the same value. There are no problems if
          // we will use this phi and its argument from the same resource.
          if (isO1Compiled && beforeRegAlloc) {
            insertPhiArgCopies(edge)
          }
          block.blockEnd.addConstraints() ++= liveness.edgeIn(edge)
        case _ if block.succBlocks.nonEmpty => // critical edges are split => only single-exit blocks may lead to phies
          block.blockEnd.addConstraints() ++= liveness.out(block)
        case _ =>
      }

      if (block.isInstanceOf[XBlock]) {
        for (input @ Edge(xpoint: XPoint, _) <- block.inEdges) {
          xpoint.owner.addConstraints() ++= liveness.edgeIn(input)
        }
      }
    }
  }

  /** 0 (default) - only basic logs
    * 1 - block generation logs
    * 2 - node generation logs
    * 3 - node selection logs
    */
  private val codegenLogsLevel = env.valueOf(CodegenLogsLevel)

  /** [[BackEnd]]-specific debug print with given `message`. */
  protected def beDebugPrint(level: Int)(message: String, extraBlockInfo: Block => String = _ => ""): Unit = {
    if (codegenLogsLevel >= level) {
      dbgPrinter.debugNodes(message, {
        case b: Block => extraBlockInfo(b)

        case st: Copy =>
          if (st.allowedResults.isUniverse) {
            "  allowed: any"
          } else {
            st.allowedResults.asSeq.mkString("  allowed: [", ",", "]")
          }

        case _ => ""
      })
    }
  }

  protected def step[T](name: String, action: => T): T = {
    val result = action
    dbgPrinter.debugNodes(name)
    checkIRConsistency(CheckLevels.Optional)
    result
  }

  protected def rtOffset(slot: Frame.Slot): Int = {
    assert(slot.base == SlotBase.SP)
    slot.offset
  }

  protected def dragNodesToSingleUse(fromFastCodeOrdering: Boolean): Unit = {
    for (node <- allNodes) {
      val toDrag = node match {
        case fp: FlagProducer => singleton(fp.valueOutEdges) match {
          case Some(Branch.SelectorEdge(_: If)) =>
            // TODO-REDESIGN-GROUPS
            fromFastCodeOrdering // Because FastCodeOrdering doesn't order grouped nodes in one block, so we have to pull them here
          case _ => false
        }
        case x: FloatingNode if x.isFragilePointer && fromFastCodeOrdering => true
        case _ => false
      }

      if (toDrag) {
        val singleUse = node.singleValueUse.groupRoot
        val block = singleUse.block
        if (CodeOrder contains node) {
          CodeOrder remove node
          CodeOrder.insertBefore(singleUse, node)
        }
        if (node.block != block) {
          val point = lowerPoint(singleUse)
          node.allGroupNodes foreach {
            case x: FloatingNode => x.atLowerPoint(point)
            case _ =>
          }
        }
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Backend script

  def backEnd(): Unit = {
    requireNoGlobalCodeMotion()

    frameSlotID = 0

    step("critical edges eliminated",             splitCriticalEdges(withXHandlers = true))
    step("exceptional critical edges eliminated", splitExceptionalCriticalEdges())

    // Conwor swore that no memory nodes would be inserted after that point.
    prohibitMemoryNodeInsertion()

    if (isO1Compiled) {
      stage(Stage.O1CodeOrdering) { step("code ordered by FastCodeOrdering", fastCodeOrdering()) }
    } else {
      bGCMHints = stage(Stage.O2CodeOrdering) { step("code ordered by BulldozerGCM", doBulldozerGCM()) }
    }
    checkIRConsistency(CheckLevels.Desirable)

    step("constraints created", createConstraints(beforeRegAlloc = true))

    onCommit.withCallback (updateValuesOnCommit) {
      onDecommit.withCallback (CodeOrder.remove) {
        createInitialValues()

        if (isO1Compiled) {
          stage(Stage.O1RegAlloc) { step("registers allocated by FastRegAlloc", new FastRegAllocImpl().interpret()) }
        } else {
          stage(Stage.O2RegAlloc) { step("registers allocated by GlobalGenerator", new GlobalGeneratorImpl().iterate()) }
        }

        cleanupCachesAfterRegAlloc()

        if (isO1Compiled) {
          stage(Stage.O1PostProcess) { postProcessO1() }
        } else {
          stage(Stage.O2PostProcess) { postProcessO2() }
        }
      }
    }

    val codeGenerator = makeCodeGeneratorImpl()
    val code = codeGenerator.genCode()

    if (isDirtyForClassGC) {
      dirtyFramesLogAndStatUpdate("frame marked as untrusted by class GC")
      code.xinfo.markAsDirtyForClassGC()
    }

    sendCode(code)
  }

  protected def sendCode(code: Code): Unit

  def branchOp(condition: Condition, tpe: Type): BranchOp = {
    if (tpe.isIntegralType) {
      (condition: @unchecked) match {
        case Condition.EQ => BranchOp.EQ
        case Condition.NE => BranchOp.NE
        case Condition.LT => BranchOp.LT
        case Condition.LE => BranchOp.LE
        case Condition.GT => BranchOp.GT
        case Condition.GE => BranchOp.GE
        case Condition.ULT => BranchOp.ULT
        case Condition.ULE => BranchOp.ULE
        case Condition.UGT => BranchOp.UGT
        case Condition.UGE => BranchOp.UGE
      }

    } else if (tpe.isTraceableRefType || tpe.isStructureType) {
      // TODO: BranchOp.EQ|FEQ|REQ should be the same BranchOp. For more details look at BranchOp.
      (condition: @unchecked) match {
        case Condition.EQ => BranchOp.REQ
        case Condition.NE => BranchOp.RNE
      }

    } else {
      assert(tpe.isFloatingPointType)
      (condition: @unchecked) match {
        case Condition.EQ => BranchOp.FEQ
        case Condition.NE => BranchOp.FNE
        case Condition.LT => BranchOp.FLT
        case Condition.LE => BranchOp.FLE
        case Condition.GT => BranchOp.FGT
        case Condition.GE => BranchOp.FGE
        case Condition.GE_OR_UNORDERED => BranchOp.FNLT
        case Condition.GT_OR_UNORDERED => BranchOp.FNLE
        case Condition.LE_OR_UNORDERED => BranchOp.FNGT
        case Condition.LT_OR_UNORDERED => BranchOp.FNGE
      }
    }
  }
}

trait BackEndMach { self: Universe with BackEnd with DataGenerator =>
  override protected def sendCode(code: Code): Unit = {
    val CodeMach(seg, xinfo, markedRegions, siberiaOffset) = code
    env.sendMethodCode(codeUnit, seg, xinfo, markedRegions, siberiaOffset, frame, rtOffset)
    sendDataSegments()
  }
}