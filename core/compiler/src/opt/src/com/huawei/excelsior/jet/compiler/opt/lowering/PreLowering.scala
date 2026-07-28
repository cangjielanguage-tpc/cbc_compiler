/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.common.Language.CANGJIE
import com.huawei.excelsior.jet.compiler.Env.{isStandalone, targetArch}
import com.huawei.excelsior.jet.compiler.bytecode.NoPosition
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.opt.backend.preparation.FieldChains
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.MarkedRegions.Hotness
import com.huawei.excelsior.jet.compiler.opt.middle.types.CompileTimeComputations
import com.huawei.excelsior.jet.compiler.opt.middle.{DCEComponent, LiveRangesOptimization}
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption
import com.huawei.excelsior.jet.compiler.options.NumOption.InlineNewTinySize
import com.huawei.excelsior.jet.compiler.symlevel.{Field, MethodReference, MethodReferenceAccessKind}
import com.huawei.excelsior.jet.compiler.{Env, RTConst, RTSProc, StatsKind}
import com.huawei.excelsior.jet.util.ScalaCollections
import xscala.matching.Regex
import xscala.util.StringOps.r

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.tailrec

/** Optimize lowering of some nodes by saving hints for lowering
  * (hints are usually saved at node instance fields and are not serialized).
  *
  * These optimizations do not affect any other analyses or optimizations so it is done once just before lowering.
  */
trait PreLowering extends CompileTimeComputations with LiveRangesOptimization with DCEComponent with Toolbox with FieldChains { self: Universe =>

  import PreLowering.Regex.*

  private val newArrayInlinedHitsPercentThreshold = env.valueOf(NumOption.NewArrayInlinedHitsPrecentThreshold) / 100.0

  private val newArrayCopyInlinedHitsPercentThreshold = env.valueOf(NumOption.NewArrayCopyInlinedHitsPrecentThreshold) / 100.0

  def optimizeNopMemoryBarriers(): Unit = {}

  def optimizeEnriches(): Unit = {}

  def analyzeForLowering(): Unit = {

    lazy val ensureNoCE = splitCriticalEdges() // prevent moving heavy ops (e.g. weakcasts) too high in CFG by splitting critical edges before GCM
    lazy val loops = { ensureNoCE; cfg.loops }
    lazy val coldBlocks = { ensureNoCE; findWarmAndColdBlocks() }

    def markInlinedNewOps(): Unit = {
      if (isO1Compiled || env.enabled(InlineNoNew)) {
        return
      }

      val inlineAll = env.enabled(InlineAllNew)

      // This size is a rough limit when it is ok to inline by some static heuristics.
      val tinySize = env.valueOf(InlineNewTinySize)
      assert(0 <= tinySize && tinySize <= RTConst.Allocator.MAX_SIZE_OF_SPECIALIZED_OBJECT.intValue)

      for (newOp <- all[InlineableAllocator]) {
        assert(!newOp.shouldBeInlined)

        val allocType = newOp.allocType

        def shouldBeInlinedWithGuard(newOp: InlineableAllocatorWithGuard, length: Option[Long], pattern: Regex, threshold: Double, useMaxSmallSizeGuard: Boolean): (Boolean, String) = {
          if (profile.isPGOHost && profile.getHotness(newOp) == Hotness.Hot) {
            def maxSmallObjSize = RTConst.SmallAllocConfig.MAX_SMALL_OBJ_SIZE.intValue

            if (allocType.isAJArray) {
              // TODO: support inlining of AJ array allocators
              return (false, "AJ array")
            }

            if (allocType.isCangjieArray) {
              // TODO: support inlining of Cangjie array allocators
              return (false, "Cangjie array")
            }

            if (allocType.isXScalaArray) {
              // TODO: support inlining of Scala array allocators
              return (false, "Scala array")
            }

            length match {
              case Some(len) =>

                def isSmallAllocatable(len: Long): Boolean = {
                  assert(allocType.isArray)
                  0 <= len &&
                    ((len * allocType.getArrayElemType.symType.size) + RTConst.JavaArray.ARRAY_BODY_OFFS.intValue) <= maxSmallObjSize
                }

                if (len < 0) {
                  (false, "negative length")
                } else if (isSmallAllocatable(len)) {
                  newOp.shouldBeInlined = true
                  (true, "small")
                } else {
                  (false, "non-small")
                }

              case _ =>
                import InlineableAllocatorWithGuard.*
                val allTargets = profile.calledMethods(newOp.pos).toSeq
                val totalHits = ScalaCollections.sumBy(allTargets)(_._2)
                val dominatingTargetSizes = allTargets flatMap {
                  case (t, h) if h >= totalHits * threshold =>
                    condOpt(t.getName) { case pattern(size) => size.toInt }
                  case _ => None
                }
                if (dominatingTargetSizes.nonEmpty) {
                  val inlinedSize = dominatingTargetSizes.head
                  newOp.shouldBeInlined = true
                  newOp.sizeGuard = PointGuard(inlinedSize)
                  (true, s"dominating target with size = $inlinedSize")

                } else if (useMaxSmallSizeGuard) {
                  newOp.shouldBeInlined = true
                  newOp.sizeGuard = LevelGuard(maxSmallObjSize)
                  (true, "no dominating target, yet inlining under MAX_SMALL_OBJ_SIZE guard")

                } else {
                  (false, "no dominating target")
                }
            }
          } else {
            (false, "no motivation")
          }
        }

        val (success, statsMsg) = newOp match {
          case newOp: New =>
            if (allocType.symType.finalizable) {
              (false, "has finalize")
            } else if (allocType.symType.getHeapObjectSize > RTConst.Allocator.MAX_SIZE_OF_SPECIALIZED_OBJECT.intValue) {
              (false, "too big")

            } else {
              def tinyInLoop =
                (allocType.symType.getHeapObjectSize <= tinySize) &&                     // tiny object
                  !(typeProvider.getThrowableType isAssignableFrom allocType.symType) && // of non-exception type,
                  inLoop(newOp)                                                          // in loop

              def isHotByProfile = {
                profile.isPGOHost && {
                  // Given calledProc is marked as never inlined but all of them are included in profile forcibly.
                  // See InlinePlanner.isAlwaysInlinedRTProc().
                  val proc = RT.Allocator.newObject(newOp.allocType.symType.getHeapObjectSize)
                  profile.inlinePlanContains(newOp.pos, proc)
                }
              }

              val reasonOpt =
                (if (tinyInLoop) Some("tiny in loop") else None) orElse
                (if (isHotByProfile) Some("PGI") else None) orElse
                (if (inlineAll) Some(s"+${InlineAllNew.name}") else None)

              reasonOpt match {
                case Some(reason) =>
                  newOp.shouldBeInlined = true
                  (true, reason)

                case None =>
                  (false, "no motivation")
              }
            }

          case newOp: NewArray =>
            val lenOpt = newOp.lengths.head match {
              case IntegralConst(len) => Some(len)
              case _ => None
            }
            shouldBeInlinedWithGuard(newOp, lenOpt, NewArraySpecialized, newArrayInlinedHitsPercentThreshold, useMaxSmallSizeGuard = false)

          case newOp: NewArrayCopy =>
            val lenOpt = newOp.length match {
              case IntegralConst(len) => Some(len)
              case _ => None
            }
            shouldBeInlinedWithGuard(newOp, lenOpt, NewArrayCopySpecialized, newArrayCopyInlinedHitsPercentThreshold, useMaxSmallSizeGuard = true)

          case newOp: NewArrayCopyRT =>
            val lenOpt = (newOp.from, newOp.to) match {
              case (IntegralConst(from), IntegralConst(to)) => Some(to - from)
              case _ => None
            }
            shouldBeInlinedWithGuard(newOp, lenOpt, NewArrayCopySpecialized, newArrayCopyInlinedHitsPercentThreshold, useMaxSmallSizeGuard = true)

          case _ => shouldNotReachHere(newOp)
        }

        inliningStats(StatsKind.NewOptimization, newOp, success, statsMsg)
      }
    }

    def markColdStrConcats(): Unit = {
      for (sc <- all[StrConcat] if coldBlocks(sc.block)) {
        assert(!sc.cold)
        sc.cold = true
      }
    }

    /** Some allocators have built-in initialization (i.e. JR_NEW, JR_NEW_STRING).
      * Prevent double initialization check by absorbing explicit Clinit node.
      */
    def eliminateClinitsAbsorbedByNewOps(): Unit = {
      if (isO1Compiled || !env.enabled(ClinitNewAbsorption)) return

      def hasBuiltInClinit(n: AnyNew) = n match {
        case _: AnyNewArray | _: NewArrayCopy => false // new array does not perform initialization
        case n: New => !n.shouldBeInlined
        case _: NewStackAllocated => false
        case _: NewString => true
        case _ => shouldNotReachHere(s"unexpected class allocation node $n")
      }

      for (
        // We match only pair of [Clinit, New] without any control nodes between them.
        // We might allow some nodes between them (e.g. ColdCodeMarker)
        // but there is no motivation for such complication.
        newOp @ HasInControl(clinit: Clinit) <- all[AnyNew]
        if
          // We might absorb clinit of allocType's superclasses but it's not the goal of this optimization.
          clinit.klass == newOp.allocType.symType &&
          hasBuiltInClinit(newOp) &&
          safeToMergeXPointsOf(clinit, newOp)
      ) {
        // At the moment, the code in allocators requires InstanceDescriptor to be already initialized
        // (more specifically -- its objTypeInfo), so we must guarantee that.
        // TODO: only RTTI/InstanceDesc creation is required here not full preparation!
        if (ProjectLogic.useLazyPreparation) {
          insertCodeBefore(clinit) {
            ensurePrepared(clinit.klass)
          }
        } else {
          // Note: There will be no optimization cycle until lowering,
          //       so we must manually perform EagerPreparationChecksElimination here
          env.markForPreparation(clinit.klass)
        }

        stats.count(StatsKind.NewOptimization, "absorbed explicit clinit", clinit)
        clinit.replaceUses {
          case TaggedEdge(Tag.CONTROL | Tag.MEMORY, _, use) if use != newOp => newOp
        }
        strikeOut(clinit)
      }

      // Note that this optimization could be generalized using ContextTypes.
      // E.g. consider pair [Clinit(Foo), InvokeStatic(Foo.bar)].
      // TODO: integrate this transformation with ContextTypes
    }

    def markInlinedInterfaceOperations(): Unit = {
      if (isO1Compiled && env.enabled(InlineNoIfaceOps)) {
        return
      }

      requireGlobalCodeMotion() // for usage of InstanceOf.block in inLoop()

      val inlineAll = env.enabled(InlineAllIfaceOps)

      for (op <- all[AbstractTypeCheck] if op.targetType.isInterface) {
        assert(!op.shouldBeInlined)

        def isHotByProfile = {
          // We don't check exact method call in profile because some RT methods (i.e. instof) are unmanaged
          // and aren't included in profile.
          profile.isPGOHost
        }

        val reasonOpt =
          (if (inLoop(op)) Some("in loop") else None) orElse
          (if (isHotByProfile) Some("PGI") else None) orElse
          (if (inlineAll) Some(s"+${InlineAllIfaceOps.name}") else None)

        val (success, statsMsg) = reasonOpt match {
          case Some(reason) =>
            op.shouldBeInlined = true
            (true, reason)

          case None =>
            (false, "no motivation")
        }

        inliningStats(StatsKind.IFaceOps, op, success, statsMsg)
      }
    }

    def inLoop(op: Node) = {
      assert(op.block != null)
      loops.isInLoop(op.block) &&
        !(coldBlocks contains op.block)
    }

    def inliningStats(stat: StatsKind, op: Node, success: Boolean, statsMsg: String): Unit = {
      if (stats.isEnabled(stat)) {
        val status = if (success) "successful" else "failed"
        stats.count(stat, s"$status inline ($statsMsg)", op)
      }
    }

    /** Mark deprive-weakCast pairs which can be lowered together to produce better quality code (see JET-10329).
      *
      * Deprive followed by weak cast of the same target type can result in two enrichment checks instead of one.
      * To avoid it we lower deprive and weak cast into a single diamond with enrichment check,
      * which can be done only if they are in the same block (then weak cast can be safely pulled up to deprive for lowering).
      *
      * Checking for this pattern requires GCM, so it can't be performed during lowering phase.
      */
    def markDepriveWeakCastPairs(): Unit = {
      requireGlobalCodeMotion()
      for (wc <- all[WeakCast] if !wc.hasDominatingCheck) {
        wc.obj match {
          case d: Deprive if d.interfaceType == wc.targetType && wc.block == d.block =>
            d.isLoweredWithWeakCast = true
          case _ =>
        }
      }
    }

    /** Pins all ClassObjects to a point with valid xHandler information.
      *
      * Checking for this pattern requires GCM, so it can't be performed during lowering phase.
      */
    def pinClassObjects(): Unit = {
      requireGlobalCodeMotion()

      if (all[ClassObject].isEmpty) {
        return
      }

      val noXHandlers = all[XBlock].isEmpty
      if (!noXHandlers) {
        // make entryBlock handler-free
        Block.splitAfter(entryBlock, keepControlled = true)
      }

      for (classObject <- all[ClassObject]) {
        val pinPoint = if (noXHandlers) {
          // if there are no xHandlers, pin class object where it was before
          classObject.upperPoint
        } else {
          // otherwise pin it to the entryBlock to be safe
          // TODO: implement proper analysis if you a brave enough
          entryBlock
        }

        val xclassObject = insertCodeAfter(pinPoint, useDefaultHandler = true) {
          withPos(classObject.pos ensuring (_ != NoPosition)) {
            XClassObject(classObject.symType)()
          }
        }

        classObject replaceBy xclassObject

        collect[FloatingNode](pinPoint.pinnedNodes) foreach (_ atUpperPoint xclassObject)
      }
    }

    /**
      * Arbitrary low limit number of cases in the interpreter switch.
      * Used to filter out other switches in the interpreter.
      */
    val INTERPRETER_SWITCH_CASE_LOW_LIMIT_NUMBER = 50

    /**
      * For interpreter main methods we mark switch cases with InterpreterCaseMarker for BGCM special working mode.
      * We are doing this before lowering, because in the lowering we may replace TableJump by tree of branches.
      */
    def markInterpreterCases(): Unit = {
      if (rootMethod.isInterpretationLoop && !Env.isWorkMode) {
        for (switch <- all[Switch].filter(_.caseExits.size >= INTERPRETER_SWITCH_CASE_LOW_LIMIT_NUMBER)) {
          for (switchCase <- switch.succBlocks) {
            insertCodeAfter(switchCase) { InterpreterCaseMarker() }
          }
        }
      }
    }

    def markHotBoxedValues(): Unit = {
      for (box <- all[BoxedValue]) {
        box.isHot = !coldBlocks(box.block) && profile.isPGOHost && profile.inlinePlanContains(box.pos, box.target)
      }
    }

    markInlinedNewOps()
    eliminateClinitsAbsorbedByNewOps() // must be done after inlining
    markColdStrConcats()

    ensureNoCE
    linkStructuredSynchronization()

    val engine = new GCMEngine(allowRematerialization = true) {
      override def needsRematerialization(n: FloatingNode) = n match {
        case _: InstanceOf => true
        case n: WeakCast if !n.hasDominatingCheck => true
        case _ => false
      }
    }

    withGCM(engine) {
      pinClassObjects()
      markInlinedInterfaceOperations()
      markDepriveWeakCastPairs()
      optimizeEnriches()
    }
    markInterpreterCases()
    markHotBoxedValues()

    // TODO: add ASC analysis here, remove serialization of its flags
  }

  def redirectNotComputedAtCompileTimeIntrinsicsToRT(): Boolean = {
    val computables = all[IsComputableAtCompileTime]
    val nonEmpty = computables.nonEmpty
    computables.foreach(replaceTransitively(_, False()))
    nonEmpty
  }

  def eliminateAJArrayFillZeroing(): Boolean = {
    var changed = false

    for (arrayFill <- all[AJArrayFill]) {

      def eliminate(node: SpinalNode, kind: StatsKind, event: String): Unit = {
        stats.count(kind, event, node)
        changed = true
        strikeOut(node)
      }

      // Eliminate filling with statically known zero value,
      // because array allocator already zeroes elements, see JET-13319.
      val value = arrayFill.value
      value match {
        case ZeroValueNode() =>
          eliminate(arrayFill, StatsKind.ArrayZeroingElimination, "explicit array zeroing eliminated")

        case value @ StackAlloc(FrameSlot.Local(t, cangjieZeroValue)) if t.isRecord =>
          // Ensure that value is not modified and is still zeroed stack alloc memory.
          // Note: Only Cangjie zeroValue can be removed without any checks.
          // TODO: add more precise check if needed
          if (cangjieZeroValue || value.zeroed && value.uses.forall(_.isInstanceOf[AJArrayFill])) {
            arrayFill.inCtrl match {
              case array @ NewArray(arrayType, _) if array == arrayFill.array && arrayType == arrayFill.arrayType =>
                array.uninitialized = t.symType.hasNoRefFields
              case _ =>
            }
            eliminate(arrayFill, StatsKind.ArrayZeroingElimination, "explicit array zeroing eliminated")
          }

        case _ =>
          val arrayType = arrayFill.arrayType

          arrayFill.inCtrl match {
            case array @ NewArray(`arrayType`, lengths) if array == arrayFill.array && !isStandalone =>
              val elemType = arrayType.getArrayElemType
              if (elemType.isPrimitive) {
                val valueAboveArray = withIncrementalGCM {
                  upperPoint(value) strictDominates array
                }

                if (valueAboveArray) {
                  val length = lengths(0)

                  def castFloatingPointToIntegral = ReinterpretCast(value.tpe, if (value.tpe == DoubleType) LongType else IntType)(value)

                  val castedValue = if (value.tpe.isIntegralType) value else castFloatingPointToIntegral
                  val bfxValue = BitFieldExtract.BFX(LongType, 0, elemType.toAsm.sizeInBits, signExtension = false, castedValue)

                  replaceByCode(array) {
                    if (targetArch == CBC) {
                      NewArrayFill(arrayType)(length, bfxValue)
                    } else {
                      val log2Size = elemType.symType.log2Size
                      val arrayDesc = RawInstanceDescriptor(arrayType.symType)
                      val patternToMul = log2Size match {
                        case 0 => 0x0101010101010101L
                        case 1 => 0x0001000100010001L
                        case 2 => 0x0000000100000001L
                        case 3 => 0x0000000000000001L
                      }
                      val patternValue = Mul(bfxValue, LConst(patternToMul))
                      RTSCall(RTSProc.JR_NEW_CJ_PRIMARRAY)(IConst(log2Size), arrayDesc, length, patternValue)
                    }
                  }
                  eliminate(arrayFill, StatsKind.CangjieArrayFillingElimination, "explicit array filling with primitive values eliminated")
                }
              }
            case _ =>
          }
      }
    }

    if (changed) {
      // Cleanup now useless stack alloc and zero values.
      eliminateDeadCode()
      dbgPrinter.debugNodes("all graph after aj array filling elimination")
    }

    changed
  }

  /** Insert GC point before copying the result in methods that return records.
    * Thus, make sure that reference fields of the record are properly traced
    * before returning the control to the calling function, since this cannot
    * be done using the epilogue GC point.
    *
    * @see [[com.huawei.excelsior.jet.compiler.symlevel.Method.shouldContainGCPointInEpilogue Method#shouldContainGCPointInEpilogue]]
    * @see [[com.huawei.excelsior.jet.compiler.opt.backend.codegen.RecordSlotsLiveness RecordSlotsLiveness]]
    */
  private def insertGCPointsInRecordReturningMethods(): Unit = {
    if (!env.enabled(SmartRecordZeroing) || !rootMethod.shouldContainGCPointBeforeResultTransfer) return

    val recordParamIdx = rootMethod.getRetByValArgIdx
    assert(rootMethod.getParamType(recordParamIdx).isRecord)

    allNodes foreach {
      case n @ CopyStructure(_, Param(`recordParamIdx`), _) => insertCodeBefore(n) { GCPoint() }
      case _ =>
    }
  }

  def transformForLowering(): Unit = {

    def prepareMutFuncCalls(): Unit = {
      def replaceWithNoHost(mutOffs: MutFunc.Offset, isGlobal: Boolean): Unit = {
        if (env.enabled(GenerateWriteBarriers)) {
          mutOffs.host.replaceBy(if (isGlobal) MutFunc.HostGlobal() else MutFunc.HostLocal())
        } else {
          mutOffs.host.replaceBy(Null())
          mutOffs.replaceBy(ReinterpretCast(ValueType(mutOffs.recordType), AddrType)(mutOffs.record))
        }
      }

      for (mutOffs <- all[MutFunc.Offset].toList) {
        collectOneChain(mutOffs.record, Nil) match {
          case Some((combine: MutFunc.Combine, Nil)) =>
            // Pass to mut function our own mut receiver
            // (Host, Offset(Host, Combine(param0, param1)) => (param0, param1)
            mutOffs.host.replaceBy(combine.host)
            mutOffs.replaceBy(combine.offset)

          case Some((obj, chain)) =>
            // Pass to mut function a record obtained from
            // definite reference or our own mut receiver through chained fields access
            if (targetArch == CBC) {
              mutOffs.replaceBy(MutFunc.OffsetCBC(obj, obj.tpe, permanent(chain)))
            } else {
              // For non-cbc platforms the host is replaced by definite reference (or null)
              // and offset from it will be calculated in code generation
              obj match {
                case _: Void =>
                  replaceWithNoHost(mutOffs, isGlobal = true)
                case _: StackAlloc =>
                  replaceWithNoHost(mutOffs, isGlobal = false)
                case p: Param if p.tpe.isRecordAddrType =>
                  replaceWithNoHost(mutOffs, isGlobal = false)
                case c: MutFunc.Combine =>
                  mutOffs.host.replaceBy(c.host)
                case _ =>
                  mutOffs.host.replaceBy(obj)
              }
            }

          case _ =>
            // Pass to mut function a record received as parameter thus it is stack allocated
            // (records are passed by pointer to copy of record in JET CC)
            if (targetArch == CBC) {
              // Keep Offset node to generate (host, offset) pair right before the call taking into account GC mode
            } else {
              replaceWithNoHost(mutOffs, isGlobal = false)
            }
        }
      }

      // All Hosts should be replaced by null or specific local/static host
      assert(targetArch == CBC || all[MutFunc.Host].isEmpty)
    }

    /** Replaces uses of the value passed to barrier with the value returned from it. */
    def propagateWriteBarrierValues(): Unit = withIncrementalGCM {
      if (!env.enabled(PropagateWriteBarrierValue)) return

      def adjustTypesWhenAssign[T](matcher: Node => Boolean)(action: => T): T = {
        def converter(tpe: Type, n: Node) = (n.tpe, tpe) match {
          case (EopType.Plain, EopType.Null) if matcher(n) => ReinterpretCast(EopType.Plain, EopType.Null)(n)
          case _ => n
        }
        Node.withImplicitArgConversion(converter)(action)
      }

      def phiAndProxy(n: Node) = cond(n) { case _: Phi | _: Proxy => true }
      def rawDeprive(n: Node) = cond(n) { case _: RawDeprive => true }

      var changed = false
      for (wb <- all[WriteBarrier]) {
        if (!changed) {
          splitCriticalEdges()
        }

        val Some(assign) = replaceAllValueUsesByVar(wb.value)
        insertCodeAfter(wb) {
          adjustTypesWhenAssign(rawDeprive) {
            AssignVar(assign.variable)(RawDeprive(wb))
          }
        }

        changed = true
      }

      if (changed) {
        adjustTypesWhenAssign(phiAndProxy) {
          completeSSA()
        }
        eliminateUnreachableCode()
        eliminateDeadCode()
      }
    }

    optimizeLiveRanges()
    prepareMutFuncCalls()
    propagateWriteBarrierValues()
    insertGCPointsInRecordReturningMethods()
  }

}

object PreLowering {
  object Regex {
    private[PreLowering] val NewArraySpecialized: Regex = """newArray(\d+)""".r
    private[PreLowering] val NewArrayCopySpecialized: Regex = """newArrayCopy(\d+)""".r
  }
}
