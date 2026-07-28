/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.preparation

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.{isWorkMode, targetArch}
import com.huawei.excelsior.jet.compiler.RTSProc
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.ir.LineNumber
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.{PhiWebsTranslation, SimplifyComponent}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{DebugHaltWithPositions, SplitHugeBlocks}
import com.huawei.excelsior.jet.compiler.options.NumOption.SplitBlockPartitionSize
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.WhileChanged.whileChanged
import xscala.util.MathUtils

import scala.collection.mutable

/** Preparation steps made on normal IR (with enabled type checks, identity and value numbering).
  * In most cases may be re-ordered or re-entered without any problem.
  *
  * @author conwor
  */
trait SimpleSteps extends SimplifyComponent with PhiWebsTranslation { self: Universe with BackEnd =>

  private[preparation] def updateSOEThrowsFlags(): Unit = {
    for (t <- all[Throw]) {
      val args = Closure(t.valueArgs) {
        case x: Phi => x.valueArgs
        case x: ConvertDomain => Iterator.single(x.obj)
        case _ => Iterator.empty
      } filterInPlace (n => !n.isInstanceOf[Phi] && !n.isInstanceOf[ConvertDomain])

      // When an SOE occurs, the runtime throws a "bare" SOE (without a stack trace) and sets
      // the bare SOE instantiation flag. When it is set, each exception handler tries to
      // instantiate it into a "full" SOE with a proper stack trace. However, if the user code
      // interacts with the "bare" SOE in a way other than rethrowing it, we need to unset
      // this flag and stop trying to instantiate the exception. This is done to avoid
      // the user observing different exceptions coming from a single stack overflow.
      t.shouldPreventBareSOEInstantiation = args.exists(!_.isInstanceOf[Catch])
    }
  }

  private[preparation] def replaceCatches(): Unit = {
    for (c <- all[Catch]) {
      c replaceBy insertCodeAfter(c.block) {
        if (targetArch == CBC) {
          CatchCBC()
        } else {
          RTSCall(RTSProc.JR_ObtainPendingException)()
        }
      }
    }
  }

  private[preparation] def replaceConvertDomainByCalls(): Unit = {
    for (c <- all[ConvertDomain]) {
      if (targetArch == CBC) {
        strikeOutWithValueUses(c, c.obj) // In CBC all conversions are done inside catch instruction implementations (both interp/jit)
      } else {
        replaceByCode(c) {
          RTSCall(RTSProc.ExceptionHandling_convertIntoDomain)(c.obj, IConst(c.domain.ordinal))
        }
      }
    }
  }

  private[preparation] def translatePhiWebs(): Unit = {
    if (eliminateConditionPhies() | eliminateIntraReferencePhies()) {
      whileChanged { changed =>
        bulkReplace {
          for (phi <- all[Phi]) {
            if (eliminateCyclicPhies(phi)) {
              changed()
            }
          }
        }
      }
    }
  }

  private[preparation] def removeValueRangeFilters(): Unit =
    all[RawValueRangeFilter] foreach strikeOut

  private[preparation] def resolveImportedIndices(): Unit =
    all[ImportedIndex] foreach { idx => idx.replaceBy(IConst(env.getImportedClassIdx(idx.targetType, rootMethod)))}

  private[preparation] def removeMarkers(): Unit = {
    for (marker <- all[Marker].toList) {
      marker match {
        case _: ColdCodeMarker | _: WarmCodeMarker =>
          val block = marker.block
          if (marker.inCtrl != block) {
            // TODO: find out why this transformation is required.
            insertCodeAfter(block) { Node.clone(marker) }
            strikeOut(marker)
          }

        case _: ICRegionOp | _: NoLoopUnrollingMarker | _: CountedLoopMarker =>
          strikeOut(marker)

        case _: InterpreterCaseMarker =>
          if (isO1Compiled) strikeOut(marker) // Otherwise BGCM will use it

        case _: WriteBarrierMarker | _: NoTDBarrierMarker =>
          // Markers for BGCM, nothing to do.

        case _ =>
          shouldNotReachHere()
      }
    }
  }

  /** Ensures that there are no trusted checks in IR (all of them should be lowered or eliminated in lowering). */
  protected[preparation] def ensureNoTrustedChecks(): Unit = {
    assert(all[PureCheck] forall (!_.trusted))
  }

  /** Insert special node for zeroing stack alloc regions if needed. */
  protected[preparation] def insertStackAllocZeroing(): Unit = {
    if (all[StackAlloc] exists { _.zeroed }) {
      insertCodeAfter(entryBlock) { StackZeroing.Massive() }
    }
  }

  protected[preparation] def convertBFXToAnd(): Unit = {
    for (bfx <- all[BitFieldExtract].toList if bfx.isGroupRoot) {
      bfx match {
        case BitFieldExtract(0, size, false, arg) if bfx.tpe == arg.tpe && addrOrIntType(bfx.tpe) =>
          replaceTransitively(bfx, And(arg, IntegralConst(bfx.tpe)(MathUtils.rightNBits64(size))))
        case _ =>
      }
    }
  }

  private[preparation] def splitHugeBlocks(): Unit = {
    if (env.enabled(SplitHugeBlocks)) {
      val blockPartitionSize = env.valueOf(SplitBlockPartitionSize)

      for (b <- all[BBlock] if b.spine.drop(blockPartitionSize * 2).nonEmpty) {
        for (Seq(partHead) <- b.spineBackward.sliding(1, blockPartitionSize).toList) {
          Block.splitAfter(partHead)
        }
      }
    }
  }

  private[preparation] def insertFatalErrorBeforeHalt(): Unit = {
    if (isWorkMode ||
      // This can happen if the whole method becomes unreachable (i.e. is never actually called).
      // Note: we must not generate an empty code region,
      //       so we insert fatal error here even in enduser mode (see JET-14729).
      // TODO: remove such methods from generated obj files
      entryBlock.blockEnd.isInstanceOf[Halt]) {

      // Insert immediate fatal error before UB.
      for (halt <- all[Halt]) {
        halt.reason match {
          // Don't insert fatal RTSCall after erroneous RTSCall
          // Otherwise there will be two sequential RTSCalls with different messages
          case _: Halt.Reason.AfterRTSCall | _: Halt.Reason.AfterThrow =>
          case _ =>
            insertCode(halt.inCtrl, halt, useDefaultHandler = false, halt) {
              val pos = if env.enabled(DebugHaltWithPositions) then s"Position:\n${halt.pos.toString}\n" else ""
              RTSCall(RTSProc.JR_FatalError)(AJString.bstr(s"Control flow reached unreachable halt.\n${halt.reason}\n" ++ pos))
            }
        }
      }
    }
  }

  private[preparation] def groupCallTargets(): Unit = {
    for (ct <- all[CallTarget].toList) {
      // All java invoke targets should be replaced in lowering,
      // but CBC preserves original form of virtual/interface calls
      assert(targetArch == CBC || !ct.isInstanceOf[AnyInvokeTarget], ct)

      assert(ct.valueUses.forall(_.isInstanceOf[Call]))
      Node.rematerializeCompletely(ct).foreach { target =>
        target.attachToGroup(target.singleUse, Group.AttachReason.CALL_TARGET_ARG)
      }
    }
  }

  private[preparation] def prepareTailParameters(): Unit = {
    if (rootABI.hasTail) {
      for (param <- all[Param].toList) {
        rootABI.paramLocations(param.num) match {
          case _: TailSlot =>
            val load = LoadTailParam(param.tpe)(TailPointer(), param)
            param.replaceUses { case e if e.isValue && e.target != load => load }
          case _ =>
        }
      }
    }

    if (rootABI.isJETVarArgs) {
      assert(rootABI.hasTail)
      for (va <- all[VarArguments]) {
        va.replaceBy(Lea.Base(TailPointer(), frame.abi.jetVarArgsOffset))
      }
    }
  }

  private[preparation] def replaceUnusedValueArgs(): Unit = {
    // Set unused value edge to null that could be used on immediate to eliminate unnecessary allocation
    for (n <- allNodes) {
      n match {
        case aic: ArrayIndexCheck =>
          aic.updateArg(ArrayIndexCheck.ArrayEdge.index, Null())
        case c: InvokeVirtualStaticTarget =>
          c.updateArg(InvokeVirtualStaticTarget.ThisTypeInfo.index, LConst(0))
        case ait: AnyInvokeTarget if ait.targetRef.hasNonRecordReceiverParameter =>
          ait.updateArg(AnyInvokeTarget.ReceiverEdge.index, Null())
        case _ =>
      }
    }
  }

  private [preparation] def insertCoverageCounter(): Unit = {
    for (block <- all[Block]) {
      def emptyLocs = mutable.HashMap.empty[String, mutable.HashSet[Int]]
      var locs = emptyLocs
      def updateLocs(src: String, line: Int) = locs.getOrElseUpdate(src, mutable.HashSet.empty) += line

      for (node <- block.spineBackward) {
        node match {
          // For blocks that don't have corresponding source lines (e.g. XBlock, synthesized code), skip it
          case n: SpinalNode if n.canThrow && locs.nonEmpty =>
            insertCodeAfter(n) { CoverageCounter(locs)() }
            locs = emptyLocs

          case _: DebugPrologueEndBreakpoint =>
            updateLocs(rootMethod.getSourceFile.toString, rootMethod.getSourceLine)

          case DebugBreakpointWithKnownInfo(context, line, _, _) if LineNumber.isKnown(line) =>
            updateLocs(context.method.getSourceFile.toString, line)

          case _ =>
        }
      }

      if (locs.nonEmpty) {
        insertCodeAfter(block) { CoverageCounter(locs)() }
      }
    }
  }
}
