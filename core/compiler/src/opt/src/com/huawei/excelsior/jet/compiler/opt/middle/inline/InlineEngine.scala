/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline

import com.huawei.excelsior.jet.compiler.RTSProc
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, NoPosition}
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.frontend.bytecode.JBCParser
import com.huawei.excelsior.jet.compiler.opt.frontend.cangjie.CangjieLLVMIRParser
import com.huawei.excelsior.jet.compiler.opt.ir.*
import com.huawei.excelsior.jet.compiler.opt.serialization.SerializerLayerComponent
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.util.Log
import com.huawei.excelsior.jet.util.ScalaCollections.singleton

trait InlineEngine extends SerializerLayerComponent with CallSites { self: Universe =>

  private def log = Log(Log.Kind.Inline)

  def inlineLogSession[T](mode: String)(action: => T): T = log.inSession(s"$codeUnit ($mode)")(action)

  def inlineLog(title: String, details: String): Unit =
    log(s"- $title: $details")

  def inlineDebugLog(msg: String): Unit = {
    if (env.enabled(DetailedInlineLogs)) {
      dbgPrinter.debugNodes(msg)
    }
  }

  def doInline(cs: CallSite, allowFromBytecode: Boolean): Boolean = {
    assert(!cs.target.isNeverInline)

    val targetName = cs.target.getFullName
    inlineDebugLog("Inline - before " + targetName)
    checkIRConsistency(CheckLevels.Optional)

    loadMethodBody(cs, allowFromBytecode) match {
      case Some((scope, rtPartsInfo)) =>
        inlineBody(cs, scope, rtPartsInfo)

        inlineDebugLog("Inline - after " + targetName)
        checkIRConsistency(CheckLevels.Optional)
        true

      case None => false
    }
  }

  private def loadMethodBody(cs: CallSite, allowFromBytecode: Boolean): Option[(Scope, RTPartsInfo)] = {
    val target = cs.target
    val invoke = cs.node
    assert(!target.isVarArgs)

    val ic = createInlineContextFor(cs)

    val (scope, rtPartsInfo): (Scope, Option[RTPartsInfo]) =
      withInlineContext(ic) {
        createScope(invoke, BytecodePosition(currentInlineContext), None) {
          if (passFront(target)) {
            Some(serialization.loadMethod(target, invoke.invokeArgs))
          } else if (allowFromBytecode) {
            Some(loadMethodBodyFromBytecode(cs))
          } else {
            None
          }
        }
      }

    if (rtPartsInfo.isDefined && cs.target.shouldContainGCPoints != cs.node.inlineContext.method.shouldContainGCPoints) {
      makeICRegionAround(scope, ic)
    }

    rtPartsInfo match {
      case None =>
        scope.drop()
        None
      case Some(rpi) =>
        Some(scope, rpi)
    }
  }

  private[inline] def loadMethodBodyFromBytecode(cs: CallSite): RTPartsInfo = shouldNotCallThis()

  private def createInlineContextFor(cs: CallSite) = cs.node.pos match {
    case pos: BytecodePosition =>
      assert(pos.inlineContext.isTopLevel)
      InlineContext.newInlined(cs.target, pos.lineNumber, pos.offset, pos.inlineContext)

    case NoPosition =>
      shouldNotReachHere("Invoke without bytecode position")
  }

  private def inlineBody(cs: CallSite, inlinedScope: Scope, inlinedBodyInfo: RTPartsInfo): Unit = {
    requireNoGlobalCodeMotion()

    val oldNode = cs.node

    if (!isDirtyForClassGC && inlinedBodyInfo.isDirtyForClassGC) {
      val inlineContextIsSpoiled = cs.node.inlineContext.toRoot.exists(_.method.isDirtyForClassGC)
      if (inlineContextIsSpoiled) {
        if (env.enabled(LogDirtyFrameReasons)) {
          env.print("isUntrustedByClassGC in " + rootMethod + " after inline of " + cs.target.getFullName + "\n")
        }
        isDirtyForClassGC = true
      }
    }

    if (isStructuredLocking) {
      if (locallyAnalyzeMethod(cs.target).exists(_.isUnstructuredLocking)) {
        isUnstructuredLocking = true
        // Don't do anything, it's ok to have unstructured state.

      } else if (all[SynchronizedRegion].nonEmpty) {
        // Tie inlined monitor operations to ours if locking remains structured.

        val innerRegions = inlinedScope.allNodes collect {
          case x: SynchronizedRegion if x.isOutermost => x
        }

        if (innerRegions.nonEmpty) {
          SynchronizedRegion.enclosing(oldNode) match {
            case Some(enclRegion) => innerRegions foreach { _.outer = enclRegion }
            case None =>
          }
        }
      }
    }

    if (genDebug) {
      // we better not inline at all but for AJ it can not be avoided
      singleton(inlinedScope.all[DebugPrologueEndBreakpoint]).map(strikeOut(_))
    }

    inlinedScope.merge()
  }

  def transformTailRec(callSites: collection.Seq[CallSite]): Unit = {
    // Original IR:
    //
    //   (entry block with controlled nodes)
    //                   |
    //                   V
    //                 . . .
    //                   |
    //                   V
    //        (block with tailrec call)
    //
    //
    // Transformed IR:
    //
    //       (new totally empty entry block)
    //                   |
    //                   V
    //        /   loop header block   \
    //       (with old controlled nodes) <------\
    //        \ and new phi functions /          \
    //                   |                       |
    //                   V                       |
    //                 . . .                     |
    //                   |                       |
    //                   V                       /
    //       (block until tailrec call) --------/

    dbgPrinter.debugNodes(s"Tailrec transformation - before (${callSites.size} calls)")
    checkIRConsistency(CheckLevels.Optional)

    withJoinAfter(entryBlock, callSites) { cs =>
      val backwardBranchGoto = Block.splitBefore(cs.node)
      backwardBranchGoto.makeUsesUnreachable() // invoke.block is unreachable from this moment
      backwardBranchGoto

    } { join =>
      for (param <- all[Param]) {
        join(param, _.node.invokeArgs(param.num))
      }
    }

    // Invoke nodes still reside in unreachable blocks, UCE will clean them up.
    // However it is quite easy to decommit whole block right now, do it if you need it.

    dbgPrinter.debugNodes("Tailrec transformation - after")
    checkIRConsistency(CheckLevels.Optional)
  }

}

trait InlineFromBytecode extends JBCParser with CangjieLLVMIRParser { self: Universe with InlineEngine =>

  override private[inline] def loadMethodBodyFromBytecode(cs: CallSite): RTPartsInfo = {
    val invoke = cs.node
    if (cs.target.getDeclaringClass.isCangjieType) {
      if (env.enabled(AllowInlineFromBitcode)) {
        loadHLIRMethod(cs.target, invoke.invokeArgs)
      } else {
        shouldNotReachHere("inline from bitcode is disabled")
      }
    } else {
      loadJBCMethod(cs.target, invoke.invokeArgs)
    }
  }
}
