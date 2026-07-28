/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.*
import com.huawei.excelsior.jet.compiler.opt.Opt
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.*
import com.huawei.excelsior.jet.compiler.{Env, Stage}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.escape.StackAllocAnalysis
import com.huawei.excelsior.jet.compiler.opt.middle.sync.SynchronizationElimination
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.opt.serialization.OptExtraInfo
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo.*
import com.huawei.excelsior.jet.compiler.symlevel.{Method, Type as SymType}
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind as MAK
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.annotation.tailrec

trait CallSites extends OptExtraInfo with InlineIRInfo with StackAllocAnalysis with SynchronizationElimination with CallSitesHelper { self: Universe =>

  private val scalarInlineMultiplierBase = env.valueOf(ScalarInlineMultiplierBase)
  private val scalarInlineMultiplierExpBase = env.valueOf(ScalarInlineMultiplierExpBase)
  private val bodySizeGapOfCDIBytes = env.valueOf(InlineBodySizeGapOfCDIBytes)
  private val maxWeightOfCDITicks = env.valueOf(InlineMaxWeightOfCDITicks)
  private val maxWeightOfCDINestedSync = env.valueOf(InlineMaxWeightOfCDINestedSync)
  private val maxWeightOfRecursiveCDI = env.valueOf(InlineMaxWeightOfRecursiveCDI)

  private val bodyWeightMultiplier = env.valueOf(InlineBodyWeightMultiplier) / 100.0
  private def adjustedBodyWeight(info: MethodExtraInfoLocal) = info.bodyWeight * bodyWeightMultiplier

  /** Number of times method may be inlined into itself (e.g. 0 - prohibit recursive inline). */
  private def maxRecursiveInlinedCallsDepth = env.valueOf(
    if (profile.isPGOHost) {
      InlineMaxRecursiveDepthInPGOHosts
    } else {
      InlineMaxRecursiveDepth
    })

  sealed class CallSite(val target: Method, val direct: Boolean, val node: AbstractCall) {
    override def toString = s"[$node -> ${target.getFullName}, ${if (direct) "direct" else "indirect"}]"
  }

  final class JavaCallSite(target: Method, direct: Boolean, node: Call) extends CallSite(target, direct, node) {

    def this(node: Call) =
      this(node.targetRef.method, node.akind == MAK.SPECIAL || node.akind == MAK.STATIC || node.akind == MAK.MUT, node)

    private def baseDecision(preinline: Boolean): InlineDecision = {
      denyNeverInlined orElse
      denyIfCannotInline(preinline) orElse
      denyNonCangjeInlineIntoCBC orElse
      denyUniversalGeneric orElse
      denyStdlibInlineIntoCBC orElse
      denyLongSafeInline orElse
      denyInlineManagedIntoManual orElse
      allowTailRec orElse
      denyRecursiveInline(preinline) orElse
      allowAJInline orElse
      denyCangjieInline(preinline) orElse
      denyIfGenDebug orElse
      allowAJInlineIfConstParams orElse
      denyUnstructuredLocking orElse
      allowJCAInlined orElse
      allowJCAInlinedWithContextPointTest orElse
      allowCangjiePerformanceCrutch orElse
      denyExceptionConstructors orElse
      denyColdCode
    }

    def shouldInline(preinline: Boolean): InlineDecision = stage(Stage.InlineDecision) {
      baseDecision(preinline) orElse
      allowAJReplaced orElse {
        locallyAnalyzeMethod(target) match {
          case Some(info) =>
            allowCFI(info, guarded = false) orElse
            allowCDIBytes(info) orElse
            allowCDITicks(info) orElse
            allowCDIScalar(info) orElse
            allowCDICangjieSafeArithmetic(info) orElse
            allowCDIBitShiftsForJ2CJ(info) orElse
            allowRecursiveCDI(info) orElse
            allowNestedSynchronizationCDI(info)
          case None => assert(preinline); DoNotKnow // it's a preinline from bytecode
        }
      } orElse
      extraAllowOnPostinline(preinline)
    }

    /** Guarded devirtualization is quite expensive
      * so it should be done only if devirtualized invoke would be inlined
      * (it means that if `shouldInlineWithGuard` returns `Yes`,
      * `shouldInline` has to return `Yes` too).
      */
    def shouldInlineWithGuard(): InlineDecision = stage(Stage.InlineDecision) {
      baseDecision(preinline = false) orElse {
        locallyAnalyzeMethod(target) match {
          case Some(info) =>
            allowCFI(info, guarded = true) orElse
            allowCDITicks(info) orElse
            allowRecursiveCDI(info) orElse
            allowNestedSynchronizationCDI(info)
          case None => shouldNotReachHere()
        }
      } orElse
      extraAllowOnPostinline(preinline = false)
    }

    private def extraAllowOnPostinline(preinline: Boolean) =
      if (!preinline) {
        {
          locallyAnalyzeMethod(target) match {
            case Some(info) =>
              allowSynchronizationOnNewCDI(info)
            case None => shouldNotReachHere()
          }
        } orElse {
          globallyAnalyzeMethod(target) match {
            case Some(info) =>
              allowStackAllocatableGeneralizedNew(info)
            case None => DoNotKnow
          }
        } orElse
        profileGuidedDecision()
      } else DoNotKnow

    /** Check AJ @Inline annotation. */
    private def allowAJInline =
      if (target.isAJInline) {
        if (target.isInlineAllAndRemove) Yes(s"AJ @Inline(forced = ${target.isAJInlineForced})")
        else if (!env.enabled(InlineOnlyForced)) Yes("AJ @Inline(forced = false)")
        else DoNotKnow
      } else DoNotKnow

    /** Check GenDebug is enabled. */
    private def denyIfGenDebug =
      if (genDebug) No("GenDebug is enabled")
      else DoNotKnow

    /** Check AJ @InlineIfConstParams annotation. */
    private def allowAJInlineIfConstParams = {
      val paramsIndices = target.getAJInlineIfConstParams
      if (paramsIndices != null) {
        val args = node.invokeArgs
        if (paramsIndices forall (args(_).isInstanceOf[Constant])) {
          Yes("AJ @InlineIfConstParams")
        } else DoNotKnow
      } else DoNotKnow
    }

    /** Check JCA ALWAYS_INLINE directive. */
    private def allowJCAInlined =
      if (target.isJCAInline) Yes("JCA ALWAYS_INLINE")
      else DoNotKnow

    /** Check JCA INLINE_WITH_CONTEXT_POINT_TEST directive. */
    private def allowJCAInlinedWithContextPointTest =
      if (target.isJCAInlineWithContextPointTest && shouldInlineWithContextPointTest(target, node.receiver)) Yes("JCA INLINE_WITH_CONTEXT_POINT_TEST")
      else DoNotKnow

    private def allowCangjiePerformanceCrutch = {
      val t = target.getDeclaringClass
      if (t.isCangjieType && (
        // Range and its iterator in for-in pattern
        t.getName.contains(STD_CORE_RANGE_PART) ||
          t.getName.contains(STD_CORE_RANGE_ITERATOR_PART) ||
          // Array iterator in for-in pattern
          t.getName.contains(STD_CORE_ARRAY_ITERATOR_PART) ||
          (target.getName.contains(STD_CORE_ARRAY_EXTEND_PART) && target.getName.contains("8iterator")) ||
          // Array constructor, getter/setter and slice
          (target.getName.contains(STD_CORE_ARRAY_PART) && (
            target.getName.contains("6<init>") || target.getName.contains("2[]") || target.getName.contains("12getUnchecked")
              || target.getName.contains("10rangeSlice"))) ||
          // Future-less spawn implementation
          (target.getName.contains(STD_CORE_FUTURE_PART) && target.getName.contains("14executeClosure")) ||
          // Option of array slice
          (target.getName.contains(STD_CORE_OPTION_ARRAY_PART) && target.getName.contains("10getOrThrow"))
        )) {
        Yes("Cangjie performance crutch")
      } else DoNotKnow
    }

    /** Check AJ annotation and JCA directive. */
    private def denyNeverInlined =
      if (target.isNeverInline) No("AJ @NoInline or JCA NOT_INLINE")
      else DoNotKnow

    private def denyIfCannotInline(preinline: Boolean) =
      if (target.getDeclaringClass.isDeferred) No("target is absent")
      else if (!direct) No("virtual call")
      // it's not easy to inline varargs method
      else if (target.isVarArgs) No("varargs")
      // native and @CallToManaged wrappers are compiled by wrapper-generator
      else if (target.isNative && !target.isAJReplaced) No("native or @CallToManaged wrapper")
      // require serialized body or ability to parse from bytecode (only on preinline)
      else if (!((preinline && Opt.canParseMethod(target)) || passFront(target))) No("body unavailable")
      else DoNotKnow

    private def denyCangjieInline(preinline: Boolean) =
      if (target.getDeclaringClass.isCangjieType &&
        (target.isIntrinsicCall || target.isIntrinsicWithBodyCall || target.isAJReplaced)) {
        // We do not know on preinline what is compiling -- CBC or AOT.
        if (preinline) No("avoid inlining cangjie platform-specific methods on preinline")
        // Replacements or Intrinsics could lead to generation of platform-specific code, that broke portability of CBC.
        else if (!preinline && Env.targetArch == CBC) No("avoid inlining intrinsics and replacements in CBC mode")
        else DoNotKnow
      } else DoNotKnow

    private def denyNonCangjeInlineIntoCBC =
      if (!target.getDeclaringClass.isCangjieType && Env.targetArch == CBC) No("avoid inlining non-Cangjie in CBC mode")
      else DoNotKnow

    private def denyUniversalGeneric =
      if (target.hasUniversalGenericContext) No("avoid inlining Universal Generic functions")
      else DoNotKnow

    private def denyStdlibInlineIntoCBC = {
      if (Env.targetArch == CBC && !target.getDeclaringClass.isInCurrentCompilationSet) {
        if (env.enabled(NeverInlineStdlibToCBC)) {
          No("avoid inlining stdlib in CBC mode")
        } else {
          locallyAnalyzeMethod(target) match {
            case Some(info) => if (info.badForCBC) No("avoid inlining bad code from stdlib in CBC mode") else DoNotKnow
            case None => No("avoid inlining potentially bad code from stdlib in CBC mode")
          }
        }
      } else DoNotKnow
    }

    private def denyLongSafeInline =
      if (target.getMethodType.isAJLongSafe) No("AJ @LongSafe")
      else DoNotKnow

    private def denyInlineManagedIntoManual =
      if (rootMethod.isManual && target.isManaged) No("avoid inlining Managed into Manual methods")
      else DoNotKnow

    private def denyRecursiveInline(preinline: Boolean) =
      // prohibit recursive inline on preinline
      if (preinline && node.inlineContext.contains(target)) No("recursive inline on preinline")
      // limit recursive inline on postinline
      else if (!preinline && node.inlineContext.count(target) > maxRecursiveInlinedCallsDepth) No("too deep recursive inline on postinline")
      else DoNotKnow

    private def denyExceptionConstructors =
      if (target.isConstructor &&
          target.getDeclaringClass.isJavaReference &&
          (ReferenceType.javaLangThrowable >= ReferenceType(target.getDeclaringClass))) {
        No("exception constructor")
      }
      else DoNotKnow

    private def denyColdCode =
      if (node.block.isCold) No("cold code")
      else DoNotKnow

    private def denyUnstructuredLocking =
      if (isStructuredLocking) {
        locallyAnalyzeMethod(target) match {
          case Some(info) =>
            // prevent propagation of unstructured locking to our method
            if (info.isUnstructuredLocking) No("unstructured locking")
            else DoNotKnow
          case None => No("potentially unstructured locking") // it's a preinline from bytecode
        }
      } else DoNotKnow

    private def allowAJReplaced =
      if (target.isAJReplaced) Yes("@Replaced")
      else DoNotKnow

    /** Context-Free Inline. */
    private def allowCFI(info: MethodExtraInfoLocal, guarded: Boolean) =
      if ((!guarded && info.cfi) || (guarded && info.cfiWithGuard)) Yes("CFI")
      else DoNotKnow

    /** Context-Dependent Inline: abstract byte weighting (callsite vs target body). */
    private def allowCDIBytes(info: MethodExtraInfoLocal) = {
      val bodyWeight = adjustedBodyWeight(info)
      val callSiteWeight = nodeWeight(node)
      assert(callSiteWeight < Double.PositiveInfinity)

      if (bodyWeight <= (callSiteWeight + bodySizeGapOfCDIBytes)) {
        Yes(s"CDI (bytes, $bodyWeight <= $callSiteWeight + $bodySizeGapOfCDIBytes)")
      } else DoNotKnow
    }

    private def isInLoop =
      cfg.loops.isInLoop(node.block)

    /** Context-Dependent Inline: abstract tick weighting (callsite in loop, 0 or infinity weight). */
    private def allowCDITicks(info: MethodExtraInfoLocal) = {
      if (info.bodyDuration < Double.PositiveInfinity &&
          adjustedBodyWeight(info) <= maxWeightOfCDITicks &&
          isInLoop) {
        Yes("CDI (ticks)")
      } else DoNotKnow
    }

    /** Context-Dependent Inline: abstract byte weighting (callsite vs target body). */
    private def allowCDIScalar(info: MethodExtraInfoLocal) = {
      if (info.isScalarMethod) {
        val nonConstArgsCount = node.invokeArgs.count(!_.isInstanceOf[Constant])
        if (nonConstArgsCount == 0 && info.bodyDuration < Double.PositiveInfinity) {
          Yes(s"CDI (scalar, all constants, no loops)")

        } else {
          val bodyWeight = adjustedBodyWeight(info)
          val callSiteWeight = nodeWeight(node)
          assert(callSiteWeight < Double.PositiveInfinity)

          val scalarInlineMultiplier = 1 + (scalarInlineMultiplierBase / Math.pow(scalarInlineMultiplierExpBase, nonConstArgsCount - 1))
          if (bodyWeight <= (callSiteWeight * scalarInlineMultiplier)) {
            Yes(s"CDI (scalar, $nonConstArgsCount) (bytes, $bodyWeight <= $callSiteWeight * $scalarInlineMultiplier)")

          } else DoNotKnow
        }
      } else DoNotKnow
    }

    private def allowCDICangjieSafeArithmetic(info: MethodExtraInfoLocal) = {
      if (node.invokeArgs.forall(_.isInstanceOf[Constant]) && isCangjieSafeArithmeticMethod(target)) {
        Yes("CDI (cangjie safe arithmetic, all constants)")
      } else DoNotKnow
    }

    private def isCangjieSafeArithmeticMethod(m: Method) = {
      // Example of typical name: _ZN8std.core11throwingAddll
      val name = m.getName
      val packagePart = "_ZN8std.core"
      name.startsWith(packagePart) && (
        name.startsWith("14overflowing", packagePart.length) ||
        name.startsWith("13saturating",  packagePart.length) ||
        name.startsWith("11throwing",    packagePart.length)
      )
      // We hope that remaining three letters would be one of arithmetic operations.
    }

    private def allowCDIBitShiftsForJ2CJ(info: MethodExtraInfoLocal) = {
      def isJ2CJBitShiftMethod(m: Method) = {
        // Example of typical name: _ZN13j2cjlib.utils3shlEll
        val name = m.getName
        val subPackagePart = ".utils"
        name.indexOf(subPackagePart) match {
          case -1 => false
          case i =>
            val offset = i + subPackagePart.length
            name.startsWith("3shl", offset) ||
              name.startsWith("3shr", offset) ||
              name.startsWith("4ushr", offset)
        }
      }

      if (node.invokeArgs.exists(_.isInstanceOf[Constant]) && isJ2CJBitShiftMethod(target)) {
        Yes("CDI (J2CJ bit shift, at least one constant arg)")
      } else DoNotKnow
    }

    private def allowRecursiveCDI(info: MethodExtraInfoLocal) = {
      if (isRecursive && adjustedBodyWeight(info) <= maxWeightOfRecursiveCDI) {
        Yes(s"CDI (recursive) (bytes, ${adjustedBodyWeight(info)} <= $maxWeightOfRecursiveCDI)")
      } else DoNotKnow
    }

    private def profileGuidedDecision() = {
      assert(direct) // `inlinePlanContains` should only be called with the method which we actually want to inline
      if (profile.inlinePlanContains(node.pos, target)) {
        Yes("PGI (leaf)")
      } else if (profile.isPGOHost &&
        (typeProvider.isIteratorLike(target.getReturnType.symType) || typeProvider.isIteratorLike(target.getDeclaringClass))) {
        Yes("PGI (iterator-like)")
      } else DoNotKnow
    }

    private def allowSynchronization(info: MethodExtraInfoLocal, isMethodGood: => Boolean, allowArg: Node => InlineDecision): InlineDecision = {
      if (info.syncedParams.nonEmpty && isStructuredLocking && isMethodGood) {
        for (argNum <- info.syncedParams) {
          allowArg(node.invokeArgs(argNum)) match {
            case y: Yes => return y
            case _ =>
          }
        }
      }

      DoNotKnow
    }

    private def allowNestedSynchronizationCDI(info: MethodExtraInfoLocal): InlineDecision = {
      val bodyWeightWithoutSync = adjustedBodyWeight(info) - info.bodySyncOperationsWeight

      lazy val enclosingSyncRegions = ScalaCollections.iterateUntilNone(SynchronizedRegion.enclosing(node))(_.outer).toList

      allowSynchronization(info,
        info.leaf && (bodyWeightWithoutSync <= maxWeightOfCDINestedSync) && enclosingSyncRegions.nonEmpty,
        arg =>
          if (enclosingSyncRegions flatMap (_.singleMonitorObj) contains arg) {
            Yes(s"CDI (synchronization on already synchronized arg, $bodyWeightWithoutSync <= $maxWeightOfCDINestedSync)")
          } else DoNotKnow
      )
    }

    private def allowSynchronizationOnNewCDI(info: MethodExtraInfoLocal): InlineDecision = {
      // escape information could be calculated precisely only after serialization
      allowSynchronization(info,
        info.leaf,
        {
          case arg: AnyNew if mayRemoveSynchronizationOn(arg) => Yes("CDI (synchronization on non-escaping new)")
          case _ => DoNotKnow
        }
      )
    }

    private def allowStackAllocatableGeneralizedNew(info: MethodExtraInfoGlobal): InlineDecision = {
      if (info.generalizedNewTypes.nonEmpty) {
        // this inline may dramatically increase size of IR so we skip it on preinline to avoid triggering HugeMethodsLimit
        // also escape information could be calculated precisely only after serialization

        object InlinableNewType {
          def unapply(allocType: SymType): Option[String] = {
            // TODO: evaluate these heuristics and add other cases (e.g. Scala lambda & iterators), JET-9211
            if (allocType.isClass) {
              if (allocType.isAnonymous) {
                return Some("lambda")
              } else if ((allocType doesImplement typeProvider.getIteratorType) || (allocType doesImplement typeProvider.getAJIteratorType)) {
                return Some("Iterator")
              }
            }
            if (env.enabled(InlineAllGNew)) Some(s"+$InlineAllGNew") else None
          }
        }

        info.generalizedNewTypes collectFirst {
          case allocType @ InlinableNewType(reason) if mayBeAllocatedOnStack(node, allocType) =>
            Yes(s"GNEW ($reason)")
        } getOrElse DoNotKnow
      } else DoNotKnow
    }

    private def allowTailRec: InlineDecision =
      if (isTailRec) Yes(s"TailRec")
      else DoNotKnow

    def isTailRec: Boolean =
      !isO1Compiled && !env.enabled(NoTailRec) && isRecursive && isTailCall

    private def isRecursive: Boolean =
      target == rootMethod

    private def isTailCall: Boolean = {
      def phiesCorrespondingToValues(block: Block, inEdge: Edge, values: Set[Node]) =
        block.phies filter { phi => values contains phi.phiArg(inEdge) }

      @tailrec
      def nextOperationIsReturnOf(ctrl: ControlNode, possibleRetVals: Set[Node]): Boolean = ctrl match {
        case Return(_, _, _: Void) =>
          true

        case Return(_, _, `node`) =>
          true

        case Return(_, _, retVal) =>
          possibleRetVals contains retVal

        case goto @ Goto(_, nextBlock) =>
          // ColdCodeMarker may be safely ignored.
          // Currently there is no motivation to handle other marker-nodes and moreover it could be incorrect.
          val emptyBlock = nextBlock.spine.forall { case _: ColdCodeMarker => true; case _ => false }
          emptyBlock && {
            val newRetVals = phiesCorrespondingToValues(nextBlock, goto.targetEdge, possibleRetVals)
            nextOperationIsReturnOf(nextBlock.blockEnd, possibleRetVals ++ newRetVals)
          }

        case _ => false
      }

      !node.hasXHandler && nextOperationIsReturnOf(node.outCtrl, Set(node))
    }
  }

}
