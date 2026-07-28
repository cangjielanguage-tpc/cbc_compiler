/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.escape

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.ir.{CallEscapeKind, EscapeKind, EscapeKindTuple, NewEscapeKind}
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.CallTargetInfos
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.CallTargetSearchResults.*
import com.huawei.excelsior.jet.compiler.types.Guards.Guard
import com.huawei.excelsior.jet.compiler.ir.EscapeKindTuple.*
import com.huawei.excelsior.jet.compiler.ir.NewEscapeKind.GuaranteeEscape
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.compiler.types.References.{Cone, Point, ReferenceApprox}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{Method, Type as SymType}
import com.huawei.excelsior.jet.compiler.util.{Log, Sets}

import scala.annotation.nowarn
import scala.collection.mutable

/** Analysis of escape kind of nodes.
  * Initially described in JET-9206.
  *
  * @author cypok
  */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait EscapeAnalysis extends ClassReceiverEscapeAnalysis with CallTargetInfos { self: Universe =>

  sealed abstract class EscapeResult {
    def escape: EscapeKind
  }

  object EscapeResult {
    case class Plain(escape: EscapeKind) extends EscapeResult
    case class Guarded(escape: EscapeKind, guard: GuardKey, invokes: Seq[(Call, Cone)]) extends EscapeResult {
      require(!escape.containsEscape)
    }
  }

  private def log = Log(Log.Kind.EscapeAnalysis)

  def escapeKindOfNew(node: AnyNew): EscapeKind = {
    escapeKindOfGeneralizedNew(node, node.allocType.symType, allowGuarded = false) match {
      case EscapeResult.Plain(esc) => esc
      case EscapeResult.Guarded(_, _, _) => shouldNotReachHere()
    }
  }

  def escapeKindOfGeneralizedNew(node: Node, allocType: SymType, allowGuarded: Boolean): EscapeResult = {
    val res = calcEscape(node, Some(typeApproxFromAllocType(allocType)), allowGuarded)
    if (currentPhase > CompilerPhase.InterProceduralAnalysis) {
      unfoldRcvEscape(res, allocType)
    } else {
      res
    }
  }

  private[escape] def typeApproxFromAllocType(allocType: SymType) = {
    require(!allocType.isDeferred)
    Point(ReferenceType(asClassType(allocType)), mayBeNull = false)
  }

  private def unfoldRcvEscape(res: EscapeResult, rcvType: SymType): EscapeResult = {
    assert(currentPhase > CompilerPhase.InterProceduralAnalysis,
      "RcvEscape must be unfolded only after inter-procedural analysis to prevent parsing cycles with inaccurate results")
    if (res.escape.containsReceiverEscape) {
      val rcvClass =
        if (rcvType.isJavaArray) {
          typeProvider.getObjectType
        } else {
          assert(rcvType.isClass)
          asClassType(rcvType)
        }
      val escUnfolded = res.escape.transformReceiverEscapeTo(classReceiverEscape(rcvClass))
      res match {
        case _ if escUnfolded.containsEscape => EscapeResult.Plain(NewEscapeKind.GuaranteeEscape)
        case res: EscapeResult.Plain   => res.copy(escape = escUnfolded)
        case res: EscapeResult.Guarded => res.copy(escape = escUnfolded)
      }
    } else {
      res
    }
  }

  def calcRefinedParamsEscape(): Option[Seq[EscapeKind]] = {
    // Note that we do not have fast paths for empty or only primitive params because it is quite rare case.
    val params = all[Param].toList
    val paramsCount = rootMethod.getParamsCount
    val escs = Array.tabulate(paramsCount) { idx =>
      val paramType = rootMethod.getParamType(idx)
      if (paramType.isTraceableReference) {
        if (rootMethod.isVarArgs && idx == paramsCount - 1) {
          // This value is actually not used by escape analysis but it is convenient to have non-null escape value
          // for all "reference" parameters
          NewEscapeKind.GuaranteeEscape
        } else {
          params find (_.num == idx) match {
            case Some(arg) =>
              calcEscape(arg, None, allowGuarded = false).escape

            case None =>
              // it means that this parameter is not used in rootMethod
              NewEscapeKind.NoEscape
          }
        }
      } else {
        null
      }
    }

    if (escs exists { e => e != null && !e.containsEscape }) {
      // keep information only if there is something non-conservative
      Some(escs.toIndexedSeq)
    } else {
      None
    }
  }

  /** Returns method parameters escape information.
    * Returns `None` if this information cannot be obtained at this moment.
    */
  private[escape] def methodEscapeInfo(method: Method): Option[Int => EscapeKind] = {
    import Function.const

    // TODO: use information from @CompilerHint("no-escape") (verify these annotations and separate NoEscape & RetEscape (e.g. JR_ArrayCast with "no-escape" returns first argument))
    val jcaInfo = method.getJCAParamsEscapeInfo
    if (jcaInfo != Method.JCA_PARAMS_ESCAPE_NO_INFO) {
      assert(!method.isVarArgs)
      if (jcaInfo == Method.JCA_PARAMS_ESCAPE_NO_ESCAPE) {
        Some(const(NewEscapeKind.NoEscape))
      } else {
        val retEscapeParamNum = jcaInfo - Method.JCA_PARAMS_ESCAPE_RET_ESCAPE_BASE
        assert(retEscapeParamNum >= 0)
        Some({ idx => if (idx == retEscapeParamNum) CallEscapeKind.RetEscape else NewEscapeKind.NoEscape })
      }
    } else {
      globallyAnalyzeMethod(method) map { info =>
        info.paramsEscape match {
          case None =>
            // empty paramsEscape means that all arguments are escaped, materialize this information
            const(NewEscapeKind.GuaranteeEscape)

          case Some(paramsEscape) =>
            if (method.isVarArgs) {
              // all varargs are conservatively escaped, but it does not matter, as no Java refs or Thins are allowed
              { idx => assert(idx < method.getParamsCount - 1); paramsEscape(idx) }
            } else {
              paramsEscape
            }
        }
      }
    }
  }

  private def calcEscape(node: Node, typeApproxOpt: Option[ReferenceApprox], allowGuarded: Boolean): EscapeResult = {
    if (isO1Compiled || !env.enabled(BoolOption.EscapeAnalysis)) {
      return EscapeResult.Plain(NewEscapeKind.GuaranteeEscape)
    }
    val visitedPhies = Sets[Phi].newMSet
    var singleGuard: Option[GuardKey] = None
    val guardedInvokesInfo = mutable.ListBuffer.empty[(Call, Cone)]

    import EscapeResult._

    def analyzeUses(n: Node): EscapeKind = {
      assert(n.tpe.isTraceableRefType)

      var esc: EscapeKind = NewEscapeKind.NoEscape
      for (edge <- n.valueOutEdges) {
        esc = esc /\ analyzeUse(edge)
        if (esc.containsPotentialEscape) {
          return NewEscapeKind.PotentialEscape
        }
      }
      esc
    }

    def analyzeUse(useEdge: Edge): EscapeKind = {
      // Note: this algorithm should correspond to checkAllPossibleUsesOfNonEscapedNode.

      def globalEscape(extraMsg: String = null): EscapeKind = {
        log(s"NewEscapeKind.GuaranteeEscape because of: ${useEdge.target.name}${if (extraMsg != null) s" ($extraMsg)" else ""}")
        GuaranteeEscape
      }

      def directCallEscape(method: Method, call: AbstractCall): EscapeKind = {
        val paramIdx = call.invokeArgIdx(useEdge)
        methodEscapeInfo(method) match {
          case Some(paramEscape) =>
            val esc = paramEscape(paramIdx)
            assert(esc != null)
            if (esc.containsEscape) {
              globalEscape(s"direct call: param #$paramIdx")
            } else if (esc.containsRetEscape) {
              esc.transformRetEscapeTo(analyzeUses(call))
            } else {
              esc
            }

          case None =>
            if (mayBeCalledVirtually(method) && paramIdx == method.getReceiverArgIdx) {
              // this case helps a lot if there are loops in call graph of instance methods
              CallEscapeKind.ReceiverEscape
            } else {
              globalEscape("direct call: no information")
            }
        }
      }

      def guardedDirectCallEscape(call: Call, method: Method, guard: Guard, rcvType: Cone) = {
        assert(allowGuarded)

        val callGuard = GuardKey(call.receiver, guard)

        def guardedGlobalEscape(reason: String) = {
          val paramIdx = call.invokeArgIdx(useEdge)
          assert(paramIdx != call.targetRef.getReceiverArgIndex, "guards are used only if analyzed node is not a call receiver")
          globalEscape(s"virtual call: param #$paramIdx, $reason")
        }

        if (singleGuard.isEmpty) {
          singleGuard = Some(callGuard)
        }

        if (singleGuard contains callGuard) {
          guardedInvokesInfo += ((call, rcvType))
          directCallEscape(method, call)
        } else {
          guardedGlobalEscape("different guards")
        }
      }

      def virtualCallEscape(call: Call) = {
        assert(mayBeCalledVirtually(call.targetRef.method))
        val paramIdx = call.invokeArgIdx(useEdge)
        if (paramIdx == call.targetRef.getReceiverArgIndex) {
          // our node is receiver, so don't bother with guarded stack alloc
          val typeApprox = typeApproxOpt getOrElse nodeTypeAt(node, call)
          findTargetMethod(call, node, typeApprox, GuardMode.NoGuards) match {
            case OneDirectTarget(method) => directCallEscape(method, call)
            case UnknownTarget | ProbableNoTarget => CallEscapeKind.ReceiverEscape
            case _: NoTarget => NewEscapeKind.NoEscape
            case x => shouldNotReachHere(x)
          }

        } else if (allowGuarded) {
          // our node is argument, so we can try for guarded stack alloc
          findTargetMethod(call, GuardMode.RealGuards) match {
            case OneGuardedTarget(method, guard, _, rcvType) =>
              guardedDirectCallEscape(call, method, guard, rcvType)

            case OneDirectTarget(method) => directCallEscape(method, call)
            case _: MultipleGuardedTargets => globalEscape(s"virtual call: param #$paramIdx, multiple guards")
            case UnknownTarget | ProbableNoTarget => globalEscape(s"virtual call: param #$paramIdx")
            case _: NoTarget => NewEscapeKind.NoEscape
          }

        } else {
          globalEscape(s"virtual call: param #$paramIdx")
        }
      }

      useEdge.target match {
        case _: Deferred | _: BitcodeDeferred | _: AssignVar => NewEscapeKind.GuaranteeEscape

        case _: Return => CallEscapeKind.RetEscape
        case _: Throw => globalEscape()
        case _: ConvertDomain => globalEscape() // All non-escaping ConvertDomain nodes will be eliminated anyway.

        case _: ConcealRef | _: AcquireRawData => globalEscape()

        // TODO: we may analyze object of putfield/arrayput to use its escape properties
        case u: PutMemoryOperation => if (u.isPutValue(useEdge)) globalEscape() else NewEscapeKind.NoEscape

        case u: AJArrayFill => if (u.isFillValue(useEdge)) globalEscape() else NewEscapeKind.NoEscape

        case _: ExtractEnrichment => NewEscapeKind.NoEscape
        case u: EOPOperation => analyzeUses(u)

        case u: AbstractCall => u match {
          case u @ AnyDirectCall(method) => directCallEscape(method, u)

          case u @ AnyVirtualCall() => virtualCallEscape(u)

          case BitcodeDeferred.Invoke(_) => globalEscape()

          case UniversalGeneric.InvokeConstraintMethod(_) => globalEscape()

          case _ => shouldNotReachHere("unexpected kind of call: " + u)
        }

        case u: Phi =>
          if (visitedPhies contains u) {
            NewEscapeKind.NoEscape
          } else {
            visitedPhies += u
            analyzeUses(u)
          }

        case NodeWithNoEscapeArguments() => NewEscapeKind.NoEscape

        case _: DelayedGet | _: DelayedPut | _: DelayedInstanceMethodVNum |
             _: DelayedInstanceFieldAddress | _: DelayedMethodAddr => NewEscapeKind.GuaranteeEscape // TODO: ?

        case x: TDBarrier => analyzeUses(x)
        case x: WriteBarrier => analyzeUses(x)

        case x: UniversalGeneric.ConvertHolder => NewEscapeKind.GuaranteeEscape
        case x: UniversalGeneric.GetField => NewEscapeKind.GuaranteeEscape // FIXME-UG
        case x: UniversalGeneric.GetFieldOHM => NewEscapeKind.GuaranteeEscape // FIXME-UG
        case x: UniversalGeneric.PutField => NewEscapeKind.GuaranteeEscape // FIXME-UG

        case x: MutFunc.Combine => NewEscapeKind.NoEscape

        case _ => shouldNotReachHere(s"unexpected use of ${useEdge.source.name} at ${useEdge.target.name}")
      }
    }


    log.inSession(s"node escape at $codeUnit for ${node.name} with type $typeApproxOpt") {
      val esc = analyzeUses(node)
      if (!esc.containsEscape) {
        log(s"result: $esc${singleGuard map (" with " + _) getOrElse ""}")
      }

      singleGuard match {
        case Some(guard) if !esc.containsEscape =>
          assert(allowGuarded)
          Guarded(esc, guard, guardedInvokesInfo.toList)

        case _ => Plain(esc)
      }
    }
  }

  /** Analyzes all direct & indirect uses of node's value in terms of escape analysis
    * and checks that given predicate holds for all of them.
    */
  def checkAllPossibleUsesOfNonEscapedNodeValue(node: AnyNew)(check: Node => Boolean): Boolean =
    checkAllPossibleUsesOfNonEscapedNodeValue(node, typeApproxFromAllocType(node.allocType.symType), allowGuarded = false)(check)

  /** Analyzes all direct & indirect uses of node's value in terms of escape analysis
    * and checks that given predicate holds for all of them.
    */
  // TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
  @nowarn("msg=The outer reference in this type test cannot be checked at run time")
  def checkAllPossibleUsesOfNonEscapedNodeValue(node: Node, typeApprox: ReferenceApprox, allowGuarded: Boolean)(check: Node => Boolean): Boolean = {
    val visitedPhies = Sets[Phi].newMSet

    def checkUses(n: Node): Boolean =
      n.valueOutEdges forall checkUse

    def checkUse(useEdge: Edge): Boolean = {
      // Note: this algorithm should correspond to calcEscape.
      // All global escape uses should be filtered earlier by escape analysis.

      def checkDirectCall(method: Method, call: AbstractCall) = {
        methodEscapeInfo(method) match {
          case Some(paramEscape) =>
            val paramIdx = call.invokeArgIdx(useEdge)
            val esc = paramEscape(paramIdx)
            if (esc.containsRetEscape) {
              checkUses(call)
            } else {
              true
            }

          case None =>
            true
        }
      }

      def checkVirtualCall(call: Call) = {
        assert(mayBeCalledVirtually(call.targetRef.method))
        val paramIdx = call.invokeArgIdx(useEdge)
        if (paramIdx == call.targetRef.getReceiverArgIndex) {
          // our node is receiver, so don't bother with guarded stack alloc
          findTargetMethod(call, node, typeApprox, GuardMode.NoGuards) match {
            case OneDirectTarget(method) => checkDirectCall(method, call)
            case UnknownTarget | ProbableNoTarget | _: NoTarget => true
            case x => shouldNotReachHere(x)
          }

        } else {
          assert(allowGuarded)
          // our node is argument, so we can try for guarded stack alloc
          findTargetMethod(call, GuardMode.AnyGuards) match {
            case OneGuardedTarget(method, _, _, _) => checkDirectCall(method, call)
            case OneDirectTarget(method) => checkDirectCall(method, call)
            case _: NoTarget => true
            case x => shouldNotReachHere(x)
          }
        }
      }

      useEdge.target match {
        case u: Return => check(u)

        case u: PutMemoryOperation => assert(!u.isPutValue(useEdge)); check(u)

        case u: AJArrayFill => assert(!u.isFillValue(useEdge)); check(u)

        case u: EOPOperation => check(u) && checkUses(u)

        case u: ConvertDomain => check(u) && checkUses(u)

        case u: WriteBarrier => check(u) && checkUses(u)

        case u: AbstractCall => check(u) && (u match {
          case u @ AnyDirectCall(method) => checkDirectCall(method, u)

          case u @ AnyVirtualCall() => checkVirtualCall(u)

          case _ => shouldNotReachHere("unexpected kind of call: " + u)
        })

        case u: Phi =>
          if (visitedPhies contains u) {
            true
          } else {
            visitedPhies += u
            check(u) && checkUses(u)
          }

        case u @ NodeWithNoEscapeArguments() => check(u)

        case _ => shouldNotReachHere(s"unexpected use of ${useEdge.source.name} at ${useEdge.target.name}")
      }
    }

    checkUses(node)
  }

  private object NodeWithNoEscapeArguments {
    def unapply(n: Node): Boolean = n match {
      case _: GetMemoryOperation | _: ArrayLength |
           _: AbstractNullCheck | _: ArrayStoreCheck | _: ArrayIndexCheck |
           _: CheckCast | _: InstanceOf | _: WeakCast | _: Cmp | _: TauTest |
           _: TauSwitch | _: CheckCastTrustedDelayed |
           _: MonitorOperation |
           _: AnyInvokeTarget |
           _: StrConcat | // StrConcat itself has no side-effects, String.valueOf() is called preliminarily
           _: LocalReachabilityShield |
           _: IsComputableAtCompileTime | _: ComputeAtCompileTime |
           _: ThinCheckCast | _: ThinInstanceOf |
           _: NewArrayCopy | _: NewArrayCopyRT | _: NewArrayRT |
           _: ArrayFill | _: StackZeroing |
           _: GetClass |
           _: AssertNode |
           _: VirtualMethodAddr |
           _: BeginLocalUnmovable | _: EndLocalUnmovable |
           _: ReleaseRawData |
           _: ThisTypeInfoBy
             => true
      case _ => false
    }
  }

  def implicitlyEscapedType(allocType: SymType): Boolean =
    allocType.finalizable ||
      (typeProvider.getReferenceType isAssignableFrom allocType) ||
      (typeProvider.getAJWeakRefType isAssignableFrom allocType) ||
      (typeProvider.isCangjieWeakRef(allocType))
}
