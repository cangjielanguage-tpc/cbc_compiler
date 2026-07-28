/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline

import com.huawei.excelsior.jet.compiler.options.BoolOption.{InstrumentTauBackupPath, InstrumentTauFastPath, UseIsa12}
import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString.ascii
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.{CallTargetInfos, LightInterfCalls, TauInfo, WeakCastElimination}
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.CallTargetSearchResults.*
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo.{PGO, Static}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.References.Cone
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Method, MethodReference, MethodReferenceAccessKind as MAK, Type as SymType}
import com.huawei.excelsior.jet.compiler.{PreparationRequired, RTSProc, Stage, Stats, StatsKind}

import scala.PartialFunction.cond
import scala.collection.mutable

/**
  * Devirtualize virtual calls.
  *
  * @author cypok
  */
private[inline] trait Devirtualization extends CallTargetInfos with LightInterfCalls with WeakCastElimination with CallSites { self: Universe =>

  /** Try to devirtualize call site and return devirtualized invoke and devirtualization method if successfully devirtualized. */
  def devirtualize(call: Call, allowGuarded: Boolean): Devirtualization.Result = stage(Stage.Devirt) {
    import Devirtualization._
    call match {
      case AnyVirtualCall() =>
        def msg(devirtWay: String) = devirtWay + " for invoke" + call.akind.toString.toLowerCase

        val guardMode = if (allowGuarded) GuardMode.RealGuards else GuardMode.NoGuards

        findTargetMethod(call, guardMode) match {
          case OneDirectTarget(target) =>
            Changed(Seq(transformCallToDirect(call, target)), msg("static direct"))

          case result: OneOrMultipleGuardedTargets =>
            assert(allowGuarded)
            assert(result.info != TauInfo.Unknown)
            devirtualizeWithGuards(call, result.info, result.rcvType, result.guardedTargets) match {
              case Nil =>
                tryLightInterfCall(call, guardMode)

              case devirtInvokesWithGuardLogs =>
                val (devirtInvokes, guardLogs) = devirtInvokesWithGuardLogs.unzip
                val guardLog = if (guardLogs.size == 1) guardLogs.head else "<" + guardLogs.mkString(", ") + ">"
                val infoStr = result.info match {
                  case TauInfo.Static => "static"
                  case TauInfo.JCA | _: TauInfo.PGO => "guided"
                  case TauInfo.Unknown => shouldNotReachHere()
                }
                Changed(devirtInvokes, msg(infoStr + " " + guardLog))
            }

          case ProbableNoTarget =>
            if (call.block.isCold) {
              // prevent double transformation
              Unchanged
            } else {
              call.block.markAsCold()
              Cold
            }

          case ErroneousCall(error) =>
            insertErrorRTSCallBefore(call, error.rtsProc)()
            Erroneous(error.toString)

          case UnreachableCall =>
            insertHaltBefore(call)
            Unreachable

          case UnknownTarget =>
            tryLightInterfCall(call, guardMode)
        }

      case DirectCall(realMethod) if call.targetRef.method.isIndirectCall =>
        Changed(Seq(transformCallToDirect(call, realMethod)), "static direct for AJ indirect call")

      case _ => Unchanged
    }
  }

  private def tryLightInterfCall(call: Call, guardMode: GuardMode): Devirtualization.Result = {
    import Devirtualization._
    import LightInterfCalls._

    findLightInterfCallResult(call, guardMode) match {
      case Inferred(klass) =>
        count("inferred light interf call")
        ChangedLight(transformInvokeToLight(call, klass), s"inferred $klass")

      case Guarded(ocGuard, _) =>
        import WeakCastElimination._
        isWeakCastEliminationPossible(call, ocGuard) match {
          case Possible =>
            ChangedLight(transformToGuardedLightInterfCall(call, ocGuard, "WeakCast elimination possible"), s"$ocGuard")

          case Absorbable(absorbingGuard) =>
            assert(absorbingGuard == ocGuard) // ocGuard.root is always implements absorbed checkcast's type
            ChangedLight(transformToGuardedLightInterfCall(call, ocGuard, "WeakCast absorbable"), s"$ocGuard")

          case Impossible(reason) =>
            count(s"no guarded light interf call ($reason)")
            TauTest.log(s"- no deinterf with $ocGuard for ${call.targetRef.method.getFullName} ($reason)")
            Unchanged
        }

      case Unknown =>
        Unchanged
    }
  }

  private def count(msg: String): Unit = {
    stats.count(StatsKind.Devirt, s"total/$msg")
  }

  object Devirtualization {
    sealed abstract class Result
    case class Changed(newCalls: Seq[Call], log: String) extends Result
    case class ChangedLight(newCall: Call, log: String) extends Result
    case object Unchanged extends Result
    case object Cold extends Result
    case object Unreachable extends Result
    case class Erroneous(log: String) extends Result
  }

  case class DevirtCandidate(target: Method, guard: Guard, reason: String, guardInfo: String)

  private def devirtualizeWithGuards(call: Call, info: TauInfo, rcvType: Cone, guardedTargets: Seq[(Method, Guard)]): Seq[(Call, String)] = {
    // TODO: current implementation of TauSwitch does not support MethodGuards (see JET-12289)
    //       so we skip them, or switch to single-target devirtualization
    val filteredGuardedTargets = guardedTargets.zipWithIndex match {
      case Seq(head @ ((_, _: MethodGuard), 0), _*) => Seq(head)
      case xs => xs filter {
        case ((_, _: MethodGuard), _) => false
        case _ => true
      }
    }
    assert(filteredGuardedTargets.nonEmpty)

    val singleGuard = filteredGuardedTargets.size == 1

    object Candidate {
      def unapply(x: (Method, Guard)) = x match { case (devirtTarget, guard) =>
        if (singleGuard) {
          import WeakCastElimination._
          isWeakCastEliminationPossible(call, guard) match {
            case Possible =>
              // Devirtualize even if there is no motivation for inline
              // because WeakCast will be moved to the backuppath.
              Some(DevirtCandidate(devirtTarget, guard, "WeakCast elimination possible", guard.toString))

            case Absorbable(absorbingGuard) =>
              // Devirtualize even if there is no motivation for inline
              // because WeakCast with CheckCast will be moved to the backuppath.
              Some(DevirtCandidate(devirtTarget, absorbingGuard, "WeakCast absorbable", s"$absorbingGuard (was $guard)"))

            case Impossible(_) =>
              // Else devirtualize only if there is motivation for inline.
              candidateForInline(call, devirtTarget, guard)
          }

        } else { // Multiple guards
          // Only devirtualize if there is motivation for inline.
          candidateForInline(call, devirtTarget, guard)
        }
      }
    }

    val candidatesMap = filteredGuardedTargets.collect{ case (Candidate(x), i) => (i, x) }.toMap
    if (candidatesMap.nonEmpty) {
      val devirtInfo = info match {
        case x: PGO => x.filterByIndex(candidatesMap.contains)
        case x => x
      }
      val devirtCandidates = candidatesMap.valuesIterator.toSeq
      val devirtInvokes = transformToMultiGuardedInvoke(call, devirtInfo, rcvType, devirtCandidates)
      devirtInvokes zip (devirtCandidates map (_.guardInfo))
    } else {
      Seq()
    }
  }

  private def candidateForInline(call: Call, devirtTarget: Method, guard: Guard): Option[DevirtCandidate] = {
    val devirtCS = new JavaCallSite(devirtTarget, direct = true, call)
    devirtCS.shouldInlineWithGuard() match {
      case Yes(inlineReason) =>
        checkConsistency(CheckLevels.Optional) {
          assert(devirtCS.shouldInline(preinline = false).isInstanceOf[Yes],
            "if we do guarded devirtualization, this callsite have to be inlined later")
        }
        Some(DevirtCandidate(devirtTarget, guard, inlineReason, guard.toString))

      case _ => None
    }
  }

  private def createInvoke(kind: MAK, target: Method, call: Call) = {
    assert(!target.isCangjieMut)
    // Method type can change if target's return type is more specific than original one (in Cangjie).
    // TODO: introduce bridge methods for such narrowing return type overriding and assert equal method types.
    val mt = call.targetRef.methodType
      .changeReturnType(target.getReturnType)

    val ref = call.targetRef
      .withAccessKind(kind)
      .withMethod(target)
      .withMethodType(mt)

    assert(PreparationRequired.forInvoke(ref) == null || (kind == MAK.SPECIAL && ref.refClass.isUltraThinClass))
    withPos(call) { kind match {
      case MAK.INTERFACE =>
        val InvokeInterfaceTarget(ciao) = call.target
        InvokeInterface(ref, ciao)(call.invokeArgs: _*)

      case _ => Invoke(ref)(call.invokeArgs: _*)
    }}
  }

  private def transformInvokeToLight(call: Call, rcvType: ClassType) = {
    val ciao = if (targetArch == CBC && env.enabled(UseIsa12)) {
      LightInterfCastCBC(rcvType)
    } else {
      lightInterfCast(rcvType, call.targetRef.refClass)
    }
    call.target.asInstanceOf[InvokeInterfaceTarget].ciao = ciao
    call
  }

  private def transformCallToDirect(call: Call, realTarget: Method) = {
    assert(!realTarget.isCangjieMut)
    val ref = if (call.targetRef.method.isIndirectCall) {
      val akind = if (realTarget.isStatic) MAK.STATIC else MAK.SPECIAL
      new MethodReference(realTarget, akind).withMethodType(call.targetRef.methodType)
    } else {
      // Method type can change if target's return type is more specific than original one (in Cangjie).
      // TODO: introduce bridge methods for such narrowing return type overriding and assert equal method types.
      val mt = call.targetRef.methodType
        .changeReturnType(realTarget.getReturnType)

      call.targetRef
        .withAccessKind(MAK.SPECIAL)
        .withMethod(realTarget)
        .withMethodType(mt)
    }
    replaceByCode(call)(DirectCall(ref)(call.invokeArgs: _*))
  }

  /** Transform invoke to guarded devirtualized invoke with old invoke as backup path.
    * <br/>
    * <pre>
    *   rcv.virtualCall(args);
    * </pre>
    * transforms to
    * <pre>
    *   tauSwitch(rcv) {
    *     case guard1:
    *       rcv.directCall1(args);
    *     case guard2:
    *       rcv.directCall2(args);
    *     ...
    *     default:
    *       rcv.virtualCall(args);
    *   }
    * </pre>
    * or just to
    * <pre>
    *   if (tauTest(rcv)) {
    *     rcv.directCall(args);
    *   } else {
    *     rcv.virtualCall(args);
    *   }
    * </pre>
    * in case of single devirt target.
    */
  private def transformToMultiGuardedInvoke(call: Call, info: TauInfo, rcvType: Cone, devirtCandidates: Seq[DevirtCandidate]) = {
    // because of block splitting and new nodes generation
    requireNoGlobalCodeMotion()

    def targets = devirtCandidates map (_.target)
    def guards = devirtCandidates map (_.guard)

    val combinedReason = (devirtCandidates map (_.reason)).distinct mkString ","

    count(s"guarded devirt ($combinedReason)")

    val devirtInvokes = replaceByMultiDiamondWithFastPaths(call, call.receiver, info)(guards: _*)(
      targets map (t => () => createInvoke(MAK.SPECIAL, t, call)): _*
    )

    workaroundForJET13686(call)

    TauTest.log(s"- devirt ${guards.mkString(",")} ($combinedReason) $info")
    val msg = s"[Devirt]\n codeUnit=$codeUnit.\n pos=${call.pos}\n devirtTargets=${targets.map(_.getFullName).mkString(",")}.\n invoke=${call.name}."
    markTauBackupPath(call, call.receiver, guards, msg)
    markTauFastPaths(call, devirtCandidates, devirtInvokes, msg)

    devirtInvokes
  }

  private def markTauFastPaths(call: Call, devirtCandidates: Seq[DevirtCandidate], devirtCalls: Seq[Call], msg: String): Unit = {
    if (env.enabled(InstrumentTauFastPath)) {
      for ((DevirtCandidate(devirtTarget, guard, _, _), devirtInvoke) <- devirtCandidates zip devirtCalls) {
        if (MethodTest.canBeGeneratedFor(devirtTarget)) {
          val (_, _, falseBlock) = insertEmptyDiamondBefore(devirtInvoke, PredicateConstructor.atom(ctrl => {
            val methodGuard = MethodGuard(call.targetRef, devirtTarget)
            val methodTest = MethodTest.withCIAO(methodGuard, TauInfo.Unknown, ctrl, call.receiver) {
              call.target.asInstanceOf[InvokeInterfaceTarget].ciao
            }
            methodTest.canBeUsedInDiamondDust = false
            TauTest.log(s"- instr  ${methodTest.name}")
            methodTest
          }))

          // transform backup path into ErrorRTSCall
          val debugMessage = AJString.bstr(ascii(s"$msg.\n guard=$guard"))
          replaceByErrorRTSCall(falseBlock.blockEnd, devirtInvoke, RTSProc.JR_TauFastPath)(call.receiver, debugMessage)

        } else {
          TauTest.log(s"- failed to instrument fast path (target is not really virtual or host is interface): ${devirtTarget.getFullName}")
        }
      }
    }
  }

  /** Transform invokeinterface to guarded light interf call with old invoke as backup path.
    * <br/>
    * <pre>
    *   ...
    *   rcv.interfCall(args);
    *   ...
    * </pre>
    * transforms to
    * <pre>
    *   ...
    *   if (tauTest(rcv)) {
    *     rcv.lightInterfCall(args);
    *   } else {
    *     rcv.interfCall(args);
    *   }
    *   ...
    * </pre>
    */
  private def transformToGuardedLightInterfCall(call: Call, guard: OpenConeGuard, reason: String) = {
    assert(call.target.isInstanceOf[InvokeInterfaceTarget])

    // because of block splitting and new nodes generation
    requireNoGlobalCodeMotion()

    count(s"guarded light interf call ($reason)")

    val Seq(deinterfInvoke) = replaceByMultiDiamondWithFastPaths(call, call.receiver, Static)(guard)(() =>
      createInvoke(MAK.INTERFACE, call.targetRef.method, call)
    )

    workaroundForJET13686(call)

    TauTest.log(s"- deinterf $guard ($reason)")
    val msg = s"[Deinterf]\n codeUnit=$codeUnit.\n invoke=${call.name}"
    markTauBackupPath(call, call.receiver, guard, msg)

    transformInvokeToLight(deinterfInvoke, guard.root)
  }

  /** Move invoke target to original call after inserting diamond between call and its target.
    * TODO: remove this after final call targets rework.
    */
  def workaroundForJET13686(call: Call): Unit = {
    call.target match {
      case t: AnyInvokeTarget => t.inCtrl = call.inCtrl
      case t => shouldNotReachHere(t)
    }
  }
}
