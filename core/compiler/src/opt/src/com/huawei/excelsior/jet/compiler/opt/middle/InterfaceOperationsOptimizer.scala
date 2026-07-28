/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.util.{ScalaCollections, Worklist}

import scala.annotation.nowarn
import scala.collection.mutable

/**
 * Optimization for interface operations (interface calls, casts, instanceofs, enrichments).
 *
 * @author paul
 * @author cypok
 */
trait InterfaceOperationsOptimizer { self: Universe =>

  import PartialFunction.cond

  object CIAOOptimizer {

    private sealed abstract class CIAO
    private case class Optimized(value: Node) extends CIAO
    private case object Unoptimizable extends CIAO
    private sealed abstract class Pending extends CIAO
    private case class PendingDirect(stub: Proxy) extends Pending
    private case object PendingTransitive extends Pending

    @nowarn("msg=match may not be exhaustive")
    private def optimizeCIAOForPhi(ciaoForPhies: mutable.Map[Phi, CIAO], itype: symlevel.ClassType, phi: Phi, point: ControlNode, forcePendingPhies: Boolean): CIAO = {
      // So it's time to describe what's happening here.
      //
      // If there are no loops the algorithm is straightforward:
      // 1) recursively optimize CIAO for phi's joined objects
      // 2) if there is at least one optimized then create parallel phi-function for CIAO
      //    using simple WeakCasts for all unoptimizable arguments
      //    and treat created phi as optimized CIAO
      // 4) otherwise state that CIAO for this phi is unoptimizable
      //
      //
      // However in case of cyclic dependencies between phies everything becomes (a lot) more complicated.
      //
      // We maintain mapping (marks) for all visited phies which contains the state of this phi.
      // (Note that this mapping is shared during optimization of CIAO for the same interface type.
      // Thus optimized/unoptimizable marks could save our efforts.)
      //
      // Once we visit new phi we mark it as PendingDirect(stubCIAO).
      // Cyclic visit of the same phi will pessimistically treat it like "unoptimizable".
      // Corresponding stub is used if there are other optimized args and we need to create parallel phi.
      // In later case during backward recursive steps we will create parallel phies for all stubbed phies
      // and replace stubs by real phies.
      //
      // Moreover we specially handle unoptimizable phies which have at least one pending phi as argument.
      // Such phies are marked as PendingTransitive.
      // In case of parallel phi creation such phies are processed one more time with forced creation of parallel phies
      // for all pending phies: this is profitable because we already know that this cycle is valuable.

      (ciaoForPhies get phi, forcePendingPhies) match {
        case (Some(ciao @ (_: Optimized | Unoptimizable | _: PendingDirect)), _) =>
          // This phi was already processed. We cannot do anything else.
          ciao

        case (Some(PendingTransitive), false) =>
          // This phi was already processed. We cannot do anything else (unless forcePendingPhies is enabled).
          PendingTransitive

        case (None, _) | (Some(PendingTransitive), true) =>
          // This phi wasn't processed earlier or we were asked to reprocess it treating pending phies as profitable.

          // No need to decommit the stub later because it's not committed.
          val stub = Proxy.raw(AddrIntType)(phi.block)
          ciaoForPhies(phi) = PendingDirect(stub)

          def optimizePhiEdge(force: Boolean)(e: Edge) = {
            optimizeCIAO(ciaoForPhies, itype, e.source, e.usePoint, forceTransitivePhies = force)
          }

          val potentialCIAOAbove = (phi.inEdges map optimizePhiEdge(forcePendingPhies)).toList
          val makePhi =
            // Immediate profit.
            (potentialCIAOAbove exists (_.isInstanceOf[Optimized])) ||
            // Cyclic profit only if it's required by caller.
            (forcePendingPhies && (potentialCIAOAbove exists (_.isInstanceOf[Pending])))

          val ciao = if (makePhi) {
            val joinedCIAO = phi.inEdges zip potentialCIAOAbove map {
              case (_, Optimized(value)) => value
              case (_, PendingDirect(stub)) => stub

              case (e, PendingTransitive) =>
                optimizePhiEdge(force = true)(e) match {
                  case Optimized(v) => v
                  case x => shouldNotReachHere(x)
                }

              case (Edge(arg, _), Unoptimizable) =>
                WeakCast(itype)(arg, WeakCast.NoCheck())
            }
            val ciaoPhi = Phi(AddrIntType)(phi.block +: joinedCIAO.toSeq: _*)

            stub.replaceBy(ciaoPhi)
            Optimized(ciaoPhi)

          } else {
            // If this phi is not optimized then the stub could not be used in any of phies above.
            assert(stub.uses.isEmpty)

            if (potentialCIAOAbove exists (_.isInstanceOf[Pending])) {
              PendingTransitive
            } else {
              Unoptimizable
            }
          }

          // Eventually every phi is either optimized to some value or is remembered as unoptimizable.
          ciaoForPhies(phi) = ciao
          ciao
      }
    }

    private def optimizeCIAO(ciaoForPhies: mutable.Map[Phi, CIAO], itype: symlevel.ClassType, obj: Node, point: ControlNode, forceTransitivePhies: Boolean): CIAO = {
      // First we try to perform simple local optimizations.
      val optimizedLocally = lightInterfCast(obj, itype, point) orElse
        (ContextTypesMap.findInterfaceTypeCheckForWeakCast(obj, itype, point) map (WeakCast(itype)(obj, _)))

      optimizedLocally match {
        case Some(v) => Optimized(v)
        case None =>
          // If there is no way to optimize locally we try to pull up evaluation of CIAO.
          // Currently only phies are used as motivation for pull up
          // however there could be other cases (e.g. see unit-test "weakcast: pull up to multiple casts")
          // but there were no real-world motivation for this (yet).
          obj match {
            case phi: Phi => optimizeCIAOForPhi(ciaoForPhies, itype, phi, point, forceTransitivePhies)
            case _ => Unoptimizable
          }
      }
    }

    private def optimizeWeakCast(ciaoForPhies: mutable.Map[Phi, CIAO], wc: WeakCast, point: ControlNode) = {
      optimizeCIAO(ciaoForPhies, asClassType(wc.targetType), wc.obj, point, forceTransitivePhies = false) match {
        case Optimized(ciao) => Some(ciao)
        case Unoptimizable | PendingTransitive => None
        case PendingDirect(_) => shouldNotReachHere()
      }
    }

    /** Optimizes WeakCasts which are not linked to some check above.
      * Heavily relies on the fact the WC could be rematerialized to use points and
      * might be optimized better (i.e. replaced by constant after class cast).
      * Similarly Enrich nodes could be rematerialized to use points so they are processed simultaneously.
      */
    def optimizeWeakCastsAndEnriches(): Boolean = withIncrementalGCM {
      val candidatesByType = ScalaCollections.groupBy(all[WeakCast] filterNot (_.hasDominatingCheck))(_.targetType)
      if (candidatesByType.isEmpty) {
        return false
      }

      var smthChanged = false
      for ((_, candidates) <- candidatesByType) {
        // See #optimizeCIAOForPhi for more information about this map.
        val ciaoForPhies = Maps[Phi].newMMap[CIAO]
        for (wc <- candidates) {
          // These are the points where WeakCast() or Enrich(_, WeakCast()) are actually used,
          // we will try to calculate CIAO at these points.
          val allUsePoints = wc.outEdges flatMap {
            case Edge(_, enrich: Enrich) => enrich.outEdges
            case e => Iterator.single(e)
          } map (_.usePoint)

          // These are the points where we have successfully optimized WeakCast() to something better.
          var replacedPoints: Map[ControlNode, Node] = Maps[ControlNode].newImmMap
          for {
            p <- allUsePoints
            if p != null && !(replacedPoints contains p)
            ciao <- optimizeWeakCast(ciaoForPhies, wc, p)
          }{
            replacedPoints += (p -> ciao)
          }

          // Perform the actual replacement of uses.
          if (replacedPoints.nonEmpty) {
            for (enrich <- collect[Enrich](wc.uses)) {
              enrich replaceUses {
                case e if replacedPoints contains e.usePoint => Enrich(wc.targetType)(wc.obj, replacedPoints(e.usePoint))
              }
            }
            wc replaceUses {
              case e if replacedPoints contains e.usePoint => replacedPoints(e.usePoint)
            }
            smthChanged = true
          }
        }
      }
      smthChanged
    }
  }


  /**
   * Replace `Enrich(Phi(..., Null | Deprive(x), ...))`
   * by `Phi(Enrich(...), x, Enrich(...))`.
   */
  private def pullUpEnriches(): Boolean = {

    def canReuseEnrichment(enrich: Enrich) = {
      val block = enrich.obj.asInstanceOf[Phi].block
      cond(enrich.enrichment) {
        case IntegralConst(_) => true
        case x: Phi => x.block dominates block
        case x: ControlNode => x dominates block
      }
    }

    /** Do not pull up Enrich if this may lead to heavy re-evaluation of enrichment */
    def allowEvalEnrichment(enrich: Enrich) = {
      val block = enrich.obj.asInstanceOf[Phi].block
      cond(enrich.enrichment) {
        case wc: WeakCast if !wc.hasDominatingCheck => true
        case wc: WeakCast => cond(wc.dominatingCheck) {
          case x: ControlNode => x dominates block
        }
      }
    }

    def hasProfit(e: Enrich)(arg: Node) = cond(arg) {
      case _: AnyNull => true
      case Deprive(t, _) if t == e.interfaceType => true
    }

    def isCandidate(enrich: Enrich) = cond(enrich.obj) {
      case phi: Phi if phi.args forall hasProfit(enrich) => true
      case phi: Phi if phi.args exists hasProfit(enrich) => canReuseEnrichment(enrich) || allowEvalEnrichment(enrich)
    }

    val worklist = Worklist.from(all[Enrich] filter isCandidate)
    if (worklist.isEmpty) {
      return false
    }

    val replaced = Maps[Enrich].newQMap[Node]
    // To process cyclic phies correctly we do work in two phases
    // Phase 1: construct all new phies and enriches; don't touch old ehrich's uses
    for (enrich @ Enrich(itype, obj: Phi, enrichment) <- worklist.accumulate) {

      def newEnrichment(e: Edge) = enrichment match {
        case _ if hasProfit(enrich)(e.source) => NoValue() // will be optimized out
        case x: Phi if x.block == obj.block => x.phiArg(obj.controlInput(e))
        case x if canReuseEnrichment(enrich) => x
        case _ =>
          assert(currentPhase <= CompilerPhase.Lowering)
          WeakCast(itype)(e.source, WeakCast.NoCheck())
      }

      val pulledUp = (obj.inEdges map { e => Enrich(itype)(e.source, newEnrichment(e)) }).toList
      replaced(enrich) = Phi(enrich.tpe)(obj.block +: pulledUp :_*)
      worklist ++= collect[Enrich](pulledUp) filter isCandidate
    }

    // Phase 2: replace all uses and remove old enriches
    for ((enrich, phi) <- replaced) enrich replaceBy phi
    true
  }

  /**
   * Replace `Deprive(Phi(..., Null | Enrich(x), ...))`
   * by `Phi(Deprive(...), x, Deprive(...))`.
   */
  private def pullUpDeprives(): Boolean = {

    def isCandidate(d: Deprive) = {
      def hasProfit(arg: Node) = cond(arg) {
        case _: AnyNull => true
        case Enrich(t, _, _) if t == d.interfaceType => true
      }

      cond(d.obj) { case phi: Phi => phi.args exists hasProfit }
    }

    val worklist = Worklist.from(all[Deprive] filter isCandidate)
    if (worklist.isEmpty) {
      return false
    }

    val replaced = Maps[Deprive].newQMap[Node]
    // To process cyclic phies correctly we do work in two phases
    // Phase 1: construct all new phies and deprives; don't touch old deprive's uses
    for (deprive @ Deprive(itype, obj: Phi) <- worklist.accumulate) {
      val pulledUp = (obj.inEdges map { e => Deprive(itype)(e.source) }).toList
      replaced(deprive) = Phi(deprive.tpe)(obj.block +: pulledUp :_*)
      worklist ++= collect[Deprive](pulledUp) filter isCandidate
    }
    // Phase 2: replace all uses and remove old deprives
    for ((deprive, phi) <- replaced) deprive replaceBy phi
    true
  }

  def optimizeInterfaceOperations(): Boolean = {
    var smthChanged = false
    smthChanged |= CIAOOptimizer.optimizeWeakCastsAndEnriches()
    smthChanged |= pullUpEnriches()
    smthChanged |= pullUpDeprives()
    smthChanged
  }

}
