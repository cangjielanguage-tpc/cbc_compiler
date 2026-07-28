/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.{Stats, StatsKind}
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.options.BoolOption._
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.ArrayIndexCheckOptimizer
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales
import com.huawei.excelsior.jet.compiler.options.NumOption.VersionedLoopImpactPercentThreshold
import com.huawei.excelsior.jet.util.ScalaCollections._
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind}

import scala.PartialFunction.cond
import scala.collection.mutable.ArrayBuffer

trait ArrayIndexCheckLoopVersioning extends ArrayIndexCheckOptimizer with XiTransform with Scales { self: Universe =>

  private val impactRatioThreshold = env.valueOf(VersionedLoopImpactPercentThreshold).toDouble / 100.0

  private def loopVersioningEnabled = env.enabled(AICLoopVersioning) ||
    (env.enabled(AICLoopVersioningInPGOHosts) && profile.isPGOHost)

  /** Performs impact-driven loop versioning with the goal of removing [[ArrayIndexCheck]]s (and array [[NullCheck]]s).
    *
    * The unoptimized version is marked as cold, which also prevents repeated versioning.
    *
    * Impact ratio of a loop represents estimated profit from versioning and is calculated as `impact / weight`, where
    *   `impact` is a sum of all impact values of given loop's checks;
    *   `weight` is a weight of given loop without nodes that will become invariant after versioning.
    */
  def versionArrayIndexCheckLoops(collectFailStats: Boolean = false): Boolean = {
    if (!XiTransform.enabled) {
      return false
    }

    if (!loopVersioningEnabled) {
      return false
    }

    withArrayIndexCheckOptimizer { optimizer =>

      class VersioningCandidate(val loop: Loop[Block], checks: Seq[ArrayIndexCheck]) {
        assert(loop.kind != LoopKind.IRREDUCIBLE)

        private def loopInvariant(n: Node) = n.block strictDominates loop.header

        private def canBeVersioned(range: ValueRange): Boolean = range match {
          case range: ConstValueRange => 0 <= range.from && range.to < maxValue(range.tpe)
          case range: HalfSymbolicValueRange => 0 <= range.from && loopInvariant(range.to)
          case range: SymbolicValueRange => loopInvariant(range.from) && loopInvariant(range.to)
          case _: EmptyValueRange => false
        }

        private def canBeVersioned(check: ArrayIndexCheck): Boolean = {
          lazy val ranges = optimizer.ranges(check)
          loopInvariant(check.array) && ranges.nonEmpty && (ranges forall canBeVersioned)
        }

        /** Nodes that will be removed in optimized version of loop. */
        private lazy val impactNodes = {
          val impactNodes = Sets[Node].newQSet
          val versionedArrays = Sets[Node].newQSet

          def collectImpactNodes(check: ArrayIndexCheck): Unit = {
            assert(!check.trusted)
            val array = check.array
            versionedArrays += array
            impactNodes ++= array.valueUses filter {
              // After versioning under AIC predicate, all AIC with same array and index in loop will be removed as well.
              case x: ArrayIndexCheck if !x.trusted && x.idx == check.idx => loop.body contains x.block

              // AIC versioning predicate contains null test for array, so all NullChecks for that array in loop will be removed.
              case x: NullCheck if !x.trusted => loop.body contains x.block

              case _ => false
            }
            assert(impactNodes contains check) // sanity check
          }

          val (dominatingChecks, nonDominatingChecks) = checks.iterator filter canBeVersioned partition (dominatesLoopBackwardEdges(_, loop))

          // Checks that dominate all backward edges are always versioned and eliminated.
          dominatingChecks foreach collectImpactNodes

          // Other (non-dominating) checks are versioned only in PGO hosts with additional heuristic restrictions:
          if (env.enabled(NonDominatingAICLoopVersioningInPGOHosts) && profile.isPGOHost) {
            for (check <- nonDominatingChecks) {

              // 1. The array must not be versioned yet -- versioning AIC for already versioned array increases register pressure
              //      instead of reducing it (ask vitvit why).
              def validArray = env.enabled(NonDominatingAICLoopVersioningAllArrays) || !(versionedArrays contains check.array)

              // 2. The index must be actual inductive variable without any adjustments -- helps reduce predicate complexity
              //      and reduces register pressure in the rest of the loop.
              def validIndex = env.enabled(NonDominatingAICLoopVersioningAllIndices) || cond(check.idx) {
                case phi: Phi => optimizer.inductiveVariables contains phi
              }

              if (validArray && validIndex) {
                collectImpactNodes(check)
              }
            }
          }

          impactNodes
        }

        private def versionedLoopInvariant(n: Node): Boolean = n match {
          case n: ArrayLength => loopInvariant(n.array)
          case n: PureCheck => n.trusted
          case _ => (impactNodes contains n) || loopInvariant(n)
        }

        // Impact values were experimentally chosen during performance audit.
        // TODO: re-evaluate values when this optimization is enabled by default
        private def nodeImpact(n: Node): Int = n match {
          case _: ArrayIndexCheck => 30
          case _: NullCheck => 10
          case _ => shouldNotReachHere(n)
        }

        lazy val impact: Int = sumBy(impactNodes)(nodeImpact)

        lazy val weight: Double = nonInvariantLoopWeight(loop, versionedLoopInvariant)

        def impactChecks = collect[ArrayIndexCheck](impactNodes)

        def logFail(): Unit = log(success = false)

        def logSuccess(): Unit = log(success = true)

        private def log(success: Boolean): Unit = {
          val versioned = if (success) "versioned" else "not versioned"
          stats.count(StatsKind.XiTransformations, s"AIC loops $versioned", loop.header)
          if (XiTransform.log.isEnabled) {
            // TODO: write out bounds
            XiTransform.log(s"- loop $versioned", loop.header)
            XiTransform.log(s"  with impact ratio: $impact / $weight (${impact / weight})")
            for (n <- impactNodes) {
              XiTransform.log(s"  with impact node: ${n.name} [impact ${nodeImpact(n)}]", n)
            }
          }
        }

        def pred: PredicateConstructor =
          ArrayIndexCheckOptimizer.VersioningBound.joinedPredicate((impactChecks map optimizer.versioningBound).toSeq)

      }

      withIncrementalGCM {
        XiTransform.log.inSession("AIC loop versioning", codeUnit) {
          val candidates = ArrayBuffer.empty[VersioningCandidate]

          val cold = findColdBlocks()
          val checksByLoop = groupBy(all[ArrayIndexCheck] filterNot (_.trusted))(c => optimizer.loops.loopOf(c.block))
          for ((loop, checks) <- checksByLoop if loop != null && loop.kind != LoopKind.IRREDUCIBLE && !cold(loop.header)) {
            val candidate = new VersioningCandidate(loop, checks)
            val impact = candidate.impact
            val weight = candidate.weight
            if (impact > 0 && impact >= weight * impactRatioThreshold) {
              candidates += candidate
            } else if (collectFailStats) {
              candidate.logFail()
            }
          }

          if (candidates.nonEmpty) {
            val candidatesByEvidence = Maps[Block].newQMap[VersioningCandidate]
            xiTransformAndPostProcess { scheduler =>
              for (candidate <- minimalElements(candidates)(partialOrderingBy(_.loop isInnerOf _.loop))) {
                // Note that the "true" version is the one being optimized, so we need its pre-header as evidence
                val (_, versionedPreHeader, _) = scheduler.version(candidate.pred, candidate.loop)
                candidatesByEvidence(versionedPreHeader) = candidate
                candidate.logSuccess()
              }
              assert(candidatesByEvidence.nonEmpty)
            } { (xi, _) =>
              for ((evidence, candidate) <- candidatesByEvidence) {
                val impact = candidate.impact
                val weight = candidate.weight
                for (check <- candidate.impactChecks) {
                  optimizer.strikeOutCheck(xi.copyOf(check), evidence,
                    s" because it was versioned with impact ratio: $impact / $weight (${impact / weight})")
                }
                // Note that this also required to prevent repeated versioning.
                candidate.loop.header.markAsCold()
              }
            }
          }

          candidates.nonEmpty
        }
      }
    }
  }

}
