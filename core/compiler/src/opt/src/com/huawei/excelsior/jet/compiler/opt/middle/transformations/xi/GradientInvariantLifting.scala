/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.options.BoolOption.GradientInvariantLifting
import com.huawei.excelsior.jet.compiler.options.NumOption.StreamlinedTauPercentThreshold
import com.huawei.excelsior.jet.util.ScalaCollections.{minimalElements, partialOrderingBy, singleton, toMultiMap}

import scala.PartialFunction.condOpt

/** Appends ''gradient invariant predicates'' to the closest gradient versioning point,
  * effectively "lifting" them to that point.
  *
  * Gradient invariant predicate is the one that splits control flow into hot and cold paths.
  *
  * @author liontiger
  */
trait GradientInvariantLifting extends XiTransform { self: Universe =>

  private val tauRatioThreshold = env.valueOf(StreamlinedTauPercentThreshold).toDouble / 100.0

  def liftGradientInvariants(): Boolean = {
    if (!env.enabled(GradientInvariantLifting) || !GradientVersioningPoint.enabled) {
      return false
    }

    if (!profile.isPGOHost) {
      return false
    }

    if (all[GradientVersioningPoint].isEmpty || (all[GradientVersioningPoint] exists (_.branchOption.isEmpty))) {
      return false
    }

    withIncrementalGCM {

      case class Candidate(coldExit: If.Exit, _pred: PredicateConstructor) {
        def branch = coldExit.owner
        def hotExit = coldExit.otherExit
        def pred: PredicateConstructor = if (coldExit.isTrue) !_pred else _pred
      }

      def detectInvariant(branch: If, point: GradientVersioningPoint): Option[PredicateConstructor] = {
        def invariant(n: Node) = n.block strictDominates point.branch

        def validInfo(info: TauInfo) = info match {
          case TauInfo.Unknown => false
          case TauInfo.PGO(Seq(trueWeight), falseWeight) => trueWeight >= (trueWeight + falseWeight) * tauRatioThreshold
          case _ => true
        }

        branch.selector match {
          case test: TauTest if !validInfo(test.info) =>
            None

          case test: TauTest if invariant(test.obj) =>
            Some(PredicateConstructor.tauTest(test.guard, test.info, test.obj))

          case test if invariant(test) =>
            Some(PredicateConstructor.atom(test))

          case _ => None
        }
      }

      val sortedVersioningPoints = all[GradientVersioningPoint].filter(_.block.reachable).toSeq.sortBy(_.block)(cfg.topSort.reverse)
      def alreadyLifted(branch: If) = sortedVersioningPoints exists (p => p.dominates(branch) && branch.dominates(p.branch))
      def findVersioningPoint(branch: If): Option[GradientVersioningPoint] = sortedVersioningPoints find (_.hotExit dominates branch)

      lazy val cold = findWarmAndColdBlocks()
      val candidatesByPoint = toMultiMap[GradientVersioningPoint, Candidate](
        for {
          branch <- all[If] if !cold(branch.block) && branch.block.reachable
          coldExit <- singleton(branch.exits filter (e => cold(e.target)))
          if !alreadyLifted(branch)
          point <- findVersioningPoint(branch)
          pred <- detectInvariant(branch, point)
        } yield point -> Candidate(coldExit, pred)
      )

      if (candidatesByPoint.isEmpty) {
        return false
      }

      XiTransform.log.inSession("gradient invariant lifting", codeUnit) {
        for ((point, candidates) <- candidatesByPoint) {

          stats.count(StatsKind.XiTransformations, "gradient invariants lifted", point)
          if (XiTransform.log.isEnabled) {
            XiTransform.log("- gradient invariants lifted", point)
            XiTransform.log("  with invariants:")
            for (candidate <- candidates) {
              XiTransform.log(s"    ${candidate.branch.selector.name}", candidate.branch)
            }
          }

          val pred = candidates.iterator map (_.pred) reduce (_ && _)
          point.append(pred)
          for (candidate <- candidates) {
            replaceByGoto(candidate.hotExit)
          }
        }

        true
      }
    }
  }
}
