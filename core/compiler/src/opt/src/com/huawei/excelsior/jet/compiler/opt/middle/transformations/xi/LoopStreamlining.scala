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
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.LoopsNormalizer
import com.huawei.excelsior.jet.compiler.options.BoolOption.LoopStreamlining
import com.huawei.excelsior.jet.compiler.options.NumOption.StreamlinedTauPercentThreshold
import com.huawei.excelsior.jet.util.ScalaCollections._
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind}

import scala.collection.mutable

/** Loop streamlining optimization.
  *
  * Loop streamlining eliminates cold paths from the loop,
  * thus enabling more aggressive code motion and loop invariant versioning.
  *
  * This is achieved by creating a copy of the loop and redirecting its cold branches to the original loop's cold paths.
  * The original loop's enter edges are redirected to the copy and its backward edges are redirected to the pre-header,
  * effectively turning the original loop into the outer loop for the copy.
  *
  * Because of this transformation inner loop no longer contains cold code, but instead has cold exits to the outer loop,
  * which acts as a backup path and finishes execution of the current iteration and then continues back to the optimized
  * inner loop.
  *
  * The former pre-header is transformed into a versioning point, which can accumulate invariant predicates from
  * the optimized loop.
  *
  * Example transformation with B & E as cold blocks (enclosed in square brackets):
  * {{{
  *                                  X
  *                                  | ________________
  *                                  |/                |
  *          X                       P                 |
  *   ______ |                 _____ |\_______         |
  *  |      \|                |     \|        \        |
  *  |       A                |      A'        A       |
  *  |      / \f              |     f|\____   / \f     |
  *  |     /   \              |      |     \ /   \     |
  *  |   [B]    C             |      C'    [B]    C    |
  *  |     \   /              |      |       \   /     |
  *  |      \ /               |      |        \ /      |
  *  |       D      ------>   |      D'        D       |
  *  |      / \t              |     t|\____   / \t     |
  *  |     /   \              |      |     \ /   \     |
  *  |   [E]    F             |      F'    [E]    F    |
  *  |     \   /              |      |       \   /     |
  *  |      \ /               |      |        \ /      |
  *  |       G                |      G'        G       |
  *  |______/|f               |_____/|f       f|\______|
  *          |                       |____ ____|
  *          Y                            |
  *                                       Y
  * }}}
  *
  * @author liontiger
  */
trait LoopStreamlining extends XiTransform with LoopsNormalizer { self: Universe =>

  private val tauRatioThreshold = env.valueOf(StreamlinedTauPercentThreshold).toDouble / 100.0

  def streamlineLoops(): Boolean = {
    if (!XiTransform.enabled(LoopStreamlining) || !GradientVersioningPoint.enabled) {
      return false
    }

    if (!profile.isPGOHost) {
      return false
    }

    val loops = cfg.loops
    if (loops.isEmpty) {
      return false
    }

    def validSelector(n: Node): Boolean = n match {
      case TauTest(_, TauInfo.Unknown, _) => false
      case TauTest(_, TauInfo.PGO(Seq(trueWeight), falseWeight), _) => trueWeight >= (trueWeight + falseWeight) * tauRatioThreshold
      case _ => true
    }

    val cold = findWarmAndColdBlocks()
    val candidatesByLoop = toMultiMap[Loop[Block], If.Exit](
      for {
        loop <- loops.iterator if loop.kind != LoopKind.IRREDUCIBLE && isNormalizedLoop(loop)
        if !(loop.header.spine exists (_.isInstanceOf[GradientVersioningPoint]))

        branch <- collect[If](loop.body filterNot cold map (_.blockEnd))
        if !collect[GradientVersioningPoint](loop.body.iterator.flatMap(_.spine)).exists(_.dominates(branch))
        if branch.succBlocks forall loop.body.contains // we only need diamonds in the same loop

        coldExit <- singleton(branch.exits filter (e => cold(e.target)))
        if validSelector(branch.selector)
      } yield loop -> coldExit
    )

    if (candidatesByLoop.isEmpty) {
      return false
    }

    withIncrementalGCM {
      XiTransform.log.inSession("loop streamlining", codeUnit) {

        val coldExits = mutable.ArrayBuffer.empty[If.Exit]
        val minCandidatesByLoop = minimalElements(candidatesByLoop)(partialOrderingBy(_._1 isInnerOf _._1))
        xiTransformAndPostProcess { scheduler =>
          for ((loop, candidates) <- minCandidatesByLoop) {
            val header = loop.header

            /////////////////////
            // Logging

            stats.count(StatsKind.XiTransformations, "loops streamlined", header)
            XiTransform.log("- loop streamlined", header)
            XiTransform.log("  with exits under:")
            def addColdExitWithLogging(coldExit: If.Exit): Unit = {
              val branch = coldExit.owner
              XiTransform.log(s"    ${branch.selector.name}", branch)
              coldExits += coldExit
            }

            /////////////////////
            // Preparation

            candidates foreach addColdExitWithLogging

            // Replace clinits by diamonds to provide more cold exits.
            for (clinit <- collect[Clinit](loop.body flatMap (_.spine)) if !cold(clinit.block)) {
              val clinitExit = Clinit.wrapUnderInitializedTest(clinit)
              addColdExitWithLogging(clinitExit)
            }

            // Replace all phies in loop header by vars to ensure that backup loop will continue iteration and not restart it.
            val (enterEdge, backwardEdge) = normalizedLoopHeaderEdges(loop)
            splitCriticalEdge(backwardEdge)
            splitCriticalEdge(enterEdge)
            header.phies.toList foreach replacePhiByVar

            // Note that the loop structure may be corrupted at this point, so we manually ensure pre-header existence.
            val preHeader = enterEdge.source.block
            val backupHeader = Block.splitBefore(preHeader.blockEnd).target
            val versioningPoint = insertCodeAfter(backupHeader)(GradientVersioningPoint())

            /////////////////////
            // Transformation

            // Note that loop.body is invalidated at this point, so we use anchor-based versioning directly.
            val (_, _, failBlock) = scheduler.version(PredicateConstructor.atom(versioningPoint), backupHeader.blockEnd, header.outCtrl)
            failBlock.markAsCold()
            scheduler.unsafe.redirect(backwardEdge, _ => backupHeader)
          }

        } { (xi, _) =>
          for (coldExit <- coldExits) {
            // Redirect copied cold exits to the backup version.
            val coldBlock = coldExit.target
            val copiedColdExit = xi.copyOf(coldExit)
            assert(coldBlock.phies.isEmpty)
            makeUnreachable(copiedColdExit.outEdge)
            coldBlock.addArg(copiedColdExit)
            // Ensure that it is actually cold.
            coldBlock.markAsCold()
          }
        }

        true
      }
    }
  }
}
