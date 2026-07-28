/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.{Stats, StatsKind}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales
import com.huawei.excelsior.jet.compiler.opt.middle.{CountedLoopsRecognizer, ValueRangeAnalysis}
import com.huawei.excelsior.jet.compiler.options.BoolOption.FullLoopUnrolling
import com.huawei.excelsior.jet.compiler.options.NumOption._
import com.huawei.excelsior.jet.util.ScalaCollections.{minimalElements, partialOrderingBy, singleton}
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.graph.Loop

/** Full loop unrolling optimization.
  *
  * Fully unrolls loops with known constant bounds and limited body size.
  *
  * Originally introduced for optimization of Caffeine.Float benchmark.
  *
  * @author liontiger
  */
trait FullLoopUnrolling extends XiTransform with CountedLoopsRecognizer with ValueRangeAnalysis with Scales { self: Universe =>

  def fullyUnrollLoops(collectFailStats: Boolean = false): Boolean = {
    if (!XiTransform.enabled(FullLoopUnrolling)) {
      return false
    }

    val loops = cfg.loops
    if (loops.isEmpty) {
      return false
    }

    withIncrementalGCM {
      XiTransform.log.inSession("full loop unrolling", codeUnit) {

        def potentialCandidate(loop: Loop[Block]): Option[FullUnrollingCandidate] = {
          val constIterNum = singleton(findInductiveVariables(loop)) flatMap calcValueRangeOfInductiveVariable collect {
            case range: ConstValueRange => (range.size, range.evidence)
          }

          // Ignore loops with non-constant bounds (without logging).
          if (constIterNum.isEmpty) {
            return None
          }

          val (iterNum, evidence) = constIterNum.get

          // Trivial fully unrollable loop:
          // - iterates only once, and
          // - has no side effects before loop exit.
          val trivial =
            iterNum == 1 &&
              evidence.isInstanceOf[Branch.Exit] &&
              evidence.block == loop.header &&
              loop.header.spine.forall(SpinalNode.sideEffectFree)

          lazy val weight = if (trivial) {
            nonInvariantLoopWeight(loop, n => evidence dominates n.block)
          } else {
            nonInvariantLoopWeight(loop)
          }

          def candidate = FullUnrollingCandidate(loop, iterNum, weight, trivial)

          if (trivial ||

            (loop.depth >= env.valueOf(FullyUnrollableLoopMinDepth) &&
              iterNum <= env.valueOf(FullyUnrollableLoopIterNumLimit) &&
              weight <= env.valueOf(FullyUnrollableLoopBodySizeLimit)) ||

            (iterNum <= env.valueOf(FullyUnrollableLoopIterNumLimitInTrials) &&
              trials.opportunisticOptimizationAllowed())) {

            Some(candidate)

          } else {
            if (collectFailStats) {
              candidate.logFail()
            }
            None
          }
        }

        val candidates = loops.iterator flatMap potentialCandidate
        if (candidates.isEmpty) {
          return false
        }

        // Collect only innermost candidates to guarantee that they do not overlap (needed for xiTransform).
        // Note that other candidates will still be optimized on the following optimization iterations.
        val innermostCandidates = minimalElements(candidates)(partialOrderingBy(_.loop isInnerOf _.loop))
        if (innermostCandidates.isEmpty) {
          return false
        }

        innermostCandidates foreach (_.logSuccess())

        val enterEdges = Sets[Edge].newQSet
        xiTransformAndPostProcess { scheduler =>
          for (FullUnrollingCandidate(loop, iterNum, _, _) <- innermostCandidates) {
            // We need to create an exclusive pre-header for each loop,
            // because initial pre-header may reside int another loop body,
            // and if that loop is also fully unrolled then enterEdges would be invalidated
            // (see unit-test for JET-12147).
            val (preHeader, _) = getOrCreateLoopPreHeader(loop)
            Block.splitBefore(preHeader.blockEnd)

            enterEdges ++= loopEnterEdges(loop)
            scheduler.unroll(loop, Math.toIntExact(iterNum))
          }
        } { (_, _) =>
          for (FullUnrollingCandidate(loop, _, _, _) <- innermostCandidates) {
            val header = loop.header.asInstanceOf[BBlock]
            val backEdges = header.inEdges filterNot enterEdges
            val haltBlock = BBlock.extractInputEdges(header, backEdges.toSeq)
            replaceByHalt(haltBlock.blockEnd)
          }
        }

        true
      }
    }
  }

  private case class FullUnrollingCandidate(loop: Loop[Block], iterNum: Long, weight: Double, trivial: Boolean) {

    def logFail(): Unit = log(loop, success = false)

    def logSuccess(): Unit = log(loop, success = true)

    private def log(loop: Loop[Block], success: Boolean): Unit = {
      val unrolled = if (success) "unrolled fully" else "not unrolled fully"
      val kind = if (trivial) "trivial " else ""
      stats.count(StatsKind.XiTransformations, s"loops $unrolled", loop.header)
      if (XiTransform.log.isEnabled) {
        XiTransform.log(s"- ${kind}loop $unrolled", loop.header)
        XiTransform.log(s"  with iterNum = $iterNum, weight = $weight, depth = ${loop.depth}")
      }
    }
  }

}
