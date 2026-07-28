/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.options.BoolOption._
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.XiTransform
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.Loops

/** Optimizer of CFG crossroads.
  *
  * @author liontiger
  */
trait CrossroadsOptimizer extends XiTransform { self: Universe =>

  def optimizeSpecializedCrossroads(): Boolean = {
    // Note: this optimization may work before and after lowering
    if (!env.enabled(CrossroadsOptimizer)) {
      return false
    }

    if (all[Branch].isEmpty) {
      return false
    }

    val loops = cfg.loops

    withIncrementalGCM {

      object SpecializedCrossroad {
        def unapply(crossroad: Branch): Option[collection.SeqMap[Branch.Exit, Seq[Edge]]] = {
          val block = crossroad.block

          def isLoopHeader(b: Block) = {
            val loop = loops.loopOf(block)
            loop != null && loop.header == b
          }

          def noPhies = block.phies.isEmpty
          def multiplePhies = block.phies.size > 1
          def moreThanTwoPredecessors = block.predBlocks.size > 2

          /** Note: any optimization of loop backward or forward branches is prohibited due to potential issues:
            *
            *   - JET-11244: crossroads optimization of loop header may lead to counted loops becoming uncounted;
            *
            *   - JET-11237: uncontrollable crossroads optimization of some loop headers
            *                can lead to full unrolling of that loop and potentially endless compilation,
            *                which is cool in theory, but there are better tools for it
            *                (e.g. [[com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.LoopPeeling LoopPeeling]]);
            *
            *   - JET-11238: crossroads optimization of loop backward and forward branches leads to complicated
            *                and potentially enormous phi-function graphs, which are harder to analyse and generate.
            */

          def loopHeader = isLoopHeader(block)
          def backwardBranch(exit: Branch.Exit) = isLoopHeader(exit.target)

          if (noPhies || loopHeader) {
            return None
          }

          if (!env.enabled(UnleashCrossroadsOptimizer)) {
            if ((multiplePhies && moreThanTwoPredecessors) || !sideEffectFreeSpine(block)) {
              return None
            }
          }

          // Note: nodes should be collected outside of temp scope, because dominators can't handle it
          val nodes = Block.collectNodes(block)

          def multipleCmp = collect[Cmp](nodes).size > 1
          if (env.enabled(MultiCmpWorkaroundForCrossroadsOptimizer) && multipleCmp) {
            return None
          }

          def cloneWith(edge: Edge) = {
            def clone(n: Node): Node = n match {
              case phi: Phi if nodes contains phi => phi.phiArg(edge)
              case n: FloatingNode if nodes contains n => Node.clone(n, clone _)
              case _ => n
            }
            clone(crossroad.selector)
          }

          val pairs = inTempScope {
            for {
              edge <- block.inEdges.toList
              exit <- crossroad.constExit(cloneWith(edge))
              if !backwardBranch(exit)
            } yield (exit, edge)
          }
          if (pairs.isEmpty) None else Some(ScalaCollections.toMultiMap(pairs))
        }
      }

      var changed = false

      // If blocks have unreachable input edges, applying CrossroadsOptimizer to them may cause problems (JET-12806).
      if (eliminateUnreachableCode()) {
        dbgPrinter.debugNodes("All graph after UCE")
        changed = true
      }

      for (crossroad @ SpecializedCrossroad(exitToInEdges) <- all[Branch]; (exit, edges) <- exitToInEdges) {
        xiTransformAndPostProcess { scheduler =>
          scheduler.extract(crossroad.block, edges: _*)
        } { (xi, _) =>
          replaceByGoto(xi.copyOf(exit))
          stats.count(StatsKind.CrossroadsOptimization, "specialized", crossroad)
          changed = true
        }
      }
      changed
    }
  }

  private def sideEffectFreeSpine(block: Block) = block.spine forall {
    case _: Marker => true // ok because markers should not affect optimization
    case _: MemBarrier => true // ok because runtime engineers require optimization of such patterns (e.g. sync)
    case _ => false
  }
}