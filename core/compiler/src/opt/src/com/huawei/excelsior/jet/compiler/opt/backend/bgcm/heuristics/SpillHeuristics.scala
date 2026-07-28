/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm.heuristics

import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.UAI
import com.huawei.excelsior.jet.compiler.opt.backend.util.SieveUtil
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Worklist

trait SpillHeuristics extends SieveUtil { self: Universe with BackEnd with UAI =>

  import RegFile.*


  object Spill {

    /** Cache of nodes, that was spilled anywhere in already processed blocks, except cold blocks. */
    private val spilledInHot = Sets[Node].newQSet

    /** Set of nodes used only in cold code or in phi-function webs, used only in cold code. */
    private val usedOnlyInCold = Sets[Node].newQSet

    /** Set of nodes which rematerialization is zero-cost (like constants). */
    private val zeroCostRematerialized = Sets[Node].newQSet

    /** Collect information about spill in normal code */
    def register(node: Node): Unit = {
      spilledInHot += node
    }

    private var maxLoopDepth: Int = _

    def init(gcm: GCMEngine): Unit = {
      maxLoopDepth = gcm.loops.maxDepth

      for (node <- allNodes if node.producesValue && node.isGroupRoot) {
        def usedInHot(node: Node): Boolean = {
          val ws = Worklist.from(node.groupedValueUses)
          ws.track exists { x =>
            if (x.isInstanceOf[Phi]) ws ++= x.groupedValueUses
            !gcm.cold(x.block) && !x.isInstanceOf[Phi]
          }
        }

        if (!gcm.cold(node.block) && !usedInHot(node)) {
          usedOnlyInCold += node
        }

        if (zeroCostRematerialization(node)) {
          zeroCostRematerialized += node
        }
      }
    }

    trait LocalImpl { self: UpwardAI =>

      object LocalSpill {

        import Sieve.*

        private def currentLoop = currentState.loop

        /** Returns index of the outermost loop from current loop nest, for which `node` is outsider.
          * If there is no such loop, returns max loop depth. */
        private val outsiderIndex = { (node: Node) =>
          val i = currentLoop.outsidersList.indexWhere(_.live(node))
          if (i != -1) i else maxLoopDepth
        }

        private val basicSort = Sieve(
                          root(usedOnlyInCold)            ||
                      leaf      |     spilledInHot        )


        private val rules = Sieve(

                 root(zeroCostRematerialized)                                                                       ||

          basicSort          |                (outsiderIndex, maxLoopDepth + 1)                                     ||

                                  dup(basicSort, maxLoopDepth)     |      usedOnlyInCold                            ||

                                                                        leaf    |    currentLoop.isArgument _       ||

                                                                                     leaf    |    spilledInHot      )


        /** Returns `amount` nodes from current state without `exclude` which occupy a register of `file`
          * and are better to spill than other nodes. */
        def selectFrom(file: RegFile, amount: Int, exclude: collection.Set[Node]): Seq[Node] = {
          val candidates = currentState.registerNodesSet(file) &~ exclude
          assert(candidates.size >= amount, "not enough available registers to spill")

          rules.selectFrom(candidates, amount)
        }

      }
    }
  }
}