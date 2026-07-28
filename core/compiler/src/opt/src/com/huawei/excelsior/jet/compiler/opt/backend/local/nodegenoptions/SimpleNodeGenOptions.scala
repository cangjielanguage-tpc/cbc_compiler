/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.local.nodegenoptions

import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement

trait SimpleNodeGenOptions { self: Universe with BackEnd with NodeGenOptionsComponent =>

  trait SimpleNodeGenOptionsImpl { self: LocalGeneratorImpl with NodeGenOptionsImpl =>

    /** Generation options for nodes with result or single attached result, but without spoiled resources. */
    class SimpleNodeGenOptions(node: Node, normalized: Boolean) extends NodeGenOptions(node, normalized) {
      override val results: Seq[Allocation] = {
        val target = singleElement(node.groupResults)
        Seq(select(resultCandidates(target)(state.resources), target, emptySet))
      }

      override def spoiled: Seq[Allocation] = Seq.empty
    }


    /** Generation options for nodes with all resources fully determined (e.g. calls). */
    class CallGenOptions(node: Node, normalized: Boolean) extends NodeGenOptions(node, normalized) {
      private val _results = (node.groupResults map (r => resultCandidates(r)(state.resources))).toSeq
      private val _spoiled = spoiledResourcesSets(node)

      private var touched = {
        val sets = _results ++ _spoiled
        assert(sets.forall(_.isSingleton))
        unionOf(sets) ensuring (_.size == sets.size)
      }

      private def allocate(resource: Resource): Allocation = {
        val allocation = select(setOf(resource), null, untouchable = touched)
        touched |= allocation.touchedResources
        allocation
      }

      override val results: Seq[Allocation] = _results.map(s => allocate(s.head))
      override val spoiled: Seq[Allocation] = _spoiled.map(s => allocate(s.head))
    }
  }
}
