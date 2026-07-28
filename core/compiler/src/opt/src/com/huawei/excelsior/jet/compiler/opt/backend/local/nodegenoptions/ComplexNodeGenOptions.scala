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

import scala.collection.mutable.ArrayBuffer

trait ComplexNodeGenOptions { self: Universe with BackEnd with NodeGenOptionsComponent =>

  trait ComplexNodeGenOptionsImpl { self: LocalGeneratorImpl with NodeGenOptionsImpl =>

    /** Generation options for nodes with any resources combination. */
    class ComplexNodeGenOptions(node: Node, normalized: Boolean) extends NodeGenOptions(node, normalized) {

      /** Wrapper for candidates set to make allocation choices in any order. */
      private class Wrapper(val target: Node, var candidates: ResourceSet) {
        var allocation: Allocation = _
      }

      private val resultsWrappers = (node.groupResults map { r => new Wrapper(r, resultCandidates(r)(state.resources)) }).toSeq
      private val spoiledWrappers = spoiledResourcesSets(node) map (new Wrapper(null, _))

      {
        var touched = emptySet

        val wrappers = ArrayBuffer.from(resultsWrappers) ++= spoiledWrappers
        val (singles, others) = wrappers.partition(w => w.candidates.isSingleton)

        val singlesSet = unionOf(singles map (_.candidates))
        assert(singlesSet.size == singles.size)

        touched |= singlesSet
        for (wrapper <- others) {
          wrapper.allocation = select(wrapper.candidates &~ touched, wrapper.target, touched)
          touched |= wrapper.allocation.touchedResources
        }

        for (wrapper <- singles) {
          wrapper.allocation = select(wrapper.candidates, wrapper.target, touched)
          touched |= wrapper.allocation.touchedResources
        }
      }

      override val results: Seq[Allocation] = resultsWrappers map (_.allocation)
      override val spoiled: Seq[Allocation] = spoiledWrappers map (_.allocation)
    }
  }
}
