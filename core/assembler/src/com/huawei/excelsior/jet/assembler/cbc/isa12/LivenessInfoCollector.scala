/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12

import com.huawei.excelsior.jet.assembler.{Label, Segment}
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.cbc.StackSlot
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer.LivenessMark
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.FlowAnalyzer.Resource
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector.{AllStates, LiveState, StackCheckState}

import scala.collection.mutable

object LivenessInfoCollector {
  case class LiveState(label: Label, regMask: Int, untypedSlots: Seq[Int], derivedPairs: Seq[(Int, Int)])
  case class StackCheckState(label: Label, stackPtrHolders: Seq[Int])

  case class AllStates(liveStates: Seq[LiveState], stackCheckStates: Seq[StackCheckState])

  def empty: AllStates = AllStates(Seq.empty, Seq.empty)
}

class LivenessInfoCollector {

  private val liveStates = mutable.ArrayBuffer.empty[LiveState]

  private val stackCheckStates = mutable.ArrayBuffer.empty[StackCheckState]

  private def resToIdx(res: Resource): Int = res match {
    case ireg: IR => ireg.idx
    case untyped: StackSlot.Untyped => IR.count + untyped.slot
  }
  
  def saveStates(seg: Segment, state: Seq[(Resource, LivenessMark)], dpairs: Seq[(Resource, Resource)] = Seq.empty): Unit = {
    assert(seg != null)
    
    val label = seg.newBoundLabel
    val regsMask = state.collect {
      case (r: IR, LivenessMark.REF) => 1 << r.idx
    }.sum
    val slots = state.collect {
      case (us: StackSlot.Untyped, LivenessMark.REF) => us.slot
    }
    val derivedPairs = dpairs.map((base, derived) => (resToIdx(base), resToIdx(derived)))
    liveStates.addOne(LiveState(label, regsMask, slots, derivedPairs))
  }

  def saveResources(seg: Segment, refResources: Seq[Resource], dpairs: Seq[(Resource, Resource)]): Unit =
    saveStates(seg, refResources.map(res => res -> LivenessMark.REF), dpairs)

  def saveStackPtrs(seg: Segment, resources: Seq[Resource]) = {
    assert(seg != null)

    val label = seg.newBoundLabel
    stackCheckStates.addOne(StackCheckState(label, resources.map(resToIdx)))
  }

  def collect: AllStates = AllStates(liveStates.toSeq, stackCheckStates.toSeq)

}
