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
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector.LiveState

import scala.collection.mutable

object LivenessInfoCollector {
  case class LiveState(label: Label, regMask: Int, untypedSlots: Seq[Int])
}

class LivenessInfoCollector {

  private val liveStates = mutable.ArrayBuffer.empty[LiveState]

  def saveStates(seg: Segment, state: Seq[(Resource, LivenessMark)]): Unit = {
    assert(seg != null)

    val label = seg.newBoundLabel
    val regsMask = state.collect {
      case (r: IR, LivenessMark.REF) => 1 << r.idx
    }.sum
    val slots = state.collect {
      case (us: StackSlot.Untyped, LivenessMark.REF) => us.slot
    }
    liveStates.addOne(LiveState(label, regsMask, slots))
  }

  def saveResources(seg: Segment, refResources: Seq[Resource]): Unit =
    saveStates(seg, refResources.map(res => res -> LivenessMark.REF))

  def collect: Seq[LiveState] = liveStates.toSeq

}
