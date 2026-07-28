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
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer.{LivenessMark, Mark, UsageMark}
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer.LivenessMark.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer.UsageMark.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.FlowAnalyzer
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.FlowAnalyzer.Resource

import scala.collection.mutable

object LivenessAnalyzer {
  sealed trait Mark(val usage: Boolean)

  private[cbc] enum LivenessMark extends Mark(usage = false) {
    case REF, REC, PRIM
  }

  private[cbc] enum UsageMark extends Mark(usage = true) {
    case USE_ANY, USE_REF, USE_REC, USE_PRIM

    def acceptable(from: LivenessMark) = this match {
      case USE_PRIM => from == PRIM
      case USE_REC => from == REC
      case USE_REF => from == REF
      case USE_ANY => true
    }
  }
}

class LivenessAnalyzer(strict: Boolean = true) extends FlowAnalyzer {
  private case class BlockState(entryState: Map[Resource, LivenessMark])

  private val instructionState = mutable.Set.empty[(Resource, Mark)]
  private var inOp = false

  private val currentState = mutable.LinkedHashMap.empty[Resource, LivenessMark]
  private var currentBlockState = BlockState(currentState.toMap)
  private val entryStates = mutable.LinkedHashMap.empty[Label, BlockState]

  override def ref[T <: Resource](arg: T): T = mark(arg, LivenessMark.REF)
  override def rec[T <: Resource](arg: T): T = mark(arg, LivenessMark.REC)
  override def prim[T <: Resource](arg: T): T = mark(arg, LivenessMark.PRIM)
  override def useRef[T <: Resource](arg: T): T = mark(arg, UsageMark.USE_REF)
  override def useRec[T <: Resource](arg: T): T = mark(arg, UsageMark.USE_REC)
  override def usePrim[T <: Resource](arg: T): T = mark(arg, UsageMark.USE_PRIM)
  override def useAny[T <: Resource](arg: T): T = mark(arg, UsageMark.USE_ANY)

  // RESOURCES MANAGEMENT

  private[cbc] def mark[T <: Resource](arg: T, mark: Mark): T = { instructionState.add((arg, mark)).ensuring(inOp); arg }
  
  override def trans[T <: Resource, U <: Resource](to: T, from: U): Unit = {
    useAny(from) // mark usage for a last use checking
    mark(to, currentState(from))
  }

  private def lastUse[T <: Resource](arg: T): Boolean = instructionState.exists((r, m) => r == arg && m.usage)

  private def use[T <: Resource](arg: T, mark: UsageMark): Unit = {
    assert(arg == IR.IRZ || mark.acceptable(currentState(arg)), s"Resource liveness check failed for $arg with $mark")
  }

  private def define[T <: Resource](arg: T, mark: LivenessMark): Unit = {
    assert(arg != IR.IRZ)
    currentState.put(arg, mark).ensuring(!strict || _.isEmpty || lastUse(arg), s"Resource liveness define failed for $arg with $mark")
  }

  override def op[T](action: => T): T = {
    instructionState.clear()

    assert(!inOp)
    inOp = true

    val result = action
    instructionState.collect { case (arg: Resource, use: UsageMark) => (arg, use) } foreach use
    instructionState.collect { case (arg: Resource, use: LivenessMark) => (arg, use) } foreach define

    inOp = false
    result
  }

  override def dead(arg: Resource): Unit = currentState.remove(arg).ensuring(_.isDefined && !inOp)

  def state: collection.Map[Resource, LivenessMark] = currentState

  // BLOCKS MANAGEMENT

  private def fork(): BlockState = BlockState(currentState.toMap)
  private def checkState(that: BlockState): Boolean =
    currentState.keySet.forall(that.entryState.contains) &&
      that.entryState.forall((r, mark) => currentState(r) == mark)

  override def branch(label: Label): Unit = {
    val target = entryStates.getOrElseUpdate(label, fork()).ensuring(checkState)

    val newBlock = fork()

    currentBlockState = newBlock
  }

  override def merge(label: Label): Unit = {
    val target = entryStates.getOrElseUpdate(label, fork()).ensuring(checkState)

    currentBlockState = target
  }
}
