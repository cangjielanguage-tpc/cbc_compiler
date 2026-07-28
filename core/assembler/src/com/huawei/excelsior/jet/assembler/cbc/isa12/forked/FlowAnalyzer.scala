/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12.forked

import com.huawei.excelsior.jet.assembler.Label
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.cbc.{Register, StackSlot}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.FlowAnalyzer.Resource
import xscala.io.ByteBuffer

trait FlowAnalyzer {
  def op[T](action: => T): T
  def branch(label: Label): Unit
  def merge(label: Label): Unit

  def trans[T <: Resource, U <: Resource](to: T, from: U): Unit
  def ref[T <: Resource](arg: T): T
  def rec[T <: Resource](arg: T): T
  def prim[T <: Resource](arg: T): T
  def useRef[T <: Resource](arg: T): T
  def useRec[T <: Resource](arg: T): T
  def usePrim[T <: Resource](arg: T): T
  def useAny[T <: Resource](arg: T): T
  def dead(arg: Resource): Unit
}

object FlowAnalyzer {
  type Resource = IR | StackSlot.Untyped

  object Stub extends FlowAnalyzer {
    override def trans[T <: Resource, U <: Resource](to: T, from: U): Unit = {}
    override def op[T](action: => T): T = action
    override def merge(label: Label): Unit = {}
    override def branch(label: Label): Unit = {}
    override def ref[T <: Resource](arg: T): T = arg
    override def rec[T <: Resource](arg: T): T = arg
    override def prim[T <: Resource](arg: T): T = arg
    override def useRef[T <: Resource](arg: T): T = arg
    override def useRec[T <: Resource](arg: T): T = arg
    override def usePrim[T <: Resource](arg: T): T = arg
    override def useAny[T <: Resource](arg: T): T = arg
    override def dead(arg: Resource): Unit = {}
  }
}
