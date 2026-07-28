/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.Width.{W16, W8}
import com.huawei.excelsior.jet.assembler.arm64.IRegister.W.WZR
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.XZR
import com.huawei.excelsior.jet.assembler.arm64.MemAtomicOp.nameModifier
import com.huawei.excelsior.jet.assembler.arm64.MemoryOrdering.*

/** aarch64/instrs/memory/memop/MemAtomicOp
  *
  * @author paul
  */
enum MemAtomicOp {
  case ADD, BIC, EOR, ORR
  case SMAX, SMIN, UMAX, UMIN
  case SWP

  def o3 = ordinal >> 3
  def opc = ordinal & 0x7 // TODO: 0b111

  private def baseName(st: Boolean) = {
    val prefix = if (st) "ST" else "LD"
    this match {
      case SWP => this.toString
      case BIC => prefix + "CLR"
      case ORR => prefix + "SET"
      case _ => s"$prefix$this"
    }
  }

  def format(ord: MemoryOrdering, width: Width, rt: IRegister) = {
    val st = (this != SWP) && ((rt == WZR) || (rt == XZR)) && (ord.a == 0)
    val args = if (st) " Rs, [Xn|SP]" else " Rs, Rt, [Xn|SP]"
    s"${baseName(st)}${nameModifier(ord, width)}$args"
  }
}

object MemAtomicOp {
  def nameModifier(ord: MemoryOrdering): String = ord match {
    case NONE => ""
    case ACQUIRE => "A"
    case RELEASE => "L"
    case ACQUIRE_RELEASE => "AL"
  }

  def nameModifier(width: Width): String = width match {
    case W8 => "B"
    case W16 => "H"
    case _ => ""
  }

  def nameModifier(ord: MemoryOrdering, width: Width): String = s"${nameModifier(ord)}${nameModifier(width)}"
}
