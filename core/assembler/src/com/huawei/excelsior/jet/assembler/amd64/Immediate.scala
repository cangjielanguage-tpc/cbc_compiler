/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.BYTE
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.DWORD
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.QWORD
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.WORD
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.ADDR32
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.ADDR64
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.BYTE_STR_32
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.OFFS32_IN_SEG
import com.huawei.excelsior.jet.assembler.Label
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.fixups.{Relocation, RelocationKind}

/** Immediate value for assembler instruction.
  * It may be simple number or immediate with fixup.
  *
  * @author paul
  * @author cypok
  */
abstract class Immediate {
  final def fitsTo(w: Width) = w >= smallestSize
  def smallestSize: Width
}

object Immediate {
  private[amd64] final class Value(val value: Long) extends Immediate {
    override def smallestSize = Immediate.smallestSize(value)
    override def toString = s"${value.toHexString}H"
  }

  private[amd64] final class Relocated private(val relocation: Relocation) extends Immediate {
    def this(kind: RelocationKind, target: Symbol) = this(new Relocation(kind, target))
    def this(kind: RelocationKind, target: Symbol, addend: Int) = this(new Relocation(kind, target, addend))

    def size = relocation.kind.width

    override def smallestSize = size
    override def toString = relocation.toString
  }

  def asImm(imm: Long) = new Value(imm)

  def smallestSize(x: Long): Width = {
    if (Byte.MinValue <= x && x <= Byte.MaxValue) BYTE
    else if (Short.MinValue <= x && x <= Short.MaxValue) WORD
    else if (Int.MinValue <= x && x <= Int.MaxValue) DWORD
    else QWORD
  }

  def fitsTo(value: Long, w: Width) = w >= smallestSize(value)

  ///////////////////////////////////////////////////
  //             Immediates with fixups
  ///////////////////////////////////////////////////

  def addr32(symbol: Symbol): Immediate = new Relocated(ADDR32, symbol)
  def addr32(symbol: Symbol, addend: Int): Immediate = new Relocated(ADDR32, symbol, addend)

  def addr64(symbol: Symbol): Immediate = new Relocated(ADDR64, symbol)
  def addr64(symbol: Symbol, addend: Int): Immediate = new Relocated(ADDR64, symbol, addend)

  def stringRef32(symbol: Symbol): Immediate = new Relocated(BYTE_STR_32, symbol)
  def offset32InSegment(label: Label): Immediate = new Relocated(OFFS32_IN_SEG, label)
}
