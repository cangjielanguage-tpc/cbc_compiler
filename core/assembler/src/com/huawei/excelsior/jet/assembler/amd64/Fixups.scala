/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.Width.W32
import com.huawei.excelsior.jet.assembler.Width.W8
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.CODE_OFFS32
import com.huawei.excelsior.jet.assembler.{Fixup, Label, Symbol}
import com.huawei.excelsior.jet.assembler.fixups.Alignment
import com.huawei.excelsior.jet.assembler.fixups.ControlFixup
import com.huawei.excelsior.jet.assembler.fixups.Relocation
import xscala.util.MathUtils.isNBitsSigned

/** AMD64-specific fixups.
  *
  * @author ijorch
  * @author conwor
  */
private[amd64] object Fixups {

  private val MaxInstructionLength = 15

  private val NOPS = Array(
    Array(0x90),                                                      // nop
    Array(0x66, 0x90),                                                // xchg ax, ax
    Array(0x0f, 0x1f, 0x00),                                          // nop DWORD PTR [rax]
    Array(0x0f, 0x1f, 0x40, 0x00),                                    // nop DWORD PTR [rax+0x0]
    Array(0x0f, 0x1f, 0x44, 0x00, 0x00),                              // nop DWORD PTR [rax+rax*1+0x0]
    Array(0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00),                        // nop WORD PTR [rax+rax*1+0x0]
    Array(0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00),                  // nop DWORD PTR [rax+0x0]
    Array(0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00),            // nop DWORD PTR [rax+rax*1+0x0]
    Array(0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00),      // nop WORD PTR [rax+rax*1+0x0]
    Array(0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00) // nop WORD PTR cs:[rax+rax*1+0x0]
  )

  final class CodeAlignment(_align: Int) extends Alignment(_align) {
    override protected def resolveImpl(): Unit = {
      var count = size
      var pos = position
      // place as much `MaxInstructionLength`-byte NOPs as fits and then finish with a NOP of suitable length
      while (count != 0) {
        var len = count min MaxInstructionLength
        count -= len

        while (len > NOPS.length) {
          segment.setByte(pos, 0x66) // add some `data16` prefixes to make longer instruction
          pos += 1
          len -= 1
        }
        segment.setBytes(pos, NOPS(len - 1))
        pos += len
      }
    }
  }

  final class Branch(_target: Label, cc: CC, canBeShort: Boolean)
    extends ControlFixup(_target, canBeShort, 2, 6) {

    override protected[assembler] def expectedSize = if (isNBitsSigned(targetDistance - 2, 8)) 2 else 6

    override def resolve(converter: Relocation.Converter): Unit = (size: @unchecked) match {
      case 2 =>
        segment.setByte(position, 0x70 + cc.code)
        segment.setSigned(W8, position + 1, targetDistance - size)
      case 6 =>
        segment.setByte(position, 0x0f)
        segment.setByte(position + 1, 0x80 + cc.code)
        segment.setSigned(W32, position + 2, targetDistance - size)
    }

    override protected def guts = Fixup.seq(target, cc)
  }

  final class Jump(_target: Symbol, link: Boolean, canBeShort: Boolean)
    extends ControlFixup(_target, canBeShort && !link && _target.isInstanceOf[Label], 2, 5) {

    override protected[assembler] def expectedSize = if (isNBitsSigned(targetDistance - 2, 8)) 2 else 5

    override def resolve(converter: Relocation.Converter): Unit = {
      val offset = if (isLocal) targetDistance - size else -4

      (size: @unchecked) match {
        case 2 =>
          assert(isLocal && !link)
          segment.setByte(position, 0xeb)
          segment.setSigned(W8, position + 1, offset)
        case 5 =>
          segment.setByte(position, if (link) 0xe8 else 0xe9  )
          segment.setSigned(W32, position + 1, offset)
      }

      if (!isLocal) converter.send(position + 1, CODE_OFFS32, target)
    }

    override protected def guts = Fixup.seq(target, link)
  }
}
