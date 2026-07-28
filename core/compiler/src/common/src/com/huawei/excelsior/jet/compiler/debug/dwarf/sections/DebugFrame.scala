/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.sections

import com.huawei.excelsior.common.Arch.{AMD64, ARM64}
import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.assembler.Location.{IReg, MemBased}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{addressSize, targetArch}
import com.huawei.excelsior.jet.compiler.debug.CodeRecord
import com.huawei.excelsior.jet.compiler.debug.dwarf.{Dwarf, DwarfRegEncodings}
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels.CallerFrameInfoLabel
import xscala.util.MathUtils.{isAligned, isNBits}

/** Container of the CIE and FDE (content of .debug_frame section).
  *
  * 6.4. Call Frame Information
  *
  * @author conwor
  */
object DebugFrame extends Dwarf.Section {
  val (codeAlignmentFactor, dataAlignmentFactor, returnAddressRegister) = targetArch match {
    case AMD64 => (1, -8, 16) // TODO-DWARF: replace this hardcode with ABI/Frame/Arch API
    case ARM64 => (1, -8, 30) // TODO-DWARF: replace this hardcode with ABI/Frame/Arch API
    case arch => notImplemented(s"CFI for $arch")
  }

  object CFA {
    def nop(): Unit = ubyte(0)

    def advanceLoc(delta: Int): Unit = {
      assert(isAligned(delta, codeAlignmentFactor))
      val encodedDelta = delta / codeAlignmentFactor
      if (isNBits(encodedDelta, 6)) {
        ubyte((0x1 << 6) | encodedDelta)
      } else {
        if (isNBits(encodedDelta, 8)) {
          ubyte(0x02)
          ubyte(encodedDelta)
        } else if (isNBits(encodedDelta, 16)) {
          ubyte(0x03)
          uhalf(encodedDelta)
        } else {
          ubyte(0x04)
          uword(encodedDelta)
        }
      }
    }

    def defCFA(base: IReg, offset: Int): Unit = {
      ubyte(0x0c)
      uleb128(DwarfRegEncodings(base))
      uleb128(offset)
    }

    def offset(regNumber: Int, offset: Int): Unit = {
      assert(isNBits(regNumber, 6))
      assert(isAligned(offset, dataAlignmentFactor.abs))
      ubyte((0x2 << 6) | regNumber)
      uleb128(offset / dataAlignmentFactor)
    }
  }

  // TODO-DWARF: make several CIE entries created on-demand
  val commonCIE = newBoundLabel

  val cieEnd = newLabel
  initialLength(cieEnd)                                 // length
  sword(-1)                                             // CIE_id
  ubyte(Dwarf.VERSION)                                  // version
  nullTerminatedString(XString.empty)                   // augmentation
  ubyte(addressSize)                                    // address_size
  ubyte(0)                                              // segment_size
  uleb128(codeAlignmentFactor)                          // code_alignment_factor
  sleb128(dataAlignmentFactor)                          // data_alignment_factor
  uleb128(returnAddressRegister)                        // return_address_register
  while (!isAligned(seg.length, addressSize)) CFA.nop() // padding TODO-DWARF (optimize FIE by encoding initial instructions here)
  bind(cieEnd)

  def makeFDE(record: CodeRecord): Unit = {
    val fdeEnd = newLabel

    initialLength(fdeEnd)           // length
    uword(0) // TODO-DWARF: introduce DWARF_SECTION fixup to DebugFrame and replace this hack with `sectionOffset(commonCIE)          // CIE_pointer`
    address(record.scope)           // initial_location TODO-DWARF: investigate how to use segment_size of CIE
    addressSized(record.seg.length) // address_range

    // instructions
    var position = 0
    for (label @ CallerFrameInfoLabel(callerSP, callerRA) <- record.callerFrameInfoLabels) {
      if (label.position != position) {
        assert(label.position > position)
        CFA.advanceLoc(label.position - position)
        position = label.position
      }
      val cfa = callerSP match {
        case loc: MemBased => CFA.defCFA(loc.base, loc.disp); loc
        case loc => notImplemented(s"cfa definition in $loc")
      }
      callerRA match {
        case loc: MemBased if loc.base == cfa.base => CFA.offset(returnAddressRegister, loc.disp - cfa.disp)
        case _: IReg => assert (targetArch == ARM64) // do not emit CFI instruction for initial ret addr at LR
        case loc => notImplemented(s"return address definition in $loc")
      }
    }

    // padding and close
    while (!isAligned(seg.length, addressSize)) CFA.nop()
    bind(fdeEnd)
  }
}