/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Location.{AnyReg, MemBased, MemStatic}
import com.huawei.excelsior.jet.assembler.*
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.{ADDR64, DWARF_SECTION}
import com.huawei.excelsior.jet.assembler.fixups.{FixedSizeFixup, Relocation}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{addressSize, stackPointer}
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.DwarfEmitter.ExprLoc
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.AbbreviationsElements.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.DebugAbbrev.Abbreviation
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.DebugStr
import com.huawei.excelsior.jet.compiler.debug.info.Language
import com.huawei.excelsior.jet.compiler.debug.info.Language.{LANG_CPP_14, LANG_Java}
import xscala.io.LEB128Encoder.*
import xscala.util.MathUtils.{isNBits, isNBitsSigned}

import scala.collection.mutable

/** DWARF version 4 emitter.
  *
  * @author conwor
  * @author gatimosh
  * @author orangebyte256
  */
object DwarfEmitter {
  case class ExprLoc(base: Location, deref: Boolean)
}

class DwarfEmitter extends Emitter.WithSegment {

  /////////////////////////////////////////////////////////////////////////////
  // Common data emitter parts
  // TODO-DATA-EMITTER: consider extracting data emitter

  /** Closes `entry` and include its segment to this entry. */
  def include(entry: Dwarf.Entry): Unit = segment.append(entry.close())


  /////////////////////////////////////////////////////////////////////////////
  // Basic types
  // TODO-DATA-EMITTER: consider using some format-independent terminology and extracting data emitter

  def ubyte(x: Int): Unit = seg.putW8((x ensuring { isNBits(_, 8) }) & 0xFF)
  def uhalf(x: Int): Unit = seg.putW16((x ensuring { isNBits(_, 16) }) & 0xFFFF)
  def uword(x: Int): Unit = seg.putW32(x)

  def sbyte(x: Int): Unit = seg.putW8((x ensuring { isNBitsSigned(_, 8) }) & 0xFF)
  def shalf(x: Int): Unit = seg.putW16((x ensuring { isNBitsSigned(_, 16) }) & 0xFFFF)
  def sword(x: Int): Unit = seg.putW32(x)

  def addressSized(x: Long): Unit = addressSize match {
    case 4 => seg.putW32(x.toInt)
    case 8 => seg.putW64(x)
  }

  def uleb128(x: Int): Unit = seg.putULEB(x)
  def sleb128(x: Int): Unit = seg.putSLEB(x)

  def nullTerminatedString(str: XString): Unit = {
    for (x <- str.toPlatformBytes) sbyte(x)
    ubyte(0)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Fixups

  /** Generates 32-bit offset of `to` label from `from` label. */
  private def distance(from: Label, to: Label): Unit = addFixup(new FixedSizeFixup(4) {
    override def resolve(converter: Relocation.Converter): Unit = {
      assert(from.segment == to.segment)
      segment.setW32(position, to.position - from.position)
    }
  })

  /** Generates 32-bit offset of `target` label from byte after this 32-bit offset. */
  def initialLength(target: Label): Unit = {
    val start = newLabel
    distance(start, target)
    bind(start)
  }

  /** Generates 32-bit offset of `target` label from `entry` start. */
  def entryOffset(entry: Dwarf.Entry, target: Label): Unit = distance(entry.start, target)

  /** Generates 32-bit size in bytes of `entry` segment. */
  def sectionLength(entry: Dwarf.Entry): Unit = distance(entry.start, entry.end)

  /** Generates 32-bit offset of `target` label from it's section start. Will be finally resolved by linker. */
  def sectionOffset(target: Label): Unit = addFixup(new FixedSizeFixup(4) {
    override def resolve(converter: Relocation.Converter): Unit = target.segment.getSymbol match {
      case section: Dwarf.Section =>
        segment.setW32(position, target.position)
        converter.send(position, DWARF_SECTION, section)
      case s =>
        shouldNotReachHere(s"unexpected symbol for sectionOffset fixup: $s")
    }
  })

  /** Generates 32-bit offset of `entry` start from it's section start. */
  def sectionOffset(entry: Dwarf.Entry): Unit = sectionOffset(entry.start)

  /** Generates 64-bit fixup with absolute address of `target` + `offset`, which should be finally resolved by linker. */
  def address(target: Symbol, offset: Int = 0): Unit = {
    val targetName = DebugStr.label(linkageName(target))
    addFixup(new FixedSizeFixup(8) {
      override def resolve(converter: Relocation.Converter): Unit = {
        segment.setW32(position, targetName.position)
        segment.setW32(position + 4, offset)
        converter.send(position, ADDR64, target)
      }
    })
  }


  /////////////////////////////////////////////////////////////////////////////
  // Abbreviations

  private def emitAbbr(abbr: Abbreviation, params: Seq[Any]): Unit = {
    uleb128(abbr.index)

    val attributesWithParams = abbr.attributes filter { _.form.hasParams }
    assert(attributesWithParams.size == params.size)

    for ((attribute, param) <- attributesWithParams zip params) {
      attribute.form match {
        case DW_FORM_data1 => ubyte(param.asInstanceOf[Int])
        case DW_FORM_data2 => uhalf(param.asInstanceOf[Int])
        case DW_FORM_data4 => uword(param.asInstanceOf[Int])
        case DW_FORM_udata => uleb128(param.asInstanceOf[Int])

        case DW_FORM_string => nullTerminatedString(param.asInstanceOf[XString])

        case DW_FORM_strp => sectionOffset(DebugStr.label(param.asInstanceOf[XString]))

        case DW_FORM_addr => param match {
          case symbol: Symbol   => address(symbol)
          case mem: MemStatic   => address(mem.symbol, mem.disp)
        }

        case DW_FORM_sec_offset => param match {
          case entry: Entry => sectionOffset(entry)
          case label: Label => sectionOffset(label)
          case _ => shouldNotReachHere()
        }

        case DW_FORM_ref_addr => sectionOffset(param.asInstanceOf[Label])

        case DW_FORM_exprloc =>
          val (loc, deref) = param match {
            case loc: Location => (loc, false)
            case ExprLoc(loc, deref) => (loc, deref)
          }

          loc match {
            case reg: AnyReg =>
              val opcodesLen = if (deref) 2 else 1
              val code = DwarfRegEncodings(reg)
              if (code < 32) {
                ubyte(opcodesLen)
                ubyte(0x50 + code) // DW_OP_regN (0x50 + N)
              } else {
                ubyte(opcodesLen + calcSizeULEB128(code))
                ubyte(0x90) // DW_OP_regx
                uleb128(code)
              }
              if (deref) ubyte(0x06) // DW_OP_deref

            case based: MemBased if based.base == stackPointer =>
              val opcodesLen = if (deref) 2 else 1
              assert(based.base == stackPointer)
              ubyte(opcodesLen + calcSizeSLEB128(based.disp))
              ubyte(0x91) // DW_OP_fbreg
              sleb128(based.disp)
              if (deref) ubyte(0x06) // DW_OP_deref

            case static: MemStatic =>
              val opcodesLen = if (deref) 2 else 1
              ubyte(opcodesLen + addressSize)
              ubyte(0x03) // DW_OP_addr
              address(static.symbol, static.disp) // the address
              if (deref) ubyte(0x06) // DW_OP_deref

            case _ => shouldNotReachHere(s"unexpected location for DW_FORM_exprloc: $loc")
          }

        case f => shouldNotReachHere(s"unexpected abbreviation form: $f")
      }
    }
  }

  private def unwrapParamsSeq(params: Seq[Any]) = params match {
    case Seq(seq: Seq[Any]) => seq
    case seq => seq
  }

  def abbreviation(abbr: Abbreviation)(params: Any*): Unit = {
    assert(!abbr.hasChildren)
    emitAbbr(abbr, unwrapParamsSeq(params))
  }

  def abbreviationScope(abbr: Abbreviation)(params: Any*)(inner: => Unit): Unit = {
    assert(abbr.hasChildren)
    emitAbbr(abbr, unwrapParamsSeq(params))
    inner
    ubyte(0) // close abbreviation scope
  }
}

object DwarfLanguageEncodings {
  def encodeLang(lang: Language): Int = lang match {
    case LANG_CPP_14 => 0x21
    case LANG_Java   => 0x0b
  }
}

object DwarfRegEncodings {
  private lazy val encodings: collection.Map[AnyReg, Int] = {
    val map = new mutable.LinkedHashMap[AnyReg, Int]

    def append(seq: (AnyReg, Int)*): Unit = {
      val encodings = seq.map(_._2)
      assert(encodings.toSet.size == encodings.size)
      map ++= seq.toMap
    }

    // TODO-DWARF: remove dependencies of all architectures

    {
      // see System V Application Binary Interface AMD64 Architecture Processor Supplement
      import com.huawei.excelsior.jet.assembler.amd64.GPR._
      import com.huawei.excelsior.jet.assembler.amd64.XMM._
      append((RCX, 2), (RDX, 1), (RSP, 7), (RSI, 4), (RDI, 5), (R8, 8), (R9, 9),
        (XMM0, 17), (XMM1, 18), (XMM2, 19), (XMM3, 20), (XMM4, 21), (XMM5, 22), (XMM6, 23), (XMM7, 24))
    }

    {
      // see DWARF for the ARM Architecture
      import com.huawei.excelsior.jet.assembler.arm64.IRegister.X._
      import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.D._
      val encodedIRegs = Seq(X0, X1, X2, X3, X4, X5, X6, X7, SP) map (x => (x.asInstanceOf[AnyReg], x.encoding))
      val encodedFRegs = Seq(D0, D1, D2, D3, D4, D5, D6, D7) map (x => (x.asInstanceOf[AnyReg], 64 + x.encoding))
      append(encodedIRegs ++ encodedFRegs: _*)
    }

    map
  }

  def apply(r: AnyReg): Int = encodings(r)
}
