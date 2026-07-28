/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.*
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.addressSize
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.o2lib.be_386.CodeDefModule.Segment
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule as cd, opAttrsModule as at, opStdModule as std}
import com.huawei.excelsior.jet.compiler.o2lib.fe.pc.*
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, NumerateModule as Numerate, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule as js
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.*

import scala.collection.mutable

object opDefModule {

  def objectSize(o: pc.Symbol, needSegm: Boolean = false): Int = {
    val tsize: Int = o match {
      case o: pcO.StringTable => addressSize + addressSize * o.getLength // first field in string table length
      case o: pcO.StaticField => return o.size
      case o: pc.DataSymbol.Sized => o.size.getOrElse(-1)
      case _ => -1
    }

    if (o.ownsSegment) {
      val segm = at.getSegment(o)
      val size = segm.length
      assert((o eq at.currClass.typeHandle) || o.isInstanceOf[pcO.ModuleObject] || o.isInstanceOf[pcO.Method] || size == tsize || (o match {
        case o: pc.DataSymbol.Sized => o.size.isEmpty
        case _ => true
      }))
      size
    } else {
      o match {
        case o: pc.DataSymbol.Sized => assert(!needSegm && o.size.isDefined)
      }
      tsize
    }
  }

  def objectAlign(o: pc.Symbol, maxAlign: Int): Int = {
    val t_align: Int = o match {
      case _: pcO.StringTable => addressSize
      case o: pcO.StaticField => return o.alignment ensuring (a => (a <= maxAlign) && (a <= RTConst.HeapObj.alignment))
      case o: pc.DataSymbol.Sized if o.size.isDefined => 1 // It does not look right
      case _ => 0
    }

    var align: Int = 0

    if (o.ownsSegment) {
      val segm = at.getSegment(o)
      align = segm.alignment
    } else {
      align = 0
    }

    if (align != 0) {
      assert(t_align == 0 || t_align == align || o.isInstanceOf[pc.DataSymbol.Sized])
      assert(align <= maxAlign)
    } else {
      // guess alignment by size
      // kto-nibut, uberite eto bezobrazie!!!
      align = Math.max(t_align, Numerate.getAlign(objectSize(o)))
      align = Math.min(align, maxAlign)
    }

    align
  }

  /** --------------------------------------------------------------------- */
  def putStaticFieldValue(f: pcO.StaticField): Unit = f.value match {
    case v: IntValue => f.size match {
      case 1 => cd.genByte(v.value & 0xFF)
      case 2 => cd.genWord(v.value.toShort)
      case 4 => cd.genLWord(v.value)
      case x => shouldNotReachHere(s"unexpected static field size: $x")
    }
    case v: LongValue   => cd.genQWord(v.value)
    case v: FloatValue  => cd.genFloat(v.value)
    case v: DoubleValue => cd.genDouble(v.value)
    case x => shouldNotReachHere(s"unexpected value: $x")
  }

  def createCstrPool(table: pcO.StringTable): Unit = {
    assert(!table.ownsSegment)
    at.setSegment(table, cd.makeSeg { seg =>
      seg.putW32(table.getLength)
      seg.putZeroes(addressSize - 4)
      for (i <- 0 until table.getLength) {
        val holder = at.StringHolder(table.getStringByIndex(i))
        table.setStringHolder(i, holder)
        seg.addFixup(BYTE_STR_32, holder, 0)
        seg.putZeroes(addressSize - 4)
      }
    })
  }


  /////////////////////////////////////////////////////////////////////////////
  // Shared string constants

  private var constStrings = mutable.HashMap.empty[(XString, Boolean), pc.Symbol]

  def getStrConst(s: XString, bstr: Boolean): pc.Symbol = constStrings.getOrElseUpdate((s, bstr), {
    val seg = cd.makeSeg { if (bstr) cd.genBstr(s) else cd.genUstr(s) }
    val obj = at.newSizedConst(js.format("$c_$_%d", constStrings.size + 1), seg.length)
    at.setSegment(obj, seg)
    obj
  })


  /* --------- A l l o c a t e   G l o b a l   V a r i a b l e s -------------- */
  def initModule(): Unit = {
    cd.initModule()
    std.initModule()
  }

  def exitModule(): Unit = {
    std.exitModule()
    cd.exitModule()

    constStrings = mutable.HashMap.empty[(XString, Boolean), pc.Symbol]

    at.currProc = null
    at.currClass = null
  }
}
