/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.*
import com.huawei.excelsior.jet.assembler.fixups.{Relocation, RelocationKind}
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule as at
import com.huawei.excelsior.jet.compiler.o2lib.fe.pc
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.getO2Method
import com.huawei.excelsior.jet.compiler.{CodeMetadata, symlevel}
import com.huawei.excelsior.o2j.runtime.*
import xscala.io.ByteBuffer

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.collection.mutable.ArrayBuffer

object CodeDefModule {

  class Fixup(val kind: RelocationKind, val target: assembler.Symbol, val addend: Int, val position: Int) {
    def getTargetAsOBJECT: pc.Symbol = target match {
      case m: symlevel.Method => getO2Method(m)
      case o: pc.Symbol => o
      case _ => shouldNotReachHere(s"unexpected fixup target: $target")
    }
  }

  class Segment {
    private var code = new ByteBuffer
    def length = code.length

    var fixups = new ArrayBuffer[Fixup]
    var alignment: Int = _
    var requiredSectionAlignmentLg: Int = _ // log2 of required section alignment

    def getCode: (Array[Byte], Int) = (code.getBytesPointer, length)

    def setCode(arr: Array[Byte], len: Int): Unit = {
      assert(length == 0)
      code = new ByteBuffer(arr, len)
    }

    def getByte(pos: Int): Byte = code.getByte(pos)
    def getW16(pos: Int): Short = code.getW16(pos)
    def getW32(pos: Int): Int = code.getW32(pos)

    def setW16(pos: Int, value: Int): Unit = code.setW16(pos, value)
    def setW32(pos: Int, value: Int): Unit = code.setW32(pos, value)

    def patchW32(pos: Int, patch: Int): Unit = code.patchW32(pos, patch)
    def patchW64(pos: Int, patch: Long): Unit = code.patchW64(pos, patch)

    def putW8(b: Int): Unit = code.putW8(b)
    def putW16(w: Int): Unit = code.putW16(w)
    def putW32(w: Int): Unit = code.putW32(w)
    def putW64(q: Long): Unit = code.putW64(q)

    def putZeroes(n: Int): Unit = code.putZeroes(n)

    def append(that: ByteBuffer): Unit = {
      for (i <- 0 until that.length) {
        putW8(that.getW8(i).toInt & 0xFF)
      }
    }

    def append(that: Segment): Unit = {
      val l = length
      append(that.code)
      for (f <- that.fixups) {
        this.fixups += new Fixup(f.kind, f.target, f.addend, f.position + l)
      }
    }

    def transformFixups(transform: Fixup => Option[Fixup]): Unit = {
      val newFixups = new ArrayBuffer[Fixup]
      for (fixup <- fixups) {
        transform(fixup) match {
          case Some(newFixup) => newFixups += newFixup
          case None => // TODO: fixup.resolve()
        }
      }
      fixups = newFixups
    }

    def addFixup(kind: RelocationKind, target: assembler.Symbol, addend: Int): Unit = {
      fixups += new Fixup(kind, target, addend, length)
      code.putZeroes(kind.width.nbytes)
    }

    def alignData(alignment: Int, filler: Int = 0): Unit = code.align(alignment, filler)

    def fixupsIterator: Iterator[Fixup] = fixups.iterator

    def fixupsLength: Int = fixups.length
  }

  private var cSeg: Segment = _

  /** Returns false iff fixup are not exist in .exe/.dll */
  def isRTFixup(fx: Fixup): Boolean = fx.kind match {
    case ADDR32 | ADDR64 | OFFS32 | CODE_OFFS32 => true
    case TD_REL_32 | TD_REL_16 | TD_REL_32_DEL => false
    case TD_INDEX_16 | OFFS32_LOCAL | BYTE_STR_32 | RVA_32 => false
    case x => shouldNotReachHere(s"unexpected fixup kind: $x")
  }

  def withSeg(seg: Segment)(action: => Unit): Segment = {
    val oldSeg = cSeg
    cSeg = seg
    action
    cSeg = oldSeg
    seg
  }

  def newSeg(alignment: Int = 0): Segment = {
    val sg = new Segment()
    sg.alignment = alignment
    sg
  }

  def makeSeg(alignment: Int)(action: => Unit): Segment = withSeg(newSeg(alignment)) { action }

  def makeSeg(action: => Unit): Segment = makeSeg(0) { action }

  def makeSeg(action: Segment => Unit): Segment = makeSeg(0) { action(cSeg) }

  def makeSeg(alignment: Int, buf: Array[Byte], length: Int): Segment = makeSeg(alignment) { cSeg.setCode(buf, length) }

  def makeSeg(buf: Array[Byte], length: Int): Segment = makeSeg(0, buf, length)

  def getSeg: Segment = cSeg

  def setSeg(sg: Segment): Unit = { cSeg = sg }

  def getCodeLen: Int = getSeg.length

  def genByte(b: Int): Unit = cSeg.putW8(b)
  def genWord(w: Short): Unit = cSeg.putW16(w.toInt & 0xFFFF)
  def genLWord(w: Int): Unit = cSeg.putW32(w)
  def genQWord(q: Long): Unit = cSeg.putW64(q)
  def genFloat(f: Float): Unit = genLWord(floatToRawIntBits(f))
  def genDouble(d: Double): Unit = genQWord(doubleToRawLongBits(d))

  def genBstr(s: XString): Unit = {
    for (ch <- s) {
      genByte(ch & 0xff)
    }
    genByte(0)
  }

  def genUstr(s: XString): Unit = {
    for (ch <- s.unicodeIterator) {
      genWord(ch.toShort)
    }
    genWord(0.toShort)
  }

  def addFixupAt(position: Int)(kind: RelocationKind, target: assembler.Symbol, addend: Int): Unit =
    cSeg.fixups += new Fixup(kind, target, addend, position)

  def addFixup(kind: RelocationKind, target: assembler.Symbol, addend: Int): Unit =
    cSeg.addFixup(kind, target, addend)

  def initModule(): Unit =
    cSeg = null

  def exitModule(): Unit =
    cSeg = null

  def segmentsHaveSameBytesAndFixups(x: Segment, y: Segment): Boolean = {
    if (x eq y) return true

    if (x.length != y.length) return false
    for (i <- 0 until x.length) {
      if (x.getByte(i) != y.getByte(i)) {
        return false
      }
    }

    if (x.fixupsLength != y.fixupsLength) return false
    for ((f1, f2) <- x.fixupsIterator zip y.fixupsIterator) {
      if ((f1.target ne f2.target) || f1.addend != f2.addend || f1.position != f2.position || f1.kind != f2.kind) {
        return false
      }
    }

    true
  }
}
