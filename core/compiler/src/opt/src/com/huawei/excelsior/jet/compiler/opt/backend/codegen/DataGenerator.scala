/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.{AsmType, Label, Segment, Symbol}
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.Env.addressSize
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import xscala.io.ByteBuffer

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.collection.mutable

/**
 * Generation of data segments accompanying code.
 *
 * @author alexm
 */
trait DataGenerator { self: Universe with BackEnd =>

  private val FloatSize = 4
  private val FloatAlign = 4

  private val DoubleSize = 8
  private val DoubleAlign = 8

  private val IntSize = 4
  private val IntAlign = 4

  private val LongSize = 8
  private val LongAlign = 8

  private val floatConstants  = new mutable.LinkedHashMap[Int, Symbol]
  private val doubleConstants = new mutable.LinkedHashMap[Long, Symbol]
  private val intConstants    = new mutable.LinkedHashMap[Int, Symbol]
  private val longConstants   = new mutable.LinkedHashMap[Long, Symbol]

  private val dataSegments = new mutable.ListBuffer[Segment]

  private def makeConst[K](key: K, cache: mutable.Map[K, Symbol], size: Int, align: Int)
                          (putValue: ByteBuffer => Unit): Symbol = {
    cache.getOrElseUpdate(key, {
      val buf = ByteBuffer(size)

      putValue(buf)

      symbolLinker.makeConstData(buf.toByteArray, align)
    })
  }

  /** Generates segment with float constant. */
  def floatConstant(value: Float): Symbol = {
    makeConst(floatToRawIntBits(value), floatConstants, FloatSize, FloatAlign)(_.putW32(floatToRawIntBits(value)))
  }

  /** Generates segment with double constant. */
  def doubleConstant(value: Double): Symbol = {
    makeConst(doubleToRawLongBits(value), doubleConstants, DoubleSize, DoubleAlign)(_.putW64(doubleToRawLongBits(value)))
  }

  /** Generates segment with int constant. */
  def intConstant(value: Int): Symbol = {
    makeConst(value, intConstants, IntSize, IntAlign)(_.putW32(value))
  }

  /** Generates segment with long constant. */
  def longConstant(value: Long): Symbol = {
    makeConst(value, longConstants, LongSize, LongAlign)(_.putW64(value))
  }

  /** Convert given values to a sequence of constant bytes. */
  def getConstBytes(size: Int, elemType: AsmType, values: Seq[Long]): Array[Byte] = {
    import AsmType.{I8, U8, F16, I16, U16, I32, U32, I64, U64}
    val buf = ByteBuffer(size)

    elemType match {
      case I8  | U8  => values foreach (v => buf.putW8 (v.toInt))
      case F16 | // F16 is implemented with I16
           I16 | U16 => values foreach (v => buf.putW16(v.toInt))
      case I32 | U32 => values foreach (v => buf.putW32(v.toInt))
      case I64 | U64 => values foreach (v => buf.putW64(v))
      case _ => shouldNotReachHere(s"Unexpected element type for const bytes: $elemType")
    }

    buf.toByteArray
  }

  /** Generates given values as a sequence of constant bytes. */
  def genConstBytes(size: Int, align: Int, elemType: AsmType, values: Seq[Long]): Symbol = {
    symbolLinker.makeConstData(getConstBytes(size, elemType, values), align)
  }

  /** Generates table with addresses of given labels. */
  def genAddressTable(tableSym: Symbol, labels: Seq[Label]): Unit = {
    assert(dataSegments forall (_.getSymbol != tableSym),
      "this table was already bound to the segment (TableJump was cloned?)")

    def genFixup(seg: Segment, target: Label): Unit = {
      seg.addDataAddress(target, Env.targetArch)
    }

    val seg = new Segment(tableSym)
    seg.alignStart(addressSize)

    labels foreach (genFixup(seg, _))
    dataSegments += seg
  }

  /** Sends all generated data segments into compiler environment. */
  def sendDataSegments(): Unit = {
    dataSegments foreach (symbolLinker.sendData(_, rootMethod))
  }

  def ensureNoDataSegments(): Unit = {
    assert(dataSegments.isEmpty)
  }

}
