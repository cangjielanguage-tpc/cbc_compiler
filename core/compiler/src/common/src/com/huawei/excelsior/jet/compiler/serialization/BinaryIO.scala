/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.serialization

import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.{Domain, Environment, PreparationKind}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.symlevel.{CallConv, CallKind, ClassType, MethodReferenceAccessKind}
import xscala.io.{ByteBuffer, DataInput, DataOutput}
import xscala.io.LEB128Encoder.{decodeSLEB128, decodeULEB128}

import scala.collection.mutable

trait BinaryIO extends IOBase {

  protected def isUByte(x: Int) = 0 <= x && x <= 255
  protected def asUByte(x: Int) :Int = x ensuring isUByte

  class BinaryWriter(rawOut: DataOutput, contextClass: ClassType, env: Environment) extends Writer(env) {

    override protected lazy val symlevelWriter = env.getSymlevelWriter(this, contextClass)

    type Buffer = ByteBuffer

    private var buffer: ByteBuffer = _
    private var out: DataOutput = rawOut

    private def buffering = (buffer != null)

    override def bufferPosition: Int = buffer.length

    override def withBuffering(action: => Unit): Buffer = {
      assert (!buffering)
      val buf = new ByteBuffer()
      buffer = buf
      out = buf
      try {
        action
      } finally {
        buffer = null
        out = rawOut
      }
      buf
    }

    protected def putUByte(x: Int): Unit = out.putByte(asUByte(x))
    protected def putUInt(x: Int): Unit = out.putULEB(x)

    override def putInt(x: Int): Unit = out.putSLEB(x)
    private def putLong(x: Long): Unit = out.putW64(x)
    private def putFloat(x: Float): Unit = out.putF32(x)
    private def putDouble(x: Double): Unit = out.putF64(x)

    private val strings = new mutable.LinkedHashMap[XString, Int]
    private var lastStringIndex = -1

    override def writeHeader(): Unit = {
      assert (!buffering)
      putUInt(strings.size)
      for (str <- strings.keys) {
        val len = str.length
        putUInt(len)
        for (i <- (0 until len)) out.putByte(str.charAt(i))
      }
    }

    override def writeBuffer(buffer: Buffer): Unit = {
      assert (!buffering)
      out.putBytes(buffer.toByteArray)
      buffer.reset()
    }

    override def putXString(x: XString): Unit = {
      assert(buffering)
      putUInt(strings.getOrElseUpdate(x, {
        lastStringIndex += 1
        lastStringIndex
      }))
    }

    override def enumeration(x: scala.reflect.Enum): Unit = putUByte(x.ordinal)

    override def arithOp(op: ArithOp): Unit = enumeration(op)

    override def width(width: Width): Unit = enumeration(width)

    override def asmType(asmType: AsmType): Unit = enumeration(asmType)

    override def methodRefAccessKind(kind: MethodReferenceAccessKind): Unit = enumeration(kind)

    override def preparationKind(kind: PreparationKind): Unit = putUByte(kind.toBitmask)

    override def callConv(cc: CallConv): Unit = enumeration(cc)

    override def callKind(ck: CallKind): Unit = enumeration(ck)

    override def domain(domain: Domain): Unit = enumeration(domain)

    override def xstring(str: XString): Unit = putXString(str)

    override def bool(value: Boolean): Unit = putUByte(if (value) 1 else 0)

    override def number(num: Int): Unit = putInt(num)

    override def unsignedNumber(num: Int): Unit = putUInt(num)

    override def longNumber(num: Long): Unit = putLong(num)

    override def floatNumber(num: Float): Unit = putFloat(num)

    override def doubleNumber(num: Double): Unit = putDouble(num)

    override def delimiter(): Unit = {}
  }

  class BinaryReader(in: DataInput, contextClass: ClassType, env: Environment) extends Reader(env) {

    override protected lazy val symlevelReader = env.getSymlevelReader(this, contextClass)

    private var strings: Array[XString] = _

    protected def nextUByte(): Int = in.getUW8()
    protected def nextUInt(): Int = in.getULEB()

    override def nextInt(): Int = in.getSLEB()
    private def nextLong(): Long = in.getW64()
    private def nextFloat(): Float = in.getF32()
    private def nextDouble(): Double = in.getF64()

    override def readHeader(): Unit = {
      strings = Array.fill(nextUInt()) {
        val len = nextUInt()
        XString.fill(len) { in.getW8() }
      }
    }

    override def nextXString() = strings(nextUInt())

    override def enumeration[T <: scala.reflect.Enum](fromOrdinal: Int => T): T = fromOrdinal(nextUByte())

    override def arithOp() = enumeration(ArithOp.fromOrdinal)
    override def width(): Width = enumeration(Width.fromOrdinal)
    override def asmType(): AsmType = enumeration(AsmType.fromOrdinal)
    override def methodRefAccessKind() = enumeration(MethodReferenceAccessKind.fromOrdinal)
    override def preparationKind() = PreparationKind.fromBitmask(nextUByte().toByte)
    override def callConv() = enumeration(CallConv.fromOrdinal)
    override def callKind() = enumeration(CallKind.fromOrdinal)
    override def domain() = enumeration(Domain.fromOrdinal)

    override def xstring() = nextXString()
    override def bool() = nextUByte() != 0
    override def number() = nextInt()
    override def unsignedNumber() = nextUInt()
    override def longNumber() = nextLong()
    override def floatNumber() = nextFloat()
    override def doubleNumber() = nextDouble()

    override def delimiter(): Unit = {}

    override def isEOF = { in.available == 0 }

    override def skip(n: Int): Unit = {
      in.skip(n)
    }
  }
}
