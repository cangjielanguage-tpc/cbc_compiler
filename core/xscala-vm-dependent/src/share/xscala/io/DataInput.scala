/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.text.ModifiedUtf8Encoding

import java.io.Closeable
import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat

object DataInput {
  def from(data: Array[Byte]): DataInput = from(data, 0, data.length)

  def from(data: Array[Byte], offset: Int, size: Int): DataInput = new DataInput {
    var pos = offset

    override def available = size - (pos - offset)

    override def getByte() = {
      if (available == 0) {
        -1
      } else {
        val b = data(pos) & 0xFF
        pos += 1
        b
      }
    }

    override def skip(n: Int): Int = {
      val sk = n.min(available)
      pos += sk
      sk
    }
  }

  def from(buf: ByteBuffer): DataInput = from(buf.getBytesPointer, 0, buf.length)

  def from(path: Path, buffered: Boolean = false): DataInput = {
    val f = FileSystem.newFileInputStream(path)
    if (buffered) new Buffered(f) else f
  }

  def fromFile(fileName: String, buffered: Boolean = false): DataInput = {
    from(Path(fileName), buffered)
  }

  def wrapHandle(strmHandle: Object, close: Boolean = true): DataInput = new HandleWrapper(strmHandle, doClose = close)

  private class HandleWrapper(strmHandle: Object, doClose: Boolean) extends DataInput {
    override def available: Int = InputStreamVMDependent.get.bytesAvailable(strmHandle)
    override def getByte(): Int = InputStreamVMDependent.get.readByte(strmHandle)
    override def close(): Unit = if (doClose) InputStreamVMDependent.get.close(strmHandle)
  }

  private class Buffered(in: DataInput, bufSize: Int = 8192) extends DataInput {
    private val buffer = new Array[Byte](bufSize)
    private var pos = 0
    private var length = 0

    private def bufCount = length - pos

    def available: Int = bufCount + in.available

    override def close(): Unit = in.close()

    override def skip(n: Int): Int = {
      val n0 = n min bufCount
      pos += n0
      if (n0 != n) {
        n0 + in.skip(n - n0)
      } else n0
    }

    private def fillBuffer(maxRead: Int): Unit = {
      assert(bufCount == 0)
      length = 0 max in.getBytes(buffer, 0, maxRead min bufSize)
      pos = 0
    }

    def getByte(): Int = {
      if (bufCount == 0) {
        fillBuffer(bufSize) // will block if no input is yet available
        if (bufCount == 0) {
          return -1
        }
      }
      pos += 1
      buffer(pos - 1) & 0xFF
    }

    override def getBytes(data: Array[Byte], offset: Int, size: Int): Int = {
      val n = size min bufCount
      if (n != 0) {
        Array.copy(buffer, pos, data, offset, n)
        pos += n
      }
      val rest = size - n
      if (rest == 0) {
        return n
      }

      val n2 = if (rest < bufSize) {
        fillBuffer(if (n != 0) in.available else bufSize) // will block if `n` is zero and no input is yet available
        if (bufCount == 0) -1 else getBytes(data, offset + n, rest)
      } else {
        in.getBytes(data, offset + n, rest)
      }
      val received = n + (n2 max 0)
      if (received == 0) -1 else received
    }
  }
}

trait DataInput extends Closeable { self =>

  def available: Int

  def getByte(): Int

  def close(): Unit = {}

  def getBytes(data: Array[Byte]): Int = {
    getBytes(data, 0, data.length)
  }

  def getBytes(data: Array[Byte], offset: Int, size: Int): Int = {
    for (i <- 0 until size) {
      val in = getByte()
      if (in == -1) {
        return if (i == 0) -1 else i
      }
      data(i + offset) = in.toByte
    }
    size
  }

  def skip(n: Int): Int = {
    var i = 0
    while (i < n && getByte() != -1) {
      i += 1
    }
    i
  }

  final def getUW8(): Int = getByte() ensuring (_ >= 0)

  final def getW8(): Byte = getUW8().toByte

  final def getBoolean(): Boolean = getUW8() == 1

  final def getW16(): Short = (getUW8() | (getUW8() << 8)).toShort

  final def getUW16(): Int = 0xFFFF & getW16()

  final def getW32(): Int = getUW16() | (getUW16() << 16)

  final def getUW32(): Long = 0xFFFFFFFFL & getW32().toLong

  final def getW64(): Long = getUW32() | (getUW32() << 32)

  final def getF32(): Float = intBitsToFloat(getW32())

  final def getF64(): Double = longBitsToDouble(getW64())

  final def getULEB(): Int = LEB128Encoder.decodeULEB128(getUW8)
  final def getULEBLong(): Long = LEB128Encoder.decodeULEB128Long(getUW8)

  final def getSLEB(): Int = LEB128Encoder.decodeSLEB128(getUW8)
  final def getSLEBLong(): Long = LEB128Encoder.decodeSLEB128Long(getUW8)

  private def getUTFAsArray(): Array[Byte] = {
    val length = getW32()
    val bytes = new Array[Byte](length)
    getBytes(bytes) ensuring (_ == length)
    bytes
  }

  final def getUTF(): String = {
    ModifiedUtf8Encoding.decodeStringThrowing(getUTFAsArray())
  }
}
