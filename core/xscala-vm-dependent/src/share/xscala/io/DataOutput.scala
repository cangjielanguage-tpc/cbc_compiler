/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.text.ModifiedUtf8Encoding
import xscala.util.MathUtils.zeroExtend

import java.io.Closeable
import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits


object DataOutput {
  def from(path: Path, bufSize: Int = 8192): DataOutput = {
    new Buffered(FileSystem.newFileOutputStream(path, append = false), bufSize)
  }

  def fromFile(fileName: String, bufSize: Int = 8192): DataOutput = {
    from(Path(fileName), bufSize)
  }

  def wrapHandle(strmHandle: Object, close: Boolean = true): DataOutput = new HandleWrapper(strmHandle, doClose = close)

  private class HandleWrapper(strmHandle: Object, doClose: Boolean) extends DataOutput {
    def putByte(b: Int): Unit = OutputStreamVMDependent.get.writeByte(strmHandle, b)

    /* TODO:
    override def putBytes(data: Array[Byte], offset: Int, size: Int): Unit = {
      //transfer data
    }*/

    override def flush(): Unit = OutputStreamVMDependent.get.flush(strmHandle)
    override def close(): Unit = if (doClose) OutputStreamVMDependent.get.close(strmHandle)
  }

  private class Buffered(out: DataOutput, bufSize: Int = 8192) extends DataOutput {
    private val buffer = new Array[Byte](bufSize)
    private var length = 0

    private def tryReserve(size: Int): Boolean = {
      if (size > bufSize - length) {
        localFlush()
      }
      size < bufSize
    }

    private def localFlush(): Unit = {
      if (length > 0) {
        out.putBytes(buffer, 0, length)
        length = 0
      }
    }

    def putByte(b: Int): Unit = {
      tryReserve(1)
      buffer(length) = b.toByte
      length += 1
    }

    override def putBytes(data: Array[Byte], offset: Int, size: Int): Unit = {
      if (tryReserve(size)) {
        Array.copy(data, offset, buffer, length, size)
        length += size
      } else {
        out.putBytes(data, offset, size)
      }
    }

    override def flush(): Unit = {
      localFlush()
      out.flush()
    }

    override def close(): Unit = {
      flush()
      out.close()
    }
  }
}

trait DataOutput extends Closeable { self =>

  def flush(): Unit = {}

  def close(): Unit = {}

  def putByte(b: Int): Unit

  def putBytes(data: Array[Byte]): Unit = {
    putBytes(data, 0, data.length)
  }

  def putBytes(data: Array[Byte], offset: Int, size: Int): Unit = {
    for (i <- 0 until size) {
      putByte(data(offset + i))
    }
  }

  final def putBytes(data: Int*): Unit = {
    for (b <- data) putByte(b)
  }

  final def putZeroes(n: Int): Unit = {
    for (_ <- 0 until n) putByte(0)
  }

  final def putBoolean(b: Boolean): Unit = putByte(if (b) 1 else 0)

  final def putW8(w8: Int): Unit = putByte(w8)

  final def putW16(w16: Int): Unit = {
    putW8(w16 & 0xFF)
    putW8((w16 >>> 8) & 0xFF)
  }

  final def putW32(w32: Int): Unit = {
    putW16(w32 & 0xFFFF)
    putW16(w32 >>> 16)
  }

  final def putW64(w64: Long): Unit = {
    putW32(w64.toInt)
    putW32((w64 >>> 32).toInt)
  }

  final def putF32(f: Float): Unit = putW32(floatToRawIntBits(f))

  final def putF64(d: Double): Unit = putW64(doubleToRawLongBits(d))

  final def putULEB(v: Int): Unit = LEB128Encoder.encodeULEB128(zeroExtend(v), putByte)
  final def putULEB(v: Long): Unit = LEB128Encoder.encodeULEB128(v, putByte)

  final def putSLEB(v: Int): Unit = LEB128Encoder.encodeSLEB128(v.toLong, putByte)
  final def putSLEB(v: Long): Unit = LEB128Encoder.encodeSLEB128(v, putByte)

  final def putUTF(s: String): Unit = {
    val arr = ModifiedUtf8Encoding.encodeStringReplacing(s)
    putW32(arr.length)
    putBytes(arr)
  }
}
