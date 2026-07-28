/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.properties.OS
import xscala.text.{Encoding, Utf8Encoding}

import java.io.Closeable

object TextOutput {
  val defaultEncoding: Encoding = Utf8Encoding

  def apply(out: DataOutput, encoding: Encoding, close: Boolean = true): TextOutput =
    new DataWrapper(out, close, encoding)

  def apply(sb: StringBuilder): TextOutput = new TextOutput {
    def print(ch: Char): Unit = sb.append(ch)
    def print(s: String): Unit = sb.append(s)
  }

  def apply(buf: ByteBuffer, encoding: Encoding): TextOutput = apply(buf, encoding, close = false)

  def asString(action: TextOutput => Unit): String = {
    val sb = new StringBuilder()
    action(apply(sb))
    sb.toString
  }

  def asBytes(action: TextOutput => Unit, encoding: Encoding = defaultEncoding): ByteBuffer = {
    val buf = new ByteBuffer()
    action(apply(buf, encoding))
    buf
  }

  def from(path: Path, bufSize: Int = 8192, encoding: Encoding = defaultEncoding): TextOutput = {
    apply(DataOutput.from(path, bufSize), encoding, close = true)
  }

  def fromFile(fileName: String, bufSize: Int = 8192, encoding: Encoding = defaultEncoding): TextOutput = {
    from(Path(fileName), bufSize, encoding)
  }

  def wrapHandle(strmHandle: Object, encoding: Encoding, close: Boolean = true): TextOutput =
    apply(DataOutput.wrapHandle(strmHandle, close), encoding, close)

  private class DataWrapper(out: DataOutput, doClose: Boolean, encoding: Encoding) extends TextOutput {
    def print(s: String): Unit = {
      out.putBytes(encoding.encodeStringReplacing(s))
    }

    def print(ch: Char): Unit = {
      out.putBytes(encoding.encodeReplacing(Array[Char](ch)))
    }

    override def flush(): Unit = out.flush()

    override def close(): Unit = {
      out.flush()
      if (doClose) out.close()
    }
  }
}

abstract class TextOutput extends Closeable {
  def print(ch: Char): Unit
  def print(s: String): Unit

  def println(): Unit = print(OS.host.lineSeparator)

  def println(s: String): Unit = {
    print(s)
    println()
  }

  def flush(): Unit = {}
  def close(): Unit = {}

  def printStackTrace(ex: Throwable): Unit = {
    IOVMDependent.get.printStackTrace(ex, this)
    flush()
  }
}
