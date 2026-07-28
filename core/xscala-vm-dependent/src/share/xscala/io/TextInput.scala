/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.text.{Encoding, Utf8Encoding}

import java.io.Closeable

object TextInput {
  val defaultEncoding: Encoding = Utf8Encoding

  def apply(in: DataInput, encoding: Encoding, close: Boolean = true): TextInput =
    new DataWrapper(in, close, encoding)

  def from(path: Path, buffered: Boolean = false, encoding: Encoding = defaultEncoding) =
    apply(DataInput.from(path, buffered), encoding, close = true)

  def fromFile(fileName: String, buffered: Boolean = false, encoding: Encoding = defaultEncoding) =
    from(Path(fileName), buffered, encoding)

  def fromString(str: String): TextInput = fromStrings(Array(str))

  def fromStrings(strs: Array[String]): TextInput = new StringWrapper(strs)

  def wrapHandle(strmHandle: Object, encoding: Encoding, close: Boolean = true) =
    apply(DataInput.wrapHandle(strmHandle, close), encoding, close)

  private class StringWrapper(strs: Array[String]) extends TextInput {
    private var idx = 0
    private var pos = 0

    /** Reads one line and returns it as a string NOT including newline character(s).
      * Treats any of \r\n, \r, or \n as a line separator.
      * Returns `null` on EOF.
      */
    def getLine(): String = {
      if (idx >= strs.length) {
        return null
      }

      val s = strs(idx)
      var end = pos
      var wasSep = false

      while (!wasSep && end < s.length) {
        val ch = s(end)
        end += 1
        wasSep = (ch == '\r' || ch == '\n')
      }
      
      val result = s.substring(pos, if (wasSep) end - 1 else end)
      pos = end

      if (wasSep && pos < s.length && s(pos - 1) == '\r' && s(pos) == '\n') {
        pos += 1
      }

      if (pos == s.length) {
        idx += 1
        pos = 0
      }

      result
    }
  }

  private class DataWrapper(in: DataInput, doClose: Boolean, encoding: Encoding) extends TextInput {
    private val buf = new ByteBuffer()

    /** Reads one line and returns it as a string NOT including newline character(s).
      * Treats any of \r\n, \r, or \n as a line separator.
      * Returns `null` on EOF.
      */
    def getLine(): String = {
      var b = in.getByte()
      val wasRead = (b != -1)

      while (b != -1 && b != '\r' && b != '\n') {
        buf.putByte(b)
        b = in.getByte()
      }
      
      if (!wasRead) {
        null
      } else {
        val result = encoding.decodeStringThrowing(buf.getBytesPointer, 0, buf.length)
        buf.reset()

        if (b == '\r') {
          b = in.getByte()
          if (b != -1 && b != '\n') {
            buf.putByte(b)
          }
        }

        result
      }
    }

    override def close(): Unit = {
      if (doClose) in.close()
    }
  }
}

abstract class TextInput extends Closeable {
  /** Reads one line and returns it as a string NOT including newline character(s).
    * Treats any of \r\n, \r, or \n as a line separator.
    * Returns `null` on EOF.
    */
  def getLine(): String

  /** Returns an iterator who returns all lines. */
  def getLines(): Iterator[String] = new Iterator[String] {
    var first = true
    var _head: String = null
    def head = { if (first) { _head = getLine(); first = false }; _head }

    def hasNext = head != null

    def next() = if (hasNext) { val s = head; _head = getLine(); s } else Iterator.empty.next()
  }

  def close(): Unit = {}
}

