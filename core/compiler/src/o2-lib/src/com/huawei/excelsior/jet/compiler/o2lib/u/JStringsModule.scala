/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.{XString, XStringInternTable}
import com.huawei.excelsior.jet.compiler.Env.isWorkMode
import com.huawei.excelsior.jet.compiler.symlevel.{MethodSignature, SignatureType}
import com.huawei.excelsior.o2j.runtime.O2JSupport
import xscala.util.StringOps.asciiToUpperCase

object JStringsModule {

  private val STRING_BUFFER_INITIAL_CAPACITY = 56
  final class StringBuffer(private var capacity: Int) {
    private var buf = new Array[Byte](capacity)
    private var len = 0
    def this() = this(STRING_BUFFER_INITIAL_CAPACITY)

    def intern(): XString = {
      if (len == 0) {
        return interntable.internedEmptyString
      }
      interntable.put(buf, len)
    }

    def toUpperCase(): Unit = {
      for (i <- 0 until len) {
        buf(i) = buf(i).toChar.asciiToUpperCase.toByte
      }
    }

    def endsWith(suffix: Array[Byte]): Boolean = {
      val from = len - suffix.length
      if (from < 0) false else startsWith(suffix, from)
    }

    // only ASCII string consts are allowed here (came form O2 string literals)
    def endsWith(suffix: String): Boolean = endsWith(O2JSupport.byteArrStringConst(suffix))

    def startsWith(prefix: Array[Byte], from: Int): Boolean = {
      assert(from >= 0 && from + prefix.length <= len)
      for (i <- 0 until prefix.length) {
        val c1 = buf(i + from) & 0xff
        val c2 = prefix(i) & 0xff
        if (c1 != c2) {
          return false
        }
      }
      true
    }

    def lastIndexOf(ch: Byte): Int = lastIndexOf(ch, len)

    def lastIndexOf(ch: Byte, index: Int): Int = {
      val from = if (index >= len) len - 1 else index
      for (i <- from to 0 by -1) {
        if (buf(i) == ch)
          return i
      }
      -1
    }

    def indexOf(ch: Byte): Int = indexOf(ch, 0)

    def indexOf(ch: Byte, index: Int): Int = {
      val from = if (index < 0) 0 else index
      for (i <- from until len) {
        if (buf(i) == ch) return i
      }
      -1
    }

    def replace(from: Byte, to: Byte): Unit = {
      replaceInRegion(0, len, from, to)
    }

    def replaceInRegion(index: Int, len: Int, from: Byte, to: Byte): Unit = {
      assert(index >= 0)
      assert(len >= 0)
      assert(index <= this.len - len)
      for (i <- index until index + len) {
        if (buf(i) == from) buf(i) = to
      }
    }

    def toJString = XString.slice(buf, 0, len)

    def fromPlatform = XString(toJString.platformToString)

    def appendString(s: XString): Unit = {
      if (!s.isEmpty) {
        ensureCapacity(len + s.length)
        s.getChars(0, s.length, buf, len)
        len += s.length
      }
    }

    def appendInt(i: Int): Unit = {
      appendf("%d", i)
    }

    def appendf(format: String, args: Any*): Unit = {
      FormatImpl.appendf(this, format, args: _*)
    }

    def insert(index: Int, str: XString): Unit = {
      assert(0 <= index && index <= this.len)
      val len = str.length
      if (len == 0)
        return
      ensureCapacity(this.len + len)
      this.moveChars(index, index + len, this.len - index)
      str.getChars(0, len, buf, index)
      this.len += len
    }

    def appendChar(ch: Byte): Unit = {
      ensureCapacity(len + 1)
      buf(len) = ch
      len += 1
    }

    def appendChar(ch: Char): Unit = {
      assert(ch < 256)
      ensureCapacity(this.len + 1)
      this.buf(this.len) = ch.toByte
      this.len += 1
    }

    def trunc(len: Int): Unit = {
      assert(len <= this.len)
      this.len = len
    }

    def assign(s: String): Unit = {
      len = 0
      append(s)
    }

    def charAt(index: Int): Byte = {
      assert(0 <= index && index < len)
      buf(index)
    }

    def charAtAsChar(index: Int) = (charAt(index) & 0xff).toChar

    private def moveChars(srcPos: Int, dstPos: Int, count: Int): Unit =
      Array.copy(buf, srcPos, buf, dstPos, count)


    def length = this.len

    private def expandCapacity(len: Int): Unit = {
      assert(capacity >= STRING_BUFFER_INITIAL_CAPACITY)
      capacity = capacity * 2
      if (len > capacity) {
        capacity = len
      }
      val _new = new Array[Byte](capacity)
      Array.copy(buf, 0, _new, 0, this.len)
      buf = _new
    }

    def ensureCapacity(len: Int): Unit = if (len > capacity) expandCapacity(len)

    def append(s: String): Unit = if (s.nonEmpty) {
      ensureCapacity(this.len + s.length)
      Array.copy(O2JSupport.byteArrStringConst(s), 0, buf, this.len, s.length)
      this.len += s.length
    }
  }

  val interntable = new XStringInternTable()
  val jstrClinit = internJString("<clinit>")
  val jstrDot = internJString(".")
  val jstrQuote = internJString("\"")
  val jstrEmpty = internJString("")
  val jstrFinalize = internJString("finalize")
  val jstrFinalizeCangjie = internJString("~init")
  val jstrInit = internJString("<init>")
  val jstrMININT = internJString("-2147483648")
  val jstrMainName = internJString("main")
  val jstrMainSig = internJString("([Ljava/lang/String;)V")
  val jstrXScalaMainSig = internJString("([Lxscala/String;)V")
  val jstrLWRTMainSig = internJString("(Lcom/huawei/excelsior/aj/lang/AJRefArray;)I")
  val jstrCangjieMainSig = internJString("(IJ)I")
  val jstrObject = internJString("java/lang/Object")
  val jstrStringSig = internJString("Ljava/lang/String;")
  val jstrTwoDots = internJString("..")
  val jstrTwoSlashes = internJString("//")
  val jstrVoidMethodSig = internJString("()V")

  def newJString(s: String) = XString(s)

  def parseInt(s: XString): Int = {
    parseIntImpl(s, None)
  }

  def parseIntOrElse(s: XString, default: Int): Int = {
    parseIntImpl(s, Some(default))
  }

  private def parseIntImpl(s: XString, default: Option[Int]): Int = {
    if (s == null || s.length == 0) {
      return default.get
    }
    val len: Int = s.length
    var result: Int = 0
    val neg = s.charAt(0) == 45.toByte
    var pos: Int = 0
    if (neg) {
      pos += 1
      if (1 == len) {
        return default.get
      }
      if (s == jstrMININT) {
        return -0x080000000
      }
    }
    while (pos < len) {
      val ch = s.charAt(pos)
      if (ch < 48.toByte || ch > 57.toByte) {
        return default.get
      }
      val digit = (ch & 0xff) - 48
      if (result > (Int.MaxValue / 10)) {
        return default.get
      }
      result = result * 10
      if (result > Int.MaxValue - digit) {
        return default.get
      }
      result += digit
      pos += 1
    }
    if (neg) result = -result
    result
  }

  def parseULong(s: XString): Long = {
    val len: Int = s.length
    if (s == null || len == 0) {
      return -1
    }
    var result: Long = 0
    var pos: Int = 0
    while (pos < len) {
      val ch = s.charAt(pos)
      if (ch < 48.toByte || ch > 57.toByte) {
        return -1
      }
      val digit = (ch & 0xff) - 48
      if (result > (Long.MaxValue / 10)) {
        return -1
      }
      result = result * 10
      if (result > (Long.MaxValue - digit)) {
        return -1
      }
      result += digit.toLong
      pos += 1
    }
    result
  }

  def TODO2(s: XString): String = {
    if (s == null) return null
    if (s.length == 0) return ""
    s.toString
  }

  def format(format: String, args: Any*): XString = {
    val buf = new StringBuffer()
    buf.appendf(format, args: _*)
    buf.toJString
  }

  // only ASCII string consts are allowed her that came from o2 string literals
  def internJString(s: String): XString = {
    val length = s.length()
    if (length == 0) {
      return interntable.internedEmptyString
    }
    interntable.put(O2JSupport.byteArrStringConst(s), length)
  }

  def intern(str: XString): XString = interntable.put(str)

  def internSubstring(str: XString, from: Int, to: Int): XString = {
    val toInd = if (to == -1) str.length else to
    interntable.put(str, from, toInd)
  }

  def cleanStringsCache(): Unit = {
    xmConfigModule.clearCaches()
    val oldSize = interntable.size
    val oldCapacity = interntable.capacity
    interntable.cleanAndResize()

    if (isWorkMode) {
      val newSize = interntable.size
      val newCapacity = interntable.capacity

      if (oldCapacity == newCapacity) {
        xiEnvModule.info.print(s"\n\nXString interntable cleaned: $oldSize -> $newSize, no resize")
      } else {
        xiEnvModule.info.print(s"\n\nXString interntable cleaned: $oldSize -> $newSize, resized: $oldCapacity -> $newCapacity")
      }
    }
  }
}
