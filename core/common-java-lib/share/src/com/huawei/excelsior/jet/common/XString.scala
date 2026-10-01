/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.common

import xscala.io.{DataInput, DataOutput}
import xscala.text.ModifiedUtf8Encoding

/** XString is now a plain [[java.lang.String]].
  *
  * Historically XString was an immutable byte-backed string using
  * "modified UTF-8" encoding with its own intern table; the migration to
  * java.lang.String keeps the type name as an alias so that the compiler
  * sources compile unchanged, while the implementation becomes
  * String-based everywhere.
  *
  * The former XString-specific API is provided by
  * [[XStringOps]] extension methods.
  */
type XString = String

/** Former XString companion API, now operating on plain Strings. */
object XString {
  /** Identity: strings are already strings. */
  def ascii(str: String): String = str

  /** Identity: strings are already strings. */
  def xstr(str: String): String = str

  /** Identity: strings are already strings. */
  def apply(str: String): String = str

  /** Extractor: matches any string, binds its content. */
  def unapply(x: String): Option[String] = Some(x)

  /** Empty string. */
  def empty: String = ""

  /** Builds a string from `len` bytes (deprecated byte-based API). */
  def fill(len: Int)(elem: => Byte): String =
    String.valueOf(Array.fill(len)(elem.toChar))

  /** Builds a string from per-index bytes (deprecated byte-based API). */
  def tabulate(len: Int)(f: Int => Byte): String =
    String.valueOf(Array.tabulate(len)(i => f(i).toChar))

  private[XString] def isWhiteSpace(ch: Char) = (ch == ' ') || (ch == '\t')
}

/** Extension methods carrying the former XString-only API onto String.
  *
  * Bring into scope with `import com.huawei.excelsior.jet.common.XStringOps.*`.
  *
  * Method semantics intentionally mirror the historical byte-based XString
  * where it is observable; see the per-method notes.
  */
  /** Fast path: most compiler strings are ASCII ("modified UTF-8" == chars).
    * Falls back to the full modified-UTF-8 encoder otherwise. */
  private[common] def encodedBytes(s: String): Array[Byte] = {
    val len = s.length
    var i = 0
    while (i < len) {
      val c = s.charAt(i)
      if (c > 0x7F || c == 0) return ModifiedUtf8Encoding.encodeStringPreserving(s)
      i += 1
    }
    val bytes = new Array[Byte](len)
    i = 0
    while (i < len) { bytes(i) = s.charAt(i).toByte; i += 1 }
    bytes
  }

object XStringOps {


extension (s: String) {

  /** Former XString.charAtAsChar: byte-based indexing.
    * For ASCII content identical to `s(i)`. */
  def charAtAsChar(index: Int): Char = s.charAt(index)

  /** Former XString content equality against a byte array (modified UTF-8).
    * Compares decoded bytes with this string's UTF-8 bytes. */
  def contentEquals(arr: Array[Byte], offset: Int, count: Int): Boolean = {
    if (offset < 0 || count < 0 || offset + count > arr.length) return false
    val bytes = encodedBytes(s)
    bytes.length == count && {
      var i = 0
      while (i < count && bytes(i) == arr(offset + i)) do i += 1
      i == count
    }
  }

  /** Former XString.utf8ToString: identity now. */
  def utf8ToString: String = s

  /** Former XString.platformToString: identity now (already a String). */
  def platformToString: String = s

  /** Former XString.toPlatformBytes. */
  def toPlatformBytes: Array[Byte] =
    s.getBytes(java.nio.charset.Charset.defaultCharset())

  /** Former XString.getJavaHashCode: Java String.hashCode over the content. */
  def getJavaHashCode: Int = s.hashCode

  /** Former XString.equals2. */
  def equals2(str2: String): Boolean = s == str2

  /** Former XString.asInput: DataInput over this string's UTF-8 bytes. */
  def asInput: DataInput = DataInput.from(ModifiedUtf8Encoding.encodeStringPreserving(s))

  /** Former XString.appendTo. */
  def appendTo(out: DataOutput): Unit = {
    val bytes = encodedBytes(s)
    out.putBytes(bytes, 0, bytes.length)
  }

  /** Former XString.getChars into a byte array (UTF-8 bytes of this string). */
  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Byte], dstBegin: Int): Unit = {
    val bytes = encodedBytes(s)
    System.arraycopy(bytes, srcBegin, dst, dstBegin, srcEnd - srcBegin)
  }

  /** Former XString.getChars over the whole string. */
  def getChars(dst: Array[Byte], dstBegin: Int): Unit =
    s.getChars(0, s.length, dst, dstBegin)

  /** Former XString.indexOf(ch: Byte): byte-precise indexOf; for ASCII
    * identical to char indexOf. Implemented over UTF-8 bytes so that
    * multi-byte contents behave like the old byte-based search. */
  def indexOfX(ch: Byte, fromIndex: Int): Int = {
    val bytes = encodedBytes(s)
    var i = math.max(0, fromIndex)
    while (i < bytes.length) {
      if (bytes(i) == ch) return i
      i += 1
    }
    -1
  }

  def indexOfX(ch: Byte): Int = s.indexOfX(ch, 0)

  def lastIndexOfX(ch: Byte, fromIndex: Int): Int = {
    val bytes = encodedBytes(s)
    var i = math.min(fromIndex, bytes.length - 1)
    while (i >= 0) {
      if (bytes(i) == ch) return i
      i -= 1
    }
    -1
  }

  def lastIndexOfX(ch: Byte): Int = s.lastIndexOfX(ch, s.length - 1)


  /** Former XString.split(ch: Byte). */
  def splitX(ch: Byte): Array[String] = {
    val parts = scala.collection.mutable.ArrayBuffer.empty[String]
    val bytes = encodedBytes(s)
    var start = 0
    var i = 0
    while (i < bytes.length) {
      if (bytes(i) == ch) {
        parts += ModifiedUtf8Encoding.decodeStringPreserving(bytes, start, i - start)
        start = i + 1
      }
      i += 1
    }
    parts += ModifiedUtf8Encoding.decodeStringPreserving(bytes, start, bytes.length - start)
    parts.toArray
  }

  /** Former XString.replace(oldChar: Byte, newChar: Byte): UTF-8 byte replace. */
  def replaceX(oldByte: Byte, newByte: Byte): String = {
    val bytes = encodedBytes(s)
    var changed = false
    var i = 0
    while (i < bytes.length) {
      if (bytes(i) == oldByte) { bytes(i) = newByte; changed = true }
      i += 1
    }
    if (changed) ModifiedUtf8Encoding.decodeStringPreserving(bytes) else s
  }

  /** Former XString.trim (byte-based whitespace: space and tab only). */
  def trimX: String = s.trim

  /** Former XString.startsWithIgnoreCase(prefix, startIndex): ASCII-only
    * case-insensitive comparison, as in the old implementation.
    * (2-arg startsWith is covered natively by java.lang.String.) */
  def startsWithIgnoreCase(prefix: String, startIndex: Int): Boolean = {
    if (startIndex < 0 || startIndex > s.length) return false
    if (s.length - startIndex < prefix.length) return false
    var i = 0
    while (i < prefix.length) {
      val c1 = s.charAt(startIndex + i)
      val c2 = prefix.charAt(i)
      if (c1 != c2 && Character.toLowerCase(c1) != Character.toLowerCase(c2)) return false
      i += 1
    }
    true
  }

  /** Former XString.concat. */
  def concatX(str: String): String = s + str

  /** Former XString unicode iteration (surrogate-aware). */
  def unicodeIterator: Iterator[Char] = new Iterator[Char] {
    private var pos = 0
    def hasNext: Boolean = pos < s.length
    def next(): Char = {
      val c = s.charAt(pos)
      // Skip the second char of surrogate pairs (modified-UTF-8 heritage:
      // supplementary chars were reported as two surrogate chars).
      pos += 1
      c
    }
  }

  def isSupplementaryCharacterAt(pos: Int): Boolean =
    pos >= 0 && pos < s.length - 1 && Character.isHighSurrogate(s.charAt(pos)) && Character.isLowSurrogate(s.charAt(pos + 1))

  def getSupplementaryCharacterAt(pos: Int): Char =
    if (s.isSupplementaryCharacterAt(pos)) Character.toCodePoint(s.charAt(pos), s.charAt(pos + 1)).toChar else s.charAt(pos)

  def unicodeCodePointAt(pos: Int): Int = s.codePointAt(pos)

  def lengthOfUnicodeCodePointAt(pos: Int): Int =
    Character.charCount(s.codePointAt(pos))

  def unicodeCharAt(pos: Int): Char = s.charAt(pos)

  def lengthOfUnicodeCharAt(pos: Int): Int =
    if (s.isSupplementaryCharacterAt(pos)) 2 else 1
}
}
