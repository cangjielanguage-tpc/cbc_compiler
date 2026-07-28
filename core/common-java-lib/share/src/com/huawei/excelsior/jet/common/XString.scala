/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.common

import MyPredef.*
import com.huawei.excelsior.jet.common.XString.*
import com.huawei.excelsior.jet.common.XStringInternTable.*
import xscala.io.{DataInput, DataOutput}
import xscala.text.{ModifiedUtf8Encoding, PlatformEncoding}

import java.lang.ref.{ReferenceQueue, WeakReference}
import scala.collection.mutable.ArrayBuffer

/** General-purpose immutable string.
  *
  * Unlike [[java.lang.String]], [[XString]] is based on `byte` characters.
  *
  * [[XString]] is used in JET compiler and runtime to represent various strings.
  * Typically [[XString]] contains unicode chars encoded in "modified UTF-8" encoding,
  * however an [[XString]] may consist of any arbitrary sequence of bytes.
  *
  * @author alexm
  * @author paul
  */
class XString protected (_chars: Array[Byte], _offset: Int, private val count: Int, copyChars: Boolean) extends Ordered[XString] { self =>
  if (_offset < 0) throw new StringIndexOutOfBoundsException(_offset)
  if (count < 0) throw new StringIndexOutOfBoundsException(count)
  // Note: offset or count might be near -1>>>1.
  if (_offset > _chars.length - count) throw new StringIndexOutOfBoundsException(_offset + count)

  /** Byte array containing string characters. Its contents should never be changed. May be shared by several strings. */
  private val value: Array[Byte] = if (copyChars) _chars.slice(_offset, _offset + count) else _chars

  /** The index of the first string character in [[value]] array. */
  private val offset: Int = if (copyChars) 0 else _offset

  /** Cached hash code of the string, or `0` if hash is not calculated yet. */
  protected var hash: Int = 0

  private def this(chars: Array[Byte], copyChars: Boolean) = this(chars, 0, chars.length, copyChars)

  final def length = count
  final def isEmpty = count == 0
  final def nonEmpty = count > 0

  /** Returns the string character at the specified index.
    *
    * @param index Index of the character. Should be in range `0`..`length()-1`.
    * @return the character at the specified index as byte
    */
  final def charAt(index: Int): Byte = {
    if ((index < 0) || (index >= count)) throw new StringIndexOutOfBoundsException(index)
    value(index + offset)
  }

  /** Returns the string character at the specified index.
    *
    * @param index Index of the character. Should be in range `0`..`length()-1`.
    * @return the character at the specified index
    */
  final def charAtAsChar(index: Int): Char = (charAt(index) & 0xff).toChar

  override final def equals(obj: Any): Boolean = obj match {
    case that: AnyRef if this eq that => true
    case that: XString =>
      val thisID = internTableID
      val thatID = that.internTableID
      if (thisID != 0 && thatID != 0) {
        guarantee(thisID == thatID)
        false
      } else {
        contentEquals(that.value, that.offset, that.count)
      }
    case _ => false
  }

  /** Returns non-zero internTable ID if string is interned, otherwise returns zero.
    *
    * @return non-zero internTable ID if string is interned, otherwise zero
    */
  protected[common] def internTableID: Int = 0

  /** Compares contents of this string to given byte array region.
    *
    * @param arr    the array that contains the string to compare with
    * @param offset the start offset of the string to compare in the byte array
    * @param count  the length in bytes of the string to compare with
    * @return `true` iff this string contents is equal to the given byte array region
    */
  final def contentEquals(arr: Array[Byte], offset: Int, count: Int) =
    (this.count == count) && byteEquals(this.value, this.offset, arr, offset, count)

  /** Compares two strings lexicographically.
    *
    * @param that the string to be compared.
    * @return a negative integer, zero, or a positive integer as this object
    *         is less than, equal to, or greater than the specified object.
    */
  override final def compare(that: XString): Int = {
    val len1 = count
    val len2 = that.count
    var n = len1 min len2
    val v1 = value
    val v2 = that.value
    var i = offset
    var j = that.offset

    if (i == j) {
      var k = i
      val lim = n + i
      while (k < lim) {
        val c1 = v1(k)
        val c2 = v2(k)
        if (c1 != c2) return (c1 & 0xff) - (c2 & 0xff)
        k += 1
      }
    } else {
      while (n != 0) {
        n -= 1
        val c1 = v1(i)
        i += 1
        val c2 = v2(j)
        j += 1
        if (c1 != c2) return (c1 & 0xff) - (c2 & 0xff)
      }
    }
    len1 - len2
  }

  /** Returns a hash code of this string.
    * Note: it should be the same as [[com.huawei.excelsior.aj.util.BString# getHashCode]],
    * which also differs from Java [[String.hashCode]].
    *
    * @return hash code of this string
    */
  override final def hashCode() = {
    if (hash == 0 && count > 0) {
      hash = computeHashCode(value, offset, count)
    }
    hash
  }

  /** Returns the index within this string of the first occurrence of the
    * specified character, or `-1` if the character does not occur.
    *
    * @param ch the character to find
    * @return the first index of the character, or `-1`
    */
  final def indexOf(ch: Byte): Int = indexOf(ch, 0)

  /** Returns the index within this string of the first occurrence of the
    * specified character, starting the search at the specified index.
    * Returns `-1` if the character does not occur.
    *
    * @param ch        the character to find
    * @param fromIndex the start index to search from
    * @return the first index of the character greater or equal than `fromIndex`, or `-1`
    */
  final def indexOf(ch: Byte, fromIndex: Int): Int = indexOf(ch, fromIndex, count)

  /** Returns the index within this string of the first occurrence of the
    * specified character, starting the search at the specified index.
    * Returns `-1` if the character does not occur.
    *
    * @param ch        the character to find
    * @param fromIndex the start index to search from
    * @return the first index of the character greater or equal than `fromIndex`, or `-1`
    */
  final def indexOf(ch: Char, fromIndex: Int): Int = {
    assert(ch < 256)
    indexOf(ch.toByte, fromIndex, count)
  }

  final def indexOf(ch: Char): Int = indexOf(ch, 0)

  /** Returns the index within this string of the first occurrence of the
    * specified character, starting the search at the `fromIndex`
    * and ending the search before `endIndex`.
    * Returns `-1` if the character does not occur.
    *
    * @param ch        the character to find
    * @param fromIndex the start index to search from, inclusive
    * @param endIndex  the end index for the search, exclusive
    * @return the first index in the given range of the character, or `-1`
    */
  final def indexOf(ch: Byte, fromIndex: Int, endIndex: Int): Int = {
    if (fromIndex >= count || endIndex <= fromIndex) {
      return -1
    }

    val start = offset + (fromIndex max 0)
    val end = offset + (endIndex min count)
    val v = value

    var i = start
    while (i < end) {
      if (v(i) == ch) return i - offset
      i += 1
    }

    -1
  }

  /** Returns the index within this string of the last occurrence of the
    * specified character, or `-1` if the character does not occur.
    *
    * @param ch the character to find
    * @return the last index of the character, or `-1`
    */
  final def lastIndexOf(ch: Byte): Int = lastIndexOf(ch, count - 1)

  /** Returns the index within this string of the last occurrence of the
    * specified character, or `-1` if the character does not occur.
    *
    * @param ch the character to find
    * @return the last index of the character, or `-1`
    */
  final def lastIndexOf(ch: Char): Int = lastIndexOf(ch, count - 1)

  /** Returns the index within this string of the last occurrence of the
    * specified character, starting the search at the specified index.
    * Returns `-1` if the character does not occur.
    *
    * @param ch        the character to find
    * @param fromIndex the start index to search from
    * @return the last index of the character greater or equal than `fromIndex`, or `-1`
    */
  final def lastIndexOf(ch: Byte, fromIndex: Int): Int = {
    val min = offset
    val max = offset + (fromIndex min (count - 1))
    val v = value

    var i = max
    while (i >= min) {
      if (v(i) == ch) return i - offset
      i -= 1
    }

    -1
  }

  /** Returns the index within this string of the last occurrence of the
    * specified character, starting the search at the specified index.
    * Returns `-1` if the character does not occur.
    *
    * @param ch        the character to find
    * @param fromIndex the start index to search from
    * @return the last index of the character greater or equal than `fromIndex`, or `-1`
    */
  final def lastIndexOf(ch: Char, fromIndex: Int): Int = {
    assert(ch < 256)
    lastIndexOf(ch.toByte, fromIndex)
  }

  /** Returns the index within this string of the first occurrence of the
    * specified string, or `-1` if the character does not occur.
    *
    * @param str the substring to find
    * @return the first index of the substring occurrence, or `-1`
    */
  final def indexOf(str: XString): Int = indexOf(str, 0)

  /** Returns the index within this string of the first occurrence of the
    * specified string, starting the search at the specified index.
    * Returns `-1` if the character does not occur.
    *
    * @param str       the substring to find
    * @param fromIndex the start index to search from
    * @return the first index of the substring occurrence greater or equal than `fromIndex`, or `-1`
    */
  final def indexOf(str: XString, fromIndex: Int): Int = {
    val srcVal = value
    val srcOff = offset
    val srcCount = count

    val strVal = str.value
    val strOff = str.offset
    val strCount = str.count

    val fixedFromIndex = fromIndex max 0

    if (fixedFromIndex >= srcCount) {
      return if (strCount == 0) srcCount else -1
    }
    if (strCount == 0) return fixedFromIndex

    val first = strVal(strOff)
    val max = srcOff + (srcCount - strCount)
    var i = srcOff + fixedFromIndex

    while (i <= max) {
      // Look for first character.
      if (srcVal(i) != first) {
        i += 1
        while (i <= max && srcVal(i) != first) {
          i += 1
        }
      } // Found first character, now look at the rest of characters
      if (i <= max) {
        if (byteEquals(srcVal, i + 1, strVal, strOff + 1, strCount - 1)) {
          // Found whole string.
          return i - srcOff
        }
      }
      i += 1
    }

    -1
  }

  /** Returns a new string that is a substring of this string. The
    * substring begins with the character at the specified index and
    * extends to the end of this string.
    *
    * @param startIndex the start index of the substring to extract, inclusive
    * @return the extracted substring
    */
  final def substring(startIndex: Int): XString = substring(startIndex, count)

  /** Returns a new string that is a substring of this string. The
    * substring begins at the specified `beginIndex` and
    * extends to the character at index `endIndex - 1`.
    * Thus the length of the substring is `endIndex-beginIndex`.
    *
    * @param beginIndex the start index of the substring to extract, inclusive
    * @param endIndex   the end index of the substring to extract, exclusive
    * @return the extracted substring
    */
  final def substring(beginIndex: Int, endIndex: Int): XString = {
    if (beginIndex < 0) throw new StringIndexOutOfBoundsException(beginIndex)
    if (endIndex > count) throw new StringIndexOutOfBoundsException(endIndex)
    if (beginIndex > endIndex) throw new StringIndexOutOfBoundsException(endIndex - beginIndex)

    if ((beginIndex == 0) && (endIndex == count)) {
      this
    } else if (beginIndex == endIndex) {
      empty
    } else {
      unsafeWrap(value, offset + beginIndex, endIndex - beginIndex)
    }
  }

  /** Tests if this string starts with the specified prefix.
    *
    * @param prefix the prefix to check
    * @return `true` iff this string starts with the specified prefix
    */
  final def startsWith(prefix: XString): Boolean = startsWith(prefix, 0)

  /** Tests if the substring of this string beginning at the
    * specified index starts with the specified prefix.
    *
    * @param prefix     the prefix to check
    * @param startIndex the start index of the substring to check
    * @return `true` iff the substring of this string specified by `startIndex` starts with the specified prefix
    */
  final def startsWith(prefix: XString, startIndex: Int): Boolean = {
    val len = prefix.count
    if ((startIndex < 0) || (startIndex > count - len)) {
      false
    } else {
      byteEquals(value, offset + startIndex, prefix.value, prefix.offset, len)
    }
  }

  /** Tests if the substring of this string beginning at the
    * specified index starts with the specified prefix, ignoring case.
    * Only case of ASCII characters is ignored.
    *
    * @param prefix     the prefix to check
    * @param startIndex the start index of the substring to check
    * @return `true` iff the substring of this string specified by `startIndex` starts with the specified prefix
    */
  final def startsWithIgnoreCase(prefix: XString, startIndex: Int): Boolean = {
    var prefixCount = prefix.count
    if ((startIndex < 0) || (startIndex > count - prefixCount)) {
      return false
    }
    var sourceOffset = offset + startIndex
    var prefixOffset = prefix.offset
    while (prefixCount > 0) {
      if (asciiToUpperCase(value(sourceOffset)) != asciiToUpperCase(prefix.value(prefixOffset))) {
        return false
      }
      prefixCount -= 1
      sourceOffset += 1
      prefixOffset += 1
    }
    true
  }

  /** Tests if this string ends with the specified suffix.
    *
    * @param suffix the suffix to check
    * @return `true` iff this string ends with the specified suffix
    */
  final def endsWith(suffix: XString) = startsWith(suffix, count - suffix.count)

  /** Converts this string to uppercase. Only ASCII characters are converted.
    *
    * @return the copy of this string with all ASCII characters converted to upper case
    */
  final def toUpperCase: XString = {
    val len = count
    if (len == 0) return this
    val v = value
    val off = offset
    tabulate(len)(i => asciiToUpperCase(v(i + off)))
  }

  /** Splits this string around one byte character.
    * This implementation was copied from [[String.split( String]] with minor specialization.
    *
    * @param ch the character to split around
    * @return the array of this string parts that is the result of splitting around the given character
    */
  def split(ch: Byte): Array[XString] = {
    var off = 0
    val buf = new ArrayBuffer[XString]

    var next: Int = -1
    while ( {
      next = indexOf(ch, off); next != -1
    }) {
      buf += substring(off, next)
      off = next + 1
    }

    // If no match was found, return this
    if (off == 0) return Array[XString](this)

    // Add remaining segment
    buf += substring(off, length)

    // Construct result
    val resultSize = buf.lastIndexWhere(_.nonEmpty) + 1
    buf.takeInPlace(resultSize).toArray
  }

  /** Returns input stream consisting of this string characters.
    *
    * @return input stream consisting of this string characters
    */
  final def asInput = DataInput.from(value, offset, count)

  /** Copies characters from the string into byte array.
    *
    * @param srcBegin the start index in this string of the characters to copy, inclusive
    * @param srcEnd   the start index in this string of the characters to copy, exclusive
    * @param dst      the destination byte array
    * @param dstBegin the start index in the destination byte array to copy the characters to
    */
  final def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Byte], dstBegin: Int): Unit = {
    if (srcBegin < 0) throw new StringIndexOutOfBoundsException(srcBegin)
    if (srcEnd > count) throw new StringIndexOutOfBoundsException(srcEnd)
    if (srcBegin > srcEnd) throw new StringIndexOutOfBoundsException
    Array.copy(value, offset + srcBegin, dst, dstBegin, srcEnd - srcBegin)
  }

  /** Copy characters from this string into dst starting at dstBegin.
    *
    * @param dst      the destination byte array
    * @param dstBegin the start index in the destination byte array to copy the characters to
    */
  final def getChars(dst: Array[Byte], dstBegin: Int): Unit = {
    Array.copy(value, offset, dst, dstBegin, count)
  }

  /** Copy characters from this string into output stream.
    *
    * @param out the output stream
    */
  final def appendTo(out: DataOutput): Unit = {
    out.putBytes(value, offset, count)
  }

  /** Concatenates the specified string to the end of this string.
    *
    * @param str the string to be appended to this one
    * @return the concatenated string
    */
  final def concat(str: XString): XString = {
    val otherLen = str.length
    if (otherLen == 0) return this
    val buf = new Array[Byte](count + otherLen)
    getChars(buf, 0)
    str.getChars(buf, count)
    unsafeWrap(buf)
  }

  /** Returns a new string resulting from replacing all occurrences of
    * `oldChar` in this string with `newChar`.
    *
    * @param oldChar the character to be replaced
    * @param newChar the character to use for replacement
    * @return a new string resulting from replacing all occurrences of `oldChar` with `newChar`
    */
  final def replace(oldChar: Byte, newChar: Byte): XString = {
    if (oldChar == newChar) return this

    val len = count
    val v = value
    val off = offset
    var i = 0

    while (i < len && v(i + off) != oldChar) {
      i += 1
    }

    if (i == len) return this

    val buf = new Array[Byte](len)
    Array.copy(v, off, buf, 0, i)
    buf(i) = newChar
    i += 1

    while (i < len) {
      val ch = v(i + off)
      if (ch == oldChar) {
        buf(i) = newChar
      } else {
        buf(i) = ch
      }
      i += 1
    }

    unsafeWrap(buf)
  }

  /** Returns a new string resulting from replacing all occurrences of
    * `oldChar` in this string with `newChar`.
    *
    * @param oldChar the character to be replaced
    * @param newChar the character to use for replacement
    * @return a new string resulting from replacing all occurrences of `oldChar` with `newChar`
    */
  final def replace(oldChar: Char, newChar: Char): XString = {
    assert((oldChar < 256) && (newChar < 256))
    replace(oldChar.toByte, newChar.toByte)
  }

  /** Removes the leading and trailing whitespace.
    * Only space and tab characters are considered whitespace.
    *
    * @return a substring of this string with removed leading and trailing whitespaces
    */
  final def trim() = {
    var len = count
    var st = 0
    val off = offset
    val v = value

    while (st < len && isWhiteSpace(v(off + st))) {
      st += 1
    }
    while (st < len && isWhiteSpace(v(off + len - 1))) {
      len -= 1
    }

    if ((st > 0) || (len < count)) {
      substring(st, len)
    } else {
      this
    }
  }

  /** Returns a [[String]] representation of this ASCII or modified UTF-8 string.
    *
    * @return a [[String]] representation of this ASCII or modified UTF-8 string.
    */
  override final def toString = utf8ToString

  // =========== Conversion between Modified UTF-8 and unicode/UTF-16 used by Java strings ==============

  /** Returns `true` if unicode supplementary character is encoded at given position of the string.
    *
    * @param pos the position to check
    * @return `true` iff unicode supplementary character is encoded at given position of the string
    */
  final def isSupplementaryCharacterAt(pos: Int) = {
    if ((pos < 0) || (pos >= count)) {
      throw new StringIndexOutOfBoundsException(pos)
    }

    (pos + SUPPLEMENTARY_CHARACTER_LENGTH <= count) &&
      ((value(pos + 0) & 0xFF) == 0xED) &&
      ((value(pos + 1) & 0xF0) == 0xA0) &&
      ((value(pos + 2) & 0xC0) == 0x80) &&
      ((value(pos + 3) & 0xFF) == 0xED) &&
      ((value(pos + 4) & 0xF0) == 0xB0) &&
      ((value(pos + 5) & 0xC0) == 0x80)
  }

  /** Returns unicode supplementary character at given position of the string.
    *
    * @param pos the position of unicode supplementary character
    * @return unicode supplementary character at given position
    */
  final def getSupplementaryCharacterAt(pos: Int) = {
    if ((pos < 0) || (pos > count - SUPPLEMENTARY_CHARACTER_LENGTH)) {
      throw new StringIndexOutOfBoundsException(pos)
    }
    if (!isSupplementaryCharacterAt(pos)) {
      throw new IllegalArgumentException(s"Supplementary character expected at position $pos")
    }

    0x10000 +
      ((value(pos + 1) & 0x0F) << 16) +
      ((value(pos + 2) & 0x3F) << 10) +
      ((value(pos + 4) & 0x0F) << 6) +
      ((value(pos + 5) & 0x3F) << 0)
  }

  /** Returns unicode code point at the given position of the string.
    *
    * @param pos the position in this string
    * @return unicode code point at the given position
    */
  final def unicodeCodePointAt(pos: Int): Int = {
    if (isSupplementaryCharacterAt(pos)) {
      getSupplementaryCharacterAt(pos)
    } else {
      unicodeCharAt(pos)
    }
  }

  /** Returns the length in bytes of the unicode code point at the given position of the string.
    * May be used to calculate the position of the next unicode point
    *
    * @param pos the position in this string
    * @return the length in bytes of the unicode code point at the given position
    */
  final def lengthOfUnicodeCodePointAt(pos: Int): Int = {
    if (isSupplementaryCharacterAt(pos)) {
      SUPPLEMENTARY_CHARACTER_LENGTH
    } else {
      lengthOfUnicodeCharAt(pos)
    }
  }

  /** Returns unicode character at the given position of the string.
    * This method cannot handle unicode supplementary characters.
    *
    * @param pos the position in this string
    * @return unicode character at the given position
    */
  final def unicodeCharAt(pos: Int): Char = {
    val b0 = charAt(pos)

    if (((b0 & 0xE0) == 0xC0) && ((pos + 1) < count)) {
      // 110xxxxx 10xxxxxx
      val b1 = charAt(pos + 1)
      if ((b1 & 0xC0) == 0x80) {
        return (((b0 & 0x1F) << 6) + (b1 & 0x3F)).toChar
      }
    } else if (((b0 & 0xF0) == 0xE0) && ((pos + 2) < count)) {
      // 1110xxxx 10xxxxxx 10xxxxxx
      val b1 = charAt(pos + 1)
      if ((b1 & 0xC0) == 0x80) {
        val b2 = charAt(pos + 2)
        if ((b2 & 0xC0) == 0x80) {
          return (((b0 & 0x0F) << 12) + ((b1 & 0x3F) << 6) + (b2 & 0x3F)).toChar
        }
      }
    }

    // 1-byte or incorrect UTF-8
    (b0 & 0xFF).toChar
  }

  /** Returns iterator over modified UTF-8 string as UTF-16 characters. */
  def unicodeIterator: Iterator[Char] = new Iterator[Char] {
    var pos = 0

    def hasNext = pos < self.count

    def next() = {
      if (!hasNext) Iterator.empty.next() else {
        val result = unicodeCharAt(pos)
        pos += lengthOfUnicodeCharAt(pos)
        guarantee(pos <= self.count)
        result
      }
    }
  }

  /** Returns length of the unicode character at given position.
    * This method cannot handle unicode supplementary characters.
    *
    * @param pos the position in this string
    * @return the length in bytes of the unicode character at the given position
    */
  final def lengthOfUnicodeCharAt(pos: Int): Int = {
    val b0 = charAt(pos)

    if (((b0 & 0xE0) == 0xC0) && ((pos + 1) < count)) {
      // 110xxxxx 10xxxxxx
      val b1 = charAt(pos + 1)
      if ((b1 & 0xC0) == 0x80){
        return 2
      }
    } else if (((b0 & 0xF0) == 0xE0) && ((pos + 2) < count)) {
      // 1110xxxx 10xxxxxx 10xxxxxx
      val b1 = charAt(pos + 1)
      if ((b1 & 0xC0) == 0x80) {
        val b2 = charAt(pos + 2)
        if ((b2 & 0xC0) == 0x80) {
          return 3
        }
      }
    }

    // 1-byte or incorrect UTF-8
    1
  }

  /** Converts this string from modified UTF-8 encoding to [[java.lang.String]].
    *
    * @return the converted string
    */
  final def utf8ToString: String = ModifiedUtf8Encoding.decodeStringPreserving(value, offset, count)

  /** Converts this string from platform default encoding to [[java.lang.String]].
    *
    * @return the converted string
    */
  final def platformToString = PlatformEncoding.native.decodeStringReplacing(value, offset, count)

  /** Converts this ASCII or modified UTF-8 string to bytes using the platform default encoding
    *
    * @return the converted string as byte array
    */
  final def toPlatformBytes = PlatformEncoding.native.encodeStringReplacing(toString)

  /** Calculate Java [[java.lang.String.hashCode]] of this string.
    * This string should be encoded in the modified UTF-8 encoding.
    *
    * @return the hash code calculated by Java rules
    */
  final def getJavaHashCode = {
    var res = 0
    for (ch <- unicodeIterator) {
      res = res * 31 + ch
    }
    res
  }

  /** Compares this string to another string.
    *
    * @param str2 the string to compare with
    * @return `true` iff this string is equal to another
    */
  final def equals2(str2: String) = toString == str2 //TODO-DECAF: remove when XString literals will be introduced

  def foreach[U](f: Byte => U): Unit = {
    var i = 0
    while (i < count) {
      f(value(i + offset))
      i += 1
    }
  }
}

object XString {

  def xstr(str: String) = apply(str)

  /** Creates modified utf-8 [[XString]] by given [[java.lang.String]]. */
  def apply(str: String): XString = if (str == null) null else unsafeWrap(decodeContent(str, isASCII = false))

  /** Returns {@link String} representation */
  def unapply(xstr: XString) = Some(xstr.toString)

  /** Creates [[XString]] by given ASCII [[java.lang.String]]. */
  def ascii(str: String) = unsafeWrap(decodeContent(str, isASCII = true))

  /** Creates a string by given characters. The characters are copied. */
  def apply(chars: Array[Byte]): XString = slice(chars, 0, chars.length)

  /** Creates a string by given characters. The characters are copied. */
  def slice(value: Array[Byte], offset: Int, count: Int) = {
    if (count == 0) empty else new XString(value, offset, count, copyChars = true)
  }

  /** Converts one byte character to upper case. Only ASCII characters are converted.
    *
    * @param ch the character to convert
    * @return the converted to uppercase character, if it was ASCII, or unchanged non-ASCII character otherwise
    */
  private def asciiToUpperCase(ch: Byte): Byte = {
    if (('a' <= ch) && (ch <= 'z')) {
      (ch - 'a' + 'A').toByte
    } else {
      ch
    }
  }

  def fill(len: Int)(elem: => Byte): XString = {
    unsafeWrap(Array.fill(len)(elem))
  }

  def tabulate(len: Int)(f: Int => Byte): XString = {
    unsafeWrap(Array.tabulate(len)(f))
  }

  /** Unsafe XString operations. Use with caution! */

  /** Obtains the byte array that stores the characters of the given string (and maybe other strings too).
    *
    * @param str the string instance
    * @return the raw storage of the string's characters
    */
  def unsafeGetValue(str: XString) = str.value

  /** Returns the offset of the given string's characters in the byte array returned by [[XString.unsafeGetValue()]].
    *
    * @param str the string instance
    * @return the offset of the string's characters in the byte array returned by [[XString.unsafeGetValue()]]
    */
  def unsafeGetOffset(str: XString) = str.offset

  /** Creates a new [[XString]] without copying characters.
    *
    * @param buffer Byte array containing string characters. Its contents should never be changed.
    * @param offset The initial offset of the string in the `buffer`.
    * @param count  String length.
    * @return the string instance that uses the given byte array as the storage of its characters
    */
  def unsafeWrap(buffer: Array[Byte], offset: Int, count: Int): XString = {
    new XString(buffer, offset, count, copyChars = false)
  }

  /** Creates a new [[XString]] without copying characters.
    *
    * @param buffer Byte array containing string characters. Its contents should never be changed.
    * @return the string instance that uses the given byte array as the storage of its characters
    */
  def unsafeWrap(buffer: Array[Byte]): XString = {
    unsafeWrap(buffer, 0, buffer.length)
  }

  private def isWhiteSpace(ch: Byte) = (ch == ' ') || (ch == '\t')

  /** Internal assertion check which is always enabled. */
  private[common] def guarantee(condition: Boolean): Unit = {
    if (!condition) throw new InternalError("XString guarantee failed")
  }

  /** Length of unicode supplementary character encoded in modified UTF-8. */
  private val SUPPLEMENTARY_CHARACTER_LENGTH = 6

  /** Helper methods to work with byte arrays. */

  /** Compares two regions of given byte arrays.
    *
    * @param buf1  the byte array that contains the first region to compare
    * @param _ofs1  the offset of the first region to compare
    * @param buf2  the byte array that contains the second region to compare
    * @param _ofs2  the offset of the second region to compare
    * @param _count the length in bytes of the regions
    * @return `true` iff the contents of the regions is equal
    */
  def byteEquals(buf1: Array[Byte], _ofs1: Int, buf2: Array[Byte], _ofs2: Int, _count: Int): Boolean = {
    var count = _count
    var ofs1 = _ofs1
    var ofs2 = _ofs2
    while (count != 0) {
      if (buf1(ofs1) != buf2(ofs2)) {
        return false
      }
      count -= 1
      ofs1 += 1
      ofs2 += 1
    }
    true
  }

  /** Calculates hash code of a byte array region.
    *
    * @param buf    the byte array that contains the region
    * @param offset the offset of the region
    * @param count  the length in bytes of the region
    * @return the hash code of the region
    */
  def computeHashCode(buf: Array[Byte], offset: Int, count: Int) = {
    val end = offset + count
    var h = 0
    for (i <- offset until end) {
      h = 31 * h + (buf(i) & 0xff)
    }
    h
  }

  /** Converts a Java String to ASCII or Modified UTF-8 sequence of bytes.
    *
    * @param str     a Java String to convert
    * @param isASCII whether `str` is expected to be ASCII String
    * @return the converted byte array
    * @throws IllegalArgumentException if `isASCII` is `true` but non-ASCII characters are found
    */
  def decodeContent(str: String, isASCII: Boolean): Array[Byte] = {

    def isBasicLatin(ch: Int) = (0x0001 <= ch) && (ch <= 0x007F)

    if (isASCII) {
      val length = str.length
      val value = new Array[Byte](length)
      for (i <- 0 until length) {
        val ch = str.charAt(i)
        if (!isBasicLatin(ch)) throw new IllegalArgumentException(s"ASCII string expected: \"$str\"")
        value(i) = ch.toByte
      }
      value
    } else {
      ModifiedUtf8Encoding.encodeStringPreserving(str)
    }
  }

  /** Class containing string constants. It is a separate class to avoid clinit in XString. */
  private object Constants {
    /** Empty string. */
    val EMPTY = unsafeWrap(new Array[Byte](0))
  }

  def empty = Constants.EMPTY
}
