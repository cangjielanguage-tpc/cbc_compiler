/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.util.MathUtils.isNBits

import scala.annotation.tailrec

/** Utility methods for signed and unsigned LEB128 encoding.
  *
  * @author alexm
  * @author liontiger
  */
object LEB128Encoder {
  /** Encodes given unsigned value with unsigned LEB128 variable-length encoding and writes the result. */
  def encodeULEB128(value: Long, writeByte: Int => Unit): Unit = {
    encodeULEB128Tail(value, writeByte)
  }

  @tailrec
  private def encodeULEB128Tail(value: Long, writeByte: Int => Unit): Unit = {
    val b = (value & 0x7f).toInt
    val remaining = value >>> 7
    if (remaining == 0) {
      writeByte(b)
    } else {
      writeByte(0x80 | b)
      encodeULEB128Tail(remaining, writeByte)
    }
  }

  /** Calculates number of bytes that will unsigned LEB128 encoded `value` occupy */
  def calcSizeULEB128(value: Long): Int = calcSizeULEB128Tail(value, 1)

  @tailrec
  private def calcSizeULEB128Tail(value: Long, sizePart: Int): Int = {
    val shifted = value >>> 7
    if (shifted == 0) {
      sizePart
    } else {
      calcSizeULEB128Tail(shifted, sizePart + 1)
    }
  }

  /** Reads and decodes unsigned LEB128 value. */
  def decodeULEB128Long(readByte: () => Int) = decodeULEB128Tail(0, 0, readByte)

  def decodeULEB128(readByte: () => Int) = decodeULEB128Long(readByte).ensuring(isNBits(_, 32)).toInt

  @tailrec
  private def decodeULEB128Tail(valuePart: Long, shift: Int, readByte: () => Int): Long = {
    val bitsPerLong = 64
    assert((0 <= shift) && (shift < bitsPerLong))
    val b = readByte().toLong
    val value = valuePart | ((b & 0x7f) << shift)
    if ((b & 0x80) == 0) {
      value
    } else {
      decodeULEB128Tail(value, shift + 7, readByte)
    }
  }

  /** Encodes given signed value with signed LEB128 variable-length encoding and writes the result. */
  def encodeSLEB128(value: Long, writeByte: Int => Unit): Unit = {
    encodeSLEB128Tail(value, writeByte)
  }

  @tailrec
  private def encodeSLEB128Tail(value: Long, writeByte: Int => Unit): Unit = {
    val b = (value & 0x7f).toInt
    val signBit = b & 0x40
    val remaining = value >> 7
    if (((remaining == 0) && (signBit == 0)) || ((remaining == -1) && (signBit != 0))) {
      writeByte(b)
    } else {
      writeByte(0x80 | b)
      encodeSLEB128Tail(remaining, writeByte)
    }
  }

  /** Calculates number of bytes that will signed LEB128 encoded `value` occupy */
  def calcSizeSLEB128(value: Long) = calcSizeSLEB128Tail(value, 1)

  @tailrec
  private def calcSizeSLEB128Tail(value: Long, sizePart: Int): Int = {
    val b = (value & 0x7f).toInt
    val signBit = b & 0x40
    val remaining = value >> 7
    if (((remaining == 0) && (signBit == 0)) || ((remaining == -1) && (signBit != 0))) {
      sizePart
    } else {
      calcSizeSLEB128Tail(remaining, sizePart + 1)
    }
  }

  /** Reads and decodes signed LEB128 value. */
  def decodeSLEB128Long(readByte: () => Int) = decodeSLEB128Tail(0, 0, readByte)

  def decodeSLEB128(readByte: () => Int) = decodeSLEB128Long(readByte).ensuring(_.isValidInt).toInt

  @tailrec
  private def decodeSLEB128Tail(valuePart: Long, shift: Int, readByte: () => Int): Long = {
    val bitsPerLong = 64
    assert((0 <= shift) && (shift < bitsPerLong))
    val b = readByte().toLong
    val value = valuePart | ((b & 0x7f) << shift)
    val nextShift = shift + 7
    if ((b & 0x80) == 0) {
      if ((nextShift < bitsPerLong) & ((b & 0x40) != 0)) {
        // sign extend
        value | (-(1L << nextShift))
      } else {
        value
      }
    } else {
      decodeSLEB128Tail(value, nextShift, readByte)
    }
  }
}
