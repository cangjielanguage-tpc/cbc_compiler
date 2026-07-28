/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

import scala.util.boundary
import scala.util.boundary.break

object ModifiedUtf8Encoding extends Encoding {

  override def encodeStep(state: EncodingState, input: EncodingInputCharIterator, output: EncodingOutputByte): Unit = {
    boundary {
      val ch = input.next()
      if ((0x0001 <= ch) && (ch <= 0x007F)) {
        output(state, ch.toByte)
      } else if (ch > 0x07FF) {
        output(state, (0xE0 | ((ch >> 12) & 0x0F)).toByte)
        output(state, (0x80 | ((ch >> 6) & 0x3F)).toByte)
        output(state, (0x80 | ((ch >> 0) & 0x3F)).toByte)
      } else {
        output(state, (0xC0 | ((ch >> 6) & 0x1F)).toByte)
        output(state, (0x80 | ((ch >> 0) & 0x3F)).toByte)
      }
    }
  }

  override def decodeStep(state: EncodingState, input: EncodingInputByteIterator, output: EncodingOutputChar): Unit = {
    boundary {
      val peek = input.peekIterator
      peek.next(state) { b0 =>
        (b0 & 0xFF) >>> 4 match {
          case 0xC | 0xD =>
            // 110xxxxx 10xxxxxx
            peek.next(state, c => (c & 0xC0) == 0x80) { b1 =>
              val highFive = b0 & 0x1F
              val lowSix = b1 & 0x3F
              input.assertNext(b0, b1)
              output(state, ((highFive << 6) + lowSix).toChar)
            }
          case 0xE =>
            // 1110xxxx 10xxxxxx 10xxxxxx
            peek.next(state, c => (c & 0xC0) == 0x80) { b1 =>
              peek.next(state, c => (c & 0xC0) == 0x80) { b2 =>
                val highFour = b0 & 0x0F
                val midSix = b1 & 0x3F
                val lowSix = b2 & 0x3F
                input.assertNext(b0, b1, b2)
                output(state, ((((highFour << 6) + midSix) << 6) + lowSix).toChar)
              }
            }
          case _ =>
            if (b0 > 0) {
              // Single byte
              input.assertNext(b0)
              output(state, b0.toChar)
            } else {
              state.setMalformed()
              break()
            }
        }
      }
    }
  }

  // The following are replaced in JET

  override def encodeReplacing(array: Array[Char], start: Int, length: Int) = super.encodeReplacing(array, start, length)

  override def decodeReplacing(array: Array[Byte], start: Int, length: Int) = super.decodeReplacing(array, start, length)

  override def encodePreserving(array: Array[Char], start: Int, length: Int) = super.encodePreserving(array, start, length)

  override def decodePreserving(array: Array[Byte], start: Int, length: Int) = super.decodePreserving(array, start, length)

  override def encodeThrowing(array: Array[Char], start: Int, length: Int) = super.encodeThrowing(array, start, length)

  override def decodeThrowing(array: Array[Byte], start: Int, length: Int) = super.decodeThrowing(array, start, length)

  override def encodeStringReplacing(string: String, start: Int, length: Int) = super.encodeStringReplacing(string, start, length)

  override def decodeStringReplacing(array: Array[Byte], start: Int, length: Int) = super.decodeStringReplacing(array, start, length)

  override def encodeStringPreserving(string: String, start: Int, length: Int) = super.encodeStringPreserving(string, start, length)

  override def decodeStringPreserving(array: Array[Byte], start: Int, length: Int) = super.decodeStringPreserving(array, start, length)

  override def encodeStringThrowing(string: String, start: Int, length: Int) = super.encodeStringThrowing(string, start, length)

  override def decodeStringThrowing(array: Array[Byte], start: Int, length: Int) = super.decodeStringThrowing(array, start, length)
}
