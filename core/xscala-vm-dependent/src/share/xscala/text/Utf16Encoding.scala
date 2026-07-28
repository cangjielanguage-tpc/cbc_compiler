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
import scala.util.boundary.Label
import UtfUtils.*

final class Utf16Encoding private(private var little: Boolean, private var big: Boolean) extends Encoding {

  private inline def put(state: EncodingState, c: Char, output: EncodingOutputByte)(using exit: Label[Unit]): Unit = {
    val high = (c >> 8).toByte
    val low = (c & 0xFF).toByte
    if (little) {
      output(state, low)
      output(state, high)
    } else {
      output(state, high)
      output(state, low)
    }
  }

  private inline def decodeLittle(b1: Int, b2: Int) = (((b2 & 0xFF) << 8) | (b1 & 0xFF)).toChar

  private inline def decodeBig(b1: Int, b2: Int) = (((b1 & 0xFF) << 8) | (b2 & 0xFF)).toChar

  private inline def decode(b1: Int, b2: Int): Char = {
    if (little) {
      decodeLittle(b1, b2)
    } else {
      val bigResult = decodeBig(b1, b2)
      if (big) {
        bigResult
      } else {
        if (bigResult == REVERSED_BOM) {
          little = true
          decodeLittle(b1, b2)
        } else if (bigResult == EXPECTED_BOM) {
          big = true
          bigResult
        } else {
          bigResult
        }
      }
    }
  }

  override def encodeStep(state: EncodingState, input: EncodingInputCharIterator, output: EncodingOutputByte): Unit = {
    boundary {
      val peek = input.peekIterator
      peek.next(state) { c0 =>
        if (isSurrogate(c0)) {
          if (isHighSurrogate(c0)) {
            peek.next(state, isLowSurrogate) { c1 =>
              input.assertNext(c0, c1)
              put(state, c0, output)
              put(state, c1, output)
            }
          } else {
            state.setMalformed()
            break()
          }
        } else {
          input.assertNext(c0)
          put(state, c0, output)
        }
      }
    }
  }

  override def decodeStep(state: EncodingState, input: EncodingInputByteIterator, output: EncodingOutputChar): Unit = {
    boundary {
      val peek = input.peekIterator
      peek.next(state) { b0 =>
        peek.next(state) { b1 =>
          val c0 = decode(b0, b1)

          if (isSurrogate(c0)) {
            if (isHighSurrogate(c0)) {
              peek.next(state) { b2 =>
                peek.next(state) { b3 =>
                  val c1 = decode(b2, b3)
                  if (isLowSurrogate(c1)) {
                    input.assertNext(b0, b1, b2, b3)
                    output(state, c0)
                    output(state, c1)
                  } else {
                    state.setMalformed()
                    break()
                  }
                }
              }
            } else {
              state.setMalformed()
              break()
            }
          } else {
            input.assertNext(b0, b1)
            output(state, c0)
          }
        }
      }
    }
  }

}

object Utf16Encoding {
  val Big = Utf16Encoding(false, true)
  val Little = Utf16Encoding(true, false)
  def Detect = Utf16Encoding(false, false)
}

