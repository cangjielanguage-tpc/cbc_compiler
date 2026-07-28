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

object USAsciiEncoding extends Encoding {

  override def encodeStep(state: EncodingState, input: EncodingInputCharIterator, output: EncodingOutputByte): Unit = {
    boundary {
      val peek = input.peekIterator
      peek.next(state) { c0 =>
        if (c0 <= 0x007F) {
          input.assertNext(c0)
          output(state, c0.toByte)
        } else {
          state.setUnmappable()
          break()
        }
      }
    }
  }

  override def decodeStep(state: EncodingState, input: EncodingInputByteIterator, output: EncodingOutputChar): Unit = {
    boundary {
      val peek = input.peekIterator
      peek.next(state, c => c >= 0) { b0 =>
        input.assertNext(b0)
        output(state, b0.toChar)
      }
    }
  }

}
