/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

object BigEndian {
  def apply(out: DataOutput) = new Output(out)

  def apply(in: DataInput) = new Input(in)

  class Output(underlying: DataOutput) {
    def putW8(w8: Int): Unit = underlying.putW8(w8)

    def putW16(w16: Int): Unit = {
      putW8((w16 >>> 8) & 0xFF)
      putW8(w16 & 0xFF)
    }

    def putW32(w32: Int): Unit = {
      putW16(w32 >>> 16)
      putW16(w32 & 0xFFFF)
    }

    def putW64(w64: Long): Unit = {
      putW32((w64 >>> 32).toInt)
      putW32(w64.toInt)
    }
  }

  class Input(underlying: DataInput) {
    def getW8(): Byte = underlying.getW8()

    def getUW8(): Int = underlying.getUW8()

    def getW16(): Short = ((getUW8() << 8) | getUW8()).toShort

    def getUW16(): Int = 0xFFFF & getW16()

    def getW32(): Int = (getUW16() << 16) | getUW16()

    def getUW32(): Long = 0xFFFFFFFFL & getW32().toLong

    def getW64(): Long = (getUW32() << 32) | getUW32()
  }
}


