
/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.o2s.runtime

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import xscala.util.{Set32, UByte, UInt, ULong, UShort}

import scala.util.control.Breaks

object O2SSupport {

  object Keywords {

    private val breaks = new Breaks

    def loop(body: => Unit): Unit = {
      breaks.breakable {
        while (true) {
          body
        }
      }
    }

    def infiniteLoop(body: => Unit): Nothing = {
      loop(body)
      shouldNotReachHere()
    }

    def break() = breaks.break()

    implicit class ByteForRange(val self: Byte) extends AnyVal {
      def to(end: Byte) = new ByteRange(self, end, 1)
    }

    implicit class ShortForRange(val self: Short) extends AnyVal {
      def to(end: Short) = new ShortRange(self, end, 1)
    }

    implicit class ConvertableByte(val self: Byte) extends AnyVal {
      def toUByte  = UByte(self)
      def toUShort = UShort(self)
      def toUInt   = UInt(self)
      def toSet32  = Set32(self)
    }

    implicit class ConvertableShort(val self: Short) extends AnyVal {
      def toUByte  = UByte(self)
      def toUShort = UShort(self)
      def toUInt   = UInt(self)
      def toSet32  = Set32(self)
    }

    implicit class ConvertableInt(val self: Int) extends AnyVal {
      def toUByte  = UByte(self)
      def toUShort = UShort(self)
      def toUInt   = UInt(self)
      def toSet32  = Set32(self)
    }

    implicit class ConvertableLong(val self: Long) extends AnyVal {
      def toUInt   = UInt(self.toInt)
      def toULong  = ULong(self)
    }
  }

  // short integral ranges

  class ByteRange(start: Int, end: Int, step: Int) extends IndexedSeq[Byte] {
    private val range = new Range.Inclusive(start, end, step)

    def length = range.length
    def apply(idx: Int) = range(idx).toByte

    def by(step: Int) = new ByteRange(start, end, step)
  }

  class ShortRange(start: Int, end: Int, step: Int) extends IndexedSeq[Short] {
    private val range = new Range.Inclusive(start, end, step)

    def length = range.length
    def apply(idx: Int) = range(idx).toShort

    def by(step: Int) = new ShortRange(start, end, step)
  }

}
