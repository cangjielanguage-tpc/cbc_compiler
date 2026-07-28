/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.time

case class LocalDateTime(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int, millisecond: Int) {
  def toString(format: String): String = toString(LocalDateTime.Formatter(format))

  def toString(format: LocalDateTime.Formatter): String = format.format(this)

  override def toString: String = toString(LocalDateTime.Formatter.DEFAULT)
}

object LocalDateTime {
  private type PartFormatter = (LocalDateTime, StringBuilder) => Unit

  def now: LocalDateTime = TimeVMDependent.get.nowLocalDateTime()

  final class Formatter private(val parts: Seq[PartFormatter]) {
    def format(value: LocalDateTime): String = {
      val sb = new StringBuilder()
      for (part <- parts) {
        part(value, sb)
      }
      sb.result()
    }
  }

  object Formatter {
    val DEFAULT = apply("yyyy-MM-dd HH:mm:ss")

    private def leftPad(contentsPart: PartFormatter, width: Int, fill: Char): PartFormatter = {
      (value, output) => {
        val temp = new StringBuilder()
        contentsPart(value, temp)
        for (_ <- temp.length until width) {
          output.append(fill)
        }
        output.append(temp)
      }
    }

    private def part(getter: LocalDateTime => Int, width: Int): PartFormatter = leftPad(
      (value, output) => {
        output.append(getter(value))
      },
      width, '0'
    )

    private def constant(c: Char): PartFormatter = (*, output) => {
      output.append(c)
    }

    private def year(width: Int): PartFormatter = part(_.year, width)

    private def month(width: Int): PartFormatter = part(_.month, width)

    private def day(width: Int): PartFormatter = part(_.day, width)

    private def hour(width: Int): PartFormatter = part(_.hour, width)

    private def minute(width: Int): PartFormatter = part(_.minute, width)

    private def second(width: Int): PartFormatter = part(_.second, width)

    def apply(format: String): Formatter = {
      import scala.collection.mutable.ArrayBuffer
      var pos = 0
      val length = format.length
      val parts = ArrayBuffer.empty[PartFormatter]
      while (pos < length) {
        val c = format.charAt(pos)
        if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) {
          def countWidth: Int = {
            var p = pos + 1
            while (p < length) {
              if (format.charAt(p) != c) {
                return p - pos
              }
              p += 1
            }
            p - pos
          }

          val width = countWidth
          val part = if (c == 'y') {
            year(width)
          } else if (c == 'M') {
            month(width)
          } else if (c == 'd') {
            day(width)
          } else if (c == 'H') {
            hour(width)
          } else if (c == 'm') {
            minute(width)
          } else if (c == 's') {
            second(width)
          } else {
            throw RuntimeException(s"Format string '$format' has unknown symbol '$c' at index $pos")
          }
          parts.addOne(part)
          pos += width
        } else {
          parts.addOne(constant(c))
          pos += 1
        }
      }
      new Formatter(parts.toSeq)
    }
  }
}
