/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.time

object TimeUnit {
  private def safeMultiply(a: Long, b: Long): Long = {
    val m: Long = Long.MaxValue / b
    if (a > m) Long.MaxValue
    else if (a < -m) Long.MinValue
    else a * b
  }
}

enum TimeUnit(private val scale: Long) {
  case Nanoseconds  extends TimeUnit(                 1L)
  case Microseconds extends TimeUnit(             1_000L)
  case Milliseconds extends TimeUnit(         1_000_000L)
  case Seconds      extends TimeUnit(     1_000_000_000L)
  case Minutes      extends TimeUnit(    60_000_000_000L)
  case Hours        extends TimeUnit( 3_600_000_000_000L)
  case Days         extends TimeUnit(86_400_000_000_000L)

  def convert(duration: Long, source: TimeUnit): Long = {
    if (this == source) duration // Nothing to do
    else if (source.scale < this.scale) duration / (this.scale / source.scale) // Reduce numerical value
    else TimeUnit.safeMultiply(duration, source.scale / this.scale) // Increase numerical value
  }
}
