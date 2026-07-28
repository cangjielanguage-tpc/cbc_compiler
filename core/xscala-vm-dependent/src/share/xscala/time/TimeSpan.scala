/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.time

case class TimeSpan private(totalSeconds: Long, private val nanos: Int) {
  assert(0 <= nanos && nanos < 1_000_000_000)

  def totalNanoseconds: Long = totalSeconds * 1_000_000_000 + nanos

  def nanoseconds: Int = nanos % 1_000

  def totalMicroseconds: Long = totalSeconds * 1_000_000 + nanos / 1_000

  def microseconds: Int = nanos / 1_000 % 1_000

  def totalMilliseconds: Long = totalSeconds * 1_000 + milliseconds

  def milliseconds: Int = nanos / 1_000_000

  def second: Int = (totalSeconds % 60).toInt

  def totalMinutes: Long = totalSeconds / 60

  def minute: Int = (totalMinutes % 60).toInt

  def totalHours: Long = totalSeconds / 60 / 60

  def hour: Int = (totalHours % 24).toInt

  def totalEpochDays: Long = totalSeconds / 60 / 60 / 24

  override def toString = {
    val sb = new StringBuilder()
    sb.append(totalSeconds)
    sb.append('.')
    val nanos = this.nanos.toString
    for (_ <- nanos.length until 9) {
      sb.append('0')
    }
    sb.append(nanos)
    sb.result()
  }
}

object TimeSpan {
  def now: TimeSpan = nowCoarse

  def nowCoarse: TimeSpan = fromMilliseconds(TimeVMDependent.get.nowMilliseconds())

  def nowPrecise: TimeSpan = fromNanoseconds(TimeVMDependent.get.nowNanoseconds())

  def fromMilliseconds(value: Long): TimeSpan = {
    assert(value >= 0, "Only dates after Unix epoch (1970-01-01) are currently supported")
    val secs = value / 1_000
    val mos = value % 1_000 * 1_000_000
    TimeSpan(secs, mos.toInt)
  }

  def fromNanoseconds(value: Long): TimeSpan = {
    assert(value >= 0, "Only dates after Unix epoch (1970-01-01) are currently supported")
    val secs = value / 1_000_000_000
    val mos = value % 1_000_000_000
    TimeSpan(secs, mos.toInt)
  }
}
