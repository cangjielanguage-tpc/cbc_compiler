/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.time

import java.time.ZonedDateTime
import java.time.temporal.ChronoField

private[xscala] final class TimeJDK extends TimeVMDependent {
  override def nowMilliseconds(): Long = System.currentTimeMillis()

  override def nowNanoseconds(): Long = System.nanoTime()

  override def nowLocalDateTime(): LocalDateTime = {
    val dateTime = ZonedDateTime.now()
    LocalDateTime(
      dateTime.getYear, dateTime.getMonthValue, dateTime.getDayOfMonth,
      dateTime.getHour, dateTime.getMinute, dateTime.getSecond,
      dateTime.get(ChronoField.MILLI_OF_SECOND)
    )
  }
}
