/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.xmlib

import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.time.TimeUnit.*
import xscala.time.{unixMilliseconds, unixNanoseconds}
import xscala.util.UInt

object TimeModule {

  private final val MillisecondsInSingleDay = Milliseconds.convert(1, Days)

  /** System time in seconds. */
  def getTime: Int =
    Seconds.convert(unixMilliseconds, Milliseconds).toInt

  /** Milliseconds since 0:00:00 of the current date. */
  def getTimeMillisFromMidnight: UInt =
    (unixMilliseconds % MillisecondsInSingleDay).toUInt

  def getHighResolutionTime: Long = unixNanoseconds

}
