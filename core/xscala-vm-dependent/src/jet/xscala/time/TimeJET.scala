/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.time

import scala.annotation.static

private[xscala] final class TimeJET extends TimeVMDependent {

  override def nowMilliseconds(): Long = TimeJET.nowMilliseconds0()

  override def nowNanoseconds(): Long = TimeJET.nowNanoseconds0()

  override def nowLocalDateTime(): LocalDateTime = TimeJET.nowLocalDateTime0().asInstanceOf[LocalDateTime]
}

object TimeJET {
  @native @static private def nowMilliseconds0(): Long
  @native @static private def nowNanoseconds0(): Long
  @native @static private def nowLocalDateTime0(): Object
}
