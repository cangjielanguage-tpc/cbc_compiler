/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

/** Prefetch operation, defined as <type><target><policy>.
  *
  * @author paul
  */
object PrfOp {
  // <type>
  val PLD = 0x00
  val PLI = 0x08
  val PST = 0x10

  // <target>
  val L1 = 0x00
  val L2 = 0x02
  val L3 = 0x04
  
  // <policy>
  val KEEP = 0x00
  val STRM = 0x01
}