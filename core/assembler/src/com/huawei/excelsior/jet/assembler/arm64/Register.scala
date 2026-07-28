/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Width

/** Base interface for all register on ARM64
  *
  * @author orangebyte256
  */
trait Register extends Location {

  /** Returns the unique integer code for the register that can be used in serialization. */
  def encoding: Int

  /** Returns the version of this register for the given width, e.g. 32-bit lower part of 64-bit register.
    * Shall only be invoked for registers that have corresponding register of the given width.
    */
  def as(width: Width): Register
}
