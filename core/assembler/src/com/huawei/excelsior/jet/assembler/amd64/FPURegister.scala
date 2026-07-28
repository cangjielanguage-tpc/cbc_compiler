/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Width

/** x87 register.
  *
  * @author paul
  * @author cypok
  */
enum FPURegister extends Location.Other {
  case ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7

  /** Returns the unique integer code for the register that can be used in serialization. */
  def code = ordinal

  override def width = Width.W80
}

object FPURegister {
  /** ST register that is actually an alias for ST0. */
  val ST = ST0
}
