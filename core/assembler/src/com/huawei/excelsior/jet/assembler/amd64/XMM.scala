/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.Location.FReg
import com.huawei.excelsior.jet.assembler.Width

/** SSE register
  *
  * @author cypok
  * @author paul
  * @author alexm
  */
enum XMM extends Register with FReg {
  case XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
       XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15

  private val addrModeFromRegister = AddrMode.fromRegister(this)

  override def code = ordinal
  override def width = Width.W128

  override def toAddrMode = addrModeFromRegister

  override def as(width: Width) = shouldNotCallThis("this register is not general-purpose register")
  override def asGPR            = shouldNotCallThis("this register is not general-purpose register")
  override def asReg32          = shouldNotCallThis("this register is not general-purpose register")
  override def asReg16          = shouldNotCallThis("this register is not general-purpose register")
  override def asReg8           = shouldNotCallThis("this register is not general-purpose register")
  override def asHighReg8       = shouldNotCallThis("this register is not general-purpose register")

  /** Returns this XMM register. */
  override def asXMM = this
}

object XMM {
  /** The size of XMM registers in bytes. */
  val SIZE = 16
}
