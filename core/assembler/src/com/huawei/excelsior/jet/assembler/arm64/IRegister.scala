/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Width.W32
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.SP
import com.huawei.excelsior.jet.assembler.Width

import scala.annotation.nowarn

/** ARM64 GP registers.
  *
  * @author gatimosh
  * @author orangebyte256
  */
trait IRegister extends Register with Arg.RArith with Arg.RLogical {

  /** Returns W register corresponding to this (W or X) register.
    *
    * For W register, returns it back. For X register, returns its W part.
    * Shall not be invoked for SP register, as it does not have W part.
    */
  def asW = {
    assert(this != SP)
    IRegister.W.fromOrdinal(this.encoding)
  }

  /** Returns X register corresponding to this (W or X) register.
    *
    * For X or SP register, returns it back. For W register, returns the X register that contains this W register
    * as its part.
    */
  def asX = if (this == SP) {
    SP
  } else {
    IRegister.X.fromOrdinal(this.encoding)
  }

  /** Returns the version of this register for the given width, i.e. W register for 32-bit width and X register for
    * 64-bit width.
    * Shall only be invoked for registers that have corresponding register of the given width.
    */
  @nowarn("msg=match may not be exhaustive")
  override def as(width: Width): IRegister = width match {
    case Width.W32 => asW
    case Width.WPTR | Width.W64 => asX
  }
}

object IRegister {

  /** ARM64 general-purpose W registers, that are lower halfs of corresponding X registers. */
  enum W extends IRegister with Location.SubReg[X] {
    case W0,  W1,  W2,  W3,  W4,  W5,  W6,  W7,
         W8,  W9,  W10, W11, W12, W13, W14, W15,
         W16, W17, W18, W19, W20, W21, W22, W23,
         W24, W25, W26, W27, W28, W29, W30, WZR

    override def encoding = ordinal
    override def width = W32
    override def host = this.asX
  }

  /** ARM64 general-purpose full (X) registers. */
  enum X extends IRegister with Location.IReg {
    case X0,  X1,  X2,  X3,  X4,  X5,  X6,  X7,
         X8,  X9,  X10, X11, X12, X13, X14, X15,
         IP0, IP1, X18, X19, X20, X21, X22, X23,
         X24, X25, X26, X27, X28, X29, LR

    case XZR, SP // both are 31-encoded

    override def encoding = if (this == XZR || this == SP) 31 else ordinal
    override def width = Width.W64
  }
}
