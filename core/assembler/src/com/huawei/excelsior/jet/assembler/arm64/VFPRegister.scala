/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Width

import scala.annotation.nowarn

/** ARM64 floating-point registers.
  *
  * @author gatimosh
  * @author orangebyte256
  */
trait VFPRegister extends Register {

  def asH = VFPRegister.H.fromOrdinal(this.encoding)
  def asS = VFPRegister.S.fromOrdinal(this.encoding)
  def asD = VFPRegister.D.fromOrdinal(this.encoding)
  def asV = VFPRegister.V.fromOrdinal(this.encoding)

  /** Returns the version of this register (H, S, D or V) for the given width. */
  @nowarn("msg=match may not be exhaustive")
  override def as(width: Width): VFPRegister = width match {
    case Width.W16 => asH
    case Width.W32 => asS
    case Width.W64 | Width.WPTR => asD
    case Width.W128 => asV
  }
}

object VFPRegister {

  /** ARM64 VFP H registers, that are lower 16-bit part of corresponding V registers. */
  enum H extends VFPRegister with Location.SubReg[D] {
    case H0,  H1,  H2,  H3,  H4,  H5,  H6,  H7,
         H8,  H9,  H10, H11, H12, H13, H14, H15,
         H16, H17, H18, H19, H20, H21, H22, H23,
         H24, H25, H26, H27, H28, H29, H30, H31

    override def width = Width.W16
    override def encoding = ordinal
    override def host = this.asD
  }

  /** ARM64 VFP S registers, that are lower 32-bit part of corresponding V registers. */
  enum S extends VFPRegister with Location.SubReg[D] {
    case S0,  S1,  S2,  S3,  S4,  S5,  S6,  S7,
         S8,  S9,  S10, S11, S12, S13, S14, S15,
         S16, S17, S18, S19, S20, S21, S22, S23,
         S24, S25, S26, S27, S28, S29, S30, S31

    override def width = Width.W32
    override def encoding = ordinal
    override def host = this.asD
  }

  // FIXME: V, not D should be an FReg
  /** ARM64 VFP D registers, that are lower 64-bit part of corresponding V registers. */
  enum D extends VFPRegister with Location.FReg {
    case D0,  D1,  D2,  D3,  D4,  D5,  D6,  D7,
         D8,  D9,  D10, D11, D12, D13, D14, D15,
         D16, D17, D18, D19, D20, D21, D22, D23,
         D24, D25, D26, D27, D28, D29, D30, D31

    override def width = Width.W64
    override def encoding = ordinal
  }

  /** ARM64 VFP V (full 128-bit) registers. */
  enum V extends VFPRegister with Location.Other {
    case V0,  V1,  V2,  V3,  V4,  V5,  V6,  V7,
         V8,  V9,  V10, V11, V12, V13, V14, V15,
         V16, V17, V18, V19, V20, V21, V22, V23,
         V24, V25, V26, V27, V28, V29, V30, V31

    override def width = Width.W128
    override def encoding = ordinal
  }
}
