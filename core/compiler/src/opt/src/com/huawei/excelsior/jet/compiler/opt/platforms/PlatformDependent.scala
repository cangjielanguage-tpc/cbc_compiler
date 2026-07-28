/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.platforms

import com.huawei.excelsior.jet.assembler.{Location, Segment, Symbol}
import com.huawei.excelsior.jet.compiler.abi

/**
  * Platform-dependent types and utility functions
  */
trait PlatformDependent {
  type IREG >: Null <: Location.IReg
  type FREG <: Location.FReg
  type ABI <: abi.ABI[IREG, FREG]
  type FRAME <: abi.Frame[IREG, FREG, ABI]
  type Platform <: abi.Platform[IREG, FREG, ABI]

  def platform: Platform
}
