/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.cbc

import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.assembler.cbc.Register.FR.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.*
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.compiler.abi.{CallingConvention, CallingConventionCache, RegFile}
import com.huawei.excelsior.jet.compiler.symlevel.CallConv

object CallingConventionCBC extends CallingConventionCache[IR, FR] {
  val iRegs = RegFile(Array(IR1, IR2, IR3, IR4, IR5, IR6, IR7, IR8, IR9, IR10, IR11, IR12, IR13),
    volatiles = Array(IR1, IR2, IR3, IR4, IR5, IR6, IR7),
    headArea = Array(IR1, IR2, IR3, IR4, IR5, IR6)
  )

  val fRegs = RegFile(Array(FR0, FR1, FR2, FR3, FR4, FR5, FR6, FR7, FR8, FR9, FR10, FR11, FR12, FR13, FR14, FR15),
    volatiles = Array(FR0, FR1, FR2, FR3, FR4, FR5, FR6, FR7),
    headArea = Array(FR0, FR1, FR2, FR3, FR4, FR5, FR6, FR7)
  )

  val alwaysVolatile = Array[AnyReg](IR7)

  override protected def create(sourceCC: CallConv) = CallingConvention(sourceCC, iRegs, fRegs, alwaysVolatile)
}
