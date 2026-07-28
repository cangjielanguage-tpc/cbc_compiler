/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.AsmError
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.*
import com.huawei.excelsior.jet.assembler.amd64.Register16.*
import com.huawei.excelsior.jet.assembler.amd64.Register32.*
import com.huawei.excelsior.jet.assembler.amd64.Register8.*
import org.scalatest.funsuite.AnyFunSuite

/** Tests for [[Register]] and its descendants.
  *
  * @author cypok
  */
class IRegisterAmd64Suite extends AnyFunSuite {
  test("AsReg8") {
    assertResult(BL)(BL.asReg8)
    assertResult(BL)(BX.asReg8)
    assertResult(BL)(EBX.asReg8)
    assertResult(BL)(RBX.asReg8)
  }

  test("AsHighReg8") {
    assertResult(BH)(BL.asHighReg8)
    assertResult(BH)(BX.asHighReg8)
    assertResult(BH)(EBX.asHighReg8)
    assertResult(BH)(RBX.asHighReg8)
  }

  test("AsReg16") {
    assertResult(BX)(BL.asReg16)
    assertResult(BX)(BX.asReg16)
    assertResult(BX)(EBX.asReg16)
    assertResult(BX)(RBX.asReg16)
  }

  test("AsReg32") {
    assertResult(EBX)(BL.asReg32)
    assertResult(EBX)(BX.asReg32)
    assertResult(EBX)(EBX.asReg32)
    assertResult(EBX)(RBX.asReg32)
  }

  test("AsGPR") {
    assertResult(RBX)(BL.asGPR)
    assertResult(RBX)(BX.asGPR)
    assertResult(RBX)(EBX.asGPR)
    assertResult(RBX)(RBX.asGPR)
  }

  test("RegAddrModeWithWidth") {
    assertResult(RBX.toAddrMode)( RBX.toAddrMode.as(WPTR))
    assertResult(RBX.toAddrMode)( RBX.toAddrMode.as(QWORD))
    assertResult(EBX.toAddrMode)( RBX.toAddrMode.as(DWORD))
    assertResult(BX.toAddrMode)( RBX.toAddrMode.as(WORD))
    assertResult(BL.toAddrMode)( RBX.toAddrMode.as(BYTE))
  }

  test("RegAddrModeWithIncorrectWidth") {
    assertThrows[AsmError] {
      val _ = RBX.toAddrMode.as(NO_WIDTH)
      val _ = RBX.toAddrMode.as(OWORD)
      val _ = RBX.toAddrMode.as(TWORD)
      val _ = RBX.toAddrMode.as(ZERO)
    }
  }
}
