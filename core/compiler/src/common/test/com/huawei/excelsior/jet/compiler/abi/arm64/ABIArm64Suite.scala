/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.arm64

import com.huawei.excelsior.common.{LanguagePack}
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.*
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.D.*
import com.huawei.excelsior.jet.assembler.arm64.{IRegister, VFPRegister}
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.abi.ABITestHelper
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeMethod
import com.huawei.excelsior.jet.compiler.symlevel.{CallConv, TypeKind}

/** Tests for [[ABIArm64]]
  *
  * @author gatimosh
  */
class ABIArm64Suite extends ABITestHelper[IRegister.X, VFPRegister.D, ABIArm64] {

  private def test(callConv: CallConv,
                   result: Option[TypeKind], params: Seq[TypeKind], varArgParams: Option[Seq[TypeKind]],
                   expectedVarArgsOccupiedIRegs: Option[Array[IRegister.X]],
                   expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]): (FakeMethod, ABIArm64) = {
    Env.setUnitTestsEnv()
    Env.init(new PlatformArm64, isJIT = false, isWorkMode = true, isDynamicBundle = false, LanguagePack.JAVA, isStandalone = true)

    val (method, abi) = test(callConv,
      result, params, varArgParams,
      expectedSizeInCallerFrameInSlots, expectedPlacements)
    if (method.isVarArgs) {
      val receiverABI = abiForMethod(method, null)
      assertResult(expectedVarArgsOccupiedIRegs.get)(receiverABI.getVarArgsOccupiedIRegs)
    }
    (method, abi)
  }

  private def test(callConv: CallConv, params: Seq[TypeKind],
                   expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]): (FakeMethod, ABIArm64) =
    test(callConv, result = None,
      params, varArgParams = None, expectedVarArgsOccupiedIRegs = None,
      expectedSizeInCallerFrameInSlots, expectedPlacements)

  private def test(callConv: CallConv, result: TypeKind,
                   params: Seq[TypeKind],
                   expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]): (FakeMethod, ABIArm64) =
    test(callConv, Some(result),
      params, varArgParams = None, expectedVarArgsOccupiedIRegs = None,
      expectedSizeInCallerFrameInSlots, expectedPlacements)

  private def test(callConv: CallConv, params: Seq[TypeKind],
                   varArgParams: Seq[TypeKind], expectedVarArgsOccupiedIRegs: Array[IRegister.X],
                   expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]): (FakeMethod, ABIArm64) =
    test(callConv, None,
      params, Some(varArgParams), Some(expectedVarArgsOccupiedIRegs),
      expectedSizeInCallerFrameInSlots, expectedPlacements)

  private val fd = 1

  test("EmptyManaged") {
    test(MANAGED, Seq(), fd, Seq())
  }

  test("EmptyUnmanaged") {
    test(UNMANAGED, Seq(), fd, Seq())
  }

  test("EmptyRTCall") {
    test(RTCALL, Seq(), fd, Seq())
  }

  test("EmptyVMCall") {
    test(VMCALL, Seq(), fd, Seq())
  }

  test("EmptyCCall") {
    test(CCALL, Seq(), 0, Seq())
  }

  test("OneArgInt") {
    test(MANAGED, Seq(INT),
      fd, Seq(X0))
  }

  test("OneArgLong") {
    test(MANAGED, Seq(LONG),
      fd, Seq(X0))
  }

  test("OneArgFloat") {
    test(MANAGED, Seq(FLOAT),
      fd, Seq(D0))
  }

  test("OneArgDouble") {
    test(MANAGED, Seq(DOUBLE),
      fd, Seq(D0))
  }

  test("OneArgClass") {
    test(MANAGED, Seq(CLASS),
      fd, Seq(X0))
  }

  test("IntAndLongParams0") {
    test(MANAGED, Seq(INT, INT, INT, INT),
      fd,
      Seq(X0, X1, X2, X3))
  }

  test("IntAndLongParams1") {
    test(MANAGED, Seq(INT, LONG, INT, LONG),
      fd,
      Seq(X0, X1, X2, X3))
  }

  test("IntAndLongParams2") {
    test(MANAGED, Seq(INT, LONG, INT, LONG, INT, LONG, INT, LONG, INT, LONG),
      fd + 4 /* frame descriptor + int + long + int + long */,
      Seq(X0, X1, X2, X3, X4, X5, (0, I32), (8, I64), (16, I32), (24, I64)))
  }

  test("IntAndLongParams3") {
    test(MANAGED, Seq(INT, LONG, INT, LONG, INT, LONG, INT, LONG, INT, LONG, INT),
      fd + 5 /* frame descriptor + int + long + int + long + int */,
      Seq(X0, X1, X2, X3, X4, X5, (0, I32), (8, I64), (16, I32), (24, I64), (32, I32)))
  }

  test("FloatParamsJET") {
    test(MANAGED, Seq(FLOAT, FLOAT, FLOAT, FLOAT),
      fd,
      Seq(D0, D1, D2, D3))
  }

  test("FloatParamsUnix") {
    test(CCALL, Seq(FLOAT, FLOAT, FLOAT, FLOAT),
      0,
      Seq(D0, D1, D2, D3))
  }

  test("DoubleParamsJET") {
    test(MANAGED, Seq(DOUBLE, DOUBLE, DOUBLE, DOUBLE),
      fd,
      Seq(D0, D1, D2, D3))
  }

  test("DoubleParamsUnix") {
    test(CCALL, Seq(DOUBLE, DOUBLE, DOUBLE, DOUBLE),
      0,
      Seq(D0, D1, D2, D3))
  }

  test("FloatAndDoubleParamsJET") {
    test(MANAGED, Seq(FLOAT, DOUBLE, FLOAT, DOUBLE),
      fd,
      Seq(D0, D1, D2, D3))
  }

  test("FloatAndDoubleParamsUnix") {
    test(CCALL, Seq(FLOAT, DOUBLE, FLOAT, DOUBLE),
      0,
      Seq(D0, D1, D2, D3))
  }

  test("FloatAndDoubleParamsOnStackJET0") {
    test(MANAGED, Seq(FLOAT, DOUBLE, FLOAT, DOUBLE, FLOAT, DOUBLE, FLOAT, DOUBLE, FLOAT, DOUBLE),
      fd + 2 /* frame descriptor + float + double */,
      Seq(D0, D1, D2, D3, D4, D5, D6, D7, (0, F32), (8, F64)))
  }

  test("FloatAndDoubleParamsOnStackJET1") {
    test(MANAGED, Seq(FLOAT, DOUBLE, FLOAT, DOUBLE, FLOAT, DOUBLE, FLOAT, DOUBLE, DOUBLE, DOUBLE, FLOAT),
      fd + 3 /* frame descriptor + double + double + float */,
      Seq(D0, D1, D2, D3, D4, D5, D6, D7, (0, F64), (8, F64), (16, F32)))
  }

  // TODO make tests with varargs

//  test("IntVarArgs") {
//    test(CallConv.C, Seq(INT, INT, ARRAY),
//      Seq(INT, INT, INT, INT), Array(X2, X3),
//      2 /* int + int */,
//      Seq(X0, X1, X2, X3, 0, 4))
//  }
//
//  test("IntVarArgsOnStack") {
//    test(CallConv.C, Seq(INT, INT, LONG, ARRAY),
//      Seq(INT, INT), Array(),
//      2 /* int + int */,
//      Seq(X0, X1, pair(X2, X3), 0, 4))
//  }
//
//  test("DoubleVarArgs") {
//    test(CallConv.C, Seq(INT, ARRAY),
//      Seq(DOUBLE, DOUBLE, INT), Array(X1, X2, X3),
//      3 /* double + int */,
//      Seq(X0, pair(X2, X3), 0, 8))
//  }
//
//  test("FloatArgBeforeVarArgs") {
//    test(CallConv.C, Seq(INT, FLOAT, ARRAY),
//      Seq(DOUBLE, DOUBLE, INT), Array(X2, X3),
//      3 /* double + int */,
//      Seq(X0, X1, pair(X2, X3), 0, 8))
//  }
//
//  test("DoubleArgBeforeVarArgs") {
//    test(CallConv.C, Seq(INT, DOUBLE, ARRAY),
//      Seq(DOUBLE, DOUBLE, INT), Array(),
//      5 /* double + double + int */,
//      Seq(X0, pair(X2, X3), 0, 8, 16))
//  }

  test("param passing regs managed int") {
    val (_, abi) = test(MANAGED, Seq(LONG, INT, LONG, INT),
      fd,
      Seq(X0, X1, X2, X3))
    assertUsedIRegs(X0, X1, X2, X3)(abi.usedArgumentIRegs)
    assertUsedFRegs()(abi.usedArgumentFRegs)
  }

  test("param passing regs managed floats") {
    val (_, abi) = test(MANAGED, Seq(FLOAT, DOUBLE, FLOAT, DOUBLE, FLOAT, DOUBLE, FLOAT, DOUBLE),
      fd,
      Seq(D0, D1, D2, D3, D4, D5, D6, D7))
    assertUsedIRegs()(abi.usedArgumentIRegs)
    assertUsedFRegs(D0, D1, D2, D3, D4, D5, D6, D7)(abi.usedArgumentFRegs)
  }

  test("param passing regs CCall") {
    val (_, abi) = test(CCALL, Seq(FLOAT, LONG, INT, DOUBLE, DOUBLE, DOUBLE, FLOAT),
      0,
      Seq(D0, X0, X1, D1, D2, D3, D4))
    assertUsedIRegs(X0, X1)(abi.usedArgumentIRegs)
    assertUsedFRegs(D0, D1, D2, D3, D4)(abi.usedArgumentFRegs)
  }

  // TODO

//  test("param passing regs CCall vararg") {
//    val (_, abi) = test(CallConv.C, Seq(FLOAT, INT, ARRAY),
//      Seq(INT, INT), Array(X2, X3),
//      0,
//      Seq(X0, X1, X2, X3))
//
//    assertUsedIRegs(X0, X1, X2, X3)(abi.usedArgumentIRegs)
//    assertUsedFRegs()(abi.usedArgumentFRegs)
//  }

  // TODO
  test("is touched") {
    val volGP = Seq(IP0, X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, IP1, X18, LR)
    val savedGP = Seq(X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29)
    val volFP = Seq(
      D0, D1, D2, D3, D4, D5, D6, D7,
      D16, D17, D18, D19, D20, D21, D22, D23,
      D24, D25, D26, D27, D28, D29, D30, D31)
    val savedFP = Seq(D8, D9, D10, D11, D12, D13, D14, D15)

    {
      val (_, abi) = test(CCALL, Seq(INT, DOUBLE, INT, DOUBLE),
        0,
        Seq(X0, D0, X1, D1))
      checkTouched(abi, true, X0, X1, D0, D1) // params
      checkTouched(abi, true, volGP*)
      checkTouched(abi, true, volFP*)
      checkTouched(abi, false, savedGP*)
      checkTouched(abi, false, savedFP*)
    }

    {
      val (_, abi) = test(MANAGED, Seq(INT, DOUBLE, INT, DOUBLE),
        fd,
        Seq(X0, D0, X1, D1))
      checkTouched(abi, true, X0, X1, D0, D1) // params
      checkTouched(abi, true, volGP*)
      checkTouched(abi, true, volFP*)
      checkTouched(abi, false, savedGP*)
      checkTouched(abi, false, savedFP*)
    }

    {
      val (_, abi) = test(MANAGED, Seq(),
        fd,
        Seq())
      checkTouched(abi, true /*, nothing */) // params
      checkTouched(abi, true, volGP*)
      checkTouched(abi, true, volFP*)
      checkTouched(abi, false, savedGP*)
      checkTouched(abi, false, savedFP*)
    }

    {
      val (_, abi) = test(RTCALL, TypeKind.VOID, Seq(INT, DOUBLE, INT, DOUBLE),
        fd,
        Seq(X0, D0, X1, D1))
      checkTouched(abi, true, IP0, LR) // volatile gp
      checkTouched(abi, true, X0, D0, X1, D1) // params
      checkTouched(abi, false, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15)
      checkTouched(abi, false, D2, D3, D4, D5, D6, D7, D16, D17, D18, D19, D20)
      checkTouched(abi, false, D21, D22, D23, D24, D25, D26, D27, D28, D29, D30, D31)
    }

    {
      val (_, abi) = test(RTCALL, INT, Seq(),
        fd,
        Seq())
      checkTouched(abi, true, IP0, X0, LR) // volatile gp
      checkTouched(abi, true /*, nothing */) // params
      checkTouched(abi, false, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15)
      checkTouched(abi, false, volFP*)
    }
  }
}
