/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.amd64

import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.XMM.*
import com.huawei.excelsior.jet.assembler.amd64.{GPR, XMM}
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.abi.ABITestHelper
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.*
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind
import xscala.properties.OS
import xscala.properties.OS.{LINUX, WINDOWS}

import org.junit.Assert.assertEquals

/** Tests for [[ABIAmd64]] */
class ABIAmd64Suite extends ABITestHelper[GPR, XMM, ABIAmd64] {

  private def makeEnv(targetOS: OS): Unit = {
    CallingConventionAmd64.dropCache()
    Env.setUnitTestsEnv()
    Env.init(new PlatformAmd64(targetOS), isJIT = false, isWorkMode = true, isDynamicBundle = false, LanguagePack.JAVA, isStandalone = true)
  }

  private def testManaged(params: Seq[TypeKind], expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any], preserved: Seq[Boolean] = Seq.empty) = {
    makeEnv(WINDOWS)
    test(MANAGED,
      result = None, params, varArgParams = None,
      expectedSizeInCallerFrameInSlots, expectedPlacements, preserved)
  }

  private def testManaged(params: Seq[TypeKind], headInLimit: Int, headOutLimit: Int, expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]) = {
    makeEnv(WINDOWS)
    test(MANAGED, headInLimit, headOutLimit,
      result = None, params, varArgParams = None,
      expectedSizeInCallerFrameInSlots, expectedPlacements, Seq.empty)
  }

  private def testWindows(params: Seq[TypeKind], expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]) = {
    makeEnv(WINDOWS)
    test(CCALL,
      result = None, params, varArgParams = None,
      expectedSizeInCallerFrameInSlots, expectedPlacements)
  }

  private def testWindows(params: Seq[TypeKind], varArgParams: Seq[TypeKind], expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]) = {
    makeEnv(WINDOWS)
    test(CCALL,
      result = None, params, Some(varArgParams),
      expectedSizeInCallerFrameInSlots, expectedPlacements)
  }

  private def testUnix(params: Seq[TypeKind], expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]) = {
    makeEnv(LINUX)
    test(CCALL,
      result = None, params, varArgParams = None,
      expectedSizeInCallerFrameInSlots, expectedPlacements)
  }

  private def testUnix(params: Seq[TypeKind], varArgParams: Seq[TypeKind], expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]) = {
    makeEnv(LINUX)
    test(CCALL,
      result = None, params, Some(varArgParams),
      expectedSizeInCallerFrameInSlots, expectedPlacements)
  }

  private def testUnmanaged(params: Seq[TypeKind], varArgParams: Seq[TypeKind], expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]) = {
    makeEnv(LINUX)
    test(UNMANAGED,
      result = None, params, Some(varArgParams),
      expectedSizeInCallerFrameInSlots, expectedPlacements)
  }

  private def testRTCall(result: TypeKind, params: Seq[TypeKind], expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any], preserved: Seq[Boolean] = Seq.empty) = {
    makeEnv(WINDOWS)
    test(RTCALL,
      Some(result), params, varArgParams = None,
      expectedSizeInCallerFrameInSlots, expectedPlacements, preserved)
  }

  private val ss = 4
  private val fd = 1

  test("empty") {
    testManaged(Seq(), 0 + fd, Seq())
  }

  test("one param") {
    testManaged(Seq(INT), 0 + fd, Seq(RCX))
  }

  test("many params") {
    testManaged(
      Seq(INT, LONG, CHAR, LONG, CLASS, LONG, BOOLEAN, LONG),
      2 + fd,
      Seq(RCX, RSI, RDX, RDI, R8, R9, (0, I32), (8, I64)))
  }

  test("many params compact JET") {
    testManaged(
      Seq(INT, FLOAT, LONG, DOUBLE, INT, FLOAT, LONG, DOUBLE, INT, FLOAT, LONG, DOUBLE, INT, FLOAT, LONG, DOUBLE, INT, FLOAT),
      4 + fd,
      Seq(RCX, XMM0, RSI, XMM1, RDX, XMM2, RDI, XMM3, R8, XMM4, R9, XMM5, (0, I32), XMM8, (8, I64), XMM9, (16, I32), (24, F32)))
  }

  test("many params not compact Windows") {
    testWindows(
      Seq(INT, FLOAT, LONG, DOUBLE, INT, FLOAT),
      2 + ss,
      Seq(RCX, XMM1, R8, XMM3, (0, I32), (8, F32)))
  }

  test("many params compact Unix") {
    testUnix(
      Seq(INT, FLOAT, LONG, DOUBLE,
          INT, FLOAT, LONG, DOUBLE,
          INT, FLOAT, LONG, DOUBLE,
          INT, FLOAT, LONG, DOUBLE,
          INT, FLOAT, LONG, DOUBLE),
      6,
      Seq(RDI,       XMM0,      RSI,       XMM1,
          RDX,       XMM2,      RCX,       XMM3,
          R8,        XMM4,      R9,        XMM5,
         (0, I32) ,  XMM6,     (8, I64),   XMM7,
         (16, I32), (24, F32), (32, I64), (40, F64)))
  }

  test("Windows with varargs") {
    val (_, abi) = testWindows(
      params = Seq(CLASS, ARRAY),
      varArgParams = Seq(INT, DOUBLE, INT, DOUBLE, INT),
      2 + ss,
      Seq(RCX, RDX, XMM2, R9, (0, F64), (8, I32)))
    assertEquals(R8, abi.parameterSecondaryLocation(2))
  }

  test("Unix with varargs") {
    testUnix(
      params = Seq(CLASS, ARRAY),
      varArgParams = Seq(INT, DOUBLE, INT, DOUBLE, INT, INT, INT, INT, INT),
      2,
      Seq(RDI, RSI, XMM0, RDX, XMM1, RCX, R8, R9, (0, I32), (8, I32)))
  }

  test("Unmanaged with varargs, with stack-passed params") {
    testUnmanaged(
      params = Seq(CLASS, INT, INT, INT, INT, INT, INT, LONG, INT, ARRAY),
      varArgParams = Seq(LONG, LONG, LONG),
      7,
      Seq(RCX, RSI, RDX, RDI, R8, R9, (0, I32), (8, I64), (16, I32), (24, I64), (32, I64), (40, I64)))
  }

  test("Unmanaged with varargs") {
    testUnmanaged(
      params = Seq(CLASS, ARRAY),
      varArgParams = Seq(INT, LONG, INT),
      4,
      Seq(RCX, (0, I32), (8, I64), (16, I32)))
  }

  test("param passing regs not compactPack") {
    val (_, abi) = testWindows(
      Seq(CLASS, INT, DOUBLE, INT, DOUBLE, INT),
      2 + ss,
      Seq(RCX, RDX, XMM2, R9, (0, F64), (8, I32)))
    assertUsedIRegs(RCX, RDX, R9)(abi.usedArgumentIRegs)
    assertUsedFRegs(XMM2)(abi.usedArgumentFRegs)
  }

  test("param passing regs not compactPack with varargs") {
    val (_, abi) = testWindows(
      Seq(CLASS, ARRAY),
      Seq(INT, DOUBLE, INT, DOUBLE, INT),
      2 + ss,
      Seq(RCX, RDX, XMM2, R9, (0, F64), (8, I32)))
    assertUsedIRegs(RCX, RDX, R8, R9)(abi.usedArgumentIRegs)
    assertUsedFRegs(XMM2)(abi.usedArgumentFRegs)
  }

  test("param passing regs compactPack Unix") {
    val (_, abi) = testUnix(
      Seq(CLASS, INT, DOUBLE, INT, DOUBLE, INT),
      0,
      Seq(RDI, RSI, XMM0, RDX, XMM1, RCX))
    assertUsedIRegs(RDI, RSI, RDX, RCX)(abi.usedArgumentIRegs)
    assertUsedFRegs(XMM0, XMM1)(abi.usedArgumentFRegs)
  }

  test("param passing regs compactPack managed") {
    val (_, abi) = testManaged(
      Seq(CLASS, INT, DOUBLE, INT, DOUBLE, INT),
      0 + fd,
      Seq(RCX, RSI, XMM0, RDX, XMM1, RDI))
    assertUsedIRegs(RCX, RSI, RDX, RDI)(abi.usedArgumentIRegs)
    assertUsedFRegs(XMM0, XMM1)(abi.usedArgumentFRegs)
  }

  test("is touched Windows") {
    val (_, abi) = testWindows(
      Seq(INT, DOUBLE, INT, DOUBLE),
      ss,
      Seq(RCX, XMM1, R8, XMM3))
    checkTouched(abi, true, RAX, RCX, RDX, R8, R9, R10, R11,
      XMM0, XMM1, XMM2, XMM3, XMM4, XMM5) // volatile
    checkTouched(abi, true, RCX, R8, XMM1, XMM3) // params
    checkTouched(abi, false, RBX, RBP, RSP, R12, R13, R14, R15, XMM6, XMM7, XMM8, XMM9)
  }

  test("is touched managed non-empty") {
    val (_, abi) = testManaged(
      Seq(INT, DOUBLE, INT, DOUBLE),
      fd,
      Seq(RCX, XMM0, RSI, XMM1))
    checkTouched(abi, true, RAX, RCX, RSI, RDX, RDI, R8, R9, R10,
      XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM8, XMM9) // volatile
    checkTouched(abi, true, RCX, RSI, XMM0, XMM1) // params
    check(abi.isVolatile, true, RCX, RSI, XMM0, XMM1) // non-preserved
    checkTouched(abi, false, RBX, RBP, RSP, R11, R12, R13, R14, R15, XMM6, XMM7)
  }

  test("is touched managed non-empty half preserved") {
    val (_, abi) = testManaged(
      Seq(INT, DOUBLE, INT, DOUBLE),
      fd,
      Seq(RCX, XMM0, RSI, XMM1),
      Seq(false, true, true, false))
    checkTouched(abi, true, RAX, RCX, RDX, RDI, R8, R9, R10,
      XMM1, XMM2, XMM3, XMM4, XMM5, XMM8, XMM9) // volatile
    checkTouched(abi, true, RCX, RSI, XMM0, XMM1) // params
    check(abi.isVolatile, true, RCX, XMM1) // non-preserved
    check(abi.isVolatile, false, RSI, XMM0) // preserved
    checkTouched(abi, false, RBX, RBP, RSP, R11, R12, R13, R14, R15, XMM6, XMM7)
  }

  test("is touched managed empty") {
    val (_, abi) = testManaged(Seq(), fd, Seq())
    checkTouched(abi, true, RAX, RCX, RSI, RDX, RDI, R8, R9, R10,
      XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM8, XMM9) // volatile
    checkTouched(abi, true /*, nothing */) // params
    checkTouched(abi, false, RBX, RBP, RSP, R11, R12, R13, R14, R15, XMM6, XMM7)
  }

  test("is touched RTCall non-empty") {
    val (_, abi) = testRTCall(TypeKind.VOID,
      Seq(INT, DOUBLE, INT, DOUBLE),
      fd,
      Seq(RCX, XMM0, RSI, XMM1))
    checkTouched(abi, true, RCX, RSI, XMM0, XMM1) // params
    check(abi.isVolatile, true, RCX, RSI, XMM0, XMM1) // non-preserved
    checkTouched(abi, false, RAX, RBX, RDX, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15,
      XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9)
  }

  test("is touched RTCall non-empty half preserved") {
    val (_, abi) = testRTCall(TypeKind.VOID,
      Seq(INT, DOUBLE, INT, DOUBLE),
      fd,
      Seq(RCX, XMM0, RSI, XMM1),
      Seq(false, true, true, false))
    checkTouched(abi, true, RCX, RSI, XMM0, XMM1) // params
    check(abi.isVolatile, true, RCX, XMM1) // non-preserved
    check(abi.isVolatile, false, RSI, XMM0) // preserved
    checkTouched(abi, false, RAX, RBX, RDX, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15,
      XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9)
  }

  test("is touched RTCall empty") {
    val (_, abi) = testRTCall(INT, Seq(), fd, Seq())
    checkTouched(abi, true, RCX) // volatile
    checkTouched(abi, true /*, nothing */) // params
    checkTouched(abi, false, RAX, RDX, RBX, RSI, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15,
      XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9)
  }

  test("many params with inLimit 2") {
    testManaged(
      Seq(INT, LONG, CHAR, LONG, CLASS, LONG, BOOLEAN, LONG),
      headInLimit = 2, headOutLimit = 1,
      6 + fd,
      Seq(RCX, RSI, (0, I32), (8, I64), (16, PTR), (24, I64), (32, I32), (40, I64)))
  }

  test("many params with inLimit 0") {
    testManaged(
      Seq(INT, LONG, CHAR, LONG, CLASS, LONG, BOOLEAN, LONG),
      headInLimit = 0, headOutLimit = 1,
      8 + fd,
      Seq((0, I32), (8, I64), (16, I32), (24, I64), (32, PTR), (40, I64), (48, I32), (56, I64)))
  }
}
