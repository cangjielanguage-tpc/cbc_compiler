/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.Location.{AnyReg, FReg, IReg}
import com.huawei.excelsior.jet.compiler.Env.targetPlatform
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.Primitive
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethod, FakeMethodType}
import com.huawei.excelsior.jet.compiler.symlevel.{CallConv, Method, SignatureType, TypeKind}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

abstract class ABITestHelper[IR <: IReg, FR <: FReg, XABI <: ABI[IR, FR]] extends AnyFunSuite {

  protected def test(callConv: CallConv,
                     result: Option[TypeKind], params: Seq[TypeKind], varArgParams: Option[Seq[TypeKind]],
                     expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any]): (FakeMethod, XABI) =
    test(callConv, result, params, varArgParams, expectedSizeInCallerFrameInSlots, expectedPlacements, Seq.empty)

  protected def test(callConv: CallConv,
                     result: Option[TypeKind], params: Seq[TypeKind], varArgParams: Option[Seq[TypeKind]],
                     expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any], preserved: Seq[Boolean]): (FakeMethod, XABI) =
    test(callConv, Int.MaxValue, 1, result, params, varArgParams, expectedSizeInCallerFrameInSlots, expectedPlacements, preserved)

  protected def test(callConv: CallConv, headInLimit: Int, headOutLimit: Int,
                     result: Option[TypeKind], params: Seq[TypeKind], varArgParams: Option[Seq[TypeKind]],
                     expectedSizeInCallerFrameInSlots: Int, expectedPlacements: Seq[Any], preserved: Seq[Boolean]): (FakeMethod, XABI) = {
    require(preserved.size <= params.size)

    val isVarArgs = varArgParams.isDefined
    if (isVarArgs) {
      assertResult(TypeKind.ARRAY, "last formal parameter of varargs method should be an array")(params.last)
    }

    val method = new FakeMethod(
      FakeMethodType.create(result.getOrElse(TypeKind.VOID), params*)
        .changeCallConv(callConv)
        .changeVarArgsFlag(isVarArgs)
        .changeHeadLimits(headInLimit, headOutLimit)
        .changePreservedParameterMask(preservedParams(preserved)))

    val abi = abiForMethod(method, varArgs(varArgParams))

    if (!isVarArgs) {
      assertResult(params.length, "params count")(abi.parameterCount)
      for (i <- params.indices) {
        assertResult(params(i), s"param #$i")(abi.parameterType(i).symKindErased)
        assert(!abi.isVarArgParam(i), s"param #$i")
      }
    } else {
      assertResult(params.length + varArgParams.get.length - 1, "params count")(abi.parameterCount)
      for (i <- 0 until params.length - 1) {
        assertResult(params(i), s"param #$i")(abi.parameterType(i).symKindErased)
        assert(!abi.isVarArgParam(i), s"param #$i")
      }
      for (i <- varArgParams.get.indices) {
        val paramIdx = params.length - 1 + i
        assertResult(varArgParams.get(i), s"param #$paramIdx")(abi.parameterType(paramIdx).symKindErased)
        assert(abi.isVarArgParam(paramIdx), s"param #$paramIdx")
      }
    }

    assertResult(expectedSizeInCallerFrameInSlots,
      "size on caller frame in slots")(
      abi.sizeOnCallerFrameInSlots)

    for (i <- 0 until abi.parameterCount) {
      assertResult(
        createLocation(abi.parameterType(i).symKindErased.toBytecodeApproximation, expectedPlacements(i)),
        s"placement #$i")(
        abi.paramLocations(i))
    }

    def check(regs: Array[? <: AnyReg]): Unit = {
      val tail = regs dropWhile abi.isVolatile
      assert(tail forall abi.isNonVolatile)
    }

    check(abi.availableIRegs)
    check(abi.availableFRegs)

    (method, abi)
  }

  private def varArgs(types: Option[Seq[TypeKind]]) = {
    types match {
      case Some(types) => types map Primitive.apply
      case None => null
    }
  }

  private def preservedParams(preserved: Seq[Boolean]): Int = {
    preserved.zipWithIndex.foldLeft(0) { case (mask, (preserved, idx)) => if preserved then mask | (1 << idx) else mask }
  }

  protected def check(p: AnyReg => Boolean, expected: Boolean, regs: AnyReg*): Unit = {
    val processed = mutable.HashSet.empty[AnyReg]
    for (reg <- regs) {
      assert(processed.add(reg), "regs passed to check are expected to be unique")
      assertResult(expected, reg.toString)(p(reg))
    }
  }

  protected def checkTouched(abi: XABI, expected: Boolean, regs: AnyReg*): Unit = {
    check(abi.isTouched, expected, regs: _*)
  }

  protected def abiForMethod(method: Method, varArgs: Seq[SignatureType]) =
    targetPlatform.abi(method, varArgs).asInstanceOf[XABI]

  protected def createLocation(`type`: TypeKind, expectedLocation: Any) = {
    (expectedLocation: @unchecked) match {
      case iReg: IReg => iReg
      case fReg: FReg => fReg
      case (offs: Int, tpe: AsmType) => TailSlot(offs, tpe)
    }
  }

  protected def assertUsedIRegs(expected: IR*)(actual: collection.Set[IR]): Unit =
    assertResult(expected.toSet)(actual)

  protected def assertUsedFRegs(expected: FR*)(actual: collection.Set[FR]): Unit =
    assertResult(expected.toSet)(actual)
}
