/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.{CompilerSuite, RTSProc}
import com.huawei.excelsior.jet.compiler.abi.arm64.PlatformArm64
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.lowering.arm64.LoweringArm64
import com.huawei.excelsior.jet.compiler.opt.middle.Optimize
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.ScalesArm64
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethod, FakeMethodType}

class MathIntrinsicLoweringSuite extends CompilerSuite with GlobalNodesBuilder
    with LoweringArm64 with Optimize with ScalesArm64 {

  override def platformForEnv = new PlatformArm64

  startPhase(CompilerPhase.Lowering)

  test("Float32 log runtime fallback converts both sides of the Double call") {
    val target = new FakeMethod("JR_log", FakeMethodType.create(TypeKind.DOUBLE, TypeKind.DOUBLE))
    env.setRtsProc(RTSProc.JR_log, target)

    currentScope.inState(entryBlock, entryMemory) {
      val arg = Fake(FloatType)
      val log = MathIntrinsic(Java.Lang.MathIntrinsic.F_LOG)(arg).asInstanceOf[MathIntrinsic]
      val result = lowerMathIntrinsic(log)
      result.tpe shouldBe FloatType
      result should matchPattern {
        case ValueConvert(F64, F32, AnyDirectCall(`target`)) =>
      }
      val call = result.arg.asInstanceOf[Call]
      call.invokeArgs shouldBe Seq(ValueConvert(F32, F64)(arg))
    }
  }

  test("Float32 floor runtime fallback converts both sides of the Double call") {
    val target = new FakeMethod("JR_floor", FakeMethodType.create(TypeKind.DOUBLE, TypeKind.DOUBLE))
    env.setRtsProc(RTSProc.JR_floor, target)

    currentScope.inState(entryBlock, entryMemory) {
      val arg = Fake(FloatType)
      val floor = MathIntrinsic(Java.Lang.MathIntrinsic.F_FLOOR)(arg).asInstanceOf[MathIntrinsic]
      val result = lowerMathIntrinsic(floor)
      result.tpe shouldBe FloatType
      result should matchPattern {
        case ValueConvert(F64, F32, AnyDirectCall(`target`)) =>
      }
      val call = result.arg.asInstanceOf[Call]
      call.invokeArgs shouldBe Seq(ValueConvert(F32, F64)(arg))
    }
  }

  test("all narrow math runtime fallbacks use typed Double arguments and Float results") {
    import Java.Lang.MathIntrinsic.*

    for (kind <- Seq(F_SIN, F_COS, F_CEIL, F_LOG10, F_LOG2, F_EXP, F_EXP2, F_TRUNC, F_POW)) {
      val params = Seq.fill(kind.argsCount)(TypeKind.DOUBLE)
      val target = new FakeMethod(kind.rtProc.toString, FakeMethodType.create(TypeKind.DOUBLE, params: _*))
      env.setRtsProc(kind.rtProc, target)
      currentScope.inState(entryBlock, entryMemory) {
        val args = Seq.fill(kind.argsCount)(Fake(FloatType))
        val intrinsic = MathIntrinsic(kind)(args: _*).asInstanceOf[MathIntrinsic]
        val result = lowerMathIntrinsic(intrinsic)
        result.tpe shouldBe FloatType
        result should matchPattern {
          case ValueConvert(F64, F32, AnyDirectCall(`target`)) =>
        }
        result.arg.asInstanceOf[Call].invokeArgs shouldBe args.map(arg => ValueConvert(F32, F64)(arg))
      }
    }
  }
}
