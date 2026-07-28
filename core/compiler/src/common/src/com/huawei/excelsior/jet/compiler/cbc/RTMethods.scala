/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.common.Language
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.{Environment, RTSProc}

object RTMethods {

  def methods(env: Environment): Seq[Method] = allRTMethods.map(proc => env.getRTSProc(proc))

  def contains(m: Method, env: Environment): Boolean = methods(env).contains(m)

  private def allRTMethods = rtMethods ++ (if (languagePack.supports(Language.JAVA)) rtMethodsJava else Seq.empty)

  val typeDef: String = "RUNTIME_LIB"

  private val rtMethods: Seq[RTSProc] = Seq(
    RTSProc.JR_FatalError,
    RTSProc.JR_ThrowCJArithmeticException,
    RTSProc.JR_ThrowCJIndexOutOfBoundsException,
    RTSProc.JR_ThrowCJNegativeArraySizeException,
    RTSProc.JR_ThrowCJNoneValueException,
    RTSProc.JR_ThrowAJSubOverflowException,
    RTSProc.JR_ThrowAJAddOverflowException,
    RTSProc.JR_ThrowAJMulOverflowException,
    RTSProc.JR_ThrowAJDivOverflowException,

    RTSProc.CJ_UncheckedArrayCopy,
    RTSProc.CJ_ArrayCopyGeneric,
    RTSProc.CJ_ArrayCopyDirty,
    RTSProc.CJ_AcquireRawData,
    RTSProc.CJ_ReleaseRawData,

    RTSProc.JR_ul2f, // TODO: remove after JET-17798
    RTSProc.JR_ul2d, // TODO: remove after JET-17798
    RTSProc.JR_f2ul, // TODO: remove after JET-17798
    RTSProc.JR_d2ul, // TODO: remove after JET-17798

    RTSProc.CJ_IdentityHashCode,
    RTSProc.CJ_Spawn,

    RTSProc.CJ_saturatingAddI8,
    RTSProc.CJ_saturatingSubI8,
    RTSProc.CJ_saturatingMulI8,
    RTSProc.CJ_saturatingDivI8,
    RTSProc.CJ_saturatingModI8,
    RTSProc.CJ_saturatingIncI8,
    RTSProc.CJ_saturatingDecI8,
    RTSProc.CJ_saturatingNegI8,
    RTSProc.CJ_saturatingAddU8,
    RTSProc.CJ_saturatingSubU8,
    RTSProc.CJ_saturatingMulU8,
    RTSProc.CJ_saturatingDivU8,
    RTSProc.CJ_saturatingModU8,
    RTSProc.CJ_saturatingIncU8,
    RTSProc.CJ_saturatingDecU8,
    RTSProc.CJ_saturatingNegU8,
    RTSProc.CJ_saturatingAddI16,
    RTSProc.CJ_saturatingSubI16,
    RTSProc.CJ_saturatingMulI16,
    RTSProc.CJ_saturatingDivI16,
    RTSProc.CJ_saturatingModI16,
    RTSProc.CJ_saturatingIncI16,
    RTSProc.CJ_saturatingDecI16,
    RTSProc.CJ_saturatingNegI16,
    RTSProc.CJ_saturatingAddU16,
    RTSProc.CJ_saturatingSubU16,
    RTSProc.CJ_saturatingMulU16,
    RTSProc.CJ_saturatingDivU16,
    RTSProc.CJ_saturatingModU16,
    RTSProc.CJ_saturatingIncU16,
    RTSProc.CJ_saturatingDecU16,
    RTSProc.CJ_saturatingNegU16,
    RTSProc.CJ_saturatingAddI32,
    RTSProc.CJ_saturatingSubI32,
    RTSProc.CJ_saturatingMulI32,
    RTSProc.CJ_saturatingDivI32,
    RTSProc.CJ_saturatingModI32,
    RTSProc.CJ_saturatingIncI32,
    RTSProc.CJ_saturatingDecI32,
    RTSProc.CJ_saturatingNegI32,
    RTSProc.CJ_saturatingAddU32,
    RTSProc.CJ_saturatingSubU32,
    RTSProc.CJ_saturatingMulU32,
    RTSProc.CJ_saturatingDivU32,
    RTSProc.CJ_saturatingModU32,
    RTSProc.CJ_saturatingIncU32,
    RTSProc.CJ_saturatingDecU32,
    RTSProc.CJ_saturatingNegU32,
    RTSProc.CJ_saturatingAddI64,
    RTSProc.CJ_saturatingSubI64,
    RTSProc.CJ_saturatingMulI64,
    RTSProc.CJ_saturatingDivI64,
    RTSProc.CJ_saturatingModI64,
    RTSProc.CJ_saturatingIncI64,
    RTSProc.CJ_saturatingDecI64,
    RTSProc.CJ_saturatingNegI64,
    RTSProc.CJ_saturatingAddU64,
    RTSProc.CJ_saturatingSubU64,
    RTSProc.CJ_saturatingMulU64,
    RTSProc.CJ_saturatingDivU64,
    RTSProc.CJ_saturatingModU64,
    RTSProc.CJ_saturatingIncU64,
    RTSProc.CJ_saturatingDecU64,
    RTSProc.CJ_saturatingNegU64,
    RTSProc.CJ_saturatingPowI64,
    RTSProc.CJ_throwingPowI64,
  )

  private val rtMethodsJava: Seq[RTSProc] = Seq(
    RTSProc.JR_ThrowNullPointerException,
  )
}
