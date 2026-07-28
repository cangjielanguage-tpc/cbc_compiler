/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.types.TypesToolbox

class CompileTimeComputationsSuite extends CompilerSuite with TypesToolbox with GlobalNodesBuilder with CompileTimeComputations {

  import TypeApproximationBuildingHelperStrict._
  import GetClassApprox._
  import CompileTimeOp.Kind._

  startPhase(CompilerPhase.PreInline)

  for (((tpe, res), pos) <- Seq(
    tp(WithSubtypes(tObj), None),
    tp(Exact(tObj), Some(False())),
    tp(WithSubtypes(tAI), None),
    tp(WithSubtypes(tI), Some(False())),
    tp(Exact(tI), Some(False())),
    tp(WithSubtypes(tA), Some(False())),
    tp(Exact(tA), Some(False())),
    tp(Exact(tInt1D), Some(True())),
    tp(WithSubtypes(tB2D), Some(True())),
    tp(Exact(symInt), Some(False())),
  )) {
    test(s"containsOnlyArrays: $tpe") {
      tpe.containsOnlyArrays map (_()) shouldBe res
    }
  }

  for (((to, from, result), pos) <- Seq(
    tp(WithSubtypes(tObj), WithSubtypes(tObj), None),
    tp(WithSubtypes(tObj), WithSubtypes(tB), None),
    tp(WithSubtypes(tC), WithSubtypes(tObj), None),
    tp(WithSubtypes(tC), WithSubtypes(tB), Some(False())),
    tp(WithSubtypes(tI), WithSubtypes(tB), None),
    tp(Exact(tI), WithSubtypes(tB), None),
    tp(Exact(tI), WithSubtypes(tIB), Some(True())),
    tp(Exact(tC), Exact(tB), Some(False())),
    tp(Exact(tA), Exact(tB), Some(True())),
    tp(Exact(tB), Exact(tA), Some(False())),
    tp(Exact(tB), WithSubtypes(tA), None),
    tp(WithSubtypes(tA), Exact(tB), None),
    tp(WithSubtypes(tB), Exact(tA), Some(False())),
    tp(Exact(tA), WithSubtypes(tB), Some(True())),
    tp(Exact(symInt), Exact(symInt), Some(True())),
    tp(Exact(symLong), Exact(symInt), Some(False())),
    tp(Exact(symInt), Exact(symLong), Some(False())),
    tp(Exact(tObj), Exact(symInt), Some(False())),
    tp(Exact(symInt), Exact(tObj), Some(False())),
  )) {
    test(s"$to isAssignableFrom $from") {
      to.isAssignableFrom(from) map (_()) shouldBe result
    }
  }

  for (((tpe, res), pos) <- Seq(
     tp(c(tObj),    None)
    ,tp(p(tA),      None)
    ,tp(p(tInt1D),  Some(symInt))
    ,tp(p(tInt2D),  Some(tInt1D.symType))
    ,tp(p(tObj1D),  None)
    ,tp(c(tObj1D),  None)
    ,tp(p(tA1D),    Some(tA.symType))
    ,tp(c(tA1D),    Some(tA.symType))
  )) {
    test(s"getRefArrElemFormalTypeNonTrivial: $tpe") {
      getRefArrElemFormalTypeNonTrivial(tpe) should be (res)
    }
  }

  test("good structure") {
    makeCFG(1 -> (2 || 3) -> 4)
    val obj = addObjNode(p(tObj1D))
    addCondition(b(1), Cmp(IntType, Condition.NE)(CondVal(IsComputableAtCompileTime(IsArray)(b(1), obj)), IConst(0)))
    val check = ComputeAtCompileTime(IsArray)(b(2), obj)

    computeCompileTime()
    check should not be Symbol("committed")
  }

  test("good structure complex key") {
    makeCFG(1 -> (2 || 3) -> 4)
    val clsObj = ClassObject(tObj)(b(1))
    val clsArr = ClassObject(tObj1D)(b(1))
    addCondition(b(1), Cmp(IntType, Condition.NE)(CondVal(IsComputableAtCompileTime(IsAssignable)(b(1), clsArr, clsObj)), IConst(0)))
    val check = ComputeAtCompileTime(IsAssignable)(b(2), clsArr, clsObj)

    env.disable(BoolOption.DisableClassNativesIntrinsification)
    try {
      computeCompileTime()
    } finally {
      env.define(BoolOption.DisableClassNativesIntrinsification, BoolOption.DisableClassNativesIntrinsification.defaultValueOrNull(env))
    }
    check should not be Symbol("committed")
  }
}
