/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.IRBuilderDSL
import com.huawei.excelsior.jet.compiler.types.TypesToolbox

class TypeAnalysisSuite extends CompilerSuite with TypesToolbox with IRBuilderDSL with TypeAnalysis {

  import TypeApproximationBuildingHelperStrict._

  for ((((arr, appr, res), pos), chaTypes) <- withCHA(tB, tC, tAI)(Seq(
    tp(tObj1D, e, e),
    tp(tObj1D, n, e),

    tp(tObj1D, cn(tA1D),   cn(tA)),
    tp(tObj1D, cn(tA2D),   cn(tA1D)),
    tp(tObj1D, pn(tInt2D), pn(tInt1D)),

    tp(tA1D,   cn(tObj1D), cn(tA)),
    tp(tA2D,   cn(tObj1D), cn(tA1D)),
    tp(tInt2D, cn(tObj1D), pn(tInt1D)),

    tp(tObj1D, pn(tA2D),   cn(tA1D)),
    tp(tA1D,   cn(tObj2D), e),
    tp(tObj2D, cn(tA1D),   e),
    tp(tA1D,   pn(tObj1D), e),

    tp(tObj1D, cn(tAI),  cn(tObj)),

    tp(tObj1D, cn(aObj), cn(tObj)),
    tp(tObj1D, cn(tObj), cn(tObj)),
    tp(tA1D,   cn(tObj), cn(tA)),
    tp(tInt2D, cn(tObj), pn(tInt1D)),

    tp(tObj1D, cc(tObj), e),
    tp(tObj1D, pn(tObj), e),

    tp(tObj1D, cn(tA),   e),
    tp(tObj1D, cc(tA),   e),
    tp(tObj1D, pn(tA),   e),

    tp(tObj1D, wn(tObj, e), wn(tObj, e)),
    tp(tObj1D, wn(tObj, n), wn(tObj, e)),
    tp(tObj1D, wn(tObj, c(tA1D)), wn(tObj, cn(tA))),
    tp(tObj1D, wn(tObj2D, c(tA2D)), wn(tObj1D, cn(tA1D))),
  ))) {
    test(s"arrayGet type appr: $arr, $appr") {
      resetCHASeq(chaTypes)
      arrayGetTypeApproximation(arr, appr) should beTA (res)
    }
  }

}
