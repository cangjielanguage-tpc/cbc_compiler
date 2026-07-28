/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.middle.types.LoweredReferences.LoweredReferenceApprox
import com.huawei.excelsior.jet.compiler.opt.middle.types.LoweredReferences.LoweredReferenceApprox.*
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.TypesToolbox

class LoweredTypesSuite extends CompilerSuite with TypesToolbox {

  val leq = Seq(
     tp(LoweredRefEmpty,    LoweredRefEmpty,    true)
    ,tp(LoweredRefNull,     LoweredRefNull,     true)
    ,tp(LoweredRefNonNull,  LoweredRefNonNull,  true)
    ,tp(LoweredRefNullable, LoweredRefNullable, true)

    ,tp(LoweredRefNullable, LoweredRefEmpty,    true)
    ,tp(LoweredRefNullable, LoweredRefNull,     true)
    ,tp(LoweredRefNullable, LoweredRefNonNull,  true)

    ,tp(LoweredRefNull,     LoweredRefEmpty,    true)
    ,tp(LoweredRefNonNull,  LoweredRefEmpty,    true)

    ,tp(LoweredRefNull,     LoweredRefNonNull,  false)

    ,tp(LoweredRefEmpty,    LoweredRefNull,     false)
    ,tp(LoweredRefEmpty,    LoweredRefNonNull,  false)
    ,tp(LoweredRefEmpty,    LoweredRefNullable, false)

    ,tp(LoweredRefNull,     LoweredRefNullable, false)
    ,tp(LoweredRefNonNull,  LoweredRefNullable, false)
  )

  for (((l, r, res), pos) <- leq) {
    test(s"$l >= $r") {
      l >= r should be (res)
    }
  }

  val intersectInfo = Seq(
     tp(LoweredRefEmpty,    LoweredRefEmpty,    (LoweredRefEmpty,    true))
    ,tp(LoweredRefNull,     LoweredRefNull,     (LoweredRefNull,     true))
    ,tp(LoweredRefNonNull,  LoweredRefNonNull,  (LoweredRefNonNull,  true))
    ,tp(LoweredRefNullable, LoweredRefNullable, (LoweredRefNullable, true))

    ,tp(LoweredRefEmpty,    LoweredRefNull,     (LoweredRefEmpty,    true))
    ,tp(LoweredRefEmpty,    LoweredRefNonNull,  (LoweredRefEmpty,    true))
    ,tp(LoweredRefEmpty,    LoweredRefNullable, (LoweredRefEmpty,    true))

    ,tp(LoweredRefNull,     LoweredRefNonNull,  (LoweredRefEmpty,    true))

    ,tp(LoweredRefNullable, LoweredRefNull,     (LoweredRefNull,     true))
    ,tp(LoweredRefNullable, LoweredRefNonNull,  (LoweredRefNonNull,  true))
  )

  for (((l, r, res), pos) <- intersectInfo) {
    test(s"$l intersect $r") {
      l weakIntersect r should be (res)
      r weakIntersect l should be (res)
    }
  }

  val subtractInfo = Seq(
     tp(LoweredRefEmpty,    LoweredRefEmpty,    (LoweredRefEmpty,    true))
    ,tp(LoweredRefNull,     LoweredRefNull,     (LoweredRefEmpty,    true))
    ,tp(LoweredRefNonNull,  LoweredRefNonNull,  (LoweredRefEmpty,    true))
    ,tp(LoweredRefNullable, LoweredRefNullable, (LoweredRefEmpty,    true))

    ,tp(LoweredRefEmpty,    LoweredRefNull,     (LoweredRefEmpty,    true))
    ,tp(LoweredRefEmpty,    LoweredRefNonNull,  (LoweredRefEmpty,    true))
    ,tp(LoweredRefEmpty,    LoweredRefNullable, (LoweredRefEmpty,    true))

    ,tp(LoweredRefNull,     LoweredRefEmpty,    (LoweredRefNull,     true))
    ,tp(LoweredRefNull,     LoweredRefNonNull,  (LoweredRefNull,     true))
    ,tp(LoweredRefNull,     LoweredRefNullable, (LoweredRefEmpty,    true))

    ,tp(LoweredRefNonNull,  LoweredRefEmpty,    (LoweredRefNonNull,  true))
    ,tp(LoweredRefNonNull,  LoweredRefNull,     (LoweredRefNonNull,  true))
    ,tp(LoweredRefNonNull,  LoweredRefNullable, (LoweredRefEmpty,    true))

    ,tp(LoweredRefNullable, LoweredRefEmpty,    (LoweredRefNullable, true))
    ,tp(LoweredRefNullable, LoweredRefNull,     (LoweredRefNonNull,  true))
    ,tp(LoweredRefNullable, LoweredRefNonNull,  (LoweredRefNull,     true))
  )

  for (((l, r, res), pos) <- subtractInfo) {
    test(s"$l subtract $r") {
      l subtract r should be (res)
    }
  }

  val allTypes = Seq(LoweredRefEmpty, LoweredRefNull, LoweredRefNonNull, LoweredRefNullable)

  for (l <- allTypes; r <- allTypes) {
    test(s"$l subtract $r == Empty <=> $r >= $l") {
      ((l subtract r) == (LoweredRefEmpty, true)) should be(r >= l)
    }
    test(s"$l subtract $r == $l <=> $l intersect $r == Empty") {
      ((l subtract r) == (l, true)) should be ((l weakIntersect r) == (LoweredRefEmpty, true))
    }
  }

  import TypeApproximationBuildingHelperNonStrict._

  val fromTypeApprInfo = Seq(
     tp(e,      LoweredRefEmpty)
    ,tp(n,      LoweredRefNull)
    ,tp(p(tA),  LoweredRefNonNull)
    ,tp(c(tA),  LoweredRefNonNull)
    ,tp(pn(tA), LoweredRefNullable)
    ,tp(cn(tA), LoweredRefNullable)
  )

  for (((appr, res), pos) <- fromTypeApprInfo) {
    test(s"fromReferenceApproximation $appr") {
      fromReferenceApproximation(appr) should be (res)
    }
  }
  
  val cmp = Seq(
    tp(LoweredRefEmpty,     LoweredRefNullable, CC.Less),
    tp(LoweredRefEmpty,     LoweredRefNull,     CC.Less),
    tp(LoweredRefEmpty,     LoweredRefNonNull,  CC.Less),
    tp(LoweredRefEmpty,     LoweredRefEmpty,    CC.Equal),

    tp(LoweredRefNull,      LoweredRefNullable, CC.Less),
    tp(LoweredRefNull,      LoweredRefNull,     CC.Equal),
    tp(LoweredRefNull,      LoweredRefNonNull,  CC.Incomparable),
    tp(LoweredRefNull,      LoweredRefEmpty,    CC.Greater),
    
    tp(LoweredRefNonNull,   LoweredRefNullable, CC.Less),
    tp(LoweredRefNonNull,   LoweredRefNull,     CC.Incomparable),
    tp(LoweredRefNonNull,   LoweredRefNonNull,  CC.Equal),
    tp(LoweredRefNonNull,   LoweredRefEmpty,    CC.Greater),
    
    tp(LoweredRefNullable,  LoweredRefNullable, CC.Equal),
    tp(LoweredRefNullable,  LoweredRefNull,     CC.Greater),
    tp(LoweredRefNullable,  LoweredRefNonNull,  CC.Greater),
    tp(LoweredRefNullable,  LoweredRefEmpty,    CC.Greater),
  )

  for (((l, r, res), pos) <- cmp) {
    test(s"$l compare $r") {
      l compare r should be(res)
    }
  }

}
