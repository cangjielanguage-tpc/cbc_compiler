/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.amd64

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.preparation.PreparationAmd64
import com.huawei.excelsior.jet.compiler.opt.backend.preparation.PreparationSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{GlobalNodesBuilder, IRBuilderDSLAmd64}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType

/**
  * Tests for PreparationAmd64.
  */
class PreparationAmd64Suite extends CompilerSuite with GlobalNodesBuilder with IRBuilderDSLAmd64 with PreparationSuite with PreparationAmd64 with BackEndAmd64 {

  override def parsableAttributes() = Seq(
    new SimpleAttribute("CAS")({ case Seq(x, y, z) => CAS(AsmType.I64)(x, y, z) }),
    new SimpleAttribute("fkl")({ case Seq() => Fake(LongType) }),
    new SimpleAttribute("fka")({ case Seq() => Fake(AddrType) }),
    new SimpleAttribute("CmpEQ")({ case Seq(x, y) => Cmp(LongType, Condition.EQ)(x, y) }),

  ) ++ super.parsableAttributes()

  test("test cmp cas if optimization") {
    resetUniverse()

    startPhase(CompilerPhase.Preparation)

    makeCFG(0@@("x=fka()", "y=fkl()", "z=fkl()", "cas=CAS(x,y,z)", "cmp=CmpEQ(cas,y)", "iff=if(cmp)") -> (1 || 2))

    combineCmpCASWithIf()

    val selector = n("iff").asInstanceOf[If].selector
    selector shouldBe a [CmpCAS]
    selector.singleValueUse shouldBe n("iff")
  }


  override def beforeEach(): Unit = {
    super.beforeEach()
    disableValueNumbering()
  }

  test("arch independent lea creation") {
    archIndependent()
  }

  test("amd64 specific lea creation") {
    // base + index + disp
    testLea(
      Add(Add(foo, bar), const(42)),
      lea(foo, bar, 1, 42))

    // base + index*scale + disp
    testLea(
      Add(Add(foo, lsl(bar, 3)), const(63)),
      lea(foo, bar, 8, 63))

    // base + index*notScale + disp
    testLea(
      Add(Add(foo, lsl(bar, 12)), const(11)),
      lea(foo, lsl(bar, 12), 1, 11))

    // lea(base + index*scale) + disp
    testLea(
      Add(lea(foo, bar, 4), const(37)),
      lea(foo, bar, 4, 37))

    // change add order
    testLea(
      {
        val add1 = Add(baz, const(111))
        add1.updateArg(0, Add(foo, bar))
        add1
      },
      lea(foo, bar, 1, 111))

    // index*scale + base + disp
    testLea(
      Add(Add(lsl(foo, 1), bar), const(419)),
      lea(bar, foo, 2, 419))

    // index*scale1 + base*scale2 + disp
    testLea(
      Add(Add(lsl(foo, 2), lsl(bar, 3)), const(914)),
      lea(lsl(foo, 2), bar, 8, 914))

    // index*scale1 + base*scale2 + disp
    // base*scale2 has other uses
    testLea(
      {
        val r = lsl(bar, 3)
        Add(r, r)
        Add(Add(lsl(foo, 2), r), const(194))
      },
      lea(lsl(bar, 3), foo, 4, 194))
  }
}
