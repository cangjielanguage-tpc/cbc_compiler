/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder

class MemoryBarrierHalfDiamondOptimizationSuite extends CompilerSuite
  with GlobalNodesBuilder
  with MemoryBarrierHalfDiamondOptimization
  with IRTransformationsCollection {

  override def parsableAttributes() = Seq(
    new SimpleAttribute("mb_SS")({ case Seq() => MemBarrier(Set(BarrierKind.STORE_STORE))()}),
    new SimpleAttribute("s")    ({ case Seq() => FakeSpinal(IntType)() }),
  ) ++ super.parsableAttributes()

  test("half-diamond squash") {
    makeCFG(0 -> ((1@@"mb_SS()" -> 3) || 3))
    memoryBarrierDiamondElimination({ x => true })
    val memBarrier = b(0).outCtrl.asInstanceOf[MemBarrier]
    val goto = memBarrier.outCtrl.asInstanceOf[Goto]
    goto.target shouldBe b(3)
  }

  test("half-diamond NO squash") {
    makeCFG(0 -> ((1@@"mb_SS()" -> 3) || 3))
    memoryBarrierDiamondElimination({ x => false })
    val iff = b(0).outCtrl.asInstanceOf[If]
    iff.trueBlock shouldBe b(1)
    iff.falseBlock shouldBe b(3)
    b(1).outCtrl shouldBe a[MemBarrier]
    b(1).blockEnd shouldBe a[Goto]
  }

  test("half-diamond NO squash (block with memory barrier has spine)") {
    makeCFG(0 -> ((1@@("mb_SS()", "s()") -> 3) || 3))
    memoryBarrierDiamondElimination({ x => true })
    val iff = b(0).outCtrl.asInstanceOf[If]
    iff.trueBlock shouldBe b(1)
    iff.falseBlock shouldBe b(3)
    b(1).outCtrl shouldBe a[MemBarrier]
    b(1).blockEnd shouldBe a[Goto]
  }

  test("half-diamond NO squash (merge point has phi)") {
    makeCFG(0@@("x", "y=use(x)", "z=use(x)") -> ((1@@"mb_SS()" -> 3@@"p=phi(y,z)") || 3))
    memoryBarrierDiamondElimination({ x => true })
    val iff = b(0).blockEnd.asInstanceOf[If]
    iff.trueBlock shouldBe b(1)
    iff.falseBlock shouldBe b(3)
    val memBarrier = b(1).outCtrl.asInstanceOf[MemBarrier]
    memBarrier.outCtrl should be (b(1).blockEnd)
    b(1).blockEnd shouldBe a[Goto]
    b(3).phies.size should be (1)
    val phi = b(3).phies.toSeq.head
    phi.valueArgs.contains(n("y")) should be (true)
    phi.valueArgs.contains(n("z")) should be (true)
  }
}
