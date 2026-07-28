/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.ConstBranchElimination
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}

class CrossroadsOptimizerSuite extends CompilerSuite
                                  with GlobalNodesBuilder
                                  with CFGTransformationDSL
                                  with CrossroadsOptimizer
                                  with UCEComponent
                                  with SimplifyComponent
                                  with ConstBranchElimination {

  startPhase(CompilerPhase.PostInline)

  override def transformation(): Unit = {
    while (optimizeSpecializedCrossroads() | completeSSA() | simplifyIR() | eliminateConstBranches()) { }
    eliminateUnreachableCode()
  }

  override def makeDebug = false

  private def setSelector(block: Block, selector: Node): Unit = {
    block.blockEnd match {
      case branch: If => branch.selector = selector
    }
  }

  private def addCmp(block: Block): Unit = {
    setSelector(block, Cmp(IntType, Condition.EQ)(Param(IntType, 0), Param(IntType, 1)))
  }

  private def addCrossroadWithCmp(block: Block, cond: Condition, cmpArg: Node, phiArgs: Node*): Unit = {
    setSelector(block, Cmp(IntType, cond)(cmpArg, Phi(IntType)((block +: phiArgs): _*)))
  }

  private def addCrossroadWithCmpAnd(block: Block, cond: Condition, cmpArg: Node, andArg: Node, phiArgs: Node*): Unit = {
    setSelector(block, Cmp(IntType, cond)(cmpArg, And(andArg, Phi(IntType)((block +: phiArgs): _*))))
  }

  /**
    * Creates such CFG with cmp in 0 block and crossroad with given parameters in 3 block:
    *
    *       0
    *      / \
    *     1   2
    *      \ /
    *       3
    *      / \
    *     4   5
    *      \ /
    *       6
    */
  private def diamonds = 0@@("x") -> (1 || 2) -> 3 -> (4@@("ut=use(x)") || 5@@("uf=use(x)")) -> 6


  private def testSimple(cond: Condition, arg: Node, l: Node, r: Node, check:(FakeSpinalUnary, FakeSpinalUnary, Phi) => Unit): Unit = {
    withClue(s"cmp($cond, $arg, phi($l, $r))") {
      makeCFG(diamonds)
      addCmp(b(0))
      addCrossroadWithCmp(b(3), cond, arg, l, r)

      val phi = b(3).phies.toSeq.ensuring(_.size == 1).head
      val ut = ("ut": Node).asInstanceOf[FakeSpinalUnary]
      val uf = ("uf": Node).asInstanceOf[FakeSpinalUnary]
      ut.inValue = phi
      uf.inValue = phi

      transformation()

      check(ut, uf, phi)
    }
  }

  test("simple specialization - 1") {
    testSimple(Condition.EQ, IConst(0), IConst(0), IConst(1), { (ut, uf, phi) =>
      ut.inValue should be (IConst(0))
      uf.inValue should be (IConst(1))
    })
  }

  test("simple specialization - 2") {
    testSimple(Condition.EQ, IConst(42), IConst(0), IConst(1), { (ut, uf, phi) =>
      ut.isCommitted should be (false)
      uf.inValue.asInstanceOf[Phi].argsSeq should be (Seq(IConst(0), IConst(1)))
    })
  }

  test("simple specialization - 3") {
    testSimple(Condition.EQ, Param(IntType, 0), IConst(0), IConst(1), { (ut, uf, phi) =>
      ut.inValue should be (phi)
      uf.inValue should be (phi)
    })
  }

  test("simple specialization - 4") {
    testSimple(Condition.EQ, IConst(0), IConst(0), Param(IntType, 0), { (ut, uf, phi) =>
      ut.inValue.asInstanceOf[Phi].args.toSet should be (Set(IConst(0), Param(IntType, 0)))
      uf.inValue should be (Param(IntType, 0))
    })
  }


  private def testCmpAnd(cond: Condition, cmpArg: Node, andArg: Node, l: Node, r: Node, check:(FakeSpinalUnary, FakeSpinalUnary, Phi) => Unit): Unit = {
    withClue(s"cmp($cond, $cmpArg, and($andArg, phi($l, $r))") {
      makeCFG(diamonds)
      addCmp(b(0))
      addCrossroadWithCmpAnd(b(3), cond, cmpArg, andArg, l, r)

      val phi = b(3).phies.toSeq.ensuring(_.size == 1).head
      val ut = ("ut": Node).asInstanceOf[FakeSpinalUnary]
      val uf = ("uf": Node).asInstanceOf[FakeSpinalUnary]
      ut.inValue = phi
      uf.inValue = phi

      transformation()

      check(ut, uf, phi)
    }
  }

  test("Cmp(And) specialization - 1") {
    testCmpAnd(Condition.EQ, IConst(1), IConst(0x1), IConst(3), IConst(0), { (ut, uf, phi) =>
      ut.inValue should be (IConst(3))
      uf.inValue should be (IConst(0))
    })
  }

  test("Cmp(And) specialization - 2") {
    testCmpAnd(Condition.EQ, IConst(1), IConst(0x1), IConst(4), IConst(0), { (ut, uf, phi) =>
      ut.isCommitted should be (false)
      uf.inValue.asInstanceOf[Phi].argsSeq should be (Seq(IConst(4), IConst(0)))
    })
  }

  test("Cmp(And) specialization - 3") {
    testCmpAnd(Condition.EQ, IConst(1), IConst(0x1), IConst(5), IConst(1), { (ut, uf, phi) =>
      ut.inValue.asInstanceOf[Phi].argsSeq should be (Seq(IConst(5), IConst(1)))
      uf.isCommitted should be (false)
    })
  }

  ////////////////////////////////////////
  // See JET-11244, JET-11237, JET-11238

  test("no loop specialization - 1") {
    before(0@@"s=ic(1)" -> wd(1@@("p=phi(s,a)", "a=add(p,ic(1))", "if(cmp(p,ic(5)))") -> 2) -> 3)
    after(0 -> wd(1 -> 2) -> 3)
  }

  test("no loop specialization - 2") {
    before(0@@"s=ic(1)" -> wd(1@@("p=phi(s,a)", "a=add(p,ic(1))", "if(cmp(p,ic(5)))")) -> 2)
    after(0 -> wd(1) -> 2)
  }

  test("no loop specialization - 3") {
    before(0@@("s1=ic(1)", "s2=ic(2)") -> dw(1@@"p=phi(s1,s2)" -> (2 || 3) -> 4@@"if(cmp(p,s1))") -> 5)
    after(0 -> dw(1 -> (2 || 3) -> 4) -> 5)
  }

  test("no loop specialization - 4") {
    before(0@@("s1=false()", "s2=true()") -> wd(1@@"p=phi(s1,s2)" -> 2@@"if(p)" -> (3 || 4) -> 5) -> 6)
    after(0 -> wd(1 -> 2 -> (3 || 4) -> 5) -> 6)
  }

  test("loop specialization - 1") {
    before(0@@("s1=ic(1)", "s2=ic(2)") -> dw(1 -> (2 || 3) -> 4@@("p=phi(s1,s2)", "if(cmp(p,s1))")) -> 5)
    after(0 -> wd(1 -> 2 -> 4 -> 11) -> 3 -> 44 -> 5)
  }

}
