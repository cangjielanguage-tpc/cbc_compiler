/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import org.scalatest.Inside._

class PhiWebsTranslationSuite extends CompilerSuite
                                      with GlobalNodesBuilder
                                      with PhiWebsTranslation {
  import Condition._

  private def setSelector(block: Block, selector: Node): Unit = {
    block.blockEnd match {
      case branch: If => branch.selector = selector
    }
  }

  test("simple") {
    makeCFG(0@@("a", "b", "c", "d") -> (1 || 2) -> 3 -> (4 || 5))
    val cmp1 = Cmp(IntType, EQ)("a": Node, "b": Node)
    val cmp2 = Cmp(IntType, EQ)("c": Node, "d": Node)
    val b3 = (3: Block)
    setSelector(b3, Phi(ConditionType)(b3, cmp1, cmp2))

    eliminateConditionPhies() should be (true)

    inside (b3.blockEnd) { case If(Cmp(NE, Phi(`b3`, l, r), IConst(0))) =>
      (l, r) should matchPattern {
        case (CondVal(false, `cmp1`), CondVal(false, `cmp2`)) =>
      }
    }
  }

  test("simple with identity") {
    makeCFG(0@@("a", "b", "c", "d") -> (1 || 2) -> 3 -> (4 || 5))
    val cmp1 = Cmp(IntType, EQ)("a": Node, "b": Node)
    val cmp2 = False()
    val b3 = (3: Block)
    setSelector(b3, Phi(ConditionType)(b3, cmp1, cmp2))

    eliminateConditionPhies() should be (true)

    inside (b3.blockEnd) { case If(Cmp(NE, Phi(`b3`, l, r), IConst(0))) =>
      (l, r) should matchPattern {
        case (CondVal(false, `cmp1`), IConst(0)) =>
      }
    }
  }

  test("chain") {
    makeCFG(0@@("a", "b", "c", "d") -> (1 || 2) -> 3 -> (4 || 5) -> 6 -> (7 || 8))
    val cmp1 = Cmp(IntType, EQ)("a": Node, "b": Node)
    val cmp2 = Cmp(IntType, EQ)("c": Node, "d": Node)
    val cmp3 = Cmp(IntType, EQ)("a": Node, "d": Node)
    val b3 = (3: Block)
    val phi3 = Phi(ConditionType)(b3, cmp1, cmp2)
    val b6 = (6: Block)
    setSelector(b6, Phi(ConditionType)(b6, phi3, cmp3))

    eliminateConditionPhies() should be (true)

    inside (b6.blockEnd) { case If(Cmp(NE, Phi(`b6`, Phi(`b3`, x, y), r), IConst(0))) =>
      // for some reason scalac can't match Phi(...) inside, so both phies are matched before
      (x, y, r) should matchPattern {
        case (CondVal(false, `cmp1`), CondVal(false, `cmp2`), CondVal(false, `cmp3`)) =>
      }
    }
  }

  test("loop") {
    makeCFG(0@@("a", "b", "c", "d") -> dw(1 -> (2 || 3) -> 4) -> 5 -> (6 || 7))
    val cmp1 = Cmp(IntType, EQ)("a": Node, "b": Node)
    val cmp2 = Cmp(IntType, EQ)("c": Node, "d": Node)
    val b1 = (1: Block)
    val b4 = (4: Block)
    val phi4 = Phi.cyclic(ConditionType)(b4, phi => Seq(Phi(ConditionType)(b1, cmp1, phi), cmp2))
    val b5 = (5: Block)
    setSelector(b5, phi4)

    eliminateConditionPhies() should be (true)

    inside (b5.blockEnd) { case If(Cmp(NE, p @ Phi(`b4`, Phi(`b1`, x, y), r), IConst(0))) =>
      // for some reason scalac can't match Phi(...) inside, so both phies are matched before
      (x, y, r) should matchPattern {
        case (CondVal(false, `cmp1`), `p`, CondVal(false, `cmp2`)) =>
      }
    }
  }

  test("cloned Cmp look-alike") {
    makeCFG(0@@("a", "b", "c", "d") -> (1 || 2) -> 3 -> (4 || 5))
    val cmp1 = Cmp(IntType, EQ)("a": Node, "b": Node)
    val cmp2 = Cmp(IntType, NE)("a": Node, IConst(0)) // <- "a" should not be used instead of CondVal(Cmp(NE, c, 0))
    val b3 = (3: Block)
    setSelector(b3, Phi(ConditionType)(b3, cmp1, cmp2))

    eliminateConditionPhies() should be (true)

    inside (b3.blockEnd) { case If(Cmp(NE, Phi(`b3`, l, r), IConst(0))) =>
      (l, r) should matchPattern {
        case (CondVal(false, `cmp1`), CondVal(false, `cmp2`)) =>
      }
    }
  }

  test("cloned Cmp(phi) look-alike") {
    makeCFG(0@@("a", "b", "c", "d") -> (1 || 2) -> 3 -> (4 || 5) -> 6 -> (7 || 8))
    val cmp1 = Cmp(IntType, EQ)("a": Node, "b": Node)
    val b3 = (3: Block)
    val phi1 = Phi(IntType)(b3, "a": Node, "b": Node)
    val cmp2 = Cmp(IntType, NE)(phi1, IConst(0)) // <- phi1 should not be used instead of CondVal(Cmp(NE, phi1, 0))
    val b6 = (6: Block)
    setSelector(b6, Phi(ConditionType)(b6, cmp1, cmp2))

    eliminateConditionPhies() should be (true)

    inside (b6.blockEnd) { case If(Cmp(NE, Phi(`b6`, l, r), IConst(0))) =>
      (l, r) should matchPattern {
        case (CondVal(false, `cmp1`), CondVal(false, `cmp2`)) =>
      }
    }
  }

  test("CondVal(Not(x))") {
    makeCFG(0@@("a", "b", "c", "d") -> (1 || 2) -> 3 -> (4 || 5))
    val cmp1 = Cmp(IntType, EQ)("a": Node, "b": Node)
    val x = Fake(ConditionType) // e.g. CAS
    val notX = Not(x)
    val b3 = (3: Block)
    setSelector(b3, Phi(ConditionType)(b3, cmp1, notX))

    eliminateConditionPhies() should be (true)

    inside (b3.blockEnd) { case If(Cmp(NE, Phi(`b3`, l, r), IConst(0))) =>
      (l, r) should matchPattern {
        case (CondVal(false, `cmp1`), CondVal(true, `x`)) =>
      }
    }
  }
}
