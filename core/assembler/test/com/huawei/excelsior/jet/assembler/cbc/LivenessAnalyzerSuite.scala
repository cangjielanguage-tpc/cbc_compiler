/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.{Label, Segment}
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer.Mark
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer.LivenessMark.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer.UsageMark.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.FlowAnalyzer
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import xscala.util.MathUtils.minExtended

class LivenessAnalyzerSuite extends AnyFunSuite with BeforeAndAfterEach {

  var liveness: LivenessAnalyzer = _

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    liveness = new LivenessAnalyzer(strict = true)
  }

  extension (ir: IR) {
    def ->(mark: Mark): Unit = {
      liveness.mark(ir, mark)
    }
  }

  test("simple use after def") {
    liveness.op {
      IR1 -> REF
      IR2 -> PRIM
    }
    liveness.op {
      IR2 -> USE_PRIM
      IR1 -> USE_REF
    }
  }

  test("use before def") {
    liveness.op(IR1 -> REF)
    assertThrows[NoSuchElementException] {
      liveness.op( IR2 -> USE_REF )
    }
  }

  test("use diff type") {
    liveness.op(IR1 -> REF)
    assertThrows[AssertionError] {
      liveness.op( IR1 -> USE_PRIM )
    }
  }

  test("last use in same op") {
    liveness.op {
      IR1 -> REF
    }
    liveness.op {
      IR1 -> USE_REF
      IR1 -> REF
    }
    liveness.op {
      IR1 -> USE_REF
    }
  }

  test("redefine") {
    liveness.op(IR1 -> REF)
    assertThrows[AssertionError] {
      liveness.op( IR1 -> REF )
    }
  }

  test("manually dead") {
    liveness.op( IR1 -> REF )
    liveness.op( IR1 -> USE_REF )
    liveness.dead(IR1)
    liveness.op( IR1 -> REF )
  }

  test("branch consistent") {
    val target = new Label

    liveness.op( IR1 -> REF )
    liveness.branch(target)
    liveness.op( IR1 -> USE_REF )
    liveness.merge(target)
  }

  test("branch inconsistent") {
    val target = new Label

    liveness.op( IR1 -> REF )
    liveness.branch(target)
    liveness.op( IR2 -> REF )
    assertThrows[AssertionError] {
      liveness.merge(target)
    }
  }

  test("diamond consistent") {
    val target = new Label
    val exit = new Label

    liveness.op( IR1 -> REF )
    liveness.branch(target)

    liveness.op( IR2 -> REF )
    liveness.branch(exit)
    liveness.dead(IR2) // fix state after first branch

    liveness.merge(target)
    liveness.op( IR2 -> REF )

    liveness.merge(exit)
    liveness.op {
      IR1 -> USE_REF
      IR2 -> USE_REF
    }
  }

  test("backward branch consistent") {
    val loop = new Label
    val exit = new Label

    liveness.op( IR1 -> REF )

    liveness.op( IR2 -> REF ) // manually alive from backward branch

    liveness.merge(loop)
    liveness.dead(IR2) // fix state after merge
    liveness.op( IR2 -> REF )
    liveness.branch(exit)
    liveness.branch(loop)

    liveness.merge(exit)
    liveness.op {
      IR1 -> USE_REF
      IR2 -> USE_REF
    }
  }

  test("backward branch inconsistent") {
    val backward = new Label

    liveness.op( IR1 -> REF )

    liveness.merge(backward)
    liveness.op( IR2 -> REF )
    assertThrows[AssertionError] {
      liveness.branch(backward)
    }
  }
}
