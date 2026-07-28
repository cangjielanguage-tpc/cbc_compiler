/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.CheckLevels
import com.huawei.excelsior.jet.compiler.types.Guards.PointGuard
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo.PGO
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement
import com.huawei.excelsior.jet.util.ScalaCollections
import org.scalatest.Inside.inside
import xscala.util.MathUtils.signExtend

/**
 * Tests for SimplifyComponent
 */
class SimplifyComponentSuite extends CompilerSuite
                               with GlobalNodesBuilder
                               with SimplifyComponent {
  override def parsableAttributes() = Seq(
    new SimpleAttribute("cadd64") ({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.ADD, signed = true, managed = true)(l, r) }),
    new SimpleAttribute("csub64") ({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.SUB, signed = true, managed = true)(l, r) }),
    new SimpleAttribute("cmul64") ({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.MUL, signed = true, managed = true)(l, r) }),
    new SimpleAttribute("cdiv64") ({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.DIV, signed = true, managed = true)(l, r) }),
    new SimpleAttribute("cuadd64")({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.ADD, signed = false, managed = true)(l, r) }),
    new SimpleAttribute("cusub64")({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.SUB, signed = false, managed = true)(l, r) }),
    new SimpleAttribute("cumul64")({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.MUL, signed = false, managed = true)(l, r) }),
    new SimpleAttribute("cudiv64")({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.DIV, signed = false, managed = true)(l, r) }),
  ) ++ super.parsableAttributes()

  test("checked op simplification zero add") {
    makeCFG(1@@("x=lc(10)", "y=cadd64(lc(0),x)"))
    simplifyIR()
    n("y") shouldBe n("x")
  }

  test("checked op simplification const add") {
    makeCFG(1@@"x=cadd64(lc(2),lc(1))")
    simplifyIR()
    n("x") shouldBe IntegralConst(LongType)(3)
  }

  test("checked op simplification zero sub") {
    makeCFG(1@@("x=lc(10)", "y=csub64(x,lc(0))"))
    simplifyIR()
    n("y") shouldBe n("x")
  }

  test("checked op simplification const sub") {
    makeCFG(1@@"x=csub64(lc(2),lc(1))")
    simplifyIR()
    n("x") shouldBe IntegralConst(LongType)(1)
  }

  test("checked op simplification zero mul") {
    makeCFG(1@@("x=lc(10)", "y=cmul64(x,lc(0))"))
    simplifyIR()
    n("y") shouldBe IntegralConst(LongType)(0)
  }

  test("checked op simplification one mul") {
    makeCFG(1@@("x=lc(10)", "y=cmul64(x,lc(1))"))
    simplifyIR()
    n("y") shouldBe n("x")
  }

  test("checked op simplification const mul") {
    makeCFG(1@@"x=cmul64(lc(2),lc(6))")
    simplifyIR()
    n("x") shouldBe IntegralConst(LongType)(12)
  }

  test("checked op simplification unsigned div") {
    makeCFG(1@@("x=lc(10)", "y=cudiv64(x,lc(0))"))
    simplifyIR()
    n("y") shouldBe an[IDivRemOp]
  }

  test("checked op simplification not-minus-one div") {
    makeCFG(1@@("x=lc(10)", "y=cdiv64(x,lc(1))"))
    simplifyIR()
    n("y") shouldBe IntegralConst(LongType)(10)
  }

  test("checked op simplification not-min-value div") {
    makeCFG(1@@"x=cdiv64(lc(2),lc(6))")
    simplifyIR()
    n("x") shouldBe IntegralConst(LongType)(0)
  }

  test("cyclic phies elimination") {
    makeCFG((11 -> dw(12 -> dw(13 -> (14 || 15) -> 16) -> 17)) |>|
      11@@("x", "y") |>|
      12@@"fa=phi(x,fc)" |>|
      13@@"fb=phi(fa,x)" |>|
      16@@("fc=phi(fb,fa)", "r=add(fc,y)"))
    simplifyIR()
    n("r").args.toList shouldBe Seq(n("x"), n("y"))
  }

  test("cyclic phies elimination with phi values") {
    makeCFG((1 -> (2 || ((3 || 4) -> 5)) -> 11 -> dw(12 -> dw(13 -> (14 || 15) -> 16) -> 17)) |>|
      1@@"1" |>| 2@@"2" |>| 3@@"3" |>| 4@@"4" |>|
      5@@"5=phi(3,4)" |>|
      11@@("x=phi(2,5)", "y") |>|
      12@@"fa=phi(x,fc)" |>|
      13@@"fb=phi(fa,x)" |>|
      16@@("fc=phi(fb,fa)", "r=add(fc,y)"))
    simplifyIR()
    n("r").args.toSet shouldBe Set(n("x"), n("y"))
  }

  test("if exits swapping") {
    makeCFG(1 -> (2 @@ "true=spinal()" || 3 @@ "false=spinal()") -> 4)

    val cond = addSomeConditionNode()

    {
      val branch = b(1).blockEnd.asInstanceOf[If]
      branch.selector = Not(cond)
      n("true").block shouldBe branch.trueBlock
      n("false").block shouldBe branch.falseBlock
    }

    simplifyIR()

    {
      val branch = b(1).blockEnd.asInstanceOf[If]
      branch.selector shouldBe cond
      n("true").block shouldBe branch.falseBlock
      n("false").block shouldBe branch.trueBlock
    }
  }

  def makeDefaultSwitchExits(block: Block, labels: Int*) = {
    val switch = block.blockEnd.asInstanceOf[Switch]
    switch.cases shouldBe Seq(1, 2, 3, 4)

    val defaultBlock = switch.defaultExit.target

    // replace labelled case targets with default one
    for (label <- labels) {
      val exit = switch.outCtrl(label)
      makeUnreachable(exit.outEdge)
      defaultBlock.addArg(exit)
    }

    (defaultBlock, switch.caseExits map (_.target) filter (_ != defaultBlock))
  }


  test("test switch default cases elimination") {
    makeCFG(11 -> (0 || 1 || 2 || 3 || 4) -> 12)

    val (defaultBlock, postCaseBlocks) = makeDefaultSwitchExits(b(11), 3)

    simplifyIR()

    val switch = b(11).blockEnd.asInstanceOf[Switch]
    switch.cases shouldBe Seq(1, 2, 4)
    switch.defaultExit.target shouldBe defaultBlock
    switch.caseExits map (_.target) shouldBe postCaseBlocks
  }

  test("test switch default cases elimination by goto (no phies)") {
    makeCFG(11 -> (0 || 1 || 2 || 3 || 4) -> 12)

    val (defaultBlock, _) = makeDefaultSwitchExits(b(11), 1, 2, 3, 4)

    ScalaCollections.uniqueValue(b(11).succBlocks) shouldBe Some(defaultBlock)

    simplifyIR()

    b(11).blockEnd shouldBe a[Goto]
    singleElement(b(11).succBlocks) shouldBe defaultBlock
  }

  test("test switch default cases elimination with phies") {
    makeCFG(11 -> (0 || 1 || 2 || 3 || 4) -> 12)

    val (defaultBlock, postCaseBlocks) = makeDefaultSwitchExits(b(11), 1, 2)

    val x = addNode()
    val y = addNode()
    addPhi(defaultBlock, defaultBlock.inputs collect {
      case Switch.Exit(None) => x
      case Switch.Exit(Some(1)) => x
      case Switch.Exit(Some(2)) => y
    }: _*)

    simplifyIR()

    val switch = b(11).blockEnd.asInstanceOf[Switch]
    switch.cases shouldBe Seq(2, 3, 4)
    switch.defaultExit.target shouldBe defaultBlock
    switch.caseExits map (_.target) shouldBe defaultBlock +: postCaseBlocks
  }

  test("replace tau switch by if") {
    makeCFG(0 -> (1 || 2))

    startPhase(CompilerPhase.InterProceduralAnalysis)

    makeNodes { at =>
      at(0)
      val i = b(0).blockEnd.asInstanceOf[If]
      val sw = TauSwitch(Seq(PointGuard(tA)), PGO(1, 0))(addObjNode())
      i.trueExit replaceUsesBy sw.caseExits.head
      i.falseExit replaceUsesBy sw.defaultExit
      decommit(i)
    }

    simplifyIR()

    b(0).blockEnd shouldBe an[If]
    b(0).succBlocks.toSeq shouldBe Seq(b(1), b(2))
  }

  test("phi of controlled nodes optimization") {
    makeCFG((1 -> (2 || 3) -> 4) |>|
      1@@("x", "y", "z") |>|
      2@@("l1=controlled()", "l2=controlled(x)", "l3=controlled(y)") |>|
      3@@("r1=controlled()", "r2=controlled(x)", "r3=controlled(z)") |>|
      4@@("p1=phi(l1,r1)",   "p2=phi(l2,r2)",    "p3=phi(l3,r3)",
          "u1=use(p1)",      "u2=use(p2)",       "u3=use(p3)"))

    simplifyIR()
    checkIRConsistency(CheckLevels.Important)

    singleElement(n("u1").valueArgs).asInstanceOf[FakeControlled].inCtrl should be (b(4))
    singleElement(n("u2").valueArgs).asInstanceOf[FakeControlledUnary].inCtrl should be (b(4))
    singleElement(n("u3").valueArgs).asInstanceOf[Phi] should be (n("p3"))
  }

  test("phi of controlled nodes optimization with phi not in web") {
    makeCFG((1 -> (2 || 3) -> 4) |>|
      1@@("x", "y", "z") |>|
      2@@("l1=controlled(x)") |>|
      3@@("r1=controlled(x)") |>|
      4@@("p1=phi(l1,r1)", "p2=phi(x,r1)",
          "u1=use(p1)",    "u2=use(p2)"))

    simplifyIR()
    checkIRConsistency(CheckLevels.Important)

    singleElement(n("u1").valueArgs).asInstanceOf[Phi] should be (n("p1"))
    singleElement(n("u2").valueArgs).asInstanceOf[Phi] should be (n("p2"))
  }


  test("phi of controlled nodes optimization (web)") {
    makeCFG((1 -> ((2 -> (3 || 4) -> 5) || (6 -> (7 || 8) -> 9)) -> 10) |>|
       3@@("a1=controlled()",   "a2=controlled()") |>|
       4@@("b1=controlled()",   "b2=controlled()") |>|
       5@@("pab1=phi(a1,b1)",   "pab2=phi(a2,b2)", "use(pab2)") |>|
       7@@("c1=controlled()",   "c2=controlled()") |>|
       8@@("d1=controlled()",   "d2=controlled()") |>|
       9@@("pcd1=phi(c1,d1)",   "pcd2=phi(c2,d2)") |>|
      10@@("p1=phi(pab1,pcd1)", "p2=phi(pab2,pcd2)",
           "u1=use(p1)",        "u2=use(p2)"))

    simplifyIR()
    checkIRConsistency(CheckLevels.Important)

    singleElement(n("u1").valueArgs).asInstanceOf[FakeControlled].inCtrl should be (b(10))
    singleElement(n("u2").valueArgs).asInstanceOf[Phi] should be (n("p2"))
  }

  {
    import AsmType.*
    import Condition.*
    import BitFieldExtract.*

    val cases = Seq(
      tp(IntType,   0, I8,  0, 7,  true,  None),
      tp(IntType,   0, I8,  0, 7,  false, None),

      tp(IntType,   0, I8,  0, 8,  true,  Some(0)),
      tp(IntType,   0, I8,  0, 8,  false, Some(0)),
      tp(IntType,   0, I8,  0, 16, true,  None),
      tp(IntType,   0, I8,  0, 16, false, None),
      tp(IntType,   0, I16, 0, 16, true,  Some(0)),
      tp(IntType,   0, I16, 0, 16, false, Some(0)),

      tp(IntType, 127, I8,  0, 8,  true,  Some(127)),
      tp(IntType, 127, I8,  0, 8,  false, Some(127)),
      tp(IntType, 127, I16, 0, 8,  true,  None),
      tp(IntType, 127, I16, 0, 8,  false, None),
      tp(IntType, 127, I16, 0, 16, true,  Some(127)),
      tp(IntType, 127, I16, 0, 16, false, Some(127)),

      tp(IntType, 127, U8,  0, 8,  true,  Some(127)),
      tp(IntType, 127, U8,  0, 8,  false, Some(127)),
      tp(IntType, 127, U16, 0, 8,  true,  None),
      tp(IntType, 127, U16, 0, 8,  false, None),
      tp(IntType, 127, U16, 0, 16, true,  Some(127)),
      tp(IntType, 127, U16, 0, 16, false, Some(127)),

      tp(IntType, 139, I8,  0, 8,  true,  None),
      tp(IntType, 139, I8,  0, 8,  false, Some(signExtend(139, 8))),
      tp(IntType, 139, I16, 0, 8,  true,  None),
      tp(IntType, 139, I16, 0, 8,  false, None),
      tp(IntType, 139, I16, 0, 16, true,  Some(139)),
      tp(IntType, 139, I16, 0, 16, false, Some(139)),

      tp(IntType, 139, U8,  0, 8,  true,  None),
      tp(IntType, 139, U8,  0, 8,  false, Some(139)),
      tp(IntType, 139, U16, 0, 8,  true,  None),
      tp(IntType, 139, U16, 0, 8,  false, None),
      tp(IntType, 139, U16, 0, 16, true,  Some(139)),
      tp(IntType, 139, U16, 0, 16, false, Some(139)),

      tp(IntType, 255, I8,  0, 8,  true,  None),
      tp(IntType, 255, I8,  0, 8,  false, Some(signExtend(255, 8))),
      tp(IntType, 255, I16, 0, 8,  true,  None),
      tp(IntType, 255, I16, 0, 8,  false, None),
      tp(IntType, 255, I16, 0, 16, true,  Some(255)),
      tp(IntType, 255, I16, 0, 16, false, Some(255)),

      tp(IntType, 255, U8,  0, 8,  true,  None),
      tp(IntType, 255, U8,  0, 8,  false, Some(255)),
      tp(IntType, 255, U16, 0, 8,  true,  None),
      tp(IntType, 255, U16, 0, 8,  false, None),
      tp(IntType, 255, U16, 0, 16, true,  Some(255)),
      tp(IntType, 255, U16, 0, 16, false, Some(255)),

      tp(IntType, -128, I8,  0, 8,  true,  Some(-128)),
      tp(IntType, -128, I8,  0, 8,  false, None),
      tp(IntType, -128, I16, 0, 8,  true,  None),
      tp(IntType, -128, I16, 0, 8,  false, None),
      tp(IntType, -128, I16, 0, 16, true,  Some(-128)),
      tp(IntType, -128, I16, 0, 16, false, None),

      tp(IntType, -128, U8,  0, 8,  true,  Some(-128 & 0xFF)),
      tp(IntType, -128, U8,  0, 8,  false, None),
      tp(IntType, -128, U16, 0, 8,  true,  None),
      tp(IntType, -128, U16, 0, 8,  false, None),
      tp(IntType, -128, U16, 0, 16, true,  Some(-128 & 0xFFFF)),
      tp(IntType, -128, U16, 0, 16, false, None),

      tp(IntType,  -1, I8,  0, 8,  true,  Some(-1)),
      tp(IntType,  -1, I8,  0, 8,  false, None),
      tp(IntType,  -1, I16, 0, 8,  true,  None),
      tp(IntType,  -1, I16, 0, 8,  false, None),
      tp(IntType,  -1, I16, 0, 16, true,  Some(-1)),
      tp(IntType,  -1, I16, 0, 16, false, None),

      tp(IntType,  -1, U8,  0, 8,  true,  Some(-1 & 0xFF)),
      tp(IntType,  -1, U8,  0, 8,  false, None),
      tp(IntType,  -1, U16, 0, 8,  true,  None),
      tp(IntType,  -1, U16, 0, 8,  false, None),
      tp(IntType,  -1, U16, 0, 16, true,  Some(-1 & 0xFFFF)),
      tp(IntType,  -1, U16, 0, 16, false, None),

      tp(IntType,   0, I32, 0, 8,  true,  None),
      tp(IntType,   0, I32, 0, 8,  false, None),
      tp(IntType,   0, I32, 0, 16, true,  None),
      tp(IntType,   0, I32, 0, 16, false, None),
      tp(IntType,   0, I32, 0, 32, true,  Some(0)), // redundant
      tp(IntType,   0, I32, 0, 32, false, Some(0)), // redundant

      tp(IntType,   0, I64, 0, 8,  true,  None),
      tp(IntType,   0, I64, 0, 8,  false, None),
      tp(IntType,   0, I64, 0, 16, true,  None),
      tp(IntType,   0, I64, 0, 16, false, None),
      tp(IntType,   0, I64, 0, 32, true,  None),
      tp(IntType,   0, I64, 0, 32, false, None),
      //tp(IntType,   0, I64, 0, 64, true,  Some(0)), // redundant and cannot be created due to assertion in BFX
      //tp(IntType,   0, I64, 0, 64, false, Some(0)), // redundant and cannot be created due to assertion in BFX

      tp(LongType,  0, I32, 0, 8,  true,  None),
      tp(LongType,  0, I32, 0, 8,  false, None),
      tp(LongType,  0, I32, 0, 16, true,  None),
      tp(LongType,  0, I32, 0, 16, false, None),
      tp(LongType,  0, I32, 0, 32, true,  Some(0)),
      tp(LongType,  0, I32, 0, 32, false, Some(0)),

      tp(LongType,  0, U32, 0, 8,  true,  None),
      tp(LongType,  0, U32, 0, 8,  false, None),
      tp(LongType,  0, U32, 0, 16, true,  None),
      tp(LongType,  0, U32, 0, 16, false, None),
      tp(LongType,  0, U32, 0, 32, true,  Some(0)),
      tp(LongType,  0, U32, 0, 32, false, Some(0)),

      tp(LongType,  0, I64, 0, 8,  true,  None),
      tp(LongType,  0, I64, 0, 8,  false, None),
      tp(LongType,  0, I64, 0, 16, true,  None),
      tp(LongType,  0, I64, 0, 16, false, None),
      tp(LongType,  0, I64, 0, 32, true,  None),
      tp(LongType,  0, I64, 0, 32, false, None),
    )

    for (((tpe, c, asm, offset, size, sign, positive), pos) <- cases; op <- Seq(EQ, NE)) {

      test(s"cmp($tpe, $op)(BFX($tpe, $offset, $size, $sign)(Load[$asm]), Const[$c]) optimization") {
        makeCFG(0)
        val obj = addObjNode()
        val primType = SignatureType.Primitive(asm)
        val load = LoadMemory.independent(asm, primType, atomic = false)(obj)
        val bfx = BFX(tpe, offset, size, sign, load)
        val const = IntegralConst(tpe)(c)
        n("cmp") = Cmp(tpe, op)(bfx, const)

        // Add dummy use of load
        // Note: optimization does not work if the only use of load is bfx
        Cmp(load.tpe, op)(load, IntegralConst(load.tpe)(0))

        simplifyIR()

        inside (n("cmp")) { case Cmp(`op`, l, r) =>
          positive match {
            case Some(v) => (l, r) should matchPattern {
              case (`load`, IntegralConst(`v`)) =>
            }
            case None => (l, r) should matchPattern {
              case (`bfx`, `const`) =>
            }
          }
        }
      }

      test(s"cmp($tpe, $op)(BFX($tpe, $offset, $size, $sign)(Load[$asm]), Const[$c]) no optimization (only use in BFX)") {
        makeCFG(0)
        val obj = addObjNode()
        val primType = SignatureType.Primitive(asm)
        val load = LoadMemory.independent(asm, primType, atomic = false)(obj)
        val bfx = BFX(tpe, offset, size, sign, load)
        val const = IntegralConst(tpe)(c)
        val cmp = Cmp(tpe, op)(bfx, const)

        simplifyIR()

        cmp shouldBe (Symbol("committed"))
      }
    }

    for (((tpe, c, asm, offset, size, sign, positive), pos) <- cases if tpe == IntType) {

      test(s"switch(BFX($tpe, $offset, $size, $sign)(Load[$asm]), Const[$c, 1, 2]) optimization") {
        makeCFG(0 -> (1 || 2 || 3 || 4))
        val obj = addObjNode()
        val primType = SignatureType.Primitive(asm)
        val load = LoadMemory.independent(asm, primType, atomic = false)(obj)
        val bfx = BFX(tpe, offset, size, sign, load)
        val const = IntegralConst(tpe)(c)

        // Manually replace switch with new one with custom cases
        val oldSwitch = b(0).blockEnd
        val switch = Switch(Seq(c, 1, 2))(oldSwitch.inCtrl, oldSwitch.inMemory, bfx)
        for ((oldExit, newExit) <- oldSwitch.exits zip switch.exits) {
          oldExit.outEdge.source = newExit
        }
        decommit(oldSwitch)
        b(0).blockEnd = switch

        // Add dummy use of load
        // Note: optimization does not work if the only use of load is bfx
        Cmp(load.tpe, Condition.EQ)(load, IntegralConst(load.tpe)(0))

        simplifyIR()

        positive match {
          case Some(v) if v == c =>
            switch.selector shouldBe load
          case _ =>
            switch.selector shouldBe bfx
        }
      }

      test(s"switch(BFX($tpe, $offset, $size, $sign)(Load[$asm]), Const[$c, 1, 2]) no optimization (only use in BFX)") {
        makeCFG(0 -> (1 || 2 || 3 || 4))
        val obj = addObjNode()
        val primType = SignatureType.Primitive(asm)
        val load = LoadMemory.independent(asm, primType, atomic = false)(obj)
        val bfx = BFX(tpe, offset, size, sign, load)
        val const = IntegralConst(tpe)(c)

        // Manually replace switch with new one with custom cases
        val oldSwitch = b(0).blockEnd
        val switch = Switch(Seq(c, 1, 2))(oldSwitch.inCtrl, oldSwitch.inMemory, bfx)
        for ((oldExit, newExit) <- oldSwitch.exits zip switch.exits) {
          oldExit.outEdge.source = newExit
        }
        decommit(oldSwitch)
        b(0).blockEnd = switch

        simplifyIR()

        bfx shouldBe (Symbol("committed"))
      }
    }
  }
}
