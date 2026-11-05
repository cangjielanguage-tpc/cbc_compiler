/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer

/** Tests for the Equality port (C++ Equality.cpp), including its deliberate quirks. */
class EqualitySuite extends AnyFunSuite with Matchers {

  private val tI64: Type = TypeImpl(TypeKind.Int64)
  private val tI64b: Type = TypeImpl(TypeKind.Int64)
  private val tUnit: Type = TypeImpl(TypeKind.Unit)
  private val tBool: Type = TypeImpl(TypeKind.Boolean)

  private def classDef(pkg: String, srcName: String): ClassDef =
    ClassDef(identifier = "@" + srcName, srcCodeIdentifier = srcName, packageName = pkg,
      isClass = true)

  private def funcType(params: Seq[Type], ret: Type): FuncType =
    FuncType(false, false, ArrayBuffer(params :+ ret*))

  private def mkFunc(name: String, paramTys: Seq[Type], retTy: Type): (Func, Block) = {
    val f = Func(tpe = Some(funcType(paramTys, retTy)), identifier = "@" + name,
      srcCodeIdentifier = name, rawMangledName = "@" + name, packageName = "default",
      annoInfo = Some(AnnoInfo("", ArrayBuffer())), localId = 10L, blockId = 10L, blockGroupId = 10L)
    val bg = BlockGroup(identifier = "0", ownedFunc = Some(f))
    val b0 = Block(identifier = "#0", parentGroup = Some(bg))
    bg.blocks += b0
    bg.entryBlock = Some(b0)
    f.body = Some(bg)
    for (t <- paramTys) {
      f.params += Parameter(tpe = Some(t), identifier = "%p" + f.params.size,
        ownedFunc = Some(f))
    }
    (f, b0)
  }

  private def append(block: Block, e: Expression): Unit = ChirOps.appendExpression(block, e)

  // ===================================================================================
  // keys
  // ===================================================================================

  test("custom def equality: kind/pkg/identifier/srcCodeIdentifier") {
    val d1 = classDef("default", "Foo")
    val d2 = classDef("default", "Foo")
    Equality.areEqual(d1, d2) shouldBe true
    val d3 = classDef("other", "Foo")
    Equality.areEqual(d1, d3) shouldBe false
    Equality.customDefKey(d1) shouldBe Equality.customDefKey(d2)
  }

  test("function equality: key fields + funcType + parent def") {
    val parent = classDef("default", "Foo")
    val (f1, _) = mkFunc("bar", Seq(tI64), tUnit)
    val (f2, _) = mkFunc("bar", Seq(tI64), tUnit)
    f1.declaredParent = Some(parent)
    f2.declaredParent = Some(classDef("default", "Foo"))
    Equality.areEqual(f1, f2) shouldBe true

    val (f3, _) = mkFunc("bar", Seq(tI64b), tUnit)
    f3.declaredParent = Some(classDef("default", "Foo"))
    Equality.areEqual(f1, f3) shouldBe true // structurally equal func types

    val (f4, _) = mkFunc("bar", Seq(tBool), tUnit)
    Equality.areEqual(f1, f4) shouldBe false // func types differ structurally

    val (f5, _) = mkFunc("quux", Seq(tI64), tUnit)
    Equality.areEqual(f1, f5) shouldBe false

    f2.declaredParent = None
    Equality.areEqual(f1, f2) shouldBe false
  }

  test("global var equality: identifier/srcCodeIdentifier/package") {
    val v1 = GlobalVar(tpe = Some(tI64), identifier = "@gv", srcCodeIdentifier = "gv",
      rawMangledName = "@gv", packageName = "default", annoInfo = Some(AnnoInfo("", ArrayBuffer())))
    val v2 = GlobalVar(tpe = Some(tI64), identifier = "@gv", srcCodeIdentifier = "gv",
      rawMangledName = "@gv", packageName = "default", annoInfo = Some(AnnoInfo("", ArrayBuffer())))
    Equality.areEqual(v1, v2) shouldBe true
    val v3 = GlobalVar(tpe = Some(tI64), identifier = "@gv", srcCodeIdentifier = "gv",
      rawMangledName = "@gv", packageName = "other", annoInfo = Some(AnnoInfo("", ArrayBuffer())))
    Equality.areEqual(v1, v3) shouldBe false
  }

  // ===================================================================================
  // type equality
  // ===================================================================================

  test("plain types: kind + type args, identity shortcut") {
    Equality.areEqual(tI64, tI64b) shouldBe true
    Equality.areEqual(tI64, tBool) shouldBe false
    val ref1 = TypeImpl(TypeKind.Ref, ArrayBuffer(tI64))
    val ref2 = TypeImpl(TypeKind.Ref, ArrayBuffer(tI64b))
    Equality.areEqual(ref1, ref2) shouldBe true
    val ref3 = TypeImpl(TypeKind.Ref, ArrayBuffer(tBool))
    Equality.areEqual(ref1, ref3) shouldBe false
    Equality.areEqual(tI64, tI64) shouldBe true // identity shortcut
  }

  test("func types: isCFunc/varArg/params/ret") {
    val t1 = funcType(Seq(tI64), tUnit)
    val t2 = funcType(Seq(tI64b), tUnit)
    Equality.areEqual(t1, t2) shouldBe true
    val t3 = FuncType(true, false, ArrayBuffer(tI64, tUnit))
    Equality.areEqual(t1, t3) shouldBe false // isCFunc differs
    val t4 = FuncType(false, true, ArrayBuffer(tI64, tUnit))
    Equality.areEqual(t1, t4) shouldBe false // varArg differs
  }

  test("generic types: identifier + srcCodeIdentifier only (upper bounds ignored)") {
    val g1 = GenericType("G", "G", ArrayBuffer(tI64))
    val g2 = GenericType("G", "G", ArrayBuffer(tBool))
    Equality.areEqual(g1, g2) shouldBe true
    val g3 = GenericType("G2", "G2", ArrayBuffer(tI64))
    Equality.areEqual(g1, g3) shouldBe false
  }

  test("custom types: def equality + generic args") {
    val d1 = classDef("default", "Foo")
    val d2 = classDef("default", "Foo")
    val c1 = CustomType(Some(d1))
    c1.argTys += tI64
    val c2 = CustomType(Some(d2))
    c2.argTys += tI64b
    Equality.areEqual(c1, c2) shouldBe true
    val c3 = CustomType(Some(d2))
    Equality.areEqual(c1, c3) shouldBe false // generic args differ
    val d3 = classDef("default", "Bar")
    val c4 = CustomType(Some(d3))
    Equality.areEqual(c1, c4) shouldBe false
  }

  test("raw arrays and varrays: element type compared, dims/size NOT compared (C++ quirk)") {
    val r1 = RawArrayType(tI64, 0)
    val r2 = RawArrayType(tI64b, 5)
    Equality.areEqual(r1, r2) shouldBe true
    val r3 = RawArrayType(tBool, 0)
    Equality.areEqual(r1, r3) shouldBe false
    val v1 = VArrayType(tI64, 3)
    val v2 = VArrayType(tI64b, 7)
    Equality.areEqual(v1, v2) shouldBe true // size is not part of C++ default-branch equality
    val v3 = VArrayType(tBool, 3)
    Equality.areEqual(v1, v3) shouldBe false
  }

  // ===================================================================================
  // FuncCodeEquality
  // ===================================================================================

  test("identical bodies are equal (literals, applies)") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    val (callee, _) = mkFunc("g", Seq.empty, tUnit)
    for ((b, rvName) <- Seq((b1, "%1"), (b2, "%1"))) {
      val lit = IntLiteral(tpe = Some(tI64), identifier = rvName, value = 42L)
      val konst = ConstantExpr(owner = Some(b), literal0 = lit)
      konst.resultVar = Some(LocalVar(tpe = Some(tI64), identifier = "%0", associatedExpr = Some(konst)))
      append(b, konst)
      val ap = ApplyExpr(owner = Some(b), callee0 = callee, args0 = ArrayBuffer(konst.resultVar.get))
      ap.resultVar = Some(LocalVar(tpe = Some(tUnit), identifier = "%2", associatedExpr = Some(ap)))
      append(b, ap)
      append(b, ExitExpr(owner = Some(b)))
    }
    new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get) shouldBe true
  }

  test("differing literal values are not equal") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    for ((b, v) <- Seq((b1, 42L), (b2, 43L))) {
      val lit = IntLiteral(tpe = Some(tI64), identifier = "%0", value = v)
      val konst = ConstantExpr(owner = Some(b), literal0 = lit)
      konst.resultVar = Some(LocalVar(tpe = Some(tI64), identifier = "%1", associatedExpr = Some(konst)))
      append(b, konst)
      append(b, ExitExpr(owner = Some(b)))
    }
    new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get) shouldBe false
  }

  test("invoke: methodName (callee srcCodeIdentifier + strategy) and thisType compared") {
    val (caller1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (caller2, b2) = mkFunc("f", Seq.empty, tUnit)
    val recv1 = classDef("default", "R")
    val recv2 = classDef("default", "R")
    val (m1, _) = mkFunc("m", Seq.empty, tUnit)
    val (m2, _) = mkFunc("m", Seq.empty, tUnit)
    m1.srcCodeIdentifier = "m"
    m2.srcCodeIdentifier = "m"
    for ((b, callee, recv, strat) <- Seq(
        (b1, m1, recv1, OverflowStrategy.Na), (b2, m2, recv2, OverflowStrategy.Na))) {
      val inv = InvokeExpr(owner = Some(b), objType = Some(CustomType(Some(recv))),
        callee0 = callee, caller0 = callee, overflowStrategy = strat)
      append(b, inv)
      append(b, ExitExpr(owner = Some(b)))
    }
    new Equality.FuncCodeEquality().areEqual(caller1.body.get, caller2.body.get) shouldBe true

    // different strategy => different methodName => not equal
    val inv2 = b2.exprs(0).asInstanceOf[InvokeExpr]
    inv2.overflowStrategy = OverflowStrategy.Wrapping
    new Equality.FuncCodeEquality().areEqual(caller1.body.get, caller2.body.get) shouldBe false
    inv2.overflowStrategy = OverflowStrategy.Na

    // different thisType => not equal
    inv2.objType = Some(CustomType(Some(classDef("default", "S"))))
    new Equality.FuncCodeEquality().areEqual(caller1.body.get, caller2.body.get) shouldBe false
  }

  test("apply quirk: thisType/instTypeArgs not compared, only isSuperCall") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    val (callee, _) = mkFunc("g", Seq.empty, tUnit)
    for ((b, superCall) <- Seq((b1, false), (b2, false))) {
      val ap = ApplyExpr(owner = Some(b), callee0 = callee, isSuperCall = superCall)
      append(b, ap)
      append(b, ExitExpr(owner = Some(b)))
    }
    def fresh = new Equality.FuncCodeEquality()
    fresh.areEqual(f1.body.get, f2.body.get) shouldBe true
    // different objType/instTypeArgs are ignored for Apply (C++ compares l with itself)
    b2.exprs(0).asInstanceOf[ApplyExpr].objType = Some(CustomType(Some(classDef("default", "S"))))
    b2.exprs(0).asInstanceOf[ApplyExpr].instantiatedTypeArgs += tI64
    fresh.areEqual(f1.body.get, f2.body.get) shouldBe true
    // but isSuperCall is compared
    b2.exprs(0).asInstanceOf[ApplyExpr].isSuperCall = true
    fresh.areEqual(f1.body.get, f2.body.get) shouldBe false
  }

  test("visited set: self-referencing result var terminates and compares equal") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    for (b <- Seq(b1, b2)) {
      val load = LoadExpr(owner = Some(b),
        location0 = GlobalVar(tpe = Some(TypeImpl(TypeKind.Ref, ArrayBuffer(tI64))),
          identifier = "@gv", srcCodeIdentifier = "gv", rawMangledName = "@gv",
          packageName = "default", annoInfo = Some(AnnoInfo("", ArrayBuffer()))))
      val rv = LocalVar(tpe = Some(tI64), identifier = "%0", associatedExpr = Some(load))
      load.resultVar = Some(rv)
      append(b, load)
      // artificial cycle: the load's own result feeds a binary op that feeds itself
      val bin = BinaryExpr(owner = Some(b), op = BinaryOp.Add, overflowStrategy = OverflowStrategy.Na,
        leftOperand0 = rv, rightOperand0 = rv)
      bin.resultVar = Some(rv) // result var is also an operand (cycle)
      append(b, bin)
      append(b, ExitExpr(owner = Some(b)))
    }
    new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get) shouldBe true
  }

  test("different expr kinds are not equal") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    append(b1, ExitExpr(owner = Some(b1)))
    append(b2, GotoExpr(owner = Some(b2), destination0 = b2))
    new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get) shouldBe false
  }

  // Distinct C++ ExprKinds folded into one model class must compare their op field.
  test("differing binary ops are not equal (Equal vs NotEqual, port of ExprKind check)") {
    val (f1, b1) = mkFunc("f", Seq(tI64), tUnit)
    val (f2, b2) = mkFunc("f", Seq(tI64), tUnit)
    for ((b, op) <- Seq((b1, BinaryOp.Eq), (b2, BinaryOp.NotEq))) {
      val p = b.parentGroup.get.ownedFunc.get.params.head
      val bin = BinaryExpr(owner = Some(b), op = op, overflowStrategy = OverflowStrategy.Throwing,
        leftOperand0 = p, rightOperand0 = p)
      bin.resultVar = Some(LocalVar(tpe = Some(tBool), identifier = "%9", associatedExpr = Some(bin)))
      append(b, bin)
      append(b, ExitExpr(owner = Some(b)))
    }
    new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get) shouldBe false
    val (f3, b3) = mkFunc("f", Seq(tI64), tUnit)
    val (f4, b4) = mkFunc("f", Seq(tI64), tUnit)
    for (b <- Seq(b3, b4)) {
      val p = b.parentGroup.get.ownedFunc.get.params.head
      val bin = BinaryExpr(owner = Some(b), op = BinaryOp.NotEq, overflowStrategy = OverflowStrategy.Throwing,
        leftOperand0 = p, rightOperand0 = p)
      bin.resultVar = Some(LocalVar(tpe = Some(tBool), identifier = "%9", associatedExpr = Some(bin)))
      append(b, bin)
      append(b, ExitExpr(owner = Some(b)))
    }
    new Equality.FuncCodeEquality().areEqual(f3.body.get, f4.body.get) shouldBe true
  }

  test("differing unary ops are not equal (Neg vs Not)") {
    val (f1, b1) = mkFunc("f", Seq(tI64), tUnit)
    val (f2, b2) = mkFunc("f", Seq(tI64), tUnit)
    for ((b, op) <- Seq((b1, UnaryOp.Neg), (b2, UnaryOp.Not))) {
      val un = UnaryExpr(owner = Some(b), op = op, overflowStrategy = OverflowStrategy.Na,
        operand0 = b.parentGroup.get.ownedFunc.get.params.head)
      un.resultVar = Some(LocalVar(tpe = Some(tI64), identifier = "%9", associatedExpr = Some(un)))
      append(b, un)
      append(b, ExitExpr(owner = Some(b)))
    }
    new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get) shouldBe false
  }

  test("differing cast ops are not equal (Box vs UnboxToRef)") {
    val (f1, b1) = mkFunc("f", Seq(tI64), tUnit)
    val (f2, b2) = mkFunc("f", Seq(tI64), tUnit)
    for ((b, op) <- Seq((b1, CastOp.Box), (b2, CastOp.UnboxToRef))) {
      val cast = CastExpr(owner = Some(b), op = op, value0 = b.parentGroup.get.ownedFunc.get.params.head)
      cast.resultVar = Some(LocalVar(tpe = Some(tI64), identifier = "%9", associatedExpr = Some(cast)))
      append(b, cast)
      append(b, ExitExpr(owner = Some(b)))
    }
    new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get) shouldBe false
  }
}
