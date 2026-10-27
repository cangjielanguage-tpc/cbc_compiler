/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer

/** Tests for the Equality port (C++ Equality.cpp), including its deliberate quirks. */
class EqualitySpec extends AnyFunSuite {

  private val tI64: Type = TypeImpl(TypeKind.Int64)
  private val tI64b: Type = TypeImpl(TypeKind.Int64)
  private val tUnit: Type = TypeImpl(TypeKind.Unit)
  private val tBool: Type = TypeImpl(TypeKind.Boolean)

  private def classDef(pkg: String, srcName: String): ClassDef =
    ClassDef(ArrayBuffer(), None, 0L, srcName, "@" + srcName, pkg, None, None,
      ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), None, ArrayBuffer(), None,
      true, false, ArrayBuffer(), None)

  private def funcType(params: Seq[Type], ret: Type): FuncType =
    FuncType(false, false, ArrayBuffer(params :+ ret*))

  private def mkFunc(name: String, paramTys: Seq[Type], retTy: Type): (Func, Block) = {
    val f = Func(ArrayBuffer(), None, 0L, Some(funcType(paramTys, retTy)), "@" + name, name,
      "@" + name, "default", None, ArrayBuffer(), Some(AnnoInfo("", ArrayBuffer())), None,
      FuncKind.Default, false, false, None, ArrayBuffer(), None, None, ArrayBuffer(), None,
      None, 10L, 10L, 10L)
    val bg = BlockGroup(ArrayBuffer(), None, 0L, None, "0", None, ArrayBuffer(), Some(f), None)
    val b0 = Block(ArrayBuffer(), None, 0L, None, "#0", Some(bg), ArrayBuffer(), ArrayBuffer(),
      false, ArrayBuffer())
    bg.blocks += b0
    bg.entryBlock = Some(b0)
    f.body = Some(bg)
    for (t <- paramTys) {
      f.params += Parameter(ArrayBuffer(), None, 0L, Some(t), "%p" + f.params.size, Some(f), None,
        "", None)
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
    assert(Equality.areEqual(d1, d2))
    val d3 = classDef("other", "Foo")
    assert(!Equality.areEqual(d1, d3))
    assert(Equality.customDefKey(d1) == Equality.customDefKey(d2))
  }

  test("function equality: key fields + funcType + parent def") {
    val parent = classDef("default", "Foo")
    val (f1, _) = mkFunc("bar", Seq(tI64), tUnit)
    val (f2, _) = mkFunc("bar", Seq(tI64), tUnit)
    f1.declaredParent = Some(parent)
    f2.declaredParent = Some(classDef("default", "Foo"))
    assert(Equality.areEqual(f1, f2))

    val (f3, _) = mkFunc("bar", Seq(tI64b), tUnit)
    f3.declaredParent = Some(classDef("default", "Foo"))
    assert(Equality.areEqual(f1, f3)) // structurally equal func types

    val (f4, _) = mkFunc("bar", Seq(tBool), tUnit)
    assert(!Equality.areEqual(f1, f4)) // func types differ structurally

    val (f5, _) = mkFunc("quux", Seq(tI64), tUnit)
    assert(!Equality.areEqual(f1, f5))

    f2.declaredParent = None
    assert(!Equality.areEqual(f1, f2))
  }

  test("global var equality: identifier/srcCodeIdentifier/package") {
    val v1 = GlobalVar(ArrayBuffer(), None, 0L, Some(tI64), "@gv", "gv", "@gv", "default", None,
      ArrayBuffer(), Some(AnnoInfo("", ArrayBuffer())), None)
    val v2 = GlobalVar(ArrayBuffer(), None, 0L, Some(tI64), "@gv", "gv", "@gv", "default", None,
      ArrayBuffer(), Some(AnnoInfo("", ArrayBuffer())), None)
    assert(Equality.areEqual(v1, v2))
    val v3 = GlobalVar(ArrayBuffer(), None, 0L, Some(tI64), "@gv", "gv", "@gv", "other", None,
      ArrayBuffer(), Some(AnnoInfo("", ArrayBuffer())), None)
    assert(!Equality.areEqual(v1, v3))
  }

  // ===================================================================================
  // type equality
  // ===================================================================================

  test("plain types: kind + type args, identity shortcut") {
    assert(Equality.areEqual(tI64, tI64b))
    assert(!Equality.areEqual(tI64, tBool))
    val ref1 = TypeImpl(TypeKind.Ref, ArrayBuffer(tI64))
    val ref2 = TypeImpl(TypeKind.Ref, ArrayBuffer(tI64b))
    assert(Equality.areEqual(ref1, ref2))
    val ref3 = TypeImpl(TypeKind.Ref, ArrayBuffer(tBool))
    assert(!Equality.areEqual(ref1, ref3))
    assert(Equality.areEqual(tI64, tI64)) // identity shortcut
  }

  test("func types: isCFunc/varArg/params/ret") {
    val t1 = funcType(Seq(tI64), tUnit)
    val t2 = funcType(Seq(tI64b), tUnit)
    assert(Equality.areEqual(t1, t2))
    val t3 = FuncType(true, false, ArrayBuffer(tI64, tUnit))
    assert(!Equality.areEqual(t1, t3)) // isCFunc differs
    val t4 = FuncType(false, true, ArrayBuffer(tI64, tUnit))
    assert(!Equality.areEqual(t1, t4)) // varArg differs
  }

  test("generic types: identifier + srcCodeIdentifier only (upper bounds ignored)") {
    val g1 = GenericType("G", "G", ArrayBuffer(tI64))
    val g2 = GenericType("G", "G", ArrayBuffer(tBool))
    assert(Equality.areEqual(g1, g2))
    val g3 = GenericType("G2", "G2", ArrayBuffer(tI64))
    assert(!Equality.areEqual(g1, g3))
  }

  test("custom types: def equality + generic args") {
    val d1 = classDef("default", "Foo")
    val d2 = classDef("default", "Foo")
    val c1 = CustomType(Some(d1))
    c1.argTys += tI64
    val c2 = CustomType(Some(d2))
    c2.argTys += tI64b
    assert(Equality.areEqual(c1, c2))
    val c3 = CustomType(Some(d2))
    assert(!Equality.areEqual(c1, c3)) // generic args differ
    val d3 = classDef("default", "Bar")
    val c4 = CustomType(Some(d3))
    assert(!Equality.areEqual(c1, c4))
  }

  test("raw arrays and varrays: element type compared, dims/size NOT compared (C++ quirk)") {
    val r1 = RawArrayType(tI64, 0)
    val r2 = RawArrayType(tI64b, 5)
    assert(Equality.areEqual(r1, r2))
    val r3 = RawArrayType(tBool, 0)
    assert(!Equality.areEqual(r1, r3))
    val v1 = VArrayType(tI64, 3)
    val v2 = VArrayType(tI64b, 7)
    assert(Equality.areEqual(v1, v2)) // size is not part of C++ default-branch equality
    val v3 = VArrayType(tBool, 3)
    assert(!Equality.areEqual(v1, v3))
  }

  // ===================================================================================
  // FuncCodeEquality
  // ===================================================================================

  test("identical bodies are equal (literals, applies)") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    val (callee, _) = mkFunc("g", Seq.empty, tUnit)
    for ((b, rvName) <- Seq((b1, "%1"), (b2, "%1"))) {
      val lit = IntLiteral(ArrayBuffer(), None, 0L, Some(tI64), rvName, 42L)
      val konst = ConstantExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None, lit)
      konst.resultVar = Some(LocalVar(ArrayBuffer(), None, 0L, Some(tI64), "%0", Some(konst),
        isRetVal = false, ""))
      append(b, konst)
      val ap = ApplyExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None, ArrayBuffer(),
        None, callee, ArrayBuffer(konst.resultVar.get), isSuperCall = false, isTry = false)
      ap.resultVar = Some(LocalVar(ArrayBuffer(), None, 0L, Some(tUnit), "%2", Some(ap),
        isRetVal = false, ""))
      append(b, ap)
      append(b, ExitExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None))
    }
    assert(new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get))
  }

  test("differing literal values are not equal") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    for ((b, v) <- Seq((b1, 42L), (b2, 43L))) {
      val lit = IntLiteral(ArrayBuffer(), None, 0L, Some(tI64), "%0", v)
      val konst = ConstantExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None, lit)
      konst.resultVar = Some(LocalVar(ArrayBuffer(), None, 0L, Some(tI64), "%1", Some(konst),
        isRetVal = false, ""))
      append(b, konst)
      append(b, ExitExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None))
    }
    assert(!new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get))
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
      val inv = InvokeExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None, ArrayBuffer(),
        Some(CustomType(Some(recv))), callee, callee, ArrayBuffer(), strat, isTry = false)
      append(b, inv)
      append(b, ExitExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None))
    }
    assert(new Equality.FuncCodeEquality().areEqual(caller1.body.get, caller2.body.get))

    // different strategy => different methodName => not equal
    val inv2 = b2.exprs(0).asInstanceOf[InvokeExpr]
    inv2.overflowStrategy = OverflowStrategy.Wrapping
    assert(!new Equality.FuncCodeEquality().areEqual(caller1.body.get, caller2.body.get))
    inv2.overflowStrategy = OverflowStrategy.Na

    // different thisType => not equal
    inv2.objType = Some(CustomType(Some(classDef("default", "S"))))
    assert(!new Equality.FuncCodeEquality().areEqual(caller1.body.get, caller2.body.get))
  }

  test("apply quirk: thisType/instTypeArgs not compared, only isSuperCall") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    val (callee, _) = mkFunc("g", Seq.empty, tUnit)
    for ((b, superCall) <- Seq((b1, false), (b2, false))) {
      val ap = ApplyExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None, ArrayBuffer(),
        None, callee, ArrayBuffer(), superCall, isTry = false)
      append(b, ap)
      append(b, ExitExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None))
    }
    def fresh = new Equality.FuncCodeEquality()
    assert(fresh.areEqual(f1.body.get, f2.body.get))
    // different objType/instTypeArgs are ignored for Apply (C++ compares l with itself)
    b2.exprs(0).asInstanceOf[ApplyExpr].objType = Some(CustomType(Some(classDef("default", "S"))))
    b2.exprs(0).asInstanceOf[ApplyExpr].instantiatedTypeArgs += tI64
    assert(fresh.areEqual(f1.body.get, f2.body.get))
    // but isSuperCall is compared
    b2.exprs(0).asInstanceOf[ApplyExpr].isSuperCall = true
    assert(!fresh.areEqual(f1.body.get, f2.body.get))
  }

  test("visited set: self-referencing result var terminates and compares equal") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    for (b <- Seq(b1, b2)) {
      val load = LoadExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None,
        GlobalVar(ArrayBuffer(), None, 0L, Some(TypeImpl(TypeKind.Ref, ArrayBuffer(tI64))),
          "@gv", "gv", "@gv", "default", None, ArrayBuffer(), Some(AnnoInfo("", ArrayBuffer())),
          None))
      val rv = LocalVar(ArrayBuffer(), None, 0L, Some(tI64), "%0", Some(load), isRetVal = false, "")
      load.resultVar = Some(rv)
      append(b, load)
      // artificial cycle: the load's own result feeds a binary op that feeds itself
      val bin = BinaryExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None, BinaryOp.Add,
        OverflowStrategy.Na, rv, rv, isTry = false)
      bin.resultVar = Some(rv) // result var is also an operand (cycle)
      append(b, bin)
      append(b, ExitExpr(ArrayBuffer(), None, 0L, Some(b), ArrayBuffer(), None))
    }
    assert(new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get))
  }

  test("different expr kinds are not equal") {
    val (f1, b1) = mkFunc("f", Seq.empty, tUnit)
    val (f2, b2) = mkFunc("f", Seq.empty, tUnit)
    append(b1, ExitExpr(ArrayBuffer(), None, 0L, Some(b1), ArrayBuffer(), None))
    append(b2, GotoExpr(ArrayBuffer(), None, 0L, Some(b2), ArrayBuffer(), None, b2))
    assert(!new Equality.FuncCodeEquality().areEqual(f1.body.get, f2.body.get))
  }
}
