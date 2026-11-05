/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

/** Unit tests for the version-agnostic model: package accessors, terminator/successor
  * views, and enum-ordinal stability against the flatbuffer schema.
  */
class ModelSuite extends AnyFunSuite with Matchers {

  test("enum ordinals mirror the flatbuffer schema values") {
    TypeKind.Invalid.ordinal shouldBe 0
    TypeKind.Int32.ordinal shouldBe 3
    TypeKind.Tuple.ordinal shouldBe 19
    TypeKind.Struct.ordinal shouldBe 20
    TypeKind.Enum.ordinal shouldBe 21
    TypeKind.Func.ordinal shouldBe 22
    TypeKind.Class.ordinal shouldBe 23
    TypeKind.This.ordinal shouldBe 31
    ValueKind.Literal.ordinal shouldBe 0
    ValueKind.BlockGroup.ordinal shouldBe 6
    FuncKind.Default.ordinal shouldBe 0
    CustomDefKind.Struct.ordinal shouldBe 0
    CustomDefKind.Extend.ordinal shouldBe 3
    PackageAccessLevel.Public.ordinal shouldBe 3
    Phase.Opt.ordinal shouldBe 1
    // v1203-only CHECKED sits between NA and WRAPPING in the shared model enum
    OverflowStrategy.Na.ordinal shouldBe 0
    OverflowStrategy.Checked.ordinal shouldBe 1
    OverflowStrategy.Wrapping.ordinal shouldBe 2
  }

  test("Package.getFunc/getDef accept prefixed and unprefixed identifiers") {
    val pkg = Fixture.build()
    pkg.getFunc("@N.Pkg.main") shouldBe defined
    pkg.getFunc("N.Pkg.main").get shouldBe pkg.getFunc("@N.Pkg.main").get
    pkg.getFunc("@N.Pkg.missing") shouldBe empty
    pkg.getDef("N.Pkg.St").get should be theSameInstanceAs pkg.getDef("@N.Pkg.St").get
    pkg.getDef("@N.Imp.ImpSt").get should be theSameInstanceAs pkg.getDef("N.Imp.ImpSt").get
  }

  test("Package def lists keep imported/current grouping and order") {
    val pkg = Fixture.build()
    pkg.allCustomTypeDefs.map(_.identifier) shouldBe
      Seq("@N.Imp.ImpSt", "@N.Pkg.St", "@N.Pkg.Cl", "@N.Pkg.Color", "@N.Pkg.Ext")
    pkg.curPkgCustomTypeDefs.map(_.identifier) shouldBe
      Seq("@N.Pkg.St", "@N.Pkg.Cl", "@N.Pkg.Color", "@N.Pkg.Ext")
    pkg.importedStructs.head.isImported shouldBe true
    pkg.structs.head.isImported shouldBe false
  }

  test("isTerminator and successors mirror the C++ IR semantics") {
    val pkg = Fixture.build()
    val main = pkg.getFunc("@N.Pkg.main").get
    val bg = main.body.get
    val b0 = bg.blocks(0)

    val alloc = b0.exprs(0).asInstanceOf[AllocateExpr]
    val tryApply = b0.exprs(2).asInstanceOf[ApplyExpr]
    val mbranch = bg.blocks(1).exprs(1).asInstanceOf[MultiBranchExpr]
    val goto = bg.blocks(2).exprs.collectFirst { case g: GotoExpr => g }.get

    // non-try expressions are not terminators; try variants are
    alloc.isTerminator shouldBe false
    tryApply.isTerminator shouldBe true
    mbranch.isTerminator shouldBe true
    goto.isTerminator shouldBe true

    // successors: try-apply -> [normal, exception] blocks;
    // multibranch -> default then normals
    tryApply.successors shouldBe Seq(bg.blocks(1), bg.blocks(4))
    mbranch.successors shouldBe Seq(bg.blocks(3), bg.blocks(2), bg.blocks(3))
    goto.successors shouldBe Seq(bg.blocks(3))
    alloc.successors shouldBe empty
  }

  test("FuncKind/MainEntry and Func helpers") {
    val pkg = Fixture.build()
    val main = pkg.getFunc("@N.Pkg.main").get
    main.funcKind shouldBe FuncKind.MainEntry
    main.isLambda shouldBe false
    main.generateLocalId() shouldBe 20 // counter semantics: return current, then increment
    main.generateLocalId() shouldBe 21
  }
}
