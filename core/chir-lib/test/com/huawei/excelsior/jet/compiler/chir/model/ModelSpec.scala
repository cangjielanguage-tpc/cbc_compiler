/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.model

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

/** Unit tests for the version-agnostic model: package accessors, terminator/successor
  * views, and enum-ordinal stability against the flatbuffer schema.
  */
class ModelSpec extends AnyFunSuite {

  test("enum ordinals mirror the flatbuffer schema values") {
    assert(TypeKind.Invalid.ordinal == 0)
    assert(TypeKind.Int32.ordinal == 3)
    assert(TypeKind.Tuple.ordinal == 19)
    assert(TypeKind.Struct.ordinal == 20)
    assert(TypeKind.Enum.ordinal == 21)
    assert(TypeKind.Func.ordinal == 22)
    assert(TypeKind.Class.ordinal == 23)
    assert(TypeKind.This.ordinal == 31)
    assert(ValueKind.Literal.ordinal == 0)
    assert(ValueKind.BlockGroup.ordinal == 6)
    assert(FuncKind.Default.ordinal == 0)
    assert(CustomDefKind.Struct.ordinal == 0)
    assert(CustomDefKind.Extend.ordinal == 3)
    assert(PackageAccessLevel.Public.ordinal == 3)
    assert(Phase.Opt.ordinal == 1)
    // v1203-only CHECKED sits between NA and WRAPPING in the shared model enum
    assert(OverflowStrategy.Na.ordinal == 0)
    assert(OverflowStrategy.Checked.ordinal == 1)
    assert(OverflowStrategy.Wrapping.ordinal == 2)
  }

  test("Package.getFunc/getDef accept prefixed and unprefixed identifiers") {
    val pkg = Fixture.build()
    assert(pkg.getFunc("@N.Pkg.main").isDefined)
    assert(pkg.getFunc("N.Pkg.main").get eq pkg.getFunc("@N.Pkg.main").get)
    assert(pkg.getFunc("@N.Pkg.missing").isEmpty)
    assert(pkg.getDef("N.Pkg.St").get.eq(pkg.getDef("@N.Pkg.St").get))
    assert(pkg.getDef("@N.Imp.ImpSt").get.eq(pkg.getDef("N.Imp.ImpSt").get))
  }

  test("Package def lists keep imported/current grouping and order") {
    val pkg = Fixture.build()
    assert(pkg.allCustomTypeDefs.map(_.identifier) ==
      Seq("@N.Imp.ImpSt", "@N.Pkg.St", "@N.Pkg.Cl", "@N.Pkg.Color", "@N.Pkg.Ext"))
    assert(pkg.curPkgCustomTypeDefs.map(_.identifier) ==
      Seq("@N.Pkg.St", "@N.Pkg.Cl", "@N.Pkg.Color", "@N.Pkg.Ext"))
    assert(pkg.importedStructs.head.isImported)
    assert(!pkg.structs.head.isImported)
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
    assert(!alloc.isTerminator)
    assert(tryApply.isTerminator)
    assert(mbranch.isTerminator)
    assert(goto.isTerminator)

    // successors: try-apply -> [normal, exception] blocks;
    // multibranch -> default then normals
    assert(tryApply.successors == Seq(bg.blocks(1), bg.blocks(4)))
    assert(mbranch.successors == Seq(bg.blocks(3), bg.blocks(2), bg.blocks(3)))
    assert(goto.successors == Seq(bg.blocks(3)))
    assert(alloc.successors.isEmpty)
  }

  test("FuncKind/MainEntry and Func helpers") {
    val pkg = Fixture.build()
    val main = pkg.getFunc("@N.Pkg.main").get
    assert(main.funcKind == FuncKind.MainEntry)
    assert(!main.isLambda)
    assert(main.generateLocalId() == main.localId) // counter semantics: increment then return
  }
}
