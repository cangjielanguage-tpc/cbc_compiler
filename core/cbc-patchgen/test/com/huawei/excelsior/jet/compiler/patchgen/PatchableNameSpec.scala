/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*
import com.huawei.excelsior.jet.compiler.patchgen.PatchableName.GuardMethodKind
import com.huawei.excelsior.jet.compiler.patchgen.PatchableName.PackageInitAccessorKind
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer

class PatchableNameSpec extends AnyFunSuite {

  private val tI64: Type = TypeImpl(TypeKind.Int64)
  private val tRefI64: Type = TypeImpl(TypeKind.Ref, ArrayBuffer(tI64))

  private def structDef(pkg: String, srcName: String): ClassDef = {
    val d = ClassDef(identifier = "@" + srcName, srcCodeIdentifier = srcName, packageName = pkg,
      isClass = true)
    CustomType(Some(d))
    d
  }

  private def funcType(params: Seq[Type], ret: Type): FuncType =
    FuncType(false, false, ArrayBuffer(params :+ ret*))

  private def func(pkg: String, srcName: String, parent: Option[CustomTypeDef],
                   params: Seq[Type]): Func = {
    val f = Func(tpe = Some(funcType(params, TypeImpl(TypeKind.Unit))),
      identifier = "@" + pkg + "." + srcName, srcCodeIdentifier = srcName,
      rawMangledName = "@mangled", packageName = pkg, declaredParent = parent,
      annoInfo = Some(AnnoInfo("", ArrayBuffer())), localId = 1L, blockId = 1L, blockGroupId = 1L)
    f
  }

  test("package-level names") {
    val pkg = new Package()
    pkg.name = "default"
    val pn = PatchableName(pkg)
    assert(pn.qualifiedName == "default")
    assert(pn.genGuardVarInitializedName == "$default$GVI")
    assert(pn.genGuardClassName(forPackageInit = false) == "$default$Patch$GC")
    assert(pn.genGuardClassName(forPackageInit = true) == "$default$PackageInitPatch$GC")
    assert(pn.genGuardVarName(forPackageInit = false) == "$default$GV")
    assert(pn.genGuardVarName(forPackageInit = true) == "$default$packageInit$GV")
    assert(pn.genGuardVarFlagName(forPackageInit = false) == "$default$GVF")
    assert(pn.genPatchClassName == "$PatchImpl$GC")
    assert(pn.genGuardMethodName(GuardMethodKind.PackageInitCtor) ==
      "$default$PackageInitPatch$GC$<init>$GF")
    assert(pn.genGuardMethodName(GuardMethodKind.PackageInit) ==
      "$default$PackageInitPatch$GC$packageInit$GF")
    assert(pn.genGuardMethodName(GuardMethodKind.PackageLiteralInit) ==
      "$default$PackageInitPatch$GC$packageLiteralInit$GF")
  }

  test("package-level flag accessors (no class name, no func name)") {
    val pkg = new Package()
    pkg.name = "default"
    val pn = PatchableName(pkg)
    assert(pn.genPackageInitFlagAccessor(PackageInitAccessorKind.PackageInit, isGetter = true) ==
      "$defaultpackageInitGetter$GF")
    assert(pn.genPackageInitFlagAccessor(PackageInitAccessorKind.PackageInit, isGetter = false) ==
      "$defaultpackageInitSetter$GF")
    assert(pn.genPackageInitFlagAccessor(PackageInitAccessorKind.PackageLiteralInit, isGetter = true) ==
      "$defaultpackageLiteralInitGetter$GF")
    assert(pn.genPackageInitFlagAccessor(PackageInitAccessorKind.PackageLiteralInit, isGetter = false) ==
      "$defaultpackageLiteralInitSetter$GF")
  }

  test("class-level names") {
    val guardClass = structDef("default", "$default$Patch$GC")
    val pn = PatchableName(guardClass)
    assert(pn.qualifiedName == "default.$default$Patch$GC")
    // "Impl" inserted before the $GC suffix
    assert(pn.genPatchClassName == "$default$PatchImpl$GC")
    assert(pn.genSyntheticCtorName == "$default$$default$Patch$GC$$CTOR")

    val plainClass = structDef("default", "Foo")
    val pn2 = PatchableName(plainClass)
    assert(pn2.genPackageInitFlagAccessor(PackageInitAccessorKind.PackageInit, isGetter = true) ==
      "$default$FoopackageInitGetter$GF")
  }

  test("func-level names (class method with params)") {
    val foo = structDef("default", "Foo")
    val fooTy = CustomType(Some(foo))
    val f = func("default", "bar", Some(foo), Seq(fooTy, tI64))
    val pn = PatchableName(f)
    assert(pn.qualifiedName == "default.Foo.bar")
    // genName("$GC", "$GF") = "$default$Foo$GC$bar$$default.Foo$_Int64$GF",
    // then "$Patch$GC" inserted after "$default"
    assert(pn.genGuardMethodName(GuardMethodKind.Plain) ==
      "$default$Patch$GC$Foo$GC$bar$$default.Foo$_Int64$GF")
  }

  test("func-level names (global func without params)") {
    val f = func("default", "baz", None, Seq.empty)
    val pn = PatchableName(f)
    assert(pn.qualifiedName == "default.baz")
    assert(pn.genGuardMethodName(GuardMethodKind.Plain) == "$default$Patch$GC$baz$GF")
    assert(pn.genGuardVarFlagName(forPackageInit = false) == "$default$baz$GVF")
  }

  test("qualified name rendering of param types") {
    // RawArray<Ref(VArray<Int64, 3>)> as a single param exercises the nested renderers
    val varr = VArrayType(tI64, 3)
    val rawRef = TypeImpl(TypeKind.Ref, ArrayBuffer(varr))
    val raw = RawArrayType(rawRef, 0)
    val f = func("default", "qux", None, Seq(raw))
    val pn = PatchableName(f)
    assert(pn.genGuardMethodName(GuardMethodKind.Plain) ==
      "$default$Patch$GC$qux$$RawArray<VArray<Int64,3>>$GF")
  }

  test("equality is by qualified name") {
    val f1 = func("default", "bar", None, Seq(tI64))
    val f2 = func("default", "bar", None, Seq(tI64))
    assert(PatchableName(f1) == PatchableName(f2))
    val f3 = func("default", "quux", None, Seq(tI64))
    assert(PatchableName(f1) != PatchableName(f3))
  }
}
