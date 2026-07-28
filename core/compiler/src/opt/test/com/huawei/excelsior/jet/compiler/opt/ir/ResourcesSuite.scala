/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot.CallParam

class ResourcesSuite extends CompilerSuite {

  // Some custom resource
  case object FooResource extends Location.Other with CustomLocation
  val fooSet = setOf(FooResource)

  test("simple test") {
    (emptySet | immSet) shouldBe immSet
    emptySet.## shouldBe 0
    ((immSet | fooSet) - Immediate) shouldBe fooSet

    val sA = (immSet | fooSet | invalidSet)
    val sB = (invalidSet | immSet | fooSet)
    val sC = (invalidSet | fooSet | immSet)
    (sA == sB && sA == sC && sC == sB) shouldBe true
    sA.size shouldBe 3
    sA filter { r => !(immSet contains r) } shouldBe (sB - sC.head)

    (emptySet disjointWith universalSet) shouldBe true
    (fooSet disjointWith universalSet) shouldBe false
    (universalSet disjointWith immSet) shouldBe false

    (universalSet | immSet) shouldBe universalSet
    (universalSet & immSet) shouldBe immSet
    an [AssertionError] should be thrownBy (universalSet &~ immSet)
    (immSet | universalSet) shouldBe universalSet
    (immSet & universalSet) shouldBe immSet
    (immSet &~ universalSet) shouldBe emptySet

    (immSet subsetOf emptySet) shouldBe false
    (emptySet subsetOf immSet) shouldBe true
    (immSet subsetOf fooSet) shouldBe false
    (fooSet subsetOf immSet) shouldBe false
    (immSet subsetOf immSet) shouldBe true
    (fooSet subsetOf fooSet) shouldBe true
    (immSet subsetOf (immSet | fooSet)) shouldBe true
    (fooSet subsetOf (immSet | fooSet)) shouldBe true
    ((immSet | fooSet) subsetOf immSet) shouldBe false
    ((immSet | fooSet) subsetOf fooSet) shouldBe false
    (immSet subsetOf universalSet) shouldBe true
    (universalSet subsetOf immSet) shouldBe false
    (universalSet subsetOf universalSet) shouldBe true
  }

  test("setOf, unionOf") {
    setOf() shouldBe emptySet
    setOf(Immediate) shouldBe immSet
    setOf(FooResource) shouldBe fooSet
    setOf(Immediate, FooResource) shouldBe (immSet | fooSet)

    setOf(Immediate, FooResource, InvalidResource) shouldBe (immSet | fooSet | invalidSet)
    setOf(Immediate, FooResource, InvalidResource) shouldBe unionOf(Seq(immSet, fooSet, invalidSet))

    unionOf(Seq(immSet, fooSet, invalidSet, emptySet, immSet)) shouldBe (immSet | fooSet | invalidSet)
    unionOf(Seq(immSet, fooSet, universalSet, invalidSet)) shouldBe universalSet
  }

  test("++, mutable") {
    val is = fooSet
    (is ++ Seq.empty) shouldBe fooSet
    (is ++ Seq(FooResource)) shouldBe fooSet
    (is ++ Seq(Immediate)) shouldBe (immSet | fooSet)
    (is ++ Seq(InvalidResource)) shouldBe (fooSet | invalidSet)
    (is ++ Seq(Immediate, FooResource, InvalidResource)) shouldBe (immSet | fooSet | invalidSet)

    val ms = emptyMSet() | fooSet
    val ms2 = ms ++ Seq.empty
    val ms3 = ms ++ Seq(FooResource)
    ms2 shouldBe fooSet
    ms3 shouldBe fooSet
    (ms ++ Seq(Immediate)) shouldBe (immSet | fooSet)
    (ms ++ Seq(InvalidResource)) shouldBe (fooSet | invalidSet)
    (ms ++ Seq(Immediate, FooResource, InvalidResource)) shouldBe (immSet | fooSet | invalidSet)

    ms ++= Seq(Immediate, FooResource, InvalidResource)
    ms2 shouldBe fooSet
    ms3 shouldBe fooSet
  }

  test("--, mutable") {
    val is = fooSet
    val is0 = immSet | fooSet | invalidSet
    (is -- Seq.empty) shouldBe fooSet
    (is -- Seq(Immediate)) shouldBe fooSet
    (is -- Seq(FooResource)) shouldBe emptySet
    (is0 -- Seq(InvalidResource)) shouldBe (immSet | fooSet)
    (is0 -- Seq(Immediate, FooResource, InvalidResource)) shouldBe emptySet

    val ms = emptyMSet() | fooSet
    val ms0 = emptyMSet() | immSet | fooSet | invalidSet
    val ms2 = ms -- Seq.empty
    val ms3 = ms -- Seq(Immediate)
    ms2 shouldBe fooSet
    ms3 shouldBe fooSet
    (ms -- Seq(FooResource)) shouldBe emptySet
    (ms0 -- Seq(InvalidResource)) shouldBe (immSet | fooSet)
    (ms0 -- Seq(Immediate, FooResource, InvalidResource)) shouldBe emptySet

    ms ++= Seq(Immediate, FooResource, InvalidResource)
    ms2 shouldBe fooSet
    ms3 shouldBe fooSet
  }

  test("&~, refs, mutable") {
    val f0 = new FrameSlot(CallParam, 0)
    val f1 = new FrameSlot(CallParam, 0)
    val f2 = new FrameSlot(CallParam, 0)
    val x = mutableSetOf(Seq(f0, f1))
    val y = mutableSetOf(Seq(f0, f2))
    val z = x &~ y
    (z contains f0) shouldBe false
    z shouldBe mutableSetOf(Seq(f1))
  }

  test("|, refs, mutable") {
    val f0 = new FrameSlot(CallParam, 0)
    val f1 = new FrameSlot(CallParam, 0)
    val f2 = new FrameSlot(CallParam, 0)
    val x = mutableSetOf(Seq(f0, f1))
    val y = mutableSetOf(Seq(f2, f0))
    val z = x | y
    (z contains f2) shouldBe true
    z shouldBe mutableSetOf(Seq(f0, f1, f2))
  }
}
