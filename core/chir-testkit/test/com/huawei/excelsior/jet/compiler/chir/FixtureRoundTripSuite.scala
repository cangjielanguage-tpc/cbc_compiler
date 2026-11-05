/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.chir.model.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

/** Round-trip of the synthetic fixture through the materializer/serializer of
  * EVERY supported schema version (same invariants as the real-file RoundTrip
  * harness, plus a strict compareIds-true second pass, which holds for
  * materializer-assigned ids). Versions are not hardcoded — the suite iterates
  * over `CHIR.supportedVersions`.
  */
class FixtureRoundTripSuite extends AnyFunSuite with Matchers {

  /** Collects every expression of the fixture package (function bodies and
    * global var initializers; the fixture keeps lambdas inside those bodies).
    */
  private def allExprs(p: Package): ArrayBuffer[Expression] = {
    val out = ArrayBuffer[Expression]()
    def bg(bg: BlockGroup): Unit = for (b <- bg.blocks) for (e <- b.exprs) { out += e }
    def func(f: Func): Unit = f.body.foreach(bg)
    for (f <- p.globalFuncs) func(f)
    for (d <- p.allCustomTypeDefs) {
      for (m <- d.methods) func(m)
      d.instanceVarInitFunc.foreach(func)
    }
    out
  }

  for (version <- CHIR.supportedVersions) {
    test(s"v$version fixture round-trips structurally (ids may regroup on first pass)") {
      val fixture = Fixture.build()
      val bytes2 = CHIR.serializePackage(fixture, version, fixture.phase)
      val m2 = CHIR.loadPackage(bytes2, version)
      ModelCompare.compare(fixture, m2, compareIds = false) shouldBe None
    }

    test(s"v$version re-serialization is byte-stable and ids become exact") {
      val fixture = Fixture.build()
      val bytes2 = CHIR.serializePackage(fixture, version, fixture.phase)
      val m2 = CHIR.loadPackage(bytes2, version)
      val bytes3 = CHIR.serializePackage(m2, version, m2.phase)
      bytes2 shouldBe bytes3
      val m3 = CHIR.loadPackage(bytes3, version)
      ModelCompare.compare(m2, m3, compareIds = true) shouldBe None
    }
  }

  // The MATCH_CASE value of the SourceExpr enum and the MatchCaseId annotation
  // are not representable in every schema (in v1203/v13002 the MultiBranch table
  // has no sourceExpr field at all and the Annotation union has no matchCaseId;
  // their serializers reject MatchCaseId), so this invariant cannot be part of
  // the version-agnostic Fixture — the test iterates over the supported versions
  // and skips the ones that cannot represent it.
  for (version <- CHIR.supportedVersions if version != 1203 && version != 13002) {
    test(s"v$version MultiBranch.sourceExpr=MatchCase and MatchCaseId survive the round-trip") {
      val p = Fixture.build()
      val mbranch = allExprs(p).collect { case m: MultiBranchExpr => m }.head
      mbranch.sourceExpr = SourceExprTag.MatchCase
      mbranch.annos += MatchCaseId(42L)
      val bytes2 = CHIR.serializePackage(p, version, p.phase)
      val m2 = CHIR.loadPackage(bytes2, version)
      val mbranch2 = allExprs(m2).collect { case m: MultiBranchExpr => m }.head
      mbranch2.sourceExpr shouldBe SourceExprTag.MatchCase
      mbranch2.annos.collectFirst { case m: MatchCaseId => m.id } shouldBe Some(42L)
    }
  }
}
