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
      val bytes2 = CHIR.packageBytes(fixture, version, fixture.phase)
      val m2 = CHIR.loadPackage(bytes2, version)
      ModelCompare.compare(fixture, m2, compareIds = false) === None shouldBe true
    }

    test(s"v$version re-serialization is byte-stable and ids become exact") {
      val fixture = Fixture.build()
      val bytes2 = CHIR.packageBytes(fixture, version, fixture.phase)
      val m2 = CHIR.loadPackage(bytes2, version)
      val bytes3 = CHIR.packageBytes(m2, version, m2.phase)
      java.util.Arrays.equals(bytes2, bytes3) shouldBe true
      val m3 = CHIR.loadPackage(bytes3, version)
      ModelCompare.compare(m2, m3, compareIds = true) === None shouldBe true
    }
  }

  // The MatchCaseId annotation and the non-default MultiBranch.sourceExpr exist
  // only in the newest schema, so the delta fixture is exercised on it only.
  for (version <- Seq(CHIR.supportedVersions.max)) {
    test(s"v$version MultiBranch.sourceExpr and MatchCaseId survive the round-trip") {
      val p = Fixture.build()
      val exprs = allExprs(p)
      exprs.collectFirst { case m: MultiBranchExpr => m } match {
        case Some(m) => m.sourceExpr = SourceExprTag.MatchCase
        case None    => fail("fixture has no MultiBranch to exercise sourceExpr")
      }
      // MatchCaseId is a newest-schema-only annotation union element: attach one
      // to the MultiBranch and check it survives the round-trip with its payload.
      exprs.collectFirst { case m: MultiBranchExpr => m } match {
        case Some(m) => m.annos += MatchCaseId(42L)
        case None    => fail("fixture has no MultiBranch to carry the MatchCaseId anno")
      }
      val bytes2 = CHIR.packageBytes(p, version, p.phase)
      val m2 = CHIR.loadPackage(bytes2, version)
      val mbranch = allExprs(m2).collectFirst { case m: MultiBranchExpr => m }
      mbranch.isDefined shouldBe true
      mbranch.get.sourceExpr === SourceExprTag.MatchCase shouldBe true
      mbranch.get.annos.collectFirst { case m: MatchCaseId => m.id } === Some(42L) shouldBe true
    }
  }
}
