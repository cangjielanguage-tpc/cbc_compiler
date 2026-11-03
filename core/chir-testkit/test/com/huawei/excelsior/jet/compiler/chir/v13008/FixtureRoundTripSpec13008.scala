/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v13008

import com.huawei.excelsior.jet.compiler.chir.model.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

/** Round-trip of the synthetic fixture through the v13008 materializer/serializer
  * (same invariants as the v13002/v1203 fixture specs). There is no v13008 .chir
  * corpus yet, so this self-round-trip is the only verification for the v13008
  * layer.
  *
  * The shared fixture is additionally exercised on the v13008 schema deltas:
  * the MultiBranch gets a non-default sourceExpr, and a MatchCaseId annotation
  * (v13008-only) is added to one expression.
  */
class FixtureRoundTripSpec13008 extends AnyFunSuite {

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

  private def fixtureWith13008Deltas(): Package = {
    val p = Fixture.build()
    val exprs = allExprs(p)
    exprs.collectFirst { case m: MultiBranchExpr => m } match {
      case Some(m) => m.sourceExpr = SourceExprTag.MatchCase
      case None => fail("fixture has no MultiBranch to exercise sourceExpr")
    }
    // MatchCaseId is a v13008-only annotation union element: attach one to the
    // MultiBranch and check it survives the round-trip with its payload.
    exprs.collectFirst { case m: MultiBranchExpr => m } match {
      case Some(m) => m.annos += MatchCaseId(42L)
      case None => fail("fixture has no MultiBranch to carry the MatchCaseId anno")
    }
    p
  }

  private val fixture = fixtureWith13008Deltas()

  test("v13008 fixture round-trips structurally (ids may regroup on first pass)") {
    val bytes2 = Serializer13008.toBytes(fixture, fixture.phase)
    val m2 = Materializer13008.load(bytes2)
    assert(ModelCompare.compare(fixture, m2, compareIds = false) === None)
  }

  test("v13008 re-serialization is byte-stable and ids become exact") {
    val bytes2 = Serializer13008.toBytes(fixture, fixture.phase)
    val m2 = Materializer13008.load(bytes2)
    val bytes3 = Serializer13008.toBytes(m2, m2.phase)
    assert(java.util.Arrays.equals(bytes2, bytes3))
    val m3 = Materializer13008.load(bytes3)
    assert(ModelCompare.compare(m2, m3, compareIds = true) === None)
  }

  test("v13008 MultiBranch.sourceExpr and MatchCaseId survive the round-trip") {
    val bytes2 = Serializer13008.toBytes(fixture, fixture.phase)
    val m2 = Materializer13008.load(bytes2)
    val mbranch = allExprs(m2).collectFirst { case m: MultiBranchExpr => m }
    assert(mbranch.isDefined)
    assert(mbranch.get.sourceExpr === SourceExprTag.MatchCase)
    assert(mbranch.get.annos.collectFirst { case m: MatchCaseId => m.id } === Some(42L))
  }
}
