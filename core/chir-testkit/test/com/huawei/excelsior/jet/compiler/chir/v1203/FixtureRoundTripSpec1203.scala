/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.model.{Fixture, ModelCompare}
import org.scalatest.funsuite.AnyFunSuite

/** Round-trip of the synthetic fixture through the v1203 materializer/serializer.
  * Same invariants as the v13002 fixture spec. There is no v1203 .chir corpus
  * (see docs/hotfix-port-notes.md), so this self-round-trip is the only
  * verification for the v1203 layer.
  */
class FixtureRoundTripSpec1203 extends AnyFunSuite {

  private val fixture = Fixture.build()

  test("v1203 fixture round-trips structurally (ids may regroup on first pass)") {
    val bytes2 = Serializer1203.toBytes(fixture, fixture.phase)
    val m2 = Materializer1203.load(bytes2)
    assert(ModelCompare.compare(fixture, m2, compareIds = false) === None)
  }

  test("v1203 re-serialization is byte-stable and ids become exact") {
    val bytes2 = Serializer1203.toBytes(fixture, fixture.phase)
    val m2 = Materializer1203.load(bytes2)
    val bytes3 = Serializer1203.toBytes(m2, m2.phase)
    assert(java.util.Arrays.equals(bytes2, bytes3))
    val m3 = Materializer1203.load(bytes3)
    assert(ModelCompare.compare(m2, m3, compareIds = true) === None)
  }
}
