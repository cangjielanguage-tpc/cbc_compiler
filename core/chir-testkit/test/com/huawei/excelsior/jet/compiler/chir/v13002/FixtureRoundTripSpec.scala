/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.model.{Fixture, ModelCompare}
import org.scalatest.funsuite.AnyFunSuite

/** Round-trip of the synthetic fixture through the v13002 materializer/serializer
  * (same invariants as the real-file RoundTrip harness, plus a strict
  * compareIds-true second pass, which holds for materializer-assigned ids).
  */
class FixtureRoundTripSpec extends AnyFunSuite {

  private val fixture = Fixture.build()

  test("v13002 fixture round-trips structurally (ids may regroup on first pass)") {
    val bytes2 = Serializer13002.toBytes(fixture, fixture.phase)
    val m2 = Materializer13002.load(bytes2)
    assert(ModelCompare.compare(fixture, m2, compareIds = false) === None)
  }

  test("v13002 re-serialization is byte-stable and ids become exact") {
    val bytes2 = Serializer13002.toBytes(fixture, fixture.phase)
    val m2 = Materializer13002.load(bytes2)
    val bytes3 = Serializer13002.toBytes(m2, m2.phase)
    assert(java.util.Arrays.equals(bytes2, bytes3))
    val m3 = Materializer13002.load(bytes3)
    assert(ModelCompare.compare(m2, m3, compareIds = true) === None)
  }
}
