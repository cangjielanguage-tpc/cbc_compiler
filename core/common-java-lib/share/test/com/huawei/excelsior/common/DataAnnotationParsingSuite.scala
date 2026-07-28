/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import org.scalatest.funsuite.AnyFunSuite
import DataAnnotationParsing.*

class DataAnnotationParsingSuite extends AnyFunSuite {

  test("Set of integers @Data") {
    val reference = Seq(Integer(1, 1), Integer(2, 2), Integer(4, 3), Integer(8, 4))
    val dataStr = "B01S0002I00000003L0000000000000004"
    val data = DataAnnotationParsing.parse(dataStr)
    
    assert(reference.length == data.length)
    data.zip(reference).foreach((d, r) => assert(d == r))
  }

  test("Field reference @Data") {
    val reference = Seq(FieldRef("com.aaa.bbbbb.dddd.cccc.Foo.field"), FieldRef("com.aaa.bbbbb.dddd.cccc.Foo.field2"))
    val dataStr = "Acom.aaa.bbbbb.dddd.cccc.Foo.field;Acom.aaa.bbbbb.dddd.cccc.Foo.field2;"
    val data = DataAnnotationParsing.parse(dataStr)

    assert(reference.length == data.length)
    data.zip(reference).foreach((d, r) => assert(d == r))
  }

  test("Mixed @Data") {
    val reference = Seq(Integer(1, 1), FieldRef("com.aaa.bbbbb.dddd.cccc.Foo.field"), Integer(8, 2))
    val dataStr = "B01Acom.aaa.bbbbb.dddd.cccc.Foo.field;L0000000000000002"
    val data = DataAnnotationParsing.parse(dataStr)

    assert(reference.length == data.length)
    data.zip(reference).foreach((d, r) => assert(d == r))
  }

  test("Hex parsing") {
    val reference = Seq(Integer(8, 0x123456789ABCDEF0L), Integer(8, 0x123456789ABCDEF0L))
    val dataStr = "L123456789ABCDEF0L123456789abcdef0"
    val data = DataAnnotationParsing.parse(dataStr)

    assert(reference.length == data.length)
    data.zip(reference).foreach((d, r) => assert(d == r))
  }
}
