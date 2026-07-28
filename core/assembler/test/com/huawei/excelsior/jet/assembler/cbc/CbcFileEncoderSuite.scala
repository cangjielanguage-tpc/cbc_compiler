/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{BuiltinSignature, Functional}
import org.scalatest.funsuite.AnyFunSuite
import xscala.io.ByteBuffer

class CbcFileEncoderSuite extends AnyFunSuite {

  test("Single type") {
    val builder = CbcFileFormat.newBuilder()
    val typeBuilder = builder.newTypeBuilder()
    typeBuilder.setName("default")

    val methodBuilder = typeBuilder.newMethodBuilder()
    methodBuilder.setName("foo")
    methodBuilder.setSignature(Functional(Seq(BuiltinSignature.I32), BuiltinSignature.I64))

    val buffer = ByteBuffer()
    CbcFileEncoder(builder.build()).generate(buffer)
    buffer.toByteArray
  }
}
