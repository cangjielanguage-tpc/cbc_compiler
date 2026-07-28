/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.classfile

import com.huawei.excelsior.jet.classfile.TestSignatureParser.*
import com.huawei.excelsior.jet.common.XString
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe

/** Combo test for [[SignatureParser]] and
  * for [[SignatureTraverser.fromString]].
  */
object TestSignatureParser {

  private val GOOD_SIGNATURES = Array[Case](
    // Field signatures:
    Case("I", Entry("I")),
    Case("B", Entry("B")),
    Case("Z", Entry("Z")),
    Case("LFoo;", Entry("Foo")),
    Case("[[[I", Entry("I", 3)),
    Case("[LFoo;", Entry("Foo", 1)),
    Case("[[[Lfoo/bar/Baz;", Entry("foo/bar/Baz", 3)),

    // Method signatures:
    Case("()V", Entry("V")),
    Case("([C)D", Entry("C", 1), Entry("D")),
    Case("([[FJ)[S", Entry("F", 2), Entry("J"), Entry("S", 1)),
    Case("(LFoo;)V", Entry("Foo"), Entry("V")),
    Case("(ILFoo;JJLFoo;Z)Z", Entry("I"), Entry("Foo"), Entry("J"), Entry("J"), Entry("Foo"), Entry("Z"), Entry("Z")),
    Case("(Lfoo/bar/Baz;J[[Lbar/baz/Foo;)V", Entry("foo/bar/Baz"), Entry("J"), Entry("bar/baz/Foo", 2), Entry("V")),

    // These signatures are malformed but still can be parsed:
    Case("II", Entry("I"), Entry("I")),
    Case("(I", Entry("I")),
    Case(")I", Entry("I")),
    Case("(I()J", Entry("I"), Entry("J")),
    Case("(())V", Entry("V")),
    Case("(I))II", Entry("I"), Entry("I"), Entry("I")),
    Case("(([V)V", Entry("V", 1), Entry("V")),
    Case("(V)V)V", Entry("V"), Entry("V"), Entry("V")),
    Case("(L(;)L);", Entry("("), Entry(")")),
    Case("(LFoo.Bar//Baz[X];)V", Entry("Foo.Bar//Baz[X]"), Entry("V")))

  private val BAD_SIGNATURES = Seq(
    "",
    "K",
    "[K",
    "[[[",
    "LFoo",
    "LFoo;;",
    "L",
    ";",
    "I;I",
    "L;",
    "foo/bar/Baz",
    "foo/bar/Baz;",
    "[[[Lfoo/bar/Baz",
    "Ljava/lang/HashMap<TK;TV;>;",
    "()",
    "(I)",
    "I()",
    "[(I)J",
    "(I[)J",
    "([[(IJ)[Z)",
    "([K)LMN"
  )

  case class Case(sig: String, entries: Entry*)
  case class Entry(name: String, arrayDim: Int = 0)
}

class TestSignatureParser extends AnyFunSuite {

  private def getParser(sig: String) = new SignatureParser[Entry](XString.ascii(sig)) {
    override def parsePrimitive(arrayDim: Int, sigChar: Byte): Entry = Entry(Character.toString(sigChar.toChar), arrayDim)
    override def parseClass(arrayDim: Int, name: XString): Entry = Entry(name.toString, arrayDim)
  }

  test("testGoodSignatures") {
    for (c <- GOOD_SIGNATURES) {
      val parsed = getParser(c.sig).toList
      parsed shouldBe c.entries
    }
  }

  test("BadSignatures") {
    for (s <- BAD_SIGNATURES) {
      try {
        getParser(s).toList
        fail("not bad")
      } catch {
        case ignored: IllegalArgumentException =>
        // correct
      }
    }
  }

  test ("asMethodSig") {
    val x = Case("([[FJ)[S", Entry("F", 2), Entry("J"), Entry("S", 1))
    val (params, ret) = getParser(x.sig).asMethodSig
    params shouldBe List(Entry("F", 2), Entry("J"))
    ret shouldBe Entry("S", 1)
  }
}
