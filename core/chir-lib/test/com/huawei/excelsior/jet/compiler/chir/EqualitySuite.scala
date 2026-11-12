package com.huawei.excelsior.jet.compiler.chir

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EqualitySuite extends AnyFunSuite with Matchers {
  test("equivalent functions from different schemas work as map keys") {
    val functions = Seq(1203, 13002, 13008).map { version =>
      EditableCHIR.empty("fixture", version).newFunction("@f", Seq(CHIR.BuiltinType.Unit), CHIR.BuiltinType.Unit)
    }
    for (a <- functions; b <- functions) {
      a shouldBe b
      b shouldBe a
      a.hashCode shouldBe b.hashCode
      Map[CHIR.FuncSig, String](a -> "found").get(b) shouldBe Some("found")
    }
    functions.last.replaceAttributes(Seq(CHIR.Attribute.Public))
    functions.head should not equal functions.last
    Set[CHIR.FuncSig](functions.head, functions.last).size shouldBe 2
  }

  test("identity equality does not evaluate function type metadata") {
    val t = new CHIR.FuncType {
      def paramTypes: Seq[CHIR.Type] = fail("identity equality must not read parameters")
      def paramTypesWithoutReceiver: Seq[CHIR.Type] = fail("unexpected query")
      def receiverType: CHIR.Type = fail("unexpected query")
      def returnType: CHIR.Type = fail("identity equality must not read return type")
      def isC: Boolean = fail("unexpected query")
      def hasVarArg: Boolean = fail("unexpected query")
    }
    t.equals(t) shouldBe true
    t.equals(null) shouldBe false
  }
}
