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

  test("nominal definition equality compares shared metadata across schemas") {
    val fixtures = TestData.versions.map(TestData.readFixture)
    val definitions = fixtures.map(_.definition[CHIR.ClassDef](1))
    for (a <- definitions; b <- definitions) {
      a shouldEqual b
      a.hashCode shouldBe b.hashCode
    }
    fixtures.last.pkg.edit(definitions.last).replaceAttributes(Seq(CHIR.Attribute.Public))
    definitions.head should not equal definitions.last
    definitions.last should not equal definitions.head
  }

  test("different definition kinds may share hashes without comparing equal") {
    val data = v13008.CHIRFormat.CHIRPackageT.deserializeFromBinary(TestData.readFixture(13008).rawBytes)
    val cls = data.getDefs()(0).asClassDef().getBase
    val struct = data.getDefs()(1).asStructDef().getBase
    struct.setPackageName(cls.getPackageName)
    struct.setIdentifier(cls.getIdentifier)
    struct.setSrcCodeIdentifier(cls.getSrcCodeIdentifier)
    struct.getBase.setAttributes(cls.getBase.getAttributes)
    for (version <- TestData.versions) {
      val definitions = CHIR.fromBytes(data.serializeToBinary(), version).typeDefs.toVector
      definitions(0).hashCode shouldBe definitions(1).hashCode
      definitions(0) should not equal definitions(1)
      definitions(1) should not equal definitions(0)
    }
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
    t shouldEqual t
    t should not equal null
  }
}
