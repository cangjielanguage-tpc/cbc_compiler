package com.huawei.excelsior.jet.compiler.chir

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

private[chir] case class FunctionMetadata(body: Long, result: Long, signature: Long, params: Seq[Long],
                                         localCounter: Long, blockCounter: Long, groupCounter: Long,
                                         fastNative: Boolean, wrapper: Boolean, mangledName: String)

private[chir] enum BodyFault {
  case SharedBody, WrongEntry, WrongExpressionOwner, WrongResultDefinition, ForeignParameter
}

/** Only generated-table access differs between the schema adapters. */
private[chir] trait BodyFixture {
  def pkg: EditableCHIR.Package
  def rawBytes: Array[Byte]
  def metadata: FunctionMetadata
  def groupOwner(id: Long): Long
  def isReturn(id: Long): Boolean
  def corrupt(fault: BodyFault): Unit
  final def source: EditableCHIR.Func = pkg.function(1)
  final def group(id: Int): CHIR.BlockGroup = pkg.values.toVector(id - 1).asInstanceOf[CHIR.BlockGroup]
  final def local(id: Int): CHIR.LocalVar = pkg.values.toVector(id - 1).asInstanceOf[CHIR.LocalVar]
}

class BodyEditingSuite extends AnyFunSuite with Matchers {
  private val backends = Seq(
    1203 -> (() => v1203.TestData.bodyFixture()),
    13002 -> (() => v13002.TestData.bodyFixture()),
    13008 -> (() => v13008.TestData.bodyFixture()))

  for ((version, fresh) <- backends) {
    test(s"$version body replacement updates held views and ownership") {
      val f = fresh()
      val held: CHIR.Func = f.source
      val signature = held.tpe
      val params = held.params
      f.source.replaceBody(Some(f.group(5)), Some(f.local(7)))
      (held.body.get eq f.group(5)) shouldBe true
      (held.retVal.get eq f.local(7)) shouldBe true
      (held.tpe eq signature) shouldBe true
      held.params shouldBe params
      f.groupOwner(2) shouldBe 0L
      f.groupOwner(5) shouldBe 1L
      f.isReturn(4) shouldBe false
      f.isReturn(7) shouldBe true
      f.metadata.localCounter shouldBe 21L
      f.metadata.blockCounter shouldBe 12L
      f.metadata.groupCounter shouldBe 8L
    }

    test(s"$version replacement round trip preserves signature and non-CHIR metadata") {
      val f = fresh()
      val before = f.metadata
      f.source.replaceBody(Some(f.group(5)), Some(f.local(7)))
      val saved = EditableCHIR.fromBytes(f.pkg.toBytes, version).function(1)
      saved.body.get.blocks.size shouldBe 1
      saved.retVal.get.identifier shouldBe "%20"
      saved.tpe shouldBe f.source.tpe
      f.metadata.signature shouldBe before.signature
      f.metadata.params shouldBe before.params
      f.metadata.fastNative shouldBe true
      f.metadata.wrapper shouldBe true
      f.metadata.mangledName shouldBe "preserve-this-metadata"
    }

    test(s"$version removal detaches metadata and is idempotent") {
      val f = fresh()
      f.source.removeBody()
      f.source.body shouldBe None
      f.source.retVal shouldBe None
      f.groupOwner(2) shouldBe 0L
      f.isReturn(4) shouldBe false
      val before = f.rawBytes
      f.source.removeBody()
      f.rawBytes shouldBe before
      val saved = EditableCHIR.fromBytes(f.pkg.toBytes, version).function(1)
      saved.body shouldBe None
      saved.retVal shouldBe None
    }

    test(s"$version replacing a body with itself is idempotent") {
      val f = fresh()
      val before = f.rawBytes
      f.source.replaceBody(f.source.body, f.source.retVal)
      f.rawBytes shouldBe before
    }

    test(s"$version rejects another function's body atomically") {
      val f = fresh()
      val before = f.rawBytes
      intercept[IllegalArgumentException](f.source.replaceBody(Some(f.group(9)), None))
      f.rawBytes shouldBe before
    }

    test(s"$version rejects a return local outside the replacement body") {
      val f = fresh()
      val before = f.rawBytes
      intercept[IllegalArgumentException](f.source.replaceBody(Some(f.group(5)), Some(f.local(4))))
      f.rawBytes shouldBe before
    }

    test(s"$version rejects a declaration with a return local") {
      val f = fresh()
      val before = f.rawBytes
      intercept[IllegalArgumentException](f.source.replaceBody(None, Some(f.local(4))))
      f.rawBytes shouldBe before
    }

    for (fault <- BodyFault.values) {
      test(s"$version rejects $fault without partial mutation") {
        val f = fresh()
        f.corrupt(fault)
        val before = f.rawBytes
        intercept[IllegalArgumentException](f.source.replaceBody(Some(f.group(5)), Some(f.local(7))))
        f.rawBytes shouldBe before
      }
    }
  }
}
