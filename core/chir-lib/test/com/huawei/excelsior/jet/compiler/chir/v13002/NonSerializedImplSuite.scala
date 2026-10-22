package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.{CHIR, AugmentedPackage, dsl}
import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite

class NonSerializedImplSuite extends AnyFunSuite {
  test("DSL nodes keep their local editing API but cannot supply serialized ownership") {
    val block = new dsl.Block
    val group = new dsl.BlockGroup(Seq(block), block)
    val exit = new CHIR.Exit {
      def debugLoc: Option[CHIR.DebugLocation] = None
    }
    block.replaceExpressions(Seq.empty, exit)
    group.replaceBlocks(Seq(block), block)
    assert(block.terminator eq exit)
    assert(group.entryBlock eq block)
    assert(intercept[AssertionError](block.ownerPackage).getMessage.contains("DSL blocks"))
    assert(intercept[AssertionError](group.ownerPackage).getMessage.contains("DSL block groups"))
  }

  test("synthetic function editing and serialization fail without evaluating or writing the body") {
    val pkg = PackageImpl.fromBytes(TestData.fixtureData().serializeToBinary())
    val function = CHIR.newIntrinsicsPackage.values.collectFirst { case f: CHIR.Func => f }.get
    // The fixture has no runtime Exception definition: these checks must fail
    // directly, before trying to generate the synthetic function's body.
    assert(function.params.isEmpty)
    assert(intercept[AssertionError](function.ownerPackage).getMessage.contains("does not support serialized edits"))
    assert(intercept[AssertionError](function.replaceBody(None, None)).getMessage.contains("does not support serialized edits"))
    assert(intercept[AssertionError](function.moveBody(pkg.function(1))).getMessage.contains("does not support serialized edits"))
    assert(intercept[AssertionError](function.replaceAttributes(Seq(CHIR.Attribute.Public))).getMessage.contains("does not support serialized edits"))
    assert(intercept[AssertionError](function.removeBody()).getMessage.contains("does not support serialized edits"))
    val overlay = new AugmentedPackage(pkg, Seq(function))
    val output = Files.createTempFile("synthetic-chir-", ".chir")
    val sentinel = Array[Byte](1, 2, 3)
    try {
      Files.write(output, sentinel)
      assert(intercept[AssertionError](overlay.write(output)).getMessage.contains("does not support serialized edits"))
      assert(Files.readAllBytes(output).sameElements(sentinel))
    } finally Files.delete(output)
  }
}
