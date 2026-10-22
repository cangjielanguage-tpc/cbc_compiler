package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.{CHIR, CHIRThrowPatchGenerator, PatchedPackage, ThrowPatch, dsl}
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
    val generator = new CHIRThrowPatchGenerator(pkg, ThrowPatch.symbolResolutionErrorPatch)
    val function = generator.generatePatch(100).get
    // The fixture has no runtime Exception definition: these checks must fail
    // directly, before trying to generate the synthetic function's body.
    assert(function.params.isEmpty)
    assert(intercept[AssertionError](function.ownerPackage).getMessage.contains("Synthetic throw helpers"))
    assert(intercept[AssertionError](function.replaceBody(None, None)).getMessage.contains("synthetic throw helper"))
    assert(intercept[AssertionError](function.moveBody(pkg.function(1))).getMessage.contains("synthetic throw helper"))
    assert(intercept[AssertionError](function.replaceAttributes(Seq(CHIR.Attribute.Public))).getMessage.contains("synthetic throw helper"))
    assert(intercept[AssertionError](function.removeBody()).getMessage.contains("synthetic throw helper"))
    val overlay = new PatchedPackage(pkg, Seq(generator))
    val output = Files.createTempFile("synthetic-chir-", ".chir")
    val sentinel = Array[Byte](1, 2, 3)
    try {
      Files.write(output, sentinel)
      assert(intercept[AssertionError](overlay.write(output)).getMessage.contains("Compiler-only synthetic patches"))
      assert(Files.readAllBytes(output).sameElements(sentinel))
    } finally Files.delete(output)
  }
}
