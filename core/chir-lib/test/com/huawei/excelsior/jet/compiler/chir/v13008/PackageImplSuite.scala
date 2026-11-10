package com.huawei.excelsior.jet.compiler.chir.v13008

import com.huawei.excelsior.jet.compiler.chir.CHIR
import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite

class PackageImplSuite extends AnyFunSuite {
  test("new blocks, branches and groups can replace a function body and survive saving") {
    val p = PackageImpl.fromBytes(TestData.operationFixtureData().serializeToBinary())
    val entry = p.newBlock()
    val next = p.newBlock()
    entry.replaceTerminator(p.newGoto(next))
    next.replaceTerminator(p.newGoto(next))
    val body = p.newBlockGroup(Seq(entry, next), entry)
    val function = p.function(1)
    function.removeBody()
    function.replaceBody(Some(body), None)
    val condition = p.insert(entry, CHIR.Operation.BooleanConstant(true))
    entry.replaceTerminator(p.newBranch(condition, entry, next))
    body.replaceBlocks(Seq(next, entry), entry)
    function.replaceAttributes(Seq(CHIR.Attribute.Public))
    assert(p.validationIssues().isEmpty)
    val output = Files.createTempFile("chir-v13008-body-", ".chir")
    try {
      p.write(output)
      val saved = PackageImpl.read(output)
      val f = saved.function(1)
      assert(f.attributes == Seq(CHIR.Attribute.Public))
      val group = f.body.get
      val branch = group.entryBlock.terminator.asInstanceOf[CHIR.Branch]
      assert(branch.trueBlock eq group.entryBlock)
      assert(branch.falseBlock eq group.blocks.head)
      assert(branch.condition.asInstanceOf[CHIR.LocalVar].associatedExpr
        .asInstanceOf[CHIR.Constant].literal.asInstanceOf[CHIR.BoolLiteral].value)
    } finally Files.delete(output)
  }
}
