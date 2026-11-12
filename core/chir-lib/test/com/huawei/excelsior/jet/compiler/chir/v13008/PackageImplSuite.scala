package com.huawei.excelsior.jet.compiler.chir.v13008

import com.huawei.excelsior.jet.compiler.chir.EditableCHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR
import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite

class PackageImplSuite extends AnyFunSuite {
  test("patch methods can share source generic types without redeclaring them") {
    val data = TestData.fixtureData()
    val base = new CHIRFormat.TypeT
    base.setKind(CHIRFormat.CHIRTypeKind.GENERIC)
    base.setArgTys(Array.emptyLongArray)
    val generic = new CHIRFormat.GenericTypeT
    generic.setBase(base)
    generic.setIdentifier("T")
    generic.setUpperBounds(Array.emptyLongArray)
    val entry = new CHIRFormat.TypeElemUnion
    entry.setType(CHIRFormat.TypeElem.GenericType)
    entry.setValue(generic)
    data.getTypes()(1) = entry
    data.getValues()(0).asFunction().setGenericTypeParams(Array(2L))
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val from = p.function(1)
    val to = p.function(8)
    assert(from.genericTypeParams.nonEmpty && to.genericTypeParams.isEmpty)
    p.edit(from).moveBody(to)
    assert(from.body.isEmpty && to.body.nonEmpty)
    assert(to.params.head.tpe == from.genericTypeParams.head)
    val output = Files.createTempFile("chir-v13008-shared-generic-", ".chir")
    try {
      p.write(output)
      val saved = PackageImpl.fromBytes(Files.readAllBytes(output))
      assert(saved.function(8).body.nonEmpty)
      assert(saved.function(8).genericTypeParams.isEmpty)
      assert(saved.function(8).params.head.tpe == saved.function(1).genericTypeParams.head)
    } finally Files.delete(output)
  }

  test("new blocks, branches and groups can replace a function body and survive saving") {
    val p = PackageImpl.fromBytes(TestData.operationFixtureData().serializeToBinary())
    val entry = p.newBlock()
    val next = p.newBlock()
    p.edit(entry).replaceTerminator(p.newGoto(next))
    p.edit(next).replaceTerminator(p.newGoto(next))
    val body = p.newBlockGroup(Seq(entry, next), entry)
    val function = p.function(1)
    p.edit(function).removeBody()
    p.edit(function).replaceBody(Some(body), None)
    val condition = p.edit(entry).insert(EditableCHIR.Operation.BooleanConstant(true))
    p.edit(entry).replaceTerminator(p.newBranch(condition, entry, next))
    p.edit(body).replaceBlocks(Seq(next, entry), entry)
    p.edit(function).replaceAttributes(Seq(CHIR.Attribute.Public))
    assert(p.validationIssues().isEmpty)
    val output = Files.createTempFile("chir-v13008-body-", ".chir")
    try {
      p.write(output)
      val saved = PackageImpl.fromBytes(Files.readAllBytes(output))
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
