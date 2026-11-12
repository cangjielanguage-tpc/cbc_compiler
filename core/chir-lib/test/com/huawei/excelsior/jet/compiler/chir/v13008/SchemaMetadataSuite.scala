package com.huawei.excelsior.jet.compiler.chir.v13008

import com.huawei.excelsior.jet.compiler.chir.EditableCHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v13008.CHIRFormat.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SchemaMetadataSuite extends AnyFunSuite with Matchers {
  for (phase <- Seq(Phase.RAW, Phase.PLUGIN, Phase.OPT)) {
    test(s"editing and saving preserves input phase $phase") {
      val data = TestData.fixtureData()
      data.setPhase(phase)
      val p = PackageImpl.fromBytes(data.serializeToBinary())
      p.function(1).moveBody(p.function(8))
      CHIRPackageT.deserializeFromBinary(p.toBytes).getPhase shouldBe phase
      CHIRPackageT.deserializeFromBinary(p.toBytes).getPhase shouldBe phase
    }
  }

  test("match-case annotations remain live and survive body transfer and serialization") {
    val data = TestData.fixtureData()
    val marker = new MatchCaseIdT
    // uint64 metadata is not an entity ID, including its high-bit values.
    marker.setId(-1L)
    val annotation = new AnnotationUnion
    annotation.setType(Annotation.matchCaseId)
    annotation.setValue(marker)
    data.getValues()(0).asFunction().getBase.getBase.getBase.setAnnos(Array(annotation))
    data.getExprs()(0).asAllocateBase().getBase.getBase.setAnnos(Array(annotation))
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val function = p.function(1)
    val held = function.annotations.collectFirst { case m: CHIR.MatchCaseId => m }.get
    held.id shouldBe -1L
    val raw = p.p.getValues()(0).asFunction().getBase.getBase.getBase.getAnnos()(0).getValue.asInstanceOf[MatchCaseIdT]
    raw.setId(42)
    held.id shouldBe 42
    p.edit(function).moveBody(p.function(8))
    (p.validationIssues().isEmpty) shouldBe true
    val output = p.toBytes
    val saved = CHIR.fromBytes(output, 13008)
    (saved.function(1).annotations.collectFirst { case m: CHIR.MatchCaseId => m.id }.contains(42L)) shouldBe true
    (saved.function(1).body.isEmpty && saved.function(8).body.nonEmpty) shouldBe true
    val image = PackageImpl.fromBytes(output)
    val moved = image.p.getExprs()(0).asAllocateBase().getBase.getBase.getAnnos()(0)
    moved.getValue.asInstanceOf[MatchCaseIdT].getId shouldBe -1L
  }

  for (source <- Seq(SourceExpr.MATCH_CASE, SourceExpr.OTHER)) {
    test(s"multi-branch source marker $source survives a rewrite and repeated save") {
      val data = TestData.fixtureData()
      val intType = new TypeT
      intType.setKind(CHIRTypeKind.INT64)
      intType.setArgTys(Array.emptyLongArray)
      val intEntry = new TypeElemUnion
      intEntry.setType(TypeElem.Type); intEntry.setValue(intType)
      data.setTypes(data.getTypes :+ intEntry)
      val value = PackageImpl.newValue("0", ValueKind.LITERAL)
      value.setType(6)
      val literal = new LiteralValueT
      literal.setBase(value); literal.setLiteralKind(ConstantValueKind.INT)
      val number = new IntLiteralT
      number.setBase(literal); number.setVal(0L)
      val valueEntry = new ValueElemUnion
      valueEntry.setType(ValueElem.IntLiteral); valueEntry.setValue(number)
      data.setValues(data.getValues :+ valueEntry)
      val base = data.getExprs()(1).asExpression()
      base.setKind(CHIRExprKind.MultiBranch)
      base.setOperands(Array(13L, 3L, 3L))
      val branch = new MultiBranchT
      branch.setBase(base)
      branch.setCaseValues(Array(0L))
      branch.setSourceExpr(source)
      data.getExprs()(1).setType(ExpressionElem.MultiBranch)
      data.getExprs()(1).setValue(branch)
      val p = PackageImpl.fromBytes(data.serializeToBinary())
      val block = p.function(1).body.get.entryBlock
      val held = block.terminator.asInstanceOf[CHIR.MultiBranch]
      p.edit(block).insert(EditableCHIR.Operation.BooleanConstant(true))
      (held.caseValues == Seq(0L) && held.defaultBlock == block) shouldBe true
      (p.validationIssues().isEmpty) shouldBe true
      val output = p.toBytes
      val saved = PackageImpl.fromBytes(output)
      saved.p.getExprs()(1).asMultiBranch().getSourceExpr shouldBe source
      val repeated = saved.toBytes
      PackageImpl.fromBytes(repeated).p.getExprs()(1).asMultiBranch().getSourceExpr shouldBe source
    }
  }
}
