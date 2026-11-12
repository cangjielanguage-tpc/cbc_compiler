package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.EditableCHIR
import com.google.flatbuffers.FlatBufferBuilder
import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.ByteBuffer

class PackageImplSuite extends AnyFunSuite with Matchers {
  private def fresh(): PackageImpl =
    PackageImpl.fromBytes(TestData.fixtureData().serializeToBinary())

  test("moveBody detaches the replaced destination body and return local") {
    val data = TestData.fixtureData()
    // Give the destination the fixture's spare body, which defines local 7.
    data.getValues()(7).asFunction().setBody(5)
    data.getValues()(7).asFunction().setRetVal(7)
    data.getValues()(4).asBlockGroup().setOwnedFunc(8)
    data.getValues()(8).asBlockGroup().setOwnedFunc(0)
    data.getValues()(6).asLocalVar().setIsRetVal(true)
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val source = p.function(1)
    val target = p.function(8)
    val movedBody = source.body.get
    val movedReturn = source.retVal.get
    val replacedBody = target.body.get
    val replacedReturn = target.retVal.get
    p.edit(source).moveBody(target)
    (source.body.isEmpty && source.retVal.isEmpty) shouldBe true
    (target.body.get eq movedBody) shouldBe true
    (target.retVal.get eq movedReturn) shouldBe true
    p.groupTable(p.valueId(replacedBody)).getOwnedFunc shouldBe 0
    (!p.valueTable(p.valueId(replacedReturn)).asLocalVar().getIsRetVal) shouldBe true
    p.groupTable(p.valueId(movedBody)).getOwnedFunc shouldBe target.id
    (p.valueTable(p.valueId(movedReturn)).asLocalVar().getIsRetVal) shouldBe true
    (p.validationIssues().isEmpty) shouldBe true
    roundTrip(p) { saved =>
      (saved.function(1).body.isEmpty) shouldBe true
      (saved.function(8).body.exists(saved.valueId(_) == p.valueId(movedBody))) shouldBe true
      (saved.function(8).retVal.exists(saved.valueId(_) == p.valueId(movedReturn))) shouldBe true
      (!saved.valueTable(p.valueId(replacedReturn)).asLocalVar().getIsRetVal) shouldBe true
    }
  }

  test("removeBody detaches the body and return local, preserves the declaration and is idempotent") {
    val p = fresh()
    val f = p.function(1)
    val body = f.body.get
    val result = f.retVal.get
    val signature = f.tpe
    val parameters = f.params
    val attributes = f.attributes
    val identifier = f.identifier
    p.edit(f).removeBody()
    (f.body.isEmpty && f.retVal.isEmpty) shouldBe true
    p.groupTable(p.valueId(body)).getOwnedFunc shouldBe 0
    (!p.valueTable(p.valueId(result)).asLocalVar().getIsRetVal) shouldBe true
    (f.tpe eq signature) shouldBe true
    (f.params == parameters && f.attributes == attributes && f.identifier == identifier) shouldBe true
    (p.function(8).body.nonEmpty) shouldBe true
    val after = p.p.serializeToBinary()
    p.edit(f).removeBody()
    (p.p.serializeToBinary().sameElements(after)) shouldBe true
    (p.validationIssues().isEmpty) shouldBe true
    roundTrip(p) { saved =>
      val declaration = saved.function(1)
      (declaration.body.isEmpty && declaration.retVal.isEmpty) shouldBe true
      (declaration.identifier == identifier && declaration.attributes == attributes) shouldBe true
      saved.valueId(declaration.params.head) shouldBe p.valueId(parameters.head)
      (saved.function(8).body.nonEmpty) shouldBe true
    }
  }

  test("loaded nodes have stable editable identities and registry IDs") {
    val p = fresh()
    val f = p.function(1)
    (f eq p.function(1)) shouldBe true
    (f eq p.getFunc("first").get) shouldBe true
    (f eq p.getFunc("@first").get) shouldBe true
    (f eq p.function(f.id.toInt)) shouldBe true
    val group = f.body.get
    val block = group.entryBlock
    (group eq p.function(1).body.get) shouldBe true
    (block eq group.entryBlock) shouldBe true
    (p.valueId(group) == 2 && p.valueId(block) == 3) shouldBe true
    p.valueId(f.params.head) shouldBe 12
    p.expressionId(block.terminator) shouldBe 2
    p.typeId(f.tpe) shouldBe 5
    (p.values.size == 12 && p.typeDefs.isEmpty) shouldBe true
    (p.getFunc("missing").isEmpty && p.getDef("missing").isEmpty) shouldBe true
    (p.validationIssues().isEmpty) shouldBe true
  }

  test("replaceTerminator updates held views and defers predecessor maintenance") {
    val p = fresh()
    val block = p.function(1).body.get.entryBlock
    val old = block.terminator
    val jump = p.newGoto(block, Some(CHIR.DebugLocation(10, 11)))
    val newBase = p.expressionTable(p.expressionId(jump)).asExpression().getBase
    (newBase.getAnnos != null && newBase.getLoc.getFilePath != null) shouldBe true
    p.edit(block).replaceTerminator(jump)
    (block.terminator eq jump) shouldBe true
    block.nonTerminatorExpressions.size shouldBe 1
    (jump.destination eq block) shouldBe true
    (jump.debugLoc.contains(CHIR.DebugLocation(10, 11))) shouldBe true
    p.expressionTable(p.expressionId(old)).asExpression().getOwner shouldBe 0
    (p.blockTable(3).getPredecessors.isEmpty) shouldBe true
    (p.validationIssues().isEmpty) shouldBe true // Explicit diagnostics refresh derived data too.
    (p.blockTable(3).getPredecessors.sameElements(Array(3L))) shouldBe true
    roundTrip(p) { saved =>
      val b = saved.function(1).body.get.entryBlock
      (b.terminator.asInstanceOf[CHIR.Goto].destination eq b) shouldBe true
      (b.terminator.debugLoc.contains(CHIR.DebugLocation(10, 11))) shouldBe true
    }
  }

  test("retired ownership is preserved on disk without reattaching nodes in memory") {
    val p = fresh()
    val b = p.function(1).body.get.entryBlock
    p.edit(b).replaceTerminator(p.newGoto(b))
    p.expressionTable(2).asExpression().getOwner shouldBe 0
    roundTrip(p) { saved =>
      saved.expressionTable(2).asExpression().getOwner shouldBe 3
      (!saved.blockTable(3).getExprs.contains(2L)) shouldBe true
      val oldExit = saved.getExpr[CHIR.Terminator](2)
      val spare = saved.newBlock()
      saved.edit(spare).replaceTerminator(oldExit) // historical owner is not an active attachment
      val group = saved.function(1).body.get
      saved.edit(group).replaceBlocks(group.blocks :+ spare, group.entryBlock)
      (saved.validationIssues().isEmpty) shouldBe true
    }
    p.expressionTable(2).asExpression().getOwner shouldBe 0
  }

  for (failPacking <- Seq(false, true)) {
    test(s"wire overrides are restored after packing: fail=$failPacking") {
      val p = fresh()
      val f = p.function(1)
      val group = f.body.get
      val block = group.entryBlock
      val jump = p.newGoto(block)
      p.edit(block).replaceTerminator(jump)
      val spare = p.newBlock()
      val spareJump = p.newGoto(spare)
      p.edit(spare).replaceTerminator(spareJump)
      p.edit(group).replaceBlocks(group.blocks :+ spare, block)
      p.edit(group).replaceBlocks(Seq(block), block)
      val lambda = p.p.getValues()(7).asFunction()
      lambda.setFuncKind(FuncKind.LAMBDA)
      p.edit(p.function(8)).removeBody()
      (p.validationIssues().isEmpty) shouldBe true
      val values = p.p.getValues
      val exprs = p.p.getExprs
      val spareTable = p.blockTable(p.valueId(spare))
      val instructions = spareTable.getExprs
      val predecessors = spareTable.getPredecessors
      val counter = values(0).asFunction().getBlockGroupId
      val before = p.p.serializeToBinary()
      var output = Array.emptyByteArray

      if (failPacking) {
        // Validation accepts the graph; the UTF-8 packer rejects this name
        // after all serialization-only overrides have been applied.
        p.p.setName("\ud800")
        try intercept[IllegalArgumentException](p.toBytes)
        finally p.p.setName("fixture")

      } else {
        output = p.toBytes
      }
      (p.p.getValues eq values) shouldBe true
      (p.p.getExprs eq exprs) shouldBe true
      p.expressionTable(2).asExpression().getOwner shouldBe 0
      p.groupTable(9).getOwnedFunc shouldBe 0
      spareTable.getParentGroup shouldBe 0
      (spareTable.getExprs eq instructions) shouldBe true
      (spareTable.getPredecessors eq predecessors) shouldBe true
      values(0).asFunction().getBlockGroupId shouldBe counter
      lambda.getFuncKind shouldBe FuncKind.LAMBDA
      (block.terminator eq jump) shouldBe true
      (spare.terminator eq spareJump) shouldBe true
      (p.p.serializeToBinary().sameElements(before)) shouldBe true
      output = p.toBytes
      val first = output
      val saved = PackageImpl.fromBytes(output)
      saved.expressionTable(2).asExpression().getOwner shouldBe p.valueId(block)
      (saved.blockTable(p.valueId(spare)).getParentGroup > values.length) shouldBe true
      (saved.blockTable(p.valueId(spare)).getExprs.isEmpty) shouldBe true
      saved.p.getValues()(7).asFunction().getFuncKind shouldBe FuncKind.DEFAULT
      (saved.p.getValues()(0).asFunction().getBlockGroupId > counter) shouldBe true
      output = p.toBytes
      (output.sameElements(first)) shouldBe true
      (p.p.getValues eq values) shouldBe true
      (p.p.serializeToBinary().sameElements(before)) shouldBe true
    }
  }

  test("direct packing emits empty vectors on inserted nodes") {
    val p = fresh()
    val group = p.function(1).body.get
    val block = group.entryBlock
    val condition = p.edit(block).insert(EditableCHIR.Operation.BooleanConstant(true))
    val expressionId = p.valueTable(p.valueId(condition)).asLocalVar().getAssociatedExpr
    val boolTypeId = p.typeId(condition.tpe)
    val second = p.newBlock()
    p.edit(second).replaceTerminator(p.newGoto(second))
    p.edit(block).replaceTerminator(p.newBranch(condition, block, second))
    p.edit(group).replaceBlocks(group.blocks :+ second, block)
    val output = p.toBytes
    val raw = CHIRPackage.getRootAsCHIRPackage(ByteBuffer.wrap(output))
    val t = raw.types(new Type, (boolTypeId - 1).toInt).asInstanceOf[Type]
    val e = raw.exprs(new Expression, (expressionId - 1).toInt).asInstanceOf[Expression]
    val b = raw.values(new Block, (p.valueId(second) - 1).toInt).asInstanceOf[Block]
    (t.argTysVector() != null) shouldBe true
    (e.blockGroupsVector() != null) shouldBe true
    (b.exceptionCatchListVector() != null) shouldBe true
    (PackageImpl.fromBytes(output).validationIssues().isEmpty) shouldBe true
  }

  test("attribute caching follows both API and backing-mask updates") {
    val p = fresh()
    val f = p.function(1)
    val original = f.attributes
    (f.attributes eq original) shouldBe true
    p.edit(f).replaceAttributes(Seq(CHIR.Attribute.Imported))
    val imported = f.attributes
    imported shouldBe Seq(CHIR.Attribute.Imported)
    (f.attributes eq imported) shouldBe true
    p.p.getValues()(0).asFunction().getBase.getBase.getBase.setAttributes(1L << CHIR.Attribute.Public.ordinal)
    f.attributes shouldBe Seq(CHIR.Attribute.Public)
    p.edit(f).replaceAttributes(Seq.empty)
    (f.attributes.isEmpty) shouldBe true
  }

  test("batched appends preserve IDs and held views across flushes and saving") {
    val p = fresh()
    val f = p.function(1)
    val block = f.body.get.entryBlock
    val initialValues = p.values.size
    val initialTypes = p.p.getTypes.length
    val first = p.edit(block).insert(EditableCHIR.Operation.BooleanConstant(true))
    val firstId = p.valueId(first)
    for (i <- 1 until 128) {
      p.edit(block).insert(EditableCHIR.Operation.BooleanConstant(i % 2 == 0))
    }
    p.values.size shouldBe initialValues + 256
    (p.getValue[CHIR.LocalVar](firstId).get eq first) shouldBe true
    p.p.getValues.length shouldBe initialValues + 256
    p.p.getTypes.length shouldBe initialTypes + 1
    p.edit(block).insert(EditableCHIR.Operation.BooleanConstant(false))
    (p.getValue[CHIR.LocalVar](firstId).get eq first) shouldBe true
    (p.function(1) eq f) shouldBe true
    roundTrip(p) { saved =>
      saved.values.size shouldBe initialValues + 258
      saved.getValue[CHIR.LocalVar](firstId).get.tpe shouldBe CHIR.BuiltinType.Boolean
      saved.function(1).body.get.entryBlock.nonTerminatorExpressions.size shouldBe 130
      saved.p.getTypes.length shouldBe initialTypes + 1
    }
  }

  test("inserted types use the first matching table ID regardless of view access order") {
    val data = TestData.fixtureData()
    def booleanType(): TypeElemUnion = {
      val t = new TypeT
      t.setKind(CHIRTypeKind.BOOLEAN)
      t.setArgTys(Array.emptyLongArray)
      val entry = new TypeElemUnion
      entry.setType(TypeElem.Type)
      entry.setValue(t)
      entry
    }
    data.setTypes(data.getTypes ++ Array(booleanType(), booleanType()))
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    (p.getType[CHIR.Type](7).contains(CHIR.BuiltinType.Boolean)) shouldBe true
    val value = p.edit(p.function(1).body.get.entryBlock).insert(EditableCHIR.Operation.BooleanConstant(true))
    p.valueTable(p.valueId(value)).asLocalVar().getBase.getType shouldBe 6
    p.p.getTypes.length shouldBe 7
  }

  test("new nodes may be staged but must be attached before writing") {
    val p = fresh()
    val block = p.newBlock()
    val jump = p.newGoto(block)
    (p.validationIssues().exists(_.message.contains("New expression has not been attached"))) shouldBe true
    (p.validationIssues().exists(_.message.contains("New block has not been attached"))) shouldBe true
    p.edit(block).replaceTerminator(jump)
    val group = p.newBlockGroup(Seq(block), block)
    (p.validationIssues().exists(_.message.contains("New group has not been attached"))) shouldBe true
    val error = intercept[IllegalArgumentException](p.toBytes)
    (error.getMessage.contains("New group has not been attached")) shouldBe true

    p.edit(p.function(1)).replaceBody(Some(group), None)
    (p.validationIssues().isEmpty) shouldBe true
    roundTrip(p)(saved => saved.function(1).body.get.blocks.size shouldBe 1)
  }

  test("newBranch, expression replacement and new block membership survive reload") {
    val p = fresh()
    val g = p.function(1).body.get
    val first = g.entryBlock
    val exit = first.terminator
    val second = p.newBlock()
    val condition = p.edit(first).insert(EditableCHIR.Operation.BooleanConstant(true))
    val branch = p.newBranch(condition, first, second)
    p.edit(first).replaceTerminator(branch)
    p.edit(second).replaceExpressions(Seq.empty, exit)
    p.edit(g).replaceBlocks(Seq(first, second), first)
    p.blockTable(p.valueId(second)).getParentGroup shouldBe p.valueId(g)
    (PackageImpl.ids(p.blockTable(p.valueId(second)).getPredecessors).isEmpty) shouldBe true
    (p.validationIssues().isEmpty) shouldBe true
    (p.blockTable(p.valueId(second)).getPredecessors.sameElements(Array(3L))) shouldBe true
    roundTrip(p) { saved =>
      val body = saved.function(1).body.get
      val b = body.entryBlock.terminator.asInstanceOf[CHIR.Branch]
      (b.trueBlock eq body.entryBlock) shouldBe true
      (b.falseBlock eq body.blocks(1)) shouldBe true
      b.condition.asInstanceOf[CHIR.LocalVar].associatedExpr.asInstanceOf[CHIR.Constant].literal.asInstanceOf[CHIR.BoolLiteral].value shouldBe true
    }
  }

  test("terminator replacement defers unreachable block pruning until write") {
    val p = fresh()
    val g = p.function(1).body.get
    val entry = g.entryBlock
    val unused = p.newBlock()
    val unusedId = p.valueId(unused)
    // Membership can be staged; no caller-triggered cleanup is necessary.
    p.edit(g).replaceBlocks(g.blocks :+ unused, entry)
    g.blocks.size shouldBe 2
    p.edit(entry).replaceTerminator(p.newGoto(entry))
    g.blocks.size shouldBe 2
    p.blockTable(unusedId).getParentGroup shouldBe p.valueId(g)
    roundTrip(p)(saved => saved.function(1).body.get.blocks.size shouldBe 1)
    g.blocks shouldBe Seq(entry)
    p.blockTable(unusedId).getParentGroup shouldBe 0
  }

  test("moveBody checks types used by the moved body before applying edits") {
    val data = genericMoveData()
    data.getExprs()(0).asAllocateBase().getBase.setResultTy(9999)
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val before = p.p.serializeToBinary()
    val error = intercept[IllegalArgumentException](p.edit(p.function(1)).moveBody(p.function(8)))
    (error.getMessage.contains("Invalid type ID")) shouldBe true
    (p.p.serializeToBinary().sameElements(before)) shouldBe true
  }

  test("moveBody defers unrelated metadata validation until writing") {
    val data = genericMoveData()
    val method = new WrappedRawMethodT
    method.setRawMethod(9999)
    val annotation = new AnnotationUnion
    annotation.setType(Annotation.wrappedRawMethod)
    annotation.setValue(method)
    data.getValues()(7).asFunction().getBase.getBase.getBase.setAnnos(Array(annotation))
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val moved = p.function(1).body.get
    p.edit(p.function(1)).moveBody(p.function(8))
    (p.function(8).body.contains(moved)) shouldBe true
    var output = Array.emptyByteArray

    val error = intercept[IllegalArgumentException](p.toBytes)
    (error.getMessage.contains("WrappedRawMethod.rawMethod has invalid Values ID")) shouldBe true
  }

  test("typed reference validation rejects mismatched union tags") {
    val p = fresh()
    p.p.getExprs()(0).setType(ExpressionElem.Expression)
    (p.validationIssues().exists(_.message == "Expression union tag/payload mismatch")) shouldBe true
  }

  test("write automatically detaches unreachable blocks and preserves IDs") {
    val p = fresh()
    val f = p.function(1)
    val g = f.body.get
    val unused = p.newBlock()
    val unusedId = p.valueId(unused)
    p.edit(g).replaceBlocks(g.blocks :+ unused, g.entryBlock)
    (p.validationIssues().isEmpty) shouldBe true // raw CHIR permits empty unreachable blocks
    roundTrip(p) { saved =>
      val active = saved.function(1).body.get
      active.blocks.size shouldBe 1
      val retired = saved.blockTable(unusedId)
      (retired.getParentGroup != saved.valueId(active)) shouldBe true
      (saved.groupTable(retired.getParentGroup).getBlocks.contains(unusedId)) shouldBe true
      (retired.getExprs.isEmpty) shouldBe true
    }
    g.blocks.size shouldBe 1
    (p.valueId(unused) == unusedId && p.blockTable(unusedId).getParentGroup == 0) shouldBe true
  }

  test("serialization rejects dangling definitions") {
    val p = fresh()
    val b = p.function(1).body.get.entryBlock
    p.edit(b).replaceExpressions(Seq.empty, b.terminator) // removes the return-local definition
    (p.validationIssues().exists(_.message.contains("Return local is defined outside body"))) shouldBe true
    var output = Array.emptyByteArray

    intercept[IllegalArgumentException](p.toBytes)
  }

  test("validate reports malformed references and control-flow edges") {
    val p = fresh()
    p.expressionTable(1).asAllocateBase().getBase.setResultTy(9999)
    (p.validationIssues().exists(_.message.contains("invalid Types ID"))) shouldBe true
    val q = fresh()
    val b = q.function(1).body.get.entryBlock
    q.edit(b).replaceTerminator(q.newGoto(q.function(8).body.get.entryBlock))
    (q.validationIssues().exists(_.message.contains("outside group"))) shouldBe true
  }

  test("serialization preserves original metadata and identifier convention") {
    val original = TestData.fixtureData()
    val raw = new FlatBufferBuilder
    raw.finish(CHIRPackage.pack(raw, original))
    Seq(original.serializeToBinary(), raw.sizedByteArray()).foreach { bytes =>
      val expectedIdentifier = CHIRPackage.CHIRPackageBufferHasIdentifier(ByteBuffer.wrap(bytes))
      val p = PackageImpl.fromBytes(bytes)
      roundTrip(p) { saved =>
        val f = saved.p.getValues()(0).asFunction()
        (f.getIsFastNative && f.getIsCffiwrapper) shouldBe true
        f.getBase.getRawMangledName shouldBe "preserve-this-metadata"
      }
      val output = p.toBytes
      CHIRPackage.CHIRPackageBufferHasIdentifier(ByteBuffer.wrap(output)) shouldBe expectedIdentifier
      (CHIR.fromBytes(output, 13002).isInstanceOf[PackageImpl]) shouldBe true
    }
  }

  test("validate rejects non-block goto targets and locals whose defining instruction was removed") {
    val p = fresh()
    p.expressionTable(2).asExpression().setKind(CHIRExprKind.Goto)
    p.expressionTable(2).asExpression().setOperands(Array(12L))
    (p.validationIssues().exists(_.message.contains("Goto target is not a block"))) shouldBe true

    val data = TestData.fixtureData()
    data.getValues()(0).asFunction().setRetVal(0)
    val use = new ExpressionT; use.setBase(new BaseT); use.setKind(CHIRExprKind.Store)
    use.setOperands(Array(4L, 4L)); use.setOwner(3)
    val u = new ExpressionElemUnion; u.setType(ExpressionElem.Expression); u.setValue(use)
    data.setExprs(data.getExprs :+ u)
    data.getExprs()(0).asAllocateBase().getBase.setOwner(0)
    data.getValues()(2).asBlock().setExprs(Array(6L, 2L))
    val q = PackageImpl.fromBytes(data.serializeToBinary())
    (q.validationIssues().exists(_.message.contains("Local operand is defined outside"))) shouldBe true
  }

  test("distinct Unit literals retain individual identities and IDs") {
    val data = TestData.fixtureData()
    def unit(): ValueElemUnion = {
      val v = new ValueT; v.setBase(new BaseT); v.setType(2); v.setKind(ValueKind.LITERAL)
      val l = new LiteralValueT; l.setBase(v); l.setLiteralKind(ConstantValueKind.UNIT)
      val u = new UnitLiteralT; u.setBase(l)
      val e = new ValueElemUnion; e.setType(ValueElem.UnitLiteral); e.setValue(u); e
    }
    data.setValues(data.getValues ++ Array(unit(), unit()))
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val a = p.getValue[CHIR.UnitLiteralValue](13).get
    val b = p.getValue[CHIR.UnitLiteralValue](14).get
    (a ne b) shouldBe true
    (p.valueId(a) == 13 && p.valueId(b) == 14) shouldBe true
    a.tpe shouldBe CHIR.BuiltinType.Unit
    roundTrip(p)(saved => saved.values.count(_.isInstanceOf[CHIR.UnitLiteralValue]) shouldBe 2)
  }

  test("schema payloads lacking specialized CHIR views are retained") {
    val data = TestData.fixtureData()
    val base = new ExpressionT; base.setBase(new BaseT); base.setKind(CHIRExprKind.Lambda)
    val lambda = new LambdaT; lambda.setBase(base); lambda.setFuncTy(5); lambda.setIdentifier("preserve-lambda")
    val entry = new ExpressionElemUnion; entry.setType(ExpressionElem.Lambda); entry.setValue(lambda)
    data.setExprs(data.getExprs :+ entry)
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val view = p.getExpr[CHIR.Expression](6)
    p.expressionId(view) shouldBe 6
    roundTrip(p)(saved => saved.expressionTable(6).asLambda().getIdentifier shouldBe "preserve-lambda")
  }

  test("moveBody remaps generic references in held views before a single write") {
    val p = PackageImpl.fromBytes(genericMoveData().serializeToBinary())
    val source = p.function(1)
    val target = p.function(8)
    val body = source.body.get
    val result = source.retVal.get
    val allocation = body.entryBlock.nonTerminatorExpressions.head.asInstanceOf[CHIR.Allocate]
    val cast = p.getExpr[CHIR.StaticCast](6)
    (cast.value eq source.params.head) shouldBe true
    (allocation.allocatedType eq source.genericTypeParams.head) shouldBe true
    val oldReturnName = result.identifier
    p.edit(source).moveBody(target)
    (source.body.isEmpty && source.retVal.isEmpty) shouldBe true
    (target.body.get eq body) shouldBe true
    (target.retVal.get eq result) shouldBe true
    (result.identifier != oldReturnName) shouldBe true
    (cast eq p.getExpr[CHIR.StaticCast](6)) shouldBe true
    (cast.value eq target.params(1)) shouldBe true
    (cast.targetTpe eq target.genericTypeParams.head) shouldBe true
    (allocation.allocatedType eq target.genericTypeParams.head) shouldBe true
    (result.tpe.asInstanceOf[CHIR.RefType].baseType eq target.genericTypeParams.head) shouldBe true
    (p.validationIssues().isEmpty) shouldBe true
    roundTrip(p) { saved =>
      (saved.function(1).body.isEmpty) shouldBe true
      val f = saved.function(8)
      val c = saved.getExpr[CHIR.StaticCast](6)
      (c.value eq f.params(1)) shouldBe true
      (c.targetTpe eq f.genericTypeParams.head) shouldBe true
      saved.groupTable(2).getOwnedFunc shouldBe 8
      f.retVal.get.identifier shouldBe result.identifier
    }
  }

  test("moveBody rejects a foreign function and incompatible generic bounds without mutation") {
    val p = PackageImpl.fromBytes(genericMoveData().serializeToBinary())
    val foreign = PackageImpl.fromBytes(genericMoveData().serializeToBinary())
    val before = p.p.serializeToBinary()
    intercept[IllegalArgumentException](p.edit(p.function(1)).moveBody(foreign.function(8)))
    (p.p.serializeToBinary().sameElements(before)) shouldBe true
    val data = genericMoveData()
    data.getTypes()(6).asGenericType().setUpperBounds(Array(1L))
    val q = PackageImpl.fromBytes(data.serializeToBinary())
    val unchanged = q.p.serializeToBinary()
    intercept[IllegalArgumentException](q.edit(q.function(1)).moveBody(q.function(8)))
    (q.p.serializeToBinary().sameElements(unchanged)) shouldBe true
  }

  test("missing body type specialization rejects a move without partial remapping") {
    val data = genericMoveData()
    // Keep the type ID valid but remove the destination Ref<T> specialization.
    data.getTypes()(8).asType().setArgTys(Array(2L))
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val before = p.p.serializeToBinary()
    val error = intercept[IllegalArgumentException](p.edit(p.function(1)).moveBody(p.function(8)))
    (error.getMessage.contains("Missing substituted composite type")) shouldBe true
    (p.p.serializeToBinary().sameElements(before)) shouldBe true
  }

  for ((sourceMember, targetMember) <- Seq(false -> false, false -> true, true -> false, true -> true)) {
    test(s"moveBody maps equal signatures: member=$sourceMember to member=$targetMember") {
      val data = receiverMoveData(sourceMember, targetMember)
      val p = PackageImpl.fromBytes(data.serializeToBinary())
      val source = p.function(1)
      val target = p.function(8)
      val cast = p.getExpr[CHIR.StaticCast](6)
      val body = source.body.get
      (cast.value eq source.params.head) shouldBe true
      p.edit(source).moveBody(target)
      (source.body.isEmpty) shouldBe true
      (target.body.get eq body) shouldBe true
      (cast.value eq target.params.head) shouldBe true
      source.declaringDef.isDefined shouldBe sourceMember
      target.declaringDef.isDefined shouldBe targetMember
      roundTrip(p) { saved =>
        (saved.function(1).body.isEmpty) shouldBe true
        (saved.getExpr[CHIR.StaticCast](6).value eq saved.function(8).params.head) shouldBe true
        saved.function(8).declaringDef.isDefined shouldBe targetMember
      }
    }
  }

  test("moveBody permits non-default function kinds with compatible signatures") {
    val data = receiverMoveData(true, true)
    data.getValues()(0).asFunction().setFuncKind(FuncKind.GETTER)
    data.getValues()(7).asFunction().setFuncKind(FuncKind.GETTER)
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    p.edit(p.function(1)).moveBody(p.function(8))
    p.function(8).kind shouldBe CHIR.Func.Kind.Getter
    roundTrip(p)(saved => (saved.function(8).body.nonEmpty) shouldBe true)
  }

  test("moveBody can omit an unused leading source receiver") {
    val data = receiverMoveData(true, false)
    // No explicit arguments: the source's sole parameter is its receiver.
    data.getValues()(7).asFunction().setParams(Array.emptyLongArray)
    data.getValues()(7).asFunction().getBase.getBase.setType(4)
    data.getValues()(2).asBlock().setExprs(Array(1L, 2L))
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    p.edit(p.function(1)).moveBody(p.function(8))
    (p.function(8).params.isEmpty && p.function(1).body.isEmpty) shouldBe true
    roundTrip(p)(saved => (saved.function(8).body.nonEmpty) shouldBe true)
  }

  test("moveBody rejects removing a used source receiver without mutation") {
    val data = receiverMoveData(true, false)
    data.getValues()(7).asFunction().setParams(Array.emptyLongArray)
    data.getValues()(7).asFunction().getBase.getBase.setType(4)
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val before = p.p.serializeToBinary()
    val error = intercept[IllegalArgumentException](p.edit(p.function(1)).moveBody(p.function(8)))
    (error.getMessage.contains("leading parameter that is used")) shouldBe true
    (p.p.serializeToBinary().sameElements(before)) shouldBe true
  }

  private def receiverMoveData(sourceMember: Boolean, targetMember: Boolean): CHIRPackageT = {
    val data = TestData.fixtureData()
    val t = new TypeT
    t.setKind(CHIRTypeKind.CLASS)
    val custom = new CustomTypeT
    custom.setBase(t)
    custom.setCustomTypeDef(1)
    val typeEntry = new TypeElemUnion
    typeEntry.setType(TypeElem.CustomType)
    typeEntry.setValue(custom)
    data.setTypes(data.getTypes :+ typeEntry)
    val definition = new CustomTypeDefT
    definition.setBase(PackageImpl.newBase())
    definition.setType(6)
    definition.setIdentifier("@C")
    definition.setSrcCodeIdentifier("C")
    definition.setPackageName("fixture")
    val cls = new ClassDefT
    cls.setBase(definition)
    cls.setIsClass(true)
    val defEntry = new CustomTypeDefElemUnion
    defEntry.setType(CustomTypeDefElem.ClassDef)
    defEntry.setValue(cls)
    data.setDefs(Array(defEntry))
    data.getValues()(0).asFunction().getBase.setDeclaredParent(if (sourceMember) 1L else 0L)
    data.getValues()(7).asFunction().getBase.setDeclaredParent(if (targetMember) 1L else 0L)
    data.getTypes()(4).asFuncType().getBase.setArgTys(Array(6L, 2L))
    data.getValues()(10).asParameter().getBase.setType(6)
    data.getValues()(11).asParameter().getBase.setType(6)
    val cast = new ExpressionT
    cast.setBase(PackageImpl.newBase())
    cast.setOwner(3)
    cast.setKind(CHIRExprKind.StaticCast)
    cast.setOperands(Array(12L))
    cast.setResultTy(6)
    val expression = new ExpressionElemUnion
    expression.setType(ExpressionElem.Expression)
    expression.setValue(cast)
    data.setExprs(data.getExprs :+ expression)
    data.getValues()(2).asBlock().setExprs(Array(1L, 6L, 2L))
    data
  }

  private def genericMoveData(): CHIRPackageT = {
    val data = TestData.fixtureData()
    def generic(name: String): TypeElemUnion = {
      val base = new TypeT; base.setKind(CHIRTypeKind.GENERIC)
      val t = new GenericTypeT; t.setBase(base); t.setIdentifier(name); t.setUpperBounds(Array(2L))
      val u = new TypeElemUnion; u.setType(TypeElem.GenericType); u.setValue(t); u
    }
    def tpe(kind: Int, args: Long*): TypeElemUnion = {
      val base = new TypeT; base.setKind(kind); base.setArgTys(args.toArray)
      val u = new TypeElemUnion
      if (kind == CHIRTypeKind.FUNC) {
        val f = new FuncTypeT; f.setBase(base); u.setType(TypeElem.FuncType); u.setValue(f)
      } else { u.setType(TypeElem.Type); u.setValue(base) }
      u
    }
    data.setTypes(data.getTypes ++ Array(generic("Source.T"), generic("Target.T"),
      tpe(CHIRTypeKind.REFTYPE, 6L), tpe(CHIRTypeKind.REFTYPE, 7L),
      tpe(CHIRTypeKind.FUNC, 6L, 6L), tpe(CHIRTypeKind.FUNC, 2L, 7L, 7L)))
    val source = data.getValues()(0).asFunction()
    val target = data.getValues()(7).asFunction()
    source.getBase.getBase.setType(10); source.setGenericTypeParams(Array(6L))
    target.getBase.getBase.setType(11); target.setGenericTypeParams(Array(7L))
    val receiver = data.getValues()(10).asParameter()
    val parameter = new ParameterT
    val base = new ValueT; base.setBase(new BaseT); base.setIdentifier("%1")
    base.setKind(ValueKind.PARAMETER); base.setType(7)
    parameter.setBase(base); parameter.setOwnedFunc(8)
    val u = new ValueElemUnion; u.setType(ValueElem.Parameter); u.setValue(parameter)
    data.setValues(data.getValues :+ u)
    target.setParams(Array(11L, 13L))
    receiver.getBase.setIdentifier("%0")
    data.getValues()(11).asParameter().getBase.setType(6)
    data.getValues()(3).asLocalVar().getBase.setType(8)
    val allocation = data.getExprs()(0).asAllocateBase()
    allocation.setAllocatedType(6); allocation.getBase.setResultTy(8)
    val cast = new ExpressionT; cast.setBase(new BaseT); cast.setOwner(3)
    cast.setKind(CHIRExprKind.StaticCast); cast.setOperands(Array(12L)); cast.setResultTy(6)
    val e = new ExpressionElemUnion; e.setType(ExpressionElem.Expression); e.setValue(cast)
    data.setExprs(data.getExprs :+ e)
    data.getValues()(2).asBlock().setExprs(Array(1L, 6L, 2L))
    data
  }

  private def roundTrip(p: PackageImpl)(check: PackageImpl => Unit): Unit = {
    val saved = PackageImpl.fromBytes(p.toBytes)
    (saved.validationIssues().isEmpty) shouldBe true
    check(saved)
  }

}
