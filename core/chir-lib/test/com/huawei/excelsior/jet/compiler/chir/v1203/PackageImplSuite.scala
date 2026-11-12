package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.EditableCHIR
import com.google.flatbuffers.FlatBufferBuilder
import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*
import org.scalatest.funsuite.AnyFunSuite

import java.nio.ByteBuffer
import java.nio.file.Files

class PackageImplSuite extends AnyFunSuite {
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
    assert(source.body.isEmpty && source.retVal.isEmpty)
    assert(target.body.get eq movedBody)
    assert(target.retVal.get eq movedReturn)
    assert(p.groupTable(p.valueId(replacedBody)).getOwnedFunc == 0)
    assert(!p.valueTable(p.valueId(replacedReturn)).asLocalVar().getIsRetVal)
    assert(p.groupTable(p.valueId(movedBody)).getOwnedFunc == target.id)
    assert(p.valueTable(p.valueId(movedReturn)).asLocalVar().getIsRetVal)
    assert(p.validationIssues().isEmpty)
    roundTrip(p) { saved =>
      assert(saved.function(1).body.isEmpty)
      assert(saved.function(8).body.exists(saved.valueId(_) == p.valueId(movedBody)))
      assert(saved.function(8).retVal.exists(saved.valueId(_) == p.valueId(movedReturn)))
      assert(!saved.valueTable(p.valueId(replacedReturn)).asLocalVar().getIsRetVal)
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
    assert(f.body.isEmpty && f.retVal.isEmpty)
    assert(p.groupTable(p.valueId(body)).getOwnedFunc == 0)
    assert(!p.valueTable(p.valueId(result)).asLocalVar().getIsRetVal)
    assert(f.tpe eq signature)
    assert(f.params == parameters && f.attributes == attributes && f.identifier == identifier)
    assert(p.function(8).body.nonEmpty)
    val after = p.data.serializeToBinary()
    p.edit(f).removeBody()
    assert(p.data.serializeToBinary().sameElements(after))
    assert(p.validationIssues().isEmpty)
    roundTrip(p) { saved =>
      val declaration = saved.function(1)
      assert(declaration.body.isEmpty && declaration.retVal.isEmpty)
      assert(declaration.identifier == identifier && declaration.attributes == attributes)
      assert(saved.valueId(declaration.params.head) == p.valueId(parameters.head))
      assert(saved.function(8).body.nonEmpty)
    }
  }

  test("loaded nodes have stable editable identities and registry IDs") {
    val p = fresh()
    val f = p.function(1)
    assert(f eq p.function(1))
    assert(f eq p.getFunc("first").get)
    assert(f eq p.getFunc("@first").get)
    assert(f eq p.function(f.id.toInt))
    val group = f.body.get
    val block = group.entryBlock
    assert(group eq p.function(1).body.get)
    assert(block eq group.entryBlock)
    assert(p.valueId(group) == 2 && p.valueId(block) == 3)
    assert(p.valueId(f.params.head) == 12)
    assert(p.expressionId(block.terminator) == 2)
    assert(p.typeId(f.tpe) == 5)
    assert(p.values.size == 12 && p.typeDefs.isEmpty)
    assert(p.getFunc("missing").isEmpty && p.getDef("missing").isEmpty)
    assert(p.validationIssues().isEmpty)
  }

  test("replaceTerminator updates held views and defers predecessor maintenance") {
    val p = fresh()
    val block = p.function(1).body.get.entryBlock
    val old = block.terminator
    val jump = p.newGoto(block, Some(CHIR.DebugLocation(10, 11)))
    val newBase = p.expressionTable(p.expressionId(jump)).asExpression().getBase
    assert(newBase.getAnnos != null && newBase.getLoc.getFilePath != null)
    p.edit(block).replaceTerminator(jump)
    assert(block.terminator eq jump)
    assert(block.nonTerminatorExpressions.size == 1)
    assert(jump.destination eq block)
    assert(jump.debugLoc.contains(CHIR.DebugLocation(10, 11)))
    assert(p.expressionTable(p.expressionId(old)).asExpression().getOwner == 0)
    assert(p.blockTable(3).getPredecessors.isEmpty)
    assert(p.validationIssues().isEmpty) // Explicit diagnostics refresh derived data too.
    assert(p.blockTable(3).getPredecessors.sameElements(Array(3L)))
    roundTrip(p) { saved =>
      val b = saved.function(1).body.get.entryBlock
      assert(b.terminator.asInstanceOf[CHIR.Goto].destination eq b)
      assert(b.terminator.debugLoc.contains(CHIR.DebugLocation(10, 11)))
    }
  }

  test("retired ownership is preserved on disk without reattaching nodes in memory") {
    val p = fresh()
    val b = p.function(1).body.get.entryBlock
    p.edit(b).replaceTerminator(p.newGoto(b))
    assert(p.expressionTable(2).asExpression().getOwner == 0)
    roundTrip(p) { saved =>
      assert(saved.expressionTable(2).asExpression().getOwner == 3)
      assert(!saved.blockTable(3).getExprs.contains(2L))
      val oldExit = saved.getExpr[CHIR.Terminator](2)
      val spare = saved.newBlock()
      saved.edit(spare).replaceTerminator(oldExit) // historical owner is not an active attachment
      val group = saved.function(1).body.get
      saved.edit(group).replaceBlocks(group.blocks :+ spare, group.entryBlock)
      assert(saved.validationIssues().isEmpty)
    }
    assert(p.expressionTable(2).asExpression().getOwner == 0)
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
      val lambda = p.data.getValues()(7).asFunction()
      lambda.setFuncKind(FuncKind.LAMBDA)
      p.edit(p.function(8)).removeBody()
      assert(p.validationIssues().isEmpty)
      val values = p.data.getValues
      val exprs = p.data.getExprs
      val spareTable = p.blockTable(p.valueId(spare))
      val instructions = spareTable.getExprs
      val predecessors = spareTable.getPredecessors
      val counter = values(0).asFunction().getBlockGroupId
      val before = p.data.serializeToBinary()
      val output = Files.createTempFile("wire-overrides-", ".chir")
      val sentinel = Array[Byte](1, 2, 3)
      try {
        Files.write(output, sentinel)
        if (failPacking) {
          // Validation accepts the graph; the UTF-8 packer rejects this name
          // after all serialization-only overrides have been applied.
          p.data.setName("\ud800")
          try intercept[IllegalArgumentException](p.write(output))
          finally p.data.setName("fixture")
          assert(Files.readAllBytes(output).sameElements(sentinel))
        } else {
          p.write(output)
        }
        assert(p.data.getValues eq values)
        assert(p.data.getExprs eq exprs)
        assert(p.expressionTable(2).asExpression().getOwner == 0)
        assert(p.groupTable(9).getOwnedFunc == 0)
        assert(spareTable.getParentGroup == 0)
        assert(spareTable.getExprs eq instructions)
        assert(spareTable.getPredecessors eq predecessors)
        assert(values(0).asFunction().getBlockGroupId == counter)
        assert(lambda.getFuncKind == FuncKind.LAMBDA)
        assert(block.terminator eq jump)
        assert(spare.terminator eq spareJump)
        assert(p.data.serializeToBinary().sameElements(before))
        p.write(output)
        val first = Files.readAllBytes(output)
        val saved = PackageImpl.fromBytes(Files.readAllBytes(output))
        assert(saved.expressionTable(2).asExpression().getOwner == p.valueId(block))
        assert(saved.blockTable(p.valueId(spare)).getParentGroup > values.length)
        assert(saved.blockTable(p.valueId(spare)).getExprs.isEmpty)
        assert(saved.data.getValues()(7).asFunction().getFuncKind == FuncKind.DEFAULT)
        assert(saved.data.getValues()(0).asFunction().getBlockGroupId > counter)
        p.write(output)
        assert(Files.readAllBytes(output).sameElements(first))
        assert(p.data.getValues eq values)
        assert(p.data.serializeToBinary().sameElements(before))
      } finally Files.delete(output)
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
    val output = Files.createTempFile("wire-vectors-", ".chir")
    try {
      p.write(output)
      val raw = CHIRPackage.getRootAsCHIRPackage(ByteBuffer.wrap(Files.readAllBytes(output)))
      val t = raw.types(new Type, (boolTypeId - 1).toInt).asInstanceOf[Type]
      val e = raw.exprs(new Expression, (expressionId - 1).toInt).asInstanceOf[Expression]
      val b = raw.values(new Block, (p.valueId(second) - 1).toInt).asInstanceOf[Block]
      assert(t.argTysVector() != null)
      assert(e.blockGroupsVector() != null)
      assert(b.exceptionCatchListVector() != null)
      assert(PackageImpl.fromBytes(Files.readAllBytes(output)).validationIssues().isEmpty)
    } finally Files.delete(output)
  }

  test("attribute caching follows both API and backing-mask updates") {
    val p = fresh()
    val f = p.function(1)
    val original = f.attributes
    assert(f.attributes eq original)
    p.edit(f).replaceAttributes(Seq(CHIR.Attribute.Imported))
    val imported = f.attributes
    assert(imported == Seq(CHIR.Attribute.Imported))
    assert(f.attributes eq imported)
    p.data.getValues()(0).asFunction().getBase.getBase.getBase.setAttributes(1L << CHIR.Attribute.Public.ordinal)
    assert(f.attributes == Seq(CHIR.Attribute.Public))
    p.edit(f).replaceAttributes(Seq.empty)
    assert(f.attributes.isEmpty)
  }

  test("batched appends preserve IDs and held views across flushes and saving") {
    val p = fresh()
    val f = p.function(1)
    val block = f.body.get.entryBlock
    val initialValues = p.values.size
    val initialTypes = p.data.getTypes.length
    val first = p.edit(block).insert(EditableCHIR.Operation.BooleanConstant(true))
    val firstId = p.valueId(first)
    for (i <- 1 until 128) {
      p.edit(block).insert(EditableCHIR.Operation.BooleanConstant(i % 2 == 0))
    }
    assert(p.values.size == initialValues + 256)
    assert(p.getValue[CHIR.LocalVar](firstId).get eq first)
    assert(p.data.getValues.length == initialValues + 256)
    assert(p.data.getTypes.length == initialTypes + 1)
    p.edit(block).insert(EditableCHIR.Operation.BooleanConstant(false))
    assert(p.getValue[CHIR.LocalVar](firstId).get eq first)
    assert(p.function(1) eq f)
    roundTrip(p) { saved =>
      assert(saved.values.size == initialValues + 258)
      assert(saved.getValue[CHIR.LocalVar](firstId).get.tpe == CHIR.BuiltinType.Boolean)
      assert(saved.function(1).body.get.entryBlock.nonTerminatorExpressions.size == 130)
      assert(saved.data.getTypes.length == initialTypes + 1)
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
    assert(p.getType[CHIR.Type](7).contains(CHIR.BuiltinType.Boolean))
    val value = p.edit(p.function(1).body.get.entryBlock).insert(EditableCHIR.Operation.BooleanConstant(true))
    assert(p.valueTable(p.valueId(value)).asLocalVar().getBase.getType == 6)
    assert(p.data.getTypes.length == 7)
  }

  test("new nodes may be staged but must be attached before writing") {
    val p = fresh()
    val block = p.newBlock()
    val jump = p.newGoto(block)
    assert(p.validationIssues().exists(_.message.contains("New expression has not been attached")))
    assert(p.validationIssues().exists(_.message.contains("New block has not been attached")))
    p.edit(block).replaceTerminator(jump)
    val group = p.newBlockGroup(Seq(block), block)
    assert(p.validationIssues().exists(_.message.contains("New group has not been attached")))
    val output = Files.createTempFile("staged-chir-", ".chir")
    try {
      val error = intercept[IllegalArgumentException](p.write(output))
      assert(error.getMessage.contains("New group has not been attached"))
      assert(Files.size(output) == 0)
    } finally Files.delete(output)
    p.edit(p.function(1)).replaceBody(Some(group), None)
    assert(p.validationIssues().isEmpty)
    roundTrip(p)(saved => assert(saved.function(1).body.get.blocks.size == 1))
  }

  test("newBranch, expression replacement and new block membership survive reload") {
    val source = TestData.fixtureData()
    val boolType = new TypeT; boolType.setKind(CHIRTypeKind.BOOLEAN)
    val typeEntry = new TypeElemUnion; typeEntry.setType(TypeElem.Type); typeEntry.setValue(boolType)
    source.setTypes(source.getTypes :+ typeEntry)
    val value = new ValueT; value.setBase(new BaseT); value.setType(6); value.setKind(ValueKind.LITERAL)
    val literalBase = new LiteralValueT; literalBase.setBase(value); literalBase.setLiteralKind(ConstantValueKind.BOOL)
    val literal = new BoolLiteralT; literal.setBase(literalBase); literal.setVal(true)
    val literalEntry = new ValueElemUnion; literalEntry.setType(ValueElem.BoolLiteral); literalEntry.setValue(literal)
    source.setValues(source.getValues :+ literalEntry)
    val p = PackageImpl.fromBytes(source.serializeToBinary())
    val g = p.function(1).body.get
    val first = g.entryBlock
    val exit = first.terminator
    val second = p.newBlock()
    val branch = p.newBranch(p.getValue[CHIR.Value](13).get, first, second)
    p.edit(first).replaceTerminator(branch)
    p.edit(second).replaceExpressions(Seq.empty, exit)
    p.edit(g).replaceBlocks(Seq(first, second), first)
    assert(p.blockTable(p.valueId(second)).getParentGroup == p.valueId(g))
    assert(PackageImpl.ids(p.blockTable(p.valueId(second)).getPredecessors).isEmpty)
    assert(p.validationIssues().isEmpty)
    assert(p.blockTable(p.valueId(second)).getPredecessors.sameElements(Array(3L)))
    roundTrip(p) { saved =>
      val body = saved.function(1).body.get
      val b = body.entryBlock.terminator.asInstanceOf[CHIR.Branch]
      assert(b.trueBlock eq body.entryBlock)
      assert(b.falseBlock eq body.blocks(1))
      assert(b.condition.asInstanceOf[CHIR.BoolLiteral].value)
    }
  }

  test("newBlockGroup connects to the existing replaceBody implementation") {
    val p = fresh()
    val block = p.newBlock(isLandingPad = true)
    p.edit(block).replaceTerminator(p.newGoto(block))
    val body = p.newBlockGroup(Seq(block), block)
    p.edit(p.function(1)).replaceBody(Some(body), None)
    assert(p.function(1).body.get eq body)
    assert(p.groupTable(p.valueId(body)).getOwnedFunc == 1)
    assert(p.groupTable(2).getOwnedFunc == 0)
    assert(p.validationIssues().isEmpty)
    roundTrip(p) { saved => assert(saved.function(1).body.get.entryBlock.isLandingPadBlock) }
  }

  test("terminator replacement defers unreachable block pruning until write") {
    val p = fresh()
    val g = p.function(1).body.get
    val entry = g.entryBlock
    val unused = p.newBlock()
    val unusedId = p.valueId(unused)
    // Membership can be staged; no caller-triggered cleanup is necessary.
    p.edit(g).replaceBlocks(g.blocks :+ unused, entry)
    assert(g.blocks.size == 2)
    p.edit(entry).replaceTerminator(p.newGoto(entry))
    assert(g.blocks.size == 2)
    assert(p.blockTable(unusedId).getParentGroup == p.valueId(g))
    roundTrip(p)(saved => assert(saved.function(1).body.get.blocks.size == 1))
    assert(g.blocks == Seq(entry))
    assert(p.blockTable(unusedId).getParentGroup == 0)
  }

  test("moveBody checks types used by the moved body before applying edits") {
    val data = genericMoveData()
    data.getExprs()(0).asAllocateBase().getBase.setResultTy(9999)
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val before = p.data.serializeToBinary()
    val error = intercept[IllegalArgumentException](p.edit(p.function(1)).moveBody(p.function(8)))
    assert(error.getMessage.contains("Invalid type ID"))
    assert(p.data.serializeToBinary().sameElements(before))
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
    assert(p.function(8).body.contains(moved))
    val output = Files.createTempFile("invalid-metadata-", ".chir")
    val sentinel = Array[Byte](1, 2, 3)
    try {
      Files.write(output, sentinel)
      val error = intercept[IllegalArgumentException](p.write(output))
      assert(error.getMessage.contains("WrappedRawMethod.rawMethod has invalid Values ID"))
      assert(Files.readAllBytes(output).sameElements(sentinel))
    } finally Files.delete(output)
  }

  test("typed reference validation rejects mismatched union tags") {
    val p = fresh()
    p.data.getExprs()(0).setType(ExpressionElem.Expression)
    assert(p.validationIssues().exists(_.message == "Expression union tag/payload mismatch"))
  }

  test("write automatically detaches unreachable blocks and preserves IDs") {
    val p = fresh()
    val f = p.function(1)
    val g = f.body.get
    val unused = p.newBlock()
    val unusedId = p.valueId(unused)
    p.edit(g).replaceBlocks(g.blocks :+ unused, g.entryBlock)
    assert(p.validationIssues().isEmpty) // raw CHIR permits empty unreachable blocks
    roundTrip(p) { saved =>
      val active = saved.function(1).body.get
      assert(active.blocks.size == 1)
      val retired = saved.blockTable(unusedId)
      assert(retired.getParentGroup != saved.valueId(active))
      assert(saved.groupTable(retired.getParentGroup).getBlocks.contains(unusedId))
      assert(retired.getExprs.isEmpty)
    }
    assert(g.blocks.size == 1)
    assert(p.valueId(unused) == unusedId && p.blockTable(unusedId).getParentGroup == 0)
  }

  test("foreign nodes, shared expressions and invalid branches fail without edits") {
    val p = fresh(); val foreign = fresh()
    val b = p.function(1).body.get.entryBlock
    val before = p.data.serializeToBinary()
    intercept[IllegalArgumentException](p.valueId(foreign.function(1)))
    intercept[IllegalArgumentException](p.edit(b).replaceTerminator(p.function(8).body.get.entryBlock.terminator))
    intercept[IllegalArgumentException](p.newBranch(p.function(1).params.head, b, b))
    intercept[IllegalArgumentException](p.newGoto(b, Some(CHIR.DebugLocation(10, 1))))
    assert(p.data.serializeToBinary().sameElements(before))
  }

  test("block groups reject duplicate, foreign, shared and absent entry blocks") {
    val p = fresh(); val q = fresh()
    val g = p.function(1).body.get
    val b = g.entryBlock
    val other = p.function(8).body.get.entryBlock
    val before = p.data.serializeToBinary()
    intercept[IllegalArgumentException](p.edit(g).replaceBlocks(Seq(b, b), b))
    intercept[IllegalArgumentException](p.edit(g).replaceBlocks(Seq(b), other))
    intercept[IllegalArgumentException](p.edit(g).replaceBlocks(Seq(b, other), b))
    intercept[IllegalArgumentException](p.edit(g).replaceBlocks(Seq(b, q.function(1).body.get.entryBlock), b))
    intercept[IllegalArgumentException](p.newBlockGroup(Seq(b), b))
    assert(p.data.serializeToBinary().sameElements(before))
  }

  test("write validates dangling definitions before touching an existing output") {
    val p = fresh()
    val b = p.function(1).body.get.entryBlock
    p.edit(b).replaceExpressions(Seq.empty, b.terminator) // removes the return-local definition
    assert(p.validationIssues().exists(_.message.contains("Return local is defined outside body")))
    val output = Files.createTempFile("invalid-chir-", ".chir")
    val sentinel = Array[Byte](1, 2, 3)
    try {
      Files.write(output, sentinel)
      intercept[IllegalArgumentException](p.write(output))
      assert(Files.readAllBytes(output).sameElements(sentinel))
    } finally Files.delete(output)
  }

  test("validate reports malformed references and control-flow edges") {
    val p = fresh()
    p.expressionTable(1).asAllocateBase().getBase.setResultTy(9999)
    assert(p.validationIssues().exists(_.message.contains("invalid Types ID")))
    val q = fresh()
    val b = q.function(1).body.get.entryBlock
    q.edit(b).replaceTerminator(q.newGoto(q.function(8).body.get.entryBlock))
    assert(q.validationIssues().exists(_.message.contains("outside group")))
  }

  test("serialization preserves original metadata and identifier convention") {
    val original = TestData.fixtureData()
    val raw = new FlatBufferBuilder
    raw.finish(CHIRPackage.pack(raw, original))
    Seq(original.serializeToBinary(), raw.sizedByteArray()).foreach { bytes =>
      val expectedIdentifier = CHIRPackage.CHIRPackageBufferHasIdentifier(ByteBuffer.wrap(bytes))
      val p = PackageImpl.fromBytes(bytes)
      roundTrip(p) { saved =>
        val f = saved.data.getValues()(0).asFunction()
        assert(f.getIsFastNative && f.getIsCffiwrapper)
        assert(f.getBase.getRawMangledName == "preserve-this-metadata")
      }
      val output = Files.createTempFile("chir-header-", ".chir")
      try {
        p.write(output)
        assert(CHIRPackage.CHIRPackageBufferHasIdentifier(ByteBuffer.wrap(Files.readAllBytes(output))) == expectedIdentifier)
        assert(CHIR.read(output, 1203).isInstanceOf[PackageImpl])
      } finally Files.delete(output)
    }
  }

  test("validate rejects non-block goto targets and locals whose defining instruction was removed") {
    val p = fresh()
    p.expressionTable(2).asExpression().setKind(CHIRExprKind.Goto)
    p.expressionTable(2).asExpression().setOperands(Array(12L))
    assert(p.validationIssues().exists(_.message.contains("Goto target is not a block")))

    val data = TestData.fixtureData()
    data.getValues()(0).asFunction().setRetVal(0)
    val use = new ExpressionT; use.setBase(new BaseT); use.setKind(CHIRExprKind.Store)
    use.setOperands(Array(4L, 4L)); use.setOwner(3)
    val u = new ExpressionElemUnion; u.setType(ExpressionElem.Expression); u.setValue(use)
    data.setExprs(data.getExprs :+ u)
    data.getExprs()(0).asAllocateBase().getBase.setOwner(0)
    data.getValues()(2).asBlock().setExprs(Array(6L, 2L))
    val q = PackageImpl.fromBytes(data.serializeToBinary())
    assert(q.validationIssues().exists(_.message.contains("Local operand is defined outside")))
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
    assert(a ne b)
    assert(p.valueId(a) == 13 && p.valueId(b) == 14)
    assert(a.tpe == CHIR.BuiltinType.Unit)
    roundTrip(p)(saved => assert(saved.values.count(_.isInstanceOf[CHIR.UnitLiteralValue]) == 2))
  }

  test("schema payloads lacking specialized CHIR views are retained") {
    val data = TestData.fixtureData()
    val base = new ExpressionT; base.setBase(new BaseT); base.setKind(CHIRExprKind.Lambda)
    val lambda = new LambdaT; lambda.setBase(base); lambda.setFuncTy(5); lambda.setIdentifier("preserve-lambda")
    val entry = new ExpressionElemUnion; entry.setType(ExpressionElem.Lambda); entry.setValue(lambda)
    data.setExprs(data.getExprs :+ entry)
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val view = p.getExpr[CHIR.Expression](6)
    assert(p.expressionId(view) == 6)
    roundTrip(p)(saved => assert(saved.expressionTable(6).asLambda().getIdentifier == "preserve-lambda"))
  }

  test("moveBody remaps generic references in held views before a single write") {
    val p = PackageImpl.fromBytes(genericMoveData().serializeToBinary())
    val source = p.function(1)
    val target = p.function(8)
    val body = source.body.get
    val result = source.retVal.get
    val allocation = body.entryBlock.nonTerminatorExpressions.head.asInstanceOf[CHIR.Allocate]
    val cast = p.getExpr[CHIR.StaticCast](6)
    assert(cast.value eq source.params.head)
    assert(allocation.allocatedType eq source.genericTypeParams.head)
    val oldReturnName = result.identifier
    p.edit(source).moveBody(target)
    assert(source.body.isEmpty && source.retVal.isEmpty)
    assert(target.body.get eq body)
    assert(target.retVal.get eq result)
    assert(result.identifier != oldReturnName)
    assert(cast eq p.getExpr[CHIR.StaticCast](6))
    assert(cast.value eq target.params(1))
    assert(cast.targetTpe eq target.genericTypeParams.head)
    assert(allocation.allocatedType eq target.genericTypeParams.head)
    assert(result.tpe.asInstanceOf[CHIR.RefType].baseType eq target.genericTypeParams.head)
    assert(p.validationIssues().isEmpty)
    roundTrip(p) { saved =>
      assert(saved.function(1).body.isEmpty)
      val f = saved.function(8)
      val c = saved.getExpr[CHIR.StaticCast](6)
      assert(c.value eq f.params(1))
      assert(c.targetTpe eq f.genericTypeParams.head)
      assert(saved.groupTable(2).getOwnedFunc == 8)
      assert(f.retVal.get.identifier == result.identifier)
    }
  }

  test("moveBody rejects a foreign function and incompatible generic bounds without mutation") {
    val p = PackageImpl.fromBytes(genericMoveData().serializeToBinary())
    val foreign = PackageImpl.fromBytes(genericMoveData().serializeToBinary())
    val before = p.data.serializeToBinary()
    intercept[IllegalArgumentException](p.edit(p.function(1)).moveBody(foreign.function(8)))
    assert(p.data.serializeToBinary().sameElements(before))
    val data = genericMoveData()
    data.getTypes()(6).asGenericType().setUpperBounds(Array(1L))
    val q = PackageImpl.fromBytes(data.serializeToBinary())
    val unchanged = q.data.serializeToBinary()
    intercept[IllegalArgumentException](q.edit(q.function(1)).moveBody(q.function(8)))
    assert(q.data.serializeToBinary().sameElements(unchanged))
  }

  test("missing body type specialization rejects a move without partial remapping") {
    val data = genericMoveData()
    // Keep the type ID valid but remove the destination Ref<T> specialization.
    data.getTypes()(8).asType().setArgTys(Array(2L))
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val before = p.data.serializeToBinary()
    val error = intercept[IllegalArgumentException](p.edit(p.function(1)).moveBody(p.function(8)))
    assert(error.getMessage.contains("Missing substituted composite type"))
    assert(p.data.serializeToBinary().sameElements(before))
  }

  for ((sourceMember, targetMember) <- Seq(false -> false, false -> true, true -> false, true -> true)) {
    test(s"moveBody maps equal signatures: member=$sourceMember to member=$targetMember") {
      val data = receiverMoveData(sourceMember, targetMember)
      val p = PackageImpl.fromBytes(data.serializeToBinary())
      val source = p.function(1)
      val target = p.function(8)
      val cast = p.getExpr[CHIR.StaticCast](6)
      val body = source.body.get
      assert(cast.value eq source.params.head)
      p.edit(source).moveBody(target)
      assert(source.body.isEmpty)
      assert(target.body.get eq body)
      assert(cast.value eq target.params.head)
      assert(source.declaringDef.isDefined == sourceMember)
      assert(target.declaringDef.isDefined == targetMember)
      roundTrip(p) { saved =>
        assert(saved.function(1).body.isEmpty)
        assert(saved.getExpr[CHIR.StaticCast](6).value eq saved.function(8).params.head)
        assert(saved.function(8).declaringDef.isDefined == targetMember)
      }
    }
  }

  test("moveBody permits non-default function kinds with compatible signatures") {
    val data = receiverMoveData(true, true)
    data.getValues()(0).asFunction().setFuncKind(FuncKind.GETTER)
    data.getValues()(7).asFunction().setFuncKind(FuncKind.GETTER)
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    p.edit(p.function(1)).moveBody(p.function(8))
    assert(p.function(8).kind == CHIR.Func.Kind.Getter)
    roundTrip(p)(saved => assert(saved.function(8).body.nonEmpty))
  }

  test("moveBody can omit an unused leading source receiver") {
    val data = receiverMoveData(true, false)
    // No explicit arguments: the source's sole parameter is its receiver.
    data.getValues()(7).asFunction().setParams(Array.emptyLongArray)
    data.getValues()(7).asFunction().getBase.getBase.setType(4)
    data.getValues()(2).asBlock().setExprs(Array(1L, 2L))
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    p.edit(p.function(1)).moveBody(p.function(8))
    assert(p.function(8).params.isEmpty && p.function(1).body.isEmpty)
    roundTrip(p)(saved => assert(saved.function(8).body.nonEmpty))
  }

  test("moveBody rejects removing a used source receiver without mutation") {
    val data = receiverMoveData(true, false)
    data.getValues()(7).asFunction().setParams(Array.emptyLongArray)
    data.getValues()(7).asFunction().getBase.getBase.setType(4)
    val p = PackageImpl.fromBytes(data.serializeToBinary())
    val before = p.data.serializeToBinary()
    val error = intercept[IllegalArgumentException](p.edit(p.function(1)).moveBody(p.function(8)))
    assert(error.getMessage.contains("leading parameter that is used"))
    assert(p.data.serializeToBinary().sameElements(before))
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
    val output = Files.createTempFile("editable-chir-", ".chir")
    try {
      p.write(output)
      val saved = PackageImpl.fromBytes(Files.readAllBytes(output))
      assert(saved.validationIssues().isEmpty)
      check(saved)
    } finally Files.delete(output)
  }
}
