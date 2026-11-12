package com.huawei.excelsior.jet.compiler.chir

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Run shared editing contracts against every schema backend. */
class EditingApiSuite extends AnyFunSuite with Matchers {
  private val backends = Seq(1203, 13002, 13008).map(version => version -> (() => TestData.editableFixture(version)))

  for ((version, fresh) <- backends) {
    test(s"$version new blocks and groups replace a body and survive saving") {
      val pkg = fresh()
      val entry = pkg.newBlock()
      val next = pkg.newBlock()
      entry.replaceTerminator(pkg.newGoto(next))
      next.replaceTerminator(pkg.newGoto(next))
      val body = pkg.newBlockGroup(Seq(entry, next), entry)
      val function = pkg.getFunc("@first").get
      function.removeBody()
      function.replaceBody(Some(body), None)
      val condition = entry.insert(EditableCHIR.Operation.BooleanConstant(true))
      entry.replaceTerminator(pkg.newBranch(condition, entry, next))
      body.replaceBlocks(Seq(next, entry), entry)
      function.replaceAttributes(Seq(CHIR.Attribute.Public))
      val saved = EditableCHIR.fromBytes(pkg.toBytes, version).getFunc("@first").get
      saved.attributes shouldBe Seq(CHIR.Attribute.Public)
      val group = saved.body.get
      val branch = group.entryBlock.terminator.asInstanceOf[CHIR.Branch]
      (branch.trueBlock eq group.entryBlock) shouldBe true
      (branch.falseBlock eq group.blocks.head) shouldBe true
      branch.condition.asInstanceOf[CHIR.LocalVar].associatedExpr.asInstanceOf[CHIR.Constant]
        .literal.asInstanceOf[CHIR.BoolLiteral].value shouldBe true
    }

    test(s"$version constructing a group connects to body replacement") {
      val pkg = fresh()
      val function = pkg.getFunc("@first").get
      val block = pkg.newBlock()
      block.replaceTerminator(pkg.newGoto(block))
      val group = pkg.newBlockGroup(Seq(block), block)
      function.replaceBody(Some(group), None)
      (function.body.get eq group) shouldBe true
      EditableCHIR.fromBytes(pkg.toBytes, version).getFunc("@first").get.body.get.blocks.size shouldBe 1
    }

    test(s"$version rejects foreign nodes, invalid branches and shared instructions atomically") {
      val pkg = fresh()
      val foreign = fresh()
      val function = pkg.getFunc("@first").get
      val block = function.body.get.entryBlock
      val other = pkg.newBlock()
      other.replaceTerminator(pkg.newExit())
      pkg.getFunc("@second").get.replaceBody(Some(pkg.newBlockGroup(Seq(other), other)), None)
      val before = pkg.toBytes
      intercept[IllegalArgumentException](pkg.edit(foreign.getFunc("@first").get))
      intercept[IllegalArgumentException](block.replaceTerminator(other.terminator))
      intercept[IllegalArgumentException](pkg.newBranch(function.params.head, block, block))
      intercept[IllegalArgumentException](pkg.newGoto(block, Some(CHIR.DebugLocation(10, 1))))
      pkg.toBytes shouldBe before
    }

    test(s"$version block groups reject duplicate, foreign, shared and absent entry blocks") {
      val pkg = fresh()
      val group = pkg.getFunc("@first").get.body.get
      val block = group.entryBlock
      val other = pkg.newBlock()
      other.replaceTerminator(pkg.newExit())
      pkg.getFunc("@second").get.replaceBody(Some(pkg.newBlockGroup(Seq(other), other)), None)
      val foreign = fresh().getFunc("@first").get.body.get.entryBlock
      val before = pkg.toBytes
      intercept[IllegalArgumentException](group.replaceBlocks(Seq(block, block), block))
      intercept[IllegalArgumentException](group.replaceBlocks(Seq(block), other))
      intercept[IllegalArgumentException](group.replaceBlocks(Seq(block, other), block))
      intercept[IllegalArgumentException](group.replaceBlocks(Seq(block, foreign), block))
      intercept[IllegalArgumentException](pkg.newBlockGroup(Seq(block), block))
      pkg.toBytes shouldBe before
    }

    test(s"$version editable nodes retain identity when viewed through the read API") {
      val editable = fresh()
      val function: EditableCHIR.Func = editable.getFunc("@first").get
      val readFunction: CHIR.Func = editable.getFunc("@first").get
      val group: EditableCHIR.BlockGroup = function.body.get
      val block: EditableCHIR.Block = group.entryBlock
      val expression: EditableCHIR.Expression = block.terminator
      (function eq readFunction) shouldBe true
      (editable.edit(readFunction) eq function) shouldBe true
      (editable.edit(readFunction.body.get) eq group) shouldBe true
      (editable.edit(readFunction.body.get.entryBlock) eq block) shouldBe true
      (editable.edit(readFunction.body.get.entryBlock.terminator) eq expression) shouldBe true
      block.replaceTerminator(editable.newGoto(block))
      (readFunction.body.get.entryBlock.terminator.asInstanceOf[CHIR.Goto].destination eq block) shouldBe true
      function.removeBody()
      (readFunction.body.isEmpty) shouldBe true
    }

    test(s"$version refuses editable views of another package's nodes") {
      val editable = fresh()
      val foreign = fresh()
      val function: CHIR.Func = foreign.getFunc("@first").get
      val group = function.body.get
      val block = group.entryBlock
      val variable: CHIR.GlobalVar = foreign.newGlobalVar("@global", CHIR.BuiltinType.Unit)
      intercept[IllegalArgumentException](editable.edit(function))
      intercept[IllegalArgumentException](editable.edit(group))
      intercept[IllegalArgumentException](editable.edit(block))
      intercept[IllegalArgumentException](editable.edit(block.terminator))
      intercept[IllegalArgumentException](editable.edit(variable))
      intercept[IllegalArgumentException](editable.edit(function: CHIR.HasAttributes))
      (editable.getFunc("@first").get.body.nonEmpty) shouldBe true
    }

    test(s"$version rejects a foreign goto destination without changing the body") {
      val editable = fresh()
      val p = editable
      val block = p.getFunc("@first").get.body.get.entryBlock
      val before = block.expressions
      intercept[IllegalArgumentException](editable.newGoto(fresh().getFunc("@first").get.body.get.entryBlock))
      block.expressions shouldBe before
    }

    test(s"$version rejects unregistered groups without evaluating their contents") {
      val editable = fresh()
      val function = editable.getFunc("@first").get
      val body = function.body
      val group = new CHIR.BlockGroup {
        def blocks: Seq[CHIR.Block] = throw new AssertionError("Unregistered group must not be queried")
        def entryBlock: CHIR.Block = throw new AssertionError("Unregistered group must not be queried")
      }
      intercept[IllegalArgumentException](editable.edit(group))
      intercept[IllegalArgumentException](function.replaceBody(Some(group), None))
      function.body shouldBe body
    }

    test(s"$version rejects unregistered locals without evaluating their contents") {
      val editable = fresh()
      val function = editable.getFunc("@first").get
      val result = function.retVal
      val local = new CHIR.LocalVar {
        def identifier: String = throw new AssertionError("Unregistered local must not be queried")
        def srcCodeIdentifier: String = throw new AssertionError("Unregistered local must not be queried")
        def tpe: CHIR.Type = throw new AssertionError("Unregistered local must not be queried")
        def associatedExpr: CHIR.Expression = throw new AssertionError("Unregistered local must not be queried")
      }
      intercept[IllegalArgumentException](function.replaceBody(function.body, Some(local)))
      function.retVal shouldBe result
    }

    test(s"$version rejects synthetic functions without evaluating their generated body") {
      val editable = fresh()
      val function = CHIR.newIntrinsicsPackage.values.collectFirst { case f: CHIR.Func => f }.get
      (function.params.isEmpty) shouldBe true
      intercept[IllegalArgumentException](editable.edit(function).removeBody())
      intercept[IllegalArgumentException](editable.edit(function).replaceAttributes(Seq(CHIR.Attribute.Public)))
    }

    test(s"$version function and global attributes update held views and survive serialization") {
      val editable = fresh()
      val function = editable.getFunc("@first").get
      val global = editable.newGlobalVar("@global", CHIR.BuiltinType.Unit)
      editable.edit(function).replaceAttributes(Seq(CHIR.Attribute.Public, CHIR.Attribute.NoInline, CHIR.Attribute.Public))
      global.replaceAttributes(Seq(CHIR.Attribute.Private, CHIR.Attribute.Mut))
      function.attributes shouldBe Seq(CHIR.Attribute.Public, CHIR.Attribute.NoInline)
      global.attributes shouldBe Seq(CHIR.Attribute.Private, CHIR.Attribute.Mut)
      val saved = EditableCHIR.fromBytes(editable.toBytes, version)
      saved.getFunc("@first").get.attributes shouldBe function.attributes
      val savedGlobal = saved.values.collectFirst { case g: CHIR.GlobalVar => g }.get
      savedGlobal.attributes shouldBe global.attributes
      saved.edit(savedGlobal).replaceAttributes(Seq.empty)
      (savedGlobal.attributes.isEmpty) shouldBe true
      (global.attributes.nonEmpty) shouldBe true
    }

    test(s"$version moves a body through held views and preserves the destination signature") {
      val editable = fresh()
      val p = editable
      val from = p.getFunc("@first").get
      val to = p.getFunc("@second").get
      val body = from.body.get
      val signature = to.tpe
      val parameters = to.params
      editable.edit(from).moveBody(to)
      (from.body.isEmpty && to.body.contains(body)) shouldBe true
      (to.tpe eq signature) shouldBe true
      to.params shouldBe parameters
      val saved = CHIR.fromBytes(editable.toBytes, version)
      (saved.getFunc("@first").get.body.isEmpty) shouldBe true
      (saved.getFunc("@second").get.body.nonEmpty) shouldBe true
    }

    test(s"$version supports every transformation operation and saving") {
      val editable = fresh()
      val p = editable
      val function = editable.getFunc("@first").get
      val block = function.body.get.entryBlock
      val reference = new CHIR.RefType { def baseType: CHIR.Type = CHIR.BuiltinType.Unit }
      val tupleType = new CHIR.TupleType {
        def fieldTypes: Seq[CHIR.Type] = Seq(CHIR.BuiltinType.Boolean, reference)
      }
      val allocated = block.insert(EditableCHIR.Operation.Allocate(CHIR.BuiltinType.Unit))
      val cast = block.insert(EditableCHIR.Operation.StaticCast(allocated, reference))
      val flag = block.insert(EditableCHIR.Operation.BooleanConstant(true))
      val tuple = block.insert(EditableCHIR.Operation.Tuple(Seq(flag, cast), tupleType))
      val location = block.insert(EditableCHIR.Operation.Allocate(tupleType))
      block.insert(EditableCHIR.Operation.Store(tuple, location))
      val call = block.insert(EditableCHIR.Operation.Apply(p.getFunc("@second").get, Seq(function.params.head)))
      editable.edit(flag.associatedExpr).replace(EditableCHIR.Operation.BooleanConstant(false))
      (!flag.associatedExpr.asInstanceOf[CHIR.Constant].literal.asInstanceOf[CHIR.BoolLiteral].value) shouldBe true
      (call.associatedExpr.asInstanceOf[CHIR.Apply].callee eq p.getFunc("@second").get) shouldBe true
      (function.expressions.exists(_ eq call.associatedExpr)) shouldBe true
      (!function.isInternal) shouldBe true
      val saved = CHIR.fromBytes(editable.toBytes, version)
      saved.getFunc("@first").get.body.get.entryBlock.expressions.size shouldBe block.expressions.size
      (saved.getFunc("@first").get.body.get.blocks.flatMap(_.expressions).exists(_.isInstanceOf[CHIR.Apply])) shouldBe true
    }
  }
}
