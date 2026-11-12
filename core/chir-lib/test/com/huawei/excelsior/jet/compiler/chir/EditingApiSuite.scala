package com.huawei.excelsior.jet.compiler.chir

import org.scalatest.funsuite.AnyFunSuite

/** Run shared editing contracts against every schema backend. */
class EditingApiSuite extends AnyFunSuite {
  private def fixture(version: Int): EditableCHIR.Package = {
    val editable = EditableCHIR.empty("fixture", version)
    val function = editable.newFunction("@first", Seq(CHIR.BuiltinType.Unit), CHIR.BuiltinType.Unit)
    editable.newFunction("@second", Seq(CHIR.BuiltinType.Unit), CHIR.BuiltinType.Unit)
    val block = editable.newBlock()
    block.replaceTerminator(editable.newExit())
    val body = editable.newBlockGroup(Seq(block), block)
    function.replaceBody(Some(body), None)
    val result = block.insert(EditableCHIR.Operation.Allocate(CHIR.BuiltinType.Unit))
    function.replaceBody(Some(body), Some(result))
    editable
  }

  private val backends = Seq(1203, 13002, 13008).map(version => version -> (() => fixture(version)))

  for ((version, fresh) <- backends) {
    test(s"$version editable nodes retain identity when viewed through the read API") {
      val editable = fresh()
      val function: EditableCHIR.Func = editable.getFunc("@first").get
      val readFunction: CHIR.Func = editable.getFunc("@first").get
      val group: EditableCHIR.BlockGroup = function.body.get
      val block: EditableCHIR.Block = group.entryBlock
      val expression: EditableCHIR.Expression = block.terminator
      assert(function eq readFunction)
      assert(editable.edit(readFunction) eq function)
      assert(editable.edit(readFunction.body.get) eq group)
      assert(editable.edit(readFunction.body.get.entryBlock) eq block)
      assert(editable.edit(readFunction.body.get.entryBlock.terminator) eq expression)
      block.replaceTerminator(editable.newGoto(block))
      assert(readFunction.body.get.entryBlock.terminator.asInstanceOf[CHIR.Goto].destination eq block)
      function.removeBody()
      assert(readFunction.body.isEmpty)
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
      assert(editable.getFunc("@first").get.body.nonEmpty)
    }

    test(s"$version rejects a foreign goto destination without changing the body") {
      val editable = fresh()
      val p = editable
      val block = p.getFunc("@first").get.body.get.entryBlock
      val before = block.expressions
      intercept[IllegalArgumentException](editable.newGoto(fresh().getFunc("@first").get.body.get.entryBlock))
      assert(block.expressions == before)
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
      assert(function.body == body)
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
      assert(function.retVal == result)
    }

    test(s"$version rejects synthetic functions without evaluating their generated body") {
      val editable = fresh()
      val function = CHIR.newIntrinsicsPackage.values.collectFirst { case f: CHIR.Func => f }.get
      assert(function.params.isEmpty)
      intercept[IllegalArgumentException](editable.edit(function).removeBody())
      intercept[IllegalArgumentException](editable.edit(function).replaceAttributes(Seq(CHIR.Attribute.Public)))
    }

    test(s"$version function and global attributes update held views and survive serialization") {
      val editable = fresh()
      val function = editable.getFunc("@first").get
      val global = editable.newGlobalVar("@global", CHIR.BuiltinType.Unit)
      editable.edit(function).replaceAttributes(Seq(CHIR.Attribute.Public, CHIR.Attribute.NoInline, CHIR.Attribute.Public))
      global.replaceAttributes(Seq(CHIR.Attribute.Private, CHIR.Attribute.Mut))
      assert(function.attributes == Seq(CHIR.Attribute.Public, CHIR.Attribute.NoInline))
      assert(global.attributes == Seq(CHIR.Attribute.Private, CHIR.Attribute.Mut))
      val saved = EditableCHIR.fromBytes(editable.toBytes, version)
      assert(saved.getFunc("@first").get.attributes == function.attributes)
      val savedGlobal = saved.values.collectFirst { case g: CHIR.GlobalVar => g }.get
      assert(savedGlobal.attributes == global.attributes)
      saved.edit(savedGlobal).replaceAttributes(Seq.empty)
      assert(savedGlobal.attributes.isEmpty)
      assert(global.attributes.nonEmpty)
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
      assert(from.body.isEmpty && to.body.contains(body))
      assert(to.tpe eq signature)
      assert(to.params == parameters)
      val saved = CHIR.fromBytes(editable.toBytes, version)
      assert(saved.getFunc("@first").get.body.isEmpty)
      assert(saved.getFunc("@second").get.body.nonEmpty)
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
      assert(!flag.associatedExpr.asInstanceOf[CHIR.Constant].literal.asInstanceOf[CHIR.BoolLiteral].value)
      assert(call.associatedExpr.asInstanceOf[CHIR.Apply].callee eq p.getFunc("@second").get)
      assert(function.expressions.exists(_ eq call.associatedExpr))
      assert(!function.isInternal)
      val saved = CHIR.fromBytes(editable.toBytes, version)
      assert(saved.getFunc("@first").get.body.get.entryBlock.expressions.size == block.expressions.size)
      assert(saved.getFunc("@first").get.body.get.blocks.flatMap(_.expressions).exists(_.isInstanceOf[CHIR.Apply]))
    }
  }
}
