package com.huawei.excelsior.jet.compiler.chir

import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite

/** Run shared editing contracts against every schema backend. */
class EditingApiSuite extends AnyFunSuite {
  private val backends: Seq[(Int, () => CHIR.Package)] = Seq(
    1203 -> (() => v1203.PackageImpl.fromBytes(v1203.TestData.operationFixtureData().serializeToBinary())),
    13002 -> (() => v13002.PackageImpl.fromBytes(v13002.TestData.operationFixtureData().serializeToBinary())),
    13008 -> (() => v13008.PackageImpl.fromBytes(v13008.TestData.operationFixtureData().serializeToBinary()))
  )

  for ((version, fresh) <- backends) {
    test(s"$version rejects a foreign goto destination without changing the body") {
      val p = fresh()
      val block = p.function(1).body.get.entryBlock
      val before = block.expressions
      intercept[IllegalArgumentException](p.newGoto(fresh().function(1).body.get.entryBlock))
      assert(block.expressions == before)
    }

    test(s"$version moves a body through held views and preserves the destination signature") {
      val p = fresh()
      val from = p.function(1)
      val to = p.function(8)
      val body = from.body.get
      val signature = to.tpe
      val parameters = to.params
      from.moveBody(to)
      assert(from.body.isEmpty && to.body.contains(body))
      assert(to.tpe eq signature)
      assert(to.params == parameters)
      val output = Files.createTempFile("chir-api-move-", ".chir")
      try {
        p.write(output)
        val saved = CHIR.read(output, version)
        assert(saved.function(1).body.isEmpty)
        assert(saved.function(8).body.nonEmpty)
      } finally Files.delete(output)
    }

    test(s"$version supports every transformation operation and saving") {
      val p = fresh()
      val function = p.function(1)
      val block = function.body.get.entryBlock
      val reference = new CHIR.RefType { def baseType: CHIR.Type = CHIR.BuiltinType.Unit }
      val tupleType = new CHIR.TupleType {
        def fieldTypes: Seq[CHIR.Type] = Seq(CHIR.BuiltinType.Boolean, reference)
      }
      val allocated = p.insert(block, CHIR.Operation.Allocate(CHIR.BuiltinType.Unit))
      val cast = p.insert(block, CHIR.Operation.StaticCast(allocated, reference))
      val flag = p.insert(block, CHIR.Operation.BooleanConstant(true))
      val tuple = p.insert(block, CHIR.Operation.Tuple(Seq(flag, cast), tupleType))
      val location = p.insert(block, CHIR.Operation.Allocate(tupleType))
      p.insert(block, CHIR.Operation.Store(tuple, location))
      val call = p.insert(block, CHIR.Operation.Apply(p.function(8), Seq(function.params.head)))
      p.replace(flag.associatedExpr, CHIR.Operation.BooleanConstant(false))
      assert(!flag.associatedExpr.asInstanceOf[CHIR.Constant].literal.asInstanceOf[CHIR.BoolLiteral].value)
      assert(call.associatedExpr.asInstanceOf[CHIR.Apply].callee eq p.function(8))
      assert(p.expressions(function).exists(_ eq call.associatedExpr))
      assert(!p.isInternal(function))
      p.setOptimized()
      val output = Files.createTempFile("chir-api-operations-", ".chir")
      try {
        p.write(output)
        val saved = CHIR.read(output, version)
        assert(saved.function(1).body.get.entryBlock.expressions.size == block.expressions.size)
        assert(saved.expressions(saved.function(1)).exists(_.isInstanceOf[CHIR.Apply]))
      } finally Files.delete(output)
    }
  }
}
