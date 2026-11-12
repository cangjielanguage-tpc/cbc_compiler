package com.huawei.excelsior.jet.compiler.chir.v13008

import com.huawei.excelsior.jet.compiler.chir.EditableCHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PackageImplSuite extends AnyFunSuite with Matchers {
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
    (from.genericTypeParams.nonEmpty && to.genericTypeParams.isEmpty) shouldBe true
    p.edit(from).moveBody(to)
    (from.body.isEmpty && to.body.nonEmpty) shouldBe true
    to.params.head.tpe shouldBe from.genericTypeParams.head
    val output = p.toBytes
    val saved = PackageImpl.fromBytes(output)
    (saved.function(8).body.nonEmpty) shouldBe true
    (saved.function(8).genericTypeParams.isEmpty) shouldBe true
    saved.function(8).params.head.tpe shouldBe saved.function(1).genericTypeParams.head
  }

}
