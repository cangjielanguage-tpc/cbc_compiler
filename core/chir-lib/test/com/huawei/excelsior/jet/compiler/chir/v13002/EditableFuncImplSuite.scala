package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.{CHIR, EditableCHIR}
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*
import org.scalatest.funsuite.AnyFunSuite

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.file.Files
import java.util.IdentityHashMap

class EditableFuncImplSuite extends AnyFunSuite {
  test("replaceBody updates the IR view, ownership, return flags and allocation counters") {
    withFixture { f =>
      assert(f.editable.body.get eq f.groups(2L))
      assert(f.editable.retVal.get eq f.locals(4L))
      f.editable.replaceBody(Some(f.groups(5L)), Some(f.locals(7L)))
      assert(f.editable.body.get eq f.groups(5L))
      assert(f.editable.retVal.get eq f.locals(7L))
      val fn = f.data.getValues()(0).asFunction()
      assert(fn.getBody == 5 && fn.getRetVal == 7)
      assert(f.data.getValues()(1).asBlockGroup().getOwnedFunc == 0)
      assert(f.data.getValues()(4).asBlockGroup().getOwnedFunc == 1)
      assert(!f.data.getValues()(3).asLocalVar().getIsRetVal)
      assert(f.data.getValues()(6).asLocalVar().getIsRetVal)
      assert(fn.getLocalId == 21 && fn.getBlockId == 12 && fn.getBlockGroupId == 8)
      // The original input view still sees the old body; the editable wrapper
      // must resolve the backing IDs instead of delegating these two getters.
      assert(f.original.body.get.blocks.head.expressions.nonEmpty)
      assert(f.original.retVal.get eq f.locals(4L))
    }
  }

  test("replacement survives FlatBuffers serialization and a chirLib reload") {
    withFixture { f =>
      val signature = f.data.getValues()(0).asFunction().getBase.getBase.getType
      val paramIds = f.data.getValues()(0).asFunction().getParams.clone()
      f.editable.replaceBody(Some(f.groups(5L)), Some(f.locals(7L)))
      val bytes = f.data.serializeToBinary()
      val saved = CHIRPackageT.deserializeFromBinary(bytes)
      val fn = saved.getValues()(0).asFunction()
      assert(fn.getBody == 5 && fn.getRetVal == 7)
      assert(fn.getBase.getBase.getType == signature)
      assert(fn.getParams.sameElements(paramIds))
      assert(fn.getIsFastNative && fn.getIsCffiwrapper)
      assert(fn.getBase.getRawMangledName == "preserve-this-metadata")
      val output = f.directory.resolve("output.chir")
      Files.write(output, bytes)
      val reloaded = new PackageImpl(output.toString).function(1)
      assert(reloaded.body.get.blocks.size == 1)
      assert(reloaded.retVal.get.identifier == "%20")
    }
  }

  test("clearing the body produces a declaration and detaches previous metadata") {
    withFixture { f =>
      f.editable.replaceBody(None, None)
      assert(f.editable.body.isEmpty && f.editable.retVal.isEmpty)
      assert(f.data.getValues()(1).asBlockGroup().getOwnedFunc == 0)
      assert(!f.data.getValues()(3).asLocalVar().getIsRetVal)
      val fn = CHIRPackageT.deserializeFromBinary(f.data.serializeToBinary()).getValues()(0).asFunction()
      assert(fn.getBody == 0 && fn.getRetVal == 0)
    }
  }

  test("replacing a body with itself is idempotent") {
    withFixture { f =>
      val before = f.data.serializeToBinary()
      f.editable.replaceBody(Some(f.groups(2L)), Some(f.locals(4L)))
      assert(f.data.serializeToBinary().sameElements(before))
    }
  }

  test("reject another function's body without partial mutation") {
    withFixture { f =>
      unchangedOnFailure(f) {
        f.editable.replaceBody(Some(f.groups(9L)), None)
      }
    }
  }

  test("reject a body referenced by another function even if its owner field is empty") {
    withFixture { f =>
      f.data.getValues()(7).asFunction().setBody(5L)
      unchangedOnFailure(f) {
        f.editable.replaceBody(Some(f.groups(5L)), Some(f.locals(7L)))
      }
    }
  }

  test("reject a return local defined outside the replacement body") {
    withFixture { f =>
      unchangedOnFailure(f) {
        f.editable.replaceBody(Some(f.groups(5L)), Some(f.locals(4L)))
      }
    }
  }

  test("reject a declaration with a return local") {
    withFixture { f =>
      unchangedOnFailure(f) { f.editable.replaceBody(None, Some(f.locals(4L))) }
    }
  }

  test("reject a cross-package group before looking up its ID") {
    withFixture { f =>
      val foreignOwner = stub(classOf[EditableCHIR.Package]) { (_, _) =>
        throw new AssertionError("Foreign package must not be queried")
      }
      val foreignGroup = groupView(foreignOwner)
      unchangedOnFailure(f) { f.editable.replaceBody(Some(foreignGroup), None) }
    }
  }

  test("reject unregistered return locals without mutation") {
    withFixture { f =>
      val foreignLocal = stub(classOf[CHIR.LocalVar]) { (_, _) =>
        throw new AssertionError("Foreign local must not be queried")
      }
      unchangedOnFailure(f) {
        f.editable.replaceBody(Some(f.groups(5L)), Some(foreignLocal))
      }
    }
  }

  test("reject inconsistent entry blocks, expression ownership and result links") {
    withFixture { f =>
      f.data.getValues()(4).asBlockGroup().setEntryBlock(3L)
      unchangedOnFailure(f) { f.editable.replaceBody(Some(f.groups(5L)), Some(f.locals(7L))) }
      f.data.getValues()(4).asBlockGroup().setEntryBlock(6L)
      f.data.getExprs()(2).asAllocateBase().getBase.setOwner(3L)
      unchangedOnFailure(f) { f.editable.replaceBody(Some(f.groups(5L)), Some(f.locals(7L))) }
      f.data.getExprs()(2).asAllocateBase().getBase.setOwner(6L)
      f.data.getValues()(6).asLocalVar().setAssociatedExpr(1L)
      unchangedOnFailure(f) { f.editable.replaceBody(Some(f.groups(5L)), Some(f.locals(7L))) }
    }
  }

  test("reject operands referencing another function's parameters") {
    withFixture { f =>
      f.data.getExprs()(3).asExpression().setOperands(Array(11L))
      unchangedOnFailure(f) { f.editable.replaceBody(Some(f.groups(5L)), Some(f.locals(7L))) }
    }
  }

  private def unchangedOnFailure(f: Fixture)(action: => Unit): Unit = {
    val before = f.data.serializeToBinary()
    intercept[IllegalArgumentException](action)
    assert(f.data.serializeToBinary().sameElements(before))
  }

  private class Fixture(val directory: java.nio.file.Path) {
    val data = EditableTestData.fixtureData()
    private val input = directory.resolve("input.chir")
    Files.write(input, data.serializeToBinary())
    private val reader = new PackageImpl(input.toString)
    val original: CHIR.Func = reader.function(1)
    private val registered = new IdentityHashMap[CHIR.Value, java.lang.Long]()
    val owner: EditableCHIR.Package = stub(classOf[EditableCHIR.Package]) { (method, args) =>
      method.getName match {
        case "formatVersion" => Int.box(13002)
        case "valueId" =>
          val node = args(0).asInstanceOf[CHIR.Value]
          require(registered.containsKey(node), "Unregistered node")
          registered.get(node)
        case _ => throw new AssertionError(s"Unexpected package operation: $method")
      }
    }
    val groups: Map[Long, EditableCHIR.BlockGroup] = Seq(2L, 5L, 9L).map { id =>
      val group = groupView(owner)
      registered.put(group, Long.box(id))
      id -> group
    }.toMap
    val locals: Map[Long, CHIR.LocalVar] = Seq(4L, 7L).map { id =>
      val local = reader.getValue[CHIR.LocalVar](id).get
      registered.put(local, Long.box(id))
      id -> local
    }.toMap
    val editable = new EditableFuncImpl(original, owner, data, groups.apply, locals.apply)
  }

  private def withFixture(action: Fixture => Unit): Unit = {
    val directory = Files.createTempDirectory("chir-replace-body-")
    try action(new Fixture(directory))
    finally {
      Files.deleteIfExists(directory.resolve("input.chir"))
      Files.deleteIfExists(directory.resolve("output.chir"))
      Files.delete(directory)
    }
  }

  // Test doubles isolate the implemented function wrapper from the still-pending
  // editable package loader. Production graph state is real generated schema data.
  private def groupView(owner: EditableCHIR.Package): EditableCHIR.BlockGroup =
    stub(classOf[EditableCHIR.BlockGroup]) { (method, _) =>
      require(method.getName == "ownerPackage", s"Unexpected group operation: $method")
      owner
    }

  private def stub[T](kind: Class[T])(call: (Method, Array[Object]) => Object): T =
    Proxy.newProxyInstance(kind.getClassLoader, Array[Class[?]](kind), new InvocationHandler {
      def invoke(proxy: Object, method: Method, args: Array[Object]): Object = call(method, args)
    }).asInstanceOf[T]

}

private[v13002] object EditableTestData {
  def fixtureData(): CHIRPackageT = {
    def base(tpe: Long, name: String, kind: Int): ValueT = {
      val b = new ValueT
      b.setBase(new BaseT)
      b.setType(tpe)
      b.setIdentifier(name)
      b.setKind(kind)
      b
    }
    def union(tag: Byte, node: Object): ValueElemUnion = {
      val u = new ValueElemUnion; u.setType(tag); u.setValue(node); u
    }
    def function(name: String, body: Long, ret: Long, param: Long): FunctionT = {
      val gv = new GlobalValueT; gv.setBase(base(5, name, ValueKind.FUNC))
      gv.setSrcCodeIdentifier(name); gv.setPackageName("fixture")
      gv.setRawMangledName("preserve-this-metadata")
      val f = new FunctionT; f.setBase(gv); f.setBody(body); f.setRetVal(ret)
      f.setParams(Array(param)); f.setLocalId(5); f.setBlockId(4); f.setBlockGroupId(2)
      f.setIsFastNative(true); f.setIsCffiwrapper(true)
      f
    }
    def group(name: String, owner: Long, block: Long): BlockGroupT = {
      val g = new BlockGroupT; g.setBase(base(0, name, ValueKind.BLOCK_GROUP))
      g.setOwnedFunc(owner); g.setBlocks(Array(block)); g.setEntryBlock(block); g
    }
    def block(name: String, parent: Long, exprs: Array[Long]): BlockT = {
      val b = new BlockT; b.setBase(base(0, name, ValueKind.BLOCK))
      b.setParentGroup(parent); b.setExprs(exprs); b
    }
    def local(name: String, expr: Long, ret: Boolean): LocalVarT = {
      val l = new LocalVarT; l.setBase(base(3, name, ValueKind.LOCALVAR))
      l.setAssociatedExpr(expr); l.setIsRetVal(ret); l
    }
    def param(owner: Long): ParameterT = {
      val p = new ParameterT; p.setBase(base(2, "%0", ValueKind.PARAMETER))
      p.setOwnedFunc(owner); p
    }
    def expression(owner: Long, result: Long, allocate: Boolean): ExpressionElemUnion = {
      val e = new ExpressionT; e.setBase(new BaseT); e.setOwner(owner)
      e.setKind(if (allocate) CHIRExprKind.Allocate else CHIRExprKind.Exit)
      e.setResultLocalVar(result); e.setResultTy(if (allocate) 3L else 0L)
      val u = new ExpressionElemUnion
      if (allocate) {
        val a = new AllocateBaseT; a.setBase(e); a.setAllocatedType(2L)
        u.setType(ExpressionElem.AllocateBase); u.setValue(a)
      } else { u.setType(ExpressionElem.Expression); u.setValue(e) }
      u
    }
    def tpe(kind: Int, args: Array[Long] = Array.emptyLongArray): TypeElemUnion = {
      val t = new TypeT; t.setKind(kind); t.setArgTys(args)
      val u = new TypeElemUnion
      if (kind == CHIRTypeKind.FUNC) {
        val f = new FuncTypeT; f.setBase(t); u.setType(TypeElem.FuncType); u.setValue(f)
      } else { u.setType(TypeElem.Type); u.setValue(t) }
      u
    }
    val p = new CHIRPackageT
    p.setName("fixture")
    p.setTypes(Array(tpe(CHIRTypeKind.VOID), tpe(CHIRTypeKind.UNIT),
      tpe(CHIRTypeKind.REFTYPE, Array(2L)), tpe(CHIRTypeKind.FUNC, Array(2L)),
      tpe(CHIRTypeKind.FUNC, Array(2L, 2L))))
    p.setValues(Array(
      union(ValueElem.Function, function("@first", 2, 4, 12)),
      union(ValueElem.BlockGroup, group("0", 1, 3)),
      union(ValueElem.Block, block("#0", 2, Array(1L, 2L))),
      union(ValueElem.LocalVar, local("%1", 1, true)),
      union(ValueElem.BlockGroup, group("7", 0, 6)),
      union(ValueElem.Block, block("#11", 5, Array(3L, 4L))),
      union(ValueElem.LocalVar, local("%20", 3, false)),
      union(ValueElem.Function, function("@second", 9, 0, 11)),
      union(ValueElem.BlockGroup, group("0", 8, 10)),
      union(ValueElem.Block, block("#0", 9, Array(5L))),
      union(ValueElem.Parameter, param(8)), union(ValueElem.Parameter, param(1))))
    p.setExprs(Array(expression(3, 4, true), expression(3, 0, false),
      expression(6, 7, true), expression(6, 0, false), expression(10, 0, false)))
    p
  }
}
