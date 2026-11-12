package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.chir.v13008.CHIRFormat.*

/** A wire image built from fields shared by every supported schema.
  * Backend fixtures deserialize the bytes with their own generated tables.
  */
private[chir] object SharedTestData {
  def editableFixture(version: Int): EditableCHIR.Package = {
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

  def fixtureBytes: Array[Byte] = fixtureData().serializeToBinary()
  def operationFixtureBytes: Array[Byte] = operationFixtureData().serializeToBinary()

  private def operationFixtureData(): CHIRPackageT = {
    val p = fixtureData()
    def entry(kind: Int, args: Array[Long]): TypeElemUnion = {
      val t = new TypeT; t.setKind(kind); t.setArgTys(args)
      val u = new TypeElemUnion; u.setType(TypeElem.Type); u.setValue(t)
      u
    }
    p.setTypes(p.getTypes ++ Array(entry(CHIRTypeKind.BOOLEAN, Array.emptyLongArray),
      entry(CHIRTypeKind.TUPLE, Array(6L, 3L))))
    p
  }

  private def fixtureData(): CHIRPackageT = {
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
