package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.chir.v13008.CHIRFormat.*

private[chir] case class FunctionMetadata(body: Long, result: Long, signature: Long, params: Seq[Long],
                                         localCounter: Long, blockCounter: Long, groupCounter: Long,
                                         fastNative: Boolean, wrapper: Boolean, mangledName: String)

private[chir] enum BodyFault {
  case SharedBody, WrongEntry, WrongExpressionOwner, WrongResultDefinition, ForeignParameter
}

/** Only generated-table access differs between the schema adapters. */
private[chir] trait BodyFixture {
  def pkg: EditableCHIR.Package
  def rawBytes: Array[Byte]
  def metadata: FunctionMetadata
  def groupOwner(id: Long): Long
  def isReturn(id: Long): Boolean
  def corrupt(fault: BodyFault): Unit
  final def source: EditableCHIR.Func = pkg.function(1)
  final def group(id: Int): CHIR.BlockGroup = pkg.values.toVector(id - 1).asInstanceOf[CHIR.BlockGroup]
  final def local(id: Int): CHIR.LocalVar = pkg.values.toVector(id - 1).asInstanceOf[CHIR.LocalVar]
}

/** Common constructors, fixture scenarios and schema dispatch for CHIR tests.
  * Wire fixtures use fields shared by all supported schemas. Adapters handle
  * only generated-table decoding and direct access to the backing model.
  */
private[chir] object TestData {
  val versions: Seq[Int] = Seq(1203, 13002, 13008)

  trait Schema {
    type WirePackage
    def decode(bytes: Array[Byte]): WirePackage
    def load(bytes: Array[Byte]): Access
  }

  trait Access {
    def pkg: EditableCHIR.Package
    def rawBytes: Array[Byte]
    def tpe(id: Long): CHIR.Type
    def value(id: Long): CHIR.Value
    def definition(id: Long): CHIR.CustomTypeDef
    def metadata(id: Long): FunctionMetadata
    def groupOwner(id: Long): Long
    def isReturn(id: Long): Boolean
    def setFunctionBody(id: Long, body: Long): Unit
    def setGroupEntry(id: Long, block: Long): Unit
    def setExpressionOwner(id: Long, owner: Long): Unit
    def setLocalDefinition(id: Long, expression: Long): Unit
    def setOperands(id: Long, operands: Seq[Long]): Unit
    def setGenericBounds(id: Long, bounds: Seq[Long]): Unit
    def setArrayDimension(id: Long, dimension: Long): Unit
    def setFixedArraySize(id: Long, size: Long): Unit
    def setFunctionTypeArgs(id: Long, args: Seq[Long]): Unit
    def setFunctionTypeFlags(id: Long, isC: Boolean, hasVarArg: Boolean): Unit
    def setParameter(id: Long, name: String, tpe: Long): Unit
    def setInteger(id: Long, value: Long): Unit
    def setGlobalPackage(id: Long, name: String): Unit
    def setMemberName(id: Long, index: Int, name: String): Unit
    def setMethodName(id: Long, table: Int, index: Int, name: String): Unit
    def setFunctionAttributes(id: Long, attributes: Long): Unit
    def setAutoEnvAnnotation(id: Long, index: Int, value: Boolean): Unit
    def setOverrideAnnotation(id: Long, index: Int, tpe: Long): Unit
    def setSourceFile(id: Long, name: String): Unit
    def setGenericNames(id: Long, identifier: String, sourceName: String): Unit
  }

  private def schema(version: Int): Schema = version match {
    case 1203 => v1203.TestAdapter
    case 13002 => v13002.TestAdapter
    case 13008 => v13008.TestAdapter
    case _ => throw new IllegalArgumentException("Unsupported test schema: " + version)
  }

  val phases: Seq[(Int, String)] = Seq(Phase.RAW -> "RAW", Phase.PLUGIN -> "PLUGIN", Phase.OPT -> "OPT")

  def phaseFixture(version: Int, phase: Int): EditableCHIR.Package = {
    val data = fixtureData()
    data.setPhase(phase)
    schema(version).load(data.serializeToBinary()).pkg
  }

  def storedPhase(bytes: Array[Byte]): Int = CHIRPackageT.deserializeFromBinary(bytes).getPhase

  def fixtureData(s: Schema): s.WirePackage = s.decode(fixtureBytes)
  def operationFixtureData(s: Schema): s.WirePackage = s.decode(operationFixtureBytes)
  def bodyFixture(version: Int): BodyFixture = new Fixture(schema(version).load(fixtureBytes))
  def readFixture(version: Int): Fixture = new Fixture(schema(version).load(readFixtureBytes))

  final class Fixture private[TestData] (access: Access) extends BodyFixture {
    def pkg: EditableCHIR.Package = access.pkg
    def rawBytes: Array[Byte] = access.rawBytes
    def metadata: FunctionMetadata = access.metadata(1)
    def groupOwner(id: Long): Long = access.groupOwner(id)
    def isReturn(id: Long): Boolean = access.isReturn(id)
    def tpe[T <: CHIR.Type](id: Long): T = access.tpe(id).asInstanceOf[T]
    def value[T <: CHIR.Value](id: Long): T = access.value(id).asInstanceOf[T]
    def definition[T <: CHIR.CustomTypeDef](id: Long): T = access.definition(id).asInstanceOf[T]

    def corrupt(fault: BodyFault): Unit = fault match {
      case BodyFault.SharedBody => access.setFunctionBody(8, 5)
      case BodyFault.WrongEntry => access.setGroupEntry(5, 3)
      case BodyFault.WrongExpressionOwner => access.setExpressionOwner(3, 3)
      case BodyFault.WrongResultDefinition => access.setLocalDefinition(7, 1)
      case BodyFault.ForeignParameter => access.setOperands(4, Seq(11L))
    }

    def changeTypeMetadata(): Unit = {
      access.setGenericBounds(11, Seq(6L))
      access.setArrayDimension(12, 3)
      access.setFixedArraySize(13, 9)
      access.setFunctionTypeArgs(5, Seq(6L, 7L))
      access.setFunctionTypeFlags(5, true, true)
    }

    def changeValueMetadata(): Unit = {
      access.setParameter(12, "%renamed", 6)
      access.setInteger(14, 100)
      access.setGlobalPackage(13, "changed")
    }

    def renameMembers(): Unit = {
      access.setMemberName(1, 0, "renamed")
      access.setMethodName(1, 0, 0, "renamedMethod")
    }

    def changeFunctionMetadata(): Unit = {
      access.setFunctionAttributes(1, 0)
      access.setAutoEnvAnnotation(1, 0, false)
      access.setOverrideAnnotation(1, 1, 4)
      access.setSourceFile(1, "changed.cj")
    }

    def useEnumReturns(): Unit = {
      access.setFunctionTypeArgs(5, Seq(2L, 19L))
      access.setFunctionTypeArgs(4, Seq(19L))
    }

    def useGenericSourceName(): Unit = {
      access.setGenericNames(11, "mangled.function.T", "T")
      access.setFunctionTypeArgs(5, Seq(11L, 2L))
    }
  }


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

  private def readFixtureBytes: Array[Byte] = {
    val data = fixtureData()
    def tpe(kind: Int, args: Long*): TypeT = {
      val t = new TypeT; t.setKind(kind); t.setArgTys(args.toArray); t
    }

    def tu(tag: Byte, x: Object): TypeElemUnion = {
      val u = new TypeElemUnion; u.setType(tag); u.setValue(x); u
    }
    val generic = new GenericTypeT; generic.setBase(tpe(CHIRTypeKind.GENERIC)); generic.setIdentifier("T")
    generic.setUpperBounds(Array(2L))
    val raw = new RawArrayTypeT; raw.setBase(tpe(CHIRTypeKind.RAWARRAY, 6)); raw.setDims(2)
    val fixed = new VArrayTypeT; fixed.setBase(tpe(CHIRTypeKind.VARRAY, 6)); fixed.setSize(4)
    def custom(kind: Int, id: Long): TypeElemUnion = {
      val t = new CustomTypeT; t.setBase(tpe(kind)); t.setCustomTypeDef(id); tu(TypeElem.CustomType, t)
    }
    data.setTypes(data.getTypes ++ Array(
      tu(TypeElem.Type, tpe(CHIRTypeKind.INT64)), tu(TypeElem.Type, tpe(CHIRTypeKind.FLOAT64)),
      tu(TypeElem.Type, tpe(CHIRTypeKind.BOOLEAN)), tu(TypeElem.Type, tpe(CHIRTypeKind.RUNE)),
      tu(TypeElem.Type, tpe(CHIRTypeKind.C_STRING)), tu(TypeElem.GenericType, generic),
      tu(TypeElem.RawArrayType, raw), tu(TypeElem.VArrayType, fixed),
      tu(TypeElem.Type, tpe(CHIRTypeKind.C_POINTER, 6)), tu(TypeElem.Type, tpe(CHIRTypeKind.BOXTYPE, 6)),
      tu(TypeElem.Type, tpe(CHIRTypeKind.TUPLE, 6, 2)),
      custom(CHIRTypeKind.CLASS, 1), custom(CHIRTypeKind.STRUCT, 2), custom(CHIRTypeKind.ENUM, 3)))
    def vb(id: Long, name: String): ValueT = {
      val v = newValue(name, ValueKind.LITERAL); v.setType(id); v
    }

    def vu(tag: Byte, x: Object): ValueElemUnion = {
      val u = new ValueElemUnion; u.setType(tag); u.setValue(x); u
    }

    def literal(id: Long, kind: Int): LiteralValueT = {
      val l = new LiteralValueT; l.setBase(vb(id, "")); l.setLiteralKind(kind); l
    }
    val globalBase = new GlobalValueT; globalBase.setBase(vb(6, "@global"))
    globalBase.setPackageName("fixture"); globalBase.setSrcCodeIdentifier("global")
    val global = new GlobalVarT; global.setBase(globalBase); global.setInitializer(14)
    val int = new IntLiteralT; int.setBase(literal(6, ConstantValueKind.INT)); int.setVal(42)
    val float = new FloatLiteralT; float.setBase(literal(7, ConstantValueKind.FLOAT)); float.setVal(1.5)
    val bool = new BoolLiteralT; bool.setBase(literal(8, ConstantValueKind.BOOL)); bool.setVal(true)
    val rune = new RuneLiteralT; rune.setBase(literal(9, ConstantValueKind.RUNE)); rune.setVal(65)
    val string = new StringLiteralT; string.setBase(literal(10, ConstantValueKind.STRING)); string.setVal("text")
    val nul = new NullLiteralT; nul.setBase(literal(2, ConstantValueKind.NULL))
    val unit = new UnitLiteralT; unit.setBase(literal(2, ConstantValueKind.UNIT))
    data.setValues(data.getValues ++ Array(vu(ValueElem.GlobalVar, global), vu(ValueElem.IntLiteral, int),
      vu(ValueElem.FloatLiteral, float), vu(ValueElem.BoolLiteral, bool), vu(ValueElem.RuneLiteral, rune),
      vu(ValueElem.StringLiteral, string), vu(ValueElem.NullLiteral, nul), vu(ValueElem.UnitLiteral, unit)))
    val public = 1L << CHIR.Attribute.Public.ordinal
    def definition(id: Long, name: String): CustomTypeDefT = {
      val d = new CustomTypeDefT; d.setBase(newBase()); d.setType(id)
      d.setIdentifier("@" + name); d.setSrcCodeIdentifier(name); d.setPackageName("fixture"); d
    }
    val c = new ClassDefT; c.setBase(definition(17, "C")); c.setIsClass(true)
    val member = new MemberVarInfoT; member.setName("field"); member.setType(6); member.setAttributes(public)
    c.getBase.setInstanceMemberVars(Array(member)); c.getBase.setStaticMemberVars(Array(13L))
    c.getBase.setMethods(Array(1L)); c.getBase.setImplementedInterfaces(Array(17L))
    val method = new VirtualMethodInfoT; method.setFuncName("method"); method.setSigType(5)
    method.setOriginalType(5); method.setInstance(1); method.setParentType(17); method.setReturnType(2)
    method.setMethodGenericTypeParams(Array(11L)); method.setAttributes(public)
    val table = new VTableInTypeT; table.setSrcParentType(17); table.setVirtualMethods(Array(method))
    c.getBase.setVtable(Array(table))
    val struct = new StructDefT; struct.setBase(definition(18, "S"))
    val enumeration = new EnumDefT; enumeration.setBase(definition(19, "E")); enumeration.setNonExhaustive(true)
    val ctor = new EnumCtorInfoT; ctor.setFuncType(4); enumeration.setCtors(Array(ctor))
    val extension = new ExtendDefT; extension.setBase(definition(6, "Extension"))
    extension.setExtendedType(6); extension.setGenericParams(Array(11L))
    def du(tag: Byte, x: Object): CustomTypeDefElemUnion = {
      val u = new CustomTypeDefElemUnion; u.setType(tag); u.setValue(x); u
    }
    data.setDefs(Array(du(CustomTypeDefElem.ClassDef, c), du(CustomTypeDefElem.StructDef, struct),
      du(CustomTypeDefElem.EnumDef, enumeration), du(CustomTypeDefElem.ExtendDef, extension)))
    val base = newBase(); base.getLoc.setFilePath("fixture.cj"); base.setAttributes(public)
    val auto = new IsAutoEnvClassT; auto.setValue(true)
    val overridden = new OverrideSrcFuncTypeT; overridden.setType(5)
    val wrapped = new WrappedRawMethodT; wrapped.setRawMethod(1)
    def annotation(tag: Byte, x: Object): AnnotationUnion = {
      val u = new AnnotationUnion; u.setType(tag); u.setValue(x); u
    }
    base.setAnnos(Array(annotation(Annotation.isAutoEnvClass, auto),
      annotation(Annotation.overrideSrcFuncType, overridden), annotation(Annotation.wrappedRawMethod, wrapped)))
    data.getValues()(0).asFunction().getBase.getBase.setBase(base)
    data.getValues()(0).asFunction().getBase.setDeclaredParent(1)
    data.serializeToBinary()
  }

  private def newBase(): BaseT = {
    val loc = new DebugLocationT
    loc.setFilePath("")
    loc.setBeginPos(new PosT)
    loc.setEndPos(new PosT)
    loc.setScope(Array.emptyIntArray)
    val base = new BaseT
    base.setLoc(loc)
    base.setAnnos(Array.empty[AnnotationUnion])
    base
  }

  private def newValue(identifier: String, kind: Int): ValueT = {
    val value = new ValueT
    value.setBase(newBase())
    value.setIdentifier(identifier)
    value.setKind(kind)
    value
  }

}
