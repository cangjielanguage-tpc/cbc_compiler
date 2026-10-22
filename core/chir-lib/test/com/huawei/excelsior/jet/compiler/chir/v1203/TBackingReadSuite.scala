package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*
import org.scalatest.funsuite.AnyFunSuite

/** Verify existing read contracts against the shared mutable object model. */
class TBackingReadSuite extends AnyFunSuite {
  test("type getters read live T fields, including generic and composite types") {
    val p = fixture()
    assert(p.getType[CHIR.CPointerType](14).get.elementType == CHIR.BuiltinType.Int64)
    assert(p.getType[CHIR.BoxType](15).get.baseType == CHIR.BuiltinType.Int64)
    assert(p.getType[CHIR.TupleType](16).get.fieldTypes == Seq(CHIR.BuiltinType.Int64, CHIR.BuiltinType.Unit))
    val generic = p.getType[CHIR.GenericType](11).get
    val array = p.getType[CHIR.RawArrayType](12).get
    val fixed = p.getType[CHIR.VArrayType](13).get
    val function = p.getType[CHIR.FuncType](5).get
    assert(generic.identifier == "T" && generic.upperBounds == Seq(CHIR.BuiltinType.Unit))
    assert(array.dimension == 2 && fixed.size == 4)
    assert(array.elementType == CHIR.BuiltinType.Int64 && fixed.elementType == array.elementType)
    assert(function.paramTypes == Seq(CHIR.BuiltinType.Unit))
    assert(function.paramTypesWithoutReceiver.isEmpty && function.receiverType == CHIR.BuiltinType.Unit)
    assert(function.returnType == CHIR.BuiltinType.Unit && !function.isC && !function.hasVarArg)
    p.data.getTypes()(10).asGenericType().setUpperBounds(Array(6L))
    p.data.getTypes()(11).asRawArrayType().setDims(3)
    p.data.getTypes()(12).asVArrayType().setSize(9)
    p.data.getTypes()(4).asFuncType().getBase.setArgTys(Array(6L, 7L))
    p.data.getTypes()(4).asFuncType().setIsCfuncType(true)
    p.data.getTypes()(4).asFuncType().setHasVarArg(true)
    assert(generic.upperBounds == Seq(CHIR.BuiltinType.Int64))
    assert(array.dimension == 3 && fixed.size == 9)
    assert(function.paramTypes == Seq(CHIR.BuiltinType.Int64))
    assert(function.returnType == CHIR.BuiltinType.Float64 && function.isC && function.hasVarArg)
  }

  test("literal, parameter and global getters use the same mutable T nodes") {
    val p = fixture()
    val global = p.getValue[CHIR.GlobalVar](13).get
    assert(global.id == 13 && global.identifier == "@global" && global.srcCodeIdentifier == "global")
    assert(global.packageName == "fixture" && global.tpe == CHIR.BuiltinType.Int64)
    assert(global.initializer.get eq p.getValue[CHIR.IntLiteral](14).get)
    assert(p.getValue[CHIR.IntLiteral](14).get.value == 42)
    assert(p.getValue[CHIR.FloatLiteral](15).get.value == 1.5)
    assert(p.getValue[CHIR.BoolLiteral](16).get.value)
    assert(p.getValue[CHIR.RuneLiteral](17).get.value == 65)
    assert(p.getValue[CHIR.StringLiteral](18).get.value == "text")
    assert(p.getValue[CHIR.NullLiteral](19).get.tpe == CHIR.BuiltinType.Unit)
    assert(p.getValue[CHIR.UnitLiteralValue](20).get.tpe == CHIR.BuiltinType.Unit)
    val parameter = p.function(1).params.head
    p.data.getValues()(11).asParameter().getBase.setIdentifier("%renamed")
    p.data.getValues()(11).asParameter().getBase.setType(6)
    p.data.getValues()(13).asIntLiteral().setVal(100)
    p.data.getValues()(12).asGlobalVar().getBase.setPackageName("changed")
    assert(parameter.identifier == "%renamed" && parameter.tpe == CHIR.BuiltinType.Int64)
    assert(p.getValue[CHIR.IntLiteral](14).get.value == 100)
    assert(global.packageName == "changed")
  }

  test("definition, member, vtable and enum constructor read APIs remain available") {
    val p = fixture()
    val cls = p.getDef[CHIR.ClassDef](1).get
    assert(cls.identifier == "@C" && cls.srcCodeIdentifier == "C" && cls.packageName == "fixture")
    assert(cls.isClass && cls.superClass.isEmpty)
    assert(cls.tpe.typeDef eq cls)
    assert(p.getDef("C").contains(cls) && p.getDef("@C").contains(cls))
    assert(cls.instanceVars.head.name == "field" && cls.instanceVars.head.tpe == CHIR.BuiltinType.Int64)
    assert(cls.instanceVars.head.attributes.contains(CHIR.Attribute.Public))
    assert(cls.staticVars.head eq p.getValue[CHIR.GlobalVar](13).get)
    assert(cls.methods.head eq p.function(1))
    assert(cls.implementedInterfaces.head eq cls.tpe)
    val table = cls.vtables.head
    val method = table.vMethods.head
    assert(table.srcParentType eq cls.tpe)
    assert(method.name == "method" && method.instance == p.function(1))
    assert(method.tpe eq p.function(1).tpe)
    assert(method.originalType == method.tpe && method.parentType == cls.tpe)
    assert(method.returnType == CHIR.BuiltinType.Unit)
    assert(method.genericTypeParams.head eq p.getType[CHIR.GenericType](11).get)
    assert(method.attributes.contains(CHIR.Attribute.Public))
    val struct = p.getDef[CHIR.StructDef](2).get
    val enumeration = p.getDef[CHIR.EnumDef](3).get
    val extension = p.getDef[CHIR.ExtendDef](4).get
    assert(struct.tpe.typeDef eq struct)
    assert(enumeration.tpe.typeDef eq enumeration)
    assert(enumeration.nonExhaustive && enumeration.ctors.head.tpe.returnType == CHIR.BuiltinType.Unit)
    assert(extension.tpe == CHIR.BuiltinType.Int64 && extension.genericTypeParams.head.identifier == "T")
    val heldField = cls.instanceVars.head
    p.data.getDefs()(0).asClassDef().getBase.getInstanceMemberVars()(0).setName("renamed")
    p.data.getDefs()(0).asClassDef().getBase.getVtable()(0).getVirtualMethods()(0).setFuncName("renamedMethod")
    assert(heldField.name == "renamed" && method.name == "renamedMethod")
  }

  test("function source, attributes and annotation getters observe T changes") {
    val p = fixture()
    val f = p.function(1)
    assert(f.sourceFile.contains("fixture.cj"))
    assert(f.declaringDef.contains(p.getDef[CHIR.ClassDef](1).get))
    assert(f.attributes.contains(CHIR.Attribute.Public))
    val auto = f.annotations.collectFirst { case x: CHIR.IsAutoEnvClass => x }.get
    val overridden = f.annotations.collectFirst { case x: CHIR.OverrideSrcFuncType => x }.get
    val wrapped = f.annotations.collectFirst { case x: CHIR.WrappedRawMethod => x }.get
    assert(auto.value && overridden.tpe == f.tpe && wrapped.rawMethod == f)
    val base = p.data.getValues()(0).asFunction().getBase.getBase.getBase
    base.setAttributes(0)
    base.getAnnos()(0).asisAutoEnvClass().setValue(false)
    base.getAnnos()(1).asoverrideSrcFuncType().setType(4)
    base.getLoc.setFilePath("changed.cj")
    assert(f.attributes.isEmpty && !auto.value)
    assert(overridden.tpe.paramTypes.isEmpty && f.sourceFile.contains("changed.cj"))
  }

  private def fixture(): PackageImpl = {
    val data = TestData.fixtureData()
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
      val v = PackageImpl.newValue(name, ValueKind.LITERAL); v.setType(id); v
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
      val d = new CustomTypeDefT; d.setBase(PackageImpl.newBase()); d.setType(id)
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
    val base = PackageImpl.newBase(); base.getLoc.setFilePath("fixture.cj"); base.setAttributes(public)
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
    PackageImpl.fromBytes(data.serializeToBinary())
  }
}
