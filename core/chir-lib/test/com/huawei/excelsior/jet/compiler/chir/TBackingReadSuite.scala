package com.huawei.excelsior.jet.compiler.chir

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Verify existing read contracts against the shared mutable object model. */
class TBackingReadSuite extends AnyFunSuite with Matchers {
  for (version <- TestData.versions) {
    test(s"$version type getters read live T fields, including generic and composite types") {
      val p = TestData.readFixture(version)
      p.tpe[CHIR.CPointerType](14).elementType shouldBe CHIR.BuiltinType.Int64
      p.tpe[CHIR.BoxType](15).baseType shouldBe CHIR.BuiltinType.Int64
      p.tpe[CHIR.TupleType](16).fieldTypes shouldBe Seq(CHIR.BuiltinType.Int64, CHIR.BuiltinType.Unit)
      val generic = p.tpe[CHIR.GenericType](11)
      val array = p.tpe[CHIR.RawArrayType](12)
      val fixed = p.tpe[CHIR.VArrayType](13)
      val function = p.tpe[CHIR.FuncType](5)
      (generic.identifier == "T" && generic.upperBounds == Seq(CHIR.BuiltinType.Unit)) shouldBe true
      (array.dimension == 2 && fixed.size == 4) shouldBe true
      (array.elementType == CHIR.BuiltinType.Int64 && fixed.elementType == array.elementType) shouldBe true
      function.paramTypes shouldBe Seq(CHIR.BuiltinType.Unit)
      (function.paramTypesWithoutReceiver.isEmpty && function.receiverType == CHIR.BuiltinType.Unit) shouldBe true
      (function.returnType == CHIR.BuiltinType.Unit && !function.isC && !function.hasVarArg) shouldBe true
      p.changeTypeMetadata()
      generic.upperBounds shouldBe Seq(CHIR.BuiltinType.Int64)
      (array.dimension == 3 && fixed.size == 9) shouldBe true
      function.paramTypes shouldBe Seq(CHIR.BuiltinType.Int64)
      (function.returnType == CHIR.BuiltinType.Float64 && function.isC && function.hasVarArg) shouldBe true
    }

    test(s"$version literal, parameter and global getters use the same mutable T nodes") {
      val p = TestData.readFixture(version)
      val global = p.value[CHIR.GlobalVar](13)
      (global.id == 13 && global.identifier == "@global" && global.srcCodeIdentifier == "global") shouldBe true
      (global.packageName == "fixture" && global.tpe == CHIR.BuiltinType.Int64) shouldBe true
      (global.initializer.get eq p.value[CHIR.IntLiteral](14)) shouldBe true
      p.value[CHIR.IntLiteral](14).value shouldBe 42
      p.value[CHIR.FloatLiteral](15).value shouldBe 1.5
      (p.value[CHIR.BoolLiteral](16).value) shouldBe true
      p.value[CHIR.RuneLiteral](17).value shouldBe 65
      p.value[CHIR.StringLiteral](18).value shouldBe "text"
      p.value[CHIR.NullLiteral](19).tpe shouldBe CHIR.BuiltinType.Unit
      p.value[CHIR.UnitLiteralValue](20).tpe shouldBe CHIR.BuiltinType.Unit
      val parameter = p.pkg.function(1).params.head
      p.changeValueMetadata()
      (parameter.identifier == "%renamed" && parameter.tpe == CHIR.BuiltinType.Int64) shouldBe true
      p.value[CHIR.IntLiteral](14).value shouldBe 100
      global.packageName shouldBe "changed"
    }

    test(s"$version definition, member, vtable and enum constructor read APIs remain available") {
      val p = TestData.readFixture(version)
      val cls = p.definition[CHIR.ClassDef](1)
      (cls.identifier == "@C" && cls.srcCodeIdentifier == "C" && cls.packageName == "fixture") shouldBe true
      (cls.isClass && cls.superClass.isEmpty) shouldBe true
      (cls.tpe.typeDef eq cls) shouldBe true
      (p.pkg.getDef("C").contains(cls) && p.pkg.getDef("@C").contains(cls)) shouldBe true
      (cls.instanceVars.head.name == "field" && cls.instanceVars.head.tpe == CHIR.BuiltinType.Int64) shouldBe true
      (cls.instanceVars.head.attributes.contains(CHIR.Attribute.Public)) shouldBe true
      (cls.staticVars.head eq p.value[CHIR.GlobalVar](13)) shouldBe true
      (cls.methods.head eq p.pkg.function(1)) shouldBe true
      (cls.implementedInterfaces.head eq cls.tpe) shouldBe true
      val table = cls.vtables.head
      val method = table.vMethods.head
      (table.srcParentType eq cls.tpe) shouldBe true
      (method.name == "method" && method.instance == p.pkg.function(1)) shouldBe true
      (method.tpe eq p.pkg.function(1).tpe) shouldBe true
      (method.originalType == method.tpe && method.parentType == cls.tpe) shouldBe true
      method.returnType shouldBe CHIR.BuiltinType.Unit
      (method.genericTypeParams.head eq p.tpe[CHIR.GenericType](11)) shouldBe true
      (method.attributes.contains(CHIR.Attribute.Public)) shouldBe true
      val struct = p.definition[CHIR.StructDef](2)
      val enumeration = p.definition[CHIR.EnumDef](3)
      val extension = p.definition[CHIR.ExtendDef](4)
      (struct.tpe.typeDef eq struct) shouldBe true
      (enumeration.tpe.typeDef eq enumeration) shouldBe true
      (enumeration.nonExhaustive && enumeration.ctors.head.tpe.returnType == CHIR.BuiltinType.Unit) shouldBe true
      (extension.tpe == CHIR.BuiltinType.Int64 && extension.genericTypeParams.head.identifier == "T") shouldBe true
      val heldField = cls.instanceVars.head
      p.pkg.edit(heldField).replaceAttributes(Seq(CHIR.Attribute.Private))
      p.pkg.edit(method).replaceAttributes(Seq.empty)
      p.pkg.edit(cls).replaceAttributes(Seq(CHIR.Attribute.Public))
      p.pkg.edit(cls).replaceMethods(Seq.empty)
      heldField.attributes shouldBe Seq(CHIR.Attribute.Private)
      (method.attributes.isEmpty) shouldBe true
      (cls.attributes == Seq(CHIR.Attribute.Public) && cls.methods.isEmpty) shouldBe true
      p.renameMembers()
      (heldField.name == "renamed" && method.name == "renamedMethod") shouldBe true
    }

    test(s"$version function source, attributes and annotation getters observe T changes") {
      val p = TestData.readFixture(version)
      val f = p.pkg.function(1)
      (f.sourceFile.contains("fixture.cj")) shouldBe true
      (f.declaringDef.contains(p.definition[CHIR.ClassDef](1))) shouldBe true
      (f.attributes.contains(CHIR.Attribute.Public)) shouldBe true
      val auto = f.annotations.collectFirst { case x: CHIR.IsAutoEnvClass => x }.get
      val overridden = f.annotations.collectFirst { case x: CHIR.OverrideSrcFuncType => x }.get
      val wrapped = f.annotations.collectFirst { case x: CHIR.WrappedRawMethod => x }.get
      (auto.value && overridden.tpe == f.tpe && wrapped.rawMethod == f) shouldBe true
      p.changeFunctionMetadata()
      (f.attributes.isEmpty && !auto.value) shouldBe true
      (overridden.tpe.paramTypes.isEmpty && f.sourceFile.contains("changed.cj")) shouldBe true
    }

    test(s"$version enum-returning methods retain hash-map identity across repeated live reads") {
      val p = TestData.readFixture(version)
      // Both the method and constructor return the enum itself. Constructor views
      // may be recreated on read, but cannot become part of the method's identity.
      p.useEnumReturns()
      val function = p.pkg.function(1)
      val methods = scala.collection.mutable.HashMap(function -> "method")
      val hash = function.hashCode()
      for (_ <- 1 to 3) {
        p.definition[CHIR.EnumDef](3).ctors.foreach(_.tpe)
        function.hashCode() shouldBe hash
        methods(function) shouldBe "method"
      }
    }

    test(s"$version patch guard names use generic source names instead of mangled identifiers") {
      val p = TestData.readFixture(version)
      p.useGenericSourceName()
      val function = p.pkg.function(1)
      val name = com.huawei.excelsior.jet.compiler.chir.patchgen.PatchableName.guardFlag(function)
      (name.endsWith("$$T$GVF")) shouldBe true
      (!name.contains("mangled.function")) shouldBe true
    }

  }
}
