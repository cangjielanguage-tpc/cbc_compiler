/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{BuiltinSignature, FieldFlag, MethodFlag, TypeEnumKind, TypeFlag}
import com.huawei.excelsior.jet.assembler.cbc.isa12.{Assembler, LivenessInfoCollector}
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector.LiveState
import com.huawei.excelsior.jet.assembler.cbc.{CbcFileEncoder, CbcFileFormat, ExceptionTable}
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.abi.XTableGenerator
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator.GenerationTarget.CBC
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator.{GenerationTarget, env}
import com.huawei.excelsior.jet.compiler.cbc.CbcSignatureAdapter.toCbc
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.FINAL
import com.huawei.excelsior.jet.compiler.layout.MethodTables
import com.huawei.excelsior.jet.compiler.options.{BoolOption, StrOption}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{NonNullableWrapper, NullableWrapper}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.util.{Closure, Worklist}
import xscala.io.{DataOutput, Path}

import scala.annotation.targetName
import scala.collection.mutable
import scala.util.Using

object CbcFileEncoderAdapter extends CBCFileGenerator {

  implicit val typeProvider: TypeProvider = env.getTypeProvider
  private val allClasses = typeProvider.getAllClasses.toArray
  private val typeDefs = Worklist.from(allClasses.filter(x => !x.isCangjiePackage && x.isInCurrentCompilationSet))
  private val pkgDefs = allClasses.filter(_.isCangjiePackage)
  private val methodsCode = mutable.LinkedHashMap.empty[Method, Code]

  private def isFunctionalType(t: Type): Boolean = {
    t.getName.startsWith("$Cg") || t.getName.startsWith("$Ci")
  }

  def cbcPackageName(aotName: String): String = {
    "$P$" + aotName
  }

  def generate(output: Path, generationTarget: GenerationTarget = CBC): Unit = {
    val builder = CbcFileFormat.newBuilder()
    for (t <- typeDefs.accumulate) {
      TypeWrapper(t).build(builder.newTypeBuilder())
      if (t.isCangjieExtend && t.getDeclaredSuperInterfaces.nonEmpty && t.isCHIRDef) {
        ExtensionWrapper(t).build(builder.newExtensionBuilder())
      }
    }

    var mainPkgName: String = null
    for (t <- pkgDefs) {
      val hasCBCMembers = (t.getDeclaredMethods ++ t.getDeclaredFields).exists(_.getCHIRDef.nonEmpty)
      if (hasCBCMembers) {
        PackageWrapper(t).build(builder.newTypeBuilder())

        if (t.hasMain) {
          assert(mainPkgName == null, s"Multiple main packages: $mainPkgName and ${t.getName}")
          mainPkgName = cbcPackageName(t.getName)
        }
      }
    }

    if (mainPkgName != null) {
      builder.setMainTypeName(mainPkgName)
    }

    builder.setBytecodeVersion(Assembler.BYTECODE_VERSION)

    val (cbcDeps, aotDeps) = allClasses.filter(_.isCangjiePackage).partition(_.isCHIRDef)
    builder.setCbcDeps(cbcDeps.map(_.getName).mkString(":"))
    if (env.defined(StrOption.AllCbcAOTDeps)) {
      builder.setAotDeps(env.valueOf(StrOption.AllCbcAOTDeps))
    } else {
      builder.setAotDeps((Option(env.valueOfOrNull(StrOption.CbcAOTDeps)) ++ aotDeps.flatMap(packageToLibName)).mkString(":"))
    }

    Option(env.valueOfOrNull(StrOption.ForeignLibs)).foreach(builder.setForeignLibs)

    val result = builder.build()
    Using.resource(DataOutput.from(output)) { out =>
      val encoder = CbcFileEncoder(result)
      encoder.generate(out)
      if (env.enabled(BoolOption.LogCbcFileStats)) {
        encoder.printStats(xscala.io.stdout)
      }
    }
  }

  def sendCode(m: Method, seg: Segment, literalsOffset: Int,
               xinfo: XTableGenerator.PackedXInfo, exTable: ExceptionTable, liveness: LivenessInfoCollector.AllStates,
               tailParamCount: Int, untypedStackSlotsCount: Int,
               usedNonVolIRegsMask: Int, usedNonVolFRegsMask: Int, maxCalleeStackArgsCount: Int,
               mayHaveNativeCalls: Boolean,
               stackAllocatedTypeSigs: Seq[SignatureType], variableSizeTypes: Seq[SignatureType]): Unit = {

    val was = methodsCode.put(m, Code(seg, literalsOffset, xinfo, exTable, liveness,
      untypedStackSlotsCount,
      usedNonVolIRegsMask, usedNonVolFRegsMask, maxCalleeStackArgsCount,
      mayHaveNativeCalls, stackAllocatedTypeSigs, variableSizeTypes))

    assert(was.isEmpty)
  }

  private class TypeWrapper(val t: ClassType) {
    require(!t.isCangjiePackage)

    def build(builder: CbcFileFormat.Type.Builder): Unit = {
      builder.setName(t.getName)

      typeDefs ++= t.getDeclaredSuperTypes.filterNot(isFunctionalType)

      typeDefs ++= Closure(t.getDeclaredFields.map(_.getType)) {
        case ft: SignatureType.InstantiatedType => ft +: ft.instantiatedTypeParameters
        case ft: SignatureType.Tuple => ft.params
        case ft: SignatureType.CangjieArray => Seq(ft.elemType)
        case ft: SignatureType.VArray => Seq(ft.elemType)
        case ft: SignatureType.Box => Seq(ft.base)
        case ft: SignatureType.CangjieEnum => ft.params
        case _ => Seq()
      } collect {
        case t: (SignatureType.InstantiatedType | SignatureType.Record | SignatureType.Reference) => asClassType(t)
      }

      if (t.isClass) {
        Option(t.getSuperClassSig)
          .filter(_ => t.isClass)
          .map(_.toCbc)
          .foreach(builder.setSuperOrEnumType)
      }

      builder.setInterfaces(t.getDeclaredSuperInterfacesSig
        .map(_.toCbc)
        .toSeq)

      if (t.isUniversalGeneric) {
        builder.setGenericConstraints(Seq.fill(t.getGenericInfo.constraints.size)(BuiltinSignature.Nil))
      }

      if (t.isCangjieEnum) {
        // TODO: store kind in symlevel type
        val ctors = t.getCangjieEnumInfo.constructors.map(_.params)
        ctors match {
          case Seq(Seq(t), Seq()) =>
            builder.setEnumKind(TypeEnumKind.Option0)
            builder.setSuperOrEnumType(t.toCbc)
          case Seq(Seq(), Seq(t)) =>
            builder.setEnumKind(TypeEnumKind.Option1)
            builder.setSuperOrEnumType(t.toCbc)
          case _ if ctors.forall(_.isEmpty) =>
            builder.setEnumKind(TypeEnumKind.Primitive)
            builder.setSuperOrEnumType(CbcFileFormat.BuiltinSignature.I32)
          case _ =>
            builder.setEnumKind(TypeEnumKind.Union)
            builder.setUnionFields(ctors.map(ps => CbcFileFormat.Tuple(ps.map(_.toCbc))))
        }
      }

      buildFlags(builder)

      val (methods, fields) = if (!t.isCHIRDef) {
        builder.addFlag(TypeFlag.AOT)
        (t.getDeclaredMethods.filter(m => isVirtual(m)),
          t.getDeclaredFields.filter(f => !f.isStatic))
      } else {
        (t.getDeclaredMethods, t.getDeclaredFields)
      }

      methods.foreach(MethodWrapper(_).build(builder.newMethodBuilder()))
      fields.foreach(FieldWrapper(_).build(builder.newFieldBuilder()))
    }

    private def buildFlags(builder: CbcFileFormat.Type.Builder): Unit = {
      if (t.isJavaArray || t.isJavaReference) {
        notImplemented(t)
      }

      val modifiers = t.getCJModifiers
      if (t.isCangjieEnum) builder.addFlag(TypeFlag.ENUM)
      else if (t.isInterface) builder.addFlag(TypeFlag.INTERFACE)
      else if (t.isRecord) builder.addFlag(TypeFlag.RECORD)
      if (t.isCangjieType && !modifiers.contains(Modifier.CJ_SEALED)) builder.addFlag(TypeFlag.SEALED)

      if (modifiers.contains(Modifier.PUBLIC)) {
        builder.addFlag(TypeFlag.PUBLIC)
      }
      if (modifiers.contains(Modifier.ABSTRACT)) {
        builder.addFlag(TypeFlag.ABSTRACT)
      }
      if (modifiers.contains(Modifier.FINAL)) {
        builder.addFlag(TypeFlag.FINAL)
      }
      if (t.getName.startsWith("$Cl") || t.getName.startsWith("$Cw")) {
        builder.addFlag(TypeFlag.LAMBDA)
      }
    }
  }

  private class ExtensionWrapper(val t: ClassType) {
    require(t.isCangjieExtend)
    require(t.isCHIRDef)

    def build(builder: CbcFileFormat.Extension.Builder): Unit = {
      typeDefs ++= t.getDeclaredSuperTypes.filterNot(isFunctionalType)

      builder.setExtendedType(t.getCangjieExtendInfo.toCbc)
      builder.setInterfaces(t.getDeclaredSuperInterfacesSig
        .map(_.toCbc)
        .toSeq)

      if (t.isUniversalGeneric) {
        builder.setGenericConstraints(Seq.fill(t.getGenericInfo.constraints.size)(BuiltinSignature.Nil))
      }

      val virtualMethods = t.getDeclaredMethods.filter(m => isVirtual(m))
      virtualMethods.foreach(MethodWrapper(_).build(builder.newMethodBuilder()))
    }
  }

  private class PackageWrapper(val t: ClassType) {
    require(t.isCangjiePackage)

    def build(builder: CbcFileFormat.Type.Builder): Unit = {
      builder.setName(cbcPackageName(t.getName))

      val methods = t.getDeclaredMethods.filter(_.getCHIRDef.nonEmpty).toSeq
      val fields  = t.getDeclaredFields.filter(_.getCHIRDef.nonEmpty).toSeq

      methods.foreach(MethodWrapper(_).build(builder.newMethodBuilder()))
      fields.foreach(FieldWrapper(_).build(builder.newFieldBuilder()))
      
      if (methods.exists(_.isPackageInit)) {
        builder.addFlag(TypeFlag.PATCH)
      }
    }
  }

  private class MethodWrapper(val method: Method) {
    def build(builder: CbcFileFormat.Method.Builder): Unit = {
      builder.setName(method.getName)
      builder.setTypeName(method.getDeclaringClass.getName)
      builder.setSignature(method.getSignature.toCbc)
      buildFlags(builder)

      if (method.hasSourceFile) {
        builder.setSourceFile(method.getSourceFile.toString)
      }
      
      if (method.getCHIRDef.isEmpty) {
        //fixme: assert virtual
        builder.addFlag(MethodFlag.AOT)
        builder.setLinkageName(method.getExportedName.toString)
      } else {
        if (method.isPackageInit)        builder.addFlag(MethodFlag.PKG_INIT)
        if (method.isPackageLiteralInit) builder.addFlag(MethodFlag.LIT_INIT)
      }

      if (!method.isAbstract && !method.isNative && method.getCHIRDef.nonEmpty) {
        val code = methodsCode(method).nn
        val codeBuilder = builder.getCodeBuilder()
        codeBuilder.setSegment(code.seg)
        val xTable = Option(code.xinfo.xTable).map(_.toByteArray).getOrElse(Array.empty[Byte])
        codeBuilder.setExceptionTable(code.exTable)
        codeBuilder.setLiveness(code.liveness)
        codeBuilder.setUntypedStackSlotsCount(code.untypedStackSlotsCount)
        codeBuilder.setUsedNonVolIRegsMask(code.usedNonVolIRegsMask)
        codeBuilder.setUsedNonVolFRegsMask(code.usedNonVolFRegsMask)
        codeBuilder.setMaxCalleeStackArgsCount(code.maxCalleeStackArgsCount)
        codeBuilder.setMayHaveNativeCalls(code.mayHaveNativeCalls)
        codeBuilder.setStackAllocatedTypeSigs(code.stackAllocatedTypeSigs.map(_.toCbc))
        codeBuilder.setVariableSizeTypes(code.variableSizeTypes.map(_.toCbc))
      }
    }

    private def buildFlags(builder: CbcFileFormat.Method.Builder): Unit = {
      val modifiers = method.getCJModifiers

      // TODO: set virtual flag for methods, that are supposed to be in vtable
      if (method.isCangjieForeign) builder.addFlag(MethodFlag.FOREIGN)
      if (method.isPublic)         builder.addFlag(MethodFlag.PUBLIC)
      if (method.isPrivate)        builder.addFlag(MethodFlag.PRIVATE)
      if (method.isProtected)      builder.addFlag(MethodFlag.PROTECTED)
      if (method.isStatic)         builder.addFlag(MethodFlag.STATIC)
      if (method.isAbstract)       builder.addFlag(MethodFlag.ABSTRACT)

      if (method.hasRetByValParameter)      builder.addFlag(MethodFlag.SRET)
      if (method.hasOuterTypeInfoParameter) builder.addFlag(MethodFlag.HAS_OUTER_TI)
      if (method.hasThisTypeInfoParameter)  builder.addFlag(MethodFlag.HAS_THIS_TI)
      if (method.hasReferenceReceiver)      builder.addFlag(MethodFlag.REF_RECEIVER)
      if (method.hasRecordReceiver)         builder.addFlag(MethodFlag.REC_RECEIVER)
      if (method.hasMutRecordParameter)     builder.addFlag(MethodFlag.MUT) // TODO: is it correct?

      if (isVirtual(method)) builder.addFlag(MethodFlag.VIRTUAL)

    }
  }

  private class FieldWrapper(val field: Field) {
    def build(builder: CbcFileFormat.Field.Builder): Unit = {
      builder.setName(field.getName)
      builder.setFieldType(field.getType.toCbc)
      buildFlags(builder)

      if (field.getCHIRDef.isEmpty) {
        builder.addFlag(FieldFlag.AOT)
      }
    }

    private def buildFlags(builder: CbcFileFormat.Field.Builder): Unit = {
      val modifiers = field.getCJModifiers

      if (field.isPublic) builder.addFlag(FieldFlag.PUBLIC)
      if (field.isPrivate) builder.addFlag(FieldFlag.PRIVATE)
      if (field.isProtected) builder.addFlag(FieldFlag.PROTECTED)
      if (field.isStatic) builder.addFlag(FieldFlag.STATIC)

      if (modifiers.contains(Modifier.FINAL)) builder.addFlag(FieldFlag.FINAL)
      if (modifiers.contains(Modifier.VOLATILE)) builder.addFlag(FieldFlag.VOLATILE)
    }
  }

  private case class Code(seg: Segment,
                          literalsOffset: Int,
                          xinfo: XTableGenerator.PackedXInfo,
                          exTable: ExceptionTable,
                          liveness: LivenessInfoCollector.AllStates,
                          untypedStackSlotsCount: Int,
                          usedNonVolIRegsMask: Int,
                          usedNonVolFRegsMask: Int,
                          maxCalleeStackArgsCount: Int,
                          mayHaveNativeCalls: Boolean,
                          stackAllocatedTypeSigs: Seq[SignatureType],
                          variableSizeTypes: Seq[SignatureType])

  private def packageToLibName(pkg: ClassType): Option[String] = {
    val packageName = pkg.getName
    if (packageName.startsWith("std.")) {
      val stdPackageName = packageName.stripPrefix("std.")
      Some(s"cangjie-std-$stdPackageName") // stdlib dynamic libraries have special naming
    } else {
      // Ignore non-std aot libraries if they are specified explicitly
      Option.unless(env.defined(StrOption.CbcAOTDeps))(packageName)
    }
  }

  private def isVirtual(method: Method): Boolean = {
    val vtable = method.getDeclaringClass.getCHIRVTable
    vtable != null && vtable.extDefs.exists(_.funcTable.exists(_.impl.contains(method)))
  }

}
