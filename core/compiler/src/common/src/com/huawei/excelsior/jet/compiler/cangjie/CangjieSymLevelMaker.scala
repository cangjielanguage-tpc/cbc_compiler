/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.{Environment, Stage, TypeProvider}
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox.Types.bitcodeTypeToDebugType
import com.huawei.excelsior.jet.compiler.debug.info.{CompilationUnitInfo, DTCompUnit, DTStaticField, Language}
import com.huawei.excelsior.jet.compiler.hlir.{HLIRErrorReporter, HLIRMetadata, HLIRSymLevelBuilder, HLIRSymLevelResolver}
import com.huawei.excelsior.jet.compiler.ir.LineNumber
import com.huawei.excelsior.jet.compiler.llvm.bitcode.{Bitcode, DIFlag, Errors}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{GenDebug, NewGlobalInitMangling, StdCoreAnyHierarchyRoot}
import com.huawei.excelsior.jet.compiler.symlevel.{Member, SignatureType, Type}
import com.huawei.excelsior.jet.util.ScalaCollections.*

import scala.collection.mutable
import scala.language.postfixOps
import scala.ref.{Reference, SoftReference}

object CangjieSymLevelMaker {
  val STD_CORE_PACKAGE_NAME              = "std.core"
  val STD_MATH_PACKAGE_NAME              = "std.math"
  val STD_CORE_ANY_NAME                  = "std.core.Any"
  val STD_CORE_ANY_LINKAGE_NAME          = "_ZN8std.core3AnyE"
  val STD_CORE_OBJECT_NAME               = "std.core.Object"
  val STD_CORE_OBJECT_LINKAGE_NAME       = "_ZN8std.core6ObjectE"
  val STD_CORE_STRING_NAME               = "record._ZN8std.core6StringE"
  val STD_CORE_NONE_VALUE_EXCEPTION_NAME = "_ZN8std.core18NoneValueExceptionE"
  val STD_CORE_OPTION_PREFIX             = "record._ZN8std.core6OptionI"
  val STD_CORE_OPTION_ARRAY_PREFIX       = STD_CORE_OPTION_PREFIX + "S_"

  val STD_CORE_ITERATOR_PART       = "8std.core8IteratorI"
  val STD_CORE_RANGE_PART          = "8std.core5RangeI"
  val STD_CORE_RANGE_ITERATOR_PART = "8std.core13RangeIteratorI"
  val STD_CORE_ARRAY_PART          = "8std.core5ArrayI"
  val STD_CORE_ARRAY_ITERATOR_PART = "8std.core13ArrayIteratorI"
  val STD_CORE_ARRAY_EXTEND_PART   = "8std.core6Extend8std.core5ArrayI"
  val STD_CORE_FUTURE_PART         = "8std.core6FutureI"
  val STD_CORE_OPTION_PART         = "8std.core6OptionI"
  val STD_CORE_OPTION_ARRAY_PART   = STD_CORE_OPTION_PART + "S_"
  val STD_REF_WEAK_REF_BASE_PART   = "7std.ref11WeakRefBaseE"

  val ZST_RECORD_NAME = "$ZST"
  val CANGJIE_ARRAY_PREFIX = "AR$"
  val CANGJIE_REF_ARRAY_NAME = CANGJIE_ARRAY_PREFIX + "RAny"
  val CANGJIE_RECORD_ARRAY_PREFIX = CANGJIE_ARRAY_PREFIX + "S"
  val EXPORTED_SYMBOL_PREFIX = "rt$"
  val INTERNAL_NAME_PREFIX = "CJ_"
  val ARRAY_SLICE_PREFIX = "AS$"
  val ARRAY_SLICE_NAME = ARRAY_SLICE_PREFIX + "_"
  val JAVA_HELPER_SUFFIX = "$Impl"
  val JAVA_HELPER_INIT = "$init"
  val JAVA_HELPER_PREINIT = "$preInit"
  val JAVA_HELPER_POSTINIT = "$postInit"
  val CONSTRUCTOR_NAME = "<init>"
  val JAVA_CLINIT_NAME = "<clinit>"
  val JAVA_HELPER_CLINIT_NAME = "$clinit"
  val CANGJIE_LAMBDA_PREFIX = "L$"
  val BOX_PREFIX = "BOX$"
  val BOX_FIELD_NAME = "value"
  val EXTENSION_PREFIX = "EXT$"

  val JAVA_LANG_NAME = "java8.java.lang"
  val JAVA_LANG_OBJECT_NAME = "_ZN15java8.java.lang6ObjectE"

  def VARRAY_PREFIX(length: Long) = "Y" + length + "$"

  def hierarchyRootName(implicit env: Environment) = if (env.enabled(StdCoreAnyHierarchyRoot)) STD_CORE_ANY_NAME else STD_CORE_OBJECT_NAME
  def hierarchyRootLinkageName(implicit env: Environment) = if (env.enabled(StdCoreAnyHierarchyRoot)) STD_CORE_ANY_LINKAGE_NAME else STD_CORE_OBJECT_LINKAGE_NAME

  // TODO improve Array representation: JET-17483
  def isArraySliceConstructor(name: String): Boolean = name.startsWith("_ZN8std.core5ArrayI") && name.contains(CONSTRUCTOR_NAME)

  def boxName(baseType: SignatureType): String = BOX_PREFIX + baseType.toJETSignature
  def extensionName(baseType: SignatureType, interfaces: Seq[SignatureType]): String =
    EXTENSION_PREFIX + baseType.toJETSignature + "$" + interfaces.size + "$" + interfaces.map(_.toJETSignature).mkString("_")

  val NO_LLVM_INDEX = -1

  private val parsedHLIR = mutable.HashMap.empty[String, scala.ref.Reference[HLIRSymLevelResolver]]

  private def registerHLIRResolver(source: String, resolver: HLIRSymLevelResolver): Unit = {
    parsedHLIR.put(source, new SoftReference(resolver))
  }

  def getHLIRResolver(source: String)(implicit env: Environment): HLIRSymLevelResolver = {
    parsedHLIR.get(source).flatMap(_.get).getOrElse {
      val module = Bitcode.ParsedModule.fromFile(source, null, preloadStrtab = false)
      val hlir = new HLIRMetadata(module)
      val resolver = new HLIRSymLevelResolver(hlir, loadPDB = false)
      registerHLIRResolver(source, resolver)
      resolver
    }
  }

  def makeSymLevel(builder: SymLevelBuilder): Type = builder.env.stage(Stage.CangjieModuleParsing) {
    parse(builder)
    builder.build()
  }

  private def parse(builder: SymLevelBuilder): Unit = {
    implicit val env: Environment = builder.env

    val source = builder.getSource
    val parsedModule = Bitcode.ParsedModule.fromFile(source, null, preloadStrtab = true)

    // verify HLIR version
    val hlir = new HLIRMetadata(parsedModule)
    val resolver = new HLIRSymLevelResolver(hlir, loadPDB = true)

    registerHLIRResolver(source, resolver)

    HLIRSymLevelBuilder.parse(builder, resolver)
  }

  def buildPackage(builder: SymLevelBuilder, parsedModule: Bitcode.ParsedModule, packageModifiers: Int)
                  (implicit reporter: HLIRErrorReporter, typeProvider: TypeProvider) = {
    val packageName = parsedModule.sourceFilename
    val symPackageName = makeSyntheticModuleName(packageName)

    if (typeProvider.findClass(xstr(symPackageName)) != null) {
      reporter.parsingError(s"duplicate package $symPackageName detected", packageName)
    }

    val pkg = builder.addPackage(symPackageName, packageModifiers)

    builder.addImport(pkg, typeProvider.getAJObjectType) // TODO: do we need this import?

    if (builder.env.enabled(GenDebug)) {
      val cu = parsedModule.getCompileUnit
      try {
        val name = xstr(packageName)
        val file = cu.file.resolve().asInstanceOf[Bitcode.DIFile]
        val dir = xstr(file.directory)
        val producer = xstr(cu.producer)
        assert(cu.language == 33) // same encoding as in DWARF (DW_LANG_CPP_14)
        reporter.require(dir != null, s"DICompileUnit.file empty directory", file)
        pkg.setDebugType(DTCompUnit(CompilationUnitInfo(name, Language.LANG_CPP_14, dir, producer)))
      } catch { case e: Errors.Error =>
        throw new RuntimeException(e)
      }

      def globalAsDTStaticField(g: Bitcode.Global, dtModule: DTCompUnit): DTStaticField = {
        val di = g.debugInfo
        val dtStaticField = DTStaticField(XString.ascii(di.name), XString.ascii(di.linkageName), di.isLocalToUnit)
        dtStaticField.baseType = bitcodeTypeToDebugType(di.`type`)
        dtStaticField.scope = dtModule
        dtStaticField
      }

      val moduleDT = pkg.getDebugType.asInstanceOf[DTCompUnit]
      // globals of UNIT type are taken as they can not be processed as normal fields
      // non-local globals are taken to distinguish globals of module scope and static fields of records
      val dtStaticFieldsForCompUnit = parsedModule.globals.filterNot(_.debugInfo == null)
        .filter(g => g.ty == Bitcode.Types.UNIT || !g.debugInfo.isLocalToUnit)
        .map(globalAsDTStaticField(_, moduleDT))
      moduleDT.elements = moduleDT.elements :++ dtStaticFieldsForCompUnit
    }

    pkg
  }

  def makeSyntheticModuleName(packageName: String) = {
    // Every Cangjie bitcode file represents a single compiled package (e.g. `default` or `std.core`).
    // All global variables and functions in such file can be viewed as belonging to the whole package.
    // In order to map it onto JET project system, we group them into a synthetic module class.

    // Mangle all unexpected characters for JET project system gods.
    packageName.replace('/', '$')
  }

  def packageInitName(packageName: String)(implicit env: Environment): String = {
    packageName.replace('/', '_') + packageInitSuffix
  }

  def isPackageInit(funcName: String)(implicit env: Environment): Boolean = {
    funcName.endsWith(packageInitSuffix)
  }

  def isGlobalInit(funcName: String)(implicit env: Environment): Boolean = {
    funcName.startsWith(globalInitPrefix)
  }

  def globalInitPrefix(implicit env: Environment) = if (env.enabled(NewGlobalInitMangling)) "$file_init$" else "global_init$"

  def packageInitSuffix(implicit env: Environment) = if (env.enabled(NewGlobalInitMangling)) "$global_init$" else "_global_init$"

  def getInternalNameByExportedMethodName(name: String): String =
    if (name.startsWith(EXPORTED_SYMBOL_PREFIX)) {
      INTERNAL_NAME_PREFIX + name.substring(EXPORTED_SYMBOL_PREFIX.length)
    } else {
      null
    }

  @throws[Errors.Error]
  private def getMemberType(member: Bitcode.CodeLinkedDIEntity): Bitcode.MDItem = {
    member match {
      case _: Bitcode.DISubprogram =>
        val resolvedType = member.`type`.resolve()
        if (resolvedType ne Bitcode.MDNull) {
          val items = resolvedType.asInstanceOf[Bitcode.DISubroutineType].types.asInstanceOf[Bitcode.MDNode].elts
          if (items.nonEmpty) {
            return items(0)
          }
        }
        Bitcode.MDNull
      case _ =>
        val resolvedType = member.asInstanceOf[Bitcode.DIGlobalVariable].staticDataMemberDeclaration.resolve()
        if ((resolvedType ne Bitcode.MDNull) && resolvedType.asInstanceOf[Bitcode.DIDerivedType].flags.contains(DIFlag.FlagStaticMember)) {
          resolvedType
        } else {
          member.`type`
        }
    }
  }

  def processMemberSourceInfo(member: Member, simpleName: String, cppName: String, debugInfo: Bitcode.CodeLinkedDIEntity, genDebug: Boolean): Unit = {
    if (debugInfo != null) {
      // Debug info becomes duplicated when FE stops mangling package name into generic type and member names.
      //
      // For example: a static field `qux` of generic type `Foo<T>` defined in package A
      // will be duplicated when instantiated as `Foo<X>` in package B and C.
      // However, if we have another package D which uses B and C
      // there we will have duplicate definitions of the same field `Foo<X>.qux`
      // with duplicate debug info. The debug info should be the same, from the package A,
      // so check it before assignment.
      def setDebugInfoPart[T](value: T, existing: T, setter: T => Unit, canBeOverridden: T => Boolean,
                              errMessage: => String): Unit = {
        if (canBeOverridden(existing)) {
          setter(value)
        } else {
          assert(existing == value, errMessage)
        }
      }
      def isNull(s: Any): Boolean = s == null

      val sourceName = xstr(if (debugInfo.name != null) debugInfo.name else simpleName)
      setDebugInfoPart(sourceName, member.getSourceName, member.setSourceName, isNull,
                       "name differs for " + debugInfo)

      // Original linkage name is bad-formatted, we reformat it in demangler above.
      val linkageName = xstr(cppName)
      setDebugInfoPart(linkageName, member.getCPPLinkageName, member.setCPPLinkageName, isNull,
                       "linkage-name differs for " + debugInfo)

      try {
        if (genDebug) {
          val tpe = getMemberType(debugInfo)
          if (tpe ne Bitcode.MDNull) {
            val debugType = CangjieDebugToolbox.Types.bitcodeTypeToDebugType(tpe)
            setDebugInfoPart(debugType, member.getDebugType, member.setDebugType, isNull,
                             "debug type differs for " + debugInfo)
          }
        }
        val file = debugInfo.file.resolve().asInstanceOf[Bitcode.DIFile]
        val path = file.fullPath
        if (path != null) {
          assert(path.nonEmpty, debugInfo.toString)
          setDebugInfoPart(xstr(path), member.getSourceFile, member.setSourceFile, isNull,
                           "source file differs for " + debugInfo)
        }
      } catch { case e: Errors.Error =>
        throw new RuntimeException(e)
      }

      def isUnknown(line: Int): Boolean = !LineNumber.isKnown(line)
      setDebugInfoPart(debugInfo.line, member.getSourceLine, member.setSourceLine, isUnknown,
                       "source line differs for " + debugInfo)
    }
  }

  def verifyCFunctionSignature(fName: String, retTy: Bitcode.Type, argTys: Array[Bitcode.Type], isForeign: Boolean): Unit = {
    var hasRefTypes = hasRefTypesIn(retTy)
    var hasValueRecords = retTy.isStruct
    for (argTy <- argTys) {
      hasRefTypes |= hasRefTypesIn(argTy)
      hasValueRecords |= argTy.isStruct
    }
    assert(!hasRefTypes, s"@c function '$fName' has value of traceable (managed, reference) type among its arguments or return value")
    assert(!hasValueRecords || isForeign, s"@c function '$fName' has record passed by value among its arguments or return value (issue #208)")
  }

  private def hasRefTypesIn(ty: Bitcode.Type): Boolean = ty match {
    case _ if ty.isInteger || ty.isFloatingPoint || ty.isZST => false
    case Bitcode.Types.REF => true
    case ty: Bitcode.StructType => hasRefTypesIn(ty.elements)
    case ty: Bitcode.ArrayType => hasRefTypesIn(ty.element)

    case ty: Bitcode.PointerType =>
      // Note that we might loop recursively if structure has pointer to itself.
      // However Cangjie has no such structures.
      // Moreover parsing of bitcode with such structures is not supported.
      hasRefTypesIn(ty.pointee)

    case ty: Bitcode.FunctionType =>
      // Note that function types cannot be recursive.
      hasRefTypesIn(ty.retTy) || hasRefTypesIn(ty.paramTys)

    case _ => shouldNotReachHere(ty)
  }

  private def hasRefTypesIn(tys: Array[Bitcode.Type]): Boolean = tys.exists(hasRefTypesIn)
}
