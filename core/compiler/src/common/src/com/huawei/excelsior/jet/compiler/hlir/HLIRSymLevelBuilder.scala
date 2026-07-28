/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.hlir

import com.huawei.excelsior.common.Language
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.*
import com.huawei.excelsior.jet.compiler.cangjie.*
import com.huawei.excelsior.jet.compiler.cangjie.interop.java.{JavaAnnotatedClassProcessor, JavaSymbols}
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox
import com.huawei.excelsior.jet.compiler.hlir.HLIRErrorReporter.{fatal, withErrorReporter}
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.{Ref, Tag}
import com.huawei.excelsior.jet.compiler.hlir.interop.java.HLIRJavaSymbols
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{ABSTRACT, CJ_MUT, FINAL, PRIVATE, PUBLIC, STATIC, SYNTHETIC}
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode
import com.huawei.excelsior.jet.compiler.options.{BoolOption, StrOption}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{GenDebug, StrictJavaInteropHLIR}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{JBCReference, JavaArray, Nothing}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Field, GenericInfo, Member, Method, MethodSignature, SignatureType, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import com.huawei.excelsior.jet.compiler.{Env, Environment, TypeProvider}
import com.huawei.excelsior.jet.util.Worklist

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Builds symlevel entities from parsed [[HLIRMetadata]] representation.
  *
  * Following are the main stages of symlevel construction process.
  *
  * ==Create symlevel types==
  *
  * Create all symlevel types, which will be referenced from anywhere in HLIR representation.
  * This includes creation of packages, since they are represented as types in symlevel.
  *
  * For `@Java` types along with symlevel representation of Java class itself,
  * we also create Java-helper class, which is a Cangjie class containing actual implementation of Java
  * methods, written in Cangjie. The Java class file will be generated in the end,
  * after all symlevel entities have been successfully created.
  *
  * Thanks to explicit references to types in HLIR itself and extra verification in [[HLIRMetadata]],
  * all referenced types are guaranteed to have [[HLIRMetadata.Ref]] associated with them.
  * So this step requires only single pass over [[HLIRMetadata.refs]].
  *
  * Note that definition-less types always reference something from another package,
  * which should have been already parsed previously and all symlevel entities created for them.
  *
  * ==Create foreign C functions==
  *
  * Technically foreign C functions do not have any package associated with them,
  * so in HLIR they do not belong to any entity, but for symlevel, we need to create symlevel methods for them
  * and put somewhere. Currently they are hosted in the package, where foreign C function metadata was declared.
  *
  * ==Create global functions and variables==
  *
  * Global functions and variables are represented as static methods and fields of defining package in symlevel.
  * Note that we process here only globals and functions, that are ''defined'' in the package that we are parsing.
  * All other definition-less references are imported globals and functions from other packages,
  * so they must already have valid symlevel representation.
  *
  * ==Fill symlevel type members==
  *
  * Then all types defined in this package are "filled" with their members.
  *
  * One notable exception is `@Java` types, for which we must not create symlevel methods and fields,
  * but instead should fill their Java-helper classes.
  *
  * Note that for records we do not currently have appropriate place to put their members (except for instance fields).
  * So currently static methods, static fields and instance methods of records are placed in the defining package.
  * Instance method signatures must also be converted to static form in this case, similar to how it happens with
  * AJ structs in AJ FE (see [[HLIRSymLevelResolver.functionSignature]].
  *
  * ==Generate Java annotated class files==
  *
  * Finally, once all Cangjie types are successfully parsed, we produce Java class files for `@Java` annotated
  * types, defined in this package via [[JavaAnnotatedClassProcessor]].
  *
  * @author liontiger
  */
object HLIRSymLevelBuilder {

  private val runtimeImplTypes = Seq(
    "com/huawei/excelsior/jet/runtime/cangjie/SpecialMathSupport",
    "com/huawei/excelsior/jet/runtime/cangjie/IdentityHashCodeSupport",
    "com/huawei/excelsior/jet/runtime/cangjie/concurrency/FutureSupport",
    "com/huawei/excelsior/jet/runtime/memory/gc/WriteBarriers",
  )

  def parse(builder: SymLevelBuilder, resolver: HLIRSymLevelResolver): Unit = withErrorReporter(builder.getSource) { implicit reporter =>
    import reporter.*

    implicit val env: Environment = builder.env
    implicit val typeProvider: TypeProvider = env.getTypeProvider
    val genDebug = env.enabled(GenDebug)
    val hlir = resolver.hlir

    // -----------------------------------------------
    // Create symlevel types
    // -----------------------------------------------

    val packageModifiers = resolver.symModifiers(hlir.packageRef.get.packageDef.get.modifiers).value
    val symPackage = buildPackage(builder, hlir.module, packageModifiers)

    locally {
      // Workaround for JET-15600.
      // These runtime classes contain AJ implementations of Cangjie intrinsics which will be replaced later.
      // So we need to add them to import for PDB gods before symlevel is serialized.
      // For some reason without this resolve procedure compiler cannot find the runtime class later (e.g. during deserialization).
      // TODO: investigate and fix this mess
      for (name <- runtimeImplTypes) {
        val tpe = typeProvider.resolveTypeByName(typeProvider.getAJObjectType, xstr(name))
        assert(tpe != null)
        builder.addImport(symPackage, tpe)
      }
    }

    if (hlir.packageName == "std.core") {
      import SignatureType.*

      // Add single unspecialized array slice.
      // TODO: get rid of it when HLIR will always have specialized slice type.
      val erasedArraySlice = builder.addArraySlice(symPackage, elemTypeOpt = None)

      val builtInSigs = Primitive.values ++ Seq(
        BString,
      )

      // Add single array for all "built-in" types, including reference types.
      val builtInArraySigs = builtInSigs ++ Seq(
        fromSymType(typeProvider.getAJObjectType), // any reference type is erased
        CPointer(Address), // any CPointer is erased
        fromSymType(erasedArraySlice), // any ArraySlice is erased
      )
      for (sig <- builtInArraySigs) {
        builder.addRawArray(symPackage, sig)
      }

      // Add single array slice for all "built-in" types
      for (sig <- builtInSigs) {
        builder.addArraySlice(symPackage, elemTypeOpt = Some(sig))
      }
    }

    val foreignCFunctions = ArrayBuffer.empty[Ref.ForeignCFunction]
    val definedTypes = mutable.LinkedHashSet.empty[Ref]

    val javaHelpers = mutable.LinkedHashMap.empty[Ref, ClassType]
    val cangjieAnnotations = mutable.LinkedHashMap.empty[AnyRef, Ref.HasAnnotations]

    def makeJavaHelperClass(ref: Ref.HasName): Unit = {
      // [TODO JAVA_INTEROP] try add [[Modifier.SYNTHETIC]] and see if it works
      // TODO: remove public?
      val helperName = resolver.symName(ref) + JAVA_HELPER_SUFFIX
      val helper = builder.addClass(symPackage, helperName, Modifiers(FINAL, PUBLIC).value, isCangjie = true, isCangjieLambda = false, genericInfo = GenericInfo.none)
      javaHelpers(ref) = helper
    }

    val lambdasToParseWithoutDuplicates = mutable.Set.from(hlir.refs.filter(n => n.tag == Tag.ClassRef && n.linkageName.initialized &&
      n.asInstanceOf[Ref.HasClassDef].name.startsWith(CANGJIE_LAMBDA_PREFIX)))

    val processed = mutable.HashSet.empty[Ref]
    hlir.refs foreach makeSymType

    def makeSymType(ref: HLIRMetadata.Ref): Unit = if (!processed(ref)) {
      processed += ref
      ref match {
        case ref: Ref.Package =>
          if (!Env.languagePack.supports(Language.JAVA) && ref.name == JAVA_LANG_NAME) {
            // Ignore "java8.java.lang" import in pure CJVM.
            // It is currently referenced in "std.ffi.java".
            // FIXME: remove these hacks when FE removes "std.ffi.java" from stdlib in pure CJVM
          } else {
            val symType = if (hlir.packageRef.get == ref) {
              symPackage
            } else {
              resolver.symType(ref) getOrElse {
                val name = resolver.symName(ref)
                if (env.defined(StrOption.DynLibs)) {
                  // TODO: check that this is stableABI package
                  builder.addBitcodeDeferredPackage(name)
                } else {
                  shouldNotReachHere(s"missing imported package ${ref.name} in ${hlir.packageName} (maybe the wrong order of package dependencies)")
                }
              }
            }
            builder.addImport(symPackage, symType)
          }

        case _: Ref.Primitive | Ref.CString | _: Ref.CPointer | _: Ref.RawEnum | _: Ref.FunctionalType | _: Ref.Nullable |
             _: Ref.Instantiated[?] =>
        // No need to create anything

        case ref: Ref.HasClassDef =>
          if (!ref.name.startsWith(CANGJIE_LAMBDA_PREFIX) || lambdasToParseWithoutDuplicates.contains(ref)) {
            val isJava = ref.isInstanceOf[Ref.Java]
            val name = resolver.symName(ref)
            val symType = ref.classDef.getOption match {
              case Some(classDef) =>
                definedTypes += ref
                if (isJava) {
                  require(resolver.symType(ref).isEmpty, s"Java type $name is already defined", ref.md)
                  makeJavaHelperClass(ref)
                }

                builder.addClass(symPackage, name,
                  resolver.symModifiers(classDef.modifiers, allowOpen = true).value,
                  isCangjie = !isJava, classDef.isLambdaClass, resolver.genericInfo(ref))
              case None =>
                resolver.symType(ref) getOrElse {
                  if (!Env.languagePack.supports(Language.JAVA) && name == JAVA_LANG_OBJECT_NAME) {
                    resolver.findClass(STD_CORE_ANY_LINKAGE_NAME).get
                  } else {
                    builder.addBitcodeDeferredType(symPackage, name, isCangjie = !isJava, isRecord = false, isInterface = false, resolver.genericInfo(ref))
                  }
                }
            }
            builder.addImport(symPackage, symType)
            collectDebugType(ref, symType)
            if (!isJava) {
              cangjieAnnotations(symType) = ref
            }
            lambdasToParseWithoutDuplicates -= ref
          }

        case ref: Ref.HasInterfaceDef =>
          val isJava = ref.isInstanceOf[Ref.Java]
          val name = resolver.symName(ref)

          val symType = ref.interfaceDef.getOption match {
            case Some(interfaceDef) =>
              definedTypes += ref
              if (isJava) {
                require(resolver.symType(ref).isEmpty, s"Java type $name is already defined", ref.md)
                makeJavaHelperClass(ref)
              }
              builder.addInterface(symPackage, name,
                resolver.symModifiers(interfaceDef.modifiers, allowOpen = true, allowFinal = false).value,
                isCangjie = !isJava,
                resolver.genericInfo(ref))
            case None =>
              resolver.symType(ref) getOrElse {
                builder.addBitcodeDeferredType(symPackage, name, isCangjie = !isJava, isRecord = false, isInterface = true, resolver.genericInfo(ref))
              }
          }
          builder.addImport(symPackage, symType)
          collectDebugType(ref, symType)
          if (!isJava) {
            cangjieAnnotations(symType) = ref
          }

        case ref: Ref.HasRecordDef =>
          val name = resolver.symName(ref)
          val symType = ref.recordDef.getOption match {
            case Some(recordDef) =>
              definedTypes += ref
              builder.addRecord(symPackage, name, resolver.genericInfo(ref))
            case None =>
              resolver.symType(ref) getOrElse {
                builder.addBitcodeDeferredType(symPackage, name, isCangjie = true, isRecord = true, isInterface = false, resolver.genericInfo(ref))
              }
          }
          if (symType != null) {
            builder.addImport(symPackage, symType)
            builder.addImport(symType, typeProvider.getAJObjectType) // TODO: do we need this import?
            collectDebugType(ref, symType)
            ref match {
              case ref: Ref.HasAnnotations => cangjieAnnotations(symType) = ref
              case _ =>
            }
          }

        case ref: Ref.Box =>
          makeSymType(ref.baseType)
          val baseSigType = resolver.refSignature(ref, ref.baseType)
          builder.addBox(symPackage, baseSigType)

        case ref: Ref.HasInterfaceExtensionDef =>
          makeSymType(ref.baseType)
          if (resolver.symType(ref).isEmpty) {
            definedTypes += ref
            val baseSigType = resolver.refSignature(ref, ref.baseType)
            val interfaceSigs = ref.interfaces.map(i => resolver.refSignature(ref, i))
            val symType = builder.addClass(symPackage, extensionName(baseSigType, interfaceSigs), Modifiers.EMPTY.value,
              isCangjie = true, isCangjieLambda = false, genericInfo = GenericInfo.none)
            builder.addImport(symPackage, symType)
          }

        case ref: Ref.Array =>
          ref.elemType match {
            case _: Ref.VArray =>
              parsingError(s"Array<VArray> are not supported yet", ref.md)
            case _: Ref.Instantiated[?] | _: Ref.TypeVariable | _: Ref.OwnTypeVariable =>
              parsingError(s"Array<${ref.elemType.tag}> are not supported yet", ref.md) // FIXME-UG
            case _ =>
          }

          makeSymType(ref.elemType)

          if (resolver.symType(ref).isEmpty) {
            val elemSigType = resolver.refSignature(ref, ref.elemType)
            if (elemSigType.isRecord) {
              builder.addRawArray(symPackage, elemSigType)
            }
          }

        case ref: Ref.ArraySlice =>
          ref.elemType match {
            case _: Ref.VArray =>
              parsingError(s"Array<VArray> are not supported yet", ref.md)
            case _: Ref.Instantiated[?] | _: Ref.TypeVariable | _: Ref.OwnTypeVariable =>
              parsingError(s"Array<${ref.elemType.tag}> are not supported yet", ref.md) // FIXME-UG
            case _ =>
          }

          if (resolver.symType(ref).isEmpty) {
            makeSymType(ref.elemType)

            val elemSigType = resolver.refSignature(ref, ref.elemType)
            if (resolver.symArrayType(ref, ref.elemType).isEmpty && elemSigType.isRecord) {
              builder.addRawArray(symPackage, elemSigType)
            }

            builder.addArraySlice(symPackage, Some(elemSigType))
          }

        case _: Ref.JavaArray =>
          // Created on-demand

        case ref: Ref.ForeignCFunction =>
          // Will be processed later
          require(resolver.function(ref).nonEmpty, s"missing bitcode function definition", ref.md)
          foreignCFunctions += ref

        case ref: Ref.GlobalVariable =>
          // Will be processed later
          require(resolver.global(ref).nonEmpty, s"missing bitcode global definition", ref.md)
          if (ref.pkg == hlir.packageRef.get) {
            require(ref.pkg.packageDef.get.globals.contains(ref), s"global variable is not declared in package metadata ${ref.pkg.name}", ref.md)
            require(ref.modifiers.initialized, s"missing ${Tag.GlobalVariableDef}", ref.md)
          }

        case ref: (Ref.GlobalFunction | Ref.GenericGlobalFunction | Ref.GlobalCFunction) =>
          // Will be processed later
          require(resolver.function(ref).nonEmpty, s"missing bitcode function definition", ref.md)
          if (ref.pkg == hlir.packageRef.get) {
            require(ref.pkg.packageDef.get.globals.contains(ref), s"global function is not declared in package metadata ${ref.pkg.name}", ref.md)
            require(ref.modifiers.initialized, s"missing ${ref.tag}", ref.md)
          }

        case ref: Ref.VArray =>
          if (resolver.symType(ref).isEmpty) {
            makeSymType(ref.elemType)

            // LLVM cannot distinguish some types (like signed and unsigned ones).
            // Moreover we don't need to distinguish them too.
            // All we need is size and alignment of element type.
            // Note that correctness is provided by FE.
            val elemSymType = resolver.symType(ref.elemType).get
            require(!elemSymType.isClassOrInterface, "VArray<T> where T is reference type are not supported yet", ref.md)
            require(!elemSymType.isDeferred, "VArray<T> where T is from StableABI module are not supported yet", ref.md)
            val erasedElemSig = SignatureType.fromSymType(elemSymType)

            builder.addVArray(symPackage, SignatureType.VArray.name(erasedElemSig, ref.length), erasedElemSig)
          }

        case _: Ref.MemberDef | _: Ref.ConstantString | _: Ref.Parameter | _: Ref.Annotation | _: Ref.JavaAnnotationRelated |
             _: Ref.TypeParameter | _: Ref.TypeVariable | _: Ref.OwnTypeVariable | _: Ref.GenericConstraints | Ref.ThisType =>
          // Will be processed later
      }
    }

    def collectDebugType(t: Ref, symType: SymType): Unit = {
      for (linkageName <- t.linkageName.getOption; debugType <- hlir.module.namedDebugInfo(linkageName)) {
        symType.setDebugType(CangjieDebugToolbox.Types.bitcodeTypeToDebugType(debugType))
      }
    }

    // -----------------------------------------------
    // Create foreign C functions
    // -----------------------------------------------

    for (func <- foreignCFunctions; bcFunc <- resolver.function(func)) {
      val vararg = bcFunc.ty.vararg
      require(bcFunc.isProto, s"unexpected foreign C function definition in bitcode", func.md)
      verifyCFunctionSignature(func.name, bcFunc.ty.retTy, bcFunc.ty.paramTys, isForeign = true) // TODO: verify HLIR signature
      val name = resolver.symName(func)
      val sig = resolver.functionSignature(func, vararg, eraseZSTReturn = true)
      builder.addExternalCMethod(name, sig, vararg, bcFunc.idx)
    }

    def processFieldSourceInfo(f: Field, typeName: String, simpleName: String, linkageName: String, global: Bitcode.Global): Unit = {
      if (f.getDeclaringClass.getCangjiePackage == symPackage) {
        processMemberSourceInfo(f, simpleName, linkageName, global.debugInfo, genDebug)
      }
    }

    def processMethodSourceInfo(m: Method, typeName: String, simpleName: String, linkageName: String, func: Bitcode.Function): Unit = {
      if (m.getDeclaringClass.getCangjiePackage == symPackage) {
        // Some renaming to have proper sourceName/sourceFullName
        val (prefix, name) = (simpleName, linkageName) match {
          case ("main", "main") => ("", "cj_entry$")         // to avoid clashing with "default.main"
          case (_, "user.main") => ("", "user_main$")        // to make it similar to AOT
          case ("<main>", _)    => (typeName + ".", "main")  // to see "default.main" in debugger
          case _                => (typeName + ".", simpleName)
        }
        m.setSourceFullName(xstr(prefix + name))
        processMemberSourceInfo(m, name, linkageName, func.debugInfo, genDebug)
      }
    }

    hlir.packageRef.get.packageDef.get.globals foreach {
      case func: Ref.GlobalCFunction =>
        for (bcFunc <- resolver.function(func)) {
          val vararg = bcFunc.ty.vararg
          require(!bcFunc.isProto, s"unexpected ${func.tag} without definition in bitcode", func.md)

          val name = func.name
          val symName = resolver.symName(func)
          verifyCFunctionSignature(name, bcFunc.ty.retTy, bcFunc.ty.paramTys, isForeign = false) // TODO: verify HLIR signature
          val sig = resolver.functionSignature(func, vararg, eraseZSTReturn = true)
          val m = builder.addCMethod(symName, sig, vararg, bcFunc.idx)

          processMethodSourceInfo(m, hlir.packageName, name, func.linkageName.get, bcFunc)
        }

      case func: (Ref.GlobalFunction | Ref.GenericGlobalFunction) => // FIXME-UG
        for (bcFunc <- resolver.function(func)) {
          val vararg = bcFunc.ty.vararg
          require(!vararg, s"unexpected vararg global function ", func.md)

          val name = func.name
          val symName = resolver.symName(func)
          val idx = bcFunc.idx

          val sig = resolver.functionSignature(func, vararg)

          val linkageName = func.linkageName.get
          val exportedName = getInternalNameByExportedMethodName(linkageName)

          val m = if (bcFunc.isProto) {
            // @intrinsic function defined in Cangjie source
            require(name.startsWith(EXPORTED_SYMBOL_PREFIX), s"unexpected ${func.tag} without definition in bitcode", func.md)
            require(!func.isInstanceOf[Ref.GenericGlobalFunction], s"unexpected generic @intrinsic method: $func", func.md)
            builder.addIntrinsicMethod(symName, sig, idx, shouldBeGenerated = true) ensuring (_.isAJReplaced, s"@intrinsic function '$name' was not replaced")

          } else if (isPackageInit(name)(env)) {
            // Primary global_init -- package init
            val packageInit = packageInitName(hlir.packageName)
            assert(name == packageInit, s"unexpected package init name '$name' (expected '$packageInit')")
            builder.addPackageInit(symName, sig, idx)

          } else if (isGlobalInit(name)(env)) {
            // Secondary global_init
            builder.addGlobalInit(symName, sig, idx)

          } else {
            val arraySliceConstrModifiers = if isArraySliceConstructor(symName) then Modifiers(CJ_MUT) else Modifiers.EMPTY

            val modifiers = arraySliceConstrModifiers | (func.modifiers.getOption match {
              case Some(mods) => resolver.symModifiers(mods)
              case None => parsingError(s"missing ${func.tag} definition", func.md); Modifiers.EMPTY
            })

            builder.addPackageMethod(symName, sig, exportedName, idx, modifiers.value, resolver.genericInfo(func), resolver.hasUGDescParameter(func), hasThisTypeInfoParam = false, isCFunc = false)
          }

          processMethodSourceInfo(m, hlir.packageName, name, linkageName, bcFunc)
          cangjieAnnotations(m) = func
        }

      case global: Ref.GlobalVariable =>
        for (bcGlobal <- resolver.global(global)) {
          val ty = bcGlobal.ty
          val name = global.name
          val symName = resolver.symName(global)

          val modifiers = global.modifiers.getOption match {
            case Some(mods) => resolver.symModifiers(mods).value
            case None => parsingError(s"missing ${global.tag} definition", global.md); 0
          }

          val sig = resolver.typeSignature(global)

          val f = builder.addPackageField(symName, sig, modifiers)

          if (!hasErrors) {
            for (initValue <- hlir.module.getConstValue(bcGlobal.initVarIdx) if initValue != 0) {
              assert(!ty.isStruct)
              builder.setStaticFieldConstValue(f, initValue)
            }

            processFieldSourceInfo(f, hlir.packageName, name, global.linkageName.get, bcGlobal)
            cangjieAnnotations(f) = global
          }
        }
    }

    // -----------------------------------------------
    // Fill symlevel type members
    // -----------------------------------------------

    def referenceType[T <: ReferenceType](companion: CompiledType.Companion[T])(ref: Ref): T =
      companion(resolver.refSignature(ref.asInstanceOf[Ref.Sig]))

    def checkInterfaces(types: Seq[Ref], t: Ref): Unit = {
      def symClassType(ref: Ref): ClassType = asClassType(resolver.symType(ref).orNull)
      val invalidInterfaces = types.map(symClassType).filterNot(x => x.isInterface || x.isDeferred)
      if (invalidInterfaces.nonEmpty) {
        val name = t match {
          case t: Ref.HasName => t.name
          case _ => t.tag.toString
        }
        parsingError(s"type $name inherits non-interface types: ${invalidInterfaces.mkString("[", ", ", "]")}", t.md)
      }
    }

    def startJavaHelperFilling(t: Ref): Unit = builder.startSyntheticClassFilling(javaHelpers(t))

    symPackage.setSourceFile(xstr(builder.getSourceForSymlevel))

    // All lambdas have an copy in `definedTypes`, because $Auto_Env and $Lambda are mapping to L$...
    // so we need to remove duplicates.
    definedTypes.toSet foreach {
      case t: Ref.HasClassDef =>
        val isJava = t.isInstanceOf[Ref.Java]
        val classDef = t.classDef.get
        val klass = asClassType(resolver.symType(t).get)

        if (isJava || klass.getCangjiePackage == symPackage) {
          // Note: do not re-write source file if the type is from another package.
          // Otherwise it affects binary stability of produced binaries (see JET-14625 and JET-15579).
          klass.setSourceFile(xstr(builder.getSourceForSymlevel))
        }

        if (t.name == hierarchyRootName) {
          require(classDef.superclass.isEmpty, s"expected no superclass definition for ${t.name}", t.md)
        } else {
          require(classDef.superclass.nonEmpty, s"missing superclass definition", t.md)
        }

        checkInterfaces(classDef.superinterfaces, t)

        val superclass = classDef.superclass.map(referenceType(RefClassType)).getOrElse(RefClassType(typeProvider.getCangjieRefType))
        val superinterfaces = classDef.superinterfaces.map(referenceType(RefInterfaceType)).toArray

        if (isJava) {
          startJavaHelperFilling(t)
        } else {
          builder.startClassFilling(klass, superclass, superinterfaces)
        }
        classDef.members foreach { m => parseMember(t, m) }

      case t: Ref.HasInterfaceDef =>
        val isJava = t.isInstanceOf[Ref.Java]
        val interfaceDef = t.interfaceDef.get
        val klass = asClassType(resolver.symType(t).get)

        if (isJava || klass.getCangjiePackage == symPackage) {
          // Note: do not re-write source file if the type is from another package.
          // Otherwise it affects binary stability of produced binaries (see JET-14625 and JET-15579).
          klass.setSourceFile(xstr(builder.getSourceForSymlevel))
        }

        checkInterfaces(interfaceDef.superinterfaces, t)

        val superinterfaces = interfaceDef.superinterfaces.map(referenceType(RefInterfaceType)).toArray

        if (isJava) {
          startJavaHelperFilling(t)
        } else {
          builder.startInterfaceFilling(klass, superinterfaces)
        }
        interfaceDef.members foreach { m => parseMember(t, m) }

      case t: Ref.HasRecordDef =>
        val recordDef = t.recordDef.get
        val klass = asClassType(resolver.symType(t).get)

        if (klass.getCangjiePackage == symPackage) {
          // Note: do not re-write source file if the type is from another package.
          // Otherwise it affects binary stability of produced binaries (see JET-14625 and JET-15579).
          klass.setSourceFile(xstr(builder.getSourceForSymlevel))
        }

        builder.startClassFilling(klass, null, null)
        recordDef.members foreach { m => parseMember(t, m) }

      case t: Ref.HasInterfaceExtensionDef =>
        val extensionDef = t.interfaceExtensionDef.get
        val klass = asClassType(resolver.symType(t).get)

        if (klass.getCangjiePackage == symPackage) {
          // Note: do not re-write source file if the type is from another package.
          // Otherwise it affects binary stability of produced binaries (see JET-14625 and JET-15579).
          klass.setSourceFile(xstr(builder.getSourceForSymlevel))
        }

        val superBoxType = builder.addBox(symPackage, resolver.refSignature(t, t.baseType))

        checkInterfaces(t.interfaces, t)

        val interfaces = t.interfaces.map(referenceType(RefInterfaceType)).toArray

        builder.startClassFilling(klass, RefClassType(superBoxType), interfaces)

        extensionDef.members foreach { m => parseMember(t, m) }

      case t => shouldNotReachHere(t)
    }

    def parseMember(t: Ref, m: Ref.MemberDef): Unit = {
      require(m.refType == t, s"inconsistent reference type (expected ${t.md})", m.md)

      val isJava = t.isInstanceOf[Ref.Java]

      val staticModifiers = m match {
        case _: (Ref.StaticField | Ref.StaticMethod | Ref.GenericStaticMethod)       => Modifiers(STATIC)
        case _: (Ref.InstanceField | Ref.InstanceMethod | Ref.GenericInstanceMethod) => Modifiers.EMPTY
      }
      val isStatic = staticModifiers contains STATIC

      val recordInitModifiers = m match {
        case ref: Ref.InstanceMethod if t.isInstanceOf[Ref.HasRecordDef] && ref.name == CONSTRUCTOR_NAME => Modifiers(CJ_MUT)
        case _ => Modifiers.EMPTY
      }

      val modifiers = staticModifiers | recordInitModifiers | Modifiers(m.modifiers.getOption match {
        case Some(mods) => resolver.symModifiers(mods, allowOpen = m.isInstanceOf[Ref.InstanceMethod]).value
        case None =>
          parsingError(s"missing metadata definition", m.md)
          0
      })

      val name = m.name
      val symName = resolver.symName(m)

      m match {
        case m: Ref.Field =>
          if (isJava) {
            // do not add fields to Java helper class
            return
          }

          val sig = resolver.typeSignature(m)

          // TODO: fix copy-paste when old symlevel maker dies
          val f = sig match {
            case sig: SignatureType.Record =>
              val tpe = asClassType(sig)
              require(tpe != null, s"unknown record name '${sig.name}'", m.md)
              builder.addClassField(symName, sig, modifiers.value)

            case sig: SignatureType.ArraySlice =>
              val tpe = asClassType(sig)
              assert(tpe != null)
              builder.addClassField(symName, sig, modifiers.value)

            case sig: SignatureType.VArray =>
              require(!t.isInstanceOf[Ref.HasClassDef] || isStatic, "VArrays in instance class fields are not supported yet", m.md)
              val tpe = asClassType(sig)
              assert(tpe != null)
              builder.addClassField(symName, sig, modifiers.value)

            case _ =>
              // FIXME
              // NOTE: static fields are not complete at this moment:
              //   - ConstValue initializer may need to be assigned
              // It will be done during processing of globals
              builder.addClassField(symName, sig, modifiers.value)
          }

          if (!isJava) {
            cangjieAnnotations(f) = m
          }

          for (global <- resolver.global(m)) {
            for (initValue <- hlir.module.getConstValue(global.initVarIdx) if initValue != 0) {
              assert(!global.ty.isStruct)
              builder.setStaticFieldConstValue(f, initValue)
            }

            processFieldSourceInfo(f, hlir.packageName, name, m.linkageName.get, global)
          }

        case m: Ref.MethodDef =>

          val sig = resolver.functionSignature(m, vararg = false)

          if (isJava) {
            if (modifiers contains ABSTRACT) {
              // do not add abstract methods to Java helper class
              return
            }

            val linkageName = m.linkageName.get
            resolver.function(hlir.ref(linkageName).get) match {
              case Some(func) =>
                assert(!func.ty.vararg)

                val isClinit = name == JAVA_CLINIT_NAME

                if (isClinit && env.enabled(StrictJavaInteropHLIR)) {
                  if (!isStatic) {
                    parsingError(s"HLIR clinit method is expected in static methods list", m.md)
                  }
                  if (!(modifiers contains STATIC)) {
                    parsingError(s"HLIR clinit method is not marked as static", m.md)
                  }
                  if (sig.toJETSignature != "()V") {
                    parsingError(s"HLIR clinit method has invalid signature '${sig.toJETSignature}'", m.md)
                  }
                }

                val javaName = if (symName == CONSTRUCTOR_NAME) {
                  JAVA_HELPER_INIT
                } else if (symName == JAVA_CLINIT_NAME) {
                  JAVA_HELPER_CLINIT_NAME
                } else {
                  symName
                }
                val javaSig = if (isStatic) {
                  sig
                } else {
                  sig.copy(parameterTypes = SignatureType.JBCReference(asClassType(resolver.symType(t).get)) +: sig.parameterTypes)
                }
                val javaModifiers = Modifiers(PRIVATE, STATIC, SYNTHETIC).value

                val sm = builder.addClassMethod(javaName, javaSig, null, func.idx, javaModifiers, resolver.genericInfo(m), resolver.hasUGDescParameter(m), resolver.hasThisTypeInfoParameter(m))

                val typeName = t.asInstanceOf[Ref.HasName].name
                processMethodSourceInfo(sm, typeName, name, linkageName, func)

                if (javaName == JAVA_HELPER_INIT) {
                  // each constructor of @java class corresponds to 3 helper methods: $init, $preInit and $postInit

                  val postInit = builder.addClassMethod(JAVA_HELPER_POSTINIT, javaSig, null, NO_LLVM_INDEX, javaModifiers, resolver.genericInfo(m), resolver.hasUGDescParameter(m), resolver.hasThisTypeInfoParameter(m))
                  processMethodSourceInfo(postInit, typeName, name, linkageName, func)

                  // TODO: $preInit may be skipped if super- or delegate- constructor is no-arg TODO: stricter check
                  // for preInit - remove $this arg and add "Object[]" return type (sig already does not have $this)
                  assert(javaSig.returnType.isZST)
                  val preInitSig = sig.copy(returnType = JavaArray(JBCReference("java/lang/Object")))
                  val preInit = builder.addClassMethod(JAVA_HELPER_PREINIT, preInitSig, null, NO_LLVM_INDEX, javaModifiers, resolver.genericInfo(m), resolver.hasUGDescParameter(m), resolver.hasThisTypeInfoParameter(m))
                  processMethodSourceInfo(preInit, typeName, name, linkageName, func)
                }

              case None =>
                parsingError(s"missing bitcode function definition with linkage name $linkageName", m.md)
            }

          } else { // Cangjie
            if (modifiers contains ABSTRACT) {
              // abstract method
              if (isStatic) {
                // do not add abstract static methods TODO: keep them for reflection
                return
              }
              val sm = (t: @unchecked) match {
                case _: Ref.HasClassDef =>
                  builder.addClassMethod(symName, sig, null, NO_LLVM_INDEX, modifiers.value, resolver.genericInfo(m), resolver.hasUGDescParameter(m), resolver.hasThisTypeInfoParameter(m))
                case _: Ref.HasInterfaceDef =>
                  builder.addClassMethod(symName, sig, null, NO_LLVM_INDEX, (modifiers + PUBLIC).value, resolver.genericInfo(m), resolver.hasUGDescParameter(m), resolver.hasThisTypeInfoParameter(m))
                case _: Ref.HasRecordDef | _: Ref.HasInterfaceExtensionDef =>
                  parsingError("invalid abstract method", m.md)
                  null
              }

              if (sm != null) {
                cangjieAnnotations(sm) = m
              }

            } else {
              val linkageName = m.linkageName.get
              resolver.function(hlir.ref(linkageName).get) match {
                case Some(func) =>
                  assert(!func.ty.vararg)

                  val exportedName = getInternalNameByExportedMethodName(linkageName)
                  val sm = builder.addClassMethod(symName, sig, exportedName, func.idx, modifiers.value, resolver.genericInfo(m), resolver.hasUGDescParameter(m), resolver.hasThisTypeInfoParameter(m))

                  val typeName = (t: @unchecked) match {
                    case t: Ref.HasName => t.name
                    case _: Ref.InterfaceExtension | _: Ref.InstantiatedInterfaceExtension =>
                      // TODO: need to use pretty base name here, like Int64
                      "<unknown-extension>"
                  }
                  processMethodSourceInfo(sm, typeName, name, linkageName, func)
                  cangjieAnnotations(sm) = m

                case None =>
                  parsingError(s"missing bitcode function definition with linkage name $linkageName", m.md)
              }
            }
          }
      }
    }

    // -----------------------------------------------
    // Check symlevel types
    // -----------------------------------------------

    definedTypes foreach {
      case t: Ref.HasClassDef =>
        val klass = asClassType(resolver.symType(t).get)
        val worklist = Worklist.from(Iterator.single(klass) ++ klass.getSuperClasses)
        for {
          tpe <- worklist.accumulate
          if tpe.isClass || tpe.isRecord
          f <- tpe.getCurrentDeclaredFields
          if !f.isStatic
        } {
          f.getType match {
            case sig: SignatureType.VArray => parsingError("VArrays in instance class fields are not supported yet", t.md)
            case sig: SignatureType.Record => worklist += asClassType(sig)
            case sig: SignatureType.ArraySlice =>
            case _ => assert(!f.isAJFlat)
          }
        }

      case _ =>
    }

    // -----------------------------------------------
    // Parse annotations
    // -----------------------------------------------

    for ((sym, ref) <- cangjieAnnotations) {
      parseCangjieAnnotations(sym, ref)
    }

    def parseCangjieAnnotations(sym: AnyRef, ref: Ref.HasAnnotations): Unit = {
      sym match {
        case sym: ClassType =>
          for (m <- findCangjieAnnotationFactory(ref)) {
            builder.addCJAnnotationFactoryForClass(sym, m)
          }

        case sym: Field =>
          for (m <- findCangjieAnnotationFactory(ref)) {
            builder.addCJAnnotationFactoryForField(sym, m)
          }

        case sym: Method =>
          for (m <- findCangjieAnnotationFactory(ref)) {
            builder.addCJAnnotationFactoryForMethod(sym, m)
          }
          val paramRefs = ref.asInstanceOf[Ref.HasParameters].parameters
          if (paramRefs.nonEmpty) {
            // Note: it is important to parse annotations according to source signature and not the ABI one,
            //       so that all source parameters are still present (including Unit type parameters).
            val paramAnnotations = new Array[Method](sym.getSignature.parameterTypes.size)
            var foundFactory = false
            for (p <- paramRefs) {
              val factory = findCangjieAnnotationFactory(p).orNull
              if (factory != null) {
                foundFactory = true
              }
              paramAnnotations(p.index) = factory
            }
            if (foundFactory) {
              builder.addCJAnnotationFactoriesForParameters(sym, paramAnnotations)
            }
          }
      }
    }

    def findCangjieAnnotationFactory(ref: Ref.HasAnnotations): Option[Method] = {
      ref.annotations.toSeq match {
        case Seq(annot: Ref.CangjieAnnotation) =>
          val klass = resolver.symRefType(annot.factory).get
          val symName = resolver.symName(annot.factory.asInstanceOf[Ref.HasName])
          val factory = klass.findDeclaredMethod(xstr(symName), null)
          Some(factory)

        case Seq() => None
        case annots =>
          parsingError(s"unexpected Cangjie annotations ${annots.mkString("[", ",", "]")}", ref.md)
          None
      }
    }

    // -----------------------------------------------
    // Generate Java annotated class files
    // -----------------------------------------------

    JavaAnnotatedClassProcessor { gen =>
      val hlirJavaSymbols = new HLIRJavaSymbols(hlir, resolver)

      def findDelegateConstructor(constr: JavaSymbols.Method) = {
        val constrFunc: Bitcode.Function = {
          import hlirJavaSymbols.*
          constr match {
            case m: MethodImpl => resolver.function(m.ref).getOrElse {
              fatal(s"missing bitcode function definition of Java constructor", m.ref.md, builder.getSource)
            }
            case m => shouldNotReachHere(m)
          }
        }
        var delegateConstructor: Option[Ref.InstanceMethod] = None
        val delegateCollector: Bitcode.InstructionConsumer[Any] = new Bitcode.InstructionConsumer[Any] {
          override def emptyValuesArray(length: Int) = new Array(length)
          override def startFunction(fn: Bitcode.Function): Unit = {}
          override def endFunction(): Unit = {}
          override def lexicalBlock(id: Long, lb: Bitcode.DILexicalBlock, lineNumber: Int, columnNumber: Int): Unit = {}
          override def instructionLocation(instrNumber: Int, file: Bitcode.DIFile, lineNumber: Int, columnNumber: Int, scopeId: Long): Unit = {}
          override def startInstruction(instrNumber: Int): Unit = {}
          override def endInstruction(): Unit = {}
          override def startXBlock(instrNumber: Int): Unit = {}
          override def noValue(): Any = null

          // Note: we need integral constants to compute correct type of getElementPtr below!
          // TODO: fix this mess!!
          override def cstIntegral(ty: Bitcode.Type, numericValue: Long) = numericValue

          override def cstFloatingPoint(ty: Bitcode.Type, bits: Long): Any = null
          override def cstNullPointer(ty: Bitcode.Type): Any = null
          override def metadata(md: Bitcode.MDItem): Any = null
          override def getMDValue(value: Any): Bitcode.MDValue = null
          override def param(ty: Bitcode.Type, idx: Int) = idx
          override def global(g: Bitcode.Global): Any = null

          override def function(fn: Bitcode.Function) = fn

          override def ret(): Unit = {}
          override def ret(ty: Bitcode.Type, value: Any): Unit = {}
          override def unreachable(): Unit = {}
          override def alloca(allocTy: Bitcode.Type, count: Any): Any = null

          override def extractValue(baseTy: Bitcode.Type, baseVal: Any, indices: Array[Int]) = {
            // Note: we must compute the correct pointer type for the resulting value,
            //       otherwise assertions in other instructions might fail.
            // TODO: fix this mess!!
            val newIndices = Array.tabulate[Any](indices.length + 1) {
              case 0 => 0L
              case i => indices(i - 1).toLong
            }
            val ptr = getElementPtr(baseTy, baseVal, newIndices, inbounds = true) // always inbounds
            new Bitcode.TypedV[Any](ptr.ty.asInstanceOf[Bitcode.PointerType].pointee, baseVal)
          }

          override def getElementPtr(baseTy: Bitcode.Type, basePtr: Any, indices: Array[Any], inbounds: Boolean) = {
            // Note: we must compute the correct pointer type for the resulting value,
            //       otherwise assertions in other instructions might fail.
            // TODO: fix this mess!!
            var curTy: Bitcode.Type = Bitcode.Types.ptrTo(baseTy)
            for (index <- indices) {
              curTy = curTy match {
                case pointerType: Bitcode.PointerType => pointerType.pointee
                case arrayType: Bitcode.ArrayType => arrayType.element
                case structType: Bitcode.StructType => structType.elements(index.asInstanceOf[Long].toInt)
                case _ => curTy
              }
            }
            new Bitcode.TypedV[Any](Bitcode.Types.ptrTo(curTy), basePtr)
          }

          override def store(ty: Bitcode.Type, mem: Any, value: Any): Unit = {}
          override def load(ty: Bitcode.Type, mem: Any): Any = null
          override def cast(op: Int, toTy: Bitcode.Type, fromTy: Bitcode.Type, value: Any): Any = null
          override def unOp(ty: Bitcode.Type, op: Int, value: Any) = null
          override def binOp(ty: Bitcode.Type, op: Int, l: Any, r: Any): Any = null
          override def br(bb: Int): Unit = {}
          override def br(cond: Any, trueBB: Int, falseBB: Int): Unit = {}
          override def phi(values: Array[Any], predBBs: Array[Int]): Any = null
          override def cmp(ty: Bitcode.Type, op: Int, l: Any, r: Any): Any = null

          override def call(fnTy: Bitcode.FunctionType, target: Any, args: Array[Any], argTys: Array[Bitcode.Type], handlerBB: Int): Any = {
            target match {
              case fn: Bitcode.Function =>
                args match {
                  case Array(0, _*) =>
                    hlir.ref(fn.name) match {
                      case Some(ref: Ref.InstanceMethod) if ref.name == CONSTRUCTOR_NAME =>
                        // call of a constructor with same receiver as of the current constructor function is a super- or delegate- constructor call
                        // we currently expect at most one such call per constructor function (but a better analysis is required if
                        // such calls on different execution paths may exist)
                        require(delegateConstructor.isEmpty,
                          s"Duplicate call of super- or delegate- <init>() from ${constrFunc.name}", s"call $fnTy @${fn.name}")
                        delegateConstructor = Some(ref)
                      case _ =>
                    }

                  case _ => null
                }
              case _ => null
            }
          }
        }

        Bitcode.parseFunctionBody(builder.getSource, hlir.module, constrFunc.idx, delegateCollector, cacheModuleValues = false)
        delegateConstructor.map(new hlirJavaSymbols.MethodImpl(_))
      }

      for ((ref, javaHelper) <- javaHelpers) {
        gen.process(hlirJavaSymbols, hlirJavaSymbols.classByRef(ref.asInstanceOf[Ref.Type]), javaHelper.getName, builder.getSource,
          findDelegateConstructor)
      }
    }

    // -----------------------------------------------
    // Dump UML
    // -----------------------------------------------

    if (env.enabled(BoolOption.DumpCangjieUML)) {
      new UMLWriter().writeClasses(typeProvider.getAllClasses.collect {
        case t: ClassType if t.getCangjiePackage == symPackage => t
      }.toSeq)
    }
  }

}
