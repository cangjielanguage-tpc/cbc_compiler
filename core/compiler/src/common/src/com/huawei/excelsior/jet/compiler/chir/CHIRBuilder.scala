/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.google.flatbuffers.Table
import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.NO_LLVM_INDEX
import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider}
import com.huawei.excelsior.jet.compiler.cangjie.{CHIRSymLevelBuilder, CHIRVTable, CangjieEnumInfo, UMLWriter}
import com.huawei.excelsior.jet.compiler.chir.CHIRUtils.*
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.*
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{ABSTRACT, PUBLIC, STATIC}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{GenericInfo, Method, SignatureType, ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}

object CHIRBuilder {

  def parse(builder: CHIRSymLevelBuilder, resolver: CHIRResolver): Unit = {

    implicit val env: Environment = builder.env
    implicit val typeProvider: TypeProvider = env.getTypeProvider
    implicit val pkg: ParsedCHIRPackage = resolver.pkg

    // -----------------------------------------------
    // Create imported types
    // -----------------------------------------------

    def makePackage(name: String): SymClassType = {
      asClassType(resolver.findClass(name).getOrElse {
        builder.addPackage(name)
      })
    }

    val symPackage = makePackage(pkg.pkg.name)
    builder.markAsCHIRDef(symPackage)

    val symTypeDefs = Array.fill[SymClassType](pkg.pkg.defsLength)(null)

    def makeSymType(id: Long): SymClassType = {
      val i = id.toInt - 1
      if (symTypeDefs(i) == null) {
        val t = pkg.getDef[Table](id) match {
          case d: (StructDef | ClassDef | EnumDef | ExtendDef) =>
            val base = d match {
              case d: StructDef => d.base
              case d: ClassDef => d.base
              case d: EnumDef => d.base
              case d: ExtendDef => d.base
            }
            makePackage(base.packageName)

            val name = resolver.symName(d)

            val genericInfo = resolver.genericInfo(d)
            val isInterface = d match {
              case d: ClassDef => !d.isClass
              case _ => false
            }
            asClassType(resolver.symType(d).getOrElse {
              d match {
                case d: ClassDef =>
                  val modifiers = resolver.symModifiers(d.base.base.attributes).value
                  if (isInterface) {
                    builder.addInterface(symPackage, name, modifiers, isCangjie = true, genericInfo)
                  } else {
                    builder.addClass(symPackage, name, modifiers, isCangjie = true, isCangjieLambda = false, genericInfo)
                  }

                case d: StructDef =>
                  builder.addRecord(symPackage, name, genericInfo)

                case d: EnumDef =>
                  val modifiers = resolver.symModifiers(d.base.base.attributes).value

                  resolver.enumKind(d) match {
                    case EnumKind.ClassBased =>
                      // Create constructors
                      for (i <- 0 until d.ctorsLength) {
                        builder.addClass(symPackage, resolver.classBasedEnumConstructorName(name, i), modifiers, isCangjie = true, isCangjieLambda = false, genericInfo)
                      }
                    case _ =>
                  }

                  // Create main type
                  val symType = builder.addClass(symPackage, name, modifiers, isCangjie = true, isCangjieLambda = false, genericInfo)
                  builder.markAsEnum(symType)
                  symType

                case d: ExtendDef =>
                  builder.addClass(symPackage, name, Modifiers(Modifier.PUBLIC).value, isCangjie = true, isCangjieLambda = false, genericInfo)
              }
            })
        }

        symTypeDefs(i) = t
      }
      symTypeDefs(i)
    }

    for (id <- 1L to pkg.pkg.defsLength) makeSymType(id)

    // -----------------------------------------------
    // Fill symlevel type fields
    // -----------------------------------------------

    def fillFields(symType: SymClassType, d: PackageFormat.CustomTypeDef): Unit = {
      // TODO: do better
      if (SignatureType.fromSymType(symType).isCangjieLambda) {
        for (name <- Seq("$g", "$i")) {
          val f = builder.addField(symType, name, SignatureType.Int64, null, Modifiers(PUBLIC).value)
          if (symType.isCHIRDef) {
            builder.markAsCHIRDef(f, NO_LLVM_INDEX)
          }
        }
      }

      for (m <- d.instanceMemberVarsVector.iterator) {
        val name = m.name
        val sig = resolver.typeSig(m.`type`)
        val modifiers = resolver.symModifiers(m.attributes)
        val linkageName = resolver.linkageName(m)
        val sym = builder.addField(symType, name, sig, linkageName, modifiers.value)
        if (symType.isCHIRDef) {
          builder.markAsCHIRDef(sym, NO_LLVM_INDEX)
        }
      }

      for (id <- d.staticMemberVarsVector.iterator; m = pkg.getValue[GlobalVar](id)) {
        val name = resolver.symName(m)
        val sig = resolver.typeSig(m.base.base.`type`)
        val modifiers = resolver.symModifiers(m.base.base.base.attributes)
        assert(modifiers contains STATIC)
        val linkageName = resolver.linkageName(m)
        val sym = builder.addField(symType, name, sig, linkageName, modifiers.value)
        if (symType.isCHIRDef) {
          builder.markAsCHIRDef(sym, id.toInt)
        }
      }
    }

    def referenceType[T <: ReferenceType](companion: CompiledType.Companion[T])(id: Long): T =
      companion(resolver.typeSig(id))

    def resolveSuperinterface(id: Long, enclosing: Table): Option[RefInterfaceType] = {
      val interf = referenceType(RefInterfaceType)(id)
      // Skip std.core:Any from interface list
      Option.when(interf.symType.getName != "std.core:Any")(interf)
    }

    for ((id, symType) <- (1L to pkg.pkg.defsLength) zip symTypeDefs if symType != null) {

      pkg.getDef[Table](id) match {
        case d: StructDef =>
          if (!resolver.isImported(d.base)) {
            builder.markAsCHIRDef(symType)
          }
          if (!resolver.isGenericInstantiated(d.base)) {
            val superinterfaces = d.base.implementedInterfacesVector.iterator.flatMap(resolveSuperinterface(_, d)).toArray
            builder.setSuperinterfaces(symType, superinterfaces)
          }
          fillFields(symType, d.base)

        case d: ClassDef =>
          if (!resolver.isImported(d.base)) {
            builder.markAsCHIRDef(symType)
          }
          if (!resolver.isGenericInstantiated(d.base)) {
            val isInterface = symType.isInterface
            val superinterfaces = d.base.implementedInterfacesVector.iterator.flatMap(resolveSuperinterface(_, d)).toArray
            
            builder.setSuperinterfaces(symType, superinterfaces)
            if (!isInterface) {
              if (d.superClass != 0) {
                val superclass = referenceType(RefClassType)(d.superClass)
                builder.setSuperclass(symType, superclass)
              }
            }
          }
          fillFields(symType, d.base)

        case d: EnumDef =>
          val imported = resolver.isImported(d.base)
          if (!imported) {
            builder.markAsCHIRDef(symType)
          }

          if (!resolver.isGenericInstantiated(d.base)) {
            val superinterfaces = d.base.implementedInterfacesVector.iterator.flatMap(resolveSuperinterface(_, d)).toArray
            builder.setSuperinterfaces(symType, superinterfaces)
          }

          val ctorSigs = d.ctorsVector.toSeq.map(c => pkg.getType[FuncType](c.funcType))
          val ctors = ctorSigs.map(_.base.argTysVector.toSeq.init).map(_.map(resolver.typeSig))

          builder.setEnumInfo(symType, CangjieEnumInfo(ctors.map(CangjieEnumInfo.Constructor.apply)))

          def addEnumField(clazz: SymClassType, name: String, sig: SignatureType): Unit = {
            val field = builder.addField(clazz, name, sig, null, Modifiers(Modifier.PUBLIC).value)
            if (!imported) {
              builder.markAsCHIRDef(field, NO_LLVM_INDEX)
            }
          }

          resolver.enumKind(d) match {
            case EnumKind.ClassBased =>
              addEnumField(symType, "tag", SignatureType.UInt32)

              for ((types, i) <- ctors.zipWithIndex) {
                val ctorSymType = resolver.findClass(resolver.classBasedEnumConstructorName(symType.getName, i)).get
                if (!imported) {
                  builder.markAsCHIRDef(ctorSymType)
                }
                builder.setSuperclass(ctorSymType, RefClassType(symType))

                for ((t, j) <- types.zipWithIndex) {
                  addEnumField(ctorSymType, s"$$f$j", t)
                }
              }
            case _ =>
          }

        case d: ExtendDef =>
          if (!resolver.isImported(d.base)) {
            builder.markAsCHIRDef(symType)
          }
          builder.setExtendInfo(symType, resolver.typeSig(d.extendedType))
          val superinterfaces = d.base.implementedInterfacesVector.iterator.flatMap(resolveSuperinterface(_, d)).toArray
          builder.setSuperinterfaces(symType, superinterfaces)
          fillFields(symType, d.base)

      }
    }

    // -----------------------------------------------
    // Fill symlevel type methods
    // -----------------------------------------------

    // Note: Computing variable-size-type information requires that fields for all types are filled before methods

    val virtMethods = Array.fill[Method](pkg.pkg.valuesLength)(null)

    def fillMethods(symType: SymClassType, d: PackageFormat.CustomTypeDef, typeSig: SignatureType): Unit = {
      val rcvSig = typeSig match {
        case typeSig: SignatureType.OptionLikeEnum if typeSig.someType.isTypeVariable => SignatureType.Box(typeSig)
        case _ => typeSig
      }
      for (id <- d.methodsVector.iterator; m = pkg.getValue[Function](id)) {
        val name = resolver.symName(m)
        val value = m.base.base
        val mutModifiers = m.funcKind match {
          case FuncKind.STRUCT_CONSTRUCTOR | FuncKind.PRIMAL_STRUCT_CONSTRUCTOR =>
            Modifiers(Modifier.CJ_MUT)
          case _ => Modifiers.EMPTY
        }
        val modifiers = resolver.symModifiers(value.base.attributes) | mutModifiers
        val (sig, rcv, _, _) = resolver.functionSig(m, hasReceiver = !modifiers.contains(STATIC))
        val genericInfo = resolver.genericInfo(m)
        val genericFuncParamsCount = m.genericTypeParamsLength
        val hasOuterTypeInfo = true // All member functions have outer type info parameter
        val hasThisTypeInfoParam = modifiers.contains(STATIC)
        val linkageName = resolver.linkageName(m)
        val hasMutParam = rcvSig.isRecord && modifiers.contains(Modifier.CJ_MUT)
        val rcvParam = if (hasMutParam) None else rcv map {
          case t: SignatureType.OptionLikeEnum if t.someType.isTypeVariable => SignatureType.Box(t)
          case t => t
        }

        for (receiver <- rcv) {
          if (!symType.isCangjieExtend && resolver.isGenericInstantiated(d)) {
            builder.setExtendInfo(symType, receiver)
          }
        }

        // TODO: explain
        val symMethods = if (rcvSig.isVariableSizeType) {

          val mutName = resolver.mutWithoutTI(name)
          val mutLinkageName = resolver.mutWithoutTI(linkageName)
          val mutMethod = builder.addMethod(symType, mutName, sig, mutLinkageName, modifiers.value, genericInfo,
            ABI.Description(rcvParam, hasMutParam, hasThisTypeInfoParam,
            isCFunc = false, hasOuterTypeInfo, hasRetByVal = false, genericFuncParamsCount))

          val mutWrapperName = name
          val mutWrapperLinkageName = linkageName
          val mutWrapperModifiers = modifiers - Modifier.CJ_MUT
          val mutWrapperHasMutParam = false
          val mutWrapperReceiver = Some(SignatureType.Box(rcvSig))
          val mutWrapper = builder.addMethod(symType, mutWrapperName, sig, mutWrapperLinkageName, mutWrapperModifiers.value, genericInfo,
            ABI.Description(mutWrapperReceiver, mutWrapperHasMutParam, hasThisTypeInfoParam,
            isCFunc = false, hasOuterTypeInfo, hasRetByVal = false, genericFuncParamsCount))

          builder.markAsMutWrapper(mutWrapper)

          virtMethods(id.toInt) = mutWrapper

          Seq(mutMethod, mutWrapper)

        } else {
          val overrideSig = resolver.getOverrideSrcFuncType(m).map(s => resolver.functionSig(s.`type`, hasReceiver = !modifiers.contains(STATIC))._1)
          val hasRetByVal = overrideSig.exists(_.returnType.isTypeVariable)
          val symMethod = builder.addMethod(symType, name, sig, linkageName, modifiers.value, genericInfo,
            ABI.Description(rcvParam,
            hasMutParam, hasThisTypeInfoParam, isCFunc = false, hasOuterTypeInfo, hasRetByVal = hasRetByVal, genericFuncParamsCount))
          if (SignatureType.fromSymType(symType).isCangjieLambda && name == "$GenericVirtualFunc") {
            assert(symMethod.hasRetByValParameter)
          }
          virtMethods(id.toInt) = symMethod
          Seq(symMethod)
        }

        for (symMethod <- symMethods) {
          if (!symType.isCHIRDef && m.body != 0) {
            builder.markAsCHIRDef(symType)
          }
          if (symType.isCHIRDef && !resolver.isImported(m.base)) {
            builder.markAsCHIRDef(symMethod, id.toInt)
          }
          m.funcKind match {
            case FuncKind.CLASS_CONSTRUCTOR | FuncKind.PRIMAL_CLASS_CONSTRUCTOR |
                 FuncKind.STRUCT_CONSTRUCTOR | FuncKind.PRIMAL_STRUCT_CONSTRUCTOR =>
              builder.markAsConstructor(symMethod)
            case _ =>
          }
        }
      }
    }

    for ((id, symType) <- (1L to pkg.pkg.defsLength) zip symTypeDefs if symType != null) {
      pkg.getDef[Table](id) match {
        case d: StructDef => fillMethods(symType, d.base, resolver.typeSig(d.base.`type`))
        case d: ClassDef  => fillMethods(symType, d.base, resolver.typeSig(d.base.`type`))
        case d: EnumDef   => fillMethods(symType, d.base, resolver.typeSig(d.base.`type`))
        case d: ExtendDef => fillMethods(symType, d.base, resolver.typeSig(d.extendedType))
      }
    }

    // Restore abstract methods that FE changed to global (still abstract) functions

    object GlobalAbstractFunc {
      def unapply(f: Function): Option[Long] = {
        if (f.base.declaredParent == 0 && (Attribute.ABSTRACT in f.base.base.base.attributes)) {
          val funcType = pkg.getType[FuncType](f.base.base.`type`)
          Some(funcType.base.argTys(0))
        } else {
          None
        }
      }
    }

    /**
     * Sorts out redundant functions marked by diff-tool.
     * For example, the global functions are referenced from vtable.
     */
    def isDeadFunction(f: PackageFormat.Function): Boolean = {
      Attribute.UNREACHABLE in f.base.base.base.attributes
    }

    for (id <- 1L to pkg.pkg.valuesLength) pkg.getValue[Table](id) match {
      case m @ GlobalAbstractFunc(declId) if !isDeadFunction(m) =>
        val symType = asClassType(resolver.symType(pkg.getType[Table](declId)).get)
        val name = resolver.symName(m)
        val value = m.base.base
        val modifiers = resolver.symModifiers(value.base.attributes)
        assert(!modifiers.contains(STATIC), name)
        assert(modifiers.contains(ABSTRACT), name)
        val (sig, rcv, _, _) = resolver.functionSig(m, hasReceiver = !modifiers.contains(STATIC))
        val genericInfo = resolver.genericInfo(m)
        val genericFuncParamsCount = m.genericTypeParamsLength
        val hasOuterTypeInfo = true // All member functions have outer type info parameter
        val hasThisTypeInfoParam = modifiers.contains(STATIC)
        val linkageName = resolver.linkageName(m)

        val symMethod = builder.addMethod(symType, name, sig, linkageName, modifiers.value, genericInfo,
          ABI.Description(rcv, hasMutParam = false, hasThisTypeInfoParam,
          isCFunc = false, hasOuterTypeInfo, hasRetByVal = false, genericFuncParamsCount))
        virtMethods(id.toInt) = symMethod

        if (symType.isCHIRDef) {
          builder.markAsCHIRDef(symMethod, id.toInt)
        }
        m.funcKind match {
          case FuncKind.CLASS_CONSTRUCTOR | FuncKind.PRIMAL_CLASS_CONSTRUCTOR |
               FuncKind.STRUCT_CONSTRUCTOR | FuncKind.PRIMAL_STRUCT_CONSTRUCTOR =>
            builder.markAsConstructor(symMethod)
          case _ =>
        }

      case _ =>
    }

    // -----------------------------------------------
    // Fill symlevel type vtable
    // -----------------------------------------------

    def getVTable(symType: SymClassType, t: Table): CHIRVTable = {
      val customTypeDef = t match {
        case d: StructDef => d.base
        case d: ClassDef  => d.base
        case d: EnumDef   => d.base
        case d: ExtendDef => d.base
      }
      val objectExtDef = Option.when(!symType.isInterface && !symType.isRecord)(CHIRVTable.ExtDef(
        ReferenceType.cangjieStdCoreObject.sigType,
        Seq.empty
      ))
      CHIRVTable(
        objectExtDef.toSeq ++ customTypeDef.vtableVector.toSeq.map { e =>
          CHIRVTable.ExtDef(
            resolver.typeSig(e.srcParentType),
            e.virtualMethodsVector.toSeq flatMap { m =>
              val impl = pkg.getValue[Function](m.instance)
              if (isDeadFunction(impl)) {
                Seq.empty
              } else {
                val implParent = impl match {
                  case GlobalAbstractFunc(t) => pkg.getType[Table](t)
                  case impl => pkg.getDef[Table](impl.base.declaredParent)
                }
                assert(implParent != null, symType)
                val lparams = m.methodGenericTypeParamsVector
                val isStatic = resolver.symModifiers(m.attributes).contains(STATIC)
                Seq(CHIRVTable.Entry(
                  m.funcName,
                  resolver.functionSig(m.sigType, hasReceiver = false)._1, // This signature does not ever contain receiver (TODO: verify it)
                  lparams.toSeq.map(resolver.typeSig),
                  Option(virtMethods(m.instance.toInt)),
                  resolver.symModifiers(m.attributes),
                  resolver.functionSig(m.originalType, hasReceiver = !isStatic)._1,
                  resolver.typeSig(m.parentType),
                  resolver.typeSig(m.returnType),
                ))
              }
            }
          )
        }
      )
    }

    for ((id, symType) <- (1L to pkg.pkg.defsLength) zip symTypeDefs if symType != null) {
      builder.setVTable(symType, getVTable(symType, pkg.getDef[Table](id)))
    }

    // -----------------------------------------------
    // Add global vars and funcs
    // -----------------------------------------------

    for (id <- 1L to pkg.pkg.valuesLength) pkg.getValue[Table](id) match {
      case m: GlobalVar if m.base.declaredParent == 0 =>
        // package global var
        val symPkg = makePackage(m.base.packageName)
        val name = resolver.symName(m)
        val sig = resolver.typeSig(m.base.base.`type`)
        val modifiers = (resolver.symModifiers(m.base.base.base.attributes) + Modifier.STATIC).value
        val linkageName = resolver.linkageName(m)
        val symField = builder.addField(symPkg, name, sig, linkageName, modifiers)
        if (!resolver.isImported(m.base)) {
          builder.markAsCHIRDef(symField, id.toInt)
        }

      case m: Function if m.base.declaredParent == 0 && !isDeadFunction(m) =>
        // package global func
        val symPkg = makePackage(m.base.packageName)
        val name = resolver.symName(m)
        val (sig, None, isCFunc, vararg) = resolver.functionSig(m, hasReceiver = false)
        val modifiers = (resolver.symModifiers(m.base.base.base.attributes) + Modifier.STATIC).value
        val genericInfo = resolver.genericInfo(m)
        val genericFuncParamsCount = m.genericTypeParamsLength
        val linkageName = resolver.linkageName(m)
        val symMethod = builder.addMethod(symPkg, name, sig, linkageName, modifiers, genericInfo,
          ABI.Description(None, hasMutParam = false, hasThisTypeInfoParam = false,
          isCFunc, hasOuterTypeInfo = false, hasRetByVal = false, genericFuncParamsCount))
        if (pkg.pkg.packageInitFunc == id) {
          builder.markAsPackageInit(symMethod)
        }
        if (pkg.pkg.packageLiteralInitFunc == id) {
          builder.markAsPackageLiteralInit(symMethod)
        }
        if (!resolver.isImported(m.base) || m.body != 0) {
          builder.markAsCHIRDef(symMethod, id.toInt)
        }

      case _ =>
    }

    // -----------------------------------------------
    // Dump UML
    // -----------------------------------------------

    if (env.enabled(BoolOption.DumpCangjieUML)) {
      new UMLWriter().writeClasses(typeProvider.getAllClasses.collect {
        case t: SymClassType if t.getCangjiePackage == symPackage => t
      }.toSeq)
    }

  }

}
