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
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.NO_LLVM_INDEX
import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider}
import com.huawei.excelsior.jet.compiler.cangjie.{CHIRSymLevelBuilder, CHIRVTable, UMLWriter}
import com.huawei.excelsior.jet.compiler.chir.CHIRUtils.*
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.*
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{PUBLIC, STATIC}
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

            if (!resolver.isGenericInstantiated(base)) {
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
                  case d: (StructDef | EnumDef) => // TODO: support proper Enum
                    builder.addRecord(symPackage, name, genericInfo)
                  case d: ExtendDef =>
                    builder.addClass(symPackage, name, Modifiers(Modifier.PUBLIC).value, isCangjie = true, isCangjieLambda = false, genericInfo)
                }
              })
            } else {
              // generic_instantiated
              null
            }
        }

        symTypeDefs(i) = t
      }
      symTypeDefs(i)
    }

    for (id <- 1L to pkg.pkg.defsLength) makeSymType(id)

    // -----------------------------------------------
    // Add global vars and funcs, create imported packages
    // -----------------------------------------------

    for (id <- 1L to pkg.pkg.valuesLength) pkg.getValue[Table](id) match {
      case m: GlobalVar if m.base.declaredParent == 0 =>
        // package global var
        val symPkg = makePackage(m.base.packageName)
        val name = resolver.symName(m)
        val sig = resolver.typeSig(m.base.base.`type`, m)
        val modifiers = (resolver.symModifiers(m.base.base.base.attributes) + Modifier.STATIC).value
        val linkageName = resolver.linkageName(m)
        val symField = builder.addField(symPkg, name, sig, linkageName, modifiers)
        if (!resolver.isImported(m.base)) {
          builder.markAsCHIRDef(symField, id.toInt)
        }

      case m: Function if m.base.declaredParent == 0 =>
        // package global func
        val symPkg = makePackage(m.base.packageName)
        val name = resolver.symName(m)
        val (sig, isCFunc, vararg) = resolver.functionSig(m, hasReceiver = false)
        val modifiers = (resolver.symModifiers(m.base.base.base.attributes) + Modifier.STATIC).value
        val genericInfo = resolver.genericInfo(m)
        val genericFuncParamsCount = m.genericTypeParamsLength
        val linkageName = resolver.linkageName(m)
        val symMethod = builder.addMethod(symPkg, name, sig, linkageName, modifiers, genericInfo,
          hasUGDesc = false, hasThisTypeInfoParam = false, isCFunc, hasOuterTypeInfo = false, genericFuncParamsCount,
          isMutWrapper = false)
        if (pkg.pkg.packageInitFunc == id) {
          builder.markAsPackageInit(symMethod)
        }
        if (pkg.pkg.packageLiteralInitFunc == id) {
          builder.markAsPackageLiteralInit(symMethod)
        }
        if (!resolver.isImported(m.base)) {
          builder.markAsCHIRDef(symMethod, id.toInt)
        }

      case _ =>
    }

    // -----------------------------------------------
    // Fill symlevel type fields
    // -----------------------------------------------

    def fillFields(symType: SymClassType, d: PackageFormat.CustomTypeDef): Unit = {
      for (m <- d.instanceMemberVarsVector.iterator) {
        val name = m.name
        val sig = resolver.typeSig(m.`type`, d)
        val modifiers = resolver.symModifiers(m.attributes)
        val linkageName = resolver.linkageName(m)
        val sym = builder.addField(symType, name, sig, linkageName, modifiers.value)
        if (symType.isCHIRDef) {
          builder.markAsCHIRDef(sym, NO_LLVM_INDEX)
        }
      }

      for (id <- d.staticMemberVarsVector.iterator; m = pkg.getValue[GlobalVar](id)) {
        val name = resolver.symName(m)
        val sig = resolver.typeSig(m.base.base.`type`, d)
        val modifiers = resolver.symModifiers(m.base.base.base.attributes)
        assert(modifiers contains STATIC)
        val linkageName = resolver.linkageName(m)
        val sym = builder.addField(symType, name, sig, linkageName, modifiers.value)
        if (symType.isCHIRDef) {
          builder.markAsCHIRDef(sym, id.toInt)
        }
      }
    }

    def referenceType[T <: ReferenceType](companion: CompiledType.Companion[T])(id: Long, enclosing: Table): T =
      companion(resolver.typeSig(id, enclosing))

    def resolveSuperinterface(id: Long, enclosing: Table): Option[RefInterfaceType] = {
      val interf = referenceType(RefInterfaceType)(id, enclosing)
      // Skip std.core:Any from interface list
      Option.when(interf.symType.getName != "std.core:Any")(interf)
    }

    for ((id, symType) <- (1L to pkg.pkg.defsLength) zip symTypeDefs if symType != null) {

      pkg.getDef[Table](id) match {
        case d: StructDef =>
          if (!resolver.isImported(d.base)) {
            builder.markAsCHIRDef(symType)
          }
          val superinterfaces = d.base.implementedInterfacesVector.iterator.flatMap(resolveSuperinterface(_, d)).toArray
          builder.setSuperinterfaces(symType, superinterfaces)
          fillFields(symType, d.base)

        case d: ClassDef =>
          if (!resolver.isImported(d.base)) {
            builder.markAsCHIRDef(symType)
          }
          val isInterface = symType.isInterface
          val superinterfaces = d.base.implementedInterfacesVector.iterator.flatMap(resolveSuperinterface(_, d)).toArray
          builder.setSuperinterfaces(symType, superinterfaces)
          if (!isInterface) {
            if (d.superClass != 0) {
              val superclass = referenceType(RefClassType)(d.superClass, d)
              builder.setSuperclass(symType, superclass)
            }
          }
          fillFields(symType, d.base)

        case d: EnumDef =>
          resolver.enumKind(d) match {
            case EnumKind.ZeroSized => // nothing to do
            case EnumKind.PrimitiveBased => // nothing to do
            case EnumKind.OptionLike(base) =>
              resolver.typeSig(base, d) match {
                case t: SignatureType.NullableWrapper.Base => // nothing to do - always nullable
                case t =>
                  builder.addField(symType, "tag", SignatureType.Boolean, null, Modifiers(Modifier.PUBLIC).value)
                  builder.addField(symType, "payload", t, null, Modifiers(Modifier.PUBLIC).value)
              }
            case EnumKind.UnionBased =>
              builder.addField(symType, "tag", SignatureType.UInt32, null, Modifiers(Modifier.PUBLIC).value)
            // TODO: add fields from largest constructor
            case EnumKind.ClassBased => notImplemented(s"class-based enum ${d.base.identifier}")
          }

        case d: ExtendDef =>
          if (!resolver.isImported(d.base)) {
            builder.markAsCHIRDef(symType)
          }
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

    def fillMethods(symType: SymClassType, d: PackageFormat.CustomTypeDef): Unit = {
      for (id <- d.methodsVector.iterator; m = pkg.getValue[Function](id)) {
        val name = resolver.symName(m)
        val value = m.base.base
        val mutModifiers = m.funcKind match {
          case FuncKind.STRUCT_CONSTRUCTOR | FuncKind.PRIMAL_STRUCT_CONSTRUCTOR =>
            Modifiers(Modifier.CJ_MUT)
          case _ => Modifiers.EMPTY
        }
        val extendModifiers = if (resolver.isExtendedBaseFunc(m)) Modifiers(STATIC) else Modifiers.EMPTY
        val modifiers = resolver.symModifiers(value.base.attributes) | extendModifiers | mutModifiers
        val (sig, _, _) = resolver.functionSig(m, hasReceiver = !modifiers.contains(STATIC))
        val genericInfo = resolver.genericInfo(m)
        val genericFuncParamsCount = m.genericTypeParamsLength
        val hasOuterTypeInfo = true // All member functions have outer type info parameter
        val hasThisTypeInfoParam = modifiers.contains(STATIC)
        val linkageName = resolver.linkageName(m)

        // TODO: explain
        val symMethods = if (symType.isVariableSizeType && modifiers.contains(Modifier.CJ_MUT)) {

          val mutName = resolver.mutWithoutTI(name)
          val mutLinkageName = resolver.mutWithoutTI(linkageName)
          val mutMethod = builder.addMethod(symType, mutName, sig, mutLinkageName, modifiers.value, genericInfo,
            hasUGDesc = false, hasThisTypeInfoParam, isCFunc = false, hasOuterTypeInfo, genericFuncParamsCount,
            isMutWrapper = false)

          val mutWrapperName = name
          val mutWrapperLinkageName = linkageName
          val mutWrapperModifiers = modifiers - Modifier.CJ_MUT
          val mutWrapper = builder.addMethod(symType, mutWrapperName, sig, mutWrapperLinkageName, mutWrapperModifiers.value, genericInfo,
            hasUGDesc = false, hasThisTypeInfoParam, isCFunc = false, hasOuterTypeInfo, genericFuncParamsCount,
            isMutWrapper = true)

          virtMethods(id.toInt) = mutWrapper

          Seq(mutMethod, mutWrapper)

        } else {
          val symMethod = builder.addMethod(symType, name, sig, linkageName, modifiers.value, genericInfo,
            hasUGDesc = false, hasThisTypeInfoParam, isCFunc = false, hasOuterTypeInfo, genericFuncParamsCount,
            isMutWrapper = false)
          virtMethods(id.toInt) = symMethod
          Seq(symMethod)
        }

        for (symMethod <- symMethods) {
          if (symType.isCHIRDef) {
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
        case d: StructDef => fillMethods(symType, d.base)
        case d: ClassDef  => fillMethods(symType, d.base)
        case d: EnumDef   => fillMethods(symType, d.base)
        case d: ExtendDef => fillMethods(symType, d.base)
      }
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
            resolver.typeSig(e.srcParentType, t),
            e.virtualMethodsVector.toSeq map { m =>
              val cparams = resolver.withGenericParams(pkg.getDef[Table](pkg.getValue[Function](m.instance).base.declaredParent))(identity)
              val lparams = m.methodGenericTypeParamsVector
              val isStatic = resolver.symModifiers(m.attributes).contains(STATIC)
              CHIRVTable.Entry(
                m.funcName,
                resolver.functionSig(m.sigType, t, hasReceiver = false)._1, // This signature does not ever contain receiver (TODO: verify it)
                lparams.toSeq.map(resolver.typeSig(_, t)),
                Option(virtMethods(m.instance.toInt)),
                resolver.symModifiers(m.attributes),
                resolver.functionSig(m.originalType, cparams, lparams, hasReceiver = !isStatic)._1,
                resolver.typeSig(m.parentType, t),
                resolver.typeSig(m.returnType, t),
              )
            }
          )
        }
      )
    }

    for ((id, symType) <- (1L to pkg.pkg.defsLength) zip symTypeDefs if symType != null) {
      builder.setVTable(symType, getVTable(symType, pkg.getDef[Table](id)))
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
