/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.NO_LLVM_INDEX
import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider}
import com.huawei.excelsior.jet.compiler.cangjie.{CHIRSymLevelBuilder, CHIRVTable, CangjieEnumInfo, UMLWriter}
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{ABSTRACT, PUBLIC, STATIC}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{Method, SignatureType, ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}

import scala.collection.mutable

object CHIRBuilder {

  def parse(builder: CHIRSymLevelBuilder, resolver: CHIRResolver): Unit = {

    implicit val env: Environment = builder.env
    implicit val typeProvider: TypeProvider = env.getTypeProvider
    implicit val pkg: CHIR.Package = resolver.pkg

    // -----------------------------------------------
    // Create imported types
    // -----------------------------------------------

    def makePackage(name: String): SymClassType = {
      asClassType(resolver.findClass(name).getOrElse {
        builder.addPackage(name)
      })
    }

    val symPackage = makePackage(pkg.name)
    builder.markAsCHIRDef(symPackage)

    val symTypeDefs = mutable.ArrayBuffer.empty[SymClassType]

    def makeSymType(d: CHIR.CustomTypeDef): SymClassType = {
      makePackage(d.packageName)

      val name = resolver.symName(d)

      val genericInfo = resolver.genericInfo(d)
      val isInterface = d match {
        case d: CHIR.ClassDef => !d.isClass
        case _ => false
      }
      asClassType(resolver.symType(d).getOrElse {
        d match {
          case d: CHIR.ClassDef =>
            val modifiers = resolver.symModifiers(d).value
            if (isInterface) {
              builder.addInterface(symPackage, name, modifiers, isCangjie = true, genericInfo)
            } else {
              builder.addClass(symPackage, name, modifiers, isCangjie = true, isCangjieLambda = false, genericInfo)
            }

          case _: CHIR.StructDef =>
            builder.addRecord(symPackage, name, genericInfo)

          case d: CHIR.EnumDef =>
            val modifiers = resolver.symModifiers(d).value

            resolver.enumKind(d) match {
              case EnumKind.ClassBased =>
                // Create constructors
                for (i <- d.ctors.indices) {
                  builder.addClass(symPackage, resolver.classBasedEnumConstructorName(name, i), modifiers, isCangjie = true, isCangjieLambda = false, genericInfo)
                }
              case _ =>
            }

            // Create main type
            val symType = builder.addClass(symPackage, name, modifiers, isCangjie = true, isCangjieLambda = false, genericInfo)
            builder.markAsEnum(symType)
            symType

          case _: CHIR.ExtendDef =>
            builder.addClass(symPackage, name, Modifiers(Modifier.PUBLIC).value, isCangjie = true, isCangjieLambda = false, genericInfo)
        }
      })
    }

    for (d <- pkg.typeDefs) symTypeDefs += makeSymType(d)

    // -----------------------------------------------
    // Fill symlevel type fields
    // -----------------------------------------------

    def fillFields(symType: SymClassType, d: CHIR.CustomTypeDef): Unit = {
      // TODO: do better
      if (SignatureType.fromSymType(symType).isCangjieLambda) {
        for (name <- Seq("$g", "$i")) {
          val f = builder.addField(symType, name, SignatureType.Int64, null, Modifiers(PUBLIC).value)
          if (symType.isCHIRDef) {
            builder.markAsCHIRDef(f, NO_LLVM_INDEX)
          }
        }
      }

      for (v <- d.instanceVars) {
        val name = v.name
        val sig = resolver.typeSig(v.tpe)
        val modifiers = resolver.symModifiers(v)
        val linkageName = resolver.linkageName(v)
        val sym = builder.addField(symType, name, sig, linkageName, modifiers.value)
        if (symType.isCHIRDef) {
          builder.markAsCHIRDef(sym, NO_LLVM_INDEX)
        }
      }

      for (v <- d.staticVars) {
        val name = resolver.symName(v)
        val sig = resolver.typeSig(v.tpe)
        val modifiers = resolver.symModifiers(v)
        assert(modifiers contains STATIC)
        val linkageName = resolver.linkageName(v)
        val sym = builder.addField(symType, name, sig, linkageName, modifiers.value)
        if (symType.isCHIRDef) {
          builder.markAsCHIRDef(sym, v.id.toInt)
        }
      }
    }

    def referenceType[T <: ReferenceType](companion: CompiledType.Companion[T])(it: CHIR.ClassType): T =
      companion(resolver.typeSig(it))

    def resolveSuperinterface(it: CHIR.ClassType, enclosing: CHIR.CustomTypeDef): Option[RefInterfaceType] = {
      val interf = referenceType(RefInterfaceType)(it)
      // Skip std.core:Any from interface list
      Option.when(interf.symType.getName != "std.core:Any")(interf)
    }

    for ((d, symType) <- pkg.typeDefs zip symTypeDefs if symType != null) {
      d match {
        case d: CHIR.StructDef =>
          if (!resolver.isImported(d)) {
            builder.markAsCHIRDef(symType)
          }
          if (!resolver.isGenericInstantiated(d)) {
            val superinterfaces = d.implementedInterfaces.flatMap(resolveSuperinterface(_, d)).toArray
            builder.setSuperinterfaces(symType, superinterfaces)
          }
          fillFields(symType, d)

        case d: CHIR.ClassDef =>
          if (!resolver.isImported(d)) {
            builder.markAsCHIRDef(symType)
          }
          if (!resolver.isGenericInstantiated(d)) {
            val isInterface = symType.isInterface
            val superinterfaces = d.implementedInterfaces.flatMap(resolveSuperinterface(_, d)).toArray
            
            builder.setSuperinterfaces(symType, superinterfaces)
            if (!isInterface) {
              d.superClass.foreach { sc =>
                val superclass = referenceType(RefClassType)(sc)
                builder.setSuperclass(symType, superclass)
              }
            }
          }
          fillFields(symType, d)

        case d: CHIR.EnumDef =>
          val imported = resolver.isImported(d)
          if (!imported) {
            builder.markAsCHIRDef(symType)
          }

          if (!resolver.isGenericInstantiated(d)) {
            val superinterfaces = d.implementedInterfaces.flatMap(resolveSuperinterface(_, d)).toArray
            builder.setSuperinterfaces(symType, superinterfaces)
          }

          val ctorSigs = d.ctors.map(_.tpe)
          val ctors = ctorSigs.map(_.paramTypes).map(_.map(resolver.typeSig))

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

        case d: CHIR.ExtendDef =>
          if (!resolver.isImported(d)) {
            builder.markAsCHIRDef(symType)
          }
          builder.setExtendInfo(symType, resolver.typeSig(d.tpe))
          val superinterfaces = d.implementedInterfaces.iterator.flatMap(resolveSuperinterface(_, d)).toArray
          builder.setSuperinterfaces(symType, superinterfaces)
          fillFields(symType, d)

      }
    }

    // -----------------------------------------------
    // Fill symlevel type methods
    // -----------------------------------------------

    // Note: Computing variable-size-type information requires that fields for all types are filled before methods

    val virtMethods = mutable.HashMap.empty[CHIR.Func, Method]

    def fillMethods(symType: SymClassType, d: CHIR.CustomTypeDef, typeSig: SignatureType): Unit = {
      val rcvSig = typeSig match {
        case typeSig: SignatureType.OptionLikeEnum if typeSig.someType.isTypeVariable => SignatureType.Box(typeSig)
        case _ => typeSig
      }
      for (m <- d.methods) {
        val name = resolver.symName(m)
        val mutModifiers = m.kind match {
          case CHIR.Func.Kind.StructCtor | CHIR.Func.Kind.PrimalStructCtor =>
            Modifiers(Modifier.CJ_MUT)
          case _ => Modifiers.EMPTY
        }
        val modifiers = resolver.symModifiers(m) | mutModifiers
        val (sig, rcv, _, _) = resolver.functionSig(m, hasReceiver = !modifiers.contains(STATIC))
        val genericInfo = resolver.genericInfo(m)
        val genericFuncParamsCount = m.genericTypeParams.size
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

          virtMethods(m) = mutWrapper

          Seq(mutMethod, mutWrapper)

        } else {
          val overrideSig = resolver.getOverrideSrcFuncType(m).map(s => resolver.functionSig(s.tpe, hasReceiver = !modifiers.contains(STATIC))._1)
          val hasRetByVal = overrideSig.exists(_.returnType.isTypeVariable)
          val symMethod = builder.addMethod(symType, name, sig, linkageName, modifiers.value, genericInfo,
            ABI.Description(rcvParam,
            hasMutParam, hasThisTypeInfoParam, isCFunc = false, hasOuterTypeInfo, hasRetByVal = hasRetByVal, genericFuncParamsCount))
          if (SignatureType.fromSymType(symType).isCangjieLambda && name == "$GenericVirtualFunc") {
            assert(symMethod.hasRetByValParameter)
          }
          virtMethods(m) = symMethod
          Seq(symMethod)
        }

        for (symMethod <- symMethods) {
          if (!symType.isCHIRDef && m.body != 0) {
            builder.markAsCHIRDef(symType)
          }
          if (symType.isCHIRDef && !resolver.isImported(m)) {
            builder.markAsCHIRDef(symMethod, m.id.toInt)
          }
          m.kind match {
            case CHIR.Func.Kind.ClassCtor | CHIR.Func.Kind.PrimalClassCtor |
                 CHIR.Func.Kind.StructCtor | CHIR.Func.Kind.PrimalStructCtor =>
              builder.markAsConstructor(symMethod)
            case _ =>
          }
        }
      }
    }

    for ((d, symType) <- pkg.typeDefs zip symTypeDefs if symType != null) {
      fillMethods(symType, d, resolver.typeSig(d.tpe))
    }

    // Restore abstract methods that FE changed to global (still abstract) functions

    object GlobalAbstractFunc {
      def unapply(f: CHIR.Func): Option[CHIR.Type] = {
        if (f.declaringDef.isEmpty && f.attributes.contains(CHIR.Attribute.Abstract)) {
          val funcType = f.tpe
          Some(funcType.receiverType)
        } else {
          None
        }
      }
    }

    for (v <- pkg.values) v match {
      case m @ GlobalAbstractFunc(declType) if !resolver.isDeadFunction(m) =>
        val symType = asClassType(resolver.symType(declType).get)
        val name = resolver.symName(m)
        val modifiers = resolver.symModifiers(m)
        assert(!modifiers.contains(STATIC), name)
        assert(modifiers.contains(ABSTRACT), name)
        val (sig, rcv, _, _) = resolver.functionSig(m, hasReceiver = !modifiers.contains(STATIC))
        val genericInfo = resolver.genericInfo(m)
        val genericFuncParamsCount = m.genericTypeParams.size
        val hasOuterTypeInfo = true // All member functions have outer type info parameter
        val hasThisTypeInfoParam = modifiers.contains(STATIC)
        val linkageName = resolver.linkageName(m)

        val symMethod = builder.addMethod(symType, name, sig, linkageName, modifiers.value, genericInfo,
          ABI.Description(rcv, hasMutParam = false, hasThisTypeInfoParam,
          isCFunc = false, hasOuterTypeInfo, hasRetByVal = false, genericFuncParamsCount))
        virtMethods(m) = symMethod

        if (symType.isCHIRDef) {
          builder.markAsCHIRDef(symMethod, m.id.toInt)
        }
        m.kind match {
          case CHIR.Func.Kind.ClassCtor | CHIR.Func.Kind.PrimalClassCtor |
               CHIR.Func.Kind.StructCtor | CHIR.Func.Kind.PrimalStructCtor =>
            builder.markAsConstructor(symMethod)
          case _ =>
        }

      case _ =>
    }

    // -----------------------------------------------
    // Fill symlevel type vtable
    // -----------------------------------------------

    def getVTable(symType: SymClassType, d: CHIR.CustomTypeDef): CHIRVTable = {
      val objectExtDef = Option.when(!symType.isInterface && !symType.isRecord)(CHIRVTable.ExtDef(
        ReferenceType.cangjieStdCoreObject.sigType,
        Seq.empty
      ))
      CHIRVTable(
        objectExtDef.toSeq ++ d.vTables.map { e =>
          CHIRVTable.ExtDef(
            resolver.typeSig(e.srcParentType),
            e.vMethods flatMap { m =>
              val impl = m.instance
              if (resolver.isDeadFunction(impl)) {
                Seq.empty
              } else {
                val implParent = impl match {
                  case GlobalAbstractFunc(t) => t
                  case impl => impl.declaringDef.get
                }
                assert(implParent != null, symType)
                val lparams = m.genericTypeParams
                val mods = resolver.symModifiers(m)
                val isStatic = mods.contains(STATIC)
                Seq(CHIRVTable.Entry(
                  m.name,
                  resolver.functionSig(m.sig, hasReceiver = false)._1, // This signature does not ever contain receiver (TODO: verify it)
                  lparams.map(resolver.typeSig),
                  Option(virtMethods(m.instance)),
                  mods,
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

    for ((d, symType) <- pkg.typeDefs zip symTypeDefs if symType != null) {
      builder.setVTable(symType, getVTable(symType, d))
    }

    // -----------------------------------------------
    // Add global vars and funcs
    // -----------------------------------------------

    for (v <- pkg.values) v match {
      case m: CHIR.GlobalVar if m.declaringDef.isEmpty =>
        // package global var
        val symPkg = makePackage(m.packageName)
        val name = resolver.symName(m)
        val sig = resolver.typeSig(m.tpe)
        val modifiers = (resolver.symModifiers(m) + Modifier.STATIC).value
        val linkageName = resolver.linkageName(m)
        val symField = builder.addField(symPkg, name, sig, linkageName, modifiers)
        if (!resolver.isImported(m)) {
          builder.markAsCHIRDef(symField, m.id.toInt)
        }

      case m: CHIR.Func if m.declaringDef.isEmpty && !resolver.isDeadFunction(m) =>
        // package global func
        val symPkg = makePackage(m.packageName)
        val name = resolver.symName(m)
        val (sig, None, isCFunc, vararg) = resolver.functionSig(m, hasReceiver = false)
        val modifiers = (resolver.symModifiers(m) + Modifier.STATIC).value
        val genericInfo = resolver.genericInfo(m)
        val genericFuncParamsCount = m.genericTypeParams.size
        val linkageName = resolver.linkageName(m)
        val symMethod = builder.addMethod(symPkg, name, sig, linkageName, modifiers, genericInfo,
          ABI.Description(None, hasMutParam = false, hasThisTypeInfoParam = false,
          isCFunc, hasOuterTypeInfo = false, hasRetByVal = false, genericFuncParamsCount))
        if (pkg.packageInitFunc == m) {
          builder.markAsPackageInit(symMethod)
        }
        if (pkg.packageInitLiteralFunc == m) {
          builder.markAsPackageLiteralInit(symMethod)
        }
        if (!resolver.isImported(m) || m.body.nonEmpty) {
          builder.markAsCHIRDef(symMethod, m.id.toInt)
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
