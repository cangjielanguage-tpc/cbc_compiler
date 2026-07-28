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
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.{CONSTRUCTOR_NAME, STD_CORE_ANY_LINKAGE_NAME, boxName, extensionName, makeSyntheticModuleName}
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.Ref
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.Ref.HasName
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.*
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{Address, BString, Primitive, LocalTypeVariable}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, GenericInfo, MethodSignature, Signature, SignatureType, Method as SymMethod, Type as SymType}
import com.huawei.excelsior.jet.compiler.{Env, Environment, TypeProvider}

import scala.PartialFunction.condOpt
import scala.collection.mutable

/** Provides resolving utilities to/from [[HLIRMetadata.Ref]]s.
  *
  * TODO: consider moving resolve of bitcode globals and functions to some other place
  *       or removing "SymLevel" specific from this class name.
  *
  * @author liontiger
  */
class HLIRSymLevelResolver(val hlir: HLIRMetadata, loadPDB: Boolean)(private implicit val env: Environment) {
  private implicit val typeProvider: TypeProvider = env.getTypeProvider

  private val symTypeByRef = mutable.LinkedHashMap.empty[Ref, SymType]
  def symType(ref: Ref)(implicit reporter: HLIRErrorReporter): Option[SymType] = symTypeByRef.get(ref) orElse {
    val res: Option[SymType] = ref match {
      case t: Ref.Package => findClass(symName(t))

      case t: Ref.Type => t match {
        case t: Ref.RawEnum => symType(t.baseType)

        case t: Ref.HasName => findClass(symName(t))

        case t: Ref.VArray => symVArrayType(t)

        case t: Ref.Primitive => Some(t.asSignatureType.symType)

        case t: Ref.Array => symArrayType(t, t.elemType)

        case t: Ref.ArraySlice => symArraySliceType(t, t.elemType)

        case t: Ref.JavaArray =>
          symType(t.baseType) map { baseSymType =>
            typeProvider.getArrayType(baseSymType, t.dimNum)
          }

        case t: Ref.Nullable => symType(t.referenceType)

        case Ref.CPointer(_) | Ref.CString => Some(Address.symType)

        case _: Ref.TypeVariable | _: Ref.OwnTypeVariable | _: Ref.Instantiated[?] =>
          shouldNotReachHere(ref)
      }

      case t: (Ref.GenericClass | Ref.GenericInterface | Ref.GenericRecord) =>
        findClass(symName(t))

      case t: Ref.Box =>
        findClass(boxName(refSignature(t, t.baseType)))

      case t: Ref.InterfaceExtension =>
        findClass(extensionName(refSignature(t, t.baseType), t.interfaces.map(i => refSignature(t, i))))

      case _: (Ref.InstantiatedInterfaceExtension | Ref.GenericInterfaceExtension) =>
        shouldNotReachHere(s"not implemented $ref")

      case Ref.ThisType =>
        shouldNotReachHere(s"not implemented $ref")

      case _: Ref.FunctionalType =>
        shouldNotReachHere(ref) // TODO: make Ref.Type when lambda and functional types are supported in HLIR

      case _: Ref.MemberRef | _: Ref.Global | _: Ref.ForeignCFunction |
           _: Ref.ConstantString | _: Ref.Parameter | _: Ref.Annotation | _: Ref.JavaAnnotationRelated |
           _: Ref.Generic | _: Ref.Instantiated[?] | _: Ref.TypeParameter | _: Ref.GenericConstraints =>
        shouldNotReachHere(ref)
    }

    for (t <- res) {
      assert(t != null, s"unexpected Some(null) while resolving symlevel type of $ref")
      symTypeByRef(ref) = t
    }

    res
  }

  def symArraySliceType(outer: Ref, elemType: Ref.Type)(implicit reporter: HLIRErrorReporter): Option[SymType] = {
    findClass(SignatureType.ArraySlice.name(refSignature(outer, elemType)))
  }

  def symArrayType(outer: Ref, elemType: Ref.Type)(implicit reporter: HLIRErrorReporter): Option[SymType] = {
    findClass(SignatureType.CangjieArray.name(refSignature(outer, elemType)))
  }

  def symVArrayType(t: Ref.VArray)(implicit reporter: HLIRErrorReporter): Option[SymType] = {
    symType(t.elemType) flatMap { elemSymType => // TODO: use refSignature instead of symType with erasure
      findClass(SignatureType.VArray.name(SignatureType.fromSymType(elemSymType), t.length))
    }
  }

  def findClass(className: String): Option[ClassType] = {
    Option(typeProvider.findClass(xstr(className), loadPDB))
  }


  private lazy val funcByRef = Map.from(hlir.module.functions collect {
    case x if !hlir.isIntrinsic(x.name) => (hlir.ref(x.name).get, x)
  })
  def function(ref: Ref): Option[Bitcode.Function] = funcByRef.get(ref)

  private lazy val globalByRef = Map.from(hlir.module.globals map (x => (hlir.ref(x.name).get, x)))
  def global(ref: Ref): Option[Bitcode.Global] = globalByRef.get(ref)

  /** Returns type parameters of generic entity, including generic type parameters of enclosing type. */
  def genericTypeParameters(ref: Ref.Generic): Seq[Ref.TypeParameter] = {
    val refType = condOpt(ref) { case ref: Ref.MethodRef => ref.refType }
    refType match {
      case Some(refType: Ref.Generic) => refType.typeParameters ++ ref.typeParameters // TODO: ensure no duplicates
      case _ => ref.typeParameters
    }
  }

  /** Returns whether given method or global function has UG_Desc parameter. */
  def hasUGDescParameter(ref: Ref.MethodDef | Ref.Global)(implicit reporter: HLIRErrorReporter): Boolean = ref match {
    case _: Ref.Generic => true                                                              // any generic method or global function
    case ref: Ref.StaticMethod => ref.refType.isInstanceOf[Ref.Generic]                      // any generic type
    case ref: Ref.InstanceMethod => ref.refType.isInstanceOf[Ref.GenericRecord]              // only generic records
    case _: Ref.Global => false
  }

  /** Returns whether given method or global function has ThisTypeInfo parameter. */
  def hasThisTypeInfoParameter(ref: Ref.MethodDef)(implicit reporter: HLIRErrorReporter): Boolean = ref match {
    case ref: Ref.StaticMethod => ref.name != CONSTRUCTOR_NAME
    case _ => false
  }

  /** For non-generics returns [[GenericInfo.none]] which is null. */
  def genericInfo(ref: Ref)(implicit reporter: HLIRErrorReporter): GenericInfo = ref match {
    case ref: Ref.Generic => genericInfo(ref)
    case _ => GenericInfo.none
  }

  /** Returns type parameter constraints of generic entity, including generic type parameters of enclosing type. */
  def genericInfo(ref: Ref.Generic)(implicit reporter: HLIRErrorReporter): GenericInfo = {

    def typeParamsWithConstraints(g: Ref.Generic): GenericInfo = {
      val constraints = g.constraints

      def upperBounds(p: Ref.TypeParameter): Seq[SignatureType] = constraints collectFirst {
        case t if t.typeVariable.param == p => t.upperBounds map (t => signature[SignatureType](ref, t, SignatureType.Nothing))
      } getOrElse Seq.empty

      GenericInfo(g.typeParameters.zipWithIndex map { (x, idx) => GenericInfo.Constraint(LocalTypeVariable(idx), upperBounds(x)) })
    }

    val refType = condOpt(ref) {
      case ref: Ref.GenericInstanceMethod => ref.refType
      case ref: Ref.GenericStaticMethod => ref.refType
    }
    refType match {
      case Some(refType: Ref.Generic) => typeParamsWithConstraints(refType) ++ typeParamsWithConstraints(ref) // TODO: ensure no duplicates
      case _ => typeParamsWithConstraints(ref)
    }
  }

  /** Returns type parameter substitutions of instantiated entity, including substitutions of enclosing type. */
  def instantiatedTypeParameters(ref: Ref.Instantiated[?]): Seq[Ref.Type] = {
    val refType = condOpt(ref) { case ref: Ref.MethodRef => ref.refType }
    val params = refType match {
      case Some(refType: Ref.Instantiated[?]) => refType.instantiatedTypeParameters ++ ref.instantiatedTypeParameters
      case _ => ref.instantiatedTypeParameters
    }
    params collect { case x: Ref.Type => x }
  }

  /** Returns type parameter substitutions of instantiated entity, including substitutions of enclosing type. */
  def instantiatedTypeParameterSignatures(ref: Ref.Instantiated[?])(implicit reporter: HLIRErrorReporter): Seq[SignatureType] = {
    instantiatedTypeParameters(ref).map(t => signature[SignatureType](ref, t, SignatureType.Nothing))
  }

  private def signature[S <: Signature](ref: Ref, sig: Ref.Sig, fallback: => S)(implicit reporter: HLIRErrorReporter): S = {
    val javaSig = ref match {
      case ref: Ref.Java => true
      case ref: Ref.MemberRef => ref.refType.isInstanceOf[Ref.Java]
      case _ => false
    }

    val checkNonJava: Ref.Sig => Unit = sig => {
      if (javaSig) {
        reporter.parsingError(s"non-Java type $sig in Java signature", ref.md)
        return fallback
      }
    }

    import SignatureType.*

    def maybeNullable(t: NullableWrapper.Base): SignatureType = {
      if (javaSig) {
        NullableWrapper(t)
      } else {
        t
      }
    }

    def maybeNonNullable(t: NonNullableWrapper.Base): SignatureType = {
      if (javaSig) {
        t
      } else {
        NonNullableWrapper(t)
      }
    }

    def typeVariableSig(host: Ref.Generic, typeParam: Ref.TypeParameter): SignatureType = {
      val typeParams = ref match {
        case ref: (Ref.GenericInstanceMethod | Ref.GenericStaticMethod) =>
          // Note: generic type parameters of outer type are moved to enclosed generic method,
          //       so we need to use its type parameter numbering instead of host.
          assert(host == ref || host == ref.refType, s"inconsistent type variable host ${host.md} use in ${ref.md}")
          genericTypeParameters(ref)
        case _ =>
          genericTypeParameters(host)
      }
      LocalTypeVariable(typeParams.indexOf(typeParam))
    }

    def convert[T <: Signature](sig: Ref.Sig): T = (sig match {
      case sig: Ref.Primitive =>
        val prim = sig.asSignatureType
        prim match {
          case Nothing | Unit => if (javaSig) Void else prim // TODO: replace with assertion when FE stops using non-Java types in Java signatures
          case Void | Boolean | Int8 | Int16 | UInt16 | Int32 | Int64 | Float32 | Float64 => prim
          case UInt8 | UnicodeChar32 | UInt32 | UInt64 | AddrInt | AddrUInt | Float16 =>
            checkNonJava(sig)
            prim
        }
      case sig: Ref.Instantiated[?] =>
        checkNonJava(sig)
        val typeParams = sig.instantiatedTypeParameters collect { case t: Ref.Type => convert[SignatureType](t) }
        sig.generic match {
          case generic: Ref.HasRecordDef                        => InstantiatedRecord(symName(generic), typeParams)
          case generic: (Ref.HasClassDef | Ref.HasInterfaceDef) => InstantiatedReference(symName(generic), typeParams)
          case _: Ref.GenericInstanceMethod | _: Ref.GenericStaticMethod | _: Ref.GenericGlobalFunction | _: Ref.GenericInterfaceExtension => shouldNotReachHere(sig)
        }
      case sig: Ref.TypeVariable    => checkNonJava(sig); typeVariableSig(sig.generic, sig.param)
      case sig: Ref.OwnTypeVariable => checkNonJava(sig); ref match {
        case ref: Ref.Generic => typeVariableSig(ref, sig.param)
        case ref => reporter.parsingError(s"unexpected ${sig.md} in non-generic function", ref.md)
      }
      case Ref.CString              => checkNonJava(sig); BString
      case sig: Ref.CPointer        => checkNonJava(sig); CPointer(convert(sig.pointee))
      case sig: Ref.HasRecordDef    => checkNonJava(sig); Record(symName(sig))
      case sig: Ref.JavaClass       => maybeNonNullable(  JBCReference(symName(sig)))
      case sig: Ref.HasClassDef     => maybeNullable   (  CangjieReference(symName(sig)))
      case sig: Ref.JavaInterface   => maybeNonNullable(  JBCReference(symName(sig)))
      case sig: Ref.HasInterfaceDef => maybeNullable   (  CangjieReference(symName(sig)))
      case sig: Ref.ArraySlice      => checkNonJava(sig); ArraySlice(convert(sig.elemType))
      case sig: Ref.Array           => checkNonJava(sig); CangjieArray(convert(sig.elemType))
      case sig: Ref.JavaArray       => maybeNonNullable(  JavaArray(convert(sig.baseType)))
      case sig: Ref.RawEnum         => checkNonJava(sig); CangjieEnumWrapper(convert(sig.baseType), sig.name)
      case sig: Ref.Nullable        =>                    convert[Signature](sig.referenceType) match {
        case NonNullableWrapper(base)                             => base
        case n: NullableWrapper.Base                              => NullableWrapper(n)
        case CangjieEnumWrapper(base: NullableWrapper.Base, name) => CangjieEnumWrapper(NullableWrapper(base), name)
      }
      case sig: Ref.FunctionalType  =>                    MethodSignature(convert(sig.returnType), sig.parameterTypes.map(convert[SignatureType]))
      case sig: Ref.VArray          =>                    VArray(convert(sig.elemType), sig.length)
      case sig: Ref.Box             => checkNonJava(sig); CangjieReference(boxName(refSignature(sig, sig.baseType))) // TODO: introduce distinct signature?
      case sig: Ref.InterfaceExtension => checkNonJava(sig); CangjieReference(extensionName(refSignature(sig, sig.baseType), sig.interfaces.map(i => refSignature(sig, i)))) // TODO: introduce distinct signature?
    }).asInstanceOf[T]

    convert[S](sig)
  }

  def refSignature(ref: Ref.Sig)(implicit reporter: HLIRErrorReporter): SignatureType = signature(ref, ref, SignatureType.Nothing)

  def refSignature(outer: Ref, sig: Ref.Sig)(implicit reporter: HLIRErrorReporter): SignatureType = signature(outer, sig, SignatureType.Nothing)

  def typeSignature(ref: Ref.HasSignature)(implicit reporter: HLIRErrorReporter): SignatureType = signature(ref, ref.sig, SignatureType.Nothing)

  def functionSignature(ref: Ref.HasSignature, vararg: Boolean, eraseZSTReturn: Boolean = false)(implicit reporter: HLIRErrorReporter): MethodSignature = {
    val sigWithoutVarargs = signature[MethodSignature](ref, ref.sig, MethodSignature()(SignatureType.Nothing))

    // any array is ok, it's required to mimic AJ varargs
    val varargParam: Option[SignatureType] = Option.when(vararg)(SignatureType.JavaArray(SignatureType.Int32))
    
    // global and external void C functions declared in CJ as returning Unit, but called as void
    // change return type to real Void type for correct ABI building
    val retType = if (eraseZSTReturn && sigWithoutVarargs.returnType.isZST) SignatureType.Void else sigWithoutVarargs.returnType
    
    sigWithoutVarargs.copy(parameterTypes = sigWithoutVarargs.parameterTypes ++ varargParam, returnType = retType)
  }

  def symName(ref: Ref.HasName): String = ref match {
    case ref: Ref.Package =>
      makeSyntheticModuleName(ref.name)

    case ref: Ref.RawEnum => ref.baseType match {
      case base: Ref.Primitive => base.sig
      case base: Ref.HasName   => symName(base)
      case _                   => shouldNotReachHere(ref)
    }

    case ref: Ref.TypeParameter =>
      ref.name

    case ref: (Ref.GenericClass | Ref.GenericInterface | Ref.GenericRecord |
               Ref.InstantiatedClass | Ref.InstantiatedInterface | Ref.InstantiatedRecord) =>
      ref.name

    case ref: Ref.Type =>
      if (!Env.languagePack.supports(Language.JAVA) && (ref.name == "java8.java.lang.Throwable" || ref.name == "java.lang.Throwable")) {
        // Erase "java8/java.lang.Throwable" in pure CJVM.
        // It is currently referenced in "std.ffi.java".
        // FIXME: remove these hacks when FE removes "std.ffi.java" from stdlib in pure CJVM
        STD_CORE_ANY_LINKAGE_NAME
      } else {
        val isJava = ref.isInstanceOf[Ref.Java]
        val rawName = if (isJava) {
          ref.name.replace('.', '/')
        } else if (env.enabled(BoolOption.IgnoreHLIRTypeNames)) {
          ref.linkageName.get
        } else if (env.enabled(BoolOption.IgnoreGenericHLIRTypeNames) && ref.name.contains('<')) {
          ref.linkageName.get
        } else {
          ref.name
        }
        rawName
      }

    case ref: Ref.ForeignCFunction =>
      ref.name

    case ref: Ref.GlobalVariable =>
      val rawName = if (env.enabled(BoolOption.IgnoreHLIRGlobalVarNames)) {
        ref.linkageName.get
      } else {
        ref.name
      }
      rawName

    case ref: (Ref.Global | Ref.InstantiatedGlobalFunction) => // global functions
      val rawName = if (env.enabled(BoolOption.IgnoreHLIRGlobalFuncNames)) {
        ref.linkageName.get
      } else {
        ref.name
      }
      rawName

    case ref: Ref.InstanceField =>
      ref.name

    case ref: Ref.StaticField =>
      ref.name

    case ref: Ref.MethodRef => ref.refType match {
      case _: Ref.Java => ref.name

      case _ =>
        ref.name
    }

    case ref: Ref.Parameter =>
      ref.name
  }

  /** Returns analog of Java bytecode "reference type" for HLIR globals, fields and methods.
    *
    * Typically result is equivalent to "declaring class" of given global or member,
    * but in case of virtual or interface method it might be one of supertypes of actual "declaring class" instead.
    */
  def symRefType(ref: Ref)(implicit reporter: HLIRErrorReporter): Option[ClassType] = {
    (ref match {
      case _: Ref.ForeignCFunction => symType(hlir.packageRef.get)
      case ref: Ref.Global => symType(ref.pkg)
      case ref: Ref.InstantiatedGlobalFunction => symType(ref.pkg)
      case ref: Ref.MemberRef => symType(ref.refType)
      case _: Ref.Package | _: Ref.Type | _: Ref.FunctionalType | _: Ref.Annotation |
           _: Ref.Parameter | _: Ref.ConstantString | _: Ref.JavaAnnotationRelated |
           _: Ref.GenericClass | _: Ref.GenericInterface | _: Ref.GenericRecord | _: Ref.GenericConstraints | _: Ref.TypeParameter |
           _: Ref.Box | _: Ref.InterfaceExtension | _: Ref.InstantiatedInterfaceExtension | _: Ref.GenericInterfaceExtension |
           Ref.ThisType =>
        shouldNotReachHere(ref)
    }) map asClassType
  }

  def symModifiers(modifiers: Set[HLIRMetadata.Modifier], allowOpen: Boolean = false, allowFinal: Boolean = true): Modifiers = {
    import HLIRMetadata.Modifier.*

    // If `open` is allowed, we must treat entity with these modifiers as `final` by default.
    var mods = Modifiers.EMPTY
    if (allowOpen && allowFinal) {
      mods += FINAL
    }

    modifiers foreach {
      case `public`    => mods += PUBLIC
      case `protected` => mods += PROTECTED
      case `private`   => mods += PRIVATE
      case `internal`  => // `internal` means `package-private` (i.e. default in JET)
      case `open`      => mods -= FINAL; assert(allowOpen)
      case `sealed`    => mods += CJ_SEALED
      case `abstract`  => mods += ABSTRACT; mods -= FINAL // non-final implied
      case `immutable` => mods += FINAL
      case `mut`       => mods += CJ_MUT
      case `redef`     => mods += CJ_REDEF
      case `override`  => mods += CJ_OVERRIDE
    }

    mods
  }
}
