/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.chir.CHIR.{Attribute, CustomTypeDef, GenericType}
import com.huawei.excelsior.jet.compiler.chir.EnumKind.ZeroSized
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.LocalTypeVariable
import com.huawei.excelsior.jet.compiler.symlevel.{GenericInfo, MethodSignature, SignatureType, ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider}
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.cond
import scala.annotation.tailrec
import scala.collection.mutable

/** Provides resolving utilities to/from [[PackageFormat]] entities.
  *
  * @author liontiger
  */
class CHIRResolver(implicit val pkg: CHIR.Package, private val env: Environment) {
  private implicit val typeProvider: TypeProvider = env.getTypeProvider

  private val symTypeByTable = mutable.HashMap.empty[CHIR.Type | CHIR.CustomTypeDef, SymType]
  def symType(v: CHIR.Type | CHIR.CustomTypeDef): Option[SymType] = symTypeByTable.get(v) orElse {
    val res = (v: @unchecked) match {
      case v: CHIR.CustomTypeDef => findClass(symName(v))
      case v: CHIR.CustomType => symType(v.typeDef)
      case v: CHIR.RefType => symType(v.baseType)
    }

    for (t <- res) {
      assert(t != null, s"unexpected Some(null) while resolving symlevel type")
      symTypeByTable(v) = t
    }

    res
  }

  def symName(v: CHIR.CustomTypeDef | CHIR.CustomType | CHIR.Func | CHIR.GlobalVar | CHIR.FuncSig): String = {
    def typeDefName(v: CHIR.CustomTypeDef): String = {
      val srcName = v.srcCodeIdentifier
      if (srcName.isEmpty || isGenericInstantiated(v)) v.identifier.tail else s"${v.packageName}:$srcName"
    }

    def globalName(_v: CHIR.Func | CHIR.GlobalVar): String = {
      val (id, identifier, srcName, annotations) = _v match {
        case v: CHIR.Func => (v.id, v.identifier, v.srcCodeIdentifier, v.annotations)
        case v: CHIR.GlobalVar => (v.id, v.identifier, v.srcCodeIdentifier, v.annotations)
      }
      val wrappedMethod = annotations.collectFirst { case m: CHIR.WrappedRawMethod => m.rawMethod }
      val v = wrappedMethod.getOrElse(_v)
      val isPrivate = v.attributes.contains(CHIR.Attribute.Private)
      val isPackageGlobal = v.declaringDef.isEmpty
      val suffix = if (isGenericInstantiated(v)) {
        // TODO another way without id usage?
        assert(id > 0)
        s"$$instantiated$$${pkg.name}$$$id"
      } else {
        ""
      }
      getOverrideSrcFuncType(v) match {
        case Some(funcType) =>
          val f = t.asInstanceOf[Function]
          val d = pkg.getDef[Table](v.declaredParent) match {
            case d: ClassDef => d.base
            case d: StructDef => d.base
            case d: EnumDef => d.base
            case d: ExtendDef => d.base
          }
          val vtableFuncs = d.vtableVector.toSeq.flatMap(_.virtualMethodsVector.toSeq)
          vtableFuncs.find(m => pkg.getValue[Function](m.instance) == f) match {
            case Some(m) => m.funcName
            case None => shouldNotReachHere(v.base.identifier)
          }
        case None =>
          if (srcName.isEmpty || srcName == "$lambda" || (isPackageGlobal && isPrivate)) identifier.tail else srcName + suffix
      }
    }

    ((v: @unchecked) match {
      case v: CHIR.StructDef => typeDefName(v)
      case v: CHIR.CustomType => symName(v.typeDef)
      case v: CHIR.ClassDef => typeDefName(v)
      case v: CHIR.EnumDef => typeDefName(v) // TODO: support proper Enum
      case v: CHIR.ExtendDef => typeDefName(v)
      case v: CHIR.Func => globalName(v)
      case v: CHIR.GlobalVar => globalName(v)
      case v: CHIR.FuncSig => v.name
    }) ensuring (_.nonEmpty)
  }

  def linkageName(v: CHIR.Func | CHIR.GlobalVar | CHIR.InstanceVar): String = v match {
    case v: CHIR.Func => v.identifier
    case v: CHIR.GlobalVar => v.identifier
    case v: CHIR.InstanceVar => null
  }

  def mutWithoutTI(name: String): String = {
    name + "$withoutTI"
  }

  def functionSig(m: CHIR.Func, hasReceiver: Boolean): (MethodSignature, Option[SignatureType], Boolean, Boolean) = {
    val (sig, rcv, isCFunc, hasVarArg) = functionSig(m.tpe, hasReceiver)
    if (m.srcCodeIdentifier == "$lambda") {
      val cparams = Seq.tabulate(m.genericTypeParams.size)(SignatureType.LocalTypeVariable.apply)
      val lparams = Seq.empty
      val lsig = sig.instantiate(cparams, lparams)
      (lsig, rcv, isCFunc, hasVarArg)
    } else {
      (sig, rcv, isCFunc, hasVarArg)
    }
  }

  def functionSig(funcType: CHIR.FuncType, hasReceiver: Boolean): (MethodSignature, Option[SignatureType], Boolean, Boolean) = {
    val params = (funcType.paramTypes :+ funcType.returnType).map(typeSig)
    val startIdx = if (hasReceiver) 1 else 0
    val paramsWithoutRcv = params.drop(startIdx)
    (MethodSignature(paramsWithoutRcv.last, paramsWithoutRcv.init), Option.when(hasReceiver)(params.head), funcType.isC, funcType.hasVarArg)
  }

  def typeSig(tpe: CHIR.Type): SignatureType = {
    import SignatureType.*

    tpe match {
      case t: CHIR.BoxType =>
        val base = typeSig(t.baseType)
        if (base.isTraceableReference && !base.isInstanceOf[OptionLikeEnum]) base else Box(base)

      case CHIR.BuiltinType.Rune => UnicodeChar32
      case CHIR.BuiltinType.Boolean => Boolean
      case CHIR.BuiltinType.Void => Void
      case CHIR.BuiltinType.Unit => Unit
      case CHIR.BuiltinType.Nothing => Nothing
      case CHIR.BuiltinType.Int8 => Int8
      case CHIR.BuiltinType.Int16 => Int16
      case CHIR.BuiltinType.Int32 => Int32
      case CHIR.BuiltinType.Int64 => Int64
      case CHIR.BuiltinType.IntNative => AddrInt
      case CHIR.BuiltinType.UInt8 => UInt8
      case CHIR.BuiltinType.UInt16 => UInt16
      case CHIR.BuiltinType.UInt32 => UInt32
      case CHIR.BuiltinType.UInt64 => UInt64
      case CHIR.BuiltinType.UIntNative => AddrUInt
      case CHIR.BuiltinType.Float16 => Float16
      case CHIR.BuiltinType.Float32 => Float32
      case CHIR.BuiltinType.Float64 => Float64
      case CHIR.BuiltinType.CString => BString
      case CHIR.BuiltinType.This => ThisTypeInfo

      case t: CHIR.ClassType =>
        val params = t.genericTypeParams.map(typeSig)
        if (params.nonEmpty) InstantiatedReference(symName(t), params) else Reference(symName(t), jbc = false)
      case t: CHIR.CPointerType =>
        CPointer(typeSig(t.elementType))
      case t: CHIR.EnumType =>
        enumKind(t.typeDef) match {
          case EnumKind.ZeroSized =>
            ZeroSizedEnum(symName(t), t.genericTypeParams.map(typeSig))
          case EnumKind.PrimitiveBased =>
            PrimitiveBasedEnum(symName(t), t.genericTypeParams.map(typeSig))
          case EnumKind.OptionLike(base) =>
            val params = t.genericTypeParams.map(typeSig)
            val baseSig = typeSig(base).instantiate(params, Seq.empty)
            OptionLikeEnum(symName(t), params, baseSig)
          case EnumKind.UnionBased =>
            UnionBasedEnum(symName(t), t.genericTypeParams.map(typeSig))
          case EnumKind.ClassBased =>
            ClassBasedEnum(symName(t), t.genericTypeParams.map(typeSig))
        }
      case t: CHIR.FuncType =>
        notImplemented("FuncType")
      case t: CHIR.GenericType =>
        typeVariableSig(t)
      case t: CHIR.RefType =>
        typeSig(t.baseType) // TODO: do we need to distinguish?
      case t: CHIR.RawArrayType =>
        Seq.iterate(typeSig(t.elementType), t.dimension.toInt + 1)(CangjieArray.apply).last
      case t: CHIR.StructType =>
        val params = t.genericTypeParams.map(typeSig)
        if (params.nonEmpty) InstantiatedRecord(symName(t), params) else Record(symName(t))
      case t: CHIR.VArrayType =>
        VArray(typeSig(t.elementType), t.size)
    }
  }

  @tailrec
  private def withGenericParams[T](v: CHIR.CustomTypeDef | CHIR.Type | CHIR.Func | CHIR.FuncSig | CHIR.VMethod)(action: Seq[CHIR.Type] => T): T = (v: @unchecked) match {
    case v: CHIR.ExtendDef => action(v.genericTypeParams)
    case v: CHIR.CustomTypeDef => action(v.tpe.genericTypeParams)
    case v: CHIR.CustomType => withGenericParams(v.typeDef)(action)
    case v: CHIR.RefType => withGenericParams(v.baseType)(action)
    case CHIR.BuiltinType.This => action(Seq.empty)
    case v: CHIR.Func => action(v.genericTypeParams)
    case v: CHIR.FuncSig => action(v.genericTypeParams)
    case v: CHIR.VMethod => action(v.genericTypeParams)
  }

  def genericInfo(v: CHIR.CustomTypeDef | CHIR.Func): GenericInfo = withGenericParams(v) { params =>
    val constraints = for ((t, i) <- params.zipWithIndex) yield {
      val upperBounds = t match {
        case t: CHIR.GenericType => t.upperBounds.map(typeSig)
        case _ => Seq.empty
      }
      GenericInfo.Constraint(LocalTypeVariable(i), upperBounds)
    }

    if (constraints.nonEmpty) {
      GenericInfo(constraints)
    } else {
      GenericInfo.none
    }
  }

  private val typeVarCache = mutable.HashMap.empty[CHIR.GenericType, SignatureType.TypeVariable]
  private var typeVarCacheInitialized = false

  private def initTypeVars(): Unit = {
    def fillTypeVars(gTypes: Seq[CHIR.GenericType], local: Boolean): Unit = {
      for ((genericType, i) <- gTypes.zipWithIndex) {
        val typeVar = if (local) SignatureType.LocalTypeVariable(i) else SignatureType.ClassTypeVariable(i)
        assert(!typeVarCache.contains(genericType) || typeVarCache(genericType) == typeVar, genericType.identifier)
        typeVarCache(genericType) = typeVar
      }
    }

    for (d <- pkg.typeDefs) {
      val gTypes = d match {
        case d: CHIR.ExtendDef => d.genericTypeParams
        case d: CHIR.CustomTypeDef if isGenericInstantiated(d) => Seq.empty
        // TODO are generic type params generic always here?
        case d: CHIR.CustomTypeDef => d.tpe.genericTypeParams collect {
          case t: CHIR.GenericType => t
        }
      }
      fillTypeVars(gTypes, local = false)
    }
    pkg.values foreach {
      case d: CHIR.Func if d.srcCodeIdentifier != "$lambda" => fillTypeVars(d.genericTypeParams, local = true)
      case _ =>
    }
  }

  private def typeVariableSig(t: CHIR.GenericType): SignatureType.TypeVariable = {
    if (!typeVarCacheInitialized) {
      initTypeVars()
      typeVarCacheInitialized = true
    }
    typeVarCache.getOrElse(t, shouldNotReachHere(t.identifier))
  }

  def findClass(className: String): Option[SymClassType] = {
    Option(typeProvider.findClass(xstr(className), loadPDB = true))
  }

  def symModifiers(a: CHIR.HasAttributes): Modifiers = {
    var mods = Modifiers.EMPTY

    a.attributes.foreach {
      case CHIR.Attribute.Public => mods += PUBLIC
      case CHIR.Attribute.Protected => mods += PROTECTED
      case CHIR.Attribute.Private => mods += PRIVATE
//      case CHIR.Attribute.Virtual => // TODO: support
      case CHIR.Attribute.Sealed => mods += CJ_SEALED
      case CHIR.Attribute.Abstract => mods += ABSTRACT
      case CHIR.Attribute.Readonly => mods += FINAL
      case CHIR.Attribute.Mut => mods += CJ_MUT
      case CHIR.Attribute.Redef => mods += CJ_REDEF
      case CHIR.Attribute.Override => mods += CJ_OVERRIDE
      case CHIR.Attribute.Static => mods -= FINAL; mods += STATIC
      case _ => // do nothing
    }

    mods
  }

  def isGenericInstantiated(t: CHIR.HasAttributes): Boolean = {
    t.attributes.contains(CHIR.Attribute.GenericInstantiated)
  }

  def isImported(t: CHIR.CustomTypeDef | CHIR.Func | CHIR.GlobalVar): Boolean = {
    val (attrs, isFunctionalTypeBase) = t match {
      case t: CHIR.CustomTypeDef => (t.attributes, isFunctionalType(t) && !isLambda(t.tpe))
      case t: (CHIR.Func | CHIR.GlobalVar)  => (t.attributes, false)
    }
    attrs.contains(CHIR.Attribute.Imported) || isFunctionalTypeBase
  }

  /**
   * Sorts out redundant functions marked by diff-tool.
   * For example, the global functions are referenced from vtable.
   */
  def isDeadFunction(f: CHIR.Func): Boolean = {
    f.attributes.contains(Attribute.Unreachable)
  }

  private def isFunctionalType(t: CHIR.CustomTypeDef): Boolean = {
    t.annotations exists isAutoEnv
  }

  private def isLambda(t: CHIR.CustomType): Boolean = {
    typeSig(t).isCangjieLambda
  }

  private def isAutoEnv(x: CHIR.Annotation): Boolean = cond(x) {
    case x: CHIR.IsAutoEnvClass => x.value
  }

  private def isZST(t: CHIR.Type): Boolean = t match {
    case CHIR.BuiltinType.Void | CHIR.BuiltinType.Unit | CHIR.BuiltinType.Nothing => true
    case t: CHIR.TupleType => t.fieldTypes.forall(isZST)
    case t: CHIR.VArrayType => isZST(t.elementType)
    case t: CHIR.StructType => t.typeDef.instanceVars.forall(i => isZST(i.tpe))
    case t: CHIR.EnumType => enumKind(t.typeDef) == EnumKind.ZeroSized
    case _ => false
  }

  @tailrec
  private def isReferenceType(t: CHIR.Type): Boolean = t match {
    case CHIR.BuiltinType.This => true
    case _: (CHIR.BoxType | CHIR.RawArrayType | CHIR.ClassType) => true
    case t: CHIR.RefType => isReferenceType(t.baseType)
    case t: CHIR.EnumType =>
      enumKind(t.typeDef) match {
        case EnumKind.OptionLike(base) => isReferenceType(base)
        case EnumKind.ClassBased => true
        case _ => false
      }
    case _ => false
  }

  private def isTraceableStruct(t: CHIR.Type): Boolean = t match {
    case t: CHIR.TupleType => t.fieldTypes.exists(p => isReferenceType(p) || isTraceableStruct(p))
    case t: CHIR.VArrayType => isReferenceType(t.elementType) || isTraceableStruct(t.elementType)
    case t: CHIR.StructType => t.typeDef.instanceVars.exists(i => isReferenceType(i.tpe) || isTraceableStruct(i.tpe))
    case t: CHIR.EnumType => enumKind(t.typeDef) match {
      case EnumKind.OptionLike(base) => isTraceableStruct(base)
      case _ => false
    }
    case _ => false
  }

  private val enumKindByEnumDef = mutable.HashMap.empty[CHIR.EnumDef, EnumKind]
  def enumKind(enumDef: CHIR.EnumDef): EnumKind = enumKindByEnumDef.getOrElseUpdate(enumDef, {
    val ctorSigs = enumDef.ctors.map(_.tpe)
    val ctors = ctorSigs.map(_.paramTypes)
    val noParams = ctors.forall(c => c.isEmpty)

    def zstParams = ctors.forall(c => c.forall(isZST))

    def hasRefParams = ctors.exists(c => c.exists(t => isReferenceType(t) || isTraceableStruct(t)))

    lazy val optionLikeParam = ScalaCollections.singleton(ctors.flatten)

    def nonGenericEnum = enumDef.tpe.genericTypeParams.isEmpty

    if (enumDef.nonExhaustive) {
      if (noParams) {
        EnumKind.PrimitiveBased

      } else {
        EnumKind.ClassBased
      }

    } else { // exhaustive
      if (ctors.size == 1 && zstParams) {
        EnumKind.ZeroSized

      } else if (noParams) {
        EnumKind.PrimitiveBased

      } else if (ctors.size == 2 && optionLikeParam.nonEmpty) {
        EnumKind.OptionLike(optionLikeParam.get)

      } else if (!hasRefParams && nonGenericEnum) {
        EnumKind.UnionBased

      } else {
        EnumKind.ClassBased
      }
    }
  })

  def classBasedEnumConstructorName(name: String, idx: Int): String = {
    assert(idx >= 0, s"$name:$idx")
    s"$name:$idx"
  }

  def getOverrideSrcFuncType(f: CHIR.Func): Option[CHIR.OverrideSrcFuncType] = {
    f.annotations.collectFirst {
      case a: CHIR.OverrideSrcFuncType => a
    }
  }
}
