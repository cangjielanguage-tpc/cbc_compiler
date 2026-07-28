/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.google.flatbuffers.{IntVector, Table}
import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.cangjie.CHIRVTable
import com.huawei.excelsior.jet.compiler.chir.CHIRUtils.*
import com.huawei.excelsior.jet.compiler.chir.EnumKind
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.*
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.LocalTypeVariable
import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider}
import com.huawei.excelsior.jet.compiler.symlevel.{GenericInfo, MethodSignature, SignatureType, ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.cond
import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

/** Provides resolving utilities to/from [[PackageFormat]] entities.
  *
  * @author liontiger
  */
class CHIRResolver(implicit val pkg: ParsedCHIRPackage, private val env: Environment) {
  private implicit val typeProvider: TypeProvider = env.getTypeProvider

  private val symTypeByTable = mutable.HashMap.empty[Table, SymType]
  def symType(v: Table): Option[SymType] = symTypeByTable.get(v) orElse {
    val res = (v: @unchecked) match {
      case v: StructDef => findClass(symName(v))
      case v: CustomType => symType(pkg.getDef[Table](v.customTypeDef))
      case v: ClassDef => findClass(symName(v))
      case v: EnumDef => findClass(symName(v)) // TODO: support proper Enum
      case v: ExtendDef => findClass(symName(v))
      case v: Type => v.kind match {
        case CHIRTypeKind.REFTYPE => symType(pkg.getType[Table](v.argTys(0)))
      }
    }

    for (t <- res) {
      assert(t != null, s"unexpected Some(null) while resolving symlevel type")
      symTypeByTable(v) = t
    }

    res
  }

  def symName(v: Table): String = {
    def typeDefName(v: CustomTypeDef): String = {
      val srcName = v.srcCodeIdentifier
      if (srcName.isEmpty) v.identifier.tail else s"${v.packageName}:$srcName"
    }

    def globalName(v: GlobalValue): String = {
      val srcName = v.srcCodeIdentifier
      if (srcName.isEmpty) v.base.identifier.tail else srcName
    }

    ((v: @unchecked) match {
      case v: StructDef => typeDefName(v.base)
      case v: CustomType => symName(pkg.getDef[Table](v.customTypeDef))
      case v: ClassDef => typeDefName(v.base)
      case v: EnumDef => typeDefName(v.base) // TODO: support proper Enum
      case v: ExtendDef => typeDefName(v.base)
      case v: Function => globalName(v.base)
      case v: GlobalVar => globalName(v.base)
      case v: FuncSigInfo => v.funcName
      case v: Type => v.kind match {
        case CHIRTypeKind.BOXTYPE => "std.core:Box"
      }
    }) ensuring (_.nonEmpty)
  }

  def linkageName(v: Function | GlobalVar | MemberVarInfo): String = v match {
    case v: Function => v.base.base.identifier.tail
    case v: GlobalVar => v.base.base.identifier.tail
    case v: MemberVarInfo => null
  }

  def mutWithoutTI(name: String): String = {
    name + "$withoutTI"
  }

  def functionSig(m: Function, hasReceiver: Boolean): (MethodSignature, Boolean, Boolean) = {
    functionSig(m.base.base.`type`, m, hasReceiver)
  }

  def functionSig(id: Long, enclosing: Table, hasReceiver: Boolean): (MethodSignature, Boolean, Boolean) = {
    val (cparams, lparams) = enclosingParams(enclosing)
    functionSig(id, cparams, lparams, hasReceiver)
  }

  def functionSig(id: Long, cparams: IntVector, lparams:IntVector, hasReceiver: Boolean): (MethodSignature, Boolean, Boolean) = {
    val funcType = pkg.getType[FuncType](id)
    val params = funcType.base.argTysVector.toSeq.map(typeSig(_, cparams, lparams))
    val startIdx = if (hasReceiver) 1 else 0
    val paramsWithoutRcv = params.drop(startIdx)
    (MethodSignature(paramsWithoutRcv.last, paramsWithoutRcv.init), funcType.isCfuncType, funcType.hasVarArg)
  }

  def typeSig(id: Long, enclosing: Table): SignatureType = {
    val (cparams, lparams) = enclosingParams(enclosing)
    typeSig(id, cparams, lparams)

  }

  def typeSig(id: Long, cparams: IntVector, lparams: IntVector): SignatureType = {
    import SignatureType.*

    def typeVariable(t: GenericType, tId: Int): SignatureType = {
      val funcTypeVariable = {
        val idx = lparams.iterator.indexOf(tId)
        Option.when(idx >= 0)(LocalTypeVariable(idx))
      }
      val classTypeVariable = {
        val idx = cparams.iterator.indexOf(tId)
        Option.when(idx >= 0)(ClassTypeVariable(idx))
      }
      ScalaCollections.singleElement(funcTypeVariable ++ classTypeVariable)
    }

    def typeParams(t: Type): Seq[SignatureType] = t.argTysVector.toSeq.map(typeSig(_, cparams, lparams))

    pkg.getType[Table](id) match {
      case t: Type => t.kind match {
        case CHIRTypeKind.RUNE => UnicodeChar32
        case CHIRTypeKind.BOOLEAN => Boolean
        case CHIRTypeKind.VOID => Void
        case CHIRTypeKind.UNIT => Unit
        case CHIRTypeKind.NOTHING => Nothing
        case CHIRTypeKind.INT8 => Int8
        case CHIRTypeKind.INT16 => Int16
        case CHIRTypeKind.INT32 => Int32
        case CHIRTypeKind.INT64 => Int64
        case CHIRTypeKind.INT_NATIVE => AddrInt
        case CHIRTypeKind.UINT8 => UInt8
        case CHIRTypeKind.UINT16 => UInt16
        case CHIRTypeKind.UINT32 => UInt32
        case CHIRTypeKind.UINT64 => UInt64
        case CHIRTypeKind.UINT_NATIVE => AddrUInt
        case CHIRTypeKind.FLOAT16 => Float16
        case CHIRTypeKind.FLOAT32 => Float32
        case CHIRTypeKind.FLOAT64 => Float64
        case CHIRTypeKind.C_STRING => BString
        case CHIRTypeKind.C_POINTER => CPointer(typeSig(t.argTys(0), cparams, lparams))
        case CHIRTypeKind.THIS => ThisTypeInfo
        case CHIRTypeKind.REFTYPE => typeSig(t.argTys(0), cparams, lparams) // TODO: do we need to distinguish?
        case CHIRTypeKind.BOXTYPE => InstantiatedReference(symName(t), Seq(ClassTypeVariable(0)))
        case CHIRTypeKind.TUPLE => Tuple(typeParams(t))
        case k => notImplemented(CHIRTypeKind.name(k))
      }
      case t: RawArrayType => Seq.iterate(typeSig(t.base.argTys(0), cparams, lparams), t.dims.toInt + 1)(CangjieArray.apply).last
      case t: VArrayType => VArray(typeSig(t.base.argTys(0), cparams, lparams), t.size)
      case t: GenericType => typeVariable(t, id.toInt)
      case t: CustomType => t.base.kind match {
        case CHIRTypeKind.CLASS =>
          val params = typeParams(t.base)
          if (params.nonEmpty) InstantiatedReference(symName(t), params) else Reference(symName(t), jbc = false)
        case CHIRTypeKind.STRUCT =>
          val params = typeParams(t.base)
          if (params.nonEmpty) InstantiatedRecord(symName(t), params) else Record(symName(t))
        case CHIRTypeKind.ENUM =>
          val enumDef = pkg.getDef[EnumDef](t.customTypeDef)
          enumKind(enumDef) match {
            case EnumKind.ZeroSized => Unit
            case EnumKind.PrimitiveBased => Int32
            case EnumKind.OptionLike(base) =>
              val params = typeParams(t.base)
              val baseSig = typeSig(base, enumDef).instantiate(params, Seq.empty)
              baseSig match {
                case b: NullableWrapper.Base => NullableWrapper(b)
                case _ => if (params.nonEmpty) InstantiatedRecord(symName(t), params) else Record(symName(t))
              }
            case EnumKind.UnionBased =>
              val params = typeParams(t.base)
              if (params.nonEmpty) InstantiatedRecord(symName(t), params) else Record(symName(t))
            case EnumKind.ClassBased => notImplemented(s"class-based enum ${enumDef.base.identifier}")
          }
        case k => notImplemented(CHIRTypeKind.name(k))
      }
      case t: FuncType => notImplemented("FuncType")
    }
  }

  private def enclosingParams(enclosing: Table): (IntVector, IntVector) = {
    val lparams = enclosing match {
      case e: Function => e.genericTypeParamsVector
      case e: InvokeBase => e.virMethodCtx.genericTypeParamsVector
      case _ => null
    }

    val cparams = {
      val cls = enclosing match {
        case e: Function => pkg.getDef[Table](e.base.declaredParent)
        case e: GlobalVar => pkg.getDef[Table](e.base.declaredParent)
        case e: InvokeBase => pkg.getType[Table](e.base.objType)
        case e => e
      }
      if (cls == null) null else withGenericParams(cls)(identity)
    }
    (cparams, lparams)
  }

  @tailrec
  final def withGenericParams[T](v: Table)(action: IntVector => T): T = (v: @unchecked) match {
    case v: CustomTypeDef => action(pkg.getType[CustomType](v.`type`).base.argTysVector)
    case v: CustomType => withGenericParams(pkg.getDef[CustomTypeDef](v.customTypeDef))(action)
    case v: StructDef => withGenericParams(v.base)(action)
    case v: ClassDef => withGenericParams(v.base)(action)
    case v: EnumDef => withGenericParams(v.base)(action)
    case v: Function => action(v.genericTypeParamsVector)
    case v: ExtendDef => action(v.genericParamsVector)
    case v: FuncSigInfo => action(v.genericTypeParamsVector)
    case v: VirtualMethodInfo => action(v.methodGenericTypeParamsVector)
    case v: Type => v.kind match {
      case CHIRTypeKind.REFTYPE => withGenericParams(pkg.getType[Table](v.argTys(0)))(action)
    }
  }

  def genericInfo(v: Table): GenericInfo = withGenericParams(v) { params =>
    val constraints = for ((id, i) <- params.toSeq.zipWithIndex) yield {
      val t = pkg.getType[GenericType](id)
      val upperBounds = t.upperBoundsVector.toSeq.map(typeSig(_, v))
      GenericInfo.Constraint(LocalTypeVariable(i), upperBounds)
    }

    if (constraints.nonEmpty) {
      GenericInfo(constraints)
    } else {
      GenericInfo.none
    }
  }

  def findClass(className: String): Option[SymClassType] = {
    Option(typeProvider.findClass(xstr(className), loadPDB = true))
  }

  def symModifiers(modifiers: Long): Modifiers = {
    var mods = Modifiers.EMPTY

    if (Attribute.PUBLIC    in modifiers) mods += PUBLIC
    if (Attribute.PROTECTED in modifiers) mods += PROTECTED
    if (Attribute.PRIVATE   in modifiers) mods += PRIVATE
    //if (Attribute.VIRTUAL   in modifiers) // TODO: support
    if (Attribute.SEALED    in modifiers) mods += CJ_SEALED
    if (Attribute.ABSTRACT  in modifiers) mods += ABSTRACT
    if (Attribute.READONLY  in modifiers) mods += FINAL
    if (Attribute.MUT       in modifiers) mods += CJ_MUT
    if (Attribute.REDEF     in modifiers) mods += CJ_REDEF
    if (Attribute.OVERRIDE  in modifiers) mods += CJ_OVERRIDE
    if (Attribute.STATIC    in modifiers) { mods -= FINAL; mods += STATIC }

    // TODO: handle more cases?

    mods
  }

  def isGenericInstantiated(t: CustomTypeDef): Boolean = {
    Attribute.GENERIC_INSTANTIATED in t.base.attributes
  }

  def isImported(t: CustomTypeDef | GlobalValue): Boolean = {
    val (attrs, packageName, annos) = t match {
      case t: CustomTypeDef => (t.base.attributes, t.packageName, annotations(t.base))
      case t: GlobalValue   => (t.base.base.attributes, t.packageName, Iterator.empty)
    }
    (Attribute.IMPORTED in attrs) || (annos exists isAutoEnv)
  }

  def isAutoEnv(x: Table): Boolean = cond(x) {
    case x: IsAutoEnvClass => x.value
  }

  def annotations(base: Base): Iterator[Table] = {
    val annos = base.annosVector
    Iterator.tabulate(annos.length) { i =>
      val obj = base.annosType(i) match {
        case Annotation.needCheckArrayBound => new NeedCheckArrayBound
        case Annotation.needCheckCast => new NeedCheckCast
        case Annotation.debugLocationInfoForWarning => new DebugLocation
        case Annotation.generatedFromForIn => new GeneratedFromForIn
        case Annotation.isAutoEnvClass => new IsAutoEnvClass
        case Annotation.isCapturedClassInCC => new IsCapturedClassInCC
        case Annotation.linkTypeInfo => new LinkTypeInfo
        case Annotation.skipCheck => new SkipCheck
        case Annotation.neverOverflowInfo => new NeverOverflowInfo
        case Annotation.enumCaseIndex => new EnumCaseIndex
        case Annotation.virMethodOffset => new VirMethodOffset
        case Annotation.wrappedRawMethod => new WrappedRawMethod
        case Annotation.overrideSrcFuncType => new OverrideSrcFuncType
      }
      annos.get(obj, i)
    }
  }

  private def isZST(id: Long): Boolean = isZST(pkg.getType[Table](id))

  private def isZST(t: Table): Boolean = t match {
    case t: Type => t.kind match {
      case CHIRTypeKind.VOID => true
      case CHIRTypeKind.UNIT => true
      case CHIRTypeKind.NOTHING => true
      case CHIRTypeKind.TUPLE => t.argTysVector.iterator.forall(isZST)
      case _ => false
    }
    case t: VArrayType => isZST(t.base.argTys(0))
    case t: CustomType => t.base.kind match {
      case CHIRTypeKind.CLASS => false
      case CHIRTypeKind.STRUCT => pkg.getDef[StructDef](t.customTypeDef).base.instanceMemberVarsVector.iterator.forall(i => isZST(i.`type`))
      case CHIRTypeKind.ENUM => enumKind(pkg.getDef[EnumDef](t.customTypeDef)) == EnumKind.ZeroSized
    }
    case _ => false
  }

  private def isReferenceType(id: Long): Boolean = isReferenceType(pkg.getType[Table](id))

  private def isReferenceType(t: Table): Boolean = t match {
    case t: Type => t.kind match {
      case CHIRTypeKind.THIS => true
      case CHIRTypeKind.REFTYPE => isReferenceType(t.argTys(0))
      case CHIRTypeKind.BOXTYPE => true
      case _ => false
    }
    case t: RawArrayType => true
    case t: CustomType => t.base.kind match {
      case CHIRTypeKind.CLASS => true
      case CHIRTypeKind.STRUCT => false
      case CHIRTypeKind.ENUM =>
        enumKind(pkg.getDef[EnumDef](t.customTypeDef)) match {
          case EnumKind.OptionLike(base) => isReferenceType(base)
          case EnumKind.ClassBased => true
          case _ => false
        }
      case _ => false
    }
    case _ => false
  }

  private def isTraceableStruct(id: Long): Boolean = isTraceableStruct(pkg.getType[Table](id))

  private def isTraceableStruct(t: Table): Boolean = t match {
    case t: Type => t.kind match {
      case CHIRTypeKind.TUPLE => t.argTysVector.iterator.forall(p => isReferenceType(p) || isTraceableStruct(p))
      case _ => false
    }
    case t: VArrayType =>
      val p = t.base.argTys(0)
      isReferenceType(p) || isTraceableStruct(p)
    case t: CustomType => t.base.kind match {
      case CHIRTypeKind.CLASS => false
      case CHIRTypeKind.STRUCT => pkg.getDef[StructDef](t.customTypeDef).base.instanceMemberVarsVector.iterator.forall(i => isReferenceType(i.`type`) || isTraceableStruct(i.`type`))
      case CHIRTypeKind.ENUM =>
        enumKind(pkg.getDef[EnumDef](t.customTypeDef)) match {
          case EnumKind.OptionLike(base) => isTraceableStruct(base)
          case _ => false
        }
    }
    case _ => false
  }

  private val enumKindByEnumDef = mutable.HashMap.empty[EnumDef, EnumKind]
  def enumKind(enumDef: EnumDef): EnumKind = enumKindByEnumDef.getOrElseUpdate(enumDef, {
    val ctorSigs = enumDef.ctorsVector.toSeq.map(c => pkg.getType[FuncType](c.funcType))
    val ctors = ctorSigs.map(_.base.argTysVector.toSeq.init)
    val noParams = ctors.forall(_.isEmpty)

    def zstParams = ctors.forall(_.forall(isZST))

    def hasRefParams = ctors.exists(_.exists(t => isReferenceType(t) || isTraceableStruct(t)))

    lazy val optionLikeParam = ScalaCollections.singleton(ctors.flatten)

    def nonGenericEnum = pkg.getType[CustomType](enumDef.base.`type`).base.argTysLength == 0

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

  def isExtendedBaseFunc(f: Function): Boolean = pkg.getDef[Table](f.base.declaredParent) match {
    case d: ExtendDef => f.base.srcCodeIdentifier.nonEmpty
    case _ => false
  }
}
