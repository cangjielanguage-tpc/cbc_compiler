/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.{Env, TypeProvider}
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.{ARRAY_SLICE_NAME, ARRAY_SLICE_PREFIX, CANGJIE_ARRAY_PREFIX, CANGJIE_RECORD_ARRAY_PREFIX, CANGJIE_REF_ARRAY_NAME, VARRAY_PREFIX}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.Primitive.primitives
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType

import scala.PartialFunction.condOpt
import scala.annotation.tailrec

sealed abstract class SignatureType extends Signature {

  protected def compileTimeAssertThatSignatureTypeExtendsProperOrWrapper: Unit

  def toJETSignature: String = {
    this match {
      case Void                     => "V"
      case Unit                     => "U"
      case Nothing                  => "N"
      case Boolean                  => "b"
      case AddrInt                  => "ia"
      case AddrUInt                 => "ua"
      case t: Integral              => s"${if t.signed then "i" else "u"}${t.bits}"
      case UnicodeChar32            => "c32"
      case t: FloatingPoint         => s"f${t.bits}"
      case BString                  => "BS"
      case t: CPointer              => s"P${t.pointee.toJETSignature}"
      case t: Record                => s"S${t.name};"
      case t: InstantiatedRecord    => s"IS${t.name};<${t.instantiatedTypeParameters.map(_.toJETSignature).mkString("_")}>"
      case t: JBCReference          => s"L${t.name};"
      case t: CangjieReference      => s"R${t.name};"
      case t: InstantiatedReference => s"IR${t.name};<${t.instantiatedTypeParameters.map(_.toJETSignature).mkString("_")}>"
      case t: ArraySlice            => s"AS${t.elemType.toJETSignature}"
      case t: CangjieArray          => s"AR${t.elemType.toJETSignature}"
      case t: JavaArray             => s"AJ${t.dimNum}${t.baseType.toJETSignature}"
      case t: CangjieEnumWrapper    => s"EW${t.baseType.toJETSignature}${t.name};"
      case t: VArray                => s"AV${t.length}${t.elemType.toJETSignature}"
      case t: LocalTypeVariable     => s"TL${t.idx}"
      case t: ClassTypeVariable     => s"TC${t.idx}"
      case ThisTypeInfo             => s"TT"
      case t: NullableWrapper       => s"?${t.baseType.toJETSignature}"
      case t: NonNullableWrapper    => s"!${t.baseType.toJETSignature}"
      case t: Tuple                 => s"TU<${t.params.map(_.toJETSignature).mkString("_")}>"
      case t: Box                   => s"BOX${t.base.toJETSignature}"
    }
  }

  private var _symType: Type = _
  final def symType(implicit typeProvider: TypeProvider): Type = {
    if (_symType == null) {
      _symType = calcSymType
      assert(_symType != null, s"could not find symlevel type for $this")
    }
    _symType
  }

  protected def calcSymType(implicit typeProvider: TypeProvider): Type

  final def symKindErased: TypeKind = Wrapper.skip(this) match {
    case Primitive(kind) => kind
    case _: JavaArray | _: Reference | _: CangjieArray | _: InstantiatedReference | _: Box => TypeKind.CLASS
    case _: Record | _: ArraySlice | _: VArray | _: InstantiatedRecord | _: Tuple => TypeKind.RECORD
    case BString | _: CPointer | _: TypeVariable | ThisTypeInfo => TypeKind.address
  }

  final def jbcKind(implicit typeProvider: TypeProvider): BytecodeTypeKind = Wrapper.skip(this) match {
    case _: Primitive => jbcKindErased
    case _: JavaArray => BytecodeTypeKind.ARRAY
    case _: Reference => if (symType.isThinClass) BytecodeTypeKind.THIN else BytecodeTypeKind.CLASS
    case _: Record | _: ArraySlice | BString | _: CPointer | _: VArray | _: Tuple | _: Box |
         _: InstantiatedReference | _: InstantiatedRecord | _: TypeVariable | _: CangjieArray | ThisTypeInfo =>
      shouldNotReachHere(s"no bytecode type kind for $this")
  }

  final def jbcKindErased: BytecodeTypeKind = Wrapper.skip(this) match {
    case Primitive(kind) => primitiveJBCKind(kind)
    case _: JavaArray | _: Reference => BytecodeTypeKind.CLASS
    case _: Record | _: ArraySlice | BString | _: CPointer | _: VArray | _: Tuple | _: Box |
         _: InstantiatedReference | _: InstantiatedRecord | _: TypeVariable | _: CangjieArray | ThisTypeInfo =>
      shouldNotReachHere(s"no bytecode type kind for $this")
  }

  final def toAsm: AsmType = (Wrapper.skip(this): @unchecked) match {
    case Boolean |
         Int8           => AsmType.I8
    case UInt8          => AsmType.U8
    case Int16          => AsmType.I16
    case UInt16         => AsmType.U16
    case Int32 |
         UnicodeChar32  => AsmType.I32
    case UInt32         => AsmType.U32
    case Int64          => AsmType.I64
    case UInt64         => AsmType.U64
    case AddrInt        => AsmType.I64
    case AddrUInt       => AsmType.U64

    case Float16 => AsmType.F16
    case Float32 => AsmType.F32
    case Float64 => AsmType.F64

    case _: JavaArray | _: Reference | _: Record | _: ArraySlice | _: CangjieArray | _: VArray |
         _: InstantiatedReference | _: InstantiatedRecord | _: Tuple | _: Box => AsmType.PTR
    case BString | _: CPointer | _: TypeVariable => AsmType.I64 // TODO: PTR ?
  }

  final def width: Width = this.toAsm.width

  final def getRawObjectSize  (implicit typeProvider: TypeProvider) = symType.getRawObjectSize
  final def getTypeHandle     (implicit typeProvider: TypeProvider) = symType.getTypeHandle
  final def getArrayElemType  (implicit typeProvider: TypeProvider) = (this: @unchecked) match {
    case CangjieArray(elemType) => elemType
    case ArraySlice(elemType) => elemType
    case x: (JavaArray | JBCReference) => x.symType.getArrayElemType
  }

  final def isPrimitive     : Boolean = symKindErased.isPrimitive
  final def isJavaArray     : Boolean = this.isInstanceOf[JavaArray]
  final def isCangjieArray  : Boolean = this.isInstanceOf[CangjieArray]

  final def isAJManagedType (implicit typeProvider: TypeProvider): Boolean = symType.isAJManagedType
  final def isAJArray       (implicit typeProvider: TypeProvider): Boolean = !Env.isStandalone && symType.isAJArray
  final def isXScalaArray   (implicit typeProvider: TypeProvider): Boolean = symType.isXScalaArray
  final def isXScalaType    (implicit typeProvider: TypeProvider): Boolean = symType.isXScalaType
  final def isCangjieType   (implicit typeProvider: TypeProvider): Boolean = symType.isCangjieType
  final def isJavaReference (implicit typeProvider: TypeProvider): Boolean = symType.isJavaReference
  final def isAbstractClass (implicit typeProvider: TypeProvider): Boolean = !this.isInstanceOf[Box] && symType.isAbstractClass
  final def hasDeferredSuper(implicit typeProvider: TypeProvider): Boolean = symType.hasDeferredSuper
  final def hasRefFields    (implicit typeProvider: TypeProvider): Boolean = (this: @unchecked) match {
    case x: Tuple => x.params.exists(p => p.isTraceableReference || (p.isRecord && p.hasRefFields))
    case x: Box => x.base.hasRefFields
    case x => x.symType.hasRefFields
  }

  final def isArraySliceLike: Boolean = this match {
    case x: ArraySlice => true
    case x: Record => x.name == ARRAY_SLICE_NAME
    case _ => false
  }

  final def isRecordArray(implicit typeProvider: TypeProvider): Boolean = Wrapper.skip(this) match {
    case CangjieArray(elemType) => elemType.isRecord
    case _ => false
  }

  final def isRecord: Boolean = Wrapper.skip(this) match {
    case _: Record | _: ArraySlice | _: VArray | _: InstantiatedRecord | _: Tuple => true
    case _: Primitive | _: Reference | _: JavaArray | _: CangjieArray |
         BString | _: CPointer | _: Box |
         _: InstantiatedReference | _: TypeVariable | ThisTypeInfo => false
  }

  final def isVArray: Boolean = Wrapper.skip(this) match {
    case _: VArray => true
    case _ => false
  }

  final def isArray(implicit typeProvider: TypeProvider) = Wrapper.skip(this) match {
    case _: JavaArray | _: CangjieArray => true
    case _: Reference => symType.isAJArray
    case _: Primitive | _: Reference | _: Record | _: ArraySlice |
         BString | _: CPointer | _: VArray | _: Tuple | _: Box |
         _: InstantiatedReference | _: InstantiatedRecord | _: TypeVariable | ThisTypeInfo => false
  }

  final def isClass(implicit typeProvider: TypeProvider): Boolean = Wrapper.skip(this) match {
    case _: Primitive | _: Record | _: ArraySlice | _: JavaArray | _: CangjieArray | BString | _: CPointer | _: VArray |
         _: InstantiatedRecord | _: TypeVariable | ThisTypeInfo | _: Tuple => false
    case _: Reference | _: InstantiatedReference => symType.isClass
    case _: Box => true
  }

  final def isInterface(implicit typeProvider: TypeProvider): Boolean = Wrapper.skip(this) match {
    case _: Primitive | _: Record | _: ArraySlice | _: JavaArray | _: CangjieArray | BString | _: CPointer | _: VArray |
         _: InstantiatedRecord | _: TypeVariable | ThisTypeInfo | _: Tuple | _: Box => false
    case _: Reference | _: InstantiatedReference => symType.isInterface
  }

  final def isThinClass(implicit typeProvider: TypeProvider): Boolean = Wrapper.skip(this) match {
    case _: Primitive | _: Record | _: ArraySlice | _: JavaArray | _: CangjieArray | BString | _: CPointer | _: VArray |
         _: InstantiatedReference | _: InstantiatedRecord | _: TypeVariable | ThisTypeInfo | _: Tuple | _: Box => false
    case _: Reference => symType.isThinClass
  }

  final def isReference: Boolean = Wrapper.skip(this) match {
    case _: Primitive | _: Record | _: ArraySlice | BString | _: CPointer | _: VArray |
         _: InstantiatedRecord | _: TypeVariable | ThisTypeInfo | _: Tuple => false
    case _: Reference | _: JavaArray | _: CangjieArray | _: InstantiatedReference | _: Box => true
  }

  final def isShortIntegral: Boolean = Wrapper.skip(this) match {
    case Boolean | UInt16 | Int16 | UInt8 | Int8 => true
    case _ => false
  }

  final def isTraceableReference(implicit typeProvider: TypeProvider): Boolean = Wrapper.skip(this) match {
    case _: Primitive | _: Record | _: ArraySlice | BString | _: CPointer | _: VArray |
         _: InstantiatedRecord | _: TypeVariable | ThisTypeInfo | _: Tuple => false
    case _: JavaArray | _: CangjieArray | _: Box => true
    case _: Reference | _: InstantiatedReference => symType.isTraceableReference
  }

  final def isDeferred(implicit typeProvider: TypeProvider): Boolean = Wrapper.skip(this) match {
    case _: Primitive | BString | _: CPointer => false
    case _: TypeVariable => shouldNotReachHere(s"FIXME-UG: $this") // FIXME-UG: find out, where this might be needed
    case ThisTypeInfo => false
    case _: Record | _: InstantiatedRecord => symType.isDeferred
    case _: CangjieArray | _: ArraySlice => assert(Env.isStandalone || !symType.isDeferred); false
    case JavaArray(baseType, _) => baseType.isDeferred
    case _: Reference | _: InstantiatedReference => symType.isDeferred
    case x: Tuple =>  x.params.exists(_.isDeferred)
    case x: Box => x.base.isDeferred
    case _: VArray => require(!symType.isDeferred, "deferred VArrays are not support yet"); false
  }

  final def isZST: Boolean = this match {
    case Void | Unit | Nothing => true
    case _ => false
  }

  @tailrec
  final def isUniversalGeneric: Boolean = Wrapper.skip(this) match {
    case _: (TypeVariable | InstantiatedType | Tuple | Box) => true
    case x: VArray => x.elemType.isUniversalGeneric
    case x: CangjieArray => x.elemType.isUniversalGeneric

    case _: Primitive | BString | _: CPointer | _: Record | _: ArraySlice | _: JavaArray | _: Reference |
         ThisTypeInfo => false
  }

  /** Type is variable size if size of field of this type can vary depending on type parameters meaning.
    * NOTE: corresponds to [[com.huawei.excelsior.jet.runtime.jit.cbc.file.Signature#isVariableSizeType]] in runtime. */
  final def isVariableSizeType(implicit typeProvider: TypeProvider): Boolean = Wrapper.skip(this) match {
    case _: TypeVariable => true
    case x: InstantiatedRecord => x.isVariableLayoutType
    case x: Tuple => x.params.exists(_.isVariableSizeType)
    case x: VArray => x.elemType.isVariableSizeType

    case _: Primitive | BString | _: CPointer | _: Record | _: ArraySlice | _: CangjieArray | _: JavaArray |
         _: Reference | _: InstantiatedReference | ThisTypeInfo | _: Box => false
  }

  /** Variable layout type is a type which layout can vary depending on type parameters meaning.
    * NOTE: corresponds to [[com.huawei.excelsior.jet.runtime.jit.cbc.file.Signature#isVariableLayoutType]] in runtime. */
  final def isVariableLayoutType(implicit typeProvider: TypeProvider): Boolean = Wrapper.skip(this) match {
    case _: TypeVariable => true
    case x: InstantiatedType =>
      val fields = asClassType(x.symType).getFields.filterNot(_.isStatic)
      fields.exists { f =>
        f.getType.instantiate(x.instantiatedTypeParameters, Seq.empty).isVariableSizeType
      }
    case x: Tuple => x.params.exists(_.isVariableSizeType)
    case x: Box => x.base.isVariableSizeType
    case x: VArray => x.elemType.isVariableSizeType
    case x: CangjieArray => x.elemType.isVariableSizeType

    case _: Primitive | BString | _: CPointer | _: Record | _: ArraySlice | _: JavaArray | _: Reference |
         ThisTypeInfo => false
  }

  final def containsTypeVariables: Boolean = Wrapper.skip(this) match {
    case _: TypeVariable => true
    case sig: InstantiatedType => sig.instantiatedTypeParameters exists (_.containsTypeVariables)
    case _ => false
  }

  def instantiate(cparams: Seq[SignatureType], lparams: Seq[SignatureType]): SignatureType = {
    if (cparams.nonEmpty || lparams.nonEmpty) instantiateImpl(cparams, lparams) else this
  }

  private[SignatureType] def instantiateImpl(cparams: Seq[SignatureType], lparams: Seq[SignatureType]): SignatureType = this match {
    case _: Primitive | BString | _: CPointer | _: Record | _: JBCReference | _: CangjieReference | _: JavaArray | ThisTypeInfo => this
    case t: InstantiatedRecord    => InstantiatedRecord(t.name, t.instantiatedTypeParameters.map(_.instantiateImpl(cparams, lparams)))
    case t: InstantiatedReference => InstantiatedReference(t.name, t.instantiatedTypeParameters.map(_.instantiateImpl(cparams, lparams)))
    case t: Tuple                 => Tuple(t.params.map(_.instantiateImpl(cparams, lparams)))
    case t: Box                   => Box(t.base.instantiateImpl(cparams, lparams))
    case t: ArraySlice            => ArraySlice(t.elemType.instantiateImpl(cparams, lparams))
    case t: CangjieArray          => CangjieArray(t.elemType.instantiateImpl(cparams, lparams))
    case t: CangjieEnumWrapper    => CangjieEnumWrapper(t.baseType.instantiateImpl(cparams, lparams).asInstanceOf[CangjieEnumWrapper.Base], t.name)
    case t: VArray                => VArray(t.elemType.instantiateImpl(cparams, lparams), t.length)
    case t: LocalTypeVariable     => lparams.applyOrElse(t.idx, _ => this)
    case t: ClassTypeVariable     => cparams.applyOrElse(t.idx, _ => this)
    case t: NullableWrapper       => NullableWrapper(t.baseType.instantiateImpl(cparams, lparams).asInstanceOf[NullableWrapper.Base])
    case t: NonNullableWrapper    => NonNullableWrapper(t.baseType.instantiateImpl(cparams, lparams).asInstanceOf[NonNullableWrapper.Base])
  }
}

object SignatureType {

  sealed abstract class Proper extends SignatureType {
    override def compileTimeAssertThatSignatureTypeExtendsProperOrWrapper: Unit = {}
  }

  sealed abstract class Wrapper extends SignatureType {
    def baseType: SignatureType
    override def compileTimeAssertThatSignatureTypeExtendsProperOrWrapper: Unit = {}
  }

  object Wrapper {
    @tailrec
    def skip(sig: SignatureType): SignatureType.Proper = sig match {
      case sig: Wrapper => skip(sig.baseType)
      case sig: Proper  => sig
    }
  }

  object Primitive {
    private val primitives = Array[Primitive](
      Void, Boolean, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, AddrInt, AddrUInt,
      Float16, Float32, Float64,
      UnicodeChar32, Unit, Nothing
    )
    def byID(id: Int) = primitives(id)

    def values = primitives.iterator

    def apply(kind: BytecodeTypeKind): Primitive = apply(TypeKind.fromBytecode(kind))

    def apply(kind: TypeKind): Primitive = (kind: @unchecked) match {
      case TypeKind.VOID => Void
      case TypeKind.BOOLEAN => Boolean
      case TypeKind.BYTE => Int8
      case TypeKind.SHORT => Int16
      case TypeKind.CHAR => UInt16
      case TypeKind.INT => Int32
      case TypeKind.LONG => Int64
      case TypeKind.FLOAT => Float32
      case TypeKind.DOUBLE => Float64
    }

    def apply(kind: AsmType): Primitive = (kind: @unchecked) match {
      case AsmType.I8   => Int8
      case AsmType.U8   => UInt8
      case AsmType.I16  => Int16
      case AsmType.U16  => UInt16
      case AsmType.I32  => Int32
      case AsmType.U32  => UInt32
      case AsmType.I64  => Int64
      case AsmType.U64  => UInt64
      case AsmType.F16  => Float16
      case AsmType.F32  => Float32
      case AsmType.F64  => Float64
      case AsmType.PTR  => Address
    }

    def unapply(x: Primitive) = Some(x.kind)
  }

  sealed abstract class Primitive extends SignatureType.Proper {
    lazy val id: Int = {
      require(primitives.contains(this))
      primitives.indexOf(this)
    }

    def kind: TypeKind = this match {
      case Void | Unit | Nothing => TypeKind.VOID

      case Boolean => TypeKind.BOOLEAN

      case Int8 | UInt8 => TypeKind.BYTE
      case Int16 => TypeKind.SHORT
      case UInt16 => TypeKind.CHAR
      case Int32 | UInt32 | UnicodeChar32 => TypeKind.INT
      case Int64 | UInt64 => TypeKind.LONG
      case _: AddressWide => TypeKind.address

      case Float16 => TypeKind.SHORT
      case Float32 => TypeKind.FLOAT
      case Float64 => TypeKind.DOUBLE
    }

    override def calcSymType(implicit typeProvider: TypeProvider): Type =
      typeProvider.getPrimitiveType(kind)
  }

  case object Void extends Primitive

  case object Boolean extends Primitive

  sealed abstract class Integral(val bits: Int, val signed: Boolean) extends Primitive

  case object Int8   extends Integral(8,  signed = true)
  case object UInt8  extends Integral(8,  signed = false)
  case object Int16  extends Integral(16, signed = true)
  case object UInt16 extends Integral(16, signed = false)
  case object Int32  extends Integral(32, signed = true)
  case object UInt32 extends Integral(32, signed = false)
  case object Int64  extends Integral(64, signed = true)
  case object UInt64 extends Integral(64, signed = false)

  sealed abstract class AddressWide(signed: Boolean) extends Integral(Env.targetArch.bitWidth, signed)

  case object AddrInt  extends AddressWide(signed = true)
  case object AddrUInt extends AddressWide(signed = false)

  val Address = AddrUInt

  sealed abstract class FloatingPoint(val bits: Int) extends Primitive

  case object Float16 extends FloatingPoint(16)
  case object Float32 extends FloatingPoint(32)
  case object Float64 extends FloatingPoint(64)

  case object UnicodeChar32 extends Primitive

  case object Unit extends Primitive

  case object Nothing extends Primitive

  case object BString extends SignatureType.Proper {
    override def calcSymType(implicit typeProvider: TypeProvider): Type = Address.symType
  }

  case class CPointer(pointee: Signature) extends SignatureType.Proper {
    override def calcSymType(implicit typeProvider: TypeProvider): Type = Address.symType
  }

  case class CangjieEnumWrapper(baseType: CangjieEnumWrapper.Base, name: String) extends SignatureType.Wrapper {
    override def calcSymType(implicit typeProvider: TypeProvider): Type = baseType.symType
  }

  object CangjieEnumWrapper {
    type Base = Primitive | CangjieReference | NullableWrapper
  }

  case class NullableWrapper(baseType: NullableWrapper.Base) extends SignatureType.Wrapper {
    override def calcSymType(implicit typeProvider: TypeProvider): Type = baseType.symType
  }

  object NullableWrapper {
    type Base = CangjieReference | InstantiatedReference | CangjieArray
  }

  case class NonNullableWrapper(baseType: NonNullableWrapper.Base) extends SignatureType.Wrapper {
    override def calcSymType(implicit typeProvider: TypeProvider): Type = baseType.symType
  }

  object NonNullableWrapper {
    type Base = JBCReference | JavaArray
  }

  // TODO: rename to JBCArray
  case class JavaArray(baseType: SignatureType, dimNum: Int) extends SignatureType.Proper {
    require(!baseType.isInstanceOf[JavaArray], s"expected non-array base type: $baseType (dimNum: $dimNum)")

    override def calcSymType(implicit typeProvider: TypeProvider): Type =
      typeProvider.getArrayType(baseType.symType, dimNum)
  }

  object JavaArray {
    def apply(elemType: SignatureType): JavaArray = SignatureType.Wrapper.skip(elemType) match {
      case JavaArray(baseType, dimNum) => JavaArray(baseType, dimNum + 1) // TODO: do not lose elem type nullability somehow
      case _ => JavaArray(elemType, 1)
    }
  }

  sealed abstract class Reference extends SignatureType.Proper {
    def name: String
    def jbc: Boolean

    override final def equals(that: Any) = that match {
      case that: AnyRef if this eq that => true
      case that: Reference => (this.name == that.name) && (this.jbc == that.jbc)
      case _ => false
    }

    override final def hashCode() = (name, jbc).##
  }

  object Reference {
    def apply(tpe: ClassType) = if (tpe.isCangjieType) CangjieReference(tpe) else JBCReference(tpe)
    def apply(name: String, jbc: Boolean) = if (jbc) JBCReference(name) else CangjieReference(name)
  }

  sealed abstract class JBCReference extends Reference {
    final def jbc = true
  }

  object JBCReference {
    def apply(tpe: ClassType) = JBCSymReference(tpe)
    def apply(name: String) = JBCNamedReference(name)
    private[SignatureType] def symTypeInvariants(t: ClassType): Boolean =
      (t.isClassOrInterface || t.isAJArray) && !t.isCangjieType
  }

  case class JBCSymReference private[SignatureType] (tpe: ClassType) extends JBCReference with SymTypeBased {
    require(JBCReference.symTypeInvariants(tpe))
  }

  case class JBCNamedReference private[SignatureType] (name: String) extends JBCReference with NameBased {
    protected def symTypeInvariants(t: ClassType) = JBCReference.symTypeInvariants(t)
  }

  sealed abstract class CangjieReference extends Reference {
    final def jbc = false
  }

  object CangjieReference {
    def apply(tpe: ClassType) = CangjieSymReference(tpe)
    def apply(name: String) = CangjieNamedReference(name)
    private[SignatureType] def symTypeInvariants(t: ClassType): Boolean =
      t.isClassOrInterface && t.isCangjieType
  }

  case class CangjieSymReference private[SignatureType] (tpe: ClassType) extends CangjieReference with SymTypeBased {
    require(CangjieReference.symTypeInvariants(tpe))
  }

  case class CangjieNamedReference private[SignatureType] (name: String) extends CangjieReference with NameBased {
    protected def symTypeInvariants(t: ClassType) = CangjieReference.symTypeInvariants(t)
  }

  sealed abstract class Record extends SignatureType.Proper {
    def name: String

    override final def equals(that: Any) = that match {
      case that: AnyRef if this eq that => true
      case that: Record => this.name == that.name
      case _ => false
    }

    override final def hashCode() = name.##
  }

  object Record {
    def apply(tpe: ClassType) = SymRecord(tpe)
    def apply(name: String) = NamedRecord(name)
  }

  case class SymRecord private[SignatureType] (tpe: ClassType) extends Record with SymTypeBased {
    require(tpe.isRecord)
    require(tpe.getClassLoaderSID == null)
  }

  case class NamedRecord private[SignatureType] (name: String) extends Record with NameBased {
    override protected def symTypeInvariants(t: ClassType) = t.isRecord
  }

  sealed trait SymTypeBased extends SignatureType {
    def tpe: ClassType
    def name: String = tpe.getName
    override final def calcSymType(implicit typeProvider: TypeProvider): Type = tpe
  }

  sealed trait NameBased extends SignatureType {
    def name: String
    protected def symTypeInvariants(t: ClassType): Boolean
    override final def calcSymType(implicit typeProvider: TypeProvider): Type = {
      val t = typeProvider.findClass(XString(name), loadPDB = true)
      assert(t != null, s"could not find symlevel type '$name' for $this")
      assert(symTypeInvariants(t), s"unexpected symlevel type $t for $this")
      t
    }
  }

  case class ArraySlice(elemType: SignatureType) extends SignatureType.Proper {
    def name: String = ArraySlice.name(elemType)

    override def calcSymType(implicit typeProvider: TypeProvider): Type = {
      val t = typeProvider.findClass(XString(name), loadPDB = true)
      assert(t != null, s"could not find symlevel type '$name' for $this")
      assert(t.isRecord, s"unexpected symlevel type $t for $this")
      t
    }
  }

  object ArraySlice {
    def name(elemType: SignatureType): String = ARRAY_SLICE_PREFIX + elemType.toJETSignature
  }

  case class CangjieArray(elemType: SignatureType) extends SignatureType.Proper {
    def name = CangjieArray.name(elemType)

    override def calcSymType(implicit typeProvider: TypeProvider): Type = {
      assert(!Env.isStandalone)
      // TODO: implement properly
      val t = typeProvider.findClass(xstr(name))
      assert(t != null, s"could not find symlevel type '$name' for $this")
      assert(t.isCangjieArray, s"unexpected symlevel type $t for $this")
      t
    }
  }

  object CangjieArray {

    /** Name format based on element type
      *
      *  - Primitive: `AR$<element type sig>`
      *  - Reference: `AR$RAny`
      *  - Record:    `AR$S<record name>`
      *
      *  CPointer element type is erased to Address and
      *  CangjieEnumWrapper, Nullable and NonNullable is unwrapped to avoid unexpected symbols in array name.
      *
      *  TODO: rework so that CangjieArray does not rely on name
      */
    def name(elemType: SignatureType): String = Wrapper.skip(elemType) match {
      case elemType @ (_: Primitive | BString) => CANGJIE_ARRAY_PREFIX + elemType.toJETSignature
      case _: CPointer => CANGJIE_ARRAY_PREFIX + Address.toJETSignature
      case _: JavaArray | _: Reference | _: CangjieArray | _: InstantiatedReference => CANGJIE_REF_ARRAY_NAME
      case elemType: Record => CANGJIE_RECORD_ARRAY_PREFIX + elemType.name
      case elemType: ArraySlice => CANGJIE_RECORD_ARRAY_PREFIX + elemType.name

      case elem: (TypeVariable | InstantiatedRecord | Tuple | Box) => // FIXME-UG
        shouldNotReachHere(s"RawArray<${elem}> is not supported yet")

      case ThisTypeInfo =>
        shouldNotReachHere(s"RawArray<ThisTypeInfo> is not supported")

      case elem: VArray =>
        shouldNotReachHere("RawArray<VArray> is not supported yet")
    }

    def erasedElemType(elemType: SignatureType)(implicit typeProvider: TypeProvider): SignatureType = Wrapper.skip(elemType) match {
      case _: Primitive | BString | _: Record => elemType
      case _: CPointer => Address
      case _: JavaArray | _: Reference | _: CangjieArray | _: InstantiatedReference => fromSymType(typeProvider.getAJObjectType)
      case _: ArraySlice => fromSymType(elemType.symType)

      case elem: (TypeVariable | InstantiatedRecord | Tuple | Box) => // FIXME-UG
        shouldNotReachHere(s"RawArray<${elem}> is not supported yet")

      case ThisTypeInfo =>
        shouldNotReachHere(s"RawArray<ThisTypeInfo> is not supported")

      case elem: VArray =>
        shouldNotReachHere("RawArray<VArray> is not supported yet")
    }
  }

  case class VArray(elemType: SignatureType, length: Long) extends SignatureType.Proper {
    override protected def calcSymType(implicit typeProvider: TypeProvider): Type = {
      val name = VArray.name(elemType, length)
      val t = typeProvider.findClass(XString(name), loadPDB = true)
      assert(t != null, s"could not find symlevel type '$name' for $this")
      t
    }
  }

  object VArray {
    def name(elemType: SignatureType, length: Long): String = VARRAY_PREFIX(length) + eraseElemTypeToBitcode(elemType).toJETSignature

    /** LLVM cannot distinguish some types (like signed and unsigned ones). */
    def eraseElemTypeToBitcode(s: SignatureType): SignatureType = s match {
      case Boolean          => Int8
      case Unit             => Void
      case UInt8            => Int8
      case UInt16 | Float16 => Int16
      case UInt32           => Int32
      case UInt64           => Int64
      case UnicodeChar32    => Int32
      case AddrUInt | AddrInt | BString | CPointer(_) => if (Env.targetArch.is64Bit) Int64 else Int32
      case VArray(elemType, length) => VArray(eraseElemTypeToBitcode(elemType), length)
      case s => s
    }
  }

  sealed trait InstantiatedType extends SignatureType.Proper {
    def name: String
    def instantiatedTypeParameters: Seq[SignatureType]
  }

  case class InstantiatedReference(name: String, instantiatedTypeParameters: Seq[SignatureType])
    extends SignatureType.Proper with InstantiatedType {

    override def calcSymType(implicit typeProvider: TypeProvider): Type = {
      // FIXME-UG: check generic
      val t = typeProvider.findClass(XString(name), loadPDB = true)
      assert(t != null, s"could not find symlevel type '$name' for $this")
      assert(t.isClassOrInterface, s"unexpected symlevel type $t for $this")
      t
    }
  }

  case class InstantiatedRecord(name: String, instantiatedTypeParameters: Seq[SignatureType]) extends InstantiatedType {
    override def calcSymType(implicit typeProvider: TypeProvider): Type = {
      val t = typeProvider.findClass(XString(name), loadPDB = true)
      assert(t != null, s"could not find symlevel type for $this")
      assert(t.isRecord, s"unexpected symlevel type $t for $this")
      t
    }
  }

  sealed abstract class TypeVariable extends SignatureType.Proper {
    def idx: Int

    override protected def calcSymType(implicit typeProvider: TypeProvider): Type = Address.symType
  }

  // TODO: rename to FuncTypeVariable
  case class LocalTypeVariable(idx: Int) extends SignatureType.TypeVariable

  case class ClassTypeVariable(idx: Int) extends SignatureType.TypeVariable

  case object ThisTypeInfo extends SignatureType.Proper {
    override protected def calcSymType(implicit typeProvider: TypeProvider): Type = Address.symType
  }

  case class Tuple(params: Seq[SignatureType]) extends SignatureType.Proper {
    override def calcSymType(implicit typeProvider: TypeProvider): Type = shouldNotReachHere("symType for Tuple")
  }

  case class Box(base: SignatureType) extends SignatureType.Proper {
    require(!base.isReference)
    override def calcSymType(implicit typeProvider: TypeProvider): Type = shouldNotReachHere("symType for Box")
  }

  def javaLangObject(implicit typeProvider: TypeProvider): SignatureType =
    JBCReference(typeProvider.getObjectType)

  def javaLangString(implicit typeProvider: TypeProvider): SignatureType =
    JBCReference(typeProvider.getStringType)

  // TODO: remove copy-paste
  def toJBCSignature(tpe: Type): String = {
    tpe.getKind match {
      case kind if kind.isPrimitive => kind.getBCSignatureChar.toString
      case _ if tpe.isJavaArray =>
        val dims = "[" * tpe.getArrayDimnum
        s"$dims${toJBCSignature(tpe.getArrayBase)}"
      case TypeKind.RECORD => "X" + tpe.getName + ";"
      case _ => "L" + tpe.getName + ";"
    }
  }

  // TODO: avoid conversion from symlevel type to signature type, because it loses information.
  def fromSymType(tpe: Type): SignatureType = {
    if (tpe == null) {
      return null
    }
    tpe.getKind match {
      case kind if kind.isPrimitive => Primitive(kind)
      case _ if tpe.isJBCArray => JavaArray(fromSymType(tpe.getArrayBase), tpe.getArrayDimnum)
      case _ if tpe.isCangjieArray => CangjieArray(tpe.getArrayElemType)
      case _ if tpe.isVArray => VArray(tpe.getVArrayElemType, tpe.getVArrayLength)
      case _ if tpe.isArraySlice && tpe.getName != ARRAY_SLICE_NAME => ArraySlice(tpe.getArraySliceElemType)
      case TypeKind.RECORD => Record(asClassType(tpe))
      case _ => Reference(asClassType(tpe))
    }
  }

  private def primitiveJBCKind(kind: TypeKind): BytecodeTypeKind = (kind: @unchecked) match {
    case TypeKind.VOID => BytecodeTypeKind.VOID
    case TypeKind.BOOLEAN => BytecodeTypeKind.BOOLEAN
    case TypeKind.BYTE => BytecodeTypeKind.BYTE
    case TypeKind.SHORT => BytecodeTypeKind.SHORT
    case TypeKind.CHAR => BytecodeTypeKind.CHAR
    case TypeKind.INT => BytecodeTypeKind.INT
    case TypeKind.LONG => BytecodeTypeKind.LONG
    case TypeKind.FLOAT => BytecodeTypeKind.FLOAT
    case TypeKind.DOUBLE => BytecodeTypeKind.DOUBLE
  }

  /** Returns whether given signatures are equal with respect to given instantiation substitutions,
    * i.e. as if each `TypeVariable(i)` was replaced by `instantiatedTypeParameters(i)` in both signatures at any depth.
    *
    * Note: some recursive signature types are intentionally omitted here (such as CPointer and CangjieEnumWrapper)
    * because they are not expected to reference type variables.
    */
  def equalInstantiatedLegacy(instantiatedTypeParameters: Seq[SignatureType])(x: SignatureType, y: SignatureType): Boolean = (x, y) match {
    case (x: InstantiatedRecord, y: InstantiatedRecord) =>
      x.name == y.name &&
        (x.instantiatedTypeParameters zip y.instantiatedTypeParameters forall equalInstantiatedLegacy(instantiatedTypeParameters))
    case (x: InstantiatedReference, y: InstantiatedReference) =>
      x.name == y.name &&
        (x.instantiatedTypeParameters zip y.instantiatedTypeParameters forall equalInstantiatedLegacy(instantiatedTypeParameters))
    case (x: ArraySlice, y: ArraySlice) =>
      equalInstantiatedLegacy(instantiatedTypeParameters)(x.elemType, y.elemType)
    case (x: CangjieArray, y: CangjieArray) =>
      equalInstantiatedLegacy(instantiatedTypeParameters)(x.elemType, y.elemType)
    case (x: NullableWrapper, y: NullableWrapper) =>
      equalInstantiatedLegacy(instantiatedTypeParameters)(x.baseType, y.baseType)
    case (x: NonNullableWrapper, y: NonNullableWrapper) =>
      equalInstantiatedLegacy(instantiatedTypeParameters)(x.baseType, y.baseType)

    case (x: LocalTypeVariable, y: LocalTypeVariable) => instantiatedTypeParameters(x.idx) == instantiatedTypeParameters(y.idx)
    case (x: LocalTypeVariable, y) => y == instantiatedTypeParameters(x.idx)
    case (x, y: LocalTypeVariable) => x == instantiatedTypeParameters(y.idx)

    case _ => x == y
  }

  /** Returns whether given signatures are equal with respect to given instantiation substitutions,
    * i.e. as if each `ClassTypeVariable(i)` and `LocalTypeVariable(i)` was replaced by `cparams(i)` and `lparams(i)`
    * respectively in both signatures at any depth.
    */
  def equalInstantiated(cparams: Seq[SignatureType], lparams: Seq[SignatureType])(x: SignatureType, y: SignatureType): Boolean = {
    if (cparams.nonEmpty || lparams.nonEmpty) equalInstantiatedImpl(cparams, lparams)(x, y) else x == y
  }

  private[SignatureType] def equalInstantiatedImpl(cparams: Seq[SignatureType], lparams: Seq[SignatureType])(x: SignatureType, y: SignatureType): Boolean = (x, y) match {
    case (x: InstantiatedRecord, y: InstantiatedRecord) =>
      x.name == y.name &&
        x.instantiatedTypeParameters.size == y.instantiatedTypeParameters.size &&
        (x.instantiatedTypeParameters zip y.instantiatedTypeParameters forall equalInstantiatedImpl(cparams, lparams))
    case (x: InstantiatedReference, y: InstantiatedReference) =>
      x.name == y.name &&
        x.instantiatedTypeParameters.size == y.instantiatedTypeParameters.size &&
        (x.instantiatedTypeParameters zip y.instantiatedTypeParameters forall equalInstantiatedImpl(cparams, lparams))
    case (x: Tuple, y: Tuple) =>
      x.params.size == y.params.size &&
        (x.params zip y.params forall equalInstantiatedImpl(cparams, lparams))
    case (x: Box, y: Box) =>
      equalInstantiatedImpl(cparams, lparams)(x.base, y.base)
    case (x: CangjieArray, y: CangjieArray) =>
      equalInstantiatedImpl(cparams, lparams)(x.elemType, y.elemType)
    case (x: VArray, y: VArray) =>
      x.length == y.length &&
        equalInstantiatedImpl(cparams, lparams)(x.elemType, y.elemType)
    case (x: NullableWrapper, y: NullableWrapper) =>
      equalInstantiatedImpl(cparams, lparams)(x.baseType, y.baseType)
    case (x: NonNullableWrapper, y: NonNullableWrapper) =>
      equalInstantiatedImpl(cparams, lparams)(x.baseType, y.baseType)

    // TODO: is this even correct?
    case (x: LocalTypeVariable, y: LocalTypeVariable) => lparams.lift(x.idx) == lparams.lift(y.idx)
    case (x: ClassTypeVariable, y: ClassTypeVariable) => cparams.lift(x.idx) == cparams.lift(y.idx)
    case (x: LocalTypeVariable, y: ClassTypeVariable) => lparams.lift(x.idx) == cparams.lift(y.idx)
    case (x: ClassTypeVariable, y: LocalTypeVariable) => cparams.lift(x.idx) == lparams.lift(y.idx)

    case (x: LocalTypeVariable, y) => lparams.lift(x.idx).contains(y)
    case (x, y: LocalTypeVariable) => lparams.lift(y.idx).contains(x)

    case (x: ClassTypeVariable, y) => cparams.lift(x.idx).contains(y)
    case (x, y: ClassTypeVariable) => cparams.lift(y.idx).contains(x)

    case _ => x == y
  }
}
