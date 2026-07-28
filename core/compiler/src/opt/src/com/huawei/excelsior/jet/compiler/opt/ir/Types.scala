/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.AsmType.I32
import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.compiler.Env.{bitsInByte, isStandalone, targetArch}
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, Type as SymlevelType, TypeKind as TKind}
import com.huawei.excelsior.jet.compiler.{CompilerEnvironment, Env}

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.{nowarn, tailrec}

/** Type system declaration.
  *
  * @author paul
  * @author liontiger
  */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait Types { this: CompilerEnvironment =>

  abstract class Type(val tag: Tag) {
    def isValueType = this.isInstanceOf[ValueType]
    def isStructureType = this.isInstanceOf[StructureType]
    def isFloatingPointType = this.isInstanceOf[FloatingPointType]
    def isIntegralType = this.isInstanceOf[IntegralType]
    def isNumericType = isIntegralType || isFloatingPointType
    def isTraceableRefType = this.isInstanceOf[EopType]
    def isRecordAddrType = this.isInstanceOf[RecordAddrType]
    def isHolderType = this.isInstanceOf[HolderType]

    def |(that: Type) = Type.join(this, that)
  }

  /** Type compatibility relation defined as lattice with `join` operation:
    *
    * {{{
    *           ValueType  =  Top
    *        /      |             \
    *       |    Eop.Any           \
    *       |     /    \          AddrType
    *      ...  Plain  Eop(T)     /      \
    *       |     \    /      ExecEnv  RecordAddr(T)
    *       |      Null           \      /
    *        \       |             \    /
    *       UnreachableValueType  =  Bottom
    * }}}
    */
  object Type extends PartialOrdering[Type] {
    override def tryCompare(x: Type, y: Type): Option[Int] = {
      if (x == y) Some(0)
      else (x | y) match {
        case `x` => Some(-1)
        case `y` => Some(1)
        case _ => None
      }
    }

    override def lteq(x: Type, y: Type): Boolean = (x | y) == y

    private def join(x: Type, y: Type): Type = {
      // TODO: eliminate ControlType and MemoryType
      require(x.tag == Tag.VALUE && y.tag == Tag.VALUE, s"joining unrelated types $x and $y")
      (x, y) match {
        // Same element
        case _ if x == y => x

        // Top
        case (ValueType, _) | (_, ValueType) => ValueType

        // Eop types: Top
        case (EopType.Any, _: EopType) | (_: EopType, EopType.Any) => EopType.Any

        // Eop types: Incomparable elements
        case (EopType.Eop(_), EopType.Plain) | (EopType.Plain, EopType.Eop(_)) => EopType.Any
        case (EopType.Eop(u), EopType.Eop(v)) => assert(u != v); EopType.Any

        // Eop types: Bottom
        case (EopType.Null, _: EopType) => y
        case (_: EopType, EopType.Null) => x

        // ArraySlice(_) = Record(ArraySlice.Type) TODO: reconsider
        // Why: to avoid intermediate ReinterpretCast, since they break Explosion optimization
        case (RecordAddrType(SignatureType.ArraySlice(_)), r @ RecordAddrType(y)) if y.isArraySliceLike => r
        case (RecordAddrType(x), r @ RecordAddrType(SignatureType.ArraySlice(_))) if x.isArraySliceLike => r

        // AddrType: ExecEnvType, RecordAddrType
        case (AddrType | ExecEnvType | RecordAddrType(_), AddrType | ExecEnvType | RecordAddrType(_)) => AddrType

        case (_: HolderType, _: HolderType) => y

        // Bottom
        case (UnreachableValueType, _) => y
        case (_, UnreachableValueType) => x

        // Incomparable elements
        case _ => ValueType
      }
    }
  }

  ///////////////
  // Control

  abstract class ControlType extends Type(Tag.CONTROL)
  case object ControlType extends ControlType
  case object BranchType extends ControlType
  case object UnreachableControlType extends ControlType

  ///////////////
  // Memory

  case object MemoryType extends Type(Tag.MEMORY)

  ///////////////
  // XControl

  case object XControlType extends Type(Tag.XCONTROL)

  ///////////////
  // Value

  abstract class ValueType extends Type(Tag.VALUE)

  abstract class TypeWithSize(val size: Int) extends ValueType
  object TypeWithSize {
    def unapply(t: TypeWithSize): Option[Int] = Some(t.size)
  }

  trait TypeWithKind extends ValueType {
    def kind: TKind
  }

  sealed abstract class NumericType(override val kind: TKind) extends TypeWithSize(kind.size) with TypeWithKind {
    require(kind.isIntegral || kind.isFloatingPoint)
  }
  case object NumericType extends ValueType {
    def apply(kind: SignatureType): NumericType = (kind: @unchecked) match {
      case    SignatureType.Boolean       => IntType
      case _: SignatureType.Integral      => IntegralType(kind.symKindErased)
      case _: SignatureType.FloatingPoint => FloatingPointType(kind.symKindErased)
    }

    def apply(kind: AsmType): NumericType = {
      import AsmType._
      (kind: @unchecked) match {
        case AsmType.I8 |
             AsmType.U8 |
             AsmType.I16 |
             AsmType.U16 |
             AsmType.I32 |
             AsmType.U32 => IntType
        case AsmType.I64 |
             AsmType.U64 |
             AsmType.PTR => LongType
        case AsmType.F32 => FloatType
        case AsmType.F64 => DoubleType
      }
    }

    def apply(kind: TKind): NumericType = kind match {
      case _ if kind.isIntegral => IntegralType(kind)
      case _ if kind.isFloatingPoint => FloatingPointType(kind)
      case _ => shouldNotReachHere(kind)
    }
  }

  sealed abstract class IntegralType(override val kind: TKind) extends NumericType(kind) {
    require(kind.isIntegral)
    require(!kind.isShortIntegral)
  }
  case object IntegralType extends ValueType {
    def apply(kind: TKind): IntegralType = kind match {
      case _ if kind.isShortIntegral => IntType
      case TKind.INT => IntType
      case TKind.LONG => LongType
      case _ => shouldNotReachHere(kind)
    }
  }

  case object IntType extends IntegralType(TKind.INT)
  case object LongType extends IntegralType(TKind.LONG)

  sealed abstract class FloatingPointType(override val kind: TKind) extends NumericType(kind) {
    require(kind.isFloatingPoint)
  }
  case object FloatingPointType extends ValueType {
    def apply(kind: TKind): FloatingPointType = kind match {
      case TKind.FLOAT => FloatType
      case TKind.DOUBLE => DoubleType
      case _ => shouldNotReachHere(kind)
    }
  }

  case object FloatType extends FloatingPointType(TKind.FLOAT)
  case object DoubleType extends FloatingPointType(TKind.DOUBLE)

  case object VoidType extends ValueType with TypeWithKind {
    override def kind = TKind.VOID
  }

  /** Condition type is the result of comparison with only two possible values: True() and False().
    * Not to be confused with JVM's boolean type which acts like a byte type.
    */
  case object ConditionType extends ValueType

  abstract class StructureType extends TypeWithSize(Env.addressSize)
  abstract class EopType extends StructureType

  case class RecordAddrType(sigType: SignatureType) extends StructureType {
    require(sigType.isRecord)
  }

  /** Represents [[SignatureType.TypeVariable]] signature as part of Universal Generic (UG) ABI implementation.
    * 
    * The main goal of [[HolderType]] is to help codegen to emit proper instruction according to its instantiated type.
    * That helps to keep track references correctly in runtime.
    * @param instantiatedSig type, which this holder is instantiated by.
    */
  case class HolderType(instantiatedSig: SignatureType) extends TypeWithSize(Env.addressSize)

  case object ThinType extends StructureType


  /////////////////////////////////////////////////////////////////////////////
  // Fragile pointers

  /** Fragile pointer is a value which may be invalidated at some point. For example, reference inside traceable object
    * may be invalidated by GC-point. Code ordering frameworks (BulldozerGCM or FastCodeOrdering) should rematerialize
    * such pointers after such points, preventing to use possibly invalidated value.
    *
    * Hierarchy of fragile pointers is the following:
    *
    *                                     [[FragilePointerType]]
    *                                       /                \
    *                                      /                  \
    *                     [[FragileReferenceType]]         [[ExecEnvType]]
    *                        /               \
    *                       /                 \
    *          [[IntraReferenceType]]   [[TDBarrieredReferenceType]]
    */
  abstract class FragilePointerType extends StructureType

  /** Subtype of [[FragilePointerType]] marking values pointed to (or inside) traceable objects. */
  abstract class FragileReferenceType extends FragilePointerType

  /** Subtype of [[FragileReferenceType]] marking values pointed inside traceable objects. */
  case object IntraReferenceType extends FragileReferenceType

  /** Subtype of [[FragileReferenceType]] marking values pointed to traceable objects after TD barrier. */
  case object TDBarrieredReferenceType extends FragileReferenceType

  /** Subtype of [[FragilePointerType]] marking values pointed to (or inside) ExecEnv object. */
  case object ExecEnvType extends FragilePointerType


  /////////////////////////////////////////////////////////////////////////////
  // EopType

  object EopType {
    /** Top of Eop type "hierarchy". */
    case object Any extends EopType

    /** Regular traceable reference without any enrichment.
      * Values of this type hold only plain references and null.
      */
    case object Plain extends EopType

    /** Reference enriched to known interface `t`.
      * Values of `Eop(T)` hold only pointers enriched to `T` and null.
      */
    case class Eop(t: SymlevelType) extends EopType {
      require(t.isInterface)
    }

    /** Bottom of Eop type "hierarchy". */
    case object Null extends EopType
  }

  val TRefType = EopType.Plain

  val AddrType = Env.addressSize match {
    case 4 => IntType
    case 8 => LongType
  }

  val AddrIntType = AddrType

  def addrOrIntType(tpe: Type) = (tpe == AddrType) || (tpe == IntType)

  case object ReturnAddressType extends ValueType

  // special verifier types; should not appear anywhere but VMState elements
  case object InvalidVerificationType extends ValueType

  case object VMStateType extends ValueType

  /** Bottom of value type "hierarchy" in terms of node arguments application. */
  case object UnreachableValueType extends ValueType


  /** Top of value type "hierarchy" in terms of node arguments application. */
  object ValueType extends ValueType {
    // Can't make it apply because multiple overloaded methods with default params are not allowed.
    // TODO: refactor this!
    def fromSig(sigType: SignatureType, eopTypeForInterfaces: Boolean = true, instantiateRich: Boolean = false): ValueType = {
      import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*

      Wrapper.skip(sigType) match {
        case Void    | Unit   | Nothing => VoidType
        case Boolean | Int8   | UInt8 |
             Int16   | UInt16 |
             Int32   | UInt32 | UnicodeChar32 => IntType
        case Int64   | UInt64 => LongType
        case _: AddressWide   => AddrType
        case Float32 => FloatType
        case Float64 => DoubleType
        case Float16 => IntType // Half type is implemented as value type over primitive short,
                                // so we consider it as an integral type in our IR.
                                // Operations with half values should convert them to float values in advance.
        case BString | _: CPointer => AddrType
        case ThisTypeInfo => AddrType
        case _: TypeVariable => assert(!isStandalone); HolderType(sigType)
        case _: Box => TRefType
        case _: Record | _: InstantiatedRecord | _: ArraySlice | _: VArray | _: Tuple => RecordAddrType(sigType)
        case _: JavaArray | _: CangjieArray => TRefType
        case _: Reference | _: InstantiatedReference =>
          if (!isStandalone && eopTypeForInterfaces && sigType.isInterface && !sigType.isDeferred) {
            if (instantiateRich && !typeProvider.isManagedEopUnderlyingType(sigType)) EopType.Eop(sigType.symType)
            else EopType.Any
          } else if (sigType.isThinClass) ThinType
          else TRefType
      }
    }

    def apply(symType: SymlevelType): ValueType = {
      apply(symType, eopTypeForInterfaces = true, instantiateRich = false)
    }

    def apply(symType: SymlevelType, eopTypeForInterfaces: Boolean, instantiateRich: Boolean): ValueType =
      fromSig(SignatureType.fromSymType(symType), eopTypeForInterfaces, instantiateRich)

    def apply(sigType: SignatureType): ValueType =
      fromSig(sigType)

    def apply(sigType: SignatureType, eopTypeForInterfaces: Boolean): ValueType =
      fromSig(sigType, eopTypeForInterfaces)

    def apply(sigType: SignatureType, eopTypeForInterfaces: Boolean, instantiateRich: Boolean): ValueType =
      fromSig(sigType, eopTypeForInterfaces, instantiateRich)

    def apply(tkind: TKind): ValueType = tkind match {
      case TKind.VOID => VoidType
      case _ => ValueType(tkind.toAsm)
    }

    def apply(asmType: AsmType): ValueType = apply(SignatureType.Primitive(asmType))

    def apply(bckind: BytecodeTypeKind): ValueType = ValueType(TKind.fromBytecode(bckind))

    def unapply(tpe: Type): Option[AsmType] = condOpt(tpe) {
      case IntType    => AsmType.I32
      case LongType   => AsmType.I64
      case FloatType  => AsmType.F32
      case DoubleType => AsmType.F64

      case ThinType   => AsmType.PTR
      case _: EopType | _: FragileReferenceType      => AsmType.PTR
      case _: RecordAddrType | _: FragilePointerType => AsmType.PTR
    }

    def width(tpe: Type) = {
      val ValueType(_tpe) = tpe
      _tpe.width
    }

    def toAsm(tpe: Type) = tpe match {
      case ValueType(tpe) => tpe
      case ConditionType if targetArch == CBC => I32
    }
  }


  /** Returns size in bytes of types with size. */
  def typeSize(tpe: Type): Int = tpe match {
    case TypeWithSize(size) => size

    case ConditionType if targetArch == CBC =>
      // In CBC ConditionType is a normal value type which may be moved from resource to resource (otherwise we
      // should treat all nodes producing ConditionType (e.g. TypeTest) like a FlagProducer's - rematerialize them,
      // order immediately before single use, ...). Thus we should know it's size.
      //
      // TODO-CBC: consider how ConditionType should be represented in CBC
      AddrType.size
  }

  /** Returns size in bytes of types with size. */
  def typeSizeInBits(tpe: Type): Int = typeSize(tpe) * bitsInByte
}
