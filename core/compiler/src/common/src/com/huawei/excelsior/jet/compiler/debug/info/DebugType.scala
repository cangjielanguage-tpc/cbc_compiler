/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.info

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.AbbreviationsElements.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.*
import com.huawei.excelsior.jet.compiler.symlevel.{Field, SignatureType, TypeKind, ClassType as SymClassType}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait TypeOrElemComparable {
  override def equals(that: Any): Boolean = {
    val startedProcessing = collection.mutable.Set[XString]()

    def equals(te1: TypeOrElemComparable, te2: TypeOrElemComparable): Boolean = {
      def equalsSuperTypes(cur: DTCompound, other: DTCompound): Boolean =
        (cur.superTypes zip other.superTypes) forall { case (x, y) => equals(x, y) }

      def equalsElements[T <: ElemOfContainer](cur: ContainerType[T], other: ContainerType[T]): Boolean =
        (cur.elements zip other.elements) forall { case (x, y) => equals(x, y) }

      def compoundEquals(cur: DTCompound, other: DTCompound): Boolean =
        cur.identifier == other.identifier && equalsSuperTypes(cur, other) && equalsElements(cur, other)

      if (te1 eq te2) {
        return true
      }

      (te1, te2) match {
        case (t1: DebugType, t2: DebugType) =>
          if (t1.identifier != t2.identifier) {
            return false
          }

          if (startedProcessing.contains(t1.identifier)) {
            return true
          }
          startedProcessing.add(t1.identifier)

          (t1, t2) match {
            case (cur: DTArray, other: DTArray)                             => equals(cur.elemType, other.elemType)
            case (cur: DTArraySlice, other: DTArraySlice)                   => compoundEquals(cur, other)
            case (cur: DTClass, other: DTClass)                             => compoundEquals(cur, other)
            case (cur: DTConst, other: DTConst)                             => equals(cur.baseType, other.baseType)
            case (cur: DTCustom, other: DTCustom)                           => (cur.sizeInBytes, cur.dwarfEncoding) == (other.sizeInBytes, other.dwarfEncoding)
            case (cur: DTEnumeration, other: DTEnumeration)                 => equals(cur.baseType, other.baseType) && equalsElements(cur, other)
            case (_: DTInterface, _: DTInterface)                           => true
            case (cur: DTOption, other: DTOption)                           => compoundEquals(cur, other)
            case (cur: DTPayloadEnumHeir, other: DTPayloadEnumHeir)         => compoundEquals(cur, other)
            case (cur: DTPayloadEnumeration, other: DTPayloadEnumeration)   => compoundEquals(cur, other)
            case (cur: DTPointer, other: DTPointer)                         => equals(cur.targetType, other.targetType)
            case (cur: DTRecord, other: DTRecord)                           => compoundEquals(cur, other)
            case (cur, other)                                               => assert(cur.getClass != other.getClass); false // implement comparison for new debug type
          }

        case (e1: ElemOfContainer, e2: ElemOfContainer) =>
          if (e1.name != e2.name) {
            return false
          }

          (e1, e2) match {
            case (cur: DTEnumerator, other: DTEnumerator)         => cur.value == other.value
            case (cur: DTInstanceField, other: DTInstanceField)   => cur.isFake == other.isFake && equals(cur.baseType, other.baseType) && equals(cur.scope, other.scope)
            case (cur: DTStaticField, other: DTStaticField)       => cur.isLocal == other.isLocal && cur.linkageName == other.linkageName && equals(cur.baseType, other.baseType) && equals(cur.scope, other.scope)
            case (cur, other)                                     => assert(cur.getClass != other.getClass); false // implement comparison for new element of container
          }

        case _ => assert(te1.getClass != te2.getClass); false // implement comparison for new TypeOrElemComparable
      }
    }

    that match {
      case that: TypeOrElemComparable => equals(this, that)
      case _ => false
    }
  }
}

trait DebugType extends TypeOrElemComparable {
  def name: XString
  def identifier: XString = name

  def serialize(number: Int => Unit, string: XString => Unit): Unit = DebugType.serialize(number, string, this)
}

object DebugType {
  private val types = Seq(DTCustom, DTArray, DTArraySlice, DTRecord, DTOption, DTPayloadEnumHeir, DTPayloadEnumeration,
    DTClass, DTCompUnit, DTInterface, DTPointer, DTConst, DTEnumeration, DTUnit)
  private val elems = Seq(DTEnumerator, DTInstanceField, DTStaticField)

  def toInt(value: Boolean): Int = if (value) 1 else 0
  def toBoolean(value: Int): Boolean = {
    assert(value == 0 || value == 1)
    value != 0
  }

  private val type2id = mutable.HashMap.empty[AnyRef, Int] ++ types.zipWithIndex
  private val id2type = type2id.map(_.swap)

  private val elem2id = mutable.HashMap.empty[AnyRef, Int] ++ elems.zipWithIndex
  private val id2elem = elem2id.map(_.swap)

  private def typeId(tpe: DebugType): Int = type2id(tpe match {
    case _: DTArray               => DTArray
    case _: DTArraySlice          => DTArraySlice
    case _: DTClass               => DTClass
    case _: DTCompUnit            => DTCompUnit
    case _: DTConst               => DTConst
    case _: DTCustom              => DTCustom
    case _: DTEnumeration         => DTEnumeration
    case _: DTInterface           => DTInterface
    case _: DTOption              => DTOption
    case _: DTPayloadEnumHeir     => DTPayloadEnumHeir
    case _: DTPayloadEnumeration  => DTPayloadEnumeration
    case _: DTPointer             => DTPointer
    case _: DTRecord              => DTRecord
  })

  private def elemId(tpe: ElemOfContainer): Int = elem2id(tpe match {
    case _: DTEnumerator => DTEnumerator
    case _: DTInstanceField => DTInstanceField
    case _: DTStaticField => DTStaticField
  })

  // TODO-DWARF get rid of multiple serializing same type for different nodes
  def serialize(number: Int => Unit, string: XString => Unit, tpe: DebugType): Unit = {
    val startedSerializing = collection.mutable.Set[XString]()

    def serializeElem(tpe: ElemOfContainer): Unit = {
      number(elemId(tpe))
      string(tpe.name)
      tpe match {
        case DTEnumerator(_, value) => number(value)
        case tpe @ DTInstanceField(_, isFake) => number(toInt(isFake)); serializeType(tpe.baseType); serializeType(tpe.scope)
        case tpe @ DTStaticField(_, ln, isLocal) => string(ln); number(toInt(isLocal)); serializeType(tpe.baseType); serializeType(tpe.scope)
      }
    }

    def serializeType(tpe: DebugType): Unit = {
      def serializeArray[T: ClassTag](elements: Array[T], serializer: T => Unit): Unit = {
        number(elements.length)
        elements foreach serializer
      }

      number(typeId(tpe))
      string(tpe.name)
      string(tpe.identifier)
      if (startedSerializing.contains(tpe.identifier)) {
        number(toInt(false))
        return
      }

      number(toInt(true))
      startedSerializing.add(tpe.identifier)
      tpe match {
        case       DTArray(_, tpe)                      => serializeType(tpe)
        case tpe @ DTClass(_, _)                        => serializeArray(tpe.superTypes, serializeType); serializeArray(tpe.elements, serializeElem)
        case       DTConst(baseType)                    => serializeType(baseType)
        case       DTCustom(_, size, encoding)          => number(size); number(encoding)
        case tpe @ DTEnumeration(_, _, baseType)        => serializeType(baseType); serializeArray(tpe.elements, serializeElem)
        case _:    DTInterface                          =>
        case tpe:  DTPlainCompound                      => serializeArray(tpe.superTypes, serializeType); serializeArray(tpe.elements, serializeElem)
        case       DTPointer(point)                     => serializeType(point)
        case tpe @ DTCompUnit(cui)                      => number(cui.language.ordinal); string(cui.directory); string(cui.producer); serializeArray(tpe.elements, serializeElem)
      }
    }

    serializeType(tpe)
  }

  def deserialize(number: () => Int, string: () => XString): DebugType = {
    val deserializedTypes = collection.mutable.Map[XString, DebugType]()

    def deserializeElem(): ElemOfContainer = {
      val id = number()
      val name = string()
      val tpe = id2elem(id) match {
        case DTEnumerator => DTEnumerator(name, number())
        case DTInstanceField => DTInstanceField(name, toBoolean(number()))
        case DTStaticField => DTStaticField(name, string(), toBoolean(number()))
      }
      tpe match {
        case tpe: DTField => tpe.baseType = deserializeType(); tpe.scope = deserializeType().asInstanceOf[DTCompound]
        case _ =>
      }
      tpe
    }

    def deserializeType(): DebugType = {
      def deserializeArray[T: ClassTag](deserializer: () => T): Array[T] = Array.fill(number()) { deserializer() }
      def deserializeTypeArray(): Array[DebugType] = deserializeArray[DebugType](() => deserializeType())
      def deserializeElemArray[T: ClassTag](): Array[T] = deserializeArray(() => deserializeElem().asInstanceOf[T])
      def deserializeSuperTypes(tpe: DTCompound): Unit = tpe.superTypes = deserializeTypeArray()
      def deserializeElements[T: ClassTag](tpe: ContainerType[T]): Unit = tpe.elements = deserializeElemArray[T]()

      val id = number()
      val name = string()
      val identifier = string()
      val needDeserialize = toBoolean(number())
      if (deserializedTypes.contains(identifier)) {
        assert(!needDeserialize)
        return deserializedTypes(identifier)
      }

      assert(needDeserialize)
      val tpe = id2type(id) match {
        case DTArray                => DTArray(name)
        case DTArraySlice           => DTArraySlice(name, identifier)
        case DTClass                => DTClass(name, identifier)
        case DTCompUnit             => DTCompUnit(CompilationUnitInfo(name, Language.fromOrdinal(number()), string(), string()))
        case DTConst                => DTConst()
        case DTCustom               => DTCustom(name, number(), number())
        case DTEnumeration          => DTEnumeration(name, identifier, deserializeType().asInstanceOf[DTCustom])
        case DTInterface            => DTInterface(name, identifier)
        case DTOption               => DTOption(name, identifier)
        case DTPayloadEnumHeir      => DTPayloadEnumHeir(name, identifier)
        case DTPayloadEnumeration   => DTPayloadEnumeration(name, identifier)
        case DTPointer              => DTPointer()
        case DTRecord               => DTRecord(name, identifier)
      }

      deserializedTypes(identifier) = tpe

      tpe match {
        case tpe: DTArray => tpe.elemType = deserializeType()
        case tpe: DTClass => deserializeSuperTypes(tpe); deserializeElements[DTField](tpe)
        case tpe: DTCompUnit => deserializeElements(tpe)
        case tpe: DTConst => tpe.baseType = deserializeType()
        case tpe: DTEnumeration => deserializeElements(tpe)
        case tpe: DTPlainCompound => deserializeSuperTypes(tpe); deserializeElements(tpe)
        case tpe: DTPointer => tpe.targetType = deserializeType()
        case _ =>
      }

      tpe
    }

    deserializeType()
  }
}

// TODO-DWARF: its not normal to be DWARF-specific here
case class DTCustom(name: XString, sizeInBytes: Int, dwarfEncoding: Int) extends DebugType

object DTCustom {
  val BoolInstance       = DTCustom(XString("Bool"),       1, DW_ATE_boolean)
  val Int8Instance       = DTCustom(XString("Int8"),       1, DW_ATE_signed)
  val UInt8Instance      = DTCustom(XString("UInt8"),      1, DW_ATE_unsigned)
  val Int16Instance      = DTCustom(XString("Int16"),      2, DW_ATE_signed)
  val UInt16Instance     = DTCustom(XString("UInt16"),     2, DW_ATE_unsigned)
  val Int32Instance      = DTCustom(XString("Int32"),      4, DW_ATE_signed)
  val UInt32Instance     = DTCustom(XString("UInt32"),     4, DW_ATE_unsigned)
  val Int64Instance      = DTCustom(XString("Int64"),      8, DW_ATE_signed)
  val UInt64Instance     = DTCustom(XString("UInt64"),     8, DW_ATE_unsigned)
  val IntNativeInstance  = DTCustom(XString("IntNative"),  8, DW_ATE_signed)
  val UIntNativeInstance = DTCustom(XString("UIntNative"), 8, DW_ATE_unsigned)
  val Float16Instance    = DTCustom(XString("Float16"),    2, DW_ATE_float)
  val Float32Instance    = DTCustom(XString("Float32"),    4, DW_ATE_float)
  val Float64Instance    = DTCustom(XString("Float64"),    8, DW_ATE_float)
  val Char32Instance     = DTCustom(XString("char32_t"),   4, DW_ATE_UTF)

  val basicTypes = List(BoolInstance, Int8Instance, UInt8Instance, Int16Instance, UInt16Instance, Int32Instance,
    UInt32Instance, Int64Instance, UInt64Instance, IntNativeInstance, UIntNativeInstance, Float16Instance,
    Float32Instance, Float64Instance, Char32Instance)

  def apply(tpe: SignatureType): DTCustom = apply(tpe.symKindErased)

  def apply(tpe: TypeKind): DTCustom = tpe match {
    case VOID     => DTUnit()
    case BOOLEAN  => BoolInstance
    case BYTE     => Int8Instance
    case SHORT    => Int16Instance
    case CHAR     => UInt16Instance
    case INT      => Int32Instance
    case LONG     => Int64Instance
    case FLOAT    => Float32Instance
    case DOUBLE   => Float64Instance
    case _        => DTUnit()
  }

  def applyWithBasicTypes(name: XString, sizeInBytes: Int, dwarfEncoding: Int): DTCustom = name match {
    case DTUnit.name  => DTUnit()
    case name         => basicTypes.find(elem => elem.name.equals(name)).getOrElse(DTCustom(name, sizeInBytes, dwarfEncoding))
  }

  def toSignaturePrimitive(`type`: DTCustom): Option[Primitive] = `type` match {
    case DTUnit.`DTUnitInstance`        => Some(Void)
    case DTCustom.`BoolInstance`        => Some(Boolean)
    case DTCustom.`Int8Instance`        => Some(Int8)
    case DTCustom.`UInt8Instance`       => Some(UInt8)
    case DTCustom.`Int16Instance`       => Some(Int16)
    case DTCustom.`UInt16Instance`      => Some(UInt16)
    case DTCustom.`Int32Instance`       => Some(Int32)
    case DTCustom.`UInt32Instance`      => Some(UInt32)
    case DTCustom.`Int64Instance`       => Some(Int64)
    case DTCustom.`UInt64Instance`      => Some(UInt64)
    case DTCustom.`IntNativeInstance`   => Some(AddrInt)
    case DTCustom.`UIntNativeInstance`  => Some(AddrUInt)
    case DTCustom.`Float16Instance`     => Some(Float16)
    case DTCustom.`Float32Instance`     => Some(Float32)
    case DTCustom.`Float64Instance`     => Some(Float64)
    case DTCustom.`Char32Instance`      => Some(UnicodeChar32)
    case _ => None
  }
}

object DTUnit {
  val name = XString("Unit")
  val DTUnitInstance = DTCustom(name, 1, 0)

  def apply(): DTCustom = DTUnitInstance
}

case class DTArray(name: XString, var elemType: DebugType = DTUnit()) extends DebugType

case class DTInterface(name: XString, override val identifier: XString) extends DebugType

case class DTPointer(var targetType: DebugType = DTUnit()) extends DebugType {
  def name = XString("*" + targetType.identifier)
}

case class DTConst(var baseType: DebugType = DTUnit()) extends DebugType {
  def name = XString("const " + baseType.identifier)
}

trait ElemOfContainer extends TypeOrElemComparable {
  def name: XString
}

abstract class ContainerType[T: ClassTag] extends DebugType {
  var elements: Array[T] = new Array[T](0)

  override def hashCode(): Int = name.hashCode() * 31
}

case class DTEnumerator(name: XString, value: Int) extends ElemOfContainer

case class DTEnumeration(name: XString, override val identifier: XString, baseType: DTCustom) extends ContainerType[DTEnumerator]

object DTCompound {
  val RecordPrefix = XString("record.")
  val ClassPrefix = XString("")
  val ObjectName = XString("Object")
}

trait DTCompound extends ContainerType[DTField] {
  var superTypes: Array[DebugType] = new Array[DebugType](0)
  def prefix: XString
}

abstract class DTField extends ElemOfContainer {
  var baseType: DebugType = DTUnit()
  var scope: DTCompound = null
}

case class DTInstanceField(name: XString, isFake: Boolean = false) extends DTField

object DTInstanceField {
  def apply(name: XString, baseType: DebugType, scope: DTCompound): DTInstanceField = {
    val newInstance = DTInstanceField(name)
    newInstance.baseType = baseType
    newInstance.scope = scope
    newInstance
  }
}

case class DTStaticField(name: XString, linkageName: XString = XString.empty, isLocal: Boolean = true) extends DTField

abstract class DTPlainCompound extends DTCompound {
  override def prefix = DTCompound.RecordPrefix
}

case class DTRecord(name: XString, override val identifier: XString) extends DTPlainCompound {
  override def prefix = elements match {
    case Array() => DTCompound.ClassPrefix
    case _ => DTCompound.RecordPrefix
  }
}

case class DTOption(name: XString, override val identifier: XString) extends DTPlainCompound {
  def payload = elements.last.baseType
}

case class DTArraySlice(name: XString, override val identifier: XString) extends DTPlainCompound

case class DTPayloadEnumHeir(name: XString, override val identifier: XString) extends DTPlainCompound
case class DTPayloadEnumeration(name: XString, override val identifier: XString) extends DTPlainCompound

case class DTClass(name: XString, override val identifier: XString) extends DTCompound {
  override def prefix = DTCompound.ClassPrefix
}

case class DTCompUnit(cuInfo: CompilationUnitInfo) extends DTCompound {
  override def prefix = DTCompound.ClassPrefix
  override val name = cuInfo.name
  override val identifier = name
}

case class Declaration(file: XString, line: Int)

case class LocalVariable(name: XString, varType: DebugType, argIndex: Int, declaration: Option[Declaration]) {
  def isArgument = argIndex >= 0
}
