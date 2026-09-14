package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v1203.CHIRUtils.toTypeSeq

final class BoxTypeImpl(b: Type)(using provider: CHIRItemProvider) extends CHIR.BoxType {
  def baseType: CHIR.Type = provider.getType[CHIR.Type](b.argTys(0)).get
}

trait CustomTypeImpl extends CHIR.CustomType {
  def ct: CustomType
  implicit def provider: CHIRItemProvider

  def typeDef: CHIR.CustomTypeDef = provider.getDef[CHIR.CustomTypeDef](ct.customTypeDef).get
  def genericTypeParams = ct.base.argTysVector.toTypeSeq[CHIR.Type]
}

final class ClassTypeImpl(val ct: CustomType)(using val provider: CHIRItemProvider) extends CustomTypeImpl with CHIR.ClassType {
  override def typeDef: CHIR.ClassDef = super.typeDef.asInstanceOf[CHIR.ClassDef]
}

final class EnumTypeImpl(val ct: CustomType)(using val provider: CHIRItemProvider) extends CustomTypeImpl with CHIR.EnumType {
  override def typeDef: CHIR.EnumDef = super.typeDef.asInstanceOf[CHIR.EnumDef]
}

final class StructTypeImpl(val ct: CustomType)(using val provider: CHIRItemProvider) extends CustomTypeImpl with CHIR.StructType {
  override def typeDef: CHIR.StructDef = super.typeDef.asInstanceOf[CHIR.StructDef]
}

final class CPointerTypeImpl(t: Type)(using provider: CHIRItemProvider) extends CHIR.CPointerType {
  def elementType: CHIR.Type = provider.getType[CHIR.Type](t.argTys(0)).get
}

final class RefTypeImpl(t: Type)(using provider: CHIRItemProvider) extends CHIR.RefType {
  def baseType: CHIR.Type = provider.getType[CHIR.Type](t.argTys(0)).get
}

final class RawArrayTypeImpl(t: RawArrayType)(using provider: CHIRItemProvider) extends CHIR.RawArrayType {
  def elementType: CHIR.Type = provider.getType[CHIR.Type](t.base.argTys(0)).get
  def dimension: Long = t.dims
}

final class TupleTypeImpl(t: Type)(using provider: CHIRItemProvider) extends CHIR.TupleType {
  def fieldTypes = t.argTysVector.toTypeSeq[CHIR.Type]
}

final class VArrayTypeImpl(t: VArrayType)(using provider: CHIRItemProvider) extends CHIR.VArrayType {
  def elementType: CHIR.Type = provider.getType[CHIR.Type](t.base.argTys(0)).get
  def size: Long = t.size
}

final class GenericTypeImpl(t: GenericType)(using provider: CHIRItemProvider) extends CHIR.GenericType {
  def identifier: String = t.identifier
  def upperBounds = t.upperBoundsVector.toTypeSeq[CHIR.Type]
}

final class FuncTypeImpl(f: FuncType)(using provider: CHIRItemProvider) extends CHIR.FuncType {
  private lazy val argsTypes = f.base.argTysVector.toTypeSeq[CHIR.Type]
  def paramTypes: Seq[CHIR.Type] = argsTypes.dropRight(1)
  def paramTypesWithoutReceiver: Seq[CHIR.Type] = paramTypes.tail
  def receiverType: CHIR.Type = paramTypes.head
  def returnType: CHIR.Type = argsTypes.last
  def isC: Boolean = f.isCfuncType
  def hasVarArg: Boolean = f.hasVarArg
}
