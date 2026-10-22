package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v1203.CHIRUtils.toTypeSeq

final class BoxTypeImpl(b: TypeT)(using provider: CHIRItemProvider) extends CHIR.BoxType {
  def baseType: CHIR.Type = provider.getType[CHIR.Type](b.getArgTys()(0)).get
}

trait CustomTypeImpl extends CHIR.CustomType {
  def ct: CustomTypeT
  implicit def provider: CHIRItemProvider
  def typeDef: CHIR.CustomTypeDef = provider.getDef[CHIR.CustomTypeDef](ct.getCustomTypeDef).get
  def genericTypeParams = ct.getBase.getArgTys.toTypeSeq[CHIR.Type]
}

final class ClassTypeImpl(val ct: CustomTypeT)(using val provider: CHIRItemProvider) extends CustomTypeImpl with CHIR.ClassType {
  override def typeDef: CHIR.ClassDef = super.typeDef.asInstanceOf[CHIR.ClassDef]
}

final class EnumTypeImpl(val ct: CustomTypeT)(using val provider: CHIRItemProvider) extends CustomTypeImpl with CHIR.EnumType {
  override def typeDef: CHIR.EnumDef = super.typeDef.asInstanceOf[CHIR.EnumDef]
}

final class StructTypeImpl(val ct: CustomTypeT)(using val provider: CHIRItemProvider) extends CustomTypeImpl with CHIR.StructType {
  override def typeDef: CHIR.StructDef = super.typeDef.asInstanceOf[CHIR.StructDef]
}

final class CPointerTypeImpl(t: TypeT)(using provider: CHIRItemProvider) extends CHIR.CPointerType {
  def elementType: CHIR.Type = provider.getType[CHIR.Type](t.getArgTys()(0)).get
}

final class RefTypeImpl(t: TypeT)(using provider: CHIRItemProvider) extends CHIR.RefType {
  def baseType: CHIR.Type = provider.getType[CHIR.Type](t.getArgTys()(0)).get
}

final class RawArrayTypeImpl(t: RawArrayTypeT)(using provider: CHIRItemProvider) extends CHIR.RawArrayType {
  def elementType: CHIR.Type = provider.getType[CHIR.Type](t.getBase.getArgTys()(0)).get
  def dimension: Long = t.getDims
}

final class TupleTypeImpl(t: TypeT)(using provider: CHIRItemProvider) extends CHIR.TupleType {
  def fieldTypes = t.getArgTys.toTypeSeq[CHIR.Type]
}

final class VArrayTypeImpl(t: VArrayTypeT)(using provider: CHIRItemProvider) extends CHIR.VArrayType {
  def elementType: CHIR.Type = provider.getType[CHIR.Type](t.getBase.getArgTys()(0)).get
  def size: Long = t.getSize
}

final class GenericTypeImpl(t: GenericTypeT)(using provider: CHIRItemProvider) extends CHIR.GenericType {
  def identifier: String = t.getIdentifier
  def upperBounds = t.getUpperBounds.toTypeSeq[CHIR.Type]
}

final class FuncTypeImpl(f: FuncTypeT)(using provider: CHIRItemProvider) extends CHIR.FuncType {
  private def argsTypes = f.getBase.getArgTys.toTypeSeq[CHIR.Type]
  def paramTypes: Seq[CHIR.Type] = argsTypes.dropRight(1)
  def paramTypesWithoutReceiver: Seq[CHIR.Type] = paramTypes.tail
  def receiverType: CHIR.Type = paramTypes.head
  def returnType: CHIR.Type = argsTypes.last
  def isC: Boolean = f.getIsCfuncType
  def hasVarArg: Boolean = f.getHasVarArg
}
