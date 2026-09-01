package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v1_0.PackageFormat.{CustomType, FuncType, GenericType, RawArrayType, Type, VArrayType}

final class BoxTypeImpl(b: Type)(using provider: CHIRItemProvider) extends CHIR.BoxType {
  override def baseType: CHIR.Type = provider.getType[CHIR.Type](b.argTys(0))
}

trait CustomTypeImpl(c: CustomType)(using provider: CHIRItemProvider) extends CHIR.CustomType {
  override def typeDef: CHIR.CustomTypeDef = provider.getDef[CHIR.CustomTypeDef](c.customTypeDef).get
  override def genericTypeParams: Seq[CHIR.Type] = {
    for (idx <- 1 to c.base.argTysLength()) yield {
      provider.getType[CHIR.Type](idx)
    }
  }
}

final class ClassTypeImpl(c: CustomType)(using provider: CHIRItemProvider) extends CustomTypeImpl(c) with CHIR.ClassType {
  override def typeDef: CHIR.ClassDef = super.typeDef.asInstanceOf[CHIR.ClassDef]
}

final class EnumTypeImpl(c: CustomType)(using provider: CHIRItemProvider) extends CustomTypeImpl(c) with CHIR.EnumType {
  override def typeDef: CHIR.EnumDef = super.typeDef.asInstanceOf[CHIR.EnumDef]
}

final class StructTypeImpl(c: CustomType)(using provider: CHIRItemProvider) extends CustomTypeImpl(c) with CHIR.StructType {
  override def typeDef: CHIR.StructDef = super.typeDef.asInstanceOf[CHIR.StructDef]
}

final class CPointerTypeImpl(t: Type)(using provider: CHIRItemProvider) extends CHIR.CPointerType {
  override def elementType: CHIR.Type = provider.getType[CHIR.Type](t.argTys(0))
}

final class RefTypeImpl(t: Type)(using provider: CHIRItemProvider) extends CHIR.RefType {
  override def baseType: CHIR.Type = provider.getType[CHIR.Type](t.argTys(0))
}

final class RawArrayTypeImpl(t: RawArrayType)(using provider: CHIRItemProvider) extends CHIR.RawArrayType {
  override def elementType: CHIR.Type = provider.getType[CHIR.Type](t.base.argTys(0))
  override def dimension: Long = t.dims
}

final class TupleTypeImpl(t: Type)(using provider: CHIRItemProvider) extends CHIR.TupleType {
  override def fieldTypes: Seq[CHIR.Type] = {
    for (idx <- 1 to t.argTysLength()) yield {
      provider.getType[CHIR.Type](idx)
    }
  }
}

final class VArrayTypeImpl(t: VArrayType)(using provider: CHIRItemProvider) extends CHIR.VArrayType {
  override def elementType: CHIR.Type = provider.getType[CHIR.Type](t.base.argTys(0))
  override def size: Long = t.size
}

final class GenericTypeImpl(t: GenericType)(using provider: CHIRItemProvider) extends CHIR.GenericType {
  override def identifier: String = t.identifier
  override def upperBounds: Seq[CHIR.Type] = {
    for (idx <- 1 to t.upperBoundsLength()) yield {
      provider.getType[CHIR.Type](idx)
    }
  }
}

final class FuncTypeImpl(f: FuncType)(using provider: CHIRItemProvider) extends CHIR.FuncType {
  private lazy val argsTypes: Seq[CHIR.Type] = {
    for (idx <- 1 to f.base.argTysLength) yield {
      provider.getType[CHIR.Type](idx)
    }
  }
  override def paramTypes: Seq[CHIR.Type] = argsTypes.init.tail
  override def receiverType: CHIR.Type = argsTypes.head
  override def returnType: CHIR.Type = argsTypes.last
  override def isC: Boolean = f.isCfuncType
  override def hasVarArg: Boolean = f.hasVarArg
}
