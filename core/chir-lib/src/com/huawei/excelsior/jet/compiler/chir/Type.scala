package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.chir.CHIRUtils.toSeq
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.*

final class BoxTypeImpl(b: Type)(using provider: CHIRItemProvider) extends CHIR.BoxType {
  override def baseType: CHIR.Type = provider.getType[CHIR.Type](b.argTys(0)).get
}

trait CustomTypeImpl(c: CustomType)(using provider: CHIRItemProvider) extends CHIR.CustomType {
  override def typeDef: CHIR.CustomTypeDef = provider.getDef[CHIR.CustomTypeDef](c.customTypeDef).get
  override def genericTypeParams: Seq[CHIR.Type] = {
    for (idx <- c.base.argTysVector.toSeq) yield {
      provider.getType[CHIR.Type](idx).get
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
  override def elementType: CHIR.Type = provider.getType[CHIR.Type](t.argTys(0)).get
}

final class RefTypeImpl(t: Type)(using provider: CHIRItemProvider) extends CHIR.RefType {
  override def baseType: CHIR.Type = provider.getType[CHIR.Type](t.argTys(0)).get
}

final class RawArrayTypeImpl(t: RawArrayType)(using provider: CHIRItemProvider) extends CHIR.RawArrayType {
  override def elementType: CHIR.Type = provider.getType[CHIR.Type](t.base.argTys(0)).get
  override def dimension: Long = t.dims
}

final class TupleTypeImpl(t: Type)(using provider: CHIRItemProvider) extends CHIR.TupleType {
  override def fieldTypes: Seq[CHIR.Type] = {
    for (idx <- t.argTysVector.toSeq) yield {
      provider.getType[CHIR.Type](idx).get
    }
  }
}

final class VArrayTypeImpl(t: VArrayType)(using provider: CHIRItemProvider) extends CHIR.VArrayType {
  override def elementType: CHIR.Type = provider.getType[CHIR.Type](t.base.argTys(0)).get
  override def size: Long = t.size
}

final class GenericTypeImpl(t: GenericType)(using provider: CHIRItemProvider) extends CHIR.GenericType {
  override def identifier: String = t.identifier
  override def upperBounds: Seq[CHIR.Type] = {
    for (idx <- t.upperBoundsVector.toSeq) yield {
      provider.getType[CHIR.Type](idx).get
    }
  }
}

final class FuncTypeImpl(f: FuncType)(using provider: CHIRItemProvider) extends CHIR.FuncType {
  private lazy val argsTypes: Seq[CHIR.Type] = {
    for (idx <- f.base.argTysVector.toSeq) yield {
      provider.getType[CHIR.Type](idx).get
    }
  }
  override def paramTypes: Seq[CHIR.Type] = argsTypes.dropRight(1)
  override def receiverType: CHIR.Type = paramTypes.head
  override def returnType: CHIR.Type = argsTypes.last
  override def isC: Boolean = f.isCfuncType
  override def hasVarArg: Boolean = f.hasVarArg
}

// TODO now it's unused in parser
final class ThisTypeImpl(t: Type) extends CHIR.Type {}