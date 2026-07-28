/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{CangjieFieldReference, Field, SignatureType}

trait CangjieNodes { self: Universe =>

  sealed trait FieldSeqOperation extends Node {
    def fields: Seq[CangjieFieldReference]
    def refType: SignatureType = FieldSeqOperation.refType(fields)
    def resType: SignatureType = FieldSeqOperation.resType(fields)
  }

  object FieldSeqOperation {
    def refType(fields: Seq[CangjieFieldReference]): SignatureType = fields.head.refType
    def resType(fields: Seq[CangjieFieldReference]): SignatureType = fields.last.fieldType

    def refTpe(fields: Seq[CangjieFieldReference]): Type = ValueType.fromSig(refType(fields))
    def resTpe(fields: Seq[CangjieFieldReference]): Type = ValueType.fromSig(resType(fields))
    def resAddrTpe(fields: Seq[CangjieFieldReference]): Type = {
      val res = resType(fields)
      if (res.isRecord) ValueType.fromSig(res) else AddrType
    }
  }

  sealed trait InstanceFieldSeqOperation extends FieldSeqOperation {
    def obj: Node
  }

  class GetFieldSeqRef private(proto: GetFieldSeqRef.Proto)
    extends FloatingNodeWithFixedArgs(proto) with InstanceFieldSeqOperation {

    override def fields: Seq[CangjieFieldReference] = proto.fields

    def obj = arg(0)
  }

  object GetFieldSeqRef {
    case class Proto private[GetFieldSeqRef](fields: Seq[CangjieFieldReference])
      extends FixedArgs[GetFieldSeqRef](FieldSeqOperation.refTpe(fields))(FieldSeqOperation.resAddrTpe(fields)) {
      require(fields.head.field.forall(!_.isStatic))

      def newInstance() = new GetFieldSeqRef(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference])(obj: Node): Node =
      proto(fields)(obj)

    def unapply(x: GetFieldSeqRef) = Some(x.fields, x.obj)
  }

  class GetStaticFieldSeqRef private(proto: GetStaticFieldSeqRef.Proto)
    extends FloatingNodeWithFixedArgs(proto) with FieldSeqOperation {

    override def fields: Seq[CangjieFieldReference] = proto.fields
  }

  object GetStaticFieldSeqRef {
    case class Proto private[GetStaticFieldSeqRef](fields: Seq[CangjieFieldReference])
      extends FixedArgs[GetStaticFieldSeqRef]()(FieldSeqOperation.resAddrTpe(fields)) {
      require(fields.head.field.exists(_.isStatic))

      def newInstance() = new GetStaticFieldSeqRef(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference]): Node =
      proto(fields)()

    def unapply(x: GetStaticFieldSeqRef) = Some(x.fields)
  }

  class LoadFieldSeq private(proto: LoadFieldSeq.Proto)
    extends FloatingNodeWithFixedArgs(proto) with InstanceFieldSeqOperation with ControlledNode with HasInMemory {

    override def fields: Seq[CangjieFieldReference] = proto.fields

    def obj = arg(2)
  }

  object LoadFieldSeq {
    case class Proto private[LoadFieldSeq](fields: Seq[CangjieFieldReference])
      extends FixedArgs[LoadFieldSeq](ControlType, MemoryType, FieldSeqOperation.refTpe(fields))(FieldSeqOperation.resTpe(fields)) {
      require(fields.head.field.forall(!_.isStatic))

      def newInstance() = new LoadFieldSeq(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference])(obj: Node): Node =
      proto(fields)(obj)

    def unapply(x: LoadFieldSeq) = Some(x.fields, x.obj)
  }

  class LoadStaticFieldSeq private(proto: LoadStaticFieldSeq.Proto)
    extends FloatingNodeWithFixedArgs(proto) with FieldSeqOperation with ControlledNode with HasInMemory {

    override def fields: Seq[CangjieFieldReference] = proto.fields
  }

  object LoadStaticFieldSeq {
    case class Proto private[LoadStaticFieldSeq](fields: Seq[CangjieFieldReference])
      extends FixedArgs[LoadStaticFieldSeq](ControlType, MemoryType)(FieldSeqOperation.resTpe(fields)) {
      require(fields.head.field.exists(_.isStatic))

      def newInstance() = new LoadStaticFieldSeq(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference]): Node =
      proto(fields)()

    def unapply(x: LoadStaticFieldSeq) = Some(x.fields)
  }

  class StoreFieldSeq private(proto: StoreFieldSeq.Proto)
    extends NodeWithFixedArgs(proto) with InstanceFieldSeqOperation with SpinalMemoryNode with NotProducesValue {

    override def fields: Seq[CangjieFieldReference] = proto.fields

    def obj = arg(2)
    def inValue = arg(3)
  }

  object StoreFieldSeq {
    case class Proto private[StoreFieldSeq](fields: Seq[CangjieFieldReference])
      extends FixedArgs[StoreFieldSeq](ControlType, MemoryType, FieldSeqOperation.refTpe(fields), FieldSeqOperation.resTpe(fields))(VoidType)
      with ControlMemoryTagged[StoreFieldSeq] {
      require(fields.head.field.forall(!_.isStatic))

      def newInstance() = new StoreFieldSeq(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference])(obj: Node, value: Node): Node =
      proto(fields)(obj, value)

    def unapply(x: StoreFieldSeq) = Some(x.fields, x.obj, x.inValue)
  }

  class StoreStaticFieldSeq private(proto: StoreStaticFieldSeq.Proto)
    extends NodeWithFixedArgs(proto) with FieldSeqOperation with SpinalMemoryNode with NotProducesValue {

    override def fields: Seq[CangjieFieldReference] = proto.fields

    def inValue = arg(2)
  }

  object StoreStaticFieldSeq {
    case class Proto private[StoreStaticFieldSeq](fields: Seq[CangjieFieldReference])
      extends FixedArgs[StoreStaticFieldSeq](ControlType, MemoryType, FieldSeqOperation.resTpe(fields))(VoidType)
      with ControlMemoryTagged[StoreStaticFieldSeq] {
      require(fields.head.field.exists(_.isStatic))

      def newInstance() = new StoreStaticFieldSeq(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference])(value: Node): Node =
      proto(fields)(value)

    def unapply(x: StoreStaticFieldSeq) = Some(x.fields, x.inValue)
  }

  /** DerivedPtr acts as an anchor for mut pair (base, derived) in mut functions. */
  class DerivedPtr private(proto: DerivedPtr.Proto)
    extends FloatingNodeWithFixedArgs(proto) {

    def recordType = proto.recordType

    def base = arg(0)
    def derived = arg(1)
  }

  object DerivedPtr {
    case class Proto private[DerivedPtr](recordType: SignatureType)
      extends FixedArgs[DerivedPtr](TRefType, AddrType)(ValueType.fromSig(recordType)) {
      require(recordType.isRecord)

      def newInstance() = new DerivedPtr(this)
    }

    def proto(recordType: SignatureType) = {
      Prototype.intern(Proto(recordType))
    }

    def apply(recordType: SignatureType)(base: Node, derived: Node): Node =
      proto(recordType)(base, derived)

    def unapply(x: DerivedPtr) = Some(x.base, x.derived)

    // Base pointers

    class Local private extends LeafNode[Local](TRefType) with FloatingNode with Constant

    object Local {
      private lazy val instance = new Local
      def apply() = instance()
    }

    class Global private extends LeafNode[Global](TRefType) with FloatingNode with Constant

    object Global {
      private lazy val instance = new Global
      def apply() = instance()
    }
  }

  class LoadTypeInfo private(proto: LoadTypeInfo.Proto)
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode with ControlledNode {
    def target: SignatureType = proto.target
  }

  object LoadTypeInfo {
    case class Proto private[LoadTypeInfo](target: SignatureType)
      extends FixedArgs[LoadTypeInfo](ControlType)(AddrType) with PrototypeStrictNodeClass[LoadTypeInfo, LoadTypeInfo] {
      def newInstance() = new LoadTypeInfo(this)
    }

    def apply(x: SignatureType) = proto(x)()

    def proto(x: SignatureType) = Prototype.intern(Proto(x))
  }

  class Box private(proto: Box.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue with CanThrow {
    def base = proto.base

    def baseTypeInfo = arg(2)
    def value = arg(3)
  }

  object Box {
    case class Proto private[Box](base: SignatureType)
      extends FixedArgs[Box](ControlType, MemoryType, AddrType, ValueType.fromSig(base, instantiateRich = true))(ValueType.fromSig(SignatureType.Box(base)))
      with ControlMemoryValueTagged[Box] {

      def newInstance() = new Box(this)
    }

    def proto(base: SignatureType) = Prototype.intern(Proto(base))
    def apply(base: SignatureType)(baseTypeInfo: Node, arg: Node) = proto(base)(baseTypeInfo, arg)
  }

  class Unbox private(proto: Unbox.Proto) extends FloatingNodeWithFixedArgs(proto) with ControlledNode with HasInMemory {
    def base = proto.base

    def baseTypeInfo = arg(2)
    def value = arg(3)
  }

  object Unbox {
    case class Proto private[Unbox](base: SignatureType)
      extends FixedArgs[Unbox](ControlType, MemoryType, AddrType, ValueType.fromSig(SignatureType.Box(base)))(ValueType.fromSig(base, instantiateRich = true)) {

      def newInstance() = new Unbox(this)
    }

    def proto(base: SignatureType) = Prototype.intern(Proto(base))
    def apply(base: SignatureType)(baseTypeInfo: Node, arg: Node) = proto(base)(baseTypeInfo, arg)
  }

  class SpawnFuture private(proto: SpawnFuture.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue with CanThrow {
    def retType = proto.retType

    def future = arg(2)
  }

  object SpawnFuture {
    case class Proto private[SpawnFuture](retType: SignatureType)
      extends FixedArgs[SpawnFuture](ControlType, MemoryType, TRefType)(TRefType)
        with ControlMemoryValueTagged[SpawnFuture] {

      def newInstance() = new SpawnFuture(this)
    }

    def proto(retType: SignatureType) = Prototype.intern(Proto(retType))
    def apply(retType: SignatureType)(future: Node) = proto(retType)(future)
  }

}
