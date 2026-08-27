/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{CangjieFieldReference, Field, SignatureType}

trait CangjieNodes { self: Universe =>

  sealed trait FieldSeqOperation extends Node {
    require(!resType.isZST)
    def fields: Seq[CangjieFieldReference]
    def refType: SignatureType = FieldSeqOperation.refType(fields)
    def resType: SignatureType = FieldSeqOperation.resType(fields)
  }

  object FieldSeqOperation {
    def refType(fields: Seq[CangjieFieldReference]): SignatureType = fields.head.refType
    def resType(fields: Seq[CangjieFieldReference]): SignatureType = fields.last.fieldType

    def refTpe(fields: Seq[CangjieFieldReference]): Type = ValueType.fromSig(refType(fields))
    def resTpe(fields: Seq[CangjieFieldReference]): Type = {
      val res = resType(fields)
      res match {
        case res: SignatureType.OptionLikeEnum if res.someType.isTypeVariable => TRefType
        case _ => if (res.isTypeVariable) TRefType else ValueType.fromSig(res)
      }
    }
    def resAddrTpe(fields: Seq[CangjieFieldReference]): Type = {
      val res = resType(fields)
      res match {
        case res: SignatureType.OptionLikeEnum if res.someType.isTypeVariable => TRefType
        case _ => if (res.isRecord) ValueType.fromSig(res) else AddrType
      }
    }

    def typeInfos(fields: Seq[CangjieFieldReference]): Seq[Type] = {
      // Ref types + res type
      Seq.fill(fields.size + 1)(AddrType)
    }
  }

  sealed trait InstanceFieldSeqOperation extends FieldSeqOperation {
    def obj: Node
  }

  sealed trait GenericFieldSeqOperation extends FieldSeqOperation {
    def typeInfos: Seq[Node]
  }

  class GetFieldSeqRef private(proto: GetFieldSeqRef.Proto)
    extends FloatingNodeWithFixedArgs(proto) with InstanceFieldSeqOperation with ControlledNode {

    override def fields: Seq[CangjieFieldReference] = proto.fields

    def obj = arg(1)
  }

  object GetFieldSeqRef {
    case class Proto private[GetFieldSeqRef](fields: Seq[CangjieFieldReference])
      extends FixedArgs[GetFieldSeqRef](ControlType, FieldSeqOperation.refTpe(fields))(FieldSeqOperation.resAddrTpe(fields)) {
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

  class GetFieldSeqRefGeneric private(proto: GetFieldSeqRefGeneric.Proto)
    extends FloatingNodeWithFixedArgs(proto) with InstanceFieldSeqOperation with GenericFieldSeqOperation with ControlledNode {

    override def fields: Seq[CangjieFieldReference] = proto.fields

    def obj = arg(1)
    def typeInfos = argsTail(2)
  }

  object GetFieldSeqRefGeneric {
    case class Proto private[GetFieldSeqRefGeneric](fields: Seq[CangjieFieldReference])
      extends FixedArgs[GetFieldSeqRefGeneric]
        (ControlType +: FieldSeqOperation.refTpe(fields) +: FieldSeqOperation.typeInfos(fields): _*)
        (FieldSeqOperation.resAddrTpe(fields)) {
      require(fields.head.field.forall(!_.isStatic))

      def newInstance() = new GetFieldSeqRefGeneric(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference])(obj: Node, typeInfos: Seq[Node]): Node =
      proto(fields)(obj +: typeInfos: _*)

    def unapply(x: GetFieldSeqRefGeneric) = Some(x.fields, x.obj, x.typeInfos)
  }

  class GetStaticFieldSeqRef private(proto: GetStaticFieldSeqRef.Proto)
    extends FloatingNodeWithFixedArgs(proto) with FieldSeqOperation with ControlledNode {

    override def fields: Seq[CangjieFieldReference] = proto.fields
  }

  object GetStaticFieldSeqRef {
    case class Proto private[GetStaticFieldSeqRef](fields: Seq[CangjieFieldReference])
      extends FixedArgs[GetStaticFieldSeqRef](ControlType)(FieldSeqOperation.resAddrTpe(fields)) {
      require(fields.head.field.exists(_.isStatic))
      require(fields.size == 1 || !fields.head.fieldType.isTraceableReference, fields)

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
      require(!FieldSeqOperation.resType(fields).isRecord)
      require(!FieldSeqOperation.resType(fields).isZST)

      def newInstance() = new LoadFieldSeq(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference])(obj: Node): Node =
      proto(fields)(obj)

    def unapply(x: LoadFieldSeq) = Some(x.fields, x.obj)
  }

  class LoadFieldSeqGeneric private(proto: LoadFieldSeqGeneric.Proto)
    extends NodeWithFixedArgs(proto) with InstanceFieldSeqOperation with GenericFieldSeqOperation
      with SpinalMemoryNode with ProducesValue with CanThrow {

    override def fields: Seq[CangjieFieldReference] = proto.fields

    def obj = arg(2)
    def typeInfos = argsTail(3)
  }

  object LoadFieldSeqGeneric {
    case class Proto private[LoadFieldSeqGeneric](fields: Seq[CangjieFieldReference])
      extends FixedArgs[LoadFieldSeqGeneric]
        (Seq(ControlType, MemoryType, FieldSeqOperation.refTpe(fields)) ++ FieldSeqOperation.typeInfos(fields): _*)
        (FieldSeqOperation.resTpe(fields)) with ControlMemoryValueTagged[LoadFieldSeqGeneric] {
      require(fields.head.field.forall(!_.isStatic))

      def newInstance() = new LoadFieldSeqGeneric(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference])(obj: Node, typeInfos: Seq[Node]): Node =
      proto(fields)(obj +: typeInfos: _*)

    def unapply(x: LoadFieldSeqGeneric) = Some(x.fields, x.obj, x.typeInfos)
  }

  class LoadStaticFieldSeq private(proto: LoadStaticFieldSeq.Proto)
    extends FloatingNodeWithFixedArgs(proto) with FieldSeqOperation with ControlledNode with HasInMemory {

    override def fields: Seq[CangjieFieldReference] = proto.fields
  }

  object LoadStaticFieldSeq {
    case class Proto private[LoadStaticFieldSeq](fields: Seq[CangjieFieldReference])
      extends FixedArgs[LoadStaticFieldSeq](ControlType, MemoryType)(FieldSeqOperation.resTpe(fields)) {
      require(fields.head.field.exists(_.isStatic))
      require(!FieldSeqOperation.resType(fields).isRecord)
      require(!FieldSeqOperation.resType(fields).isZST)
      require(fields.size == 1 || !fields.head.fieldType.isTraceableReference, fields)

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
      require(!FieldSeqOperation.resType(fields).isRecord)
      require(!FieldSeqOperation.resType(fields).isZST)

      def newInstance() = new StoreFieldSeq(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference])(obj: Node, value: Node): Node =
      proto(fields)(obj, value)

    def unapply(x: StoreFieldSeq) = Some(x.fields, x.obj, x.inValue)
  }

  class StoreFieldSeqGeneric private(proto: StoreFieldSeqGeneric.Proto)
    extends NodeWithFixedArgs(proto) with InstanceFieldSeqOperation with GenericFieldSeqOperation
      with SpinalMemoryNode with NotProducesValue {

    override def fields: Seq[CangjieFieldReference] = proto.fields

    def obj = arg(2)
    def inValue = arg(3)
    def typeInfos = argsTail(4)
  }

  object StoreFieldSeqGeneric {
    case class Proto private[StoreFieldSeqGeneric](fields: Seq[CangjieFieldReference])
      extends FixedArgs[StoreFieldSeqGeneric]
        (Seq(ControlType, MemoryType, FieldSeqOperation.refTpe(fields), FieldSeqOperation.resTpe(fields)) ++ FieldSeqOperation.typeInfos(fields): _*)
        (VoidType) with ControlMemoryTagged[StoreFieldSeqGeneric] {
      require(fields.head.field.forall(!_.isStatic))

      def newInstance() = new StoreFieldSeqGeneric(this)
    }

    def proto(fields: Seq[CangjieFieldReference]) = {
      Prototype.intern(Proto(fields))
    }

    def apply(fields: Seq[CangjieFieldReference])(obj: Node, value: Node, typeInfos: Seq[Node]): Node =
      proto(fields)(obj +: value +: typeInfos: _*)

    def unapply(x: StoreFieldSeqGeneric) = Some(x.fields, x.obj, x.inValue, x.typeInfos)
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
      require(!FieldSeqOperation.resType(fields).isRecord)
      require(!FieldSeqOperation.resType(fields).isZST)
      require(fields.size == 1 || !fields.head.fieldType.isTraceableReference, fields)

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

  class SMutObjectArg private(proto: SMutObjectArg.Proto) extends FloatingNodeWithFixedArgs(proto) {
    def recArg: SMutRecArg = arg(0).asInstanceOf[SMutRecArg]
  }

  object SMutObjectArg {
    class Proto private[SMutObjectArg] extends FixedArgs[SMutObjectArg](AddrType)(TRefType) {
      def newInstance() = new SMutObjectArg(this)
    }

    def proto() = Prototype.intern(Proto())

    def apply(recArg: Node): Node = proto()(recArg)
  }

  class SMutRecArg private(proto: SMutRecArg.Proto) extends FloatingNodeWithFixedArgs(proto) {
    def receiver: Node = arg(0)
  }

  object SMutRecArg {
    class Proto private[SMutRecArg](receiverType: Type) extends FixedArgs[SMutRecArg](receiverType)(AddrType) {
      def newInstance() = new SMutRecArg(this)
    }

    def proto(receiverType: Type) = Prototype.intern(Proto(receiverType))

    def apply(receiver: Node): Node = proto(receiver.tpe)(receiver)
  }

  class LoadTypeInfo private(proto: LoadTypeInfo.Proto)
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode with ControlledNode {
    def target: SignatureType = proto.target
  }

  object LoadTypeInfo {
    case class Proto private[LoadTypeInfo](target: SignatureType)
      extends FixedArgs[LoadTypeInfo](ControlType)(AddrType) with PrototypeStrictNodeClass[LoadTypeInfo, LoadTypeInfo] {
      assert(!target.containsTypeVariables, target)
      def newInstance() = new LoadTypeInfo(this)
    }

    def apply(x: SignatureType) = proto(x)()

    def proto(x: SignatureType) = Prototype.intern(Proto(x))
  }

  class LoadTypeInfoGeneric private(proto: LoadTypeInfoGeneric.Proto)
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode with ControlledNode {
    def target: SignatureType = proto.target
  }

  object LoadTypeInfoGeneric {
    case class Proto private[LoadTypeInfoGeneric](target: SignatureType)
      extends FixedArgs[LoadTypeInfoGeneric](ControlType +: Seq.fill(typeParamsCount(target))(AddrType): _*)(AddrType)
        with PrototypeStrictNodeClass[LoadTypeInfoGeneric, LoadTypeInfoGeneric] {
      assert(target.containsTypeVariables, target)
      assert(typeParamsCount(target) <= 6, target)
      def newInstance() = new LoadTypeInfoGeneric(this)
    }

    private def typeParamsCount(sig: SignatureType): Int = {
      import SignatureType.*
      (sig: @unchecked) match {
        case sig: InstantiatedType  => sig.instantiatedTypeParameters.size
        case sig: Tuple             => sig.params.size
        case sig: CangjieEnum       => sig.params.size
        case _: ArraySlice | _: CangjieArray | _: VArray => 1
      }
    }

    def apply(x: SignatureType) = proto(x)

    def proto(x: SignatureType) = Prototype.intern(Proto(x))
  }

  class GenericTypeArg private(proto: GenericTypeArg.Proto)
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode with ControlledNode {
    def idx: Int = proto.idx
    def typeInfo = arg(1)
  }

  object GenericTypeArg {
    case class Proto private[GenericTypeArg](idx: Int)
      extends FixedArgs[GenericTypeArg](ControlType, AddrType)(AddrType) with PrototypeStrictNodeClass[GenericTypeArg, GenericTypeArg] {
      assert(idx >= 0)
      def newInstance() = new GenericTypeArg(this)
    }

    def apply(idx: Int)(typeInfo: Node) = proto(idx)(typeInfo)

    def proto(idx: Int) = Prototype.intern(Proto(idx))
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
      assert(!base.isRecord)

      def newInstance() = new Unbox(this)
    }

    def proto(base: SignatureType) = Prototype.intern(Proto(base))
    def apply(base: SignatureType)(baseTypeInfo: Node, arg: Node) = proto(base)(baseTypeInfo, arg)
  }

  class UnboxRec private(proto: UnboxRec.Proto) extends FloatingNodeWithFixedArgs(proto) with ControlledNode with HasInMemory with HasFrameSlot with Constant {
    val kind = FrameSlot.Local(proto.base)

    def base = proto.base

    def baseTypeInfo = arg(2)
    def value = arg(3)
  }

  object UnboxRec {
    case class Proto private[UnboxRec](base: SignatureType)
      extends FixedArgs[UnboxRec](ControlType, MemoryType, AddrType, ValueType.fromSig(SignatureType.Box(base)))(ValueType.fromSig(base, instantiateRich = true)) {
      assert(base.isRecord)

      def newInstance() = new UnboxRec(this)
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

  class SpawnClosure private(proto: SpawnClosure.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with NotProducesValue with CanThrow {
    def closureType = proto.closureType

    def closure = arg(2)
  }

  object SpawnClosure {
    case class Proto private[SpawnClosure](closureType: SignatureType)
      extends FixedArgs[SpawnClosure](ControlType, MemoryType, TRefType)(VoidType)
        with ControlMemoryTagged[SpawnClosure] {

      def newInstance() = new SpawnClosure(this)
    }

    def proto(closureType: SignatureType) = Prototype.intern(Proto(closureType))
    def apply(closureType: SignatureType)(closure: Node) = proto(closureType)(closure)
  }

  class EnumCast private(proto: EnumCast.Proto) extends FloatingNodeWithFixedArgs(proto) {
    def enumType = proto.enumType
    def base = arg(0)
  }

  object EnumCast {
    case class Proto private[EnumCast](enumType: SignatureType)
      extends FixedArgs[EnumCast](TRefType)(TRefType) {
      require(enumType.isTraceableReference || enumType.isInstanceOf[SignatureType.OptionLikeEnum])
      // TODO: assert enum

      def newInstance() = new EnumCast(this)
    }

    def proto(enumType: SignatureType) = Prototype.intern(Proto(enumType))
    def apply(enumType: SignatureType)(base: Node): Node = proto(enumType)(base)
  }

  class OptionTagGeneric private(proto: OptionTagGeneric.Proto) extends FloatingNodeWithFixedArgs(proto) with ControlledNode with HasInMemory {
    def optionType = proto.optionType

    def baseTypeInfo = arg(2)
    def value = arg(3)
  }

  object OptionTagGeneric {
    case class Proto private[OptionTagGeneric](optionType: SignatureType.OptionLikeEnum)
      extends FixedArgs[OptionTagGeneric](ControlType, MemoryType, AddrType, ValueType.fromSig(SignatureType.Box(optionType)))(IntType) {
      assert(optionType.someType.isTypeVariable, optionType)

      def newInstance() = new OptionTagGeneric(this)
    }

    def proto(optionType: SignatureType.OptionLikeEnum) = Prototype.intern(Proto(optionType))
    def apply(optionType: SignatureType.OptionLikeEnum)(baseTypeInfo: Node, arg: Node) = proto(optionType)(baseTypeInfo, arg)
  }

  class OptionPayloadGeneric private(proto: OptionPayloadGeneric.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue with CanThrow {
    def optionType = proto.optionType

    def baseTypeInfo = arg(2)
    def optionTypeInfo = arg(3)
    def value = arg(4)
  }

  object OptionPayloadGeneric {
    case class Proto private[OptionPayloadGeneric](optionType: SignatureType.OptionLikeEnum)
      extends FixedArgs[OptionPayloadGeneric](ControlType, MemoryType, AddrType, AddrType, ValueType.fromSig(SignatureType.Box(optionType)))(TRefType)
        with ControlMemoryValueTagged[OptionPayloadGeneric] {
      assert(optionType.someType.isTypeVariable, optionType)

      def newInstance() = new OptionPayloadGeneric(this)
    }

    def proto(optionType: SignatureType.OptionLikeEnum) = Prototype.intern(Proto(optionType))
    def apply(optionType: SignatureType.OptionLikeEnum)(baseTypeInfo: Node, optionTypeInfo: Node, arg: Node) =
      proto(optionType)(baseTypeInfo, optionTypeInfo, arg)
  }

  class NewNoneOptionGeneric private(proto: NewNoneOptionGeneric.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue with CanThrow {
    def optionType = proto.optionType

    def baseTypeInfo = arg(2)
    def optionTypeInfo = arg(3)
  }

  object NewNoneOptionGeneric {
    case class Proto private[NewNoneOptionGeneric](optionType: SignatureType.OptionLikeEnum)
      extends FixedArgs[NewNoneOptionGeneric](ControlType, MemoryType, AddrType, AddrType)(TRefType)
        with ControlMemoryValueTagged[NewNoneOptionGeneric] {
      assert(optionType.someType.isTypeVariable, optionType)

      def newInstance() = new NewNoneOptionGeneric(this)
    }

    def proto(optionType: SignatureType.OptionLikeEnum) = Prototype.intern(Proto(optionType))
    def apply(optionType: SignatureType.OptionLikeEnum)(baseTypeInfo: Node, optionTypeInfo: Node) =
      proto(optionType)(baseTypeInfo, optionTypeInfo)
  }

  class NewSomeOptionGeneric private(proto: NewSomeOptionGeneric.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue with CanThrow {
    def optionType = proto.optionType

    def baseTypeInfo = arg(2)
    def optionTypeInfo = arg(3)
    def value = arg(4)
  }

  object NewSomeOptionGeneric {
    case class Proto private[NewSomeOptionGeneric](optionType: SignatureType.OptionLikeEnum)
      extends FixedArgs[NewSomeOptionGeneric](ControlType, MemoryType, AddrType, AddrType, TRefType)(TRefType)
        with ControlMemoryValueTagged[NewSomeOptionGeneric] {
      assert(optionType.someType.isTypeVariable, optionType)

      def newInstance() = new NewSomeOptionGeneric(this)
    }

    def proto(optionType: SignatureType.OptionLikeEnum) = Prototype.intern(Proto(optionType))
    def apply(optionType: SignatureType.OptionLikeEnum)(baseTypeInfo: Node, optionTypeInfo: Node, arg: Node) =
      proto(optionType)(baseTypeInfo, optionTypeInfo, arg)
  }

  // TODO: actually use this node in call, instead of simply relying on their relative positioning
  class SaveCallRefTypeInfo private extends NodeWithFixedArgs(SaveCallRefTypeInfo) with SpinalMemoryNode with ProducesValue with Transfer {
    def transferArgIdx = 2
  }

  object SaveCallRefTypeInfo extends FixedArgs[SaveCallRefTypeInfo](ControlType, MemoryType, AddrType)(AddrType)
    with ControlMemoryValueTagged[SaveCallRefTypeInfo] {

    def newInstance() = new SaveCallRefTypeInfo
  }

  class AssignGeneric private(proto: AssignGeneric.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with NotProducesValue with CanThrow {
    def base = proto.base

    def baseTypeInfo = arg(2)
    def dst = arg(3)
    def src = arg(4)
  }

  object AssignGeneric {
    case class Proto private[AssignGeneric](base: SignatureType)
      extends FixedArgs[AssignGeneric](ControlType, MemoryType, AddrType, TRefType, TRefType)(VoidType)
        with ControlMemoryTagged[AssignGeneric] {
      assert(base.isVariableSizeType, base)

      def newInstance() = new AssignGeneric(this)
    }

    def proto(base: SignatureType) = Prototype.intern(Proto(base))
    def apply(base: SignatureType)(baseTypeInfo: Node, dst: Node, src: Node) =
      proto(base)(baseTypeInfo, dst, src)
  }

  class InstanceOfGeneric private(proto: InstanceOfGeneric.Proto) extends FloatingNodeWithFixedArgs(proto) {
    def targetType = proto.targetType

    def obj = arg(0)
    def targetTypeInfo = arg(1)
  }

  object InstanceOfGeneric {
    case class Proto private[InstanceOfGeneric](targetType: SignatureType)
      extends FixedArgs[InstanceOfGeneric](ValueType(targetType), AddrType)(IntType) {
      assert(targetType.containsTypeVariables, targetType)

      def newInstance() = new InstanceOfGeneric(this)
    }

    def proto(targetType: SignatureType) = Prototype.intern(Proto(targetType))
    def apply(targetType: SignatureType)(targetTypeInfo: Node, obj: Node) =
      proto(targetType)(targetTypeInfo, obj)
  }

  class Abs private(proto: Abs.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue {
    def value = arg(2)
  }

  object Abs {
    case class Proto private[Abs](keyType: Type)
      extends FixedArgs[Abs](ControlType, MemoryType, keyType)(keyType) with ControlMemoryValueTagged[Abs] {
      require(keyType.isIntegralType)

      def newInstance() = new Abs(this)
    }

    def proto(keyType: Type) = Prototype.intern(Proto(keyType))
    def apply(x: Node): Node = proto(x.tpe)(x)
  }

  class ArrayBuiltInCopyTo private(proto: ArrayBuiltInCopyTo.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with NotProducesValue {
    def arrayType = proto.arrayType

    def src = arg(2)
    def dst = arg(3)
    def srcStart = arg(4)
    def dstStart = arg(5)
    def len = arg(6)
  }

  object ArrayBuiltInCopyTo {
    case class Proto private[ArrayBuiltInCopyTo](arrayType: SignatureType)
      extends FixedArgs[ArrayBuiltInCopyTo](ControlType, MemoryType, TRefType, TRefType, LongType, LongType, LongType)(VoidType) with ControlMemoryTagged[ArrayBuiltInCopyTo] {
      require(arrayType.isCangjieArray)

      def newInstance() = new ArrayBuiltInCopyTo(this)
    }

    def proto(arrayType: SignatureType) = Prototype.intern(Proto(arrayType))
    def apply(arrayType: SignatureType)(src: Node, dst: Node, srcStart: Node, dstStart: Node, len: Node): Node =
      proto(arrayType)(src, dst, srcStart, dstStart, len)
  }

  object AtomicOps {

    sealed trait AtomicNode extends SpinalMemoryNode {
      def field: CangjieFieldReference
      final def fieldType: AsmType = field.fieldType.toAsm
      def obj: Node = arg(2)
    }

    class Load private(proto: Load.Proto) extends NodeWithFixedArgs(proto) with ProducesValue with AtomicNode {
      def accessType: AsmType = field.fieldType.toAsm
      def field: CangjieFieldReference = proto.field
    }

    object Load {
      case class Proto private[Load](obj: Type, field: CangjieFieldReference)
        extends FixedArgs[Load](ControlType, MemoryType, obj)(ValueType(field.fieldType)) with ControlMemoryValueTagged[Load] {

        def newInstance() = new Load(this)
      }

      def proto(obj: Type, field: CangjieFieldReference) = Prototype.intern(Proto(obj, field))
      def apply(objType: Type, field: CangjieFieldReference)(obj: Node) = proto(objType, field)(obj)
    }

    class Store private(proto: Store.Proto) extends NodeWithFixedArgs(proto) with NotProducesValue with AtomicNode {
      def field: CangjieFieldReference = proto.field
      def value: Node = arg(3)
    }

    object Store {
      case class Proto private[Store](obj: Type, field: CangjieFieldReference)
        extends FixedArgs[Store](ControlType, MemoryType, obj, ValueType(field.fieldType))(ControlType) with ControlMemoryTagged[Store] {

        def newInstance() = new Store(this)
      }

      def proto(obj: Type, field: CangjieFieldReference) = Prototype.intern(Proto(obj, field))
      def apply(objType: Type, field: CangjieFieldReference)(obj: Node, value: Node) = proto(objType, field)(obj, value)
    }

    class CAS private(proto: CAS.Proto) extends NodeWithFixedArgs(proto) with ProducesValue with AtomicNode {
      def field: CangjieFieldReference = proto.field
      def compareValue: Node = arg(3)
      def swapValue: Node = arg(4)
    }

    object CAS {
      case class Proto private[CAS](obj: Type, field: CangjieFieldReference)
        extends FixedArgs[CAS](ControlType, MemoryType, obj, ValueType(field.fieldType), ValueType(field.fieldType))(ValueType(SignatureType.Boolean))
          with ControlMemoryValueTagged[CAS] {

        def newInstance() = new CAS(this)
      }

      def proto(obj: Type, field: CangjieFieldReference) = Prototype.intern(Proto(obj, field))
      def apply(objType: Type, field: CangjieFieldReference)(obj: Node, compareVal: Node, swapVal: Node) =
        proto(objType, field)(obj, compareVal, swapVal)
    }

    class Simple private(proto: Simple.Proto) extends NodeWithFixedArgs(proto) with ProducesValue with AtomicNode {
      def field: CangjieFieldReference = proto.field
      def value: Node = arg(3)
      def kind: Simple.Kind = proto.kind
    }

    object Simple {
      enum Kind {
        case SWAP, FETCH_ADD, FETCH_SUB, FETCH_AND, FETCH_OR, FETCH_XOR
      }

      case class Proto private[Simple](kind: Kind, obj: Type, field: CangjieFieldReference)
        extends FixedArgs[Simple](ControlType, MemoryType, obj, ValueType(field.fieldType))(ValueType(field.fieldType))
          with ControlMemoryValueTagged[Simple] {
        def newInstance() = new Simple(this)
      }

      def proto(kind: Kind, obj: Type, field: CangjieFieldReference): Proto = Proto(kind, obj, field)

      def swap    (objType: Type, field: CangjieFieldReference)(obj: Node, value: Node) = proto(Kind.SWAP,      objType, field)(obj, value)
      def fetchAdd(objType: Type, field: CangjieFieldReference)(obj: Node, value: Node) = proto(Kind.FETCH_ADD, objType, field)(obj, value)
      def fetchSub(objType: Type, field: CangjieFieldReference)(obj: Node, value: Node) = proto(Kind.FETCH_SUB, objType, field)(obj, value)
      def fetchAnd(objType: Type, field: CangjieFieldReference)(obj: Node, value: Node) = proto(Kind.FETCH_AND, objType, field)(obj, value)
      def fetchOr (objType: Type, field: CangjieFieldReference)(obj: Node, value: Node) = proto(Kind.FETCH_OR,  objType, field)(obj, value)
      def fetchXor(objType: Type, field: CangjieFieldReference)(obj: Node, value: Node) = proto(Kind.FETCH_XOR, objType, field)(obj, value)
    }
  }

  class NewGeneric private(proto: NewGeneric.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue with CanThrow {
    def allocType = proto.allocType

    def allocTypeInfo = arg(2)
  }

  object NewGeneric {
    case class Proto private[NewGeneric](allocType: SignatureType)
      extends FixedArgs[NewGeneric](ControlType, MemoryType, AddrType)(ValueType.fromSig(allocType))
      with ControlMemoryValueTagged[NewGeneric] {
      assert(allocType.containsTypeVariables || allocType.isCangjieLambda)

      def newInstance() = new NewGeneric(this)
    }

    def proto(allocType: SignatureType) = Prototype.intern(Proto(allocType))
    def apply(allocType: SignatureType)(allocTypeInfo: Node) = proto(allocType)(allocTypeInfo)
  }
}
