/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{Field, SignatureType}

import scala.util.chaining.scalaUtilChainingOps

trait HLIRNodes { self: Universe =>

  /** Universal wrapper for bitcode parsing. */
  class BCNode private (val value: Bitcode.MDItem) extends LeafNode[BCNode](ValueType) with FloatingNode {
    override def commitImpl() = shouldNotCallThis("BCNode should not be committed to IR")

    override def name = s"$simpleName[$value]"
  }

  object BCNode {
    def apply(value: Bitcode.MDItem) = new BCNode(value) tap (_.initSelfReference())
    def unapply(n: BCNode) = Some(n.value)
  }

  trait BCGlobalOrFunction extends SpinalMemoryNode with CanThrow with ProducesValue

  object BCGlobalOrFunction {
    abstract class Proto[N <: BCGlobalOrFunction] extends FixedArgs[N](ControlType, MemoryType)(AddrType)
      with ControlMemoryValueTagged[N] with PrototypeStrictNodeClass[N, N]
  }

  class BCGlobal private(proto: BCGlobal.Proto) extends NodeWithFixedArgs(proto) with BCGlobalOrFunction {
    def target = proto.target
  }

  object BCGlobal {
    case class Proto private[BCGlobal](target: Bitcode.Global) extends BCGlobalOrFunction.Proto[BCGlobal] {

      def newInstance() = new BCGlobal(this)
    }

    def apply(x: Bitcode.Global) = Prototype.intern(Proto(x))()
    def unapply(x: BCGlobal) = Some(x.target)
  }

  class BCFunction private(proto: BCFunction.Proto) extends NodeWithFixedArgs(proto) with BCGlobalOrFunction {
    def target = proto.target
  }

  object BCFunction {
    case class Proto private[BCFunction](target: Bitcode.Function) extends BCGlobalOrFunction.Proto[BCFunction] {

      def newInstance() = new BCFunction(this)
    }

    def apply(x: Bitcode.Function) = Prototype.intern(Proto(x))()
    def unapply(x: BCFunction) = Some(x.target)
  }

  class DeferredGetElementPtr private(proto: DeferredGetElementPtr.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue {
    def ref = proto.ref
    def recordType = proto.recordType
    def base = arg(2)
  }

  object DeferredGetElementPtr {
    case class Proto private[DeferredGetElementPtr](ref: HLIRMetadata.Ref.InstanceField, recordType: SignatureType)
      extends FixedArgs[DeferredGetElementPtr](ControlType, MemoryType, RecordAddrType(recordType))(AddrType)
        with ControlMemoryValueTagged[DeferredGetElementPtr] with PrototypeStrictNodeClass[DeferredGetElementPtr, DeferredGetElementPtr] {

      def newInstance() = new DeferredGetElementPtr(this)
    }

    def proto(ref: HLIRMetadata.Ref.InstanceField, recordType: SignatureType) = Prototype.intern(Proto(ref, recordType))

    def apply(ref: HLIRMetadata.Ref.InstanceField, recordType: SignatureType)(base: Node) = proto(ref, recordType)(base)
    def unapply(x: DeferredGetElementPtr) = Some(x.ref, x.recordType, x.base)
  }

  class GenericGetElementPtr private(proto: GenericGetElementPtr.Proto)
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode {

    def field = proto.field
    def instantiatedRefType: SignatureType.InstantiatedRecord = proto.instantiatedRefType
    def instantiatedFieldType: SignatureType = proto.instantiatedFieldType

    def base = arg(0)
  }

  object GenericGetElementPtr {
    case class Proto private[GenericGetElementPtr](field: Field, instantiatedRefType: SignatureType.InstantiatedRecord, instantiatedFieldType: SignatureType)
      extends FixedArgs[GenericGetElementPtr](RecordAddrType(instantiatedRefType))(AddrType) {

      def newInstance() = new GenericGetElementPtr(this)
    }

    def proto(instantiatedFieldType: Field, instantiatedRefType: SignatureType.InstantiatedRecord, fieldType: SignatureType) =
      Prototype.intern(Proto(instantiatedFieldType, instantiatedRefType, fieldType))

    def apply(field: Field, instantiatedRefType: SignatureType.InstantiatedRecord, instantiatedFieldType: SignatureType)(base: Node) =
      proto(field, instantiatedRefType, instantiatedFieldType)(base)

    def unapply(x: GenericGetElementPtr) = Some(x.field, x.instantiatedRefType, x.instantiatedFieldType, x.base)
  }
}
