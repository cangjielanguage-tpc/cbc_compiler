/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.jet.compiler.Env.isStandalone
import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Universe}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{InstantiatedType, TypeVariable}
import com.huawei.excelsior.jet.compiler.symlevel.{ConstraintCallMethodReference, Field, InstantiatedMethodReference, SignatureType}

import scala.PartialFunction.condOpt

trait UniversalGenericNodes { self: Universe with Nodes =>

  sealed trait UniversalGeneric extends Node

  object UniversalGeneric {

    /////////////////////////////////////////

    //<editor-fold desc="Holder support">

    trait ConvertHolder extends UniversalGeneric {
      def from: SignatureType
      def to: SignatureType
      def concreteType: SignatureType
    }

    /** Conversion from concrete type to vst. */
    class ToHolder private(proto: ToHolder.Proto) extends FloatingNodeWithFixedArgs(proto) with ConvertHolder {
      def from = proto.from
      def to = proto.to
      def concreteType = from
    }

    object ToHolder {
      case class Proto private[ToHolder](from: SignatureType, to: SignatureType)
        extends FixedArgs[ToHolder](ValueType.fromSig(from, instantiateRich = true))(HolderType(from)) {

        require(!from.isInstanceOf[TypeVariable])
        require(to.isInstanceOf[TypeVariable])
        def newInstance() = new ToHolder(this)
      }

      def proto(from: SignatureType, to: SignatureType) = Prototype.intern(Proto(from, to))
    }

    /** Conversion from vst to concrete type. */
    class FromHolder private(proto: FromHolder.Proto) extends FloatingNodeWithFixedArgs(proto) with ConvertHolder {
      def from = proto.from
      def to = proto.to
      def concreteType = to
    }

    object FromHolder {
      case class Proto private[FromHolder](from: SignatureType, to: SignatureType)
        extends FixedArgs[FromHolder](HolderType(to))(ValueType.fromSig(to, instantiateRich = true)) {

        require(from.isInstanceOf[TypeVariable])
        require(!to.isInstanceOf[TypeVariable])
        def newInstance() = new FromHolder(this)
      }

      def proto(from: SignatureType, to: SignatureType) = Prototype.intern(Proto(from, to))
    }

    def convertHolder(from: SignatureType, to: SignatureType)(n: Node) = {
      assert(!isStandalone)
      (from.isInstanceOf[TypeVariable], to.isInstanceOf[TypeVariable]) match {
        case (false, true) => ToHolder.proto(from, to)(n)
        case (true, false) => FromHolder.proto(from, to)(n)
        case _ => n // no conversion
      }
    }

    class HolderConst extends LeafNode[HolderConst](HolderType(SignatureType.Int64)) with FloatingNode with UniversalGeneric

    object HolderConst {
      private lazy val instance = new HolderConst
      def apply() = instance()
    }

    class CopyUniversalVariable private(proto: CopyUniversalVariable.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CompositeNode with ProducesValue with UniversalGeneric {
      def dst = arg(2)
      def src = arg(3)

      def variableType = proto.universalVariableType
    }

    object CopyUniversalVariable {
      case class Proto private[CopyUniversalVariable](universalVariableType: SignatureType)
        extends FixedArgs[CopyUniversalVariable](ControlType, MemoryType, ValueType(universalVariableType), ValueType(universalVariableType))(ValueType(universalVariableType))
          with ControlMemoryValueTagged[CopyUniversalVariable] {

        require(universalVariableType.isInstanceOf[TypeVariable])

        override def newInstance() = new CopyUniversalVariable(this)
      }

      def proto(x: SignatureType) = Prototype.intern(Proto(x))
      def apply(x: SignatureType)(dst: Node, src: Node) = proto(x)(dst, src)
    }

    //</editor-fold>

    /////////////////////////////////////////

    //<editor-fold desc="Field support">

    trait FieldOperation extends Node {
      def field: Field
      def instantiatedRefType: InstantiatedType
      def instantiatedFieldType: SignatureType
      def obj: Node
    }

    /** Address of field in record.
      *
      * Inspired by `getelementptr` instruction in LLVM IR.
      * TODO: expand functionality of this node to be as powerful as `getelementptr`
      * and use it instead of GetField/GetStatic to access nested record fields
      */
    class GetElementPtr private(proto: GetElementPtr.Proto)
      extends FloatingNodeWithFixedArgs(proto) with FieldOperation with CompositeNode {

      override def field: Field = proto.field
      override def instantiatedRefType: InstantiatedType = proto.instantiatedRefType
      override def instantiatedFieldType: SignatureType = proto.instantiatedFieldType

      def obj = arg(0)
    }

    object GetElementPtr {
      case class Proto private[GetElementPtr](field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType)
        extends FixedArgs[GetElementPtr](ValueType.fromSig(instantiatedRefType))(AddrType) {

        def newInstance() = new GetElementPtr(this)
      }

      def proto(field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType) = {
        assert(!instantiatedFieldType.isVariableSizeType)
        Prototype.intern(Proto(field, instantiatedRefType, instantiatedFieldType))
      }

      def apply(field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType)(obj: Node): Node =
        proto(field, instantiatedRefType, instantiatedFieldType)(obj)

      def unapply(x: GetElementPtr) = Some(x.field, x.instantiatedRefType, x.instantiatedFieldType, x.obj)
    }

    class GetField private(proto: GetField.Proto) extends FloatingNodeWithFixedArgs(proto)
      with FieldOperation with ControlledNode with HasInMemory with ProducesValue with UniversalGeneric {

      override def field: Field = proto.field
      override def instantiatedRefType: InstantiatedType = proto.instantiatedRefType
      override def instantiatedFieldType: SignatureType = proto.instantiatedFieldType

      def obj = arg(2)
    }

    object GetField {
      case class Proto private[GetField](field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType)
        extends FixedArgs[GetField](ControlType, MemoryType, ValueType(instantiatedRefType))(ValueType(instantiatedFieldType)) {

        def newInstance() = new GetField(this)
      }

      def proto(field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType) = {
        assert(!instantiatedFieldType.isVariableSizeType)
        Prototype.intern(Proto(field, instantiatedRefType, instantiatedFieldType))
      }

      def apply(field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType)(obj: Node): Node =
        proto(field, instantiatedRefType, instantiatedFieldType)(obj)
    }

    class GetFieldOHM private(proto: GetFieldOHM.Proto) extends FloatingNodeWithFixedArgs(proto)
      with FieldOperation with ControlledNode with HasInMemory with ProducesValue with UniversalGeneric {

      override def field: Field = proto.field

      override def instantiatedRefType: InstantiatedType = proto.instantiatedRefType
      override def instantiatedFieldType: SignatureType = proto.instantiatedFieldType

      def obj = arg(2)
      def ohms = arg(3)
    }

    object GetFieldOHM {
      case class Proto private[GetFieldOHM](field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType)
        extends FixedArgs[GetFieldOHM](ControlType, MemoryType, InstanceFieldOperation.declaringClassType(field), ValueType(instantiatedFieldType))
          (ValueType(instantiatedFieldType)) {

        def newInstance() = new GetFieldOHM(this)
      }

      def proto(field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType) = {
        assert(instantiatedFieldType.isVariableSizeType)
        Prototype.intern(Proto(field, instantiatedRefType, instantiatedFieldType))
      }

      def apply(field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType)(obj: Node, ohm: StackAlloc): Node =
        proto(field, instantiatedRefType, instantiatedFieldType)(obj, ohm)
    }

    class PutField private(proto: PutField.Proto) extends NodeWithFixedArgs(proto)
      with FieldOperation with SpinalMemoryNode with NotProducesValue with UniversalGeneric {

      override def field: Field = proto.field
      override def instantiatedRefType: InstantiatedType = proto.instantiatedRefType
      override def instantiatedFieldType: SignatureType = proto.instantiatedFieldType

      def obj = arg(2)
      def value = arg(3)
    }

    object PutField {
      case class Proto private[PutField](field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType)
        extends FixedArgs[PutField](ControlType, MemoryType, ValueType(instantiatedRefType), ValueType(instantiatedFieldType))(ControlType)
        with ControlMemoryTagged[PutField] {

        assert(!field.isAJFlat)
        assert(!field.getType.isZST)

        def newInstance() = new PutField(this)
      }

      def proto(field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType) =
        Prototype.intern(Proto(field, instantiatedRefType, instantiatedFieldType))

      def apply(field: Field, instantiatedRefType: InstantiatedType, instantiatedFieldType: SignatureType)(obj: Node, value: Node) =
        proto(field, instantiatedRefType, instantiatedFieldType)(obj, value)
    }

    //</editor-fold>

    /////////////////////////////////////////

    //<editor-fold desc="Call method support">

    object InvokeConstraintMethod {

      class Target private(proto: Target.Proto) extends CallTarget(proto) {
        override def name = s"UniversalGenericConstraintMethodCallTarget[${targetRef.name}]"

        def targetRef = proto.targetRef
      }

      object Target {
        case class Proto private[Target](targetRef: ConstraintCallMethodReference)
          extends CallTarget.Proto[Target]() {
          def newInstance() = new Target(this)
        }

        def proto(targetRef: ConstraintCallMethodReference) = Prototype.intern(Proto(targetRef))

        def unapply(target: Target) = Some(target.targetRef)
      }

      def apply(targetRef: ConstraintCallMethodReference)(args: Node*) = {
        val callTarget = Target.proto(targetRef)()
        Call(targetRef)(callTarget +: args: _*)
      }
      
      def unapply(call: Call): Option[ConstraintCallMethodReference] = condOpt(call.target) {
        case Target(targetRef) => targetRef
      }
    }

    object InvokeMethodWithGenericContext {

      class Target private(proto: Target.Proto) extends CallTarget(proto) {
        override def name = s"UniversalGenericMethodCallTarget[${targetRef.methodName}]"

        def targetRef = proto.targetRef
      }

      object Target {
        case class Proto private[Target](targetRef: InstantiatedMethodReference)
          extends CallTarget.Proto[Target]() {

          def newInstance() = new Target(this)
        }

        def proto(targetRef: InstantiatedMethodReference) =
          Prototype.intern(Proto(targetRef))

        def unapply(target: Target) = Some(target.targetRef)
      }

      def apply(targetRef: InstantiatedMethodReference)(args: Node*) = {
        val callTarget = Target.proto(targetRef)()
        Call(targetRef)(callTarget +: args: _*)
      }

      def unapply(call: Call): Option[InstantiatedMethodReference] = condOpt(call.target) {
        case Target(targetRef) => targetRef
      }
    }

    //</editor-fold>

    /////////////////////////////////////////

    //<editor-fold desc="OHM operations">

    /** Methods in universal-generic context which return a Variable-Sized Type (VST)
      * receive in additional last parameter an address of a pointer to memory allocated by caller to hold the result of such method invocation
      * in case it is instantiated to a record type. The value of that additional parameter is [[resultPointerAddress]].
      *
      * Note that the memory allocation for the result happens in run-time for a particular instantiation of [[sig]] type,
      * and so in case it is not of a record type the [[resultPointerAddress]] can be invalid and will not be dereferenced in run-time.
      *
      * This node represents a copy of the [[value]] which is returned from current function to its caller.
      * In case the run-time instantiation of [[sig]] is non-record the result is a simple [[Transfer]] of [[value]].
      * Otherwise, [[value]] points to a record and its contents is copied to destination pointed by dereferenced [[resultPointerAddress]].
      */
    class CopyResultVST private(proto: CopyResultVST.Proto) extends NodeWithFixedArgs(proto)
      with SpinalMemoryNode with ProducesValue with UniversalGeneric {
      def sig = proto.sig
      def value = arg(2)
      def resultPointerAddress = arg(3)
    }

    object CopyResultVST {
      case class Proto private[CopyResultVST](sig: SignatureType)
        extends FixedArgs[CopyResultVST](ControlType, MemoryType, ValueType(sig), ValueType(sig))(ValueType(sig))
          with ControlMemoryValueTagged[CopyResultVST] {

        def newInstance() = new CopyResultVST(this)
      }

      def proto(sig: SignatureType) = Prototype.intern(Proto(sig))
      def apply(sig: SignatureType)(value: Node, resultPointerAddress: Node) = proto(sig)(value, resultPointerAddress)
    }

    class OffHeapMemorySlotPointer private(proto: OffHeapMemorySlotPointer.Proto) extends FloatingNodeWithFixedArgs(proto) with UniversalGeneric {
      def ohms = arg(0)
    }

    object OffHeapMemorySlotPointer {
      case class Proto private[OffHeapMemorySlotPointer](sig: SignatureType)
        extends FixedArgs[OffHeapMemorySlotPointer](ValueType(sig))(ValueType(sig)) {

        def newInstance() = new OffHeapMemorySlotPointer(this)
      }

      def proto(sig: SignatureType) = Prototype.intern(Proto(sig))
      def apply(n: StackAlloc) = {
        val StackAlloc.OffHeapMemory(sig) = n
        proto(sig)(n)
      }
    }

    //</editor-fold>

    /////////////////////////////////////////

    //<editor-fold desc="Extra">

    class TypeVarIsRef private(proto: TypeVarIsRef.Proto) extends FloatingNodeWithFixedArgs(proto) with UniversalGeneric {
      def typeVar = proto.typeVar
    }

    object TypeVarIsRef {
      case class Proto private[TypeVarIsRef](typeVar: SignatureType.TypeVariable) extends FixedArgs[TypeVarIsRef]()(IntType) {

        def newInstance() = new TypeVarIsRef(this)
      }

      def proto(typeVar: SignatureType.TypeVariable) = Prototype.intern(Proto(typeVar))
      def apply(typeVar: SignatureType.TypeVariable) = proto(typeVar)()
    }
    //</editor-fold>
  }

}
