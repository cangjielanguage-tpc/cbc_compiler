/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Universe}
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeMethodReference, SignatureType}

import scala.PartialFunction.condOpt

trait BitcodeDeferredNodes { self: Universe with Nodes =>

  // TODO: support bitcode deferred interfaces (in node protos)
  trait BitcodeDeferred extends Node

  object BitcodeDeferred {

    ///////////////////// Invoke ///////////////////////

    class InvokeTarget private(proto: InvokeTarget.Proto) extends CallTarget(proto) {
      override def name = s"BitcodeDeferredCallTarget[${targetRef.methodName}]"

      def targetRef = proto.targetRef
    }

    object InvokeTarget {
      case class Proto private[InvokeTarget](targetRef: BitcodeMethodReference) extends CallTarget.Proto[InvokeTarget]() {

        def newInstance() = new InvokeTarget(this)
      }

      def apply(targetRef: BitcodeMethodReference) = Prototype.intern(Proto(targetRef))
      def unapply(target: InvokeTarget) = Some(target.targetRef)
    }

    object Invoke {
      def apply(targetRef: BitcodeMethodReference)(args: Node*) = {
        val callTarget = InvokeTarget(targetRef)()
        Call(targetRef)(callTarget +: args: _*)
      }

      def unapply(call: Call): Option[BitcodeMethodReference] = condOpt(call.target) {
        case InvokeTarget(targetRef) => targetRef
      }
    }

    ///////////////////// New ///////////////////////

    /** Creates new bitcode-deferred class instance in heap. */
    class New private(proto: New.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CanThrow with BitcodeDeferred with ProducesValue {
      def allocType = proto.allocType
    }

    object New {
      case class Proto private[New](allocType: SignatureType) extends FixedArgs[New](ControlType, MemoryType)(ValueType(allocType))
        with ControlMemoryValueTagged[New] {

        def newInstance() = new New(this)
      }

      def apply(allocType: SignatureType) = Prototype.intern(Proto(allocType))
    }

    /** Creates new array instance in heap. */
    class NewArray private(proto: NewArray.Proto) extends NodeWithVarArgs(proto) with SpinalMemoryNode with CanThrow with BitcodeDeferred with ProducesValue {
      final def lengths: Seq[Node] = argsTail(proto.fixedArgsCount)

      def allocType = proto.allocType
    }

    object NewArray {
      case class Proto private[NewArray](allocType: SignatureType) extends VarArgs[NewArray](ControlType, MemoryType)(TypedArrayOperation.lenType(allocType))(TRefType)
        with ControlMemoryValueTagged[NewArray] {

        def newInstance(): NewArray = new NewArray(this)
      }

      def apply(allocType: SignatureType) = Prototype.intern(Proto(allocType))
    }

    ///////////////////// Instance of ///////////////////////

    class InstanceOf private(proto: InstanceOf.Proto) extends NodeWithFixedArgs(proto) with FloatingNode with BitcodeDeferred {
      def targetType: SignatureType = proto.targetType
      def obj = arg(0)
    }

    object InstanceOf {
      case class Proto private[InstanceOf](targetType: SignatureType) extends FixedArgs[InstanceOf](ValueType(targetType))(IntType) {

        def newInstance() = new InstanceOf(this)
      }

      def apply(targetType: SignatureType) = Prototype.intern(Proto(targetType))
      def unapply(x: InstanceOf) = Some(x.targetType, x.obj)
    }

    class CheckCast private(proto: CheckCast.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with CanThrow with HasInMemory with BitcodeDeferred with ProducesValue {
      def targetType: SignatureType = proto.targetType
      def obj = arg(2)
    }

    object CheckCast {
      case class Proto private[CheckCast](targetType: SignatureType) extends FixedArgs[CheckCast](ControlType, MemoryType, ValueType(targetType))(IntType)
        with ControlValueTagged[CheckCast] {

        def newInstance() = new CheckCast(this)
      }

      def apply(targetType: SignatureType) = Prototype.intern(Proto(targetType))
    }

    ///////////////////// Field ///////////////////////

    class GetField private(proto: GetField.Proto) extends FloatingNodeWithFixedArgs(proto) with FieldOp with ControlledNode with HasInMemory with ProducesValue {
      def fieldRef = proto.field
    }

    class PutField private(proto: PutField.Proto) extends NodeWithFixedArgs(proto) with FieldOp with SpinalMemoryNode with NotProducesValue {
      def fieldRef = proto.field
    }

    /** Deferred field operations don't have fixed resolve point, and thus don't have fixed throw point in IR.
      * Resolve should be performed in range from method enter to field op CBC bytecode.
      */
    trait FieldOp extends BitcodeDeferred with MayHaveImplicitCheck {
      def hasObj = !fieldRef.isStatic
      def hasInValue = fieldRef.isWrite

      def objArgIdx = if (hasObj) 2 else shouldNotReachHere()
      def obj = arg(objArgIdx)

      def inValueArgIdx = if (hasInValue && hasObj) 3 else if (hasInValue) 2 else shouldNotReachHere()
      def inValue = arg(inValueArgIdx)

      def fieldRef: symlevel.BitcodeFieldReference
    }

    object GetField {
      case class Proto private[GetField](field: symlevel.BitcodeFieldReference)
        extends FixedArgs[GetField](Seq(ControlType, MemoryType) ++ args(field): _*)(retType(field)) {
        def newInstance() = new GetField(this)
      }

      private def args(fieldRef: symlevel.BitcodeFieldReference) =
        if (fieldRef.isStatic) Seq.empty else Seq(ValueType(fieldRef.refType))

      private def retType(fieldRef: symlevel.BitcodeFieldReference) =
        ValueType.fromSig(fieldRef.fieldType, instantiateRich = true)

      def proto(fieldRef: symlevel.BitcodeFieldReference) = Prototype.intern(Proto(fieldRef))
      def static(fieldRef: symlevel.BitcodeFieldReference)() = proto(fieldRef)()
      def instance(fieldRef: symlevel.BitcodeFieldReference)(obj: Node) = proto(fieldRef)(obj)
    }

    object PutField {
      case class Proto private[PutField](field: symlevel.BitcodeFieldReference)
        extends FixedArgs[PutField](Seq(ControlType, MemoryType) ++ args(field): _*)(VoidType)
          with ControlMemoryTagged[PutField] {
        def newInstance() = new PutField(this)
      }

      private def args(fieldRef: symlevel.BitcodeFieldReference) = if (fieldRef.isStatic) {
        Seq(ValueType.fromSig(fieldRef.fieldType))
      } else {
        Seq(ValueType(fieldRef.refType), ValueType.fromSig(fieldRef.fieldType))
      }

      def proto(fieldRef: symlevel.BitcodeFieldReference) = Prototype.intern(Proto(fieldRef))
      def static(fieldRef: symlevel.BitcodeFieldReference)(value: Node) = proto(fieldRef)(value)
      def instance(fieldRef: symlevel.BitcodeFieldReference)(obj: Node, value: Node) = proto(fieldRef)(obj, value)
    }
  }
}
