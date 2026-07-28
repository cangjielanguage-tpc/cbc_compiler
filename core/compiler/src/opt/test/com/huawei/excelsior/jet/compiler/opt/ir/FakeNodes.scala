/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

trait FakeNodes extends Identities with EnrichmentSupport { self: Universe =>

  override def producesRich(n: Node) = n match {
    case _: FakeNode => EnrichmentDecision.No
    case _ => super.producesRich(n)
  }

  /** Fake nodes. */
  sealed trait FakeNode extends Node with StructurallyUnique

  class Fake private (tpe: Type) extends LeafNode[Fake](tpe) with FakeNode with FloatingNode

  object Fake {
    def apply(tpe: Type) = new Fake(tpe)()
  }

  class FakePinned(keyType: Type) extends LeafNode[FakePinned](keyType: Type) with BlockParamNode with FakeNode {
    private var _block: Block = _
    override def block: Block = _block
  }

  object FakePinned {

    private def instance(tpe: Type, block: Block) = {
      val inst = new FakePinned(tpe)
      inst._block = block
      inst
    }

    def apply(tpe: Type)(block: Block): FakePinned = instance(tpe, block)()
  }

  class FakeControlled private (proto: FakeControlled.Proto) extends FloatingNodeWithFixedArgs(proto) with ControlledNode with FakeNode with ProducesValue
  object FakeControlled {
    case class Proto private[FakeControlled] (keyType: Type) extends FixedArgs[FakeControlled](ControlType)(keyType) {
      def newInstance() = new FakeControlled(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  class FakeSpinal private (proto: FakeSpinal.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with FakeNode with ProducesValue
  object FakeSpinal {
    case class Proto private[FakeSpinal] (keyType: Type) extends FixedArgs[FakeSpinal](ControlType)(keyType) with SpinalNodePrototype[FakeSpinal] with ControlValueTagged[FakeSpinal] {
      def newInstance() = new FakeSpinal(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  class FakeSpinalX private (proto: FakeSpinalX.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CanThrow with FakeNode with ProducesValue
  object FakeSpinalX {
    case class Proto private[FakeSpinalX] (keyType: Type) extends FixedArgs[FakeSpinalX](ControlType, MemoryType)(keyType) with SpinalNodePrototype[FakeSpinalX] with ControlMemoryValueTagged[FakeSpinalX] {
      def newInstance() = new FakeSpinalX(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  class FakeSpinalXNoMemory private (proto: FakeSpinalXNoMemory.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with CanThrow with FakeNode with ProducesValue
  object FakeSpinalXNoMemory {
    case class Proto private[FakeSpinalXNoMemory] (keyType: Type) extends FixedArgs[FakeSpinalXNoMemory](ControlType, MemoryType)(keyType) with SpinalNodePrototype[FakeSpinalXNoMemory] with ControlValueTagged[FakeSpinalXNoMemory] {
      def newInstance() = new FakeSpinalXNoMemory(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  class FakeControlledUnary private (proto: FakeControlledUnary.Proto) extends FloatingNodeWithFixedArgs(proto) with ControlledNode with FakeNode {
    def inValue = arg(1)
  }
  object FakeControlledUnary {
    case class Proto private[FakeControlledUnary] (keyType: Type) extends FixedArgs[FakeControlledUnary](ControlType, keyType)(keyType) {
      def newInstance() = new FakeControlledUnary(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  class FakeSpinalUnary private (proto: FakeSpinalUnary.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with FakeNode with ProducesValue {
    def inValue = arg(1)
    def inValue_=(x: Node) = updateArg(1, x)
  }
  object FakeSpinalUnary {
    case class Proto private[FakeSpinalUnary] (keyType: Type) extends FixedArgs[FakeSpinalUnary](ControlType, keyType)(keyType) with SpinalNodePrototype[FakeSpinalUnary] with ControlValueTagged[FakeSpinalUnary] {
      def newInstance() = new FakeSpinalUnary(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  class FakeSpinalBinary private (proto: FakeSpinalBinary.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with FakeNode with ProducesValue
  object FakeSpinalBinary {
    case class Proto private[FakeSpinalBinary] (keyType: Type) extends FixedArgs[FakeSpinalBinary](ControlType, keyType, keyType)(keyType) with SpinalNodePrototype[FakeSpinalBinary] with ControlValueTagged[FakeSpinalBinary] {
      def newInstance() = new FakeSpinalBinary(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  type FakeUse = FakeSpinalUnary

  class FakeUnary private (proto: FakeUnary.Proto) extends FloatingNodeWithFixedArgs(proto) with FakeNode
  object FakeUnary {
    case class Proto private[FakeUnary] (keyType: Type) extends FixedArgs[FakeUnary](keyType)(keyType) {
      def newInstance() = new FakeUnary(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  class FakeBinary private (proto: FakeBinary.Proto) extends BinaryOp(proto) with FakeNode with FloatingNode
  object FakeBinary {
    case class Proto private[FakeBinary] (keyType: Type) extends BinaryOp.Floating[FakeBinary](keyType)(keyType) {
      def newInstance() = new FakeBinary(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
  }

  override def isApplicableToConstFold(node: Node): Boolean = node match {
    case _: FakeNode => false
    case _ => super.isApplicableToConstFold(node)
  }
}
