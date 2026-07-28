/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine

import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.Node._
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.NodeType.LONG_DOUBLE_2

object Node {
  private val END_POS = new SingletonPosition("end")
  private val TEMPORARY_POS = new SingletonPosition("temporary")
  private val LONG_HALF_POS = new SingletonPosition("longhalf")

  val LONG_HALF = new Node(LONG_DOUBLE_2, LONG_HALF_POS)

  trait Position

  class BCPosition private[Node](val bcOffset: Int) extends Position {
    override def toString = s"bc[$bcOffset]"
  }

  class InputSlotPosition private[Node](val slotIdx: Int) extends Position {
    override def toString = s"slot[$slotIdx]"
  }

  private class SingletonPosition private[Node](val name: String) extends Position {
    override def toString = name
  }

  def forInputSlot(`type`: NodeType, slotIdx: Int) = new Node(`type`, new InputSlotPosition(slotIdx))

  def forBCOffset(`type`: NodeType, bcOffset: Int) = new Node(`type`, new BCPosition(bcOffset))

  //////////////////////////////////////////////////////////////////////////////////////////////
  // "INVOKE() RELEASED MY TEMPORARY NODE!!!111" by cypok
  //
  // Implementation of node liveness management is a trade-off between ease of implementation, ease of use and
  // generated code quality.
  //
  // All bytecode nodes have position of last use.
  // Generation of every bytecode instruction should call releaseLocIfNotUsedLater() for all its arguments.
  // A node is actually released only if this bytecode instruction is the last use of the node.
  //
  // Temporary nodes originally were used as parameters of RTS calls.
  // So they are always treated like not-used-later.
  //
  // Also temporary nodes are convenient for generation of some non-bytecode stuff
  // (i.e. prologue, exception handling, wrappers, ...).
  // In such a case their "only single use" semantics sometimes is not very suitable.
  // The simplest workaround is to create a copy of the node if you want to pass it to some "releasing" sub-generator
  // (e.g. copying in Generator.genGetIMT() called from GeneratorX86.preprocessInterfaceParams()).
  // Note that the sub-generator may want to pass the node to some other sub-sub-generators by creating copies
  // for all but the last (e.g. this happens while passing interface parameter at invoke generation).
  //
  // Sounds good to have persistent nodes which may be released only manually by releaseLoc().
  // However this leads to unnecessary prolongation of node liveness:
  // the node is released only in outer generator but actual release happens somewhere in inner generator
  // (i.e. release of invoke parameters happens between parameters passing and before actual calling,
  // it is important not to save released parameters around the call).
  //
  // So the only good solution for non-bytecode nodes is to add acquire/release semantics
  // for nodes implemented by counter:
  // each generator should acquire the nodes if they are passed to sub-generators
  // and release the nodes when this generator actually stops using them.
  // In such a case actual location release takes place on the true last usage.
  // Example:
  //
  //   genFoo(a, b: Node) {
  //     acquire(a, b)
  //     genBar(b)
  //     genBaz(a, lastUseOf(b))
  //     genQux(lastUseOf(a))
  //   }
  //
  //   genBaz(x, y: Node) {
  //     asm.magic(getLoc(x), getLoc(y))
  //     releaseIfNotUsedLater(x, y)
  //   }
  //
  // However such solution requires to refactor all existing code which is not affordable at the moment.
  // Also it might increase complexity of generators code.
  //
  //////////////////////////////////////////////////////////////////////////////////////////////

  def newTemporary(`type`: NodeType) = new Node(`type`, TEMPORARY_POS)
}

final class Node private(val `type`: NodeType, val definition: Position) {
  assert(`type` != null)
  val asmType = if (`type` != LONG_DOUBLE_2) `type`.toAsm else null
  var lastUse: Position = _

  override def toString = {
    if ((definition == TEMPORARY_POS) || (definition == LONG_HALF_POS)) {
      s"node<${`type`}>[$definition]"
    } else {
      s"node<${`type`}>[$definition...$lastUse]"
    }
  }

  def usedAtBCOffset(bcOffset: Int): Unit = lastUse = new BCPosition(bcOffset)

  def usedAtTheEnd(): Unit = lastUse = END_POS

  def isUsedAtTheEnd = lastUse == END_POS

  def isDead = lastUse == null

  def isTemporary = definition == TEMPORARY_POS

  def isLongHalf = `type` == LONG_DOUBLE_2
}
