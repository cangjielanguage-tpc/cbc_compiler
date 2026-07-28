/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}

import scala.annotation.nowarn
import scala.collection.mutable

/**
 * Value numbering table.
 * Maps any structural equal Nodes to single value number.
 *
 * @author paul
 */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait ValueNumbering { self: Universe =>

  private case class Value(node: Node) {
    override val hashCode = (node.proto.## * 31) + node.argsSeq.##

    /** Structural equality: Nodes A and B both structurally equal iff
      *  1) their argument lists are equal: A.args == B.args
      *     -- Note that arguments are checked for default (reference, non-structural) equality!
      *  2) Both nodes have the same semantics on their arguments: A.proto == B.proto.
      *  3) If both nodes are phies, then they structurally equal if their blocks are equal (equal by reference)
      */
    override def equals(that: Any) = that match {
      case that: Value if this.## == that.## =>
        (this.node eq that.node) || // fast path
          (this.node.proto == that.node.proto) &&
          (this.node.argsSeq == that.node.argsSeq) &&
          (if bothPhies(that) then this.node.block eq that.node.block else true)

      case _ => false
    }
    private def bothPhies(that: Value): Boolean = this.node.isInstanceOf[Phi] && that.node.isInstanceOf[Phi]
  }

  private val values = Maps[Node].newMMap[Value]
  private val vntable = mutable.HashMap.empty[Value, Node]
  private val changed = Sets[Node].newQSet
  private var enabled = true

  def resetValueNumbering(): Unit = {
    values.clear()
    vntable.clear()
    changed.clear()
    this.enabled = true
  }

  /** Marker trait for node that is structurally equal to itself only. */
  trait StructurallyUnique { this: Node => }

  private def append[N <: Node](n: N): N = n match {
    case _: StructurallyUnique => n
    case _ if values contains n => n
    case _ if n.hasUndefinedArgs => n
    case _ =>
      val v = Value(n)
      vntable.getOrElseUpdate(v, { values(n) = v; n }).asInstanceOf[N]
  }

  private def remove(n: Node): Boolean = {
    assert(n.isCommitted)
    values.remove(n) match {
      case Some(v) => vntable -= v; true
      case None => false
    }
  }

  def unValueNumber(n: Node): Unit = {
    if (enabled) {
      assert(n.isExact)
      remove(n)
    }
  }

  /** Get the representative ("value number") for the given node n.
   *  Such value number is structurally equal to n
   *  (see Node#struct_==())
   *  and so can be used instead n in any context.
   */
  def valueNumber[N <: Node](n: N): N = {
    if (!enabled) n else {
      changed foreach append
      changed.clear()
      append(n)
    }
  }
  def valueNumberingEnabled = enabled

  def disableValueNumbering() = {
    resetValueNumbering()
    enabled = false
  }

  protected def registerVNInUniverseCallbacks(): Unit = {
    onDecommit.addCallback { n => if (enabled) { remove(n) || changed.remove(n) } }

    // Remove structurally changed node from the table, because it's struct_## will be changed.
    // If we do not do this, there will be broken record in vntable which may lead to unstable compilation results.
    // Collect changed nodes in special set, which will be drained to VNTable at first call of valueNumber().
    // For more details look at JET-10309.
    beforeStructuralChange.addCallback { n => if (enabled && remove(n)) changed += n }
  }

}
