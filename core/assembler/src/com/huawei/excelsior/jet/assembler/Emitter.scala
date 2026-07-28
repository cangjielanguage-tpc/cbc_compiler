/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

/** Abstract emitter of some binary format.
  *
  * NOTE: any method of this class should be either final or abstract.
  * Otherwise implementation of Emitter.Delegate will not be correct.
  *
  * @author conwor
  */
abstract class Emitter {
  def setUp(seg: Segment): Unit
  final def setUp(symbol: Symbol): Unit = setUp(new Segment(symbol))
  final def setUp(): Unit = setUp(null: Symbol)

  protected def segment: Segment

  final def tearDown(): Segment = {
    val result = segment
    setUp(null: Segment)
    result
  }

  def freeze(): Emitter

  /** Sets up `seg`, implements `action` and sets up this emitter into previous state. */
  final def withSegment(seg: Segment)(action: => Unit): Unit = {
    val old = tearDown()
    setUp(seg)
    try {
      action
      tearDown()
    } finally {
      setUp(old)
    }
  }

  final def withNewSegment(action: => Unit): Segment = {
    val s = new Segment
    withSegment(s)(action)
    s
  }

  def emitData(e: Segment => Unit): Unit

  /** Creates new label without binding. */
  final def newLabel = new Label

  /** Creates new label which is bound to current position. */
  final def newBoundLabel = segment.newBoundLabel

  /** Binds `label` to current position. */
  final def bind(label: Label): Unit = segment.bindLabel(label)

  /** Sets alignment of segment's start. */
  final def alignStart(alignment: Int): Unit = segment.alignStart(alignment)

  /** Appends `fixup` to segment. */
  protected def addFixup(fixup: Fixup): Unit
}

object Emitter {
  /** Kind of Emitter which holds segment as part of self state. */
  abstract class WithSegment extends Emitter {
    protected var seg: Segment = _
    override def setUp(seg: Segment): Unit = this.seg = seg
    override final protected def segment = seg
    override def freeze(): Emitter = { seg.freeze(); this }
    override def emitData(e: Segment => Unit): Unit = e(seg)
    override protected def addFixup(fixup: Fixup): Unit = segment.addFixup(fixup)
  }

  /** Kind of Emitter which uses another Emitter as implementation of segment holder. */
  abstract class Delegate protected(val impl: Emitter) extends Emitter {
    override final def setUp(seg: Segment): Unit = impl.setUp(seg)
    override final protected def segment = impl.segment
    override def freeze(): Emitter = { impl.freeze(); this }
    override def emitData(e: Segment => Unit): Unit = impl.emitData(e)
    override protected def addFixup(fixup: Fixup): Unit = impl.addFixup(fixup)
  }
}
