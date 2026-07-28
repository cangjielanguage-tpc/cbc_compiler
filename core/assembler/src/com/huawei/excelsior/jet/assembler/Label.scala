/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

import AsmError.require

/** Label - a Symbol pointing into the code/data being generated.
  *
  * @author cypok
  * @author paul
  * @author shell
  * @author conwor
  */
class Label extends Symbol {
  private var _segment: Segment = _
  private var _position = 0

  def segment: Segment = {
    require(isBound, "Cannot get segment of unbound label")
    _segment
  }

  def position = {
    require(isBound, "Cannot get position of unbound label")
    _position
  }

  def isBound = _segment != null

  private[assembler] def bind(segment: Segment): Unit = {
    require(!isBound, "Labels can be bound only once")
    _segment = segment
    _position = segment.length
    register()
  }

  private def register(): Unit = {
    assert(isBound)
    val idx = segment.labels.size
    assert(idx == 0 || segment.labels(idx - 1).position <= position)
    segment.labels += this
  }

  private[assembler] def moveTo(segment: Segment, positionDelta: Int): Unit = {
    assert(isBound)
    assert(positionDelta >= 0)
    _segment = segment
    _position += positionDelta
    register()
  }

  private[assembler] def incrementPosition(addend: Int): Unit = {
    assert(isBound)
    assert(addend >= 0)
    _position += addend
  }

  override def toString = if (isBound) s"Label[$segment: $position]" else super.toString
}

object Label {
  def unapply(symbol: Symbol) = symbol match {
    case label: Label => Some((label.segment, label.position))
    case _ => None
  }
}
