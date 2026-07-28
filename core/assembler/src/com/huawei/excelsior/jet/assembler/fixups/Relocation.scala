/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.fixups

import com.huawei.excelsior.jet.assembler.{Fixup, Label, Symbol}
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.*

import scala.annotation.nowarn

/** Simple fixup with constant width.
  *
  * @author conwor
  */
final class Relocation(val kind: RelocationKind, val target: Symbol, val addend: Int) extends FixedSizeFixup(kind.width.nbytes) {
  def this(kind: RelocationKind, target: Symbol) = this(kind, target, 0) // TODO-DECAF: use default parameters

  @nowarn("msg=match may not be exhaustive")
  override def resolve(converter: Relocation.Converter): Unit = {
    val (toAddend, toSend) = (target, kind) match {
      case (Label(targetSegment, targetPosition), OFFS32) if targetSegment == this.segment =>
        (targetPosition - this.position, null)

      case (Label(targetSegment, targetPosition), ADDR32 | ADDR64 | OFFS32) =>
        (targetPosition, targetSegment.getSymbol)

      case (Label(targetSegment, targetPosition), OFFS32_IN_SEG) =>
        assert(targetSegment == segment)
        (targetPosition, null)

      case _ =>
        (0, target)
    }

    assert(segment.getSigned(kind.width, position) == 0)
    segment.setSigned(kind.width, position, addend + toAddend)

    if (toSend != null) {
      converter.send(position, kind, toSend)
    }
  }

  override protected def guts = Fixup.seq(target, kind, addend)
}

object Relocation {
  /** Fixups converter. Send fixups to some external environment format. */
  trait Converter {
    def send(position: Int, kind: RelocationKind, target: Symbol): Unit
  }
}
