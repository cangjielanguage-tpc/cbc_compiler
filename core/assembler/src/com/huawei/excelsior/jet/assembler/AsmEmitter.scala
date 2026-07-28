/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

import com.huawei.excelsior.jet.assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.fixups.{Relocation, RelocationKind}
import xscala.util.MathUtils.{isNBitsSigned, low32Bits}

import scala.annotation.nowarn

/** Abstract emitter of some binary code format.
  *
  * @author conwor
  */
abstract class AsmEmitter extends Emitter.WithSegment {

  /** Ensures that segment's contents after current position will have given alignment.
    * Note that alignment is satisfied using NOPs.
    */
  def alignCode(alignment: Int): Unit

  /** Appends segment with code to current segment. */
  def appendCode(code: Segment): Unit = segment.append(code)

  /** Returns current length of `segment`. TODO: consider removing. */
  def currentPosition = segment.length
}

object AsmEmitter {
  /** Emitter of code with data inclusions. */
  abstract class WithLiterals extends AsmEmitter {

    val literalsStart: Label = newLabel

    protected def symbolLiteralKind: RelocationKind

    /** Creates new literal of [[symbolLiteralKind]] with `symbol` + `addend` content. */
    def newLiteral(symbol: Symbol, addend: Int): Literal = new Literal(symbolLiteralKind.width) {
      override def emit(): Unit = segment.addFixup(new Relocation(symbolLiteralKind, symbol, addend))
    }

    /** Creates new literal with `data` content of given `width`. */
    def newLiteral(data: Long, width: Width): Literal = new Literal(width) {
      assert((width == W64) || (width == W32))
      assert(isNBitsSigned(data, width.nbits))

      @nowarn("msg=match may not be exhaustive")
      override def emit(): Unit = width match {
        case W32 => segment.putW32(low32Bits(data))
        case W64 => segment.putW64(data)
      }
    }

    /** Appends `literal` to segment. */
    def addLiteral(literal: Literal): Literal = segment.addLiteral(literal)

    /** Appends new literal of `kind` with `symbol` + `addend` content to segment. */
    def literal(symbol: Symbol, addend: Int): Literal = addLiteral(newLiteral(symbol, addend))

    /** Appends new literal of `kind` with `symbol` content to segment. */
    def literal(symbol: Symbol): Literal = literal(symbol, 0)

    /** Appends new literal with `data` content of given `width` to segment. */
    def literal(data: Long, width: Width): Literal = addLiteral(newLiteral(data, width))

    override def freeze(): Emitter = {
      bind(literalsStart)
      emitData (_.flushLiterals())
      super.freeze()
    }
  }
}
