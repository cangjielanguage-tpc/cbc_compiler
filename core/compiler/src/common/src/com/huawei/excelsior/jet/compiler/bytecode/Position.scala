/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.jet.compiler.ir.{BytecodeOffset, ColumnNumber, InlineContext, LexicalBlock, LineNumber}

import scala.PartialFunction.condOpt

/**
 * Abstract position (coordinate) of IR element.
 * Useful for representing source text positions/bytecode offsets etc.
 *
 * @author paul
 * @author alexm
 */

sealed abstract class Position {
  def orElse(that: Position): Position
  def toString(ignoreNumbers: Boolean): String
  final override def toString: String = toString(ignoreNumbers = false)
}

object Position {
  trait Owner {
    def pos: Position
    def posApproximation: Position = pos
  }

  def offset(p: Position) = condOpt(p) {
    case p: BytecodePosition => p.offset
  }

  def inlineContext(p: Position) = condOpt(p) {
    case p: BytecodePosition => p.inlineContext
  }

  def offsetAndInlineContext(p: Position) = condOpt(p) {
    case p: BytecodePosition => (p.offset, p.inlineContext)
  }
}

case object NoPosition extends Position {
  override def orElse(that: Position) = that
  override def toString(ignoreNumbers: Boolean): String = "no position"
}

case class BytecodePosition(offset: Int, lineNumber: Int, columnNumber: Int, inlineContext: InlineContext, scope: LexicalBlock = null) extends Position {
  require(BytecodeOffset.isValid(offset))
  require(LineNumber.isValid(lineNumber))
  require(ColumnNumber.isValid(columnNumber))

  require(LineNumber.isKnown(lineNumber) || !ColumnNumber.isKnown(columnNumber), "column should be known only if line is known")

  override def orElse(that: Position) = this

  override def toString(ignoreNumbers: Boolean): String = {
    if (ignoreNumbers) {
      s"[${inlineContext.toString(ignoreNumbers = true)}]"
    } else {
      (if (LineNumber.isKnown(lineNumber)) s"line: $lineNumber " else "") +
        (if (ColumnNumber.isKnown(columnNumber)) s"col: $columnNumber " else "") +
        s"bc: $offset [$inlineContext]"
    }
  }
}

object BytecodePosition {
  def apply(offset: Int, inlineContext: InlineContext): BytecodePosition =
    BytecodePosition(offset, LineNumber.UNKNOWN, ColumnNumber.UNKNOWN, inlineContext)

  def apply(inlineContext: InlineContext): BytecodePosition =
    apply(BytecodeOffset.SYNTHETIC, inlineContext)
}
