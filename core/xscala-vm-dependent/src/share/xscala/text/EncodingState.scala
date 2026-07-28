/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

import scala.collection.mutable.ArrayBuffer

enum EncodingStateKind {
  /** Everything is okay, the inputs may or may not have data to decode. */
  case Success
  /** Current codepoint is invalid in the source encoding. */
  case Malformed
  /** Current character cannot be represented in the target encoding. */
  case Unmappable
  /** Additional input is required for successful encoding. */
  case Underflow
  /** Additional output buffer space is required for successful encoding. */
  case Overflow
}

class EncodingState {

  private val inputs = ArrayBuffer.empty[EncodingInput]
  private var _kind = EncodingStateKind.Success

  final def addInput(input: EncodingInputByte): Unit = {
    inputs.addOne(input)
  }

  final def addInput(input: EncodingInputChar): Unit = {
    inputs.addOne(input)
  }

  private inline def byteInput(inline f: EncodingInputByte => EncodingInputByteIterator): EncodingInputByteIterator =
    inputs.size match {
      case 0 => EncodingInputByteIterator.empty
      case 1 => f(inputs.head.asInstanceOf[EncodingInputByte])
      case _ => EncodingInputByteConcatIterator(inputs.view.map(x => f(x.asInstanceOf[EncodingInputByte])))
    }

  private inline def charInput(inline f: EncodingInputChar => EncodingInputCharIterator): EncodingInputCharIterator =
    inputs.size match {
      case 0 => EncodingInputCharIterator.empty
      case 1 => f(inputs.head.asInstanceOf[EncodingInputChar])
      case _ => EncodingInputCharConcatIterator(inputs.view.map(x => f(x.asInstanceOf[EncodingInputChar])))
    }

  final def consumeBytesUntilEnd: EncodingInputByteIterator = byteInput(_.consumeUntilEndIterator)

  final def consumeBytesUntilBlocked: EncodingInputByteIterator = byteInput(_.consumeUntilBlockedIterator)

  final def peekBytesUntilEnd: EncodingInputByteIterator = byteInput(_.peekUntilEndIterator)

  final def peekBytesUntilBlocked: EncodingInputByteIterator = byteInput(_.peekUntilBlockedIterator)

  final def consumeCharsUntilEnd: EncodingInputCharIterator = charInput(_.consumeUntilEndIterator)

  final def consumeCharsUntilBlocked: EncodingInputCharIterator = charInput(_.consumeUntilBlockedIterator)

  final def peekCharsUntilEnd: EncodingInputCharIterator = charInput(_.peekUntilEndIterator)

  final def peekCharsUntilBlocked: EncodingInputCharIterator = charInput(_.peekUntilBlockedIterator)

  final def kind: EncodingStateKind = _kind

  /** @see [[EncodingStateKind.Success]] */
  inline def isSuccess: Boolean = _kind == EncodingStateKind.Success

  /** @see [[EncodingStateKind.Malformed]] */
  inline def isMalformed: Boolean = _kind == EncodingStateKind.Malformed

  /** @see [[EncodingStateKind.Unmappable]] */
  inline def isUnmappable: Boolean = _kind == EncodingStateKind.Unmappable

  /** @see [[EncodingStateKind.Underflow]] */
  inline def isUnderflow: Boolean = _kind == EncodingStateKind.Underflow

  /** @see [[EncodingStateKind.Overflow]] */
  inline def isOverflow: Boolean = _kind == EncodingStateKind.Overflow

  /** @see [[EncodingStateKind.Success]] */
  final def setSuccess(): Unit = {
    _kind = EncodingStateKind.Success
  }

  /** @see [[EncodingStateKind.Malformed]] */
  final def setMalformed(): Unit = {
    _kind = EncodingStateKind.Malformed
  }

  /** @see [[EncodingStateKind.Unmappable]] */
  final def setUnmappable(): Unit = {
    _kind = EncodingStateKind.Unmappable
  }

  /** @see [[EncodingStateKind.Underflow]] */
  final def setUnderflow(): Unit = {
    _kind = EncodingStateKind.Underflow
  }

  /** @see [[EncodingStateKind.Overflow]] */
  final def setOverflow(): Unit = {
    _kind = EncodingStateKind.Overflow
  }

  def throwStateException(context: AnyRef): Unit = {
    _kind match {
      case EncodingStateKind.Success =>
      case EncodingStateKind.Malformed => throw EncodingState.newMalformedException(context)
      case EncodingStateKind.Unmappable => throw EncodingState.newUnmappableException(context)
      case EncodingStateKind.Underflow => throw EncodingState.newUnderflowException(context)
      case EncodingStateKind.Overflow => throw EncodingState.newOverflowException(context)
    }
  }

  override def toString = s"EncodingState($kind, $inputs)"
}

object EncodingState {
  def newMalformedException(context: AnyRef): EncodingException =
    EncodingMalformedException(s"Malformed input at $context")

  def newUnmappableException(context: AnyRef): EncodingException =
    EncodingUnmappableException(s"Unmappable character at $context")

  def newUnderflowException(context: AnyRef): EncodingException =
    EncodingUnderflowException(s"Input buffer is truncated at $context")

  def newOverflowException(context: AnyRef): EncodingException =
    EncodingOverflowException(s"Output buffer overflow at $context")
}
