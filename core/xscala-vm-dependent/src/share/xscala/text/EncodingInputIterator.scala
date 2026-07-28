/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

import scala.collection.{AbstractIterator, IndexedSeqView}
import scala.reflect.ClassTag
import scala.util.boundary.{Label, break}

abstract class EncodingInputCharIterator extends AbstractIterator[Char] {
  override def hasNext: Boolean

  override def next(): Char

  inline def assertNext(c: Char): Unit = {
    val actual = next()
    assert(c == actual, s"Expected character $c, actual value is $actual")
  }

  inline def assertNext(c0: Char, c1: Char): Unit = {
    assertNext(c0)
    assertNext(c1)
  }

  inline def assertNext(c0: Char, c1: Char, c2: Char): Unit = {
    assertNext(c0, c1)
    assertNext(c2)
  }

  inline def assertNext(c0: Char, c1: Char, c2: Char, c3: Char): Unit = {
    assertNext(c0, c1, c2)
    assertNext(c3)
  }

  inline def assertNext(inline expected: Char*): Unit = {
    val it = expected.iterator
    while (it.hasNext) {
      val c = it.next()
      val actual = next()
      assert(c == actual, s"Expected character $c, actual value is $actual")
    }
  }

  inline def next(state: EncodingState, inline validator: Char => Boolean = _ => true)(inline body: Char => Unit)(using exit: Label[Unit]): Unit = {
    if (hasNext) {
      val c = next()
      if (validator(c)) {
        assert(state.isSuccess)
        body(c)
        assert(state.isSuccess)
      } else {
        state.setMalformed()
        break()
      }
    } else {
      state.setUnderflow()
      break()
    }
  }

  /** Returns a new peek (non-consuming) iterator with the same stop policy and at the same position as the current iterator. */
  def peekIterator: EncodingInputCharIterator
}

abstract class EncodingInputByteIterator extends AbstractIterator[Byte] {
  override def hasNext: Boolean

  override def next(): Byte

  inline def assertNext(c: Byte): Unit = {
    val actual = next()
    assert(c == actual, s"Expected byte $c, actual value is $actual")
  }

  inline def assertNext(c0: Byte, c1: Byte): Unit = {
    assertNext(c0)
    assertNext(c1)
  }

  inline def assertNext(c0: Byte, c1: Byte, c2: Byte): Unit = {
    assertNext(c0, c1)
    assertNext(c2)
  }

  inline def assertNext(c0: Byte, c1: Byte, c2: Byte, c3: Byte): Unit = {
    assertNext(c0, c1, c2)
    assertNext(c3)
  }

  inline def next(state: EncodingState, inline validator: Byte => Boolean = _ => true)(inline body: Byte => Unit)(using exit: Label[Unit]): Unit = {
    if (hasNext) {
      val c = next()
      if (validator(c)) {
        assert(state.isSuccess)
        body(c)
        assert(state.isSuccess)
      } else {
        state.setMalformed()
        break()
      }
    } else {
      state.setUnderflow()
      break()
    }
  }

  /** Returns a new peek (non-consuming) iterator with the same stop policy and at the same position as the current iterator. */
  def peekIterator: EncodingInputByteIterator
}

object EncodingInputCharIterator {
  val empty: EncodingInputCharIterator = new EncodingInputCharIterator {
    override def hasNext: Boolean = false

    override def next(): Char = throw new NoSuchElementException("next on empty iterator")

    override def knownSize: Int = 0

    override protected def sliceIterator(from: Int, until: Int): EncodingInputCharIterator = this

    override def peekIterator: EncodingInputCharIterator = this

    override def toString = s"EncodingInputCharIterator.empty"

    override def toArray[B >: Char: ClassTag]: Array[B] = new Array[B](0)
  }
}

object EncodingInputByteIterator {
  val empty: EncodingInputByteIterator = new EncodingInputByteIterator {
    override def hasNext: Boolean = false

    override def next(): Byte = throw new NoSuchElementException("next on empty iterator")

    override def knownSize: Int = 0

    override protected def sliceIterator(from: Int, until: Int): EncodingInputByteIterator = this

    override def peekIterator: EncodingInputByteIterator = this

    override def toString = s"EncodingInputByteIterator.empty"

    override def toArray[B >: Byte: ClassTag]: Array[B] = new Array[B](0)
  }
}

final class EncodingInputCharConcatIterator(inputsView: IndexedSeqView[EncodingInputCharIterator]) extends EncodingInputCharIterator {
  private var inputIndex: Int = 0
  private var cur: EncodingInputCharIterator = EncodingInputCharIterator.empty
  private var _hasNext: Int = -1 // -1 for None, 0 or 1 is Some[Boolean]

  override def hasNext: Boolean = {
    val hasNext = _hasNext
    if (hasNext == -1) {
      while (!cur.hasNext) {
        if (inputIndex == inputsView.length) {
          _hasNext = 0
          cur = EncodingInputCharIterator.empty
          return false
        }
        cur = inputsView(inputIndex)
        inputIndex += 1
      }
      _hasNext = 1
      true
    } else {
      hasNext == 1
    }
  }

  override def next(): Char = {
    if (hasNext) {
      _hasNext = -1
    }
    cur.next()
  }

  override def peekIterator: EncodingInputCharIterator = EncodingInputCharConcatIterator(
    inputsView.drop(inputIndex).prepended(cur).map(_.peekIterator)
  )

  override def toString = s"EncodingInputCharConcatIterator(next=$inputIndex in ${inputsView.mkString("[", ", ", "]")})"
}

final class EncodingInputByteConcatIterator(inputsView: IndexedSeqView[EncodingInputByteIterator]) extends EncodingInputByteIterator {
  private var inputIndex: Int = 0
  private var cur: EncodingInputByteIterator = EncodingInputByteIterator.empty
  private var _hasNext: Int = -1 // -1 for None, 0 or 1 is Some[Boolean]

  override def hasNext: Boolean = {
    val hasNext = _hasNext
    if (hasNext == -1) {
      while (!cur.hasNext) {
        if (inputIndex == inputsView.length) {
          _hasNext = 0
          cur = EncodingInputByteIterator.empty
          return false
        }
        cur = inputsView(inputIndex)
        inputIndex += 1
      }
      _hasNext = 1
      true
    } else {
      hasNext == 1
    }
  }

  override def next(): Byte = {
    if (hasNext) {
      _hasNext = -1
    }
    cur.next()
  }

  override def peekIterator: EncodingInputByteIterator = EncodingInputByteConcatIterator(
    inputsView.drop(inputIndex).prepended(cur).map(_.peekIterator)
  )

  override def toString = s"EncodingInputByteConcatIterator(next=$inputIndex in ${inputsView.mkString("[", ", ", "]")})"
}
