/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

import xscala.io.InputStream

import java.util.ConcurrentModificationException
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

/** The hierarchy root of everything that can be provided as an input for text API.
  * @see [[EncodingInputChar]] Inputs producing [[Char]] values
  * @see [[EncodingInputByte]] Inputs producing [[Byte]] values
  */
trait EncodingInput

/** The base trait of everything that can be provided as an input of [[Char]] elements for text API.
  * In non-trivial cases the standard operation is as follows:
  * <ul>
  *   <li>Outer loop on consuming iterator while [[Iterator.hasNext]] is `true`</li>
  *   <li>Actual workload:
  *     <ol>
  *       <li>Takes [[EncodingInputCharIterator.peekIterator peek iterator]] from the outer consuming iterator</li>
  *       <li>Decides on the action using [[EncodingInputCharIterator.next data]] from <i>peek iterator</i></li>
  *       <li>Commits the action by [[EncodingInputCharIterator.assertNext consuming]] data</li>
  *     </ol>
  *   </li>
  * </ul>
  * @see [[EncodingInputCharArray]] [[Array]] input
  */
trait EncodingInputChar extends EncodingInput {
  /** Iterator that consumes the data until the end of the entire input data is reached.
    * Consuming is an irreversible operation: you can't seek backwards (no `previous` operation).
    * All consuming iterators are in sync: even if they have different object identities, they all move as one.
    * So, unlike peek iterators, there can be no iterator at any previous position.
    * The iterator returned by this method will block as many times as necessary.
    * @see [[consumeUntilBlockedIterator]] Non-blocking version
    * @see [[peekUntilEndIterator]] Non-consuming (lookahead) version
    */
  def consumeUntilEndIterator: EncodingInputCharIterator

  /** Iterator that consumes the available data, until blocking is required to obtain further data,
    * or the end of the entire input data is reached.
    * Consuming is an irreversible operation: you can't seek backwards (no `previous` operation).
    * All consuming iterators are in sync: even if they have different object identities, they all move as one.
    * So, unlike peek iterators, there can be no iterator at any previous position.
    * Array inputs never require I/O, hence never block. But for [[EncodingInputStream streams]],
    * the blocking requirement is determined based on [[InputStream.available]].
    * If it is determined that the [[Iterator.next]] operation requires blocking,
    * this iterator will have reached its end.
    * Consider [[InputStream]] created from standard console input: likely finite file with no length estimation,
    * that is not seekable, and that frequently blocks on I/O for indeterminate amount of time.
    * If [[consumeUntilEndIterator blocking version]] is used, text APIs are likely to request further user input
    * until the underlying stream is closed. This non-blocking iterator will end text API operation gracefully,
    * so that you can process each input immediately after receiving it.
    * @see [[consumeUntilEndIterator]] Blocking version
    * @see [[peekUntilBlockedIterator]] Non-consuming (lookahead) version
    */
  def consumeUntilBlockedIterator: EncodingInputCharIterator

  /** Iterator that provides arbitrary (potentially buffering) lookahead,
    * potentially until the end of the entire input data.
    * Consider [[InputStream]] created from `/dev/random`: infinite file that is not seekable.
    * While the iteration will never stop, full input is rarely needed.
    * Unlike the full input, lookahead is much more useful: a small number of [[Char elements]]
    * ahead of the current position is enough for most APIs operating on text.
    * The iterator returned by this method will block as many times as necessary.
    * @see [[consumeUntilEndIterator]] Consuming version
    * @see [[peekUntilBlockedIterator]] Non-blocking version
    */
  def peekUntilEndIterator: EncodingInputCharIterator

  /** Iterator that provides arbitrary (potentially buffering) lookahead,
    * potentially until blocking is required to obtain further data, or the end of the entire input data is reached.
    * Array inputs never require I/O, hence never block. But for [[EncodingInputStream streams]],
    * the blocking requirement is determined based on [[InputStream.available]].
    * If it is determined that the [[Iterator.next]] operation requires blocking,
    * this iterator will have reached its end.
    * Consider [[InputStream]] created from standard console input: likely finite file with no length estimation,
    * that is not seekable, and that frequently blocks on I/O for indeterminate amount of time.
    * If [[consumeUntilEndIterator blocking version]] is used, text APIs are likely to request further user input
    * until the underlying stream is closed. This non-blocking iterator will end text API operation gracefully,
    * so that you can process each input immediately after receiving it.
    * @see [[consumeUntilBlockedIterator]] Consuming version
    * @see [[peekUntilEndIterator]] Blocking version
    */
  def peekUntilBlockedIterator: EncodingInputCharIterator
}

/** The base trait of everything that can be provided as an input of [[Byte]] elements for text API.
  * In non-trivial cases the standard operation is as follows:
  * <ul>
  *   <li>Outer loop on consuming iterator while [[Iterator.hasNext]] is `true`</li>
  *   <li>Actual workload:
  *     <ol>
  *       <li>Takes [[EncodingInputByteIterator.peekIterator peek iterator]] from the outer consuming iterator</li>
  *       <li>Decides on the action using [[EncodingInputByteIterator.next data]] from <i>peek iterator</i></li>
  *       <li>Commits the action by [[EncodingInputByteIterator.assertNext consuming]] data</li>
  *     </ol>
  *   </li>
  * </ul>
  *
  * @see [[EncodingInputByteArray]] [[Array]] input
  * @see [[EncodingInputStream]] [[InputStream]] input
  */
trait EncodingInputByte extends EncodingInput {
  /** Iterator that consumes the data until the end of the entire input data is reached.
    * Consuming is an irreversible operation: you can't seek backwards (no `previous` operation).
    * All consuming iterators are in sync: even if they have different object identities, they all move as one.
    * So, unlike peek iterators, there can be no iterator at any previous position.
    * The iterator returned by this method will block as many times as necessary.
    * @see [[consumeUntilBlockedIterator]] Non-blocking version
    * @see [[peekUntilEndIterator]] Non-consuming (lookahead) version
    */
  def consumeUntilEndIterator: EncodingInputByteIterator

  /** Iterator that consumes the available data, until blocking is required to obtain further data,
    * or the end of the entire input data is reached.
    * Consuming is an irreversible operation: you can't seek backwards (no `previous` operation).
    * All consuming iterators are in sync: even if they have different object identities, they all move as one.
    * So, unlike peek iterators, there can be no iterator at any previous position.
    * Array inputs never require I/O, hence never block. But for [[EncodingInputStream streams]],
    * the blocking requirement is determined based on [[InputStream.available]].
    * If it is determined that the [[Iterator.next]] operation requires blocking,
    * this iterator will have reached its end.
    * Consider [[InputStream]] created from standard console input: likely finite file with no length estimation,
    * that is not seekable, and that frequently blocks on I/O for indeterminate amount of time.
    * If [[consumeUntilEndIterator blocking version]] is used, text APIs are likely to request further user input
    * until the underlying stream is closed. This non-blocking iterator will end text API operation gracefully,
    * so that you can process each input immediately after receiving it.
    * @see [[consumeUntilEndIterator]] Blocking version
    * @see [[peekUntilBlockedIterator]] Non-consuming (lookahead) version
    */
  def consumeUntilBlockedIterator: EncodingInputByteIterator

  /** Iterator that provides arbitrary (potentially buffering) lookahead,
    * potentially until the end of the entire input data.
    * Consider [[InputStream]] created from `/dev/random`: infinite file that is not seekable.
    * While the iteration will never stop, full input is rarely needed.
    * Unlike the full input, lookahead is much more useful: a small number of [[Byte elements]]
    * ahead of the current position is enough for most APIs operating on text.
    * The iterator returned by this method will block as many times as necessary.
    * @see [[consumeUntilEndIterator]] Consuming version
    * @see [[peekUntilBlockedIterator]] Non-blocking version
    */
  def peekUntilEndIterator: EncodingInputByteIterator

  /** Iterator that provides arbitrary (potentially buffering) lookahead,
    * potentially until blocking is required to obtain further data, or the end of the entire input data is reached.
    * Array inputs never require I/O, hence never block. But for [[EncodingInputStream streams]],
    * the blocking requirement is determined based on [[InputStream.available]].
    * If it is determined that the [[Iterator.next]] operation requires blocking,
    * this iterator will have reached its end.
    * Consider [[InputStream]] created from standard console input: likely finite file with no length estimation,
    * that is not seekable, and that frequently blocks on I/O for indeterminate amount of time.
    * If [[consumeUntilEndIterator blocking version]] is used, text APIs are likely to request further user input
    * until the underlying stream is closed. This non-blocking iterator will end text API operation gracefully,
    * so that you can process each input immediately after receiving it.
    * @see [[consumeUntilBlockedIterator]] Consuming version
    * @see [[peekUntilEndIterator]] Blocking version
    */
  def peekUntilBlockedIterator: EncodingInputByteIterator
}

final class EncodingInputCharArray(array: Array[Char], start: Int, length: Int) extends EncodingInputCharIterator with EncodingInputChar { self =>
  private var position: Int = 0

  override def hasNext: Boolean = position < length

  override def next(): Char = if (!hasNext) Iterator.empty.next() else {
    val result = array(start + position)
    position += 1
    result
  }

  override def peekIterator: EncodingInputCharIterator = peekIterator()

  override def knownSize: Int = length - position

  override def toArray[B >: Char : ClassTag]: Array[B] = {
    val fullLength = length
    val resultLength = fullLength - position
    val array = new Array[B](resultLength)
    Array.copy(self.array, start + position, array, 0, resultLength)
    position = fullLength
    array
  }

  private def peekIterator(initialPosition: Int = position): EncodingInputCharIterator = new EncodingInputCharIterator {
    private var position = initialPosition

    override def hasNext: Boolean = position < self.length

    override def next(): Char = if (!hasNext) Iterator.empty.next() else {
      val result = array(start + position)
      position += 1
      result
    }

    override def peekIterator: EncodingInputCharIterator = self.peekIterator(position)

    override def toString = s"EncodingInputCharIterator($position/${self.length})"

    override def knownSize: Int = self.length - position

    override def toArray[B >: Char: ClassTag]: Array[B] = {
      val fullLength = self.length
      val resultLength = fullLength - position
      val array = new Array[B](resultLength)
      Array.copy(self.array, start + position, array, 0, resultLength)
      position = fullLength
      array
    }
  }

  override def consumeUntilEndIterator: EncodingInputCharIterator = this

  override def consumeUntilBlockedIterator: EncodingInputCharIterator = this

  override def peekUntilEndIterator: EncodingInputCharIterator = peekIterator()

  override def peekUntilBlockedIterator: EncodingInputCharIterator = peekIterator()

  override def toString = s"EncodingInputCharArray($position/$length)"
}

final class EncodingInputString(string: String, start: Int, length: Int) extends EncodingInputCharIterator with EncodingInputChar { self =>
  private var position: Int = 0

  override def hasNext: Boolean = position < length

  override def next(): Char = if (!hasNext) Iterator.empty.next() else {
    val result = string(start + position)
    position += 1
    result
  }

  override def peekIterator: EncodingInputCharIterator = peekIterator()

  override def knownSize: Int = length - position

  private def peekIterator(initialPosition: Int = position): EncodingInputCharIterator = new EncodingInputCharIterator {
    private var position = initialPosition

    override def hasNext: Boolean = position < self.length

    override def next(): Char = if (!hasNext) Iterator.empty.next() else {
      val result = string(start + position)
      position += 1
      result
    }

    override def peekIterator: EncodingInputCharIterator = self.peekIterator(position)

    override def toString = s"EncodingInputString($position/${self.length})"

    override def knownSize: Int = self.length - position
  }

  override def consumeUntilEndIterator: EncodingInputCharIterator = this

  override def consumeUntilBlockedIterator: EncodingInputCharIterator = this

  override def peekUntilEndIterator: EncodingInputCharIterator = peekIterator()

  override def peekUntilBlockedIterator: EncodingInputCharIterator = peekIterator()

  override def toString = s"EncodingInputString($position/$length)"
}

final class EncodingInputByteArray(array: Array[Byte], start: Int, length: Int) extends EncodingInputByteIterator with EncodingInputByte { self =>
  private var position: Int = 0

  override def hasNext: Boolean = position < length

  override def next(): Byte = if (!hasNext) Iterator.empty.next() else {
    val result = array(start + position)
    position += 1
    result
  }

  override def peekIterator: EncodingInputByteIterator = peekIterator()

  override def knownSize: Int = length - position

  override def toArray[B >: Byte : ClassTag]: Array[B] = {
    val fullLength = self.length
    val resultLength = fullLength - position
    val array = new Array[B](resultLength)
    Array.copy(self.array, start + position, array, 0, resultLength)
    position = fullLength
    array
  }

  private def peekIterator(initialPosition: Int = position): EncodingInputByteIterator = new EncodingInputByteIterator {
    private var position = initialPosition

    override def hasNext: Boolean = position < self.length

    override def next(): Byte = if (!hasNext) Iterator.empty.next() else {
      val result = array(start + position)
      position += 1
      result
    }

    override def peekIterator: EncodingInputByteIterator = self.peekIterator(position)

    override def toString = s"EncodingInputByteIterator($position/${self.length})"

    override def knownSize: Int = self.length - position

    override def toArray[B >: Byte : ClassTag]: Array[B] = {
      val fullLength = self.length
      val resultLength = fullLength - position
      val array = new Array[B](resultLength)
      Array.copy(self.array, start + position, array, 0, resultLength)
      position = fullLength
      array
    }
  }

  override def consumeUntilEndIterator: EncodingInputByteIterator = this

  override def consumeUntilBlockedIterator: EncodingInputByteIterator = this

  override def peekUntilEndIterator: EncodingInputByteIterator = peekIterator()

  override def peekUntilBlockedIterator: EncodingInputByteIterator = peekIterator()

  override def toString = s"EncodingInputByteArray($position/$length)"
}

final class EncodingInputStream(stream: InputStream) extends EncodingInputByte { self =>
  private val buffer = ListBuffer.empty[Byte]
  private var _end = false
  private var consumed = 0

  // We do not use `lazy val` here, because Text APIs can be called before even exceptions are initialized,
  // for instance in Java Class loading APIs implementation at `ClassLoadingUtils.convertToInternalForm`.
  // This happens before `LazyValsVMDependent` class is initialized, so `lazy val` cannot work.

  @volatile private var _consumeUntilEndIterator: EncodingInputByteIterator = _
  @volatile private var _consumeUntilBlockedIterator: EncodingInputByteIterator = _

  private def consumeIterator(allowBlocking: Boolean) = new EncodingInputByteIterator {
    override def hasNext: Boolean = {
      if (buffer.isEmpty) {
        bufferOne(allowBlocking)
      }
      buffer.nonEmpty
    }

    override def next(): Byte = {
      if (buffer.isEmpty) {
        bufferOne(allowBlocking)
      }
      if (buffer.nonEmpty) {
        consumed += 1
        buffer.remove(0)
      } else {
        Iterator.empty.next()
      }
    }

    override def peekIterator: EncodingInputByteIterator = self.peekIterator(allowBlocking)

    override def toString = self.toString
  }

  private def peekIterator(allowBlocking: Boolean, initialPosition: Int = 0): EncodingInputByteIterator = new EncodingInputByteIterator {
    // val consumer = input.consumeUntilEndIterator
    // val observer = input.peekUntilEndIterator
    // observer.next() // -> A
    // observer.next() // -> B
    // buffer is [A, B], consumed is 0, _position is 2, positionEpoch is 0
    // consumer.next() // -> A
    // buffer is [B], consumed is 1, _position is 2, positionEpoch is 0
    // observer.hasNext // -> true
    // buffer is [B, C], consumed is 1, _position is 1, positionEpoch is 1
    // observer.next() // -> C
    // buffer is [B, C], consumed is 1, _position is 2, positionEpoch is 1
    // consumer.next() // -> B
    // consumer.next() // -> C
    // buffer is [], consumed is 3, _position is 2, positionEpoch is 1
    // observer.hasNext // -> true
    // buffer is [D], consumed is 3, _position is 0, positionEpoch is 3
    // consumer.next() // -> D
    // buffer is [], consumed is 4, _position is 0, positionEpoch is 3
    // consumer.next() // -> E
    // buffer is [], consumed is 5, _position is 0, positionEpoch is 3
    // observer.hasNext // -> ConcurrentModificationException is thrown (newPosition is 0 - (5 - 3) = -2)

    private var _position = initialPosition
    private var positionEpoch = consumed // Normally equals consumed, provides resilience of `position` in the face of buffer element removal

    private def position: Int = {
      val consumed = self.consumed
      val newPosition = _position - (consumed - positionEpoch)
      if (newPosition < 0) {
        throw new ConcurrentModificationException(s"A consumer iterator is ${-newPosition} elements ahead of this peek iterator")
      }

      _position = newPosition
      positionEpoch = consumed
      newPosition
    }

    override def hasNext: Boolean = {
      val position = this.position
      val bufferLength = buffer.length
      if (position < bufferLength) {
        true
      } else {
        assert(position == bufferLength)
        bufferOne(allowBlocking)
        position < buffer.length
      }
    }

    override def next(): Byte = {
      val position = this.position
      if (position == buffer.length) {
        bufferOne(allowBlocking)
      }
      if (position < buffer.length) {
        val result = buffer(position)
        _position += 1
        result
      } else {
        Iterator.empty.next()
      }
    }

    override def peekIterator: EncodingInputByteIterator = self.peekIterator(allowBlocking, position)

    override def toString = s"EncodingInputByteIterator($position in $self)"
  }

  private def bufferOne(allowBlocking: Boolean): Unit = {
    if (_end) {
      return
    }
    if (allowBlocking || (stream.available() > 0)) {
      val next = stream.read()
      if (next == -1) {
        _end = true
      } else {
        buffer.append(next.toByte)
      }
    }
  }

  override def consumeUntilEndIterator: EncodingInputByteIterator = {
    if (_consumeUntilEndIterator == null) {
      _consumeUntilEndIterator = consumeIterator(true)
    }
    _consumeUntilEndIterator
  }

  override def consumeUntilBlockedIterator: EncodingInputByteIterator = {
    if (_consumeUntilBlockedIterator == null) {
      _consumeUntilBlockedIterator = consumeIterator(false)
    }
    _consumeUntilBlockedIterator
  }

  override def peekUntilEndIterator: EncodingInputByteIterator = peekIterator(true)

  override def peekUntilBlockedIterator: EncodingInputByteIterator = peekIterator(false)

  override def toString = s"EncodingInputStream($consumed, buffered=${buffer.length}, available=${stream.available()}, end=$_end)"
}

