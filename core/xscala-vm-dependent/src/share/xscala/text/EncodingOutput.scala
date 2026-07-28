/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

import xscala.io.OutputStream
import xscala.text.EncodingStateKind.Overflow

import scala.collection.mutable.ArrayBuffer
import scala.util.boundary.{Label, break}

abstract class EncodingOutput

abstract class EncodingOutputChar extends EncodingOutput {
  def write(c: Char): Boolean

  inline def apply(state: EncodingState, c: Char)(using exit: Label[Unit]): Unit = {
    if (write(c)) {
      assert(state.isSuccess)
    } else {
      state.setOverflow()
      break()
    }
  }
}

abstract class EncodingOutputByte extends EncodingOutput {
  def write(c: Byte): Boolean

  inline def apply(state: EncodingState, c: Byte)(using exit: Label[Unit]): Unit = {
    if (write(c)) {
      assert(state.isSuccess)
    } else {
      state.setOverflow()
      break()
    }
  }
}

final class EncodingOutputCharCount extends EncodingOutputChar {
  private var _result = 0

  override def write(c: Char): Boolean = {
    _result += 1
    true
  }

  def result: Int = _result

  override def toString = s"EncodingOutputCharCount($result)"
}

final class EncodingOutputByteCount extends EncodingOutputByte {
  private var _result = 0

  override def write(c: Byte): Boolean = {
    _result += 1
    true
  }

  def result: Int = _result

  override def toString = s"EncodingOutputByteCount($result)"
}

final class EncodingOutputCharArray(array: Array[Char], start: Int, length: Int) extends EncodingOutputChar {
  private var position: Int = 0

  override def write(c: Char): Boolean = {
    if (position < length) {
      array(start + position) = c
      position += 1
      true
    } else {
      false
    }
  }

  // Contents intentionally not separated by commas for increased legibility
  override def toString = s"EncodingOutputCharArray(${array.slice(start, start + position).mkString("[", " ", "]")})"
}

final class EncodingOutputByteArray(array: Array[Byte], start: Int, length: Int) extends EncodingOutputByte {
  private var position: Int = 0

  override def write(c: Byte): Boolean = {
    if (position < length) {
      array(start + position) = c
      position += 1
      true
    } else {
      false
    }
  }

  // Contents intentionally not separated by commas for increased legibility
  override def toString = s"EncodingOutputByteArray(${array.slice(start, start + position).mkString("[", " ", "]")})"
}

final class EncodingOutputStream(stream: OutputStream) extends EncodingOutputByte {
  override def write(c: Byte): Boolean = {
    stream.write(c.toInt)
    true
  }

  override def toString = s"EncodingOutputStream($stream)"
}

final class EncodingOutputCharBuffer(buffer: ArrayBuffer[Char] = ArrayBuffer.empty) extends EncodingOutputChar {
  override def write(c: Char): Boolean = {
    buffer.addOne(c)
    true
  }

  def result(): Array[Char] = {
    // We do not use `toArray` here, because Text APIs can be called before even ClassTag is initialized,
    // for instance in Java Class loading APIs implementation at `ClassLoadingUtils.convertToInternalForm`.

    val destination = new Array[Char](buffer.length)
    buffer.copyToArray(destination)
    destination
  }

  // Contents intentionally not separated by commas for increased legibility
  override def toString = s"EncodingOutputCharBuffer(${buffer.mkString("[", " ", "]")})"
}

final class EncodingOutputByteBuffer(buffer: ArrayBuffer[Byte] = ArrayBuffer.empty) extends EncodingOutputByte {
  override def write(c: Byte): Boolean = {
    buffer.addOne(c)
    true
  }

  def result(): Array[Byte] = {
    // We do not use `toArray` here, because Text APIs can be called before even ClassTag is initialized,
    // for instance in Java Class loading APIs implementation at `ClassLoadingUtils.convertToInternalForm`.

    val destination = new Array[Byte](buffer.length)
    buffer.copyToArray(destination)
    destination
  }

  // Contents intentionally not separated by commas for increased legibility
  override def toString = s"EncodingOutputByteBuffer(${buffer.mkString("[", " ", "]")})"
}