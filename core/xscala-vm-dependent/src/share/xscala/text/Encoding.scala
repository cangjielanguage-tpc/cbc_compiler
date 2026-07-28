/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

import scala.util.boundary

/** Note for users: encoding and decoding process in all modes except "throwing" is non-bijective.
  *                 Especially the "preserving" mode, which makes no attempt to match encoding and decoding formats.
  * Note for implementers: always return newly allocated array (unless it's an empty array),
  *                        because String constructor assumes the result should not be copied.
  */
abstract class Encoding extends Charset {

  // The absolute minimum required to support an encoding

  def initialState: EncodingState = EncodingState()

  def encodeStep(state: EncodingState, input: EncodingInputCharIterator, output: EncodingOutputByte): Unit

  def decodeStep(state: EncodingState, input: EncodingInputByteIterator, output: EncodingOutputChar): Unit

  // Major implementations that use the methods above

  private final inline def encodeReplacing(state: EncodingState): Array[Byte] = {
    val output = EncodingOutputByteBuffer()
    val it = state.consumeCharsUntilEnd
    boundary {
      while (state.isSuccess && it.hasNext) {
        encodeStep(state, it, output)
        if (state.isUnmappable || state.isMalformed || state.isUnderflow) {
          state.setSuccess()
          output(state, '?'.toByte)
          it.next()
        }
      }
    }
    if (!state.isSuccess) {
      state.throwStateException(it)
    }
    output.result()
  }

  private final inline def decodeReplacing(state: EncodingState): Array[Char] = {
    val output = EncodingOutputCharBuffer()
    val it = state.consumeBytesUntilEnd
    boundary {
      while (state.isSuccess && it.hasNext) {
        decodeStep(state, it, output)
        if (state.isUnmappable || state.isMalformed || state.isUnderflow) {
          state.setSuccess()
          output(state, '?')
          it.next()
        }
      }
    }
    if (!state.isSuccess) {
      state.throwStateException(it)
    }
    output.result()
  }

  private final inline def encodePreserving(state: EncodingState): Array[Byte] = {
    val output = EncodingOutputByteBuffer()
    val it = state.consumeCharsUntilEnd
    boundary {
      while (state.isSuccess && it.hasNext) {
        encodeStep(state, it, output)
        if (state.isUnmappable || state.isMalformed || state.isUnderflow) {
          state.setSuccess()
          val next = it.next()
          output(state, (next & 0xFF).toByte)
          if (next > 0xFF) {
            output(state, ((next >>> 8) & 0xFF).toByte)
          }
        }
      }
    }
    if (!state.isSuccess) {
      state.throwStateException(it)
    }
    output.result()
  }

  private final inline def decodePreserving(state: EncodingState): Array[Char] = {
    val output = EncodingOutputCharBuffer()
    val it = state.consumeBytesUntilEnd
    boundary {
      while (state.isSuccess && it.hasNext) {
        decodeStep(state, it, output)
        if (state.isUnmappable || state.isMalformed || state.isUnderflow) {
          state.setSuccess()
          output(state, (it.next() & 0xFF).toChar)
        }
      }
    }
    if (!state.isSuccess) {
      state.throwStateException(it)
    }
    output.result()
  }

  private final inline def encodeThrowing(state: EncodingState): Array[Byte] = {
    val output = EncodingOutputByteBuffer()
    val it = state.consumeCharsUntilEnd
    boundary {
      while (state.isSuccess && it.hasNext) {
        encodeStep(state, it, output)
      }
    }
    if (!state.isSuccess && !state.isUnderflow) {
      state.throwStateException(it)
    }
    output.result()
  }

  private final inline def decodeThrowing(state: EncodingState): Array[Char] = {
    val output = EncodingOutputCharBuffer()
    val it = state.consumeBytesUntilEnd
    boundary {
      while (state.isSuccess && it.hasNext) {
        decodeStep(state, it, output)
      }
    }
    if (!state.isSuccess && !state.isUnderflow) {
      state.throwStateException(it)
    }
    output.result()
  }

  // Public API that might be highly optimized

  def encodeReplacing(array: Array[Char], start: Int, length: Int): Array[Byte] = {
    val state = initialState
    state.addInput(EncodingInputCharArray(array, start, length))
    encodeReplacing(state)
  }

  def decodeReplacing(array: Array[Byte], start: Int, length: Int): Array[Char] = {
    val state = initialState
    state.addInput(EncodingInputByteArray(array, start, length))
    decodeReplacing(state)
  }

  def encodePreserving(array: Array[Char], start: Int, length: Int): Array[Byte] = {
    val state = initialState
    state.addInput(EncodingInputCharArray(array, start, length))
    encodePreserving(state)
  }

  def decodePreserving(array: Array[Byte], start: Int, length: Int): Array[Char] = {
    val state = initialState
    state.addInput(EncodingInputByteArray(array, start, length))
    decodePreserving(state)
  }

  def encodeThrowing(array: Array[Char], start: Int, length: Int): Array[Byte] = {
    val state = initialState
    state.addInput(EncodingInputCharArray(array, start, length))
    encodeThrowing(state)
  }

  def decodeThrowing(array: Array[Byte], start: Int, length: Int): Array[Char] = {
    val state = initialState
    state.addInput(EncodingInputByteArray(array, start, length))
    decodeThrowing(state)
  }

  // String-receiving or String-returning variants of public API

  def encodeStringReplacing(string: String, start: Int, length: Int): Array[Byte] = {
    val state = initialState
    state.addInput(EncodingInputString(string, start, length))
    encodeReplacing(state)
  }

  def decodeStringReplacing(array: Array[Byte], start: Int, length: Int): String =
    new String(decodeReplacing(array, start, length))

  def encodeStringPreserving(string: String, start: Int, length: Int): Array[Byte] = {
    val state = initialState
    state.addInput(EncodingInputString(string, start, length))
    encodePreserving(state)
  }

  def decodeStringPreserving(array: Array[Byte], start: Int, length: Int): String =
    new String(decodePreserving(array, start, length))

  def encodeStringThrowing(string: String, start: Int, length: Int): Array[Byte] = {
    val state = initialState
    state.addInput(EncodingInputString(string, start, length))
    encodeThrowing(state)
  }

  def decodeStringThrowing(array: Array[Byte], start: Int, length: Int): String =
    new String(decodeThrowing(array, start, length))

  // Convenience aliases

  final inline def encodeReplacing(array: Array[Char]): Array[Byte] = encodeReplacing(array, 0, array.length)

  final inline def decodeReplacing(array: Array[Byte]): Array[Char] = decodeReplacing(array, 0, array.length)

  final inline def encodePreserving(array: Array[Char]): Array[Byte] = encodePreserving(array, 0, array.length)

  final inline def decodePreserving(array: Array[Byte]): Array[Char] = decodePreserving(array, 0, array.length)

  final inline def encodeThrowing(array: Array[Char]): Array[Byte] = encodeThrowing(array, 0, array.length)

  final inline def decodeThrowing(array: Array[Byte]): Array[Char] = decodeThrowing(array, 0, array.length)

  final inline def encodeStringReplacing(string: String): Array[Byte] = encodeStringReplacing(string, 0, string.length)

  final inline def decodeStringReplacing(array: Array[Byte]): String = decodeStringReplacing(array, 0, array.length)

  final inline def encodeStringPreserving(string: String): Array[Byte] = encodeStringPreserving(string, 0, string.length)

  final inline def decodeStringPreserving(array: Array[Byte]): String = decodeStringPreserving(array, 0, array.length)

  final inline def encodeStringThrowing(string: String): Array[Byte] = encodeStringThrowing(string, 0, string.length)

  final inline def decodeStringThrowing(array: Array[Byte]): String = decodeStringThrowing(array, 0, array.length)
}

object Encoding {
  /** Lookup an existing encoding by name. Doesn't find any platform encodings. */
  def find(name: String): Option[Encoding] = {
    if (name.equalsIgnoreCase("UTF-8")) {
      Some(Utf8Encoding)
    } else if (name.equalsIgnoreCase("US-ASCII") || name.equalsIgnoreCase("ASCII")) {
      Some(USAsciiEncoding)
    } else if (name.equalsIgnoreCase("UTF-16LE")) {
      Some(Utf16Encoding.Little)
    } else if (name.equalsIgnoreCase("UTF-16BE")) {
      Some(Utf16Encoding.Big)
    } else if (name.equalsIgnoreCase("UTF-16")) {
      Some(Utf16Encoding.Detect)
    } else if (name.equalsIgnoreCase("ISO-8859-1") || name.equalsIgnoreCase("latin1")) {
      Some(Latin1Encoding)
    } else if (name.equalsIgnoreCase("MUTF-8")) {
      Some(ModifiedUtf8Encoding)
    } else {
      None
    }
  }
}
