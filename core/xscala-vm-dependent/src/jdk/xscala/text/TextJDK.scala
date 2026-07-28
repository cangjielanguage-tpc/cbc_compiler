/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

import java.nio.charset.{Charset, CodingErrorAction, StandardCharsets}
import java.nio.{ByteBuffer, CharBuffer}
import scala.util.boundary

private final class TextJDKEncoding(charset: Charset) extends PlatformEncoding {
  override protected def encodeStep(state: EncodingState, input: Array[Char], output: EncodingOutputByte): Int = {
    if (input.isEmpty) {
      return 0
    }

    val encoder = charset.newEncoder()
    encoder.onMalformedInput(CodingErrorAction.REPORT)
    encoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    val in = CharBuffer.wrap(input)
    val out = ByteBuffer.allocate(((in.remaining + 1) * encoder.maxBytesPerChar + 1).toInt)
    var res = encoder.encode(in, out, true)
    if (res.isUnderflow) {
      res = encoder.flush(out)
    }
    out.flip()
    boundary {
      while (out.hasRemaining) {
        output(state, out.get())
      }
      if (res.isMalformed) {
        state.setMalformed()
      } else if (res.isUnmappable) {
        state.setUnmappable()
      } else {
        assert(res.isUnderflow)
      }
    }

    input.length - in.remaining
  }

  override protected def decodeStep(state: EncodingState, input: Array[Byte], output: EncodingOutputChar): Int = {
    if (input.isEmpty) {
      return 0
    }

    val decoder = charset.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPORT)
    decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    val in = ByteBuffer.wrap(input)
    val out = CharBuffer.allocate(((in.remaining + 1) * decoder.maxCharsPerByte + 1).toInt)
    var res = decoder.decode(in, out, true)
    if (res.isUnderflow) {
      res = decoder.flush(out)
    }
    out.flip()
    boundary {
      while (out.hasRemaining) {
        output(state, out.get())
      }
      if (res.isMalformed) {
        state.setMalformed()
      } else if (res.isUnmappable) {
        state.setUnmappable()
      } else {
        assert(res.isUnderflow)
      }
    }

    input.length - in.remaining
  }
}

private[xscala] final class TextJDK extends TextVMDependent {

  // This is an approximation of "proper" native charset (macOS and JDK versions make it a bit weird):
  // In JDK 17+ it is native.encoding or UTF-8.
  // In JDK 16 and earlier it should be sun.jnu.encoding or UTF-8.
  private val nativeCharset = Charset.forName(System.getProperty("native.encoding", System.getProperty("sun.jnu.encoding", "UTF-8")))
  private val stdInCharset = Charset.defaultCharset // JDK doesn't care about stdin encoding, users use stream wrappers (usually with no explicit charset, meaning this charset)
  private val stdOutCharset = {
    val prop = System.getProperty("sun.stdout.encoding")
    if (prop == null) nativeCharset else Charset.forName(prop)
  }
  private val stdErrCharset = {
    val prop = System.getProperty("sun.stderr.encoding")
    if (prop == null) nativeCharset else Charset.forName(prop)
  }

  override def setLocale(category: PlatformEncoding.LocaleCategory, locale: String): String = {
    // JVM manages native locale (sets it to environment locale, aka "")
    if (locale == null) {
      nativeCharset.name()
    } else {
      null
    }
  }

  private def encoding(charset: Charset): Encoding = {
    if (charset == StandardCharsets.UTF_8) Utf8Encoding else TextJDKEncoding(charset)
  }

  override def nativeEncoding(): Encoding = encoding(nativeCharset)

  override def stdInEncoding(): Encoding = encoding(stdInCharset)

  override def stdOutEncoding(): Encoding = encoding(stdOutCharset)

  override def stdErrEncoding(): Encoding = encoding(stdErrCharset)
}
