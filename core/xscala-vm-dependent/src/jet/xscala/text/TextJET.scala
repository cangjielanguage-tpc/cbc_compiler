/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

import scala.annotation.static

private[xscala] final class TextJET extends TextVMDependent {
  override def setLocale(category: PlatformEncoding.LocaleCategory, locale: String): String = TextJET.setLocale0(category.ordinal, USAsciiEncoding.encodeStringThrowing(locale))

  override def nativeEncoding(): Encoding = TextJET.nativeEncoding0().asInstanceOf[Encoding]

  override def stdInEncoding(): Encoding = TextJET.stdInEncoding0().asInstanceOf[Encoding]

  override def stdOutEncoding(): Encoding = TextJET.stdOutEncoding0().asInstanceOf[Encoding]

  override def stdErrEncoding(): Encoding = TextJET.stdErrEncoding0().asInstanceOf[Encoding]
}

def stubImpl(): Nothing = scala.runtime.Scala3RunTime.assertFailed("Should have been replaced")

object LinuxExecutionEncoding extends PlatformEncoding {
  override protected def encodeStep(state: EncodingState, input: Array[Char], output: EncodingOutputByte): Int = stubImpl()

  override protected def decodeStep(state: EncodingState, input: Array[Byte], output: EncodingOutputChar): Int = stubImpl()
}

final class WindowsCodePageEncoding(cp: Int) extends PlatformEncoding {
  override protected def encodeStep(state: EncodingState, input: Array[Char], output: EncodingOutputByte): Int = stubImpl()

  override protected def decodeStep(state: EncodingState, input: Array[Byte], output: EncodingOutputChar): Int = stubImpl()
}

object TextJET {
  @native @static private def setLocale0(category: Int, locale: Array[Byte]): String
  @native @static private def nativeEncoding0(): AnyRef
  @native @static private def stdInEncoding0(): AnyRef
  @native @static private def stdOutEncoding0(): AnyRef
  @native @static private def stdErrEncoding0(): AnyRef

  @static private def utf8Encoding0(): AnyRef = Utf8Encoding
  @static private def linuxExecutionEncoding0(): AnyRef = LinuxExecutionEncoding
  @static private def windowsCodePageEncoding0(cp: Int): AnyRef = WindowsCodePageEncoding(cp)
}
