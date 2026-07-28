/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

/** Encodings, implemented using OS string conversion utilities. Provide noticeably worse guarantees.
  * For example on Windows, any unfortunate byte sequence in the input buffer might lead to full conversion failure.
  * Iconv on Linux works a bit better, but is severely underdefined in POSIX standard,
  * and might depend on current execution locale (setlocale).
  */
abstract class PlatformEncoding extends Encoding {
  override final def encodeStep(state: EncodingState, input: EncodingInputCharIterator, output: EncodingOutputByte): Unit = {
    val array = input.peekIterator.toArray
    if (array.isEmpty) {
      state.setUnderflow()
      return
    }

    val consumed = encodeStep(state, array, output)
    for (i <- 0 until consumed) {
      input.assertNext(array(i))
    }
  }

  override final def decodeStep(state: EncodingState, input: EncodingInputByteIterator, output: EncodingOutputChar): Unit = {
    val array = input.peekIterator.toArray
    if (array.isEmpty) {
      state.setUnderflow()
      return
    }

    val consumed = decodeStep(state, array, output)
    for (i <- 0 until consumed) {
      input.assertNext(array(i))
    }
  }

  protected def encodeStep(state: EncodingState, input: Array[Char], output: EncodingOutputByte): Int

  protected def decodeStep(state: EncodingState, input: Array[Byte], output: EncodingOutputChar): Int
}

object PlatformEncoding {
  @volatile private var _native: Encoding = _
  @volatile private var _stdIn: Encoding = _
  @volatile private var _stdOut: Encoding = _
  @volatile private var _stdErr: Encoding = _

  /** Encoding specified by `sun.jnu.encoding` property.
    * Aliased as `native.encoding` in Java 17.
    * Returned by [[java.nio.charset.Charset#defaultCharset]] in Java 17 and earlier versions (unless overriden using `-Dfile.encoding`), JDK 18+ uses UTF-8 unless overriden.
    * Do not use for console I/O.
    */
  def native: Encoding = {
    if (_native == null) {
      _native = TextVMDependent.get.nativeEncoding()
    }
    _native
  }

  def stdIn: Encoding = {
    if (_stdIn == null) {
      _stdIn = TextVMDependent.get.stdInEncoding()
    }
    _stdIn
  }

  def stdOut: Encoding = {
    if (_stdOut == null) {
      _stdOut = TextVMDependent.get.stdOutEncoding()
    }
    _stdOut
  }

  def stdErr: Encoding = {
    if (_stdErr == null) {
      _stdErr = TextVMDependent.get.stdErrEncoding()
    }
    _stdErr
  }

  enum LocaleCategory {
    /** `LC_ALL` */
    case All
    /** `LC_CTYPE` */
    case Charset
  }

  /** API to query and manipulate native execution locale.
    *
    * @param category Parts of charset to query or modify
    * @param locale   A new locale name, or `null` for query-only
    * @return Currently effective execution locale, or `null` on failure.
    */
  def setLocale(category: LocaleCategory, locale: String): String = TextVMDependent.get.setLocale(category, locale)

}
