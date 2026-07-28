/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.Width

/** Width aka operand size with Intel-specific names.
  *
  * @author paul
  * @author cypok
  * @author conwor
  */
object IntelWidth {

  /** The zero width. */
  val ZERO = Width.W0

  /** The width of one byte on Intel architecture. */
  val BYTE = Width.W8

  /** The width of one word on Intel architecture. */
  val WORD = Width.W16

  /** The width of one double-word on Intel architecture. */
  val DWORD = Width.W32

  /** The width of one quad-word on Intel architecture. */
  val QWORD = Width.W64

  /** The width of one ten-bytes word on Intel architecture. */
  val TWORD = Width.W80

  /** The width of one octo-word on Intel architecture. */
  val OWORD = Width.W128

  /** The platform-specific width of an address (8 bytes for this architecture). */
  val WPTR = Width.WPTR

  /** The special value for unspecified width. Only for internal use! */
  val NO_WIDTH = Width.WNONE

  /** Returns `true` iff the given width is exactly 8 bytes. */
  def is8(w: Width) = w == QWORD || w == WPTR

  /** Returns `true` iff the given width is either 4 or 8 bytes. */
  def is48(w: Width) = w == DWORD || is8(w)

  /** Returns `true` iff the given width is either 2, 4 or 8 bytes. */
  def is248(w: Width) = w == WORD || is48(w)

  /** Returns Intel-specific name for the given width. */
  def widthToString(w: Width) = w match {
    case Width.W8   => "BYTE"
    case Width.W16  => "WORD"
    case Width.W32  => "DWORD"
    case Width.W64  => "QWORD"
    case Width.W80  => "TWORD"
    case Width.W128 => "OWORD"
    case Width.WPTR => "WPTR"
    case _          => "???"
  }
}
