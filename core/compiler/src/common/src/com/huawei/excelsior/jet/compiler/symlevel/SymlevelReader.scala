/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.common.XString

/** Decoder from text/bytes for symlevel objects.
  *
  * @author conwor
  * @author cypok
  */
object SymlevelReader {
  trait StreamReader {
    def nextXString(): XString
    def nextInt(): Int
  }
}

trait SymlevelReader {
  def tkind(): TypeKind
  def tpe(allowAbsenceOfExternalRefs: Boolean = false): Type
  def field(): Field
  def method(): Method
  def constString(): ConstString
  def frameDesc(): FrameDescSymbol
}
