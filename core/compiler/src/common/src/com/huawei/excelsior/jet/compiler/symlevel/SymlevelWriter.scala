/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.common.XString

/** Encoder to text/bytes for symlevel objects.
  *
  * @author conwor
  * @author cypok
  */
object SymlevelWriter {
  trait StreamWriter {
    def putXString(str: XString): Unit
    def putInt(x: Int): Unit
  }
}

trait SymlevelWriter {
  def tkind(tkind: TypeKind): Unit
  def tpe(`type`: Type): Unit
  def field(field: Field): Unit
  def method(method: Method): Unit
  def constString(constString: ConstString): Unit
  def frameDesc(fd: FrameDescSymbol): Unit
}
