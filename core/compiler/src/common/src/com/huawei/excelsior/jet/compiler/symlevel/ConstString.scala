/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.common.XString

/** Some constant string.
  *
  * @author cypok
  * @author conwor
  * @author paul
  */
trait ConstString {
  def value: XString
  def getHost: Type

  def getStringTable: StringTableSymbol
  def getStringNumber: Int
}
