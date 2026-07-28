/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.entries

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.debug.info.*
import com.huawei.excelsior.jet.compiler.symlevel.Member

import scala.annotation.tailrec

/** Suitable for all langs box of magnificent tools around names: public, linkage, simple, ...
  *
  * @author conwor
  * @author gatimosh
  * @author orangebyte256
  */
object CommonToolbox {
  private val PUBNAME_SEPARATOR = XString("::")

  def constructPubName(parts: XString*): XString = parts.reduce((x, y) => x.concat(PUBNAME_SEPARATOR).concat(y))

  def memberSourceName(member: Member): XString = {
    val sourceName = member.getSourceName
    if (sourceName != null) sourceName else member.getXName
  }

  @tailrec
  def unwrapDebugType(debugType: DebugType): DebugType = debugType match {
    case DTConst(baseType) => unwrapDebugType(baseType)
    case DTPointer(baseType) => unwrapDebugType(baseType)
    case x => x
  }

}