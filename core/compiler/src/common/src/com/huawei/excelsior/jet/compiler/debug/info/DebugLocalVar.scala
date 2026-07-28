/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.info

import com.huawei.excelsior.jet.common.XString

case class DebugLocalVar(name: XString, varType: DebugType, argIndex: Int, isPointer: Boolean, declaration: Option[DebugDeclaration]) {
  def isArgument = argIndex >= 0
}