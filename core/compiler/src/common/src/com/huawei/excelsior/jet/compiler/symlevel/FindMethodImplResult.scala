/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

object FindMethodImplResult {
  case class Error(result: MethodSearchError) extends FindMethodImplResult
  case class Found(result: Method) extends FindMethodImplResult
}

sealed abstract class FindMethodImplResult {
  def contains(method: Method) = this match {
    case FindMethodImplResult.Found(`method`) => true
    case _ => false
  }
}
