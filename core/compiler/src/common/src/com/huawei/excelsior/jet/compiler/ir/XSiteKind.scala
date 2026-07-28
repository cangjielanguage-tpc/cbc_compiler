/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

/* Kind of exception site. */
enum XSiteKind {
  case CALL
  case DEFERRED_CALL
  case PRE_CALL
  case PRE_CALL_WITH_NULLCHECK
  case NULLCHECK
  case DIV
  case DIV_WITH_CHECK
  case GCPOINT
  case SOFT_EXCEPTION

  def isCall = this == CALL || this == DEFERRED_CALL

  def isPreCall = this == PRE_CALL || this == PRE_CALL_WITH_NULLCHECK

  def needGCMap = this match {
    case CALL | DEFERRED_CALL | PRE_CALL | PRE_CALL_WITH_NULLCHECK | GCPOINT | DIV_WITH_CHECK =>
      true

    case NULLCHECK | DIV | SOFT_EXCEPTION =>
      // TODO: don't we need gcmaps for instructions performing resolve?
      // Current implementation intentionally omits GC maps for all hardware exceptions (see JET-8756)
      false
  }

  def needSeparateRegion = this match {
    case NULLCHECK | DIV | PRE_CALL_WITH_NULLCHECK => true
    case _ => false
  }
}
