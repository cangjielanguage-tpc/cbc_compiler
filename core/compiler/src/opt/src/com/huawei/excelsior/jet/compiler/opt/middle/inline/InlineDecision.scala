/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline

/** Ternary decision type: yes, no or do not know. */
private[inline] sealed abstract class InlineDecision {
  def orElse(alternative: => InlineDecision): InlineDecision

  // implement more methods on request...
}

private[inline] final case class Yes(reason: String) extends InlineDecision {
  def orElse(alternative: => InlineDecision) = this
}

private[inline] final case class No(reason: String) extends InlineDecision {
  def orElse(alternative: => InlineDecision) = this
}

private[inline] case object DoNotKnow extends InlineDecision {
  def orElse(alternative: => InlineDecision) = alternative
}
