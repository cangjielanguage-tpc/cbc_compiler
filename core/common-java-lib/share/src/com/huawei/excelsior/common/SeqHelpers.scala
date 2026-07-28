/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

/** Set of helper methods over [[Seq]] to use from Java code.
  *
  * TODO-DECAF: Remove when all uses are translated into Scala.
  */
object SeqHelpers {
  def flatten[T](xs: collection.Seq[collection.Seq[T]]) = xs.flatten
}
