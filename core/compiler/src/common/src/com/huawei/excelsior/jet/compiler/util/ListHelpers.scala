/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.util

/** Set of helper methods over [[List]] to use from Java code.
  *
  * TODO-DECAF: Remove when all uses are translated into Scala.
  */
object ListHelpers {
  def empty[T]: List[T] = List.empty
  def single[T](elem: T): List[T] = List(elem)
  def prepended[T](elem: T, xs: List[T]): List[T] = elem :: xs
  def tail[T](xs: List[T]): List[T] = xs.tail
}
