/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

final class PrintStream(impl: TextOutput) {
  def print(x: String): Unit = impl.print(x)
  def println(x: String): Unit = impl.println(x)
  def println(): Unit = impl.println()

  def print(x: AnyRef)    : Unit = print(String.valueOf(x))
  def print(x: Boolean)   : Unit = print(String.valueOf(x))
  def print(x: Char)      : Unit = print(String.valueOf(x))
  def print(x: Int)       : Unit = print(String.valueOf(x))
  def print(x: Long)      : Unit = print(String.valueOf(x))
  def print(x: Float)     : Unit = print(String.valueOf(x))
  def print(x: Double)    : Unit = print(String.valueOf(x))

  def println(x: AnyRef)  : Unit = println(String.valueOf(x))
  def println(x: Boolean) : Unit = println(String.valueOf(x))
  def println(x: Char)    : Unit = println(String.valueOf(x))
  def println(x: Int)     : Unit = println(String.valueOf(x))
  def println(x: Long)    : Unit = println(String.valueOf(x))
  def println(x: Float)   : Unit = println(String.valueOf(x))
  def println(x: Double)  : Unit = println(String.valueOf(x))
}
