/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

/** Optional feature.
  *
  * Provides a general interface for enabling and accessing functionality
  * that might not be present in current configuration.
  *
  * ==Usage example==
  *
  * Definition of feature `Foo`:
  * {{{
  *   trait Foo { def bar(): Unit }
  *   object Foo extends Feature[Foo]
  * }}}
  *
  * Enable feature with a specialized implementation (e.g. for particular language/arch configuration):
  * {{{
  *   object Config {
  *     def init(): Unit = {
  *       Foo := FooImpl
  *     }
  *   }
  * }}}
  *
  * Usage of the feature:
  * {{{
  *   Foo { _.bar() }
  * }}}
  *
  * Note that the code inside of such application will be executed
  * only if this particular feature is set in current configuration.
  */
trait Feature[T] {
  private var impl: T = _

  def :=(impl: T): Unit = this.impl = impl
  def get: Option[T] = Option(impl)
  def apply[S](action: T => S): Option[S] = get map action
}
