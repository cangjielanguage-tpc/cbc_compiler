/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.vm

/** VM-dependent interface.
  *
  * Provides a general interface for dispatching required functionality
  * depending on VM configuration.
  *
  * @see [[xscala.util.Feature]]
  */
trait VMDependent[T >: Null <: AnyRef] {
  private var impl: T = _

  def :=(impl: T): Unit = this.impl = impl
  def get: T = {
    val impl = this.impl
    if (impl eq null) getSlow else impl
  }
  def apply[S](action: T => S): S = action(get)

  private final def getSlow: T = {
    VMConfig.init()
    val impl2 = this.impl
    if (impl2 eq null) {
      throw new AssertionError("VMDependent instance is not set in VMConfig")
    } else {
      impl2
    }
  }
}
