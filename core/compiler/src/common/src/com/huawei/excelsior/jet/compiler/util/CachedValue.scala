/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.util

class CachedValue[X >: Null <: AnyRef](compute: () => X) {
  private var value: X = _

  def evaluated(): Boolean = value ne null

  def get(): X = {
    if (!evaluated()) {
      value = compute()
      assert(evaluated())
    }
    value
  }

  def invalidate(): Unit = value = null
}
