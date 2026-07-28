/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.aliases

import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait AliasAnalysis { self: Universe =>

  /** Returns whether `x` & `y` may point to the same object at given point. */
  def nodesMayAliasAt(x: Node, y: Node, point: ControlNode): Boolean = {
    // More sophisticated methods might be used.
    !(nodeTypeAt(x, point).withoutNull incomparable nodeTypeAt(y, point).withoutNull)
  }
}
