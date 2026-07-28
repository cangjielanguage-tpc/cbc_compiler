/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.patterns

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait GetClass { self: Universe =>

  /** Replace `obj.getClass()` by [[GetClass]] */
  private def replaceGetClass(): Boolean = {
    var changed = false
    for (invoke @ CallMethod(Java.Lang.Object._getClass, _, Seq(obj)) <- all[Call]) {
      replaceByCode(invoke) { GetClass(obj) }
      stats.count(StatsKind.GetClass, "getClass() replaced by node", invoke)
      changed |= true
    }
    changed
  }

  /** Replace `cls.isAssignableFrom(obj.getClass)` by `cls.isInstance(obj)`. */
  private def specializeSimpleGetClassUses(): Boolean = {
    var changed = false
    for (isAssignable @ CallMethod(Java.Lang.Class.isAssignableFrom, _, Seq(cls, GetClass(obj))) <- all[Call]) {
      replaceByCode(isAssignable) { Invoke(Java.Lang.Class.isInstance)(cls, obj) }
      stats.count(StatsKind.GetClass, s"isAssignableFrom replaced by isInstance", isAssignable)
      // getClass should be struck out later by DCE
      changed |= true
    }
    changed
  }

  def optimizeGetClass(): Boolean = {
    var changed = false
    changed |= replaceGetClass()
    changed |= specializeSimpleGetClassUses()
    changed
  }
}

