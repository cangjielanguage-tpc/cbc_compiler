/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic

trait EagerPreparationChecksElimination { self: Universe =>

  def eliminateEagerPreparationChecks(): Boolean = {
    // Note: PreparationChecks must be serialized in order to end up in the correct hosting class
    if (!ProjectLogic.useLazyPreparation && (currentPhase >= CompilerPhase.PostInline)) {
      var changed = false
      val cbc = targetArch == CBC
      for (check <- all[PreparationCheck] if cbc || !check.kind.`lazy` && !check.kind.forced) { // TODO-CBC strike out prep check even if it is lazy for CBC
        if (!cbc) PreparationCheck.markForPreparation(check)
        strikeOut(check)
        changed = true
      }
      changed

    } else {
      false
    }
  }
}
