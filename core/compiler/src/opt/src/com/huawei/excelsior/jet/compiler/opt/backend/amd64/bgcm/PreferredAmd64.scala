/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.amd64.bgcm

import com.huawei.excelsior.jet.assembler.amd64.GPR.RAX
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.preferred.Preferred
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.{ResourceSet, setOf}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait PreferredAmd64 extends Preferred { self: Universe with Preferred with BackEnd =>

  override def preferredLoc(edge: Edge, unPreferred: PreferencesMap, preferred: PreferencesMap): ResourceSet = edge match {
    case CAS.ExpectedValueEdge(_) => setOf(RAX)

    case _ => super.preferredLoc(edge, unPreferred, preferred)
  }

}
