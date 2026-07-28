/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType => SymClassType}
import com.huawei.excelsior.jet.compiler.types.Approximation.CC

class VMStatesSuite extends CompilerSuite with GlobalNodesBuilder {

  def clinited(ts: SymClassType*) =
    ts.foldLeft(new VMStateApprox()) { (acc, t) => acc.withClinit(t) }

  test("clinited") {
    symA.withClinit(false)
    symB.withClinit(true)
    symC.withClinit(true)
    symCF.withClinit(true)
    symD.withClinit(true)

    clinited(symA) should be (clinited())
    clinited(symB) should not be (clinited())
    clinited(symB) should be (clinited(symA, symB))
    clinited(symCF) should not be (clinited(symC))
    clinited(symC, symCF) should be (clinited(symCF, symC))

    clinited() >= clinited(symB) should be (true)
    clinited(symA) >= clinited(symB) should be (true)
    clinited(symC) >= clinited(symB) should be (false)
    clinited(symB) >= clinited(symC) should be (false)
    clinited(symC) >= clinited(symCF) should be (true)
    clinited(symA) >= clinited(symD) should be (true)

    clinited().isClinited(symObj) should be (true)
    clinited().isClinited(symA) should be (true)
    clinited(symCF).isClinited(symC) should be (true)
  }

  test("clinited super interfaces") {
    symIB.withClinit(true)
    symI.withClinit(true)
    clinited(symIB).isClinited(symI) should be (false) // current implementation
  }

  test("VMStateApprox compare") {
    symA.withClinit(true)
    symB.withClinit(true)
    symC.withClinit(true)

    (clinited(symA) compare clinited(symA)) should be (CC.Equal)
    (clinited(symA) compare clinited(symB)) should be (CC.Greater)
    (clinited(symB) compare clinited(symA)) should be (CC.Less)
    (clinited(symB) compare clinited(symC)) should be (CC.PartiallyEqual)
  }
}
