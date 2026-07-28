/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.jet.compiler.CompilerSuite

class CHASuite extends CompilerSuite with TypesToolbox {

  test("simple") {
    resetCHA(tIBB, tJB, tCF, tD)

    CHA.subClasses(tObj).toSet should be (Set(tA, tD))
    CHA.subClasses(tA).toSet should be (Set(tB, tC))
    CHA.subClasses(tB).toSet should be (Set(tIB, tJB))
    CHA.subClasses(tD).toSet should be (Set())

    CHA.implClasses(tI).toSet should be (Set(tIB, tJB))
    CHA.implClasses(tJ).toSet should be (Set(tJB))

    CHA.maxClassHeight(tObj) should be (5)
    CHA.maxClassHeight(tD) should be (1)
  }

}
