/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.types.References.ReferenceApprox

private[middle] trait FieldsAnalysis { self: Universe =>

  protected def verifyTypeOfSafeAnalysisIsBetterThanFieldsTypeAnalysis(safeType: ReferenceApprox, typeAnalysisType: ReferenceApprox): Unit = {
    // In some cases clinit analysis may produce nullable type (i.e. in "dirty" clinit). So ignore nullability.
    val safe = safeType.withoutNull
    val widened = typeAnalysisType.withoutNull
    assert(widened >= safe)
    assert(widened.hasRefinedProbableType)
    // Usually probable parts are equal, but in some rare cases widenedType may be quite conservative.
    assert(widened.probableType >= safe.probableType)
  }

}
