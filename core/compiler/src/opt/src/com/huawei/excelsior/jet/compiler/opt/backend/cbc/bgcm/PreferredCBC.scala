/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc.bgcm


import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.preferred.Preferred
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.BackEndCBC
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.emptySet
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait PreferredCBC extends Preferred { self: Universe with Preferred with BackEndCBC =>
  
}
