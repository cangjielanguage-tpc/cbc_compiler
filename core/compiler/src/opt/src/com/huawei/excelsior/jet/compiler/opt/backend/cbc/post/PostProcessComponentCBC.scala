/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc.post

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.BackEndCBC
import com.huawei.excelsior.jet.compiler.opt.backend.post.PostProcessComponent
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

/** CBC-specific post-process actions.
  *
  * @author conwor
  */
trait PostProcessComponentCBC extends PostProcessComponent { self: Universe with BackEndCBC =>

}
