/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc.codegen

import com.huawei.excelsior.jet.compiler.opt.backend.cbc.BackEndCBC
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.FrameComponentCBC.FrameSlotCBC
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait DebugGeneratorCBC { self: Universe with BackEndCBC =>

  trait DebugGeneratorImplCBC extends DebugGeneratorImpl { self: CodeGeneratorImplCBC =>

    override protected def locationOfDebugSlot(slot: FrameSlot): Any =
      slot.asInstanceOf[FrameSlotCBC].local.encoding

    override def genCoverageCounter(locs: Array[(String, Array[Int])]): Unit = cbc.covinc(locs)
  }
}