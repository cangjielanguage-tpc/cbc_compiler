/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.platforms

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases._
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.BackEndCBC
import com.huawei.excelsior.jet.compiler.opt.frontend.bytecode.intrinsics.IntrinsicsCBC
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline.PGOStaticAnalysisPhase
import com.huawei.excelsior.jet.compiler.opt.lowering.cbc.LoweringCBC
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.ScalesCBC

class PlatformConfigCBC extends PlatformConfig {

  override def front(sandbox: IRSandbox, parent: Universe): Phase =
    new IR(this, sandbox, parent) with FrontOnly with IntrinsicsCBC with ScalesCBC with PlatformDependentCBC

  override def back(sandbox: IRSandbox, parent: Universe): Phase =
    new IR(this, sandbox, parent) with BackOnly with LoweringCBC with BackEndCBC with ScalesCBC

  override def comboFrontBack(sandbox: IRSandbox): Phase =
    new IR(this, sandbox) with ComboFrontBack with LoweringCBC with BackEndCBC with IntrinsicsCBC with ScalesCBC


  override def comboAnalysisForPGO(sandbox: IRSandbox): PGOStaticAnalysisPhase =
    new IR(this, sandbox) with PGOStaticAnalysisPhase with ScalesCBC with PlatformDependentCBC
}
