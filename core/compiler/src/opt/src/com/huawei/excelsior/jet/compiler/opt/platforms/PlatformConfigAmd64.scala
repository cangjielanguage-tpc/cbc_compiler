/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.platforms

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases._
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.BackEndAmd64
import com.huawei.excelsior.jet.compiler.opt.frontend.bytecode.intrinsics.IntrinsicsAmd64
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline.PGOStaticAnalysisPhase
import com.huawei.excelsior.jet.compiler.opt.lowering.amd64.LoweringAmd64
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.ScalesAmd64

class PlatformConfigAmd64 extends PlatformConfig {

  override def front(sandbox: IRSandbox, parent: Universe): Phase =
    new IR(this, sandbox, parent) with FrontOnly with IntrinsicsAmd64 with ScalesAmd64 with PlatformDependentAmd64

  override def back(sandbox: IRSandbox, parent: Universe) =
    new IR(this, sandbox, parent) with BackOnly with LoweringAmd64 with BackEndAmd64 with ScalesAmd64

  override def comboFrontBack(sandbox: IRSandbox): Phase =
    new IR(this, sandbox) with ComboFrontBack with LoweringAmd64 with BackEndAmd64 with IntrinsicsAmd64 with ScalesAmd64


  override def comboAnalysisForPGO(sandbox: IRSandbox): PGOStaticAnalysisPhase =
    new IR(this, sandbox) with PGOStaticAnalysisPhase with ScalesAmd64 with PlatformDependentAmd64
}
