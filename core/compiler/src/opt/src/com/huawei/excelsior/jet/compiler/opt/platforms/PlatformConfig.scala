/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.platforms

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.{IRSandbox, Phase}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline.PGOStaticAnalysisPhase

trait PlatformConfig {
  def front(sandbox: IRSandbox, parent: Universe): Phase
  def back(sandbox: IRSandbox, parent: Universe): Phase
  def comboFrontBack(sandbox: IRSandbox): Phase

  def comboAnalysisForPGO(sandbox: IRSandbox): PGOStaticAnalysisPhase
}
