/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline

import com.huawei.excelsior.jet.compiler.{CodeUnit, Stats}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases._
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline.PGOStaticAnalysisPhase.AnalysisResults
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.Method
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.platforms.PlatformConfig
import com.huawei.excelsior.jet.compiler.opt.Opt
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.util.Maps

/**
  * Gather knowledge about methods via on-demand compilation.
  * Serialized representation of compiled methods will be reused later during code generation.
  *
  * @author ijorch
  */
private[blame] class StaticAnalysis(platformConfig: PlatformConfig,
                                    stats: Stats) {

  def apply(method: Method): AnalysisResults = if (env.enabled(BoolOption.PGOStaticAnalysis)) {
    methodsAnalysisResults(method)
  } else {
    AnalysisResults.empty
  }

  private val methodsAnalysisResults = Maps[Method].newMMap[AnalysisResults] withDefault analyze

  /** Perform static analysis of given method, updates analysis map. */
  private def analyze(m: Method): AnalysisResults = {
    val method = m.toSymlevel(env)
    if (method != null) {
      env.reportStatus("Analysing", m.toString)

      new IRSandbox(CodeUnit.of(method), env, stats).run { sandbox =>
        // TODO: front & analyzer phases may be combined into one without deserialization
        if (Opt.passFront(method, env, platformConfig, stats, null)) {
          methodsAnalysisResults(m) = platformConfig.comboAnalysisForPGO(sandbox).runAnalysis(m)
        }
      }
    }

    methodsAnalysisResults.getOrElseUpdate(m, AnalysisResults.empty)
  }
}
