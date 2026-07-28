/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.patterns

import com.huawei.excelsior.jet.compiler.options.BoolOption._
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.ExplicitNullCheckFolding

trait BytecodePatterns extends StrConcat with KeyStrings with Arrays with UnsafeOffsets with Boxing with ScalaBoxing with GetClass with ExplicitNullCheckFolding { self: Universe =>
  private def optimize(name: String, condition: Boolean, result: => Boolean): Unit = {
    if (condition && result) dbgPrinter.debugNodes(name)
  }

  def optimizeBytecodePatternsO1(): Unit = {
    // Required, because xscala.Class.getDeclaredField is not implemented and its call must be replaced
    optimize("all graph compute unsafe field offsets",               true,                                                            computeUnsafeOffsets())
  }

  def optimizeBytecodePatternsO2(): Unit = {
    optimize("all graph str concat optimizing",                      env.enabled(DecompileStrConcat),                                 optimizeStrConcat())
    optimize("all graph after arrays copyOf optimizing",             !env.enabled(NoNewArrayCopy),                                    optimizeArraysCopyOf())
    optimize("all graph replace new key strings",                    !env.enabled(NoKeyStrings),                                      replaceNewKeyStrings())
    optimize("all graph replace key string alloc",                   !env.enabled(NoKeyStrings) && env.enabled(SpecializeKeyStrings), replaceKeyStringAlloc())
    optimize("all graph replace boxing operations",                  env.enabled(BoxingExplosion),                                    foldBoxedValues())
    optimize("all graph scala boxing optimization",                  env.enabled(ScalaBoxingOptimization),                            optimizeScalaBoxing())
    optimize("all graph after simple get class uses specialization", env.enabled(SpecializeSimpleGetClassUses),                       optimizeGetClass())
    optimize("all graph compute unsafe field offsets",               true,                                                            computeUnsafeOffsets())
    optimize("all graph require non null optimizing",                true,                                                            foldExplicitNullChecks())
  }
}
