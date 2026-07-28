/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.opt

import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment
import com.huawei.excelsior.jet.compiler.{CompilerEnvironment, Stats}

object O2Env extends CompilerEnvironment {
  def env: LightweightEnvironment = LightweightEnvironment.getInstance
  lazy val statsGlobal = new Stats(env)
}
