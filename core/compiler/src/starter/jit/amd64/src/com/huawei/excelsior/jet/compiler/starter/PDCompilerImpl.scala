/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.newbaseline.NewBaseline
import com.huawei.excelsior.jet.compiler.starter.JITCompilerProvider.PDCompiler
import com.huawei.excelsior.jet.compiler.wrappers.CompilerWithNativeWrappers

class PDCompilerImpl extends PDCompiler {
  override def getCompiler(env: Environment) = {
    val baseline = new NewBaseline(env, new com.huawei.excelsior.jet.compiler.newbaseline.platforms.PlatformConfigAmd64)
    new CompilerWithNativeWrappers(env, baseline, com.huawei.excelsior.jet.compiler.wrappers.platforms.PlatformConfigAmd64)
  }
}
