/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.wrappers.platforms

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.wrappers.GeneratorContext

trait PlatformConfig {
  def makeGeneratorContext(env: Environment, wrapper: Method, useFramePointer: Boolean): GeneratorContext
}
