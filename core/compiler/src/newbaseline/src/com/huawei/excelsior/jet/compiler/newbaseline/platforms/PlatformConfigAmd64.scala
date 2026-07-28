/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.platforms

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.SymbolLinker
import com.huawei.excelsior.jet.compiler.ThunkGeneratorBase
import com.huawei.excelsior.jet.compiler.bytecode.Slots
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.newbaseline.backend.GlobalInfo
import com.huawei.excelsior.jet.compiler.newbaseline.backend.MethodBytecodeGenerator
import com.huawei.excelsior.jet.compiler.newbaseline.backend.amd64.MethodBytecodeGeneratorAmd64
import com.huawei.excelsior.jet.compiler.stubs.amd64.ThunkGeneratorFactoryAmd64

class PlatformConfigAmd64 extends PlatformConfig {
  override def makeMethodBytecodeGenerator(env: Environment, rootInlineContext: InlineContext, slots: Slots, globalInfo: GlobalInfo) = 
    new MethodBytecodeGeneratorAmd64(env, rootInlineContext, slots, globalInfo)

  override def getThunkGenerator(env: Environment, symbolLinker: SymbolLinker) = 
    new ThunkGeneratorFactoryAmd64(env, symbolLinker)
}
