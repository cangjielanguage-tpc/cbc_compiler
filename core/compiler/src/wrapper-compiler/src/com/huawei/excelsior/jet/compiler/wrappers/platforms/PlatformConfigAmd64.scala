/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.wrappers.platforms

import com.huawei.excelsior.jet.codeemitter.amd64.CodeEmitterAmd64
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.GenerationContext
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.amd64.{GeneratorAmd64, GlobalLocationsAmd64}
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.wrappers.GeneratorContext

object PlatformConfigAmd64 extends PlatformConfig {
  override def makeGeneratorContext(_env: Environment, _wrapper: Method, useFramePointer: Boolean) = new GeneratorContext(_env, _wrapper) {
    override protected def init(): Unit = {
      val asm = GeneratorAmd64.createAssembler()
      val globalLocations = new GlobalLocationsAmd64(env, symbolLinker, wrapper, asm, useFramePointer)
      emit = new CodeEmitterAmd64(asm, globalLocations.scratchProvider, symbolLinker)

      initGeneration0(globalLocations)

      val ctx = GenerationContext.forMethod(InlineContext.newRoot(wrapper))
      gen = new GeneratorAmd64(env, symbolLinker, ctx, emit, globalLocations, locations, nodes, xSiteCreator,
        // code of native wrappers and call to managed wrappers may be critical for performance
        _enableOptimizedEnrichGeneration = true)
    }
  }
}
