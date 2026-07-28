/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.newbaseline.backend.arm64

import com.huawei.excelsior.jet.assembler.arm64.Assembler
import com.huawei.excelsior.jet.codeemitter.arm64.CodeEmitterArm64
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.bytecode.Slots
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.newbaseline.backend.{GlobalInfo, MethodBytecodeGenerator}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.GenerationContext
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.Generator
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.arm64.GlobalLocationsArm64
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.arm64.GeneratorArm64
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.Locations
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.Nodes

class MethodBytecodeGeneratorArm64(env: Environment, rootInlineContext: InlineContext, slots: Slots, globalInfo: GlobalInfo)
  extends MethodBytecodeGenerator[Assembler, CodeEmitterArm64](env, rootInlineContext, slots, globalInfo) {

  override protected def createAssembler() = GeneratorArm64.createAssembler()

  override protected def createCodeEmitter() = {
    new CodeEmitterArm64(asm, globalLocations.scratchProvider, symbolLinker, Env.isJIT)
  }

  override protected def createGlobalLocations() = {
    new GlobalLocationsArm64(env, symbolLinker, rootMethod, asm, useFramePointer = false)
  }

  override protected def createGenerator(locations: Locations, nodes: Nodes, xSiteCreator: Generator.XSiteCreator) = {
    new GeneratorArm64(env, symbolLinker, GenerationContext.forMethod(rootInlineContext), emit, globalLocations, locations, nodes, xSiteCreator,
      // baselined methods are not critical for performance
      _enableOptimizedEnrichGeneration = false)
  }
}
