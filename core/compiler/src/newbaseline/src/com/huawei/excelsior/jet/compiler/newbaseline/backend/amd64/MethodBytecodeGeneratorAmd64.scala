/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.newbaseline.backend.amd64

import com.huawei.excelsior.jet.assembler.amd64.Assembler
import com.huawei.excelsior.jet.codeemitter.amd64.CodeEmitterAmd64
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.bytecode.Slots
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.newbaseline.backend.{GlobalInfo, MethodBytecodeGenerator}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.GenerationContext
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.Generator
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.amd64.GlobalLocationsAmd64
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.amd64.GeneratorAmd64
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.Locations
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.Nodes

class MethodBytecodeGeneratorAmd64(env: Environment, rootInlineContext: InlineContext, slots: Slots, globalInfo: GlobalInfo)
  extends MethodBytecodeGenerator[Assembler, CodeEmitterAmd64](env, rootInlineContext, slots, globalInfo) {

  override protected def createAssembler() = GeneratorAmd64.createAssembler()

  override protected def createCodeEmitter() = {
    new CodeEmitterAmd64(asm, globalLocations.scratchProvider, symbolLinker)
  }

  override protected def createGlobalLocations() = {
    new GlobalLocationsAmd64(env, symbolLinker, rootMethod, asm, useFramePointer = false)
  }

  override protected def createGenerator(locations: Locations, nodes: Nodes, xSiteCreator: Generator.XSiteCreator) = {
    new GeneratorAmd64(env, symbolLinker, GenerationContext.forMethod(rootInlineContext), emit, globalLocations, locations, nodes, xSiteCreator,
      // baselined methods are not critical for performance
      _enableOptimizedEnrichGeneration = false)
  }
}
