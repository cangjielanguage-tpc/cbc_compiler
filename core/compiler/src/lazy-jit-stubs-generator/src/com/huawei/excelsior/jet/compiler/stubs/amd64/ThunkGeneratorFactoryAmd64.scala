/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.stubs.amd64

import com.huawei.excelsior.jet.assembler.Location.IReg
import com.huawei.excelsior.jet.assembler.amd64.GPR
import com.huawei.excelsior.jet.assembler.{Location, Symbol}
import com.huawei.excelsior.jet.codeemitter.amd64.CodeEmitterAmd64
import com.huawei.excelsior.jet.compiler.Env.targetPlatform
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.GenerationContext
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.amd64.GeneratorAmd64.{createAssembler, r}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.amd64.{GeneratorAmd64, GlobalLocationsAmd64}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{Locations, Nodes}
import com.huawei.excelsior.jet.compiler.stubs.{ThunkGenerator, ThunkGeneratorFactory}
import com.huawei.excelsior.jet.compiler.symlevel.{MethodType, Type}
import com.huawei.excelsior.jet.compiler.{Environment, RTSProc, SymbolLinker}

final class ThunkGeneratorFactoryAmd64(_env: Environment, symbolLinker: SymbolLinker)
  extends ThunkGeneratorFactory(_env, symbolLinker) {

  override protected def createGenerator(methodType: MethodType, frameDescriptor: Symbol) = {
    val asm = createAssembler()
    val globalLocations = new GlobalLocationsAmd64(env, symbolLinker, ThunkGenerator.framePropertiesFor(methodType, frameDescriptor), asm, Array[GPR](GPR.RAX), useFramePointer = false)
    new ThunkGenerator(env, globalLocations) {

      override protected val emit: CodeEmitterAmd64 = new CodeEmitterAmd64(asm, globalLocations.scratchProvider, symbolLinker)

      protected def saveParamPassingRegs(param1: IReg, param2: IReg): Unit = {
        emit.asm.push(r(param1))
        emit.asm.push(r(param2))
        emit.asm.push(GPR.RAX) // alignment
      }

      protected def restoreParamPassingRegs(param1: IReg, param2: IReg): Unit = {
        emit.asm.pop(r(param2)) // alignment
        emit.asm.pop(r(param2))
        emit.asm.pop(r(param1))
      }

      override protected def createGeneratorForThunk(locations: Locations, nodes: Nodes) =
        new GeneratorAmd64(env, symbolLinker, GenerationContext.forThunk(globalLocations.frame.abi.methodType),
          emit, globalLocations, locations, nodes, null, false)
    }
  }
}

