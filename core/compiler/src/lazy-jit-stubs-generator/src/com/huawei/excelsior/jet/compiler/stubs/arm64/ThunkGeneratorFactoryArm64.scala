/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.stubs.arm64

import com.huawei.excelsior.jet.assembler.AsmType.I64
import com.huawei.excelsior.jet.assembler.Location.{IReg, mem}
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.{LR, SP}
import com.huawei.excelsior.jet.assembler.{Location, Symbol}
import com.huawei.excelsior.jet.codeemitter.arm64.CodeEmitterArm64
import com.huawei.excelsior.jet.compiler.Env.{isJIT, targetPlatform}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.GenerationContext
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.arm64.GeneratorArm64.rX
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.arm64.{GeneratorArm64, GlobalLocationsArm64}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{Locations, Nodes}
import com.huawei.excelsior.jet.compiler.stubs.{ThunkGenerator, ThunkGeneratorFactory}
import com.huawei.excelsior.jet.compiler.symlevel.{MethodType, Type}
import com.huawei.excelsior.jet.compiler.{Environment, RTSProc, SymbolLinker}

final class ThunkGeneratorFactoryArm64(_env: Environment, symbolLinker: SymbolLinker)
  extends ThunkGeneratorFactory(_env, symbolLinker) {

  override protected def createGenerator(methodType: MethodType, frameDescriptor: Symbol) = {
    val asm = GeneratorArm64.createAssembler()
    val globalLocations = new GlobalLocationsArm64(env, symbolLinker, ThunkGenerator.framePropertiesFor(methodType, frameDescriptor), asm, useFramePointer = false)
    new ThunkGenerator(env, globalLocations) {

      override protected val emit: CodeEmitterArm64 = new CodeEmitterArm64(asm, globalLocations.scratchProvider, symbolLinker, isJIT)

      // allocate 16 bytes and store reg at [SP + 0]
      private def storeToStack(reg: IReg): Unit = {
        // TODO: Replace this ugly solution with a better one.
        emit.addPtr(SP, SP, -16)
        emit.store(mem(I64, SP, 0), reg)
      }

      // load reg from [SP + 0] and shift SP by -16
      private def loadFromStack(reg: IReg): Unit = {
        // TODO: Replace this ugly solution with a better one.
        emit.load(reg, mem(I64, SP, 0))
        emit.addPtr(SP, SP, 16)
      }

      protected def saveParamPassingRegs(param1: IReg, param2: IReg): Unit = {
        emit.pushPair(rX(param2), rX(param1))
        storeToStack(LR)
      }

      protected def restoreParamPassingRegs(param1: IReg, param2: IReg): Unit = {
        loadFromStack(LR)
        emit.popPair(rX(param2), rX(param1))
      }

      override protected def createGeneratorForThunk(locations: Locations, nodes: Nodes) =
        new GeneratorArm64(env, symbolLinker, GenerationContext.forThunk(globalLocations.frame.abi.methodType),
          emit, globalLocations, locations, nodes, null, false)
    }
  }
}

