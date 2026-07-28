/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.compiler.o2lib.opt.VZCModule
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule, opAttrsModule}
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.options.BoolOption.{AOTCPStats, SilentCompilation}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{LightweightEnvironment, VersionedMethod}

class LightweightCompiler(compiler: Compiler) extends VZCModule.CompilerInterface {
  val env = LightweightEnvironment.getInstance

  override def enterClass(_class: pcOModule.Class, stage: Pass): Unit = {
    assert(opAttrsModule.currClass eq _class)
    env.getO2Env.enterClass(_class)
    env.setPass(stage)
  }

  override def exitClass(_class: pcOModule.Class): Unit = {
    env.getO2Env.exitClass()
    _class.classInfo = null
  }

  override def compileMethod(m: pcOModule.Method, versioned: VersionedMethod): Unit = {
    opAttrsModule.currProc = m
    if (versioned != null) {
      assert(versioned.original eq m)
      compiler.genCode(versioned)
    } else {
      compiler.genCode(CodeUnit.of(env.fromO2(m)))
    }
    opAttrsModule.currProc = null
  }

  override def printFinalStatistics(): Unit = {
    compiler.printFinalStatistics()
    if (!env.enabled(SilentCompilation) && env.enabled(AOTCPStats)) {
      println(s"Total constant pool size: ${TypeMetaInfoGenerator.AOTConstantPool.CPSizeTotal / 1024}")
    }
  }
}
