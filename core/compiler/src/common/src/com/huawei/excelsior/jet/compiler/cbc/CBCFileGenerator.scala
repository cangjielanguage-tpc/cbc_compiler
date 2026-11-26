/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.assembler.cbc.{CbcFileFormat, ExceptionTable, SourceCodeInfo}
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector.LiveState
import com.huawei.excelsior.jet.compiler.Env.isStandalone
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.abi.XTableGenerator
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator.GenerationTarget
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator.GenerationTarget.CBC
import com.huawei.excelsior.jet.compiler.symlevel.{Method, SignatureType}
import xscala.io.Path

object CBCFileGenerator {

  enum GenerationTarget {
    case CBC, EXE, STDLIB
  }

  val coldStringsForWorkersOut = "cold_strings.pdb"

  private var _env: Environment = _
  def env = _env
  def env_=(newEnv: Environment): Unit = this._env = newEnv

  def generate(builder: CbcFileFormat.Builder, output: Path, generationTarget: GenerationTarget = CBC): Unit = {
    CbcFileEncoderAdapter.generate(builder, output, generationTarget)
  }

  def sendCode = CbcFileEncoderAdapter.sendCode
}