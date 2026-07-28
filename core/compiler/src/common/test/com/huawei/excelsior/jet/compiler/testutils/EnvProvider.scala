/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.testutils

import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg}
import com.huawei.excelsior.jet.compiler.abi.{ABI, Platform}
import com.huawei.excelsior.jet.compiler.abi.amd64.PlatformAmd64
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeEnvironment
import com.huawei.excelsior.jet.compiler.types.{CHA, ReferenceTypes}
import com.huawei.excelsior.jet.compiler.util.Log
import xscala.properties.OS.WINDOWS

trait EnvProvider {
  private var _env: FakeEnvironment = _

  def platformForEnv: Platform[_ <: IReg, _ <: FReg, _ <: ABI[_ <: IReg, _ <: FReg]] = new PlatformAmd64(WINDOWS)

  def resetEnvironment(): Unit = {
    _env = new FakeEnvironment(platformForEnv)
    Log.setEnv(_env)
    ReferenceTypes.setEnvForUnitTests(_env)
    ProjectLogic.setEnvForUnitTests(_env)
  }

  resetEnvironment()

  def env: FakeEnvironment = _env
}
