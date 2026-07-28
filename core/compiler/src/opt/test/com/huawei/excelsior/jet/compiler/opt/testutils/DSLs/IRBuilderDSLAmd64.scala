/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.testutils.DSLs

import com.huawei.excelsior.jet.compiler.abi.amd64.PlatformAmd64
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.ScalesAmd64
import com.huawei.excelsior.jet.compiler.opt.platforms.{PlatformConfigAmd64, PlatformDependentAmd64}
import xscala.properties.OS.WINDOWS

trait IRBuilderDSLAmd64 extends IRBuilderDSLBase with PlatformDependentAmd64 with ScalesAmd64 {
  override def platformForEnv = new PlatformAmd64(WINDOWS)
  def platformConfig = new PlatformConfigAmd64
}
