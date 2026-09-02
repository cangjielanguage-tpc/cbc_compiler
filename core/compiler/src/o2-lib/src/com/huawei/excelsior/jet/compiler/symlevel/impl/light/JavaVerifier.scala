/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import xscala.util.Feature
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule

object JavaVerifier extends Feature[JavaVerifier]

trait JavaVerifier {
  def markToVerify(_class: pcOModule.Class): Unit
  def verify(_class: pcOModule.Class): Unit
}
