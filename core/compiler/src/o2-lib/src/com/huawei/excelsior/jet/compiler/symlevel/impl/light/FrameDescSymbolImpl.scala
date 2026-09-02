/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.symlevel.FrameDescSymbol
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment._

final class FrameDescSymbolImpl private[light](val o2m: pcOModule.Method) extends SymbolImpl(o2m.getFrameDescriptor) with FrameDescSymbol {
  override def getMethod = methodByO2Object(o2m)

  override def toString = "FrameDescriptorSymbol[" + getMethod + "]"
}
