/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.amd64

import com.huawei.excelsior.jet.assembler.amd64.{Assembler, GPR}
import com.huawei.excelsior.jet.compiler.{Environment, SymbolLinker}
import com.huawei.excelsior.jet.compiler.abi.FrameProperties
import com.huawei.excelsior.jet.compiler.abi.amd64.FrameAmd64
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.GlobalLocations
import com.huawei.excelsior.jet.compiler.options.BoolOption.UseFramePointer

final class GlobalLocationsAmd64(env: Environment, symbolLinker: SymbolLinker, properties: FrameProperties, asm: Assembler,
                                 override val scratches: Array[GPR], useFramePointer: Boolean)
  extends GlobalLocations(new FrameAmd64(asm, env, symbolLinker, properties, useFramePointer || env.enabled(UseFramePointer), useSPAddressing = false)) {

  def this(env: Environment, symbolLinker: SymbolLinker, properties: FrameProperties, asm: Assembler, useFramePointer: Boolean) =
    this(env, symbolLinker, properties, asm, Array[GPR](GPR.RAX), useFramePointer)
}
