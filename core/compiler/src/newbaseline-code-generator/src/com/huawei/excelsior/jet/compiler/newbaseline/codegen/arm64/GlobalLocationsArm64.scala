/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.arm64

import com.huawei.excelsior.jet.assembler.arm64.Assembler
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X
import com.huawei.excelsior.jet.compiler.abi.FrameProperties
import com.huawei.excelsior.jet.compiler.abi.arm64.FrameArm64
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.GlobalLocations
import com.huawei.excelsior.jet.compiler.options.BoolOption.UseFramePointer
import com.huawei.excelsior.jet.compiler.{Environment, SymbolLinker}

final class GlobalLocationsArm64(env: Environment, symbolLinker: SymbolLinker, properties: FrameProperties, asm: Assembler,
                                 override val scratches: Array[X], useFramePointer: Boolean)
  extends GlobalLocations(new FrameArm64(asm, env, symbolLinker, properties, useFramePointer || env.enabled(UseFramePointer), useSPAddressing = false)) {

  def this(env: Environment, symbolLinker: SymbolLinker, properties: FrameProperties, asm: Assembler, useFramePointer: Boolean) =
    this(env, symbolLinker, properties, asm, Array(X.IP0, X.IP1), useFramePointer)
}
