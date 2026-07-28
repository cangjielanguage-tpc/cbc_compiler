/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.symlevel.Field
import com.huawei.excelsior.jet.compiler.symlevel.MethodType
import com.huawei.excelsior.jet.compiler.symlevel.Type

trait ThunkGeneratorBase {
  def genNonVirtualForwarder(target: Symbol, methodType: MethodType, receiverNullCheck: Boolean): Segment

  def genVirtualForwarder(refClass: Type, vnum: Int, methodType: MethodType, isInvokeInterface: Boolean, receiverNullCheck: Boolean): Segment

  def genFieldOperation(field: Field, isWrite: Boolean, receiverNullCheck: Boolean): Segment

  def genJSR292AppendixPlacer(methodType: MethodType, dai: Symbol, jumpTarget: Symbol): Segment
}

object ThunkGeneratorBase {
  class StubGenerationResult(val segment: Segment, val paramSize: Int, val frameSize: Int, val savedRegsBitMap: Int)
}
