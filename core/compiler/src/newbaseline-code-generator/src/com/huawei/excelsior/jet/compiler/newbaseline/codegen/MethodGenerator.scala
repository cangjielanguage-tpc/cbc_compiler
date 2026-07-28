/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.compiler.{CodeUnit, Environment, RTConst}
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.symlevel.Method

object MethodGenerator {
  def sendMethodCode(env: Environment, method: Method, body: Segment, frame: Frame[_, _, _], xinfo: XInfo): Unit = {
    xinfo.prepare(body)
    env.sendMethodCode(CodeUnit.of(method), body, xinfo, null, RTConst.MethodInfoFrameDescriptor.UNKNOWN_SIBERIA_OFFSET.intValue, frame, frame.gcMapsOffset)
  }
}
