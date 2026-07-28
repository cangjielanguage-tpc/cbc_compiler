/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.cbc.frame

import com.huawei.excelsior.jet.compiler.abi.cbc.FrameCBC
import com.huawei.excelsior.jet.compiler.abi.frame.FrameDebug

trait FrameDebugCBC extends FrameDebug { self: FrameCBC =>
  override protected def initCallerFrameInfo(): Unit = {} // TODO-CBC used at prologue creation
  override protected def updateCallerFrameInfo(spAddend: Int): Unit = {} // TODO-CBC used at addStackPointer
}
