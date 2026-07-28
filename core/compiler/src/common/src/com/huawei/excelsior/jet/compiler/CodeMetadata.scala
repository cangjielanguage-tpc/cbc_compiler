/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.compiler.abi.Frame
import xscala.io.ByteBuffer

class CodeMetadata(val xTable: ByteBuffer, val trivXHandler: Boolean, val dirtyForClassGCFrame: Boolean,
                   val hasMarkedRegions: Boolean, val siberiaOffset: Int, frame: Frame[?, ?, ?]) {

  val frameSize = if (frame == null) 0 else frame.frameSize
  val savedIRegsBitMap = if (frame == null) 0 else frame.getSavedIRegsBitMap
  val savedFRegsBitMap = if (frame == null) 0 else frame.getSavedFRegsBitMap
  val frameIsLightweight = frame == null || !frame.isFull
}
