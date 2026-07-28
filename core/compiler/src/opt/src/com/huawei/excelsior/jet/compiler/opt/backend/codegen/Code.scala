/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.assembler.cbc.ExceptionTable
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector.LiveState
import com.huawei.excelsior.jet.compiler.ir.{MarkedRegion, XInfo}

trait Code {
  def segment: Segment
  def xinfo: XInfo
}

case class CodeMach(segment: Segment, xinfo: XInfo, markedRegions: Seq[MarkedRegion], siberiaOffset: Int) extends Code
case class CodeCBC(segment: Segment, xinfo: XInfo, exTable: ExceptionTable, liveness: Seq[LiveState]) extends Code