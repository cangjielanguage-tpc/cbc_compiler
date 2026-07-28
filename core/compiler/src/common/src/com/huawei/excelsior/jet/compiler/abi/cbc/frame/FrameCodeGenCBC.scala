/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.cbc.frame

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame}
import com.huawei.excelsior.jet.compiler.abi.cbc.{ABICBC, FrameCBC}
import com.huawei.excelsior.jet.compiler.abi.frame.FrameCodeGen

trait FrameCodeGenCBC extends FrameCodeGen[IR, FR, ABI[IR, FR]] { self: FrameCBC =>

  override val emit = null

  override def genBuildAndAdjustParams(needFrameDescriptor: Boolean): Unit = {}

  override def loadCVarArgsAddrTo(reg: IR, registerSlot: Frame.Slot => Unit): Unit = shouldNotCallThis()

  override protected def buildHeader(): Unit = {}
  override protected def destroyHeaderAndReturn(shouldReturn: Boolean): Unit = {}
  override protected def touchMemory(stackPointerOffset: Int): Unit = {}
  override protected def storeFrameDescriptor(fd: Symbol): Unit = {}
  override protected def allocateAndTouchOnePage(pageSize: Int): Unit = {}
  override protected def saveNonPushableRegs(): Unit = {}
  override protected def restoreNonPushableRegs(): Unit = {}
  override protected def adjustParameter(paramIdx: Int): Unit = {}
}
