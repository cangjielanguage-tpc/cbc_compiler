/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.cbc

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.cbc.Local.LocX
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.compiler.abi.Frame.Mode.FULL
import com.huawei.excelsior.jet.compiler.abi.cbc.frame.{FrameCodeGenCBC, FrameDebugCBC}
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame, FrameProperties}
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.{Environment, SymbolLinker}

object FrameCBC {
  /** Frame slot for CBC, could be untyped (spill-slot) or typed (stack-alloc). */
  case class Slot(local: LocX) extends XInfo.Slot {
    override def order: Int = local.encoding
  }
}

class FrameCBC(_env: Environment, _symbolLinker: SymbolLinker, _properties: FrameProperties, _maxCBCRegsCnt: Int)
  extends Frame[IR, FR, ABI[IR, FR]](_env, _symbolLinker, _properties, useFramePointer = false, useFMRAddressing = false) with FrameCodeGenCBC with FrameDebugCBC {

  mode = FULL

  def newSlot(local: LocX): FrameCBC.Slot = {
    assert(local.encoding >= _maxCBCRegsCnt)
    FrameCBC.Slot(local)
  }

  override protected def preHeaderSize = shouldNotCallThis()

  override protected def fRegsArePushable: Boolean = shouldNotCallThis()

  override protected def framePointerSetupOffset: Int = shouldNotCallThis()

  override def frameSize = 0

  override def hasStackCheck = false

  // TODO: use BitMap of saved registers instead of saved register count
  override def getSavedIRegsBitMap = 0
  override def getSavedFRegsBitMap = 0
  def savedRegsIterator: Iterator[Location.AnyReg] = savedRegs.iterator
}
