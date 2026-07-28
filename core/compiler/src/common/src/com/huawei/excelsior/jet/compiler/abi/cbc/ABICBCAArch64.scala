/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.cbc

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotCallThis}
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.abi.ABI.{ParamsQueue, RetType}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType
import xscala.util.MathUtils

class ABICBCAArch64 private[cbc](_methodType: MethodType) extends ABI[IR, FR](_methodType, CallingConventionCBCAArch64(_methodType)) {

  override protected lazy val resultRegsImpl: Array[AnyReg] = RetType(methodType) match {
    case RetType.I | RetType.L => Array(cc.baseIRegs.headArea(0))
    case RetType.F | RetType.D => Array(cc.baseFRegs.headArea(0))
    case RetType.Void => Array.empty[AnyReg]
  }

  override protected def initLocations(iRegsQueue: ParamsQueue[IR], fRegsQueue: ParamsQueue[FR], _limit: Int) = {
    assert(cc.isJET && !isVarArgs && !hasAltLocationParametersOrResult)

    val locations = new Array[Location](parameterCount)
    var currTailSize = 0

    for (i <- locations.indices) {
      val kind = parameterType(i).symKindErased

      locations(i) = if (methodType.hasRetByValParameter && methodType.getRetByValArgIdx == i) {
        CallingConventionCBCAArch64.indirectResultLocation
      } else if (kind.isFloatingPoint && fRegsQueue.hasNext) {
        fRegsQueue.next()
      } else if (!kind.isFloatingPoint && iRegsQueue.hasNext) {
        iRegsQueue.next()
      } else {
        val (slot, newTailSize) = makeTailSlot(i, kind, currTailSize)
        currTailSize = newTailSize
        slot
      }
    }

    val tailSize = if (currTailSize > 0) {
      Some(currTailSize)
    } else {
      None
    }

    (locations, tailSize)
  }

  override def savedIRegsOrder = shouldNotCallThis()
  override def savedFRegsOrder = shouldNotCallThis()

  override def stackParamsStartOffset = 0 // Tail is not bound to SP in CBC, so this offset is irrelevant.

  override def callerFrameTopMayBeUsed = false // JIT-generated code of the caller will repush parameters if needed.

  override def updateRegMaskForGCMap(regMask: Int, r: IR) = {
    MathUtils.setBit(regMask, r.idx)
  }

}
