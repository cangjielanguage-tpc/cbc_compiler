/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.amd64

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.assembler.amd64.{GPR, Register8, XMM}
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.XMM.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.abi.ABI.{AltLocation, ParamsQueue, RetType}
import com.huawei.excelsior.jet.compiler.abi.amd64.CallingConventionAmd64.windowsShadowSpaceSize
import com.huawei.excelsior.jet.compiler.symlevel.MethodType

import scala.annotation.nowarn

/** ABI implementation for amd64. Each ABI instance holds all information
  * about parameter displacement for particular native calling convention.
  *
  * @author cypok
  * @author paul
  * @author conwor
  * @author shell
  */
object ABIAmd64 {
  val UNIX_VARARG_XMMS_COUNT_REG = Register8.AL
}

class ABIAmd64 private[amd64](_methodType: MethodType) extends ABI[GPR, XMM](_methodType, CallingConventionAmd64(_methodType)) {

  def isWindowsNative = !cc.isJET && targetOS.isWindows

  @nowarn("msg=match may not be exhaustive")
  override protected lazy val resultRegsImpl: Array[AnyReg] = (cc.isJET, RetType(methodType)) match {
    case (true,  RetType.I | RetType.L) => Array(cc.baseIRegs.headArea(0))
    case (false, RetType.I | RetType.L) => Array(RAX)
    case (true,  RetType.F | RetType.D) => Array(cc.baseFRegs.headArea(0))
    case (false, RetType.F | RetType.D) => Array(XMM0)
    case (_, RetType.Void) => Array.empty[AnyReg]
  }

  protected def initLocations(iRegsQueue: ParamsQueue[GPR], fRegsQueue: ParamsQueue[XMM], limit: Int): (Array[Location], Option[Int]) = {
    val isCompactPack = !isWindowsNative // True iff parameters passed on their registers files independently
    val locations = new Array[Location](parameterCount)
    var currTailSize = 0
    var curAltLocSlot = 0
    
    for (i <- locations.indices) {
      val kind = parameterType(i).symKindErased
      val inHead = i < limit

      locations(i) = if (isAltLocationParameter(i)) {
        val altLocSlot = AltLocation(curAltLocSlot)
        curAltLocSlot += 1
        altLocSlot

      } else if (inHead && kind.isFloatingPoint && fRegsQueue.hasNext) {
        if (!isCompactPack) {
          if (isVarArgParam(i)) {
            iRegsQueue.next()
          } else {
            iRegsQueue.skip()
          }
        }
        fRegsQueue.next()

      } else if (inHead && !kind.isFloatingPoint && iRegsQueue.hasNext) {
        if (!isCompactPack) {
          fRegsQueue.skip()
        }
        iRegsQueue.next()

      } else {
        val (slot, newTailSize) = makeTailSlot(i, kind, currTailSize)
        currTailSize = newTailSize
        slot
      }
    }

    val tailSize = if (currTailSize > 0 || isJETVarArgs) {
      Some(currTailSize)
    } else if (isCVarArgs && targetOS.isWindows) {
      // In this case, TR used for baseline (see [[loadVarArgsAddrTo]])
      Some(0)
    } else {
      None
    }

    (locations, tailSize)
  }

  override def savedIRegsOrder = iRegs.availableInABIOrder.iterator
  override def savedFRegsOrder = fRegs.availableInABIOrder.iterator

  /** Returns secondary location where param should be placed before call or null.
    * Floating point varargs passed on XMM should be duplicated on GPR for WINDOWS calling convention.
    */
  def parameterSecondaryLocation(paramIdx: Int): GPR = {
    val loc = paramLocations(paramIdx)
    if (isVarArgParam(paramIdx) && isWindowsNative && loc.isFReg) {
      allArgumentIRegs(allArgumentFRegs.indexOf(loc.asInstanceOf[XMM]))
    } else {
      null
    }
  }

  override def allowShortIntegers = !cc.isJET

  override protected def callerFrameTopMayBeUsed: Boolean = super.callerFrameTopMayBeUsed ||
    shadowSpaceSize > 0 // Calling convention has shadow size on top of caller frame

  /** Size of space, allocated by caller after params pushing. "Shadow space" in windows calling convention. */
  def shadowSpaceSize = if (isWindowsNative) windowsShadowSpaceSize else 0

  override def stackParamsStartOffset = {
    if (cc.hasFrameDescriptorSlotParam) {
      assert(shadowSpaceSize == 0, "There are no calling conventions with both shadow space and frameDescriptor")
      stackSlotSize
    } else {
      shadowSpaceSize
    }
  }
}
