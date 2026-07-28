/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.arm64

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.*
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.D.*
import com.huawei.excelsior.jet.assembler.arm64.{IRegister, VFPRegister}
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.abi.ABI.{AltLocation, ParamsQueue, RetType}
import com.huawei.excelsior.jet.compiler.symlevel.{MethodType, TypeKind}
import xscala.util.MathUtils.alignUp

/** ABI implementation for arm64. Each ABI instance holds all information
  * about parameter displacement for particular native calling convention.
  *
  * @author gatimosh
  */
class ABIArm64 private[arm64](_methodType: MethodType) extends ABI[IRegister.X, VFPRegister.D](_methodType, CallingConventionArm64(_methodType)) {

  override protected lazy val resultRegsImpl: Array[AnyReg] = RetType(methodType) match {
    case RetType.I | RetType.L => Array(X0)
    case RetType.F | RetType.D => Array(D0)
    case RetType.Void => Array.empty[AnyReg]
  }

  protected def initLocations(iRegsQueue: ParamsQueue[IRegister.X], fRegsQueue: ParamsQueue[VFPRegister.D], limit: Int): (Array[Location], Option[Int]) = {
    val locations = new Array[Location](parameterCount)
    var currTailSize = 0
    var curAltLocSlot = 0

    for (i <- locations.indices) {
      val kind = parameterType(i).symKindErased

      // floats should be converted to doubles earlier
      assert(!(kind == TypeKind.FLOAT && isVarArgParam(i)))

      val inHead = i < limit

      locations(i) = if (isAltLocationParameter(i)) {
        val altLocSlot = AltLocation(curAltLocSlot)
        curAltLocSlot += 1
        altLocSlot

      } else if (inHead && kind.isFloatingPoint && fRegsQueue.hasNext) {
        fRegsQueue.next()
      } else if (inHead && !kind.isFloatingPoint && iRegsQueue.hasNext) {
        iRegsQueue.next()
      } else {
        val (slot, newTailSize) = makeTailSlot(i, kind, currTailSize)
        currTailSize = newTailSize
        slot
      }
    }

    val tailSize = if (currTailSize > 0 || isJETVarArgs) {
      Some(currTailSize)
    } else {
      None
    }

    (locations, tailSize)
  }

  override def savedIRegsOrder = iRegs.availableInABIOrder.reverseIterator
  override def savedFRegsOrder = fRegs.availableInABIOrder.reverseIterator

  /** Returns subset of argument passing IRegs that may contain var args and must be pushed firstly on stack in method frame header. */
  def getVarArgsOccupiedIRegs = if (isCVarArgs) iRegArgs.remaining else Array.empty[IRegister.X]

  /** Returns subset of argument passing FRegs that may contain var args and must be pushed firstly on stack in method frame header. */
  def getVarArgsOccupiedFRegs = if (isCVarArgs) fRegArgs.remaining else Array.empty[VFPRegister.D]

  override def getSavedIRegsBitMap(savedIRegs: Iterable[IRegister.X]) = {
    assert(!savedIRegs.exists(x => x == IP0 || x == IP1), "IP0 & IP1 must never be saved")
    super.getSavedIRegsBitMap(savedIRegs)
  }
}
