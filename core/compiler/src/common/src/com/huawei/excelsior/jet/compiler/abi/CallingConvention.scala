/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.jet.assembler.Location.{AnyReg, FReg, IReg}
import com.huawei.excelsior.jet.compiler.Env.execEnvRegister
import com.huawei.excelsior.jet.compiler.symlevel.{CallConv, MethodType}

import scala.reflect.ClassTag
import scala.collection.mutable

/** Calling convention description.
  *
  * @author conwor
  */
final case class CallingConvention[IR <: IReg : ClassTag, FR <: FReg : ClassTag](sourceCC: CallConv,
                                                                                 baseIRegs: RegFile[IR],
                                                                                 baseFRegs: RegFile[FR],
                                                                                 alwaysVolatile: Array[AnyReg]
                                                                                ) {
  assert(!baseIRegs.volatilesSet.contains(execEnvRegister.asInstanceOf[IR]))

  def isJET = sourceCC.isJET

  /** Returns true iff in this CC all registers except param/result registers and some call scratches are non-volatile. */
  def ecoFriendly = sourceCC.ecoFriendly

  /** True iff this CC has additional parameter on stack - frame descriptor of caller. */
  def hasFrameDescriptorSlotParam = isJET
}

trait CallingConventionCache[IR <: IReg, FR <: FReg] {

  private val cache = mutable.HashMap.empty[CallConv, CallingConvention[IR, FR]]
  final def dropCache(): Unit = cache.clear() // For unit-tests only
  final def apply(methodType: MethodType): CallingConvention[IR, FR] = {
    val sourceCC = methodType.callConv
    cache.getOrElseUpdate(sourceCC, create(sourceCC))
  }

  protected def create(sourceCC: CallConv): CallingConvention[IR, FR]
}
