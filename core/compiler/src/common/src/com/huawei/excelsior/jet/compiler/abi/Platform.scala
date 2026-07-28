/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg}
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodReference, MethodType, SignatureType}
import xscala.properties.OS

abstract class Platform[IR <: IReg, FR <: FReg, XABI <: ABI[IR, FR]] protected
  (val arch: Arch, val os: OS,
   val stackPointer: IR,
   val framePointer: IR,
   val linkRegister: IR,
   val execEnvRegister: IR,
   val tailRegister: IR,
   val frameMiddleRegister: IR,
   val frameAlignment: Int,             // General alignment of a frame on the platform, actual alignment of some frames might differ.
   val forceFrameAlignment: Boolean) {  // Force alignment of every frame, even empty ones.

  def abi(mtype: MethodType): XABI

  final def abi(mref: MethodReference): XABI = abi(mref.realMethodType)
  final def abi(method: Method, varArgs: collection.Seq[SignatureType]): XABI = abi(method.getRealMethodType(varArgs))
  final def abi(method: Method): XABI = abi(method, null)
}
