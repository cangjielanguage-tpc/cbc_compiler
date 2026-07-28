/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.abi.SerialTypeInfoGenerator
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*

/**
  * @author liontiger
  */
object DataGen {
  def genSerialTypeInfo(_class: pcOModule.Class) = {
    val buf = SerialTypeInfoGenerator.gen(classByO2Object(_class), env)
    val seg = env.convertByteBufferToCodeSegm(buf)

    val obj = opAttrsModule.createSpecialObject(opAttrsModule.SerialTypeInfo)
    pcOModule.setPlainArrayLength(obj, seg.length)
    opAttrsModule.setSegment(obj, seg)

    obj
  }

  def genVMTEncoding(_class: pcOModule.Class)(implicit typeProvider: TypeProvider) = {
    val buf = SerialTypeInfoGenerator.encodeMTLayout(classByO2Object(_class), env)
    val seg = env.convertByteBufferToCodeSegm(buf)

    val obj = opAttrsModule.createSpecialObject(opAttrsModule.VMTEncoding)
    pcOModule.setPlainArrayLength(obj, seg.length)
    opAttrsModule.setSegment(obj, seg)

    obj
  }

  def genPreparationInfo(_class: pcOModule.Class) = {
    var obj: pc.DataSymbol.Sized = null

    val tpe = classByO2Object(_class)

    val buf = SerialTypeInfoGenerator.genPreparationInfo(tpe, env)
    if (buf != null) {
      val seg = env.convertByteBufferToCodeSegm(buf)
      obj = opAttrsModule.createSpecialObject(opAttrsModule.PreparationInfo)
      pcOModule.setPlainArrayLength(obj, seg.length)
      opAttrsModule.setSegment(obj, seg)
    }

    typesForPreparation.clear()

    obj
  }
}
