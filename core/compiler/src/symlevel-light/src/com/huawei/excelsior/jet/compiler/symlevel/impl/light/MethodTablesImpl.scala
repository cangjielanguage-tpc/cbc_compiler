/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.layout.MethodTables.{NO_VNUM, ref}
import com.huawei.excelsior.jet.compiler.layout.{FieldsLayout, MethodTables, MethodTablesScala}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule}
import com.huawei.excelsior.jet.compiler.symlevel.ClassType
import com.huawei.excelsior.jet.compiler.symlevel.FindMethodImplResult
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*

/**
  * @author liontiger
  */
object MethodTablesImpl {
  def buildMTLayout(clazz: ClassType): Unit = {
    val o2class = clazz.asInstanceOf[TypeImpl].asClass
    val layout = MethodTablesScala.buildMTLayout(clazz, env)

    o2class.setVMTSize(layout.size)

    val imtSlots = layout.inums
    if (clazz.isClass && imtSlots.nonEmpty) {
      o2class.setIMTSlots(imtSlots)
    }
  }

  def getVMTForThinType(clazz: ClassType)(implicit typeProvider: TypeProvider) = {
    assert(clazz.isThinClass)
    val fmiVMT = MethodTables.buildMT(clazz)

    Array.tabulate[pcOModule.Method](fmiVMT.length) { i =>
      fmiVMT(i) match {
        case FindMethodImplResult.Found(m) =>
          m.asInstanceOf[MethodImpl].o2m
        case FindMethodImplResult.Error(err) =>
          shouldNotReachHere(s"Corrupted class hierarchy ($err in Thin type ${clazz.getName}). Please recompile all classes.")
      }
    }
  }

  def getVNum(method: pcOModule.Method) = {
    val m = methodByO2Object(method)
    val c = m.getDeclaringClass
    if (MethodTables.canBeInMethodTable(m)) c.getMTLayout.vnum(ref(c, m)) else NO_VNUM
  }
}
