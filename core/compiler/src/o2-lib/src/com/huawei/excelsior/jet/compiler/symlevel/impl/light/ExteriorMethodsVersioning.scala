/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.classByO2Object

import scala.collection.mutable.ArrayBuffer

/**
  * This is a kind of storage to keep current class versioned methods after VZC.compiler.exitClass happens
  * and typesInfo cache get cleared. When kept here they are available for later allocateObj and generateFormOMF. 
  */
object ExteriorMethodsVersioning {
  private val versionedMethods = ArrayBuffer.empty[VersionedMethod]

  def getIteratorOverVersionedMethods(c: pcOModule.Class) = {
    assert(c == opAttrsModule.currClass)
    versionedMethods.iterator
  }

  def collectVersionedMethods(t: pcOModule.Class): Unit = {
    assert(t == opAttrsModule.currClass)
    versionedMethods.clear()
    versionedMethods ++= classByO2Object(t).getVersionedMethods.map(_.asInstanceOf[VersionedMethod])
  }
}
