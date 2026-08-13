/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, ExtraPassModule as ExtraPass, NumerateModule as Numerate, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.symlevel.{GenericInfo, MethodSignature, SignatureType}
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.Set32

object SymLevelBuilderModule {

  // TODO-MODIFIERS: refactor this

  def newClass(name: pcNames.NAME, modifiers: Modifiers, srcFD: xfs.FileDescriptor): pcO.Class = {
    val xotModifiers = modifiers.value.toSet32
    val clazz = pcO.makeClassHeadTags(name, xotModifiers, xotModifiers)
    clazz.fileDescriptor = srcFD
    clazz
  }

  def addMethod(clazz: pcOModule.Class, name: XString, sig: MethodSignature, modifier: Set32, receiver: Option[SignatureType]): pcOModule.Method = {
    addMethod(clazz, name, sig, modifier, ABI.Description(receiver))
  }

  def addMethod(clazz: pcOModule.Class, name: XString, sig: MethodSignature, modifier: Set32, abiDesc: ABI.Description): pcOModule.Method = {
    pc.withModule(clazz) {
      val m = clazz.newMethod(name, sig, modifier, addSignatureImport = true, abiDesc)
      if (m.getSignature.parameterTypes.size > 255) {
        env.errors.fault(ErrMsg2501, m.name)
      }
      m
    }
  }

  def addField(clazz: pcO.Class, name: XString, sig: SignatureType, modifiers: Set32): pcO.Field =
    pc.withModule(clazz) { clazz.newField(name, sig, modifiers, addSignatureImport = true) }

  def preprocessClass(clazz: pcO.Class): Unit = {
    numerateMembersInClassFile(clazz.declaredMethods)
    numerateMembersInClassFile(clazz.declaredFields)

    pcJCAModule.injectFields(clazz)
    Numerate.preProcessBytecode(clazz)
  }

  def processClass(clazz: pcO.Class): Unit = {
    Numerate.processClass(clazz)
    ExtraPass.passModule(clazz)
    pcO.outSymFile(clazz)
  }

  private def numerateMembersInClassFile(members: Iterator[pcO.Member]): Unit = {
    var num = 0
    for (m <- members) {
      m.setNumberInClassFile(num)
      num += 1
    }
  }
}
