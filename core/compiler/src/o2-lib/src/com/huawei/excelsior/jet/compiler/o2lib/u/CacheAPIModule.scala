/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.Language.{JAVA, SCALA}
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.RTSProc
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.symlevel.{CallConv, JBCSignature, SigPolyMethodID}

object CacheAPIModule {
  private val classes = Array.fill[pcO.Class](ClassID.values.length)(null)
  private val methods = Array.fill[pcO.MemberRef](MethodID.values.length)(null)
  private val rtProcs = Array.fill[pcO.MemberRef](RTSProc.values.length)(null)

  private def isClassEnabled(id: ClassID): Boolean = id.languageOnly match {
    case Some(lang) if !languagePack.supports(lang) => false
    case _ => true
  }

  def loadClasses(loadType: XString => pcO.Class): Unit = {
    for (id <- ClassID.values if isClassEnabled(id)) {
      classes(id.ordinal) = loadType(id.name)
    }
  }

  def getClass(id: ClassID): pcO.Class = {
    val index = id.ordinal
    if (classes(index) == null && isClassEnabled(id)) {
      classes(index) = pcO.findClass(id.name) ensuring (c => c == null || !c.isUnavailable) // in AOT compiler none of the listed system classes may be absent
    }
    classes(index)
  }

  def isThisClass(t: pc.SymType, id: ClassID): Boolean = (t.mno >= 0) && (pcO.getClassRecord(t.mno) eq getClass(id))

  def getMethod(id: MethodID): pcO.Method = {
    val index = id.ordinal
    if (methods(index) == null) {
      val cls = getClass(id.cls)
      if (cls != null) {
        val method = cls.findLocalMethod(id.nme, O2Env.env.parseMethodSignature(id.sig))
        if (method != null) {
          methods(index) = method.getRef
        }
      }
    }
    if (methods(index) == null) null else methods(index).getMethod
  }

  def getRTSProc(id: RTSProc): pcO.Method = {
    if (rtProcs(id.ordinal) == null) {
      rtProcs(id.ordinal) = getClass(ClassID.CompilerInterface).findLocalMethod(js.internJString(id.productPrefix)).getRef
    }
    rtProcs(id.ordinal).getMethod
  }

  def isThisMethod(method: pcO.Method, id: MethodID): Boolean = method == getMethod(id)
}
