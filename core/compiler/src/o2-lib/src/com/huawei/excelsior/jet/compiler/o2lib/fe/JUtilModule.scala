/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.xjRTSModule as xjRTS
import com.huawei.excelsior.jet.compiler.symlevel.{MethodSignature, SignatureType}
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.Set32

object JUtilModule {
  def addSyntheticClass(name: XString): pcO.Class = {
    var cls = pcO.findClass(name)
    if (cls == null) {
      cls = pcO.makeClassHead(pcNames.newClassName(name))
      cls.markAsSynthetic()
    }
    cls
  }

  def insertSyntheticStaticField(name: XString, sig: SignatureType): pcO.StaticField = {
    val syntheticHost = pcO.x2cClass
    pc.withModule(syntheticHost) {
      var f = syntheticHost.findLocalField(name, sig).asInstanceOf[pcO.StaticField]
      if (f == null) {
        f = syntheticHost.newStaticField(name, sig, Set32.of(xjRTS.mdf_static.toUByte), addSignatureImport = false)
        // import list of syntheticHost is not important
      }
      f
    }
  }

  private def setFrom(aclass: pcO.Class, from: pcO.Class): Unit = {
    if (aclass.isAbsent) {
      if (aclass.getFrom ne from) {
        aclass.setFrom(from)
      }
    }
  }

  def insertAbsentField(from: pcO.Class, aclass: pcO.Class, name: XString, sig: SignatureType, static: Boolean): pcO.Field = {
    var mdfs: Set32 = Set32.empty

    pc.withModule(aclass) {
      setFrom(aclass, from)
      var f = aclass.findLocalField(name, sig)
      if (f == null) {
        if (static) {
          mdfs = Set32.of(xjRTS.mdf_static.toUByte)
        } else {
          mdfs = Set32.empty
        }
        f = aclass.newField(name, sig, mdfs, addSignatureImport = false)
      }
      f
    }
  }

  def insertAbsentMethod(from: pcO.Class, aclass: pcO.Class, name: XString, sig: MethodSignature, static: Boolean): pcO.Method = {
    pc.withModule(aclass) {
      var mdfs: Set32 = Set32.empty
      setFrom(aclass, from)
      var p = aclass.findLocalMethod(name, sig)
      if (p == null) {
        if (static) {
          mdfs = Set32.of(xjRTS.mdf_static.toUByte)
        } else {
          mdfs = Set32.empty
        }
        p = aclass.newMethod(name, sig, mdfs, addSignatureImport = false)
      }
      p
    }
  }

  def checkTypeForAbsence(type0: pc.SymType): Boolean = {
    var c = pcO.getCoreClassType(type0)
    while (c != null) {
      if (c.isUnavailable) {
        return true
      }
      if (!c.isVerifiable) {
        return false
      }
      c = c.getSuperClassO2
    }
    false
  }

  private def getErrorSource(cPar: pcO.Class): pcO.Class = {
    var c = cPar

    while (c != null) {
      assert(!c.isUnavailable)
      if (!c.isVerifiable) {
        return c
      }
      c = c.getSuperClassO2
    }
    null
  }

  /*
     Returns a verify error if the verify error will be thrown at least
     at linking stage of the class "cls" (maybe earlier).
  */
  def throwsVerifyErrorAtFirstUse(cls: pcO.Class): pcO.VerifyError = {
    if (!checkTypeForAbsence(cls)) {
      val src = getErrorSource(cls)
      if (src != null) {
        return src.getVerifyError
      }
    }
    null
  }
}
