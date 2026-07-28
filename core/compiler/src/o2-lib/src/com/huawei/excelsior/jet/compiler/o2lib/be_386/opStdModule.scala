/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule as at
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, JUtilModule as ju, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.{CacheAPIModule as CacheAPI, JStringsModule as js}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.verifier.VerificationError
import com.huawei.excelsior.jet.compiler.verifier.VerificationError.ExceptionKind.*
import com.huawei.excelsior.jet.compiler.{RTSGlobal, RTSProc}

object opStdModule {
  private val symbols: Array[pcO.StaticField] = new Array[pcO.StaticField](RTSGlobal.values.length)

  def dataSymbol(N: RTSGlobal): pcO.StaticField = {
    val i = N.ordinal
    if (symbols(i) == null) {
      symbols(i) = ju.insertSyntheticStaticField(js.format(N.productPrefix), SignatureType.Int32)
    }
    symbols(i)
  }

  def stdExceptionProc(exc: VerificationError.ExceptionKind): RTSProc = exc match {
    case _ if (languagePack == LanguagePack.SCALA) => RTSProc.JR_FatalError

    case NoClassDefFoundError           => RTSProc.JR_ThrowNoClassDefFoundError
    case NoSuchFieldError               => RTSProc.JR_ThrowNoSuchFieldError
    case NoSuchMethodError              => RTSProc.JR_ThrowNoSuchMethodError
    case VerifyError                    => RTSProc.JR_ThrowVerifyError
    case ClassFormatError               => RTSProc.JR_ThrowClassFormatError
    case AbstractMethodError            => RTSProc.JR_ThrowAbstractMethodError
    case ClassCircularityError          => RTSProc.JR_ThrowClassCircularityError
    case IllegalAccessError             => RTSProc.JR_ThrowIllegalAccessError
    case UnsupportedClassVersionError   => RTSProc.JR_ThrowUnsupportedClassVersionError
    case IncompatibleClassChangeError   => RTSProc.JR_ThrowIncompatibleClassChangeError
    case FatalError                     => RTSProc.JR_FatalError
  }

  def initModule(): Unit = {
    at.ini()
    pcO.x2cClass = ju.addSyntheticClass(js.format("X2C"))
    symbols.mapInPlace(_ => null)
  }

  def exitModule(): Unit = {
    symbols.mapInPlace(_ => null)
    at.exi()
  }
}
