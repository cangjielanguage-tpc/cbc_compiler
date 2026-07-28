/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule.ClassSet
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule}
import com.huawei.excelsior.jet.compiler.o2lib.u.{CacheAPIModule, ClassID}
import com.huawei.excelsior.jet.compiler.options.BoolOption.NoVerify
import com.huawei.excelsior.jet.compiler.symlevel.Type
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.{classByO2Object, env, typeToO2Class}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.TypeImpl.fromO2Type
import com.huawei.excelsior.jet.compiler.verifier.AbstractVerifier.InfoBuilder
import com.huawei.excelsior.jet.compiler.verifier.{VerifiableType, Verifier}

class LightweightJavaVerifier extends JavaVerifier {

  private val verifier = new Verifier
  private val toVerify = new ClassSet

  private def getVerificationInfoBuilder(_class: pcOModule.Class) = new InfoBuilder() {
    override def addVerificationPairImpl(from: Type, to: Type, message: XString): Unit =
      pcOModule.addVerificationPair(_class, typeToO2Class(from), typeToO2Class(to), message)

    override def getObjectType: VerifiableType =
      new VerifiableTypeImpl(fromO2Type(CacheAPIModule.getClass(ClassID.Object)))
  }

  private def verifySuper(curClass: pcOModule.Class, superClass: pcOModule.Class): Boolean = {
    verifyClass(superClass)
    if (!superClass.isVerifiable) {
      curClass.copyVerifyErrorFrom(superClass)
      return false
    }
    if (superClass.needVerify) {
      curClass.inclModifier(pcOModule.xot_needcheckpairs)
    }
    true
  }

  private def verifyClass(_class: pcOModule.Class): Unit = {
    // Note that this verification logic is duplicated in JITVerifier.ajl

    if (!toVerify(_class)) {
      return
    }
    toVerify -= _class

    if (_class.isUnloadable) {
      return
    }

    if (env.enabled(NoVerify)) {
      return
    }

    val superClass = _class.getSuperClassO2
    if (superClass != null) {
      if (!verifySuper(_class, superClass)) {
        return
      }
    }

    val it = _class.getSuperInterfacesO2
    while (it.hasNext) {
      if (!verifySuper(_class, it.next())) {
        return
      }
    }

    val cfVersion = _class.classInfo.versionMajor.toInt
    val `type` = classByO2Object(_class).asInstanceOf[TypeImpl]
    val builder = getVerificationInfoBuilder(_class)
    val verifyError = verifier.verifyClass(new VerifiableTypeImpl(`type`), cfVersion, env, builder)
    if (verifyError != null) {
      _class.setNotVerifiedCodeError(verifyError.exceptionKind, verifyError.errorMsg)
    }
  }

  override def markToVerify(_class: pcOModule.Class): Unit = toVerify += _class

  override def verify(_class: pcOModule.Class): Unit = O2Env.stage(Stage.Verifying) { verifyClass(_class) }
}
