/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.symlevel.Type
import com.huawei.excelsior.jet.compiler.verifier.AbstractVerifier.InfoBuilder

object AbstractVerifier {

  abstract class InfoBuilder {
    private var fallBackSupported = false
    private var undefinedVerificationResult = false // valid only if fallBackSupported

    /** Check {@code to.isAssignableFrom(from)} could not be done during this class verification,
      * postpone it to run-time.
      */
    final def addVerificationPair(from: Type, to: Type, context: VerifiableMethod): Unit = {
      if (fallBackSupported) {
        undefinedVerificationResult = true
      } else {
        addVerificationPairImpl(from, to, VerificationUnit.formatMessageByContext(context, to.getName + " is not assignable from " + from.getName))
      }
    }

    protected def addVerificationPairImpl(from: Type, to: Type, message: XString): Unit

    /** Returns java.lang.Object as VerifiableType. */
    def getObjectType: VerifiableType

    protected def setupForTypeChecking(fallBackSupported: Boolean): Unit = {
      assert(!this.fallBackSupported && !this.undefinedVerificationResult)
      this.fallBackSupported = fallBackSupported
    }

    protected def finishTypeChecking = {
      fallBackSupported = false
      val res = undefinedVerificationResult
      undefinedVerificationResult = false
      res
    }
  }

  trait Info {
    /** Get verify error or class definition error information for this class or null if class is verifiable. */
    def getVerifyError: VerificationError
  }
}

abstract class AbstractVerifier {
  def verifyClass(`type`: VerifiableType, cfVersion: Int, env: Environment, builder: InfoBuilder): VerificationError
}
