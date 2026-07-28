/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.verifier.VerificationError.{ErrorKind, ExceptionKind}
import xscala.util.StringOps.*

/** Base class for components that may take part in bytecode verification.
  *
  * @author kit
  * @author cypok
  */
object VerificationUnit {
  def formatMessageByContext(context: VerifiableMethod, errorMessage: String) =
    XString(s"class: \"${context.getDeclaringClass.getName}\", method: \"${context.getName}\", signature: \"${context.getSignature}\"\n$errorMessage")

  /** Own partial implementation of [[String.format]] needed to reduce dependency on JDK.
    *
    * Only `%d` and `%s` placeholders are supported at this moment.
    */
  private def format(formatString: String, formatArgs: Any*): String = {
    // fast path
    if (formatArgs.isEmpty) return formatString

    val sb = new StringBuilder()

    var offset = 0
    var argIdx = 0
    var placeholderIdx = -1

    while ({ placeholderIdx = formatString.indexOf('%', offset); placeholderIdx != -1 }) {
      sb.append(formatString.substring(offset, placeholderIdx))
      formatString.charAt(placeholderIdx + 1) match {
        case 'd' | 's' =>
          sb.append(formatArgs(argIdx))
          argIdx += 1

        case x =>
          shouldNotReachHere(s"unsupported format type '$x'")
      }
      offset = placeholderIdx + 2
    }
    sb.append(formatString.substring(offset))

    sb.toString
  }
}

abstract class VerificationUnit protected(verify: Boolean, val verificationContext: VerifiableMethod) {
  assert(!verify || verificationContext != null)

  // We have a problem: verifyThat is called very often but condition fails very rarely.
  // So we want to build error message only if it is actually needed.
  // We could pass message "by reference" using lambda but JET currently doesn't explode such lambdas.
  // So we create multiple inlinable versions of verifyThat()
  // and actual message building happens in rarely called not inlinable method throwError().
  // Inlining and not inlining is achieved by JCA directives for JIT (see xkrn.static-shared.jca & jit.pro).
  // Also call of throwError() should be marked as cold, we achieve this by throwing result of no-return throwError().
  private def throwError(errorKind: VerificationError.ErrorKind, exceptionKind: VerificationError.ExceptionKind, errorFormat: String, formatArgs: Seq[Any]) = {
    val errorMessage = VerificationUnit.format(errorFormat, formatArgs*)
    if (verify) {
      new VerificationError(VerificationUnit.formatMessageByContext(verificationContext, errorMessage), errorKind, exceptionKind)
    } else {
      new AssertionError(errorMessage)
    }
  }

  // all throw* methods are @NoInline in JCA
  private def throwVerifyError     (errorFormat: String, formatArgs: Any*) = throwError(ErrorKind.VERIFY_ERROR,         ExceptionKind.VerifyError,      errorFormat, formatArgs)
  private def throwClassFormatError(errorFormat: String, formatArgs: Any*) = throwError(ErrorKind.CLASSFORMAT_ERROR,    ExceptionKind.ClassFormatError, errorFormat, formatArgs)
  private def throwStackMapError   (errorFormat: String, formatArgs: Any*) = throwError(ErrorKind.STACKMAPFORMAT_ERROR, ExceptionKind.ClassFormatError, errorFormat, formatArgs)

  // all verify* methods are @Inline in JCA
  final def verifyThat       (condition: Boolean, errorMessage: String                     ): Unit = if (!condition) throw throwVerifyError     (errorMessage           )
  final def verifyThat       (condition: Boolean, errorFormat: String, arg1: Int           ): Unit = if (!condition) throw throwVerifyError     (errorFormat, arg1      )
  final def verifyThat       (condition: Boolean, errorFormat: String, arg1: Int, arg2: Int): Unit = if (!condition) throw throwVerifyError     (errorFormat, arg1, arg2)
  final def verifyThat       (condition: Boolean, errorFormat: String, arg1: Any           ): Unit = if (!condition) throw throwVerifyError     (errorFormat, arg1      )
  final def verifyThat       (condition: Boolean, errorFormat: String, arg1: Any, arg2: Any): Unit = if (!condition) throw throwVerifyError     (errorFormat, arg1, arg2)
  final def verifyClassFormat(condition: Boolean, errorFormat: String                      ): Unit = if (!condition) throw throwClassFormatError(errorFormat            )
  final def verifyClassFormat(condition: Boolean, errorFormat: String, arg1: Int           ): Unit = if (!condition) throw throwClassFormatError(errorFormat, arg1      )
  final def verifyClassFormat(condition: Boolean, errorFormat: String, arg1: Int, arg2: Int): Unit = if (!condition) throw throwClassFormatError(errorFormat, arg1, arg2)
  final def verifyStackMap   (condition: Boolean, errorFormat: String                      ): Unit = if (!condition) throw throwStackMapError   (errorFormat            )
  final def verifyStackMap   (condition: Boolean, errorFormat: String, arg1: Int           ): Unit = if (!condition) throw throwStackMapError   (errorFormat, arg1      )
}