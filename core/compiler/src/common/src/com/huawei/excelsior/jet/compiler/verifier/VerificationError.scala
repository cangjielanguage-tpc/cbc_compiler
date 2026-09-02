/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier

import com.huawei.excelsior.jet.compiler.verifier.VerificationError.ErrorKind.CLASSLOADING_ERROR
import com.huawei.excelsior.jet.compiler.verifier.VerificationError.{ErrorKind, ExceptionKind}
import com.huawei.excelsior.jet.common.XString

/** Internal verifier error format.
  *
  * @author kit
  * @author cypok
  */
case class VerificationError(errorMsg: XString, errorKind: ErrorKind, exceptionKind: ExceptionKind) extends RuntimeException {
  def this(msg: String, errorKind: VerificationError.ErrorKind, exceptionKind: VerificationError.ExceptionKind) = {
    this(if (msg.isEmpty) XString.empty else XString(msg), errorKind, exceptionKind)
  }

  def toClassLoadingError = new VerificationError(errorMsg, CLASSLOADING_ERROR, exceptionKind)

  override def toString = s"[$errorKind]$exceptionKind($errorMsg)"
}

object VerificationError {

  /** Major kind of verification error. */
  enum ErrorKind {
    case VERIFY_ERROR
    case CLASSFORMAT_ERROR
    case STACKMAPFORMAT_ERROR
    case CLASSLOADING_ERROR
  }

  /** Kind of standard exception corresponding to the error. */
  enum ExceptionKind {
    case NoClassDefFoundError
    case NoSuchFieldError // should be never used
    case NoSuchMethodError // should be never used
    case IncompatibleClassChangeError
    case FatalError
    case VerifyError
    case ClassFormatError
    case AbstractMethodError // should be never used
    case ClassCircularityError
    case IllegalAccessError
    case UnsupportedClassVersionError
  }

  object ExceptionKind {
    def byIndex(index: Int) = fromOrdinal(index)
  }
}
