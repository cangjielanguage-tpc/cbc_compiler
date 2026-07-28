/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.intrinsics

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodSignature}

trait Intrinsic {

  /** Returns the declaring class name. */
  def getClassName: XString

  /** Returns the name of this intrinsic method. */
  def getMethodName: XString

  /** Returns the signature of this intrinsic method. */
  def getSignature: XString

  /** For groups of similar intrinsic methods that differs only by type of operands or return type, returns that
    * specific type.
    *
    * @return operand or return type that characterizes this intrinsic method in a group of similar intrinsics
    */
  def getOpType: BytecodeTypeKind

  private var msig: MethodSignature = _

  def methodSignature(implicit typeProvider: TypeProvider): MethodSignature = {
    if (msig == null) {
      msig = typeProvider.parseMethodSignature(getSignature)
    }
    msig
  }

  /** Checks whether the given [[Method]] is a representation of this intrinsic method. */
  def isThisMethod(method: Method)(implicit typeProvider: TypeProvider) = {
    getClassName == method.getDeclaringClass.getXName &&
      getMethodName == method.getXName &&
      methodSignature == method.getSignature
  }
}

/** Common interface for intrinsic methods supported by the compiler. */
object Intrinsic {

  /** A special symbol in signatures that means platform-dependent address type. */
  val ADDR_SIG_PLACEHOLDER = '&'
}
