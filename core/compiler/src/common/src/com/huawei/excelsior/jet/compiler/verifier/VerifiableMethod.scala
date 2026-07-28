/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Domain
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute

trait VerifiableMethod {
  /** Determines the domain of the method.
    *
    * NOTE: `@AJExtended` and `@java` classes
    * (and their managed methods) are considered as having Java domain.
    * All unmanaged methods are considered AJ.
    */
  def getDomain: Domain

  def getDeclaringClass: VerifiableType

  def getName: String
  def getFullName: String

  def getSignature: String = getXSignature.toString
  def getXSignature: XString

  def codeAttribute: MethodCodeAttribute

  def isConstructor: Boolean

  def isStatic: Boolean

  /** Mark `this` method as containing `monitorEnter`/`monitorExit` bytecodes.
    * NOTE: can be called either on AOT parsing stage, or during JIT.
    */
  def markAsContainingMonitorOperations(): Unit

  def canBeVerified: Boolean

}
