/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Domain
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute
import com.huawei.excelsior.jet.compiler.symlevel.JBCSignature
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.asVerifiableMethodType
import com.huawei.excelsior.jet.compiler.verifier.{VerifiableMethod, VerifiableMethodType, VerifiableType}

class FakeVerifiableMethod(private[fake] val impl: FakeMethod) extends VerifiableMethod {
  override def getDomain: Domain = impl.getDomain

  override def getDeclaringClass: VerifiableType = impl.getDeclaringClass match {
    case cls: FakeType => new FakeVerifiableType(cls)
  }

  override def getName: String = impl.getName

  override def getFullName: String = impl.getFullName

  override def getXSignature: XString = XString.ascii(JBCSignature(impl.getSignature))

  override def codeAttribute: MethodCodeAttribute = impl.codeAttribute

  override def isConstructor = impl.isConstructor

  override def isStatic = impl.isStatic

  override def markAsContainingMonitorOperations(): Unit = {
    impl.markAsContainingMonitorOperations()
  }

  override def canBeVerified = !impl.isNative && !impl.isAbstract && !impl.isAJReplaced
}