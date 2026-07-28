/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.STATIC
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.{SpecialParamSet, SpecialParameter}

import java.lang.reflect.{Executable, Method}

object FakeMethodType {
  private val FAKE = new MethodType(MethodSignature()(SignatureType.Void), MANAGED, CallKind.NORMAL, SpecialParamSet(), false, MethodType.UNINITIALIZED_FIRST_VAR_ARG, Int.MaxValue, 1, 0, MethodType.AltLocationInfo.NoAltLocation)

  private def typeByKind(k: TypeKind): FakeType = k match {
    case TypeKind.ARRAY => FakeType.create(classOf[Array[Object]])
    case _ => FakeType(k)
  }

  def create(): MethodType = FAKE

  def create(returnTypeKind: TypeKind): MethodType =
    create().changeReturnType(SignatureType.fromSymType(typeByKind(returnTypeKind)))

  def create(returnTypeKind: TypeKind, paramTypeKinds: TypeKind*): MethodType =
    create(returnTypeKind).changeParameters(paramTypeKinds.map(k => SignatureType.fromSymType(typeByKind(k))))

  def create(method: Executable): MethodType = {
    val retType = SignatureType.fromSymType(method match {
      case m: Method => FakeType.create(m.getReturnType)
      case _ => FakeType(TypeKind.VOID)
    })
    val paramsTypes = method.getParameterTypes.map(FakeType.create).map(SignatureType.fromSymType)
    val hasReceiver = !(Modifiers(method.getModifiers) contains STATIC)
    val receiverType = FakeType.create(method.getDeclaringClass)
    create()
      .changeReturnType(retType)
      .changeParameters(paramsTypes.toSeq)
      .insertReceiverType(SignatureType.fromSymType(receiverType), shouldHaveReceiver = hasReceiver)
  }
}
