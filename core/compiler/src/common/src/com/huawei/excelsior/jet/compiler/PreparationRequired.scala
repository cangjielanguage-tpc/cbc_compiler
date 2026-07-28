/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType

/** Encapsulates logic for determining which operations and types require preparation.
  *
  * @author liontiger
  */
object PreparationRequired {

  def forType(tpe: SignatureType)(implicit typeProvider: TypeProvider): Type = forType(tpe.symType)

  def forType(tpe: Type): Type = {
    if (tpe.preparationRequired && !tpe.isBootstrapAnnotated) tpe else null
  }

  def forMethodAddr(target: Method): Type = {
    if (target.isManaged && target.isStatic) forType(target.getDeclaringClass) else null
  }

  def forThinNewOp(newType: Type): Type = {
    // Note: actually, preparation here is needed only if there are virtual managed methods in newType
    // TODO: make it more precise if needed
    assert(newType.isThinClass)
    if (newType.isPolyThinClass) forType(newType) else null
  }

  def forCangjieStaticFieldOperation(field: Field): ClassType = {
    // Motivational example:
    //
    // class Arguments {
    //    ...
    //    public static let NULL: Array<String> = []
    // }
    // This initialization code will be called from global_init which is actually just a usual method.
    // In Cangjie we do not have clinits which usually invoke preparation in case of such code in Java, however, the
    // class must be prepared during execution of global_init. Otherwise, GC will collect an object referenced by
    // NULL field as SFBundle of this class is not yet registered.
    // Therefore any access to static fields of Cangjie class provokes its preparation.
    asClassType(forType(field.getDeclaringClass))
  }

  def forGetStatic(field: Field)(implicit typeProvider: TypeProvider): ClassType = {
    if (field.getDeclaringClass.isCangjieType) {
      forCangjieStaticFieldOperation(field)
    } else if (field.getType.symType == typeProvider.getStringType && field.hasInitialValue) {
      asClassType(forType(field.getDeclaringClass))
    } else {
      null
    }
  }

  def forPutStatic(field: Field)(implicit typeProvider: TypeProvider): ClassType = {
    if (field.getDeclaringClass.isCangjieType) {
      forCangjieStaticFieldOperation(field)
    } else if (field.getType.isTraceableReference) {
      asClassType(forType(field.getDeclaringClass))
    } else {
      null
    }
  }

  def forInvoke(methodRef: MethodReference): Type = {
    if (isDirectCallWithPreparation(methodRef) && methodRef.hasMethod && methodRef.method.isManaged) {
      forType(methodRef.method.getDeclaringClass)
    } else {
      null
    }
  }

  private def isDirectCallWithPreparation(methodRef: MethodReference): Boolean = {
    import MethodReferenceAccessKind.*
    methodRef.accessKind match {
      case STATIC | MUT => true
      case SPECIAL | VIRTUAL => methodRef.refClass.isUltraThinClass
      case INTERFACE | STATIC_VIRTUAL => false
    }
  }

  def forConstString(str: ConstString): Type = {
    // Note: AJ javac must guarantee that ConstStrings in unmanaged context will not be generated
    //       and are used only as arguments of compiler intrinsics.
    forType(str.getHost)
  }
}
