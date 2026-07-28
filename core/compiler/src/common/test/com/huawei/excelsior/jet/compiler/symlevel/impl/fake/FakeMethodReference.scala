/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind.*
import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind
import com.huawei.excelsior.jet.compiler.layout.MethodTables
import com.huawei.excelsior.jet.compiler.symlevel.*

object FakeMethodReference {
  private def methodForAccessKind(akind: MethodAccessKind): FakeMethod = {
    // Feel free to add more unit test fixes here...
    val m = new FakeMethod
    m.setStatic(akind == STATIC)
    if (akind == INTERFACE) {
      FakeType(TypeKind.INTERFACE).addMethod(m)
    }
    m
  }

  private def accessKindForMethod(method: Method): MethodAccessKind  = {
    // Feel free to add more unit test fixes here...
    if (method.isStatic) {
      STATIC
    } else if (method.getDeclaringClass.isInterface) {
      INTERFACE
    } else {
      VIRTUAL
    }
  }
}

class FakeMethodReference(_methodType: MethodType,
                          _accessKind: MethodAccessKind = STATIC,
                          _method: Method = null,
                          _permanentMethod: PermanentMember = null,
                          _refClass: ClassType = null,
                          _isMemberNameInvoke: Boolean = false,
                          _cpIndex: Int = BytecodeMethodReference.UNKNOWN_CP_INDEX)
  extends BytecodeMethodReference(_methodType, _accessKind, _method, _permanentMethod, _refClass, _isMemberNameInvoke, _cpIndex) {

  def this(method: Method, accessKind: MethodAccessKind, refClass: ClassType) =
    this(method.getMethodType, accessKind, method, _refClass = refClass)

  def this(method: Method) =
    this(method, FakeMethodReference.accessKindForMethod(method), method.getDeclaringClass)

  def this(accessKind: MethodAccessKind) =
    this(FakeMethodReference.methodForAccessKind(accessKind))

  def this() =
    this(new FakeMethod)


  private def methodOrNull: Method = if (hasMethod) method else null

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: FakeMethodReference =>
      this.methodType == that.methodType &&
        this.methodOrNull == that.methodOrNull /*&&
        this.accessKind == that.accessKind*/ // TODO: uncomment if unit-tests can handle it
    case _ => false
  }

  override def hashCode = (methodType, methodOrNull /*, accessKind()*/).## // TODO: uncomment if unit-tests can handle it

  override def hasVirtualMethodSlot = (accessKind == MethodReferenceAccessKind.VIRTUAL) || (accessKind == MethodReferenceAccessKind.INTERFACE)

  override def virtualMethodSlot = if (hasVirtualMethodSlot) 0 else MethodTables.NO_VNUM
}