/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.indy

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPool, ConstantPoolAccessResult}
import com.huawei.excelsior.jet.compiler.symlevel.ConstantPoolObject
import com.huawei.excelsior.jet.compiler.symlevel.Member

/** Represents parsed value of CONSTANT_MethodHandle constant pool entry.
  *
  * @author liontiger
  */
class MethodHandle(val refKind: ReferenceKind, val member: Member) extends ConstantPoolObject with ConstantPool.Access[MethodHandle] {

  override def getResult: ConstantPoolAccessResult = if (member.getDeclaringClass.isDeferred) {
    ConstantPoolAccessResult.DEFERRED
  } else {
    ConstantPoolAccessResult.OK
  }

  override def getObject: MethodHandle = this

  override def getError: ConstantPool.ErrorAccessInfo = shouldNotCallThis()

  override def getDeferredInfo: ConstantPool.DeferredAccessInfo = shouldNotCallThis("DeferredInfo is under development yet")
}
