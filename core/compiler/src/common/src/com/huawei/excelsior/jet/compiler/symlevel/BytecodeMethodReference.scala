/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.bytecode
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPool, ConstantPoolAccessResult, MethodAccessKind}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType

/** A method reference based on information from bytecode. See [[MethodReference]] for details.
  *
  * TODO: Eventually it should become the only class that implements [[ConstantPool.Access]] from [[Method]]
  *
  * @author ijorch
  */
object BytecodeMethodReference {
  val UNKNOWN_CP_INDEX = -1
}

/** Creates a method reference with given properties. */
class BytecodeMethodReference(_methodType: MethodType,
                              _accessKind: MethodAccessKind,
                              _method: Method = null,
                              _permanentMethod: PermanentMember = null,
                              _refClass: ClassType = null,
                              val isMemberNameInvoke: Boolean = false,
                              val cpIndex: Int = BytecodeMethodReference.UNKNOWN_CP_INDEX)
  extends MethodReference(_methodType, _accessKind.asMethodRefAccessKind, _method, _permanentMethod, ReferenceType(_refClass), None)
    with ConstantPoolObject with ConstantPool.Access[BytecodeMethodReference] with ConstantPool.DeferredAccessInfo {

  /** Creates non-deferred method reference with given properties, only method type is inferred from method itself. */
  def this(method: Method, accessKind: MethodAccessKind, refClass: ClassType, cpIndex: Int) =
    this(method.getMethodType, accessKind, method, null, refClass, false, cpIndex)

  override protected def copy(methodType: MethodType, method: Method, permanentMethod: PermanentMember, refClass: CompiledType, accessKind: MethodReferenceAccessKind, explicitVNum: Option[Int]) = {
    assert(explicitVNum.isEmpty)
    new BytecodeMethodReference(methodType, bytecode.MethodAccessKind.fromMethodRefernceAccessKind(accessKind), method, permanentMethod, asClassType(refClass.symType), isMemberNameInvoke, cpIndex)
  }

  override def getResult: ConstantPoolAccessResult = if (hasMethod) {
    ConstantPoolAccessResult.OK
  } else {
    ConstantPoolAccessResult.DEFERRED
  }

  override def getObject: BytecodeMethodReference = this

  override def getError: ConstantPool.ErrorAccessInfo = shouldNotCallThis()

  override def getDeferredInfo: ConstantPool.DeferredAccessInfo = this

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: BytecodeMethodReference =>
      super.equals(that) &&
        this.isMemberNameInvoke == that.isMemberNameInvoke &&
        this.cpIndex == that.cpIndex
    case _ => false
  }

  override def hashCode = (super.hashCode, isMemberNameInvoke, cpIndex).##
}
