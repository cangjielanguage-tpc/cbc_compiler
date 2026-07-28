/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Domain
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool

final class XHInfo[B](
  /** Should be used during code generation to correctly resolve and check access to caught type. */
  catchTypeIndex: Int,

  /** Should be used for equality check and also may be used for some heuristics/optimizations
    * (e.g., `java/lang/Throwable` catches any exception in Java domain). */
  val catchTypeName: XString,

  val handler: B,
  val domain: Domain
) {
  assert(handler != null)
  assert(domain != null)
  assert((catchTypeIndex == 0) == (catchTypeName == null))

  def isCatchAll = catchTypeName == null

  def getCatchType(cp: ConstantPool) = cp.getClassType(catchTypeIndex)

  def cloneWithHandler(anotherHandler: B) = new XHInfo[B](catchTypeIndex, catchTypeName, anotherHandler, domain)

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: XHInfo[_] =>
      if (this.handler == that.handler) {
        assert(this.domain == that.domain)
      }
      this.handler == that.handler && this.catchTypeName == that.catchTypeName
    case _ => false
  }

  override def hashCode = (catchTypeName, handler).##

  override def toString = s"XHInfo{type: $catchTypeName, handler: $handler, domain: $domain}"
}
