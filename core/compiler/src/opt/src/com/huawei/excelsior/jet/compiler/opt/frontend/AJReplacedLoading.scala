/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend

import com.huawei.excelsior.jet.compiler.PreparationRequired
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodReference, MethodReferenceAccessKind, TypeKind}
import com.huawei.excelsior.jet.util.ScalaCollections

trait AJReplacedLoading { this: Universe =>

  def loadAJReplaced(method: Method, args: Seq[Node]): Return = {
    val target = method.getAJReplacement
    assert(target != null)
    assert(target.isStatic)
    val targetRef = new MethodReference(target, MethodReferenceAccessKind.STATIC)

    currentScope.inState(entryBlock, entryBlock) {
      ensurePrepared(PreparationRequired.forInvoke(targetRef))
      val hasZSTRetByVal = method.getMethodType.hasRetByValParameter && method.getReturnType.isZST
      val adjustedArgs = if (hasZSTRetByVal) ScalaCollections.removeAt(args, method.getMethodType.getRetByValArgIdx).toSeq else args
      val call = Node.withImplicitArgConversion(enrichArg()) {
        Invoke(targetRef)(adjustedArgs: _*)
      }

      val retval =
        if (!method.getReturnType.isZST) depriveIfNeeded(call)
        else Void()

      Node.withImplicitArgConversion(enrichArg()) {
        Return.proto(ValueType.fromSig(method.getReturnType, instantiateRich = true))(retval)
      }
    }

  }

}
