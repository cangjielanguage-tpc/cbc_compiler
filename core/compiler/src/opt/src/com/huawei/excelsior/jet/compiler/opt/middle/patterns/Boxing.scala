/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.patterns

import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.ScalaCollections

/** Boxing is a process of wrapping of some primitive value in a non-primitive object called "box". */
trait Boxing { self: Universe =>

  private object ValueOfInvoke {
    def unapply(call: Call) = {
      val ref = call.targetRef
      if (ref.hasRefClass) {
        ref.refClass match {
          case Java.Support.BoxType(boxType) if ref.hasMethod && ref.method == boxType.valueOf =>
            Some(boxType, ScalaCollections.singleElement(call.invokeArgs))

          case XScala.Support.BoxType(boxType) if ref.hasMethod && ref.method == boxType.valueOf =>
            Some(boxType, ScalaCollections.singleElement(call.invokeArgs))

          case _ => None
        }
      } else {
        None
      }
    }
  }

  def foldBoxedValues(): Boolean = {
    var changed = false
    for (x @ ValueOfInvoke(boxType, primArg) <- all[Call]) {
      replaceByCode(x) { BoxedValue(boxType)(primArg) }
      changed |= true
    }
    changed
  }

}

