/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline

import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.types.Guards

import scala.PartialFunction.cond

trait CallSitesHelper { this: Universe =>

  /** Forces inline of `targetMethod` in all instance methods of `targetMethod.getDeclaringClass` subclasses using
    * [[Guards.PointGuard PointGuard]]
    * if JCA option INLINE_WITH_CONTEXT_POINT_TEST is set.
    * <br>
    * Option is intended to force inline of methods which are not covered by [[com.huawei.excelsior.jet.compiler.opt.middle.inline.CallSites CallSites]]
    * heuristics and are performance-critical (enlisted in JET-10968). For example,
    * [[java.util.Stack#pop Stack.pop]] is synchronized itself and calls synchronized method [[java.util.Vector#removeElementAt Vector.removeElementAt]]
    * which could not be inlined on common basis as it contains "heavyweight" [[java.lang.System#arraycopy]] call.
    * However, guarded inline provides a performance boost (e.g. for SpecJVM2008.xml.validation).
    * <br>
    * Note that this option should be used with care as it forces insertion of non-PGO-motivated
    * [[Guards.PointGuard PointGuard]]
    * which could degrade performance.
    **/
  def shouldInlineWithContextPointTest(targetMethod: Method, receiver: Node): Boolean = {
    assert(!targetMethod.isStatic)

    targetMethod.isJCAInlineWithContextPointTest &&
      cond(receiver) {
        case ReceiverParam() if !rootReceiverType.isAbstractClass && !rootReceiverType.isInterface =>
          targetMethod.getDeclaringClass isAssignableFrom rootReceiverType
      }
  }
}
