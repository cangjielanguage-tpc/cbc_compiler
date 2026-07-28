/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.serialization

import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo.MethodExtraInfoLocal
import com.huawei.excelsior.jet.compiler.symlevel.Method

import scala.collection.mutable

trait TestExtraInfo extends OptExtraInfo { self: Universe =>
  private val methodsExtraInfo = mutable.HashMap.empty[Method, MethodExtraInfoLocal]

  def addMethodInfo(method: Method, newInfo: MethodExtraInfoLocal => MethodExtraInfoLocal): Unit =
    methodsExtraInfo(method) = newInfo(methodsExtraInfo.getOrElse(method, MethodExtraInfoLocal.empty))

  override def locallyAnalyzeMethod(method: Method): Option[MethodExtraInfoLocal] = methodsExtraInfo.get(method)
}
