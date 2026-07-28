/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.onbytecode

import scala.collection.mutable

final class MonitorEnter[B](
  val block: B,

  /** Indices of bytecode locals that contain the reference value equal to the one used in monitorenter operation. */
  val aliasedLocals: Seq[Int]
) {
  /** Indices of bytecode locals overwritten on some execution path from monitorenter to monitorexit. */
  val spoiledLocals = mutable.HashSet.empty[Int]

  override def toString = aliasedLocals.mkString("MonitorEnter(", ", ", ")")
}
