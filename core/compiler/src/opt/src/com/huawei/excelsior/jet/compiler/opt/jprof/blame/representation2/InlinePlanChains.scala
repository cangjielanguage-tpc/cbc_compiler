/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation2

import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.PlanReasoning
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.{CallGraph, InlinePlanBase, Method}
import com.huawei.excelsior.jet.compiler.symlevel.Method as SymMethod
import com.huawei.excelsior.jet.jprof.JProfWriter

class InlinePlanChains private[blame]() extends InlinePlanBase {
  def contains(callSitePos: Position, target: SymMethod): Boolean = ???
  def methods(callSitePos: Position): Iterator[(SymMethod, Int)] = ???
  def pgoHostSet: collection.Set[Method] = ???
  private[blame] def truePGOHostSet: collection.Set[Method] = ???
  def markInlined(inlineContext: InlineContext, symTarget: SymMethod, pos: Int): Unit = ???
  def serialize(jprofWriter: JProfWriter): Unit = ???
  def printAsDOT(name: String, baseGraph: CallGraph): Unit = ???
  def printPlan(name: String): Unit = ???
}
