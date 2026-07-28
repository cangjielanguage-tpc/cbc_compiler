/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.compiler.debug.CodeRecord._
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels.*
import com.huawei.excelsior.jet.compiler.ir.{InlineContext, LexicalBlock, LineNumber}
import com.huawei.excelsior.jet.compiler.symlevel.Method

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Code description. Contains information about inlined subsections and local variables locations.
  *
  * @author gatimosh
  * @author conwor
  */
object CodeRecord {

  /** Represents inline context in form suitable for debug info serialization.
    * TODO: consider to reformat InlineContext to this. */
  case class DebugInlineContext(method: Method, callSiteLNum: Int, lexBlock: LexicalBlock, callerContext: DebugInlineContext) {
    def isRoot: Boolean = callerContext eq null
    assert(!isRoot || lexBlock == null)
    def reversedCallStack: List[DebugInlineContext] = if (isRoot) List(this) else callerContext.reversedCallStack :+ this
  }

  class Interval(val context: DebugInlineContext, val start: Int, outer: Interval) {
    val children = new ArrayBuffer[Interval]
    val localVariables = new ArrayBuffer[LocalVarLabel]
    var end: Int = -1
    if (outer != null) outer.children += this
    def setEnd(e: Int): Unit = { assert(end == -1); end = e }
    def containsSomeVariables: Boolean = localVariables.nonEmpty || children.exists(c => c.containsSomeVariables)
  }
 }

class CodeRecord(val scope: Method, val seg: Segment) {

  val codeOriginLabels = new ArrayBuffer[CodeOriginLabel]
  val callerFrameInfoLabels = new ArrayBuffer[CallerFrameInfoLabel]
  val localVariables = new ArrayBuffer[LocalVarLabel] // root locals
  val lbLocals = new mutable.LinkedHashMap[(Int, Int), mutable.Set[LocalVarLabel]] 

  private val scopeContext = DebugInlineContext(scope, LineNumber.INVALID, null, null)
  var root: Interval = new Interval(scopeContext, 0, null)

  // Constructor
  {
    val supportLabels = seg.filterLabels(_.isInstanceOf[DebugLabel]).sortBy(_.position)
    assert(supportLabels.nonEmpty)

    supportLabels foreach {
      case label: CodeOriginLabel =>
        if (codeOriginLabels.isEmpty || codeOriginLabels.last != label) codeOriginLabels += label

      case label: CallerFrameInfoLabel =>
        if (callerFrameInfoLabels.isEmpty || callerFrameInfoLabels.last != label) callerFrameInfoLabels += label

      case label: LocalVarLabel =>
        val key = label.info.declaration.filter(decl => decl.lbLine > 0).map(decl => (decl.lbLine, decl.lbCol))
        if (key.nonEmpty) {
          lbLocals.getOrElseUpdate(key.get, mutable.HashSet.empty[LocalVarLabel]) += label // locals for Interval
        } else {
          localVariables += label // root locals
        }

      case _ => shouldNotReachHere()
    }

    val contextsCache = new mutable.LinkedHashMap[(Method, Int, LexicalBlock, DebugInlineContext), DebugInlineContext]
    def addContext(method: Method, line: Int, lexBlock: LexicalBlock, outerContext: DebugInlineContext) =
      contextsCache.getOrElseUpdate((method, line, lexBlock, outerContext), { DebugInlineContext(method, line, lexBlock, outerContext) })

    def convertContext(context: InlineContext, lexBlock: LexicalBlock): DebugInlineContext = if (context.caller == null && lexBlock == null) {
      assert(context.method eq scope)
      scopeContext
    } else if (context.caller != null) {
      assert(lexBlock == null) // lexBlock must not present in case of inlining
      val method = context.method
      val line = context.caller.lineNumber
      val callerContext = convertContext(context.caller, null)
      addContext(method, line, null, callerContext)
    } else {
      assert(lexBlock != null)
      val method = context.method
      assert(method eq scope)
      val line = LineNumber.INVALID // TODO-DWARF shouldn't we use lexBlock.outer.line here?
      var outer = lexBlock.outer
      while (outer != null && outer.line == 0) { outer = outer.outer } // skip outers with line == 0
      val outerContext = if (outer != null) convertContext(context, outer) else scopeContext 
      addContext(method, line, lexBlock, outerContext)
    }

    // Build intervals tree
    val state = new mutable.LinkedHashMap[DebugInlineContext, Interval]
    state(scopeContext) = root

    codeOriginLabels foreach {
      case label @ SourceCodeLabel(_context, _, _, scope) =>
        val context = convertContext(_context, scope)
        val callStack = context.reversedCallStack
        for (newContext <- callStack if !state.contains(newContext)) {
          val newInterval = new Interval(newContext, label.position, state(newContext.callerContext))
          // lex-blocks and local vars for now are supported only for cj-code, they can be absent
          if (newContext.lexBlock != null) {
            val line = newContext.lexBlock.line
            val column = newContext.lexBlock.column
            lbLocals.get(line, column).foreach(locals => newInterval.localVariables.addAll(locals))
          }
          state(newContext) = newInterval
        }

        for ((oldContext, interval) <- state.toList if !callStack.contains(oldContext)) {
          interval.setEnd(label.position)
          state.remove(oldContext)
        }
      case _ =>
    }

    assert(state(scopeContext) == root)
    state.values.foreach(_.setEnd(seg.length))
  }
}
