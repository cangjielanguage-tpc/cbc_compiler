/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.util.ScalaCollections.{iterateUntilNull, lastElement}

/** Inline context is an attribute of operation, which contains information
  * about position of this operation before inline optimization in compiler.
  *
  * Each inline context element contains method (symlevel object) and reference
  * to the inline context of the caller. Also, non-top inline context contains
  * line number and bytecode position of inlined invoke instruction.
  *
  * A top-level inline context element corresponds to the whole method scope.
  * Node positions always reference top-level inline context. All nodes in a method scope
  * share the same top-level inline context.
  *
  * Non-top inline context (inline context, referenced by another inline context) specifies the position
  * of the inlined invoke instruction.
  *
  * @author conwor
  * @author alexm
  */
object InlineContext {
  /** Constructs a top level inline context wrapping the whole root method. */
  def newRoot(rootMethod: Method) = InlineContext(rootMethod, LineNumber.INVALID, BytecodeOffset.INVALID, null)

  /** Constructs a top level inline context wrapping the whole callee method,
    * which is inlined into the caller at given line number and bytecode position.
    */
  def newInlined(calleeMethod: Method, callerLineNumber: Int, callerBytecodeOffset: Int, callerContext: InlineContext) = {
    assert(LineNumber.isValid(callerLineNumber))
    assert(BytecodeOffset.isValid(callerBytecodeOffset))
    val caller = InlineContext(callerContext.method, callerLineNumber, callerBytecodeOffset, callerContext.caller)
    InlineContext(calleeMethod, LineNumber.INVALID, BytecodeOffset.INVALID, caller)
  }
}

/** Construct inline context with given method, line number and bytecode offset of invoke instruction
  * within the method and the caller inline context.
  * This constructor creates top-level inline context if [[lineNumber]] is [[LineNumber.INVALID]].
  */
final case class InlineContext(
  /** Symlevel object of the method. */
  method: Method,

  /** Line number of invoke instruction in the method,
    * or [[LineNumber.INVALID]] if this context is top-level. */
  lineNumber: Int,

  /** Bytecode position of invoke instruction in the method,
    * or [[BytecodeOffset.INVALID]] if this context is top-level. */
  bytecodePos: Int,

  /** Caller inline context. */
  caller: InlineContext
) {
  assert(caller == null || !caller.isTopLevel)
  assert(lineNumber == LineNumber.INVALID || LineNumber.isValid(lineNumber))
  assert(bytecodePos == BytecodeOffset.INVALID || BytecodeOffset.isValid(bytecodePos))
  assert((lineNumber == LineNumber.INVALID) == (bytecodePos == BytecodeOffset.INVALID))

  /** Symlevel object of the method. */
  def klass = method.getDeclaringClass

  /** Tests if this inline context is a top-level. */
  def isTopLevel = lineNumber == LineNumber.INVALID

  /** Returns root method of this inline context. See [[InlineContext.newRoot]]. */
  def rootMethod: Method = lastElement(toRoot).get.method

  /** Returns whether given method is contained in this inline context. */
  def contains(method: Method): Boolean = toRoot.exists(_.method == method)

  /** Returns how many times given method is contained in this inline context. */
  def count(method: Method): Int = toRoot.count(_.method == method)

  /** Returns depth of this inline context. */
  def depth: Int = toRoot.size

  override def toString: String = toString(ignoreNumbers = false)

  def toString(ignoreNumbers: Boolean): String = {
    method.getFullName +
      (if (isTopLevel || ignoreNumbers) ":*" else s":$lineNumber@$bytecodePos") +
      (if (caller != null) s", called from ${caller.toString(ignoreNumbers)}" else "")
  }

  /** Returns iterator over inline contexts from current to root. */
  def toRoot = iterateUntilNull(this)(_.caller)
}
