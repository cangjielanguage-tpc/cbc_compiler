/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.onbytecode

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.bytecode.*
import com.huawei.excelsior.jet.compiler.bytecode.parsing.HandlersTreeMap.{reachableHandlers, wouldCatchAnyException}
import com.huawei.excelsior.jet.compiler.bytecode.parsing.XHInfo
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.StructuredLockingAnalyzer.{State, analysisFailed}
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.onbytecode.StructuredLockingAnalyzerOnBytecode.{LocalValue, SomeValue, UnknownValue, Value}
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.{BlockStructure, StructuredLockingAnalyzer}
import com.huawei.excelsior.jet.util.{DisjointSet, SuffixTree}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Implementation of [[StructuredLockingAnalyzer]] based on bytecode parsing. */
object StructuredLockingAnalyzerOnBytecode {
  private sealed class Value

  private object UnknownValue extends Value

  private class SomeValue extends Value

  private case class LocalValue(localIdx: Int) extends Value
}

abstract class StructuredLockingAnalyzerOnBytecode[B >: Null](codeAttr: MethodCodeAttribute)
  extends StructuredLockingAnalyzer[B, MonitorEnter[B], MonitorExit[B]] {

  private val mayThrowCache = mutable.HashMap.empty[B, Boolean]

  protected def blockStartPC(block: B): Int

  protected def blockEndPC(block: B): Int

  protected def handlers(block: B): SuffixTree[XHInfo[B]]

  override protected def areMatching(exit: MonitorExit[B], enter: MonitorEnter[B]) =
    enter.aliasedLocals contains exit.local

  override final protected def handlerBlocks(block: B): Iterator[B] = {
    val handlersTree = handlers(block)

    if (handlersTree == null || neverThrows(block)) {
      return Iterator.empty
    }

    reachableHandlers(handlersTree).map(_.handler).iterator
  }

  override final protected def blockHasExceptionalExit(block: B): Boolean = {
    if (neverThrows(block)) {
      return false
    }

    // order is important

    val handlersTree = handlers(block)
    if (handlersTree == null) {
      return true
    }

    reachableHandlers(handlersTree) forall (!wouldCatchAnyException(_))
  }

  override protected def analyze(): Unit = {
    super.analyze()
    checkLocalsSpoiling()
  }

  /** @see [[BlockProcessor]] */
  private def checkLocalsSpoiling(): Unit = {
    for ((exit, enter) <- monitorPairs) {
      assert(areMatching(exit, enter))
      if (enter.spoiledLocals contains exit.local) {
        analysisFailed(s"$exit uses spoiled local of $enter")
      }
    }
  }

  private def processBlockImpl(block: B, inputState: State[MonitorEnter[B]]) = {
    val bc = new BytecodeIterator(codeAttr)
    val blockProcessor = new BlockProcessor(block, inputState.activeEnters)
    bc.iterate(blockProcessor, blockStartPC(block), blockEndPC(block))
    blockProcessor
  }

  override final protected def analyzeOneBlock(block: B, inputState: State[MonitorEnter[B]]) = {
    val blockProcessor = processBlockImpl(block, inputState)
    mayThrowCache.put(block, blockProcessor.mayThrow) ensuring (_.isEmpty)
    blockProcessor.blockStructure
  }

  private def neverThrows(block: B): Boolean = {
    mayThrowCache.get(block) match {
      case Some(mayThrow) => !mayThrow

      // During debug we may try to call neverThrows() before analyzing block.
      case None if debugEnabled => !processBlockImpl(block, new State[MonitorEnter[B]]).mayThrow

      case None =>
        shouldNotReachHere(s"$block must be analyzed via analyzeOneBlock() before calling neverThrows()")
    }
  }

  override final protected def debugEnabled = false

  override final protected def allInputStatesForDebug = inputStates

  override protected def blockDebugInfo(block: B, state: State[MonitorEnter[B]]) = {
    val sb = new StringBuilder

    if (neverThrows(block)) sb.append("neverThrows")
    if (blockHasNormalExit(block)) sb.append("nExit")
    if (blockHasExceptionalExit(block)) sb.append("xExit")

    sb.toString
  }

  /** Scalac may produce the following code for synchronized section:
    * {{{
    *   aload 3
    *   dup
    *   astore 7
    *   monitorenter
    *   ...
    *   aload 3
    *   monitorexit
    *   ...
    *   aload 7
    *   monitorexit
    * }}}
    * therefore we have to do simple alias analysis of monitor operation arguments. Note that the following code
    * ``should not`` be considered as structurally locked:
    * {{{
    *   aload 3
    *   dup
    *   astore 7
    *   monitorenter
    *   ...
    *   aconst_null
    *   astore 3
    *   ...
    *   aload 3
    *   monitorexit
    *   ...
    *   aload 7
    *   monitorexit
    * }}}
    * so we maintain a list of "spoiled locals" for every MonitorEnter.
    * The results are used in [[StructuredLockingAnalyzerOnBytecode.checkLocalsSpoiling]].
    */
  private class BlockProcessor(block: B, activeMonitorEnters: Iterable[MonitorEnter[B]]) extends BytecodeProcessor {
    private val currentStack = ArrayBuffer.empty[Value]
    private var bs: BlockStructure[MonitorEnter[B], MonitorExit[B]] = _
    var mayThrow = false
    private val aliases = new DisjointSet.ofInt(codeAttr.maxLocals)

    def blockStructure = if (bs != null) bs else BlockStructure.Empty

    private def makeLocalAlias(l1: Int, l2: Int): Unit = aliases.union(l1, l2)

    private def allAliases(local: Int): Seq[Int] = {
      val eqClass = aliases.find(local)
      (0 until codeAttr.maxLocals) filter (aliases.find(_) == eqClass)
    }

    private def invalidateStackAndMayThrow(): Unit = {
      invalidateStack()
      markPotentiallyThrowing()
    }

    private def markPotentiallyThrowing(): Unit = {
      mayThrow = true
    }

    private def invalidateStack(): Unit = currentStack.clear()

    private def push(v: Value): Unit = currentStack += v

    private def pop() = {
      if (currentStack.isEmpty) {
        UnknownValue
      } else {
        currentStack.remove(currentStack.size - 1)
      }
    }

    override def pushLocal(tpe: BytecodeTypeKind, index: Int): Unit =
      if (tpe.isReference) push(LocalValue(index)) else invalidateStack()

    override def storeLocal(tpe: BytecodeTypeKind, index: Int): Unit = if (tpe.isReference) storeLocal(index)

    private def storeLocal(index: Int): Unit = {
      for (m <- activeMonitorEnters; local <- m.aliasedLocals if local == index) {
        m.spoiledLocals += index
      }

      // test if prev operation with stack was dup, make following transformation
      //   ... Some Some  --> ... Local(index)
      val first = pop()
      first match {
        case _: SomeValue =>
          val second = pop()
          if (first eq second) {
            push(LocalValue(index))
          } else {
            invalidateStack()
          }

        case LocalValue(localIdx) => makeLocalAlias(index, localIdx)

        case _ => invalidateStack()
      }
    }

    override def stackOp(op: Bytecode): Unit = {
      if (op != Bytecode.DUP) {
        invalidateStack()
      } else {
        // ..., x => ..., x, x
        val currentTop = pop()
        val dup = if (currentTop == UnknownValue) new SomeValue else currentTop
        push(dup)
        push(dup)
      }
    }

    override def monitorEnter(): Unit = {
      markPotentiallyThrowing() // at least NPE or even OOM
      assert(bs == null)
      bs = pop() match {
        case LocalValue(localIdx) => BlockStructure.Enter(new MonitorEnter(block, allAliases(localIdx)))
        case _ => BlockStructure.Error
      }
    }

    override def monitorExit(): Unit = {
      // we assume that in structured locking case monitor operations may not throw
      assert(bs == null)
      bs = pop() match {
        case LocalValue(localIdx) => BlockStructure.Exit(new MonitorExit[B](block, localIdx))
        case _ => BlockStructure.Error
      }
    }

    override def pushCPEntry(index: Int): Unit                                  = invalidateStackAndMayThrow()
    override def arithOp(tpe: BytecodeTypeKind, op: ArithOp): Unit              = invalidateStackAndMayThrow()
    override def arrayGet(tpe: BytecodeTypeKind): Unit                          = invalidateStackAndMayThrow()
    override def arrayPut(tpe: BytecodeTypeKind): Unit                          = invalidateStackAndMayThrow()
    override def fieldOp(index: Int, akind: FieldAccessKind): Unit = invalidateStackAndMayThrow()
    override def invoke(index: Int, akind: MethodAccessKind): Unit = invalidateStackAndMayThrow()
    override def doNew(index: Int): Unit                                        = invalidateStackAndMayThrow()
    override def instanceOf(index: Int): Unit                                   = invalidateStackAndMayThrow()
    override def doThrow(): Unit                                                = invalidateStackAndMayThrow()
    override def newPrimitiveArray(tpe: BytecodeTypeKind): Unit                 = invalidateStackAndMayThrow()
    override def newObjectArray(index: Int): Unit                               = invalidateStackAndMayThrow()
    override def newMultiObjectArray(index: Int, dimNum: Int): Unit             = invalidateStackAndMayThrow()
    override def arrayLength(): Unit                                            = invalidateStackAndMayThrow()

    override def pushConst(tpe: BytecodeTypeKind, value: Int): Unit             = invalidateStack()
    override def convert(op: ConvertOp): Unit                                   = invalidateStack()
    override def unaryIf(tpe: BytecodeTypeKind, op: CompareOp, bc: Int): Unit   = invalidateStack()
    override def binaryIf(tpe: BytecodeTypeKind, op: CompareOp, bc: Int): Unit  = invalidateStack()
    override def jsr(bc: Int): Unit                                             = invalidateStack()
    override def doReturn(tpe: BytecodeTypeKind, isLastBytecode: Boolean): Unit = invalidateStack()
    override def lookupSwitch(bcDefault: Int, matches: Array[Int], bcTargets: Array[Int]): Unit = invalidateStack()
    override def tableSwitch(bcDefault: Int, lowMatch: Int, highMatch: Int, bcTargets: Array[Int]): Unit = invalidateStack()

    override def checkCast(index: Int): Unit = markPotentiallyThrowing()

    override def increment(local: Int, delta: Int): Unit = {}
    override def jump(bc: Int): Unit                     = {}
    override def ret(value: Int): Unit                   = {}
  }
}
