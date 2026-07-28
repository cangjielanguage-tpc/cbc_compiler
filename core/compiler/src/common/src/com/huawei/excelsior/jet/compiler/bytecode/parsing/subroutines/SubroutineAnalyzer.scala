/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing.subroutines

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.bytecode.*
import com.huawei.excelsior.jet.compiler.bytecode.parsing.BlockDataFlowParser.RET_ADDR_KIND
import com.huawei.excelsior.jet.compiler.bytecode.parsing.DataFlowAnalyzer.WorkListBlockProcessResult
import com.huawei.excelsior.jet.compiler.bytecode.parsing.subroutines.SubroutineAnalyzer.{State, newTopState, notSubroutine}
import com.huawei.excelsior.jet.compiler.bytecode.parsing.{BlockDataFlowParser, DataFlowAnalyzer, DataFlowMergeResult}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.asVerifiableMethodType
import com.huawei.excelsior.jet.compiler.verifier.{VerifiableMethod, VerifiableMethodType, VerificationUnit}

import scala.annotation.nowarn
import scala.collection.mutable

/** Computes ret instruction targets performing data flow analysis. 
  * TODO: move to Java-specific part.
  */
object SubroutineAnalyzer {
  private val NOT_SUBROUTINE = new Subroutine[Null](null)

  private def notSubroutine[B]: Subroutine[B] = NOT_SUBROUTINE.asInstanceOf[Subroutine[B]]

  private def newTopState[B](verification: VerificationUnit) =
    new State[B](verification, null, null, -1)

  /** Dataflow analysis lattice element.
    * Bytecode local and stack registers may have only two elements: a concrete [[Subroutine]] descriptor or
    * non-subroutine. We also collect subroutines call stack as they can be nested.
    *
    * State is not private because it is used in type parameter of SubroutineAnalyzer.
    *
    * @tparam B basic block
    */
  class State[B](
    verification: VerificationUnit,
    var callStack: List[Subroutine[B]],
    var slotValues: Array[Subroutine[B]],
    var stackHeight: Int
  ) extends DataFlowAnalyzer.State[State[B]] {

    def this(verification: VerificationUnit, slots: Slots) =
      this(verification, List.empty, Array.tabulate(slots.totalCount)(_ => notSubroutine), 0)

    def isTop = callStack == null

    /** Forks the input state. */
    def copy() = new State[B](verification, callStack, slotValues.clone(), stackHeight)

    /** Makes an exceptional state that we will pass by exceptional control flow. */
    def xCopy(slots: Slots): State[B] = {
      val xSlotValues = new Array[Subroutine[B]](slots.totalCount)
      if (slots.localsCount > 0) {
        val idx = slots.localIdx(0)
        Array.copy(slotValues, idx, xSlotValues, idx, slots.localsCount)
      }

      assert(slots.stackCount > 0)
      val idx = slots.stackIdx(0)
      for (i <- idx until idx + slots.stackCount) {
        xSlotValues(i) = notSubroutine
      }

      new State(verification, callStack, xSlotValues, 1)
    }

    override def mergeFrom(that: State[B]): DataFlowMergeResult = {
      if (this.isTop) {
        this.callStack = that.callStack
        this.slotValues = that.slotValues.clone()
        this.stackHeight = that.stackHeight
        return DataFlowMergeResult.INITIALIZED
      }

      verification.verifyThat(this.stackHeight == that.stackHeight,
        "Stack height mismatch: %d != %d", this.stackHeight, that.stackHeight)

      var changed = false
      for (i <- 0 until this.slotValues.length) {
        if (this.slotValues(i) != that.slotValues(i) && this.slotValues(i) != notSubroutine) {
          this.slotValues(i) = notSubroutine
          changed = true
        }
      }

      if (this.callStack != that.callStack) {
        // The call stack merge is non-associative unfortunately.
        // E.g. ABCD merge ACBD gives ABCD, while ACBD merge ABCD gives ACBD.
        // That is because at the time of merge we do not actually know which subroutine call
        // will reach its ret instruction. According to the JVMS spec, we should ignore all subroutines that
        // do not reach their ret instructions and treat their jsr's instructions as just special goto.
        // Thus if we have ABCD merge ACBD, we only know that either B or C subroutines definitely do not
        // reach their rets but we do not know which one. So we have to leave both and the order does not matter
        // as we will pop excess ones at ret instruction processing.
        // Theoretically we can lead the merge to associative one to meet data flow analysis fundamental
        // requirement by keeping possible subroutines sets as call stack elements so ABCD merge ACBD would
        // give A{B,C}D result but we were not able to find a counter-example when it makes sense.
        // So keep it simple.
        val filtered = this.callStack filter that.callStack.contains
        if (this.callStack != filtered) {
          this.callStack = filtered
          changed = true
        }
      }

      if (changed) DataFlowMergeResult.CHANGED else DataFlowMergeResult.UNCHANGED
    }
  }
}

/** Computes ret instruction targets performing data flow analysis.
  *
  * @tparam B basic block
  */
abstract class SubroutineAnalyzer[B >: Null](
  env: Environment, method: VerifiableMethod,
  jsrInfos: collection.Map[Int, JsrInfo[B]], // jsrPC -> JsrInfo
  verify: Boolean
) extends DataFlowAnalyzer.WorkListVersion[B, State[B]](verify, method) { sa =>

  protected def blockStartPC(block: B): Int

  protected def blockEndPC(block: B): Int

  private val cp = method.getDeclaringClass.getClassConstantPool
  private val codeAttr = method.codeAttribute
  private val slots = new Slots(codeAttr.maxLocals, codeAttr.maxStack)
  private val subroutines = mutable.HashMap.empty[B, Subroutine[B]] // entryBlock -> Subroutine

  def analyzeAndCollectSubroutines(): Iterable[Subroutine[B]] = {
    assert(subroutines.isEmpty)
    analyze()
    // Subroutines without ret block are just "jsr-as-goto-way".
    subroutines.values.filter(_.hasRet)
  }

  private val inputStates = mutable.HashMap.empty[B, State[B]]

  override protected def inputState(block: B) = {
    inputStates.getOrElseUpdate(block, {
      if (block == entryBlock) {
        new State[B](this, slots)
      } else {
        newTopState(this)
      }
    })
  }

  private def getSubroutine(pc: Int) = {
    val entryBlock = jsrInfos(pc).targetBlock
    subroutines.getOrElseUpdate(entryBlock, new Subroutine[B](entryBlock))
  }

  override protected def processBlock(block: B, inputState: State[B]) = {
    val state = inputState.copy()
    val xState = if (hasHandlers(block)) inputState.xCopy(slots) else null
    val blockProcessor = new BlockProcessor(block, state, xState)
    blockProcessor.iterateBytecode(codeAttr, blockStartPC(block), blockEndPC(block))
    new WorkListBlockProcessResult[B, State[B]](
      state, xState, blockProcessor.retSuccs, blockProcessor.retToReprocess)
  }

  private class BlockProcessor(block: B,
                               state: State[B],
                               xState: State[B]) // null iff block has no handlers
    extends BlockDataFlowParser[Subroutine[B]](sa.slots, state.stackHeight,
      sa.verify, sa.verificationContext) {

    var retSuccs = Iterator.empty[B]
    var retToReprocess: B = _
    private var curBC = 0

    override def iterateBytecode(code: MethodCodeAttribute, startBC: Int, endBC: Int): Unit = {
      super.iterateBytecode(code, startBC, endBC)
      state.stackHeight = curStackHeight
    }

    override protected def writeSlot(slotIdx: Int, value: Subroutine[B]): Unit = {
      state.slotValues(slotIdx) = value
      if (xState != null) {
        val oldXValue = xState.slotValues(slotIdx)
        if (oldXValue != notSubroutine && oldXValue != value) {
          xState.slotValues(slotIdx) = notSubroutine
        }
      }
    }

    override protected def readSlot(slotIdx: Int) = state.slotValues(slotIdx)

    override protected def longHalfOf(value: Subroutine[B]) = notSubroutine

    override def startInstruction(offset: Int, nextOffset: Int): Unit = curBC = offset

    override def jsr(bc: Int): Unit = {
      val s = getSubroutine(curBC)
      push(RET_ADDR_KIND, s)
      state.callStack = s +: state.callStack
      val jsrInfo = jsrInfos(curBC)
      if (s.connectToJsr(jsrInfo) && s.hasRet) {
        // If ret block of this subroutine was already processed by analysis
        // we need to process it again to process next block after this jsr block.
        // So ret block may be processed again and again but we ignore this inefficiency
        // because subroutines are rare.
        retToReprocess = s.retBlock
      }
    }

    override def ret(variable: Int): Unit = {
      val s = read(RET_ADDR_KIND, variable)
      verifyThat(s != notSubroutine, "No valid return address in local %d", variable)

      // Prohibit any other usage of this return address. Also minimize number of state merges.
      write(RET_ADDR_KIND, variable, notSubroutine)

      // As call stack may have subroutines that do not reach their rets,
      // we should pop them until we find a suitable one.
      state.callStack = state.callStack dropWhile (_ != s)
      verifyThat(state.callStack.nonEmpty, "ret from invalid subroutine")

      if (s.hasRet) {
        verifyThat(s.retBlock == block, "Subroutine returned by non-single ret instruction")
      } else {
        s.connectToRet(block)
      }

      // All blocks next to reachable jsr blocks are implicit successors of this ret block.
      retSuccs = s.jsrs.map(_.nextBlock).iterator
    }

    // region Other bytecode instructions parsing

    // TODO: All code below was copy-pasted from SimpleDataFlowParser :/
    //       Try to unify it with future implementations of bytecode processors.
    @nowarn("msg=match may not be exhaustive")
    override def pushCPEntry(index: Int): Unit = cp.getTag(index) match {
      case Tag.INTEGER => push(BytecodeTypeKind.INT, notSubroutine)
      case Tag.FLOAT => push(BytecodeTypeKind.FLOAT, notSubroutine)
      case Tag.LONG => push(BytecodeTypeKind.LONG, notSubroutine)
      case Tag.DOUBLE => push(BytecodeTypeKind.DOUBLE, notSubroutine)

      case Tag.STRING | Tag.CLASS | Tag.METHOD_TYPE | Tag.METHOD_HANDLE =>
        push(BytecodeTypeKind.CLASS, notSubroutine)
    }

    override def pushConst(tpe: BytecodeTypeKind, value: Int): Unit = push(tpe, notSubroutine)

    override def arithOp(tpe: BytecodeTypeKind, op: ArithOp): Unit = {
      op match {
        case ArithOp.NEG => // Only one arg
        case op if op.isShift => pop(BytecodeTypeKind.INT) // The second arg is the shift distance
        case _ => pop(tpe)
      }

      pop(tpe)

      push(if (op.isCmp) BytecodeTypeKind.INT else tpe, notSubroutine)
    }

    override def convert(op: ConvertOp): Unit = {
      pop(op.srcKind)
      push(op.dstKind, notSubroutine)
    }

    override def increment(local: Int, delta: Int): Unit = {
      read(BytecodeTypeKind.INT, local)
      write(BytecodeTypeKind.INT, local, notSubroutine)
    }

    override def arrayGet(tpe: BytecodeTypeKind): Unit = {
      pop(BytecodeTypeKind.INT)
      pop(BytecodeTypeKind.ARRAY)
      push(tpe, notSubroutine)
    }

    override def arrayPut(tpe: BytecodeTypeKind): Unit = {
      pop(tpe)
      pop(BytecodeTypeKind.INT)
      pop(BytecodeTypeKind.ARRAY)
    }

    override def fieldOp(index: Int, akind: FieldAccessKind): Unit = {
      val tpe = cp.getFieldTypeKind(index)
      fieldOp(akind, tpe)
    }

    private def fieldOp(akind: FieldAccessKind, tpe: BytecodeTypeKind): Unit = {
      if (akind.isStatic) {
        if (akind.isWrite) {
          pop(tpe)
        } else {
          push(tpe, notSubroutine)
        }
      } else {
        if (akind.isWrite) {
          pop(tpe)
          pop(BytecodeTypeKind.CLASS)
        } else {
          pop(BytecodeTypeKind.CLASS)
          push(tpe, notSubroutine)
        }
      }
    }

    override def invoke(index: Int, akind: MethodAccessKind): Unit = {
      val tp = env.getTypeProvider
      val methodType = MethodType.jbcErased(cp.getRefSignature(index), tp, akind.hasObjectArg)
      invoke(asVerifiableMethodType(methodType))
    }

    private def invoke(methodType: VerifiableMethodType): Unit = {
      val paramCount = methodType.parameterCount
      for (i <- paramCount - 1 to 0 by -1) {
        pop(methodType.parameterTypeKind(i))
      }

      val returnKind = methodType.returnTypeKind
      if (!returnKind.isVoid) {
        push(returnKind, notSubroutine)
      }
    }

    override def monitorEnter(): Unit = pop(BytecodeTypeKind.CLASS)

    override def monitorExit(): Unit = pop(BytecodeTypeKind.CLASS)

    override def doNew(index: Int): Unit = push(BytecodeTypeKind.CLASS, notSubroutine)

    override def instanceOf(index: Int): Unit = {
      pop(BytecodeTypeKind.CLASS)
      push(BytecodeTypeKind.INT, notSubroutine)
    }

    override def checkCast(index: Int): Unit = {
      val x = pop(BytecodeTypeKind.CLASS)
      push(BytecodeTypeKind.CLASS, x)
    }

    override def doThrow(): Unit = {
      pop(BytecodeTypeKind.CLASS)
      clearStack()
    }

    override def newPrimitiveArray(tpe: BytecodeTypeKind): Unit = {
      pop(BytecodeTypeKind.INT)
      push(BytecodeTypeKind.ARRAY, notSubroutine)
    }

    override def newObjectArray(index: Int): Unit = {
      pop(BytecodeTypeKind.INT)
      push(BytecodeTypeKind.ARRAY, notSubroutine)
    }

    override def newMultiObjectArray(index: Int, dimNum: Int): Unit = {
      for (_ <- 0 until dimNum) {
        pop(BytecodeTypeKind.INT)
      }
      push(BytecodeTypeKind.ARRAY, notSubroutine)
    }

    override def arrayLength(): Unit = {
      pop(BytecodeTypeKind.CLASS)
      push(BytecodeTypeKind.INT, notSubroutine)
    }

    override def unaryIf(tpe: BytecodeTypeKind, op: CompareOp, bc: Int): Unit = pop(tpe)

    override def binaryIf(tpe: BytecodeTypeKind, op: CompareOp, bc: Int): Unit = {
      pop(tpe)
      pop(tpe)
    }

    override def jump(bc: Int): Unit = {}

    override def doReturn(tpe: BytecodeTypeKind, isLastBytecode: Boolean): Unit = {
      if (!tpe.isVoid) {
        pop(tpe)
      }
      clearStack()
    }

    override def tableSwitch(bcDefault: Int, lowMatch: Int, highMatch: Int, bcTargets: Array[Int]): Unit =
      pop(BytecodeTypeKind.INT)

    override def lookupSwitch(bcDefault: Int, matches: Array[Int], bcTargets: Array[Int]): Unit =
      pop(BytecodeTypeKind.INT)

    // endregion
  }
}
