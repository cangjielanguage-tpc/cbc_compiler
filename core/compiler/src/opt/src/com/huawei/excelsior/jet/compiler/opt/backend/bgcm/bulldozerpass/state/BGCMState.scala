/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.state

import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.heuristics.SpillHeuristics
import com.huawei.excelsior.jet.compiler.opt.backend.{BackEnd, RegFiles}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.util.Callback

/** Interpretation state for UpwardAI of BGCM. Mostly delegates API to local and loop LRS and
  * implements three main functions:
  *
  *   1) process(node) - implements generation step for `node`, changing LRS bits for results and arguments
  *
  *   2) calculateDiffs(node) - predicts result of `process`, informing how will change register pressure and
  *                             how many spills required to make generation step on `node`
  *
  *   3) appendNode(node, tryToRegisters) - basic API for CFG knots and loops processing. Appends node to
  *                             registers (if it is required and possible), otherwise appends it to storage.
  *
  * @author conwor
  */
trait BGCMState extends LRS { self: Universe with RegFiles with BackEnd with SpillHeuristics =>

  import RegFile.*


  case class RegFileDiff(file: RegFile, spillAmount: Int, pressureDiff: Int /*, loadedConstants: Int, temporals: Int */)


  class State(val local: LocalLRS, val loop: LoopLRS, val cold: Boolean) {

    private var fileLimitDecrement: RegFile => Int = _ => 0

    private def currentLimitOfFile(file: RegFile) = limitOfFile(file) - fileLimitDecrement(file)

    def withFileLimitDecrement(decrement: RegFile => Int)(action: => Unit): Unit = {
      val oldDecrement = fileLimitDecrement
      fileLimitDecrement = f => oldDecrement(f) + decrement(f)
      action
      fileLimitDecrement = oldDecrement
    }

    /** Notifies when new node is appended to state (to registers or to storage). */
    val onAppend = new Callback[Node]

    /** Returns iterator over all live nodes. */
    def allNodes: Iterator[Node] = local.allNodes

    /** Returns true, iff `node` is alive. */
    def live(node: Node): Boolean = local.live(node)

    /** Returns true, iff `node` is alive and occupies register at the current moment. */
    def inRegister(node: Node): Boolean = local.inRegister(node)

    /** Returns true, iff `node` is alive and do not occupies register at the current moment. */
    def inStorage(node: Node): Boolean = local.inStorage(node)

    /** Occupies register for `node`. */
    def moveToRegisters(node: Node): Unit = moveToRegisters0(node, local.live(node))

    private def moveToRegisters0(node: Node, wasLive: Boolean): Unit = {
      local.moveToRegisters(node)
      // nothing to do with loop LRS
      if (!wasLive && local.live(node)) onAppend(node)
    }

    /** Removes `node` from registers to storage. */
    def moveToStorage(node: Node): Unit = moveToStorage0(node, local.live(node))

    private def moveToStorage0(node: Node, wasLive: Boolean): Unit = {
      local.moveToStorage(node)
      if (!cold) {
        loop.registerAsSpilled(node)
        Spill.register(node)
      }
      if (!wasLive && local.live(node)) onAppend(node)
    }

    /** Occupy the same resource for copy as the original and remove original. */
    def replace(original: Node, copy: Node): Unit = {
      if (live(original)) {
        if (inRegister(original)) {
          remove(original)
          moveToRegisters0(copy, wasLive = true)
        } else {
          assert(inStorage(original))
          remove(original)
          moveToStorage0(copy, wasLive = true)
        }
      }
    }

    /** Removes `node` from state during processing. */
    def remove(node: Node): Unit = {
      local.remove(node)
      assert(!loop.fromContext(node)) // It's strange to process loop context nodes
    }

    /** Returns register pressure of `file`. */
    def registersPressure(file: RegFile): Int = local.registersPressure(file)

    /** Verify that for all register files its pressure is not overrun limit.
      * Optional `additionalPressure` appended to current RP. */
    def checkLimit(additionalPressure: RegFile => Int = { _ => 0 }): Unit = {
      def fileCheckLimit(file: RegFile): Unit = {
        assert(registersPressure(file) + additionalPressure(file) <= currentLimitOfFile(file))
      }
      allRegFiles foreach { file => fileCheckLimit(file) }
    }

    /** Returns set of nodes, occupied registers of `file`. */
    def registerNodesSet(file: RegFile): collection.Set[Node] = local.registerNodesSet(file)

    /** Returns set of nodes, occupied registers of all files. */
    def registerNodesSet(): collection.Set[Node] = registerNodesSet(IREG) ++ registerNodesSet(FREG)

    /** Returns set of nodes, containing in storage of `file`. */
    def storageNodesSet(file: RegFile): collection.Set[Node] = local.storageNodesSet(file)

    /** Returns set of nodes, containing in storage of all files. */
    def storageNodesSet(): collection.Set[Node] = storageNodesSet(IREG) ++ storageNodesSet(FREG)

    /** Returns iterator over loop components from outermost to innermost (current). */
    def loopsInOrderFromOutermost(): Iterator[LoopLRS] = loop.loopsInOrderFromOutermost()


    /** Process node. In general, removes node results from state and occupies registers for node argument.
      * But there are few details.
      *
      * For hint nodes special actions defined:
      *   1) SpillAssert -> move argument to storage instead of registers
      *   2) StoreHint -> do not change argument status (backend will just guarantee that below this hint
      *        its `stored` value will be in storage and may be in registers)
      *   3) Spill | StoreLoadHint -> move argument to registers (this behaviour included in general pattern,
      *        but we write this explicitly to avoid misunderstanding)
      *
      * TODO: redesign and describe special behaviour about transfers.
      *
      * Also, special callback `insertSpillHintsForArgs` passed to this method. It requires to accurate support
      * arguments of node, which was in storage below node. If argument was in storage, we should:
      *   1) Guarantee it's absence on registers immediately below node
      *   2) Have it on registers above node
      * To accomplish this we insert two hints (StoreHint above node and SpillHint below) and collect incoming
      * edge to special set, used by backend (register allocator is free to re-use argument register in node
      * generation). But if we insert StoreHint node before calling node process, it's (StoreHint) processing
      * will rise argument to registers, which may overrun register file limit, when we will check it in node
      * processing.
      *
      * In the future backend will remove this arguments from registers during node generation, SpillHint will
      * be replaced to SpillAssertHint and all these problems will be eliminated.
      * */
    def process(node: Node, insertSpillHintsForArgs: => Unit): Unit = {
      // 1. Remove node results from state and check register files limits, taken node spoiled into account.
      node.groupedValueResults foreach remove

      // TODO: consider to take into account only registers spoiled on NORMAL ExitKind
      checkLimit({ file => spoiledRegistersAmount(node, file) })

      // 2. Move node arguments to state to corresponding resources (in general to registers).
      insertSpillHintsForArgs
      for (inEdge @ Edge(arg, _) <- node.groupedValueInEdges) {
        if (State.argumentWillBeInRegisters(inEdge, live(arg))) moveToRegisters(arg)
        else if (State.argumentWillBeInStorage(inEdge, live(arg))) moveToStorage(arg)
      }

      // 3. Finally check register files limits.
      checkLimit()
    }


    /** Calculate difference in register files, that will be iff `node` will be selected and generated.
      * Used same utilities as `process` function and calculates required amount of spills, to avoid
      * checkLimit fails in `process`.
      * */
    def calculateDiffs(node: Node): Seq[RegFileDiff] = allRegFiles map { file =>
      val inEdges = node.groupedValueInEdges filter { e => regFileOf(e.source) == file }
      val results = (node.groupedValueResults filter { regFileOf(_) == file }).toList

      val resultsOccupyingRegisters = results count inRegister
      val storedResultsGeneratedInRegisters = results count { r => inStorage(r) && !generatedInStorage(r) }
      val registerResults = resultsOccupyingRegisters + storedResultsGeneratedInRegisters

      // TODO: consider to take into account only registers spoiled on NORMAL ExitKind
      val temporals = spoiledRegistersAmount(node, file)

      val newRegisterArgsSet = (inEdges collect { case e if !inRegister(e.source) && State.argumentWillBeInRegisters(e, live(e.source)) => e.source }).toSet
      val newRegisterArgs = newRegisterArgsSet.size

      val startRP = registersPressure(file)

      // 1. Results from storage to registers
      val belowRP = startRP + storedResultsGeneratedInRegisters

      // 2. All results out of registers, temporals to registers
      val middleRP = belowRP - registerResults + temporals

      // 3. Temporals out of registers, new arguments to registers
      val aboveRP = middleRP - temporals + newRegisterArgs

      val maxRP = belowRP max middleRP max aboveRP
      val spillAmount = 0 max (maxRP - currentLimitOfFile(file))

      // val loadedConstants = newRegisterArgsSet count { _.isInstanceOf[Constant] }
      RegFileDiff(file, spillAmount, aboveRP - startRP /*, loadedConstants, temporals */)
    }

    /** Appends `node` to state. If `tryToRegisters` is true and there is free register, or we can free register
      * due to some zero-cost rematerialization node, move `node` to registers, otherwise - to storage. */
    def appendNode(node: Node, tryToRegisters: Boolean, isExceptionMerge: Boolean = false): Unit = {

      /** Returns true, iff there is free register for given `node`. */
      def haveRegisterFor(node: Node): Boolean = {
        val file = regFileOf(node)
        registersPressure(file) < currentLimitOfFile(file)
      }

      /** Tries to free register for given `node` by spilling some zero-cost rematerialization node instead. */
      def tryToFreeRegisterDueToZeroCostRematerializationFor(node: Node): Boolean = {
        // TODO: think about spill already spilled node instead of given `node`
        if (zeroCostRematerialization(node)) return false // Maybe there is a reason to spill one constant for another?

        if (isExceptionMerge) {
          // During exception merge we should not spill out of registers even constants because it may conflicts with
          // throwable node generation. E.g., if some constant is spilled below node, but will be in registers above
          // node (as its argument) we calculate spillAmount based on this fact and make sure, that there will be free
          // register for constant above node. Then we generate node (constant will occupy register) and, before
          // insert store hints, make exception merge. If we will spill constant out of register then, during store
          // hint insertion, we will overrun register file limit.
          // TODO: describe in documentation and think about refactoring
          return false
        }

        val regSet = registerNodesSet(regFileOf(node))
        regSet find zeroCostRematerialization match {
          case Some(x) => moveToStorage(x); true
          case _ => false
        }
      }

      if (tryToRegisters && (haveRegisterFor(node) || tryToFreeRegisterDueToZeroCostRematerializationFor(node))) {
        moveToRegisters(node)
      } else {
        moveToStorage(node)
      }
    }
  }

  object State {

    /** Returns pair of flags (onRegister, inStorage) defines, on which type of resource will be passed value by `edge`.
      *
      * (true,  false) - result means that value may be passed only on register
      * (false, true)  - result means that value may be passed only in storage (e.g., SpillAssert incoming value)
      * (true,  true)  - result means that value may be passed any way, so BGCM may choose
      * (false, false) - result means that value is passed some special way (e.g., through special register file)
      *
      * This function is BGCM-specific, that is why it does not implemented in MachineDescription.
      * TODO: try to make it full-backend usable.
      * */
    def passedValueResourceType(edge: Edge): (Boolean, Boolean) = (edge.source, edge.target) match {
      case (_, hint: BulldozerHint) if hint.spillAssert => (false, true)
      case (_: Param, _: LoadTailParam) => (false, true)
      case (source, _) if inSpecialFile(source) => (false, false)
      case (_: Constant, _) if mayBeUsedAsImmediate(edge) => (true, true)
      case (_, SpoiledArgSaver(_)) => (true, true)
      case _ => (true, false)
    }

    /** Returns true, iff value, used by `edge` will be strictly on register after `edge` target processing. */
    def argumentWillBeInRegisters(edge: Edge, isAlive: Boolean): Boolean = passedValueResourceType(edge) match {
      case (true, false) => true
      case (true, true) => !isAlive && generatedOnRegister(edge.source)
      case _ => false
    }

    /** Returns true, iff value, used by `edge` will be strictly in storage after `edge` target processing. */
    def argumentWillBeInStorage(edge: Edge, isAlive: Boolean): Boolean = passedValueResourceType(edge) match {
      case (false, true) => true
      case (true, true) => !isAlive && generatedInStorage(edge.source)
      case _ => false
    }
  }
}
