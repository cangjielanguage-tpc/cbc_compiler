/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.state

import com.huawei.excelsior.jet.compiler.opt.backend.{BackEnd, RegFiles}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.graph.Loop

import scala.collection.mutable.ListBuffer

/** Utility classes representing abstract LRS-bits semantic. There are two concrete types of LRS-bits
  *   1) Local LRS, where :
  *         L bit means node, live at current moment
  *         R bit means that node live on registers
  *         S bit means that node live in storage
  *   2) Loop LRS, where:
  *         L bit means context node (argument or outsider of loop)
  *         R bit means that node was not spilled in loop
  *         S bit means that node was somewhere spilled in loop
  *
  * @author conwor
  */
trait LRS { self: Universe with BackEnd =>

  import RegFile.*


  /////////////////////////////////////////////////////////////////////////////

  /** Implementation of LRS bits for one registers file. */
  private[state] class OneFileLRSImpl private (val registers: Sets[Node]#QSet,
                                               val storage: Sets[Node]#QSet,
                                               var spillPressure: Int) {

    def this() = this(Sets[Node].newQSet, Sets[Node].newQSet, 0)
    def copy() = new OneFileLRSImpl(Sets[Node].newQSet(registers), Sets[Node].newQSet(storage), spillPressure)

    def moveToRegisters(node: Node): Unit = { updateSpillPressure(node, onAdd = false); registers += node; storage -= node }
    def moveToStorage(node: Node): Unit   = { updateSpillPressure(node, onAdd = true);  registers -= node; storage += node }
    def remove(node: Node): Unit          = { updateSpillPressure(node, onAdd = false); registers -= node; storage -= node }

    def live(node: Node): Boolean       = inRegister(node) || inStorage(node)
    def inRegister(node: Node): Boolean = registers(node)
    def inStorage(node: Node): Boolean  = storage(node)

    def allNodes: Iterator[Node] = registers.iterator ++ storage.iterator
    def allNodesSet: collection.Set[Node] = registers ++ storage

    def registersPressure: Int = registers.size

    private def updateSpillPressure(node: Node, onAdd: Boolean): Unit = {
      if (onAdd) {
        if (!storage(node) && !node.isInstanceOf[Constant]) spillPressure += 1
      } else {
        if (storage(node) && !node.isInstanceOf[Constant]) spillPressure -= 1
      }
    }
  }


  /////////////////////////////////////////////////////////////////////////////

  /** Implementation of LRS bits for all existing register files. */
  class LRSImpl private[state] (private val iRegLRS: OneFileLRSImpl,
                                private val fRegLRS: OneFileLRSImpl) {

    def this() = this(new OneFileLRSImpl(), new OneFileLRSImpl())
    def copy() = new LRSImpl(iRegLRS.copy(), fRegLRS.copy())

    private def byFile(file: RegFile): OneFileLRSImpl = file match {
      case IREG => iRegLRS
      case FREG => fRegLRS
    }

    private def hasFile(node: Node): Boolean = node.producesValue && !inSpecialFile(node)

    private def fileByNode(node: Node): OneFileLRSImpl = { assert(hasFile(node)); byFile(regFileOf(node)) }

    def moveToRegisters(node: Node): Unit = if (hasFile(node)) fileByNode(node).moveToRegisters(node)
    def moveToStorage(node: Node): Unit   = if (hasFile(node)) fileByNode(node).moveToStorage(node)
    def remove(node: Node): Unit          = if (hasFile(node)) fileByNode(node).remove(node)

    def live(node: Node): Boolean       = hasFile(node) && fileByNode(node).live(node)
    def inRegister(node: Node): Boolean = hasFile(node) && fileByNode(node).inRegister(node)
    def inStorage(node: Node): Boolean  = hasFile(node) && fileByNode(node).inStorage(node)

    def allNodes: Iterator[Node] = iRegLRS.allNodes ++ fRegLRS.allNodes
    def allNodes(file: RegFile): Iterator[Node] = byFile(file).allNodes
    def allNodesSet(file: RegFile): collection.Set[Node] = byFile(file).allNodesSet

    def registersPressure(file: RegFile): Int = byFile(file).registersPressure

    def registerNodesSet(file: RegFile): collection.Set[Node] = byFile(file).registers
    def storageNodesSet(file: RegFile): collection.Set[Node] = byFile(file).storage

    def spillPressure = iRegLRS.spillPressure + fRegLRS.spillPressure
  }


  /////////////////////////////////////////////////////////////////////////////

  /** LRS bits for local state of bulldozer interpreter.
    *
    * L bit means that node is alive at current moment. It includes local nodes and
    *   context nodes (arguments & outsiders) for all nested loops
    * R bit means that node occupies register at current moment.
    * S bit means that node occupies storage slot at current moment.
    */
  type LocalLRS = LRSImpl


  /////////////////////////////////////////////////////////////////////////////

  /** LRS bits for loop state of bulldozer interpreter.
    *
    * L bit means that node is context node of corresponding loop.
    * R bit means that node was never spilled in it's loop including inner ones
    * S bit means that node was spilled in it's loop or in inner one.
    *
    * Outsiders of one loop does not included in outsiders for it's inner loops.
    * Arguments of one loop may be included in outsiders/arguments for it's inner loops.
    */
  class LoopLRS private (val loop: Loop[Block],
                         val argumentsList: ListBuffer[LRSImpl],
                         val outsidersList: ListBuffer[LRSImpl],
                         private val outer: LoopLRS) {

    private def _arguments = argumentsList.last
    private def _outsiders = outsidersList.last

    def makeForInner(inner: Loop[Block], args: IterableOnce[Node], outsiders: IterableOnce[Node]): LoopLRS = {
      val argumentsLRS = new LRSImpl()
      args.iterator foreach argumentsLRS.moveToRegisters

      val outsidersLRS = new LRSImpl()
      outsiders.iterator foreach { x => if (!isOutsider(x)) outsidersLRS.moveToRegisters(x) }

      new LoopLRS(inner, argumentsList :+ argumentsLRS, outsidersList :+ outsidersLRS, this)
    }

    /** Returns true, iff `node` is context node (argument or outsider) for this loop. */
    def fromContext(node: Node): Boolean = _arguments.live(node) || _outsiders.live(node) || outer.fromContext(node)

    /** Returns true, iff `node` was spilled somewhere in this loop (including inner loops parts). */
    def wasSpilled(node: Node): Boolean = _arguments.inStorage(node) || _outsiders.inStorage(node)

    /** Returns true, iff `node` was spilled somewhere in this loop (including inner loops parts) or any of it's outer loops. */
    def wasSpilledInThisLoopOrOuter(node: Node): Boolean = wasSpilled(node) || outer.wasSpilledInThisLoopOrOuter(node)

    /** Returns true, iff `node` is outsider of this loop. */
    def isOutsider(node: Node): Boolean = _outsiders.live(node) || outer.isOutsider(node)

    /** Returns true, iff `node` is argument of this loop. */
    def isArgument(node: Node): Boolean = _arguments.live(node)

    /** Registers `node` spill in this loop and all outer loops. */
    def registerAsSpilled(node: Node): Unit = {
      if (_arguments.live(node)) _arguments.moveToStorage(node)
      if (_outsiders.live(node)) _outsiders.moveToStorage(node)
      outer.registerAsSpilled(node)
    }

    /** Returns pair (`outer`, `dropped`), where `outer` is LoopLRS of loop `amount` steps out from current loop and
      * `dropped` is a sequence of LoopLRS for loops from current loop to last inner of `outer`.
      *
      * E.g. if there are three loops A(B(C)), C.dropInnerLoops(2) will return pair (A, Seq(C, B)).
      * */
    def dropInnerLoops(amount: Int): (LoopLRS, Seq[LoopLRS]) = {
      assert(amount >= 0)
      if (amount == 0) (this, Nil) else {
        val (outerLRS, dropped) = outer.dropInnerLoops(amount - 1)
        (outerLRS, this +: dropped)
      }
    }

    /** Returns iterator over this loop arguments. */
    def arguments: Iterator[Node] = _arguments.allNodes

    /** Returns iterator over immediately this loop outsiders (not including outsiders of outer loop). */
    def immediateOutsiders: Iterator[Node] = _outsiders.allNodes

    /** Returns iterator over this loop arguments of `file`. */
    def arguments(file: RegFile): Iterator[Node] = _arguments.allNodes(file)

    /** Returns iterator over immediately this loop outsiders (not including outsiders of outer loop) of `file`. */
    def immediateOutsiders(file: RegFile): Iterator[Node] = _outsiders.allNodes(file)

    /** Returns set of immediately this loop outsiders (not including outsiders of outer loop) of `file`. */
    def immediateOutsidersSet(file: RegFile): collection.Set[Node] = _outsiders.allNodesSet(file)

    /** Returns iterator over LoopLRS from outermost loop to current loop. */
    def loopsInOrderFromOutermost(): Iterator[LoopLRS] = outer.loopsInOrderFromOutermost() ++ Iterator.single(this)
  }

  object LoopLRS {
    /** Special object for representing no-loop (whole method body) in LoopLRS contexts. */
    val empty = new LoopLRS(null, ListBuffer.empty, ListBuffer.empty, null) {
      override def fromContext(node: Node): Boolean = false
      override def wasSpilledInThisLoopOrOuter(node: Node): Boolean = false
      override def isOutsider(node: Node): Boolean = false
      override def registerAsSpilled(node: Node): Unit = {}
      override def loopsInOrderFromOutermost(): Iterator[LoopLRS] = Iterator.empty
      override def isArgument(node: Node): Boolean = false
    }
  }
}
