/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.{AnyReg, FReg, IReg, Mem}
import com.huawei.excelsior.jet.codeemitter.CodeEmitter
import com.huawei.excelsior.jet.compiler.Env.tailRegister
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame}

import scala.collection.mutable

/** Manager for nodes' location. */
final class Nodes(val locations: Locations, val emit: CodeEmitter, val frame: Frame[? <: IReg, ? <: FReg, ? <: ABI[?, ?]]) {

  private val nodeToLocation = mutable.LinkedHashMap.empty[Node, Location]
  private val savedStates = mutable.Stack.empty[mutable.LinkedHashMap[Node, Location]]
  private var sharedStateDepth = 0

  private var curBC = -1

  def setInstructionBC(newBC: Int): Unit = curBC = newBC

  def sizeOf(n: Node) = locations.sizeOf(n.`type`)

  private def fitsToLoc(node: Node, loc: Location) = locations.fitsToLoc(node.`type`, loc)

  /** Returns non-null location of node `n`. */
  def getLoc(n: Node) = nodeToLocation(n) ensuring (_ != null)

  /** Returns true iff node `n` is bound to some location. */
  def hasLoc(n: Node) = nodeToLocation.contains(n ensuring (_ != null))

  /** Ensures that node `n` is in memory and returns it. */
  def getMemLoc(node: Node) = getLoc(node).asMem

  private def loadToRegImpl[LR <: AnyReg](n: Node): LR = {
    val loc = getLoc(n)
    if (loc.isMem) {
      val newLoc = locations.getAnyFreeRegUnsafe[LR](n.`type`)
      transfer(n, loc, newLoc)
      newLoc
      
    } else {
      assert(loc.isReg)
      assert(locations.isBusy(loc))
      val regLoc = loc.asInstanceOf[LR]
      locations.delaySpill(regLoc)
      regLoc
    }
  }

  /** Ensures that node `n` is on IReg and returns it. May spill some other node. */
  def loadToIReg(n: Node): IReg = loadToRegImpl[IReg](n)

  /** Ensures that node `n` is on FReg and returns it. May spill some other node. */
  def loadToFReg(n: Node): FReg = loadToRegImpl[FReg](n)

  private def loadToRegAndReleaseIfNotUsedLaterImpl[LR <: AnyReg](n: Node): LR = {
    val result = loadToRegImpl[LR](n)
    releaseLocIfNotUsedLater(n)
    result
  }

  /** Ensures that node `n` is on IReg and returns it. May spill some other node. @see [[isNotUsedLater]]. */
  def loadToIRegAndReleaseIfNotUsedLater(n: Node): IReg = loadToRegAndReleaseIfNotUsedLaterImpl[IReg](n)

  /** Ensures that node `n` is on FReg and returns it. May spill some other node. @see [[isNotUsedLater]]. */
  def loadToFRegAndReleaseIfNotUsedLater(n: Node): FReg = loadToRegAndReleaseIfNotUsedLaterImpl[FReg](n)

  /** Returns `loc`. */
  def bind[L <: Location](n: Node, loc: L): L = { bind(n, loc, overwrite = false); loc }

  private def bindToAnyFreeRegImpl[LR <: AnyReg](n: Node): LR =
    bind[LR](n, locations.getAnyFreeRegUnsafe[LR](n.`type`))

  /** Binds node `n` to any IReg and returns it. May spill some other node. */
  def bindToAnyFreeIReg(n: Node): IReg = bindToAnyFreeRegImpl[IReg](n)

  /** Binds node `n` to any FReg and returns it. May spill some other node. */
  def bindToAnyFreeFReg(n: Node): FReg = bindToAnyFreeRegImpl[FReg](n)

  /** Binds node `n` to any IReg and returns it. Tries to use one of the `preferred` registers. May spill some other node. */
  def bindToAnyFreeIRegWithPreferred(n: Node, preferred: IReg*): IReg =
    bind[IReg](n, locations.getAnyFreeIRegUnsafeWithPreferred(n.`type`, preferred*))

  /** Binds node `n` to any location and returns it. Do not spill any nodes. */
  def bindToAnyFreeLoc(n: Node): Location = bind[Location](n, locations.getAnyFreeLocUnsafe(n.`type`))

  /** Binds node `n` to any long memory location and returns it. Do not spill any nodes. */
  def bindToAnyFreeMem(n: Node): Mem = bind[Mem](n, locations.getAnyFreeMemUnsafe(n.asmType))

  /** Binds node `n` to special invalid location and returns it. This location cannot hold any value. Do not spill any nodes. */
  def bindToInvalidLoc(n: Node): Location.Other = bind[Location.Other](n, Locations.getInvalid)

  private def bind(n: Node, newLoc: Location, overwrite: Boolean): Unit = {
    assert(fitsToLoc(n, newLoc))

    if (n.`type`.isFP) {
      assert(!newLoc.isIReg, "floating-point value cannot be bound to non-floating-point register")
    } else {
      assert(!newLoc.isFReg, "non-floating-point value cannot be bound to floating-point register")
    }

    saveStateOnWrite()
    nodeToLocation.put(n, newLoc) match {
      case Some(oldLoc) => assert(overwrite && oldLoc != newLoc)
      case _ =>
    }
    locations.acquire(newLoc)
  }

  private[engine] def getNodeAt(loc: Location): Node =
    nodeToLocation.find(_._2 == loc).get._1

  private def transfer(n: Node, oldLoc: Location, newLoc: Location): Unit = {
    assert(newLoc != oldLoc)
    assert(fitsToLoc(n, newLoc))
    emit.copyAny(newLoc, oldLoc, n.asmType)
    locations.release(oldLoc)
    bind(n, newLoc, overwrite = true)
  }

  /** Moves value and rebinds node `n` to free location `newLoc`. Do not spill any nodes. Returns old location of node `n`. */
  def transfer(n: Node, newLoc: Location): Location = {
    val oldLoc = getLoc(n)
    transfer(n, oldLoc, newLoc)
    oldLoc
  }

  /** Unconditionally release given node. Caller should be sure that location of this node is not used later.
    *
    * @see [[isNotUsedLater]].
    */
  def releaseLoc(n: Node): Unit = {
    saveStateOnWrite()
    val loc = nodeToLocation.remove(n).get
    locations.release(loc)
  }

  def releaseLoc(nodes: mutable.LinkedHashSet[Node]): Unit = nodes foreach releaseLoc

  /** Returns true iff this node is not used in later bytecode instructions. Note that it still can be used in
    * current bytecode instruction if the result is true.
    *
    * Note that the result is always true for temporary nodes (they should not live longer than one bytecode instruction).
    */
  def isNotUsedLater(n: Node): Boolean = {
    if (n.isTemporary || n.isDead) {
      true
    } else if (n.isUsedAtTheEnd) {
      false
    } else {
      val lastUse = n.lastUse.asInstanceOf[Node.BCPosition]
      lastUse.bcOffset <= curBC
    }
  }

  /** @see [[isNotUsedLater]] */
  def releaseLocIfNotUsedLater(n: Node): Boolean = {
    if (isNotUsedLater(n)) {
      releaseLoc(n)
      true
    } else {
      false
    }
  }

  def releaseLocIfNotUsedLater(n1: Node, n2: Node): Unit = {
    releaseLocIfNotUsedLater(n1)
    if (n1 != n2) releaseLocIfNotUsedLater(n2)
  }

  def releaseLocIfNotUsedLater(n1: Node, n2: Node, n3: Node): Unit = {
    releaseLocIfNotUsedLater(n1, n2)
    if ((n3 != n1) && (n3 != n2)) releaseLocIfNotUsedLater(n3)
  }

  def releaseLocIfNotUsedLater(n1: Node, n2: Node, n3: Node, n4: Node): Unit = {
    releaseLocIfNotUsedLater(n1, n2, n3)
    if ((n4 != n1) && (n4 != n2) && (n4 != n3)) releaseLocIfNotUsedLater(n4)
  }

  def releaseLocIfNotUsedLater(nodes: collection.Set[Node]): Unit =
    nodes.iterator foreach releaseLocIfNotUsedLater

  def releaseLocIfNotUsedLater(nodes: collection.Seq[Node]): Unit = {
    val it = nodes.iterator
    if (!it.hasNext) return

    val n1 = it.next()
    releaseLocIfNotUsedLater(n1)
    if (!it.hasNext) return

    val n2 = it.next()
    if (n1 != n2) releaseLocIfNotUsedLater(n2)
    if (!it.hasNext) return

    val set = mutable.LinkedHashSet.empty[Node] ++= it -= n1 -= n2
    releaseLocIfNotUsedLater(set)
  }

  def rescueAndAcquireIRegs(regs: IReg*): Unit = {
    rescueAndSpoilIRegs(regs*)
    regs foreach locations.acquire
  }

  /** Saves `regs` from spoiling. Guarantees that all non-free locations and corresponding nodes will not move. */
  def rescueAndSpoilIRegs(regs: IReg*): Unit = {
    for (r <- regs) {
      // we iterate over array here as its length is small in practice (1 to 4 elements)
      rescueAndSpoilLocImpl(r, exclude = regs.contains)
    }
  }

  /** Saves registers which satisfy `predicate` from spoiling.
    * Guarantees that all non-free locations and corresponding nodes will not move.
    */
  def rescueAndSpoilRegs(predicate: AnyReg => Boolean): Unit = {
    for (r <- (frame.availableIRegs ++ frame.availableFRegs) filter predicate) {
      rescueAndSpoilLocImpl(r, predicate)
    }
  }

  def ensureNoAliveRefsOnRegs(isTraced: Node => Boolean): Unit = {
    for (case (node, r: IReg) <- nodeToLocation if isTraced(node)) {
      transfer(node, r, locations.getAnyFreeMemUnsafe(node.`type`))
    }
    // Spoil all unused registers as they may hold an alive reference inherited from a caller method.
    frame.availableIRegs ensuring (_ contains tailRegister) foreach frame.registerUsedReg
    // Note that Tail register must also be spoiled as it is traced by GC in a special way.
  }

  def checkNoAliveRefsOnRegs(isTraced: Node => Boolean): Unit = {
    for ((node, loc) <- nodeToLocation) {
      assert(!loc.isInstanceOf[IReg] || !isTraced(node))
    }
  }

  /** Saves `loc` from spoiling. Guarantees that all non-free locations and corresponding nodes will not move. */
  def rescueAndSpoilLoc(loc: Location) = rescueAndSpoilLocImpl(loc, _ => false) // TODO-DECAF (use default parameter)

  private def rescueAndSpoilLocImpl(loc: Location, exclude: AnyReg => Boolean) = {
    // 1. Rescue (only for busy locations)
    if (locations.isAllocatable(loc) && locations.isBusy(loc)) {
      val node = getNodeAt(loc)
      transfer(node, loc, locations.getAnyFreeLocUnsafe(node.`type`, exclude))
    }
    // 2. Spoil (only for registers)
    if (loc.isReg) {
      frame.registerUsedReg(loc.asReg)
    }
  }

  private def saveStateOnWrite(): Unit = {
    while (sharedStateDepth > 0) {
      savedStates.push(nodeToLocation.clone())
      sharedStateDepth -= 1
    }
  }

  /** Save nodes-to-locations mapping before conditional operations. To restore mapping after them use [[popState]]. */
  private def pushState(): Unit = sharedStateDepth += 1

  /** Restore nodes-to-locations mapping from states stack. Returns whether any transfers were generated.
    *
    * @see [[pushState]]
    */
  private def popState(): Boolean = {
    if (sharedStateDepth > 0) {
      sharedStateDepth -= 1
      return false
    }

    val state = savedStates.pop()
    assert(nodeToLocation.keys forall state.contains,
      "there should be no alive bindings created between pushState/popState")

    var counter = 0
    val counterLimit = state.size * 2

    var anyTransfers = false

    // TODO-DECAF: improve and use WhileChanged
    var hasConflict = true
    while (hasConflict) {
      hasConflict = false
      for ((node, oldLoc) <- state) {
        if (hasLoc(node)) {
          val newLoc = getLoc(node)
          if (newLoc != oldLoc) {
            hasConflict |= locations.isBusy(oldLoc)
            rescueAndSpoilLoc(oldLoc)
            transfer(node, oldLoc)
            anyTransfers = true
          }
        } else {
          // Some node may be bind before pushState, released after pushState but before popState.
          // It should remain released.
        }
      }

      assert(counter <= counterLimit)
      counter += 1;
    }

    anyTransfers
  }

  def withSavedState(body: => Unit): Boolean = {
    pushState()
    body
    popState()
  }

  /** Read only view of locations mapping. */
  def locationsMapping: collection.Map[Node, Location] = nodeToLocation
}
