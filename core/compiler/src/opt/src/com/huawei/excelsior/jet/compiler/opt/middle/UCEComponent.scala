/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Universe}
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.{Closure, Worklist}

import scala.collection.mutable.ArrayBuffer

/** We call block reachable if it is reachable in CFG from entry block. Otherwise we call it unreachable. Among
  * unreachable blocks there is special block ''unreachable bar'' (UB). All unreachable blocks separated into two
  * groups: ones that are reachable in CFG from UB (''bound'' unreachable blocks) and ones that are not (''free''
  * unreachable blocks).
  *
  * Edges which source is not point in reachable block are called unreachable. Unreachable code is a set of unreachable
  * blocks and edges. UCE eliminates unreachable code except UB.
  *
  * When we make some edge unreachable, we replace it's source by point in code reachable from UB. Thus in most cases
  * there are no free unreachable blocks in IR which allows fast check of unreachable code existence and simplifies its
  * elimination. Now the only moments when free unreachable blocks could exist are right after CFG parsing (because
  * bytecode could have unreachable regions) or right after CFG building in unit-tests.
  *
  * @author conwor
  * @author cypok
  * @author paul
  */
trait UCEComponent { self: Universe =>

  private var allowFreeBlocks: Boolean = false

  /** Executes `action` allowing to have free unreachable blocks during it and then eliminates unreachable code. */
  def withFreeUnreachableBlocks[T](action: => T): T = {
    assert(!allowFreeBlocks)
    allowFreeBlocks = true
    val result = action
    if (eliminateUnreachableCode()) {
      dbgPrinter.debugNodes("All graph after UCE")
    }
    allowFreeBlocks = false
    result
  }

  /** Returns iterator over free unreachable blocks. */
  private def computeFreeBlocks: Iterator[Block] = {
    def reachableFromEntry(b: Block) = b.reachable // equivalent of containing in cfg.collectReachableFrom(entryBlock)
    val reachableFromUB = if (hasUnreachableBar) cfg.collectReachableFrom(unreachableBar) else Sets[Block].newQSet
    all[Block] filterNot { b => reachableFromEntry(b) || reachableFromUB(b) }
  }

  private var ensuringNoFreeUnreachableBlocksEnabled = true

  /** Checks that free unreachable blocks are not allowed and really not exist in IR. */
  def ensureNoFreeUnreachableBlocks(): Unit = if (ensuringNoFreeUnreachableBlocksEnabled) {
    if (allowFreeBlocks) {
      shouldNotReachHere("free unreachable blocks allowed")
    }
    checkConsistency(CheckLevels.Optional) {
      val freeBlocks = computeFreeBlocks
      if (freeBlocks.nonEmpty) {
        shouldNotReachHere("free unreachable blocks:\n" + (freeBlocks mkString "\n"))
      }
    }
  }

  /** Checks that there are no free unreachable blocks in IR if they are not allowed to be. */
  def checkUnreachableConsistency(): Unit = if (!allowFreeBlocks) ensureNoFreeUnreachableBlocks()

  /** Returns true iff unreachable bar exists in IR. */
  def hasUnreachableBar: Boolean = currentScope.hasUnreachableBar

  /** Returns unreachable bar or creates it if it did not exist yet. */
  def unreachableBar: BBlock = currentScope.unreachableBar

  /** Returns true iff `b` is unreachable bar. */
  def isUnreachableBar(b: Node) = hasUnreachableBar && (b eq unreachableBar)

  /** Returns true iff there is no unreachable code in IR. */
  def noUnreachableCode =
    (!hasUnreachableBar || unreachableBar.xSuccBlocks.isEmpty) && (!allowFreeBlocks || computeFreeBlocks.isEmpty)

  /** Creates new exit from unreachable bar used to make unreachable edge incoming in `b`. */
  private def newExitFor(b: Block): Node = {
    val exit = unreachableBar.blockEnd.asInstanceOf[UnreachableBlockEnd].newExit()

    b match {
      case _: BBlock => exit

      case _: XBlock =>
        val newBlock = BBlock(exit)
        val throwing = UnreachableThrowing(newBlock, newBlock)
        Halt.afterThrow("unreachable throwing")(throwing, newBlock)
        throwing.xpoint
    }
  }

  /** Make `edge` unreachable by changing its source to unreachable bar or some other unreachable code. */
  def makeUnreachable(edge: Edge): Unit = {
    val Edge(_, target: Block) = edge
    for (phi <- target.phies) phi.phiInput(edge).source = NoValue()
    edge.source = newExitFor(target)
  }

  /** Make `edges` unreachable by changing their sources to unreachable bar or some other unreachable code. */
  def makeUnreachable(edges: IterableOnce[Edge]): Unit = ArrayBuffer.from(edges) foreach makeUnreachable

  /** Returns `true` if any unreachable code was removed. */
  def eliminateUnreachableCode(): Boolean = {
    checkUnreachableConsistency()
    if (noUnreachableCode) return false

    // 1. Collect unreachable blocks and exits from unreachable to reachable.
    val blocks = Worklist[Block](unreachableBar)
    if (allowFreeBlocks) blocks ++= computeFreeBlocks
    val exits = new ArrayBuffer[Edge]
    for (source <- blocks.accumulate; e @ Edge(_, target: Block) <- source.xSuccBlockEdges) {
      if (target.unreachable) blocks += target else exits += e
    }

    // 2. Remove exits and corresponding phi columns.
    if (allowFreeBlocks) ensuringNoFreeUnreachableBlocksEnabled = false // temporary disabled to avoid check in `removeEdges`
    Block.removeEdges(exits)
    ensuringNoFreeUnreachableBlocksEnabled = true

    // 3. Remove all nodes defined in unreachable blocks, as well as all their uses.
    val nodes = Closure[Node](Block.withParamNodes(blocks.iterator) filterNot isUnreachableBar)(_.uses)
    assert(nodes forall { n => n.block == null || blocks.contains(n.block) },
      "all nodes should be from unreachable block or be floating nodes")
    nodes foreach decommit

    // 4. Remove exits from unreachable bar and make sure there are no nodes in it.
    unreachableBar.points foreach {
      case _: Block =>

      case n: UnreachableBlockEnd =>
        n.exits foreach decommit

      case n: AssignVar =>
        // AssignVar nodes may occur in unreachable bar if VarProcessor applied on IR with unreachable code (JET-12897).
        strikeOut(n)

      case n =>
        shouldNotReachHere(s"unexpected node in unreachable bar: $n")
    }

    true
  }
}
