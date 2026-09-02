/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Tag, Universe}
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.graph.ordering.TopSort
import com.huawei.excelsior.jet.util.{ScalaCollections, Worklist}

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer

/** DCE (Dead Code Eliminator) eliminates dead code. Dead code is the code,
  * the execution of which does not lead to any effect.
  * <br/>
  * To determine the dead code, we define the root set of operations that
  * we can not prove that they are dead. There are the following operations:
  * <ol>
  * <li>return statements of method</li>
  * <li>operations, which form the skeleton of control flow</li>
  * <li>backward branches of cycles</li>
  * <li>memory operations</li>
  * </ol>
  * After that we analyse data flow upward and closing this set of operations by rules:
  * <ol>
  * <li>if operation is live, its data-flow arguments are live</li>
  * <li>if operation is live, operations, from which this operation is control dependent, are live</li>
  * </ol>
  * To determine control dependency we analyse control flow at the same time with data flow.
  * For each operation in set {BlockEnd, ControlNode, Block, projection of Branches} we define
  * the last control operation, that is dependent from it, by this rules:
  * <ol>
  * <li>for return and control nodes it is this operation</li>
  * <li>
  *   for block:
  *   <ol>
  *   <li>if it has controlled node, then it is this block</li>
  *   <li>otherwise, it is last control node after block (CFG element of control operation of block end)</li>
  *   </ol>
  * </li>
  * <li>
  *   for goto:
  *   <ol>
  *   <li>if it's successor has live phi function, then is it this block end</li>
  *   <li>otherwise, it is control operation of block end successor</li>
  *   </ol>
  * </li>
  * <li>
  *   for branch:
  *   <ol>
  *   <li>if it's successors have the same control operation, then it is their control operation</li>
  *   <li>otherwise, it is this branch.</li>
  *   </ol>
  * </li>
  * </ol>
  * In the last case (when branch has successors with different control operations), branch is marked as live.
  * This analysis is based on fact, that only control operations are control dependent is out IR.
  * <br/>
  * After closing the set, all other operations are marked as dead and we removed them from IR.
  *
  * Note: requires IR without unreachable code!
  *
  * TODO: more details
  *
  * @author paul
  * @author conwor
  */

// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait DCEComponent extends UCEComponent { self: Universe =>

  private enum Status {
    case NOT_CHANGED, WAS_DEAD, WAS_UNREACHABLE
  }
  import Status._

  private def eliminateDeadCodeOnce(): Status = stage(Stage.DCE) {

    // DCE requires IR without unreachable code!
    checkConsistency(CheckLevels.Optional) { noUnreachableCode }

    val topSort = cfg.topSort
    val dead = Sets[Node].newQSet(allNodes)
    val weak = Sets[SpinalNode].newQSet
    val worklist = Worklist.empty[Node]
    val controlToken = Maps[Node].newQMap[Node]
    val processedBlocks = Sets[Block].newQSet

    /** Set given `node` as live and add it in worklist, if it was not already set as live.
      * @param node alive node
      * @return `node`
      */
    def makeLive(node: Node): Node = {
      if (dead(node)) {
        dead -= node
        worklist += node
      }
      node
    }

    /** @return whether edge from `from` to `to` is back-chain */
    def isBackChain(from: Block, to: Block) = topSort.number(to) <= topSort.number(from)

    /** @return whether given block `b` has outgoing back-chain */
    def isBackChainStart(b: Block) = b.succBlocks.exists { succ => isBackChain(b, succ) }

    /** Distinguishes spinal nodes that are "weak" with respect to DCE liveness analysis.
      * Weak argument of such nodes (which is returned) is not considered live unless
      * there are other non-weak uses of them.
      * The idea is similar to [[java.lang.ref.WeakReference]] concept used in GC.
      * 
      * In other words these nodes to be removed unless there is someone alive using them.
      */
    object WeakNode {
      def unapply(n: SpinalNode): Option[Node] = condOpt(n) {
        case x: RawValueRangeFilter => x.filteredValue
        case check: PureCheck if check.trusted => check match {
          case x: TypeFilterNode => x.filteredArg
          case x: DivisorCheck => x.divisor
          case _ => shouldNotReachHere()
        }
      }
    }

    /** Call nodes that are known to have no side effects.
      * Such nodes can be safely removed if their value result is dead.
      */
    object SideEffectFree {
      def unapply(n: SpinalNode) = cond(n) {
        case _: GetClass | _: GradientVersioningPoint => true
      }
    }

    /** First path of DCE. Calculates control nodes influence to branches and collects
      * initial worklist for DCE.
      */
    def calculateControl(): Unit = {
      for (block <- topSort.order.reverseIterator) {
        var control = block.blockEnd match {
          case _: Return | _: Halt => makeLive(block.blockEnd)
          case x if isBackChainStart(block) => makeLive(x)
          case x => ScalaCollections.uniqueValue(x.exits map controlToken) getOrElse makeLive(x)
        }
        controlToken(block.blockEnd) = control
        block.spineBackward foreach {
          case _: Marker =>
          case x @ WeakNode(_) =>
            weak += x
            control = x
          case SideEffectFree() => // useful nodes will be marked as live by their value users
          case x => control = makeLive(x)
        }
        controlToken(block) = control
        block.args foreach { controlToken(_) = control }
      }
    }

    /** Closes worklist with data-flow edges.
      * @return set of blocks, where phi-functions are alive
      */
    def processWorklist(): Sets[Block]#QSet = {
      val result = Sets[Block].newQSet
      for (node <- worklist.drain) {
        for (e <- node.inEdges if !e.isControl) {
          makeLive(e.source)
        }
        if (node.isInstanceOf[Phi] && !processedBlocks(node.block)) result += node.block
      }
      result
    }

    /** Makes weak nodes, whose arguments did not die during worklist processing, live.
      */
    def processWeak(): Unit = {
      for (node @ WeakNode(arg) <- weak if !dead(arg)) {
        makeLive(node)
      }
    }

    /** Calculates influence from alive phi-functions in given `blocks` to IR branches
      * and adds new branches into worklist.
      * 
      * Consider following example:
      * {{{val z = if (x <= y) x else y}}}
      * 
      * This produces IR similar to:
      * {{{
      * 1: Cmp(x, y)
      * 2: If
      * 3: If (true) -> 5 // Here both If exits leads to same block
      * 4: If (false) -> 5
      * 
      * 5: BBlock
      * 6: Phi(3, 4)
      * 7: Return
      * }}}
      *
      * It may look like If-true and If-false nodes are useless, because they lead to same BBlock, but since
      * Phi function receives different values depending on by which path Block 6 was reached, we must preserve this
      * two projections, this way phi influences nodes.
      */
    def calculatePhiInfluence(blocks: Sets[Block]#QSet): Unit = {
      val maxBlockNum = blocks.map(topSort.number(_)).max
      val influence = Maps[Node].newQMap[Node]
      for (block <- topSort.order.take(maxBlockNum+1).reverseIterator) {
        val inf = block.blockEnd match {
          case x if (!dead(x)) || !(x.exits exists influence.contains) => null
          case x => ScalaCollections.uniqueValue(x.exits map influence) getOrElse makeLive(x)
        }
        if (blocks.contains(block)) {
          block.args foreach { arg => influence(arg) = arg }
        } else {
          if ((inf != null) && (controlToken(block) == controlToken(block.blockEnd)) && !processedBlocks(block)) {
            block.args foreach { arg => influence(arg) = inf }
          }
        }
      }
      processedBlocks ++= blocks
    }

    /** Eliminates calculated dead code. */
    def eliminateDead(): Status = {
      var status = NOT_CHANGED
      val deadBranches = new ArrayBuffer[Branch]

      var wasXHandler = false
      
      // Eliminate all dead code except branches. We postpone branches because they will be replaced by Goto
      // which may lead to new nodes commit and violate DCE process. For more details look at JET-12745.
      withoutNewNodes {
        for (node <- dead if node.isCommitted) node match {
          case _: NoValue =>
          // Currently NoValue is singleton, keeping this in mind let's assume that at the beginning of DCE
          // NoValue node was present, and also some Call `x` with XPoint, and let's say call will be deleted along
          // with XPoint, since it was marked as dead.
          // If order of `dead` nodes is something like that: Seq(x, NoValue), then first `x` will be deleted and
          // NoValue node will gain new uses in unreachable code, and after that we try to delete NoValue node
          // which has uses, which is the problem.
          // See JET-14444
            
          case branch: Branch =>
            deadBranches += branch

          case x @ WeakNode(_) =>
            wasXHandler |= x.hasXHandler
            strikeOut(x)
            status = WAS_DEAD

          case x @ SideEffectFree() =>
            wasXHandler |= x.hasXHandler
            replaceValueUsesByNoValueAndStrikeOut(x)
            status = WAS_DEAD

          case _: ControlNode =>

          case _: Catch => // Catch has side-effects and must be preserved

          case StackAlloc.DebugVar(_, _) => // Debug nodes should always survive

          case _ =>
            decommit(node)
            status = WAS_DEAD
        }
      }

      for (branch <- deadBranches) {
        val anyOut = branch.exits.head
        anyOut.replaceUsesBy(Goto(branch.inCtrl, branch.inMemory)) //TODO: move into class Branch.Exit
        branch.makeUsesUnreachable()
        decommit(branch)
        status = WAS_DEAD
      }

      assert(all[Proxy].isEmpty, all[Proxy].toSeq)

      // Branch elimination produces unreachable code, so it is a good idea to cleanup after ourselves,
      // especially considering that before DCE there was no unreachable code.
      if (deadBranches.nonEmpty || wasXHandler) {
        // Note: unreachable code produced by branch elimination will not have data flow uses in former branch merge point,
        //       which guarantees that UCE will not produce any dead code.
        checkConsistency(CheckLevels.Optional) {
          all[Block] forall { b =>
            b.reachable || b.succBlocks.forall { s => s.unreachable || s.phies.isEmpty }
          }
        }

        if (eliminateUnreachableCode()) {
          status = WAS_UNREACHABLE
        }
      }

      // Dead NoValue nodes may be created during DCE, if it eliminates edges in some unreachable code
      // (see Block.removeEdges, JET-10275 and JET-14444).
      for (n <- NoValue.inCurrentScope) {
        // TODO: uncomment when JET-15044 is fixed
        //assert(n.uses.isEmpty)
        if (n.uses.isEmpty) {
          decommit(n)
        }
      }

      status
    }

    def withoutNewNodes(action: => Unit): Unit = {
      onCommit.withCallback {
        // Unreachable nodes may be created during DCE and will be cleaned in eliminateDead
        case _: NoValue | _: UnreachableBlockEnd.Exit | _: BBlock | _: UnreachableThrowing | _: XPoint | _: Halt =>
        case n => shouldNotReachHere(s"unexpected node $n commit")
      } {
        action
      }
    }

    def withoutNewNodesAndArgChanges(action: => Unit): Unit = {
      withoutNewNodes {
        afterStructuralChange.withCallback(e => shouldNotReachHere(s"unexpected edge $e change")) {
          action
        }
      }
    }

    //////////////////////////////////////////////////////////////////////////////

    withoutNewNodesAndArgChanges {
      calculateControl()
      while (worklist.nonEmpty) {
        val blocks = processWorklist()
        if (blocks.nonEmpty) calculatePhiInfluence(blocks)
        processWeak()
      }
    }
    eliminateDead()
  }

  def eliminateDeadCode(): Boolean = withDeferredOnCommitOptimizations { // TODO: deferred optimizations is workaround for issue mentioned in JET-15217
    var status = eliminateDeadCodeOnce()
    val result = status != NOT_CHANGED
    while (status == WAS_UNREACHABLE) {
      status = eliminateDeadCodeOnce()
    }

    //assert(NoValue.inCurrentScope.isEmpty)
    result
  }

}
