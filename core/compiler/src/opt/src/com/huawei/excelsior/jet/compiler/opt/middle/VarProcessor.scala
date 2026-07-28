/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.ir.{DebugPrinters, Tag, Universe}
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.{Numbering, ScalaCollections}

import scala.collection.mutable.ArrayBuffer

/** Non-SSA variables processor.
  * <p>
  * Provides utilities for handling variables in IR,
  * as well as means for conversion between SSA and Var representations.
  *
  * @author liontiger
  */
trait VarProcessor extends GlobalCodeMotion { self: Universe =>

  class Var private (val id: Int, val tpe: Type) {
    override def toString: String = s"Var$id($tpe)"
  }

  object Var {
    def apply(tpe: Type): Var = {
      val v = new Var(id, tpe)
      id += 1
      allVars += v
      v
    }
  }

  private var id = 0 // only for debug purposes

  private val allVars = Sets[Var].newQSet

  def areVarsPresent: Boolean = allVars.nonEmpty

  /** Transforms given `node` into a new Var,
    * which is read at given `point`
    * and assigned at points with values specified by `assignmentAction`.
    */
  def replaceByVarAt(node: Node, point: UpperPoint)(assignmentAction: ((UpperPoint, Node) => AssignVar) => Unit): ReadVar = {
    withNewVar(node.tpe) { (assignAt, readAt) =>
      // Read should be below assignment in case of the same point for read and assignment.
      // So insert read first.
      val read = readAt(point)

      assignmentAction(assignAt)

      // Implementation note: replaceBy should be done inside of withNewVar to invalidate inCtrl context-types caches.
      node replaceBy read

      read
    }
  }

  /** Transforms given `phi` into a new Var,
    * which is assigned to `phi.joinedArgs` at each respective arg's use point
    * and is read immediately at the beginning of `phi.block`.
    *
    * Note: requires IR without critical edges.
    *
    * @see [[splitCriticalEdges]]
    */
  def replacePhiByVar(phi: Phi): ReadVar = {
    replaceByVarAt(phi, phi.block) { assignAt =>
      for (e <- phi.inEdges) {
        assignAt(lowerVarInsertionPoint(e).inCtrl, e.source)
      }
    }
  }

  /** Transforms all value uses of given `n` into a new Var,
    * which is assigned to `n` after its upper point
    * and is read at each respective value use point.
    *
    * If `n` has no value uses, then the new Var will not be assigned to `n`.
    *
    * Note: requires IR without critical edges.
    *
    * @see [[splitCriticalEdges]]
    */
  def replaceAllValueUsesByVar(n: Node): Option[AssignVar] = {
    replaceValueUsesByVar(n)(_ => true)
  }

  /** Transforms value uses of given `n` that satisfy `f` into a new Var,
    * which is assigned to `n` after its upper point
    * and is read at each respective value use point.
    *
    * If no value uses of `n` satisfy `f`, then the new Var will not be assigned to `n`.
    *
    * Note: requires IR without critical edges.
    *
    * @see [[splitCriticalEdges]]
    */
  def replaceValueUsesByVar(n: Node)(f: Edge => Boolean): Option[AssignVar] = {
    // copy edges in advance as `withNewVar` installs hook which may modify def-use list of `n` during `e.source = ..`
    val edges = ArrayBuffer.from(n.valueOutEdges filter f)
    if (edges.isEmpty) None else {
      withNewVar(n.tpe) { (assignAt, readAt) =>
        // Note: reads must be inserted before assign,
        //       because otherwise uses can be pinned to assign
        //       and their reads will end up before assign (use-before-def situation).
        edges foreach { e =>
          assert(e.source == n)
          e.source = readAt(lowerVarInsertionPoint(e).inCtrl)
        }
        Some(assignAt(upperPoint(n), n))
      }
    }
  }

  /** Provides environment for safe and correct Var insertion into IR.
    *
    * Same as [[withNewVars]] but operates with a single Var of given `tpe`.
    */
  def withNewVar[T](tpe: Type)(action: ((UpperPoint, Node) => AssignVar, UpperPoint => ReadVar) => T): T =
    withNewVars { (newVar, assignAt, readAt) =>
      val singleVar = newVar(tpe)
      def assignSingleVar(point: UpperPoint, value: Node) = assignAt(point, singleVar, value)
      def readSingleVar(point: UpperPoint) = readAt(point, singleVar)
      action(assignSingleVar, readSingleVar)
    }

  /** Provides environment for safe and correct Var insertion into IR.
    *
    * Performs given `action` providing `newVar` functionality for introduction of a new Var with given `tpe`
    * and `assignAt` and `readAt` functionality for insertion of newly created Vars assignments and reads.
    *
    * Note: `action` is performed with incremental GCM
    *        and may invalidate cached info in IR if Var insertion will break its correctness.
    *
    * @see [[replacePhiByVar]]
    * @see [[replaceValueUsesByVar]]
    */
  def withNewVars[T](action: ((Type) => Var, (UpperPoint, Var, Node) => AssignVar, (UpperPoint, Var) => ReadVar) => T): T = {

    val reads = Maps[(ControlNode, Var)].newQMap[ReadVar]

    def readAt(point: UpperPoint, variable: Var) = {
      reads.getOrElseUpdate((point, variable), point match {
        case read @ ReadVar(`variable`) => read
        case _ => insertRead(variable, point)
      })
    }

    def assignAt(point: UpperPoint, variable: Var, value: Node) = {
      insertAssign(variable, point, value)
    }

    def newVar(tpe: Type) = Var(tpe)

    // Various analyses store cached info directly in IR (as edges).
    // This cached info may become inconsistent after arbitrary transformations following Var insertion.
    // To prevent inconsistency we must conservatively invalidate (or adjust) this info.
    def invalidateCachedInfo(e: Edge): Unit = e match {

      case ContextDependency(r: ReadVar, n) =>
        // Adjust inCtrl cached by Context types to a known valid one.
        ContextTypesMap.lowerControlDependencies(n, r)

      case Edge(n: CheckCast, _: AssignVar) =>
        // Invalidate uses in WeakCasts that have n as cached dominating check.
        n.unlinkDependentWeakCasts()

      case _ =>
    }
    withIncrementalGCM {
      afterStructuralChange.withCallback(invalidateCachedInfo) {
        action(newVar, assignAt, readAt)
      }
    }
  }

  /** Selects a valid point for ReadVar/AssignVar insertion immediately before it. */
  private def lowerVarInsertionPoint(e: Edge): LowerPoint = e.usePoint match {
    case x: XPoint => x.owner
    case x: LowerPoint => x
    case _: Branch.Exit => shouldNotReachHere("unexpected critical edge") // critical edges must be eliminated earlier
    case x: UnreachableBlockEnd.Exit => x.owner
    case x => shouldNotReachHere(s"unexpected usePoint $x")
  }

  private def insertAssign(variable: Var, inCtrl: UpperPoint, value: Node) = {
    insertCodeAfter(inCtrl) {
      AssignVar(variable)(value)
    }
  }

  private def insertRead(variable: Var, inCtrl: UpperPoint) = {
    insertCodeAfter(inCtrl) {
      ReadVar(variable)()
    }
  }

  /** Transforms all Vars into SSA-form.
    * After this transformation there are no Vars in IR.
    */
  def completeSSA(): Boolean = {
    if (allVars.isEmpty) {
      return false
    }

    val reads = all[ReadVar].toList
    val assigns = all[AssignVar].toList

    def cleanup(): Unit = {
      assigns foreach strikeOut
      reads foreach strikeOut
      allVars.clear()
    }

    if (reads.isEmpty) {
      cleanup()
      return true
    }

    // FIXME: remove this hack after JET-12409 is fixed properly
    reads foreach invalidateNodeType

    withIncrementalGCM {
      def convertProxy(tpe: Type, n: Node) = (n, n.tpe, tpe) match {
        case (_: Proxy, EopType.Plain, EopType.Eop(_)) => ReinterpretCast(n.tpe, tpe)(n)
        case _ => n
      }

      Node.withImplicitArgConversion(convertProxy) {
        new SSACompleter(Numbering(allVars)).iterate()
      }

      for (read <- reads if read.hasValueUses) {
        assert(read.block.unreachable)
        read.replaceValueUsesBy(NoValue())
      }
    }

    stats.count(StatsKind.Vars, "SSA completed")

    cleanup()

    true
  }

  /** Transforms most value uses in IR into Vars (used for UnstableSSA nightmare testing mode). */
  def destabilizeSSA(): Boolean = {
    var changed = false
    splitCriticalEdges()
    withIncrementalGCM {
      for (n <- allNodes.toList if n.producesValue && !n.block.isInstanceOf[XBlock]) {
        for (phi <- collect[Phi](n.usesByTag(Tag.VALUE)).toList if phi.isCommitted) {
          replacePhiByVar(phi)
        }

        replaceAllValueUsesByVar(n)
        changed = true
      }
    }
    if (changed) {
      stats.count(StatsKind.Vars, "SSA destabilized")
    }
    changed
  }

  /** SSACompleter builds SSA for given [[Var]]s (V1, V2, ..., VX).
    * Task of SSACompleter is to replace usages of ReadVar(Vi)
    * by a value of AssignVar(Vi, value) or inserted phi-function.
    *
    * SSACompleter could produce dead code.
    */
  private class SSACompleter(keys: Numbering[Var]) extends AbstractInterpreter {

    /** Interpreter state for SSA build interpreter is an array of values,
      * that are current values of corresponding variables.
      */
    class State(private var nodes: Array[NodeRef]) extends AbstractInterpreter.State {
      protected type This = State

      def this() = this(Array.fill(keys.order.size)(Invalid))

      def apply(key: Var): Node = nodes(keys.number(key)).deref

      def update(key: Var, value: Node): Unit = {
        assert(value.deref == value)
        if (apply(key) != value) {
          copyOnWrite()
          nodes(keys.number(key)) = value
        }
      }

      protected def forkImpl(): State = new State(nodes)

      def makeUnreachableCopy(): State = new State(Array.tabulate(keys.order.size) { i => NoValue() })

      protected def copyOnWriteImpl(): Unit = { nodes = nodes.clone() }

      def mergeFrom(block: Block, states: Seq[State], identity: Boolean)(mergeFunc: (Type, Seq[Node]) => Node): State = {
        assert(states forall (_.nodes.length == this.nodes.length))
        val head = states.head
        if (!identity || states.exists(_.nodes ne head.nodes)) {
          copyOnWrite()
          for (i <- nodes.indices) {
            val values = states map (_.nodes(i).deref)
            // TODO: remove copy-paste with `mergeValues` in VMStates.VMState.mergeFrom
            val mergedValue = ScalaCollections.uniqueValue(values) match {
              case None if values.map(_.tpe).reduce(_ | _) == ValueType => Invalid // Incompatible types.
              case Some(value) if identity => value
              case _ => mergeFunc(keys.order(i).tpe, values)
            }
            nodes(i) = mergedValue
          }
        }
        this
      }

      def foreachPair(that: State)(action: (Node, Node) => Unit): Unit = {
        assert(this.nodes.length == that.nodes.length)
        for ((x, y) <- this.nodes zip that.nodes) action(x.deref, y.deref)
      }
    }

    protected def startInputState(b: Block) = new State()

    protected def interpret(block: Block, state: State): Block = {
      block.pointsForward foreach {
        case assign @ AssignVar(_, value: ReadVar) =>
          debugLogsForVar("assign of read, read", value.variable)
          debugLogsForVar("assign of read, assigned", assign.variable)
          shouldNotReachHere(s"all uses of $value must already be updated with state value at $assign")

        case AssignVar(variable, value) =>
          state(variable) = value

        case read @ ReadVar(variable) =>
          val value = state(variable)
          if (value == Invalid) {
            debugLogsForVar("read before assign", variable)
            shouldNotReachHere("read before assign")
          }
          read.replaceValueUsesBy(value)

        case point: SpinalNode if point.hasXHandler =>
          addXCtrl(point.xpoint, state.fork())

        case _ =>
      }

      block
    }
  }

  def dgiForAllVars =
    allVars.map(dgiForVar).fold(DGIProvider.empty)(_ and _)

  def dgiForVar(v: Var) = DGIProvider { b =>
    val read   = b.spine find { case ReadVar(`v`)      => true; case _ => false }
    val assign = b.spine find { case AssignVar(`v`, _) => true; case _ => false }
    (read, assign) match {
      case (None, None) => null
      case (Some(_), None) => DGI(s"read#${v.id}", "red")
      case (None, Some(_)) => DGI(s"assign#${v.id}", "green")
      case (Some(r), Some(a)) =>
        if (r dominates a) {
          DGI(s"read+assign#${v.id}", "red:green")
        } else {
          DGI(s"assign+read#${v.id}", "green:red")
        }
    }
  }

  def debugLogsForVar(msg: String, v: Var): Unit = {
    val msgWithVar = s"$msg (Var${v.id})"
    dbgPrinter.debugNodes(msgWithVar)
    dbgPrinter.debugGraphs(msgWithVar, info = dgiForVar(v))
  }

  def debugLogsForVar(msg: String, vId: Int): Unit = {
    allVars find (_.id == vId) foreach (debugLogsForVar(msg, _))
  }
}
