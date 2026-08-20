/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes


import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.ir.*
import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Universe}

import collection.mutable.ArrayBuffer
import annotation.tailrec
import scala.PartialFunction.{cond, condOpt}
import com.huawei.excelsior.jet.compiler.RTSProc
import com.huawei.excelsior.jet.compiler.bytecode.BytecodePosition
import com.huawei.excelsior.jet.compiler.ir.{ColumnNumber, InlineContext, LexicalBlock, LineNumber}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.types.Approximation
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.ScalaCollections.{partialOrderingBy, singleElement}
import com.huawei.excelsior.jet.util.graph.Dominators

import scala.collection.immutable.ArraySeq
import scala.language.implicitConversions

/**
  * Collection of traits and classes for nodes and prototypes description.
  *
  * @author cypok
  * @author paul
  * @author conwor
  */
trait NodeSpells { self: Universe with Nodes =>

  // TODO-REDESIGN-GROUPS
  class Group(private[NodeSpells] var root: Node) {
    private val _attached: Maps[Node]#QMap[Group.AttachReason] = Maps[Node].newQMap[Group.AttachReason]

    def attached = _attached.keysIterator

    def attach(node: Node, reason: Group.AttachReason): Unit = {
      if (reason.isArg) {
        reason match {
          case Group.AttachReason.IMPLICIT_CHECK_ARG =>
          case Group.AttachReason.INSTANCE_OF_BRANCH =>
          case Group.AttachReason.CANGJIE_REFERENCE =>
            assert(node.singleOutEdge.isValue)
          case _ =>
            assert(node.singleUse == root)
            assert(node.singleOutEdge.isValue)
        }

      } else if (reason.isResult) {
        // We do not allow attached results have arguments except root, or in special cases - some of root arguments.
        // All this cases should be supported in LocalGenerator.specialActions
        // TODO: support arguments of attached results more accurate
        reason match {
          case _ =>
            assert(node.args forall { _ == root })
        }
      } else {
        shouldNotReachHere("unknown reason of group attachment")
      }
      _attached.put(node, reason) ensuring { _.isEmpty }
    }

    def detach(node: Node): Unit = {
      _attached.remove(node)
    }

    def inEdges: Iterator[Edge] = {
      val nodesSeq = nodes
      (nodesSeq flatMap { x => x.inEdges} filter { e => !nodesSeq.contains(e.source) }).iterator
    }

    def nodes: Seq[Node] = Seq(root) ++ attached

    def outEdges: Iterator[Edge] = {
      root.outEdges flatMap { outEdge =>
        if (_attached.contains(outEdge.target)) {
          outEdge.target.outEdges
        } else {
          Iterator.single(outEdge)
        }
      }
    }

    def attachedByReason(reasonCheck: Group.AttachReason => Boolean) =
      _attached.iterator collect { case (node, r) if reasonCheck(r) => node }

    def attachedResults = attachedByReason(_.isResult)

    def attachedArgs = attachedByReason(_.isArg)

    def changeRoot(newRoot: Node): Unit = {
      assert(!newRoot.hasGroup)
      root._group = null
      newRoot._group = this
      root = newRoot
    }

    def attachReason(x: Node): Group.AttachReason = _attached(x)
  }

  object Group {
    enum AttachReason {
      case INLINE_ADDR_MODE
      case LOAD_EXTEND_RESULT
      case COND_BRANCH_ARG
      case COND_BRANCH_ARG_CAS
      case COND_VAL_RESULT
      case IMPLICIT_CHECK_ARG
      case CALL_TARGET_ARG
      case INSTANCE_OF_BRANCH
      case MUT_FUNC_ARG
      case BOUND_MOV_ARG
      case RECORD_ARRAY_GET
      case DERIVED_PTR
      case COPY_STRUCTURE
      case CANGJIE_REFERENCE

      def isArg = this match {
        case INLINE_ADDR_MODE | COND_BRANCH_ARG | COND_BRANCH_ARG_CAS | IMPLICIT_CHECK_ARG |
             CALL_TARGET_ARG | INSTANCE_OF_BRANCH | BOUND_MOV_ARG | MUT_FUNC_ARG | RECORD_ARRAY_GET | DERIVED_PTR | COPY_STRUCTURE | CANGJIE_REFERENCE => true
        case _ => false
      }

      def isResult = this match {
        case COND_VAL_RESULT | LOAD_EXTEND_RESULT => true
        case _ => false
      }
    }
  }

  trait ResourcesInfo { self: Node =>

    private var _resource: Resource = InvalidResource

    final def mayHaveResource: Boolean = !hasGroup || groupRoot.groupResults.contains(this)

    final def resource: Resource = { assert(mayHaveResource, s"$this"); _resource }
    final def resource_=(r: Resource): Unit = { assert(mayHaveResource); _resource = r }

    final def allocatedToFrameSlot: Boolean = _resource.isInstanceOf[FrameSlot]

    /** Returns sequence of resources, allocated to all results of node group in natural order. */
    final def allResultResources: Seq[Resource] = {
      assert(isGroupRoot)
      groupResults.map(_.resource).toSeq
    }

    private var _spoiled: Seq[Resource] = Nil

    final def mayHaveSpoiled: Boolean = isGroupRoot

    final def spoiled: Seq[Resource] = { assert(mayHaveSpoiled); _spoiled }
    final def spoiled_=(s: Seq[Resource]): Unit = { assert(mayHaveSpoiled && (_spoiled eq Nil)); _spoiled = s }

    final def spoils(x: Resource): Boolean = spoiled contains x
    final def isResultResource(x: Resource): Boolean = allResultResources contains x

    final def isResultOrSpoiledResource(x: Resource): Boolean =
      isResultResource(x) || spoils(x)

    final def isArgumentResource(x: Resource): Boolean =
      groupedValueArgs exists { _.resource == x }
  }

  /** CodeGen info and short accesses. */
  trait CodeGenInformation { self: Node with ResourcesInfo =>

    // This flag is very dirty now. During code ordering it means that node is "already ordered" and
    // during next stages of backend (NullChecks optimizations, post-process) it means that node "should be generated".
    // TODO: refactor it
    var generated: Boolean = false

    /** Replaces this node in code generation order to given `node`. */
    def replaceInCodeGenOrderTo(node: Node): Unit = {
      assert(generated)
      node.generated = true
      generated = false
      CodeOrder.replace(this, node)
    }


    private[NodeSpells] var _group: Group = _

    def hasGroup =
      _group != null

    def group = {
      assert(hasGroup)
      _group
    }

    protected def getGroupOrCreate() = {
      if (!hasGroup) {
        _group = new Group(this)
      }
      _group
    }

    def isGroupRoot: Boolean =
      (_group == null) || (_group.root == this)

    def attachToGroup(root: Node, reason: Group.AttachReason): Unit = {
      assert(!hasGroup)
      _group = root.getGroupOrCreate()
      _group.attach(this, reason)
    }

    def detachFromGroup(): Unit = {
      if (hasGroup) {
        if (!isGroupRoot) {
          _group.detach(this)
        } else {
          _group.attached.foreach(_.detachFromGroup())
        }
        _group = null
      }
    }

    def hasAttachedResults: Boolean =
      hasGroup && isGroupRoot && group.attachedResults.nonEmpty

    def attachedTo(that: Node): Boolean =
      hasGroup && group.root == that

    def attachedAs(reason: Group.AttachReason): Boolean =
      !isGroupRoot && group.root.attachedByReason(reason).contains(this)

    def attachedAsArg: Boolean =
      hasGroup && group.root.attachedArgs.contains(this)

    def attachedAsResult: Boolean =
      hasGroup && group.root.attachedResults.contains(this)

    def groupRoot: Node =
      if (hasGroup) _group.root else this

    def groupedInEdges: Iterator[Edge] = if (hasGroup) {
      assert(isGroupRoot)
      group.inEdges
    } else inEdges

    // TODO: eliminate copy-paste with BDAG
    def groupedValueInEdges: Iterator[Edge] =
      groupedInEdges withFilter { e => e.isValue && !e.source.isInstanceOf[Projection] }

    def groupedValueArgs: Iterator[Node] = groupedInEdges collect { case e if e.isValue => e.source }

    def groupedArgs: Iterator[Node] = groupedInEdges map { _.source }

    def groupedUses: Iterator[Node] = groupedOutEdges map { _.target }

    def groupedOutEdges: Iterator[Edge] = if (hasGroup) {
      assert(isGroupRoot)
      group.outEdges
    } else outEdges

    def groupedValueUses: Iterator[Node] = groupedOutEdges collect { case e if e.isValue => e.target }

    def groupResults: Iterator[Node] = if (hasGroup) {
      assert(isGroupRoot)
      val attachedResults = group.attachedResults
      if (attachedResults.nonEmpty) {
        attachedResults
      } else {
        Iterator.single(this)
      }
    } else {
      Iterator.single(this)
    }

    def groupedValueResults: Iterator[Node] = groupResults filter { _.producesValue }

    def attachedResults: Iterator[Node] =
      if (hasGroup && isGroupRoot) group.attachedResults else Iterator.empty

    def attachedArgs: Iterator[Node] =
      if (hasGroup && isGroupRoot) group.attachedArgs else Iterator.empty

    def singleAttachedByReason(reason: Group.AttachReason): Option[Node] = {
      attachedByReason(reason).collectFirst { case x: Node => x }
    }

    def attachedByReason(reason: Group.AttachReason): Iterator[Node] =
      if (hasGroup && isGroupRoot) group.attachedByReason(_ == reason) else Iterator.empty

    def isAttachedByReason(reason: Group.AttachReason): Boolean = {
      if (hasGroup && !isGroupRoot) group.attachedByReason(_ == reason).contains(this) else false
    }

    def hasAttachedByReason(reason: Group.AttachReason): Boolean =
      if (hasGroup && isGroupRoot) group.attachedByReason(_ == reason).nonEmpty else false

    def allGroupNodes: Iterator[Node] = {
      if (hasGroup) {
        group.nodes.iterator
      } else {
        Iterator.single(this)
      }
    }

    def moveGroupInfoTo(that: Node): Unit = {
      if (this.hasGroup) {
        assert(!that.hasGroup)
        val group = this.group
        if (this.isGroupRoot) {
          group.changeRoot(that)
        } else {
          val reason = group.attachReason(this)
          group.detach(this)
          group.attach(that, reason)
        }
      }
    }
  }

  /**
   * Reference to a node. During an optimization cycle nodes can be replaced with new ones and removed.
   * deref() helps to get actual valid nodes that appeared after replacements.
   * If there is no replacement for a node that means that it was just killed.
   * Replacement nodes are linked to its origin node.
   * (decommitted) originNode.referent -> (decommitted) replacement(1).referent -> ... -> (decommitted) replacement(n).referent -> validNode
   * For each valid node the following condition should be true n.referent == n.
   * We set self-reference on node creation.
   * We set a new referent on replace and decommit operations.
   */
  trait NodeRef { self: Node =>
    private var referent: Node = _
    private [ir] def initSelfReference(): Unit = { referent = this }
    private [ir] def setReferent(that: Node): Unit = { referent = that }

    def deref: Node =
      derefRaw ensuring (_ != null, "try to dereference decommited node: " + this)

    def isReferentCommitted =
      derefRaw != null

    private def derefRaw: Node = {
      val n: Node = referent
      if (n == null || (n: NodeRef).referent == n) {
        n
      } else {
        referent = (n: NodeRef).derefRaw
        referent
      }
    }
  }

  /** Prototype of node where all args are fixed (no varargs).
   *
   *  <p>
   *  Types of the arguments of node are:
   *  <pre>
   *    fixedArgTypes(0),
   *    fixedArgTypes(1),
   *    ...,
   *    fixedArgTypes(fixedArgsCount-1)
   *  </pre>
   *  </p>
   */
  protected abstract class FixedArgs[N <: Node](argTypes: Type*)(resultType: Type)
    extends Prototype[N](argTypes: _*)(resultType) {

    val arity = fixedArgTypes.size
    final override def argType(idx: Int) = fixedArgTypes(idx)
  }


  /** Shortcut for [[com.huawei.excelsior.jet.compiler.opt.ir.Nodes.Prototype]] with var args.
    */
  abstract class VarArgs[N <: Node](argTypes: Type*)(varArgType: Type)(resultType: Type)
    extends Prototype[N](argTypes: _*)(resultType) {

    final override def argType(idx: Int) = {
      if (idx < fixedArgTypes.length) {
        fixedArgTypes(idx)
      } else {
        varArgType
      }
    }
  }


  /** Mark prototype which generates nodes of class `X` such as `X <: UpperN`.
    *
    * In general case prototype of node `N` generates nodes of any class because of
    * identity optimization (e.g. `Add(IConst(1), IConst(2))` can return node `IConst(3)`).
    *
    * @tparam N corresponding node type
    * @tparam UpperN upper bound of type of really generated node
    */
  trait PrototypeStrictNodeClass[N <: UpperN, UpperN <: Node] extends Prototype[N] {
    protected type StrictNodeClass = UpperN

    override def apply(args: Node*): UpperN = {
      super.apply(args: _*).asInstanceOf[UpperN]
    }

    override def withExplicitArgs(args: Node*): UpperN = {
      super.withExplicitArgs(args: _*).asInstanceOf[UpperN]
    }
  }


  /////////////////////////////////////////
  // Node traits

  /** Node that doesn't have a fixed point in the CFG,
    * but instead may have a range between `upperPoint` and `lowerPoint` assigned to it during GCM.
    * <p>
    * One can say that this node may "float" in the "sea of nodes", hence the name.
    */
  trait FloatingNode extends Node with NonControlNode {
    protected override final def compileTimeAssertThatNodeExtendsFloatingOrPinnedNode(): Unit = {}

    private var _point: UpperPoint = _

    def pinned: Boolean = _point != null

    def block: Block = {
      val point = upperPoint
      if (point == null) null else point.block
    }

    /** "Upper point": closest point above this node. `null` if node is not scheduled in CFG. */
    final def upperPoint: UpperPoint = {
      if (_point == null) {
        onPointRecalculation(this)
      }
      _point
    }

    /** "Lower point": closest point below this node. `null` if node is not scheduled in CFG. */
    final def lowerPoint: LowerPoint = {
      val point = upperPoint
      if (point == null) null else point.outCtrl
    }

    final def atUpperPoint(p: UpperPoint): this.type = {
      if (p != _point) {
        untie()
        if (p != null) tie(p)
        _point = p
      }
      this
    }

    final def atLowerPoint(p: LowerPoint): this.type = {
      val upper = if (p != null) p.inCtrl else null
      this atUpperPoint upper
    }

    /** Returns true iff this node returns [[FragilePointerType]]-d value which has real live range in code. */
    final def isFragilePointer: Boolean = tpe.isInstanceOf[FragilePointerType] &&
      !this.isInstanceOf[ExecEnv] && // ExecEnv is a singleton and can not be copied from EE-register, thus it is not fragile by itself
      !attachedAsArg
  }

  trait HasInMemory extends Node { self: HasInControl =>
    def inMemoryArg = 1
    final def inMemory: MemoryNode = arg(inMemoryArg).asInstanceOf[MemoryNode]
    final def inMemory_=(x: MemoryNode): Unit = { updateArg(inMemoryArg, x) }
    final def memoryEdge: Edge = inEdge(inMemoryArg)
  }

  object HasInMemory {
    def unapply(x: HasInMemory) = Some(x.inMemory)
  }


  trait MemoryNode extends UpperPoint {
    require(this hasTag Tag.MEMORY)

    final def memoryUses = usesByTag(Tag.MEMORY)

    final def memoryDependentFloatingNodes: Iterator[FloatingNode] = collect[FloatingNode](memoryUses)
  }

  // Note that any spinal memory node must also has memory in.
  trait SpinalMemoryNode extends SpinalNode with MemoryNode with HasInMemory


  /** Node that's always pinned to a fixed point in the block. */
  trait PinnedNode extends Node {
    protected override final def compileTimeAssertThatNodeExtendsFloatingOrPinnedNode(): Unit = {}

    def point: ControlNode
  }

  /** Node that's always pinned to the very beginning of a fixed block.
    * This block is _implicit_ parameter of a node.
    * I.e. it is not direct or prototype argument of node.
    */
  trait BlockParamNode extends PinnedNode with NonControlNode {
    override def block: Block
    override final def point: Block = block

    override def commitImpl(): Unit = {
      tie(block)
      super.commitImpl()
    }
  }

  /** Value-producing Node. */
  trait ProducesValue extends Node {
    require(hasTag(Tag.VALUE), s"$this")
    override final protected def compileTimeAssertThatNodeProducesValueOrNotProducesValue(): Unit = {}
  }

  /** Opposite of [[ProducesValue]], this Node doesn't produce Value. */
  trait NotProducesValue extends Node {
    require(!hasTag(Tag.VALUE), s"$this")
    override final protected def compileTimeAssertThatNodeProducesValueOrNotProducesValue(): Unit = {}
  }

  /** Data flow node that does not produce any control or memory, only value. */
  trait NonControlNode extends Node with ProducesValue {
    require(tagsSeq == Seq(Tag.VALUE))

    protected override final def compileTimeAssertThatNodeExtendsControlOrNonControlNode(): Unit = {}
  }

  /** Node that forms control skeleton of the IR: blocks, spinal nodes, projections & block ends.
    * May have any number of input and output control edges.
    */
  trait ControlNode extends PinnedNode with StructurallyUnique { selfNode =>
    require(this hasTag Tag.CONTROL)

    protected override final def compileTimeAssertThatNodeExtendsControlOrNonControlNode(): Unit = {}

    final def controlUses = usesByTag(Tag.CONTROL)

    override final def point: ControlNode = this

    private[ir] def blockControlNum: Long

    this.nextPinned = this
    this.prevPinned = this

    /** Iterator over nodes pinned at this point (including `this` point itself). */
    final def pinnedNodes: Iterator[Node] = ScalaCollections.iterateUntilNull[Node](selfNode.nextPinned) {
      case `selfNode` => null
      case n => n.nextPinned
    }

    private[ir] def clearPinned(): Unit = {
      var curr: Node = this.nextPinned
      while (curr != null) {
        val next = curr.nextPinned
        curr.nextPinned = null
        curr.prevPinned = null
        curr = next
      }
    }

    private[ir] override def decommitImpl(): Unit = {
      clearPinned()
      super.decommitImpl()
    }

    final def memoryAfter: MemoryNode = this match {
      case block: Block =>
        if (block.redefinesMemory) {
          block
        } else {
          assert(block.isInstanceOf[BBlock]) // rework when xblock memory is optimized

          val reachableMemIn = block.reachableMemoriesBefore
          if (reachableMemIn.isEmpty) {
            block.scope.entryMemory
          } else {
            singleElement(reachableMemIn.toSet)
          }
        }

      case m: MemoryNode => m
      case HasInMemory(inMem) => inMem
      case exit: Branch.Exit => exit.owner.memoryAfter
      case sp: SpinalNode => sp.memoryBefore
      case _: UnreachableBlockEnd.Exit => unreachableBar
    }

    final def dominates(that: ControlNode): Boolean = {
      if (this eq that) return true
      val (thisS, thatS) = (this.scope, that.scope)

      if (thisS != thatS) {
        assert(thisS.level != thatS.level)
        thisS.level < thatS.level && (this dominates thatS.anchor)

      } else if (this.block eq that.block) {
        this.block.refreshControlNums()
        this match {
          case _: Projection => false
          case _ => this.blockControlNum <= Projection.skip(that).blockControlNum
        }

      } else this match {
        case SingleXPointInputInHandler(handler) => handler dominates that
        case _: XPoint => false

        case SingleBranchExitInputInBlock(block) => block dominates that

        case _ if !scope.hasXEdges && !this.isInstanceOf[Branch.Exit] =>
          scope.cfg.dominators.dominates(this.block, that.block)

        case _ =>
          // scope.dominators calculate dominators over SplitUCFG graph
          // where every block is splitted into two ones and first of them has xedge to xhandler.
          //
          // This is very handy for calculation of dominance of two SpinalNodes (this dom that):
          // if this node is after any throwing SpinalNode in this.block
          // we assume that exception might be thrown and check if second part of block dominates that.block;
          // if this node is before or equal to first throwing SpinalNode in this.block
          // we may check that first part of block dominates that.block.
          //
          // Note that in both cases we take first part of that.block because that.block always dominates that.
          def findSplit(n: ControlNode): ControlNode = n match {
            case _: Block | _: BlockEnd | _: Branch.Exit => n
            case n: SpinalNode => n.block.xpoints.nextOption() match {
              case Some(xp) if xp.hasHandler =>
                n.block.refreshControlNums()
                if (n.blockControlNum <= xp.owner.blockControlNum) n.block else n.block.blockEnd
              case _ => n.block
            }
          }

          assert(this.block.xHandlers.size <= 1, s"block ${this.block} has multiple different xHandlers")
          if (!areDifferentXHandlersInBlockAllowed) {
            assert(this.block.xpoints.distinctBy(_.hasHandler).length <= 1,
              s"xPoints with and without xHandler exist in one block ${this.block}")
          }

          scope.splitUCFG.dominators.dominates(findSplit(this), that.block)
      }
    }

    final def strictDominates(that: ControlNode) = (this ne that) && (this dominates that)

    final def domComparable(that: ControlNode) = (this dominates that) || (that dominates this)

    /** Returns immediate dominator of this node. May be `null` in case of `entryBlock`. */
    final def idom: ControlNode = this match {
      case n: LowerPoint => n.inCtrl
      case n: Projection => n.owner
      case XBlockWithSingleInput(x) => x // special case not handled by CFG dominators
      case n: Block =>
        // TODO: consider using somehow scope.cfg to accelerate this method
        refineCFGDominatorToControlDominator(scope.splitUCFG.dominators.idom(n))(_ dominates this)
      case _ => shouldNotReachHere(this)
    }

    /** Returns iterator over dominators of this node (starting from this node). */
    final def doms: Iterator[ControlNode] = ScalaCollections.iterateUntilNull(this)(_.idom)

    /** Nearest (most control-constrained) dominator of `this` and `that`.
      * @see [[Dominators.nearest()]]
      */
    final def nearestDom(that: ControlNode): ControlNode = {
      if (this == that) return this
      val (thisS, thatS) = (this.scope, that.scope)

      if (thisS.level > thatS.level) {
        that nearestDom thisS.anchor
      } else if (thatS.level > thisS.level) {
        this nearestDom thatS.anchor
      } else {
        assert(thisS == thatS)

        if (this.block == that.block) {
          this.block.refreshControlNums()
          val (x, y) = (Projection.skip(this), Projection.skip(that))
          if (x.blockControlNum <= y.blockControlNum) x else y

        } else {
          def xpointCase(xp: XPoint, another: ControlNode) = xp match {
            case SingleXPointInputInHandler(handler) =>
              handler nearestDom another match {
                case `handler` => xp
                case nonHandler => nonHandler
              }

            case _ => xp.owner nearestDom another
          }

          def branchExitsCase(exit: Branch.Exit, another: ControlNode) = exit match {
            case SingleBranchExitInputInBlock(block) =>
              block nearestDom another match {
                case `block` => exit
                case nonBlock => nonBlock
              }

            case _ => exit.owner nearestDom another
          }

          (this, that) match {
            case (xp: XPoint, _) => xpointCase(xp, that)
            case (_, xp: XPoint) => xpointCase(xp, this)

            case (exit: Branch.Exit, _) => branchExitsCase(exit, that)
            case (_, exit: Branch.Exit) => branchExitsCase(exit, this)

            case _ if !scope.hasXEdges =>
              val (thisBlock, thatBlock) = (this.block, that.block)
              scope.cfg.dominators.nearest(thisBlock, thatBlock) match {
                case `thisBlock` => this
                case `thatBlock` => that
                case x => x.blockEnd
              }

            case _ =>
              def blockEndOrExit(n: ControlNode) = n match {
                case x: Branch.Exit => x
                case _ => n.block.blockEnd
              }
              val (thisEnd, thatEnd) = (blockEndOrExit(this), blockEndOrExit(that))

              scope.splitUCFG.dominators.nearest(thisEnd, thatEnd) match {
                case `thisEnd` => this
                case `thatEnd` => that
                case z => refineCFGDominatorToControlDominator(z) { p => (p dominates this) && (p dominates that) }
              }
          }
        }
      }
    }

    private def refineCFGDominatorToControlDominator(dom: ControlNode)(p: ControlNode => Boolean): ControlNode = {
      dom match {
        case null | _: BlockEnd | _: Branch.Exit => dom
        case b: Block =>
          // Refine CFG dom to exact spinal node
          b.spineBackward find p getOrElse (b ensuring p)
        case _ => shouldNotReachHere(dom)
      }
    }

    private object XBlockWithSingleInput {
      def unapply(xb: XBlock): Option[XPoint] = ScalaCollections.singleton(xb.inputs)
    }

    private object SingleXPointInputInHandler {
      def unapply(xp: XPoint): Option[XBlock] = {
        if (xp.hasHandler) {
          val handler = xp.handler
          if (handler.inputs.size == 1) {
            assert(handler.inputs.head == xp)
            Some(handler)
          } else {
            None
          }
        } else {
          None
        }
      }
    }

    private object SingleBranchExitInputInBlock {
      def unapply(exit: Branch.Exit): Option[BBlock] = {
        val target = exit.target
        if (target.inputs.size == 1) {
          assert(target.inputs.head == exit)
          Some(target)
        } else {
          None
        }
      }
    }

    def projections: Seq[ControlNode] = Seq.empty
  }

  object ControlNode {
    private[nodes] val InsertionStepControlNum = 1L << 20
    private[nodes] val StepControlNum          = 1L << 30
    private[nodes] val InvalidControlNum       = -1L
    private[nodes] val BlockStartControlNum    = 0L
    private[nodes] val BlockEndControlNum      = Long.MaxValue - 1
    private[nodes] val BranchExitControlNum    = Long.MaxValue

    /** Chooses the ''lowest'' node among `x` and `y` (the one that is dominated by the other).
      *
      * @throws IllegalArgumentException if the nodes do not dominate one another.
      */
    def lowest[N <: ControlNode](x: N, y: N): N = {
      if (x dominates y) y
      else if (y dominates x) x
      else throw new IllegalArgumentException(s"Incomparable control nodes: $x and $y")
    }
  }

  /** A node that receives control token
   * (has the only control argument -- it must be the first argument).
   */
  trait HasInControl extends Node {
    final def inCtrlArg = 0
    final def inCtrl: UpperPoint = arg(inCtrlArg).asInstanceOf[UpperPoint]
    final def inCtrl_=(x: UpperPoint): Unit = { updateArg(inCtrlArg, x) }
    final def inCtrlEdge = inEdge(inCtrlArg)

    def notControlArgs: Seq[Node] = argsTail(1)

    /** Internal method for compile-time insurance that each node with incoming control extends either [[ControlledNode]] or [[LowerPoint]]. */
    protected def compileTimeAssertThatHasInControlExtendsControlledOrLowerPointNode(): Unit
  }

  object HasInControl {
    def unapply(n: HasInControl) = Some(n.inCtrl)
  }

  /** A node that has control dependency, but doesn't produce control result itself. */
  trait ControlledNode extends FloatingNode with HasInControl {
    protected override final def compileTimeAssertThatHasInControlExtendsControlledOrLowerPointNode(): Unit = {}
  }

  /** Node which could be optimized (pulled up) by context type of it's argument. */
  trait ContextDependentNode extends ControlledNode {

    /** Returns either argument of `this` node which context type may be used to optimize `this` node position in IR,
      * or null, iff context types optimization is not possible for `this` node now. */
    def contextKey: Node

    /** Returns type of [[contextKey]] which should be at `this`.[[inCtrl]] point. */
    def requiredKeyType: Approximation
  }

  object ContextDependency {
    def unapply(edge: Edge): Option[(Node, ContextDependentNode)] = edge match {
      case Edge(key, dependentNode: ContextDependentNode) if dependentNode.contextKey == key => Some((key, dependentNode))
      case _ => None
    }
  }

  /** [[ControlNode]] with single control input which comes from [[UpperPoint]].
    *
    * Such nodes may be [[FloatingNode.lowerPoint]] meaning that there is a space in CFG above this node where GCM
    * may place floating nodes.
    *
    * [[SpinalNode]] and [[BlockEnd]] are such nodes.
    */
  trait LowerPoint extends ControlNode with HasInControl {
    protected override final def compileTimeAssertThatHasInControlExtendsControlledOrLowerPointNode(): Unit = {}

    final def memoryBefore: MemoryNode = this match {
      case HasInMemory(inMem) => inMem
      case _ => inCtrl.memoryAfter
    }

    def constraintsOption: Option[Constraints] = ScalaCollections.singleton(collect[Constraints](controlUses))

    final def constraints: Constraints = constraintsOption.get

    final def hasConstraints: Boolean = constraintsOption.nonEmpty

    def addConstraints(): Constraints = {
      assert(!hasConstraints)
      val constraints = Constraints(this) atLowerPoint this
      if (CodeOrder contains this) {
        CodeOrder.insertAfter(this, constraints)
      }
      constraints
    }
  }

  /** [[ControlNode]] with single control output which goes to [[LowerPoint]].
    *
    * Such nodes may be [[FloatingNode.upperPoint]] meaning that there is a space in CFG below this node where GCM
    * may place floating nodes.
    *
    * [[Block]] and [[SpinalNode]] are such nodes.
    */
  trait UpperPoint extends ControlNode {
    private[this] var _outCtrl: LowerPoint = _
    private[this] var _outCtrlCount = 0

    def outCtrl: LowerPoint = {
      assert(_outCtrlCount == 1)
      if (_outCtrl eq null) {
        _outCtrl = ScalaCollections.singleElement(collect[LowerPoint](controlUses))
      }
      _outCtrl
    }

    /** Note: `null` may be if block has no blockEnd (e.g. during lowering). */
    def outCtrlOrNull: LowerPoint = {
      // TODO: assert that controlUses.size <= 1
      if (_outCtrlCount != 1) null else outCtrl
    }

    protected override def useAdded(use: Edge): Unit = {
      if (use.isControl && use.target.isInstanceOf[LowerPoint]) {
        _outCtrlCount += 1
        _outCtrl = if (_outCtrlCount != 1) null else use.target.asInstanceOf[LowerPoint]
      }
      super.useAdded(use)
    }

    protected override def useRemoved(use: Edge): Unit = {
      if (use.isControl && use.target.isInstanceOf[LowerPoint]) {
        _outCtrlCount -= 1
        _outCtrl = null
      }
      super.useRemoved(use)
    }

    def blockRef: BlockRef
  }

  /**
   * Control node that receives and produces control token.
   * Has one control input and one (non-exceptional) control output.
   * <br/>
   * Spinal nodes form skeleton/spine of a basic block.
   */
  trait SpinalNode extends LowerPoint with UpperPoint {

    /** Returns whether this operation can throw exception in run-time.
      * Operation canThrow if and only if it has XPoint.
      */
    def canThrow: Boolean = false

    def hasXSite: Boolean = canThrow

    private var _xpoint: XPoint = _

    protected override def useAdded(use: Edge): Unit = {
      if (use.sourceLabel == Tag.XCONTROL) {
        assert(_xpoint eq null)
        _xpoint = use.target.asInstanceOf[XPoint]
        spineChanged(keepControlNums = true)
      }
      super.useAdded(use)
    }

    protected override def useRemoved(use: Edge): Unit = {
      if (use.sourceLabel == Tag.XCONTROL) {
        assert(_xpoint eq use.target)
        _xpoint = null
        spineChanged(keepControlNums = true)
      }
      super.useRemoved(use)
    }

    /** Returns Some(XPoint), if this spinal node has XPoint, or None otherwise. */
    final def xpointOption: Option[XPoint] = Option(_xpoint)

    /** Returns true iff this operation has XPoint.
      * It's low level operation, use with care.
      * In most cases `canThrow` should be used.
      */
    final def hasXPoint = _xpoint ne null

    /** Returns XPoint for this spinal node. */
    final def xpoint = _xpoint ensuring (_ ne null)

    // TODO: refactor these methods
    final def xHandlerOption: Option[XBlock] = if (hasXHandler) Some(xHandler) else None

    /** Returns true iff this operation has exception handler. */
    final def hasXHandler: Boolean = hasXPoint && xpoint.hasHandler

    /** Returns XBlock for this node. */
    final def xHandler: XBlock = xpoint.handler

    override def projections: Seq[ControlNode] = xpointOption.toSeq

    override final def block: Block = blockRef.block

    private var _blockRef: BlockRef = BlockRef.INVALID

    override final def blockRef: BlockRef = {
      if (_blockRef.invalidated && inCtrl != null) {
        _blockRef = inCtrl.blockRef
      }
      _blockRef
    }

    private[ir] def spineChanged(keepControlNums: Boolean): Unit = {
      if (isCommitted && block != null) { // JET-10644 workaround
        block.spineChanged(keepControlNums)
      }
    }

    /** Remove XPoint if any.
      * It's dangerous operation because breaks IR consistency, use with care.
      */
    final def removeXPoint(): Unit = {
      for (xp <- xpointOption) {
        xp.nullifyArgs()
        if (xp.hasHandler) {
          makeUnreachable(xp.xEdge)
        }
        decommit(xp)
      }
    }

    override def argChanged(idx: Int): Unit = {
      if (idx == inCtrlArg && isCommitted) {
        spineChanged(keepControlNums = false)

        if (inCtrl != null && !_blockRef.invalidated) {
          assert(inCtrl.blockRef == _blockRef)
        }
      }
      super.argChanged(idx)
    }

    private[ir] var blockControlNum = ControlNode.InvalidControlNum

    private[ir] override def commitImpl(): Unit = {
      super.commitImpl()

      if (canThrow) {
        XPoint(this)
      }

      blockControlNum = inCtrl.blockControlNum + ControlNode.StepControlNum
      if (blockControlNum <= ControlNode.BlockStartControlNum || ControlNode.BlockEndControlNum <= blockControlNum) {
        block.invalidateControlNums()
      }
    }

    private[ir] override def decommitImpl(): Unit = {
      for (xpoint <- xpointOption) {
        decommit(xpoint)
      }
      super.decommitImpl()
    }
  }

  object SpinalNode {
    def sideEffectFree(n: SpinalNode): Boolean = cond(n) {
      case _: Marker | _: AssertNode | _: RawValueRangeFilter => true
      case n: PureCheck if n.trusted => true
      case n: PreparationCheck if !n.canThrow => true
    }
  }

  /** Marker trait that marks all SpinalNode that can throw implicit exceptions.
    * All such nodes must have input memory.
    */
  trait CanThrow extends HasInMemory { self: SpinalNode =>
    override def canThrow = true
  }


  /** Marks all spinal node that has no side effects except some check, e.g. NullCheck.
    *
    * Trusted checks in enduser mode are not generated and in work mode they should be generated using fatal errors
    * (work mode generation is not implemented for all checks currently).
    */
  abstract class PureCheck(proto: PureCheckPrototype[_ <: PureCheck]) extends NodeWithFixedArgs(proto) with Idempotent {
    // Backend specific method marking checks, that will be performed implicitly
    // by other node with MayHaveImplicitCheck.
    def isImplicit: Boolean = attachedAs(Group.AttachReason.IMPLICIT_CHECK_ARG)

    def trusted = proto.trusted

    override def name: String = simpleName + (if (trusted) "[trusted]" else "")

    override def protoIdempotents(thatProto: Prototype[_]) =
      this.proto.getClass == thatProto.getClass &&
        this.proto.argsExceptTrusted == thatProto.asInstanceOf[PureCheckPrototype[_]].argsExceptTrusted
  }

  abstract class PureCheckPrototype[N <: PureCheck](argTypes: Type*)(resType: Type)(_argsExceptTrusted: Any*)
    extends FixedArgs[N](argTypes: _*)(resType) with SpinalNodePrototype[N] {

    def trusted: Boolean
    private[NodeSpells] val argsExceptTrusted = Seq.from(_argsExceptTrusted)
  }


  trait ThrowingPureCheck extends PureCheck with CanThrow {
    final override def canThrow: Boolean = !trusted

    /** Information about error (RT-procedure and its arguments) thrown in case of check failure. */
    def throwInfo: (RTSProc, Seq[Node])
    def throwProc: RTSProc = throwInfo._1
  }

  /** Special nodes holding some internal information and generated as nop. */
  trait Marker extends SpinalNode


  /** Marker trait that marks all SpinalNode prototypes */
  trait SpinalNodePrototype[N <: SpinalNode] extends Prototype[N] with PrototypeStrictNodeClass[N, N]


  /**
   * A node that idempotent with respect to control flow,
   * i.e. several subsequent applications of the node with the same data arguments equal to just one application.
   * Usually, it is runtime checks such as nullcheck or clinit that is enough to perform only once on supplied data.
   */
  trait Idempotent extends SpinalNode {

    protected def protoIdempotents(thatProto: Prototype[_]): Boolean = this.proto == thatProto

    /** Returns iterator over value arguments which should be checked for equality for two idempotent nodes to optimize them. */
    def idempotentValueArgs: Iterator[Node] = this.valueArgs

    /** Returns whether `this` node idempotents another `that` node.
     *  They should be structural equal except of control argument:
     *  `this` node should strictly dominate `that` by control.
     */
    def idempotents(that: Idempotent): Boolean = {
      assert(this.isCommitted && that.inCtrl.isCommitted)
      (this != that) &&
      (this.arity == that.arity) &&
      protoIdempotents(that.proto) &&
      (this.idempotentValueArgs sameElements that.idempotentValueArgs) &&
      ( (this == that.inCtrl) || {         // If `this.xOutEdge` dominates `that` then ignore it.
        assert(this.outCtrl.isCommitted)   // We cannot just check `this.outCtrl dominates that`
        this.outCtrl dominates that.inCtrl // as `that` may be not committed yet.
      })
    }
  }


  /** Marker trait for complex high-level nodes which are decomposed to low-level nodes during lowering. */
  trait CompositeNode extends Node

  /////////////////////////////////////////
  // Common node classes

  /** Base class for a node that has args. */
  abstract class NodeWithArgs(final val proto: Prototype[_ <: Node]) extends Node {

    /** Different node instances never equal to each other */
    final override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
    final override def hashCode() = System.identityHashCode(this)
  }

  /** Base class for a node that has varargs. */
  abstract class NodeWithVarArgs(proto: Prototype[_ <: Node]) extends NodeWithArgs(proto) {

    /** Adds arg to node and returns created argument edge. */
    final def addArg(n: Node): Edge = {
      val idx = duLinks.size
      addArgs(Seq(n))
      inEdge(idx)
    }

    final def addArgs(ns: Seq[Node]): Unit = { setArgs(duLinks.size, ns.toIndexedSeq) }

    /** Duplicate argument at given index `n`-times (insert `n-1` copies of it). */
    final def duplicateArg(idx: Int, n: Int): Unit = {
      val x = arg(idx)
      replaceArgBy(idx, n, { _ => x })
    }

    /** Replace argument at given index by seq of `newArgs`. */
    final def replaceArgBySeq(idx: Int, newArgs: Seq[Node]): Unit = {
      replaceArgBy(idx, newArgs.size, newArgs)
    }

    /** Replace argument at given index by given args of size `n`. */
    private def replaceArgBy(idx: Int, n: Int, newArg: (Int => Node)): Unit = {
      assert(n >= 1)
      if (n == 1) {
        updateArg(idx, newArg(0)) // fast path
      } else {
        val oldSize = duLinks.size
        resizeArgs(oldSize + n - 1)
        for (i <- ((idx+1) until oldSize).reverse) updateArg(i + n - 1, arg(i)) // move tail of args
        for (i <- 0 until n) updateArg(idx + i, newArg(i)) // set new args
      }
    }

    final override def replaceArgsBySeq(as: Seq[Node]): Unit = { setArgs(0, as.toIndexedSeq) }

    private[nodes] def removeInEdges(indices: Iterable[Int]): Unit = {
      val idxSet = indices.toSet
      val (removed, retained) = duLinks partition (idxSet contains _.targetArgIndex)
      removed foreach (_.source = null) // trigger callbacks for structural change
      for (i <- retained.indices) {
        val e = retained(i)
        if (e.targetArgIndex != i) {
          val s = e.source
          // trigger callbacks for structural change (before index update)
          e.source = null
          // update edge index
          e.targetArgIndex = i
          // update location in duLinks
          duLinks(i) = e
          // trigger callbacks for structural change (after index update)
          e.source = s
        }
      }
      duLinks.remove(retained.size, removed.size)
    }

    protected val duLinks: ArrayBuffer[DULink] = ArrayBuffer.empty[DULink]

    private def resizeArgs(newSize: Int): Unit = {
      assert(newSize >= proto.fixedArgsCount)
      val oldSize = duLinks.size
      if (newSize > oldSize) {
        (oldSize until newSize) foreach { duLinks += new DULink(_) }
      } else if (newSize < oldSize) {
        (newSize until oldSize) foreach { updateArg(_, null) } // remove uses
        duLinks.remove(newSize, oldSize - newSize)
      }
    }

    private def setArgs(fromPos: Int, as: IndexedSeq[Node]): Unit = { //TODO: remove code duplication with Node::replaceArgsBySeq
      if (fromPos + as.size != duLinks.size) {
        resizeArgs(fromPos + as.size)
      }
      for (i <- as.indices) {
        updateArg(fromPos + i, as(i))
      }
    }
  }

  /** Base class for a floating node that has varargs. */
  abstract class FloatingNodeWithVarArgs(proto: Prototype[_ <: FloatingNode]) extends NodeWithVarArgs(proto) with FloatingNode

  /** Base class for a node with fixed number of arguments (no varargs). */
  abstract class NodeWithFixedArgs(proto: FixedArgs[_ <: Node]) extends NodeWithArgs(proto) {
    //TODO: make specialized impls for arity of 1, 2 or 3 ??
    protected val duLinks: collection.IndexedSeq[DULink] = Array.tabulate(proto.arity){ i => new DULink(i) }
  }

  /** Base class for a floating node with fixed number of arguments (no varargs). */
  abstract class FloatingNodeWithFixedArgs(proto: FixedArgs[_ <: FloatingNode]) extends NodeWithFixedArgs(proto) with FloatingNode


  trait FlagProducer extends Node

  /**
   * Cache of standard node tags types. Used to not store such values in each tagged node.
   */
  object Tags {

    val seqFromMask: Array[Seq[Tag]] = Array.tabulate(Tag.ALL_VALID_MASK + 1) { mask0 =>
      val buf = ArrayBuffer.empty[Tag]
      var mask = mask0
      var id = 0
      while (mask != 0) {
        val tag = Tag.fromOrdinal(id)
        if (tag containsInMask mask) {
          buf += tag
          mask &= ~tag.asMask
        }
        id += 1
      }
      assert(id <= Tag.VALUES.length)
      ArraySeq.from(buf)
    }

    val controlMask = Tag.CONTROL.asMask | Tag.XCONTROL.asMask
    val controlValueMask = controlMask | Tag.VALUE.asMask
    val controlMemoryMask = controlMask | Tag.MEMORY.asMask
    val controlMemoryValueMask = controlMemoryMask | Tag.VALUE.asMask
  }

  // TODO: refactor this

  trait ControlTagged[N <: SpinalNode] extends SpinalNodePrototype[N] {
    final override def tagsMask = Tags.controlMask
  }

  trait ControlValueTagged[N <: SpinalNode] extends SpinalNodePrototype[N] {
    final override def tagsMask = Tags.controlValueMask
  }

  trait ControlMemoryTagged[N <: SpinalMemoryNode] extends SpinalNodePrototype[N] {
    final override def tagsMask = Tags.controlMemoryMask
  }

  trait ControlMemoryValueTagged[N <: SpinalMemoryNode] extends SpinalNodePrototype[N] {
    final override def tagsMask = Tags.controlMemoryValueMask
  }



  /** Node without node arguments. */
  abstract class LeafNode[N <: LeafNode[N]](_tpe: Type)
    extends Prototype[N]()(_tpe) with Node
      with StructurallyUnique with PrototypeStrictNodeClass[N, N] { self: N =>

    override def tpe = _tpe

    final override def tagsMask = tpe.tag.asMask

    final override def proto: this.type = this
    final override protected def newInstance(): N = this

    final override protected def duLinks = IndexedSeq.empty[DULink]
    final override def argType(idx: Int) = shouldNotCallThis("LeafNode.argType")

    /** Leaf nodes cannot be implicitly optimized on commit, thus they are `exact` by nature. */
    final override def isExact = true

    /** Fast path for already created nodes. */
    override def apply(args: Node*): N = if (isCommitted) this else super.apply(args*)
  }

  abstract class CachedLeafNode[N <: CachedLeafNode[N]](tpe: Type) extends LeafNode[N](tpe) with Product1[Any] { self: N =>
    def cacheKey: Any

    def _1 = cacheKey
    override def canEqual(that: Any) = true

    final override def equals(that: Any): Boolean = (this eq that.asInstanceOf[AnyRef]) || (that match {
      case that: CachedLeafNode[_] => this.getClass == that.getClass && this.cacheKey == that.cacheKey
      case _ => false
    })

    final override val hashCode: Int = (getClass.## * 31) + cacheKey.##
  }


  trait ArgDependentTypeNode extends Node {
    def isTypeDependency(edge: Edge): Boolean

    override def argChanged(idx: Int): Unit = {
      if (isTypeDependency(inEdge(idx))) {
        invalidateNodeType(this)
      }
      super.argChanged(idx)
    }
  }

  object DebugBreakpointWithKnownInfo {

    def hasSourceFile(pos: BytecodePosition) = {
      // in CJ method.hasSourceFile (when it has)
      // while in java it is obtained from declaring class
      val m = pos.inlineContext.method
      val cls = m.getDeclaringClass
      val sf = if (cls.isAJArray) null else cls.getSourceFile
      m.hasSourceFile || sf != null && sf.nonEmpty
    }

    def unapply(n: Node): Option[(InlineContext, Int, Int, LexicalBlock)] = condOpt((n, n.pos)) {
      case (_: DebugBreakpoint, pos: BytecodePosition) if LineNumber.isKnown(pos.lineNumber) && hasSourceFile(pos) =>
        val column = if (ColumnNumber.isKnown(pos.columnNumber)) pos.columnNumber else 0
        (pos.inlineContext, pos.lineNumber, column, pos.scope)
    }
  }
}
