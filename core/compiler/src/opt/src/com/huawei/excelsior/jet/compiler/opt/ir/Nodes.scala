/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, NoPosition, Position}

import java.lang.InternalError
import com.huawei.excelsior.jet.compiler.opt.ir.nodes.*
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.{DisjointSet, ScalaCollections}

import scala.PartialFunction.condOpt
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NonFatal

/**
 * The nodes of the IR.
 *
 * @author paul
 * @author cypok
 */
trait Nodes extends NodeSpells with AJNodes with KernelNodes with SimpleNodes with ObjectOperationNodes with DeferredNodes with BitcodeDeferredNodes with SpecialNodes with CompileTimeIntrinsicNodes with UniversalGenericNodes with MutFuncNodes with CangjieNodes { self: Universe =>

  /**
   * Edge connects two Nodes (source -> target) and has two labels: sourceLabel & targetLabel.
   * Edge represents def-use link between `source` and `target`.
   * Edge has immutable target and mutable source.
   * Any change to the edge's source modifies corresponding def-use link in the IR.
   */
  trait Edge {
    def source: Node
    def target: Node
    def sourceLabel: Tag //TODO: replace Tag by real label

    def targetArgIndex: Int //TODO: replace this method by targetLabel
    //def targetLabel: Label

    def source_=(n: Node): Unit

    /**
     * Returns block to which this use belongs to.
     *
     * If target node is Phi, returns predecessor block according to this edge as
     * Phi data inputs are treated in special way: they reside in corresponding predecessor blocks.
     */
    def useBlock: Block = target match {
      case phi: Phi => phi.controlInput(this).source.block
      case _ => target.block
    }

    def usePoint: ControlNode = target match {
      case phi: Phi => phi.controlInput(this).source.asInstanceOf[ControlNode]
      case target: FloatingNode => target.lowerPoint
      case target: PinnedNode => target.point
    }

    override def toString: String = s"Edge($source, $target)"

    def isValue = (sourceLabel == Tag.VALUE)

    def isControl = if (sourceLabel != Tag.CONTROL) false else {
      assert(source.isInstanceOf[ControlNode])
      true
    }

    def isMemory = if (sourceLabel != Tag.MEMORY) false else {
      assert(source.isInstanceOf[MemoryNode] && target.isInstanceOf[HasInMemory])
      true
    }

    def useType: Type = target.proto.argType(targetArgIndex)
  }

  object Edge {
    def unapply(e: Edge) = Some(e.source, e.target)
  }

  object TaggedEdge {
    def unapply(e: Edge) = Some(e.sourceLabel, e.source, e.target)
  }

  object ControlEdge {
    def unapply(e: Edge) = {
      if (e.sourceLabel != Tag.CONTROL) None
      else Some((e.source.asInstanceOf[ControlNode], e.target))
    }
  }

  object MemoryEdge {
    def unapply(e: Edge) = {
      if (e.sourceLabel != Tag.MEMORY) None
      else Some((e.source.asInstanceOf[MemoryNode], e.target.asInstanceOf[HasInMemory]))
    }
  }

  object ValueEdge {
    def unapply(e: Edge) = {
      if (e.sourceLabel != Tag.VALUE) None
      else Some((e.source, e.target))
    }
  }

  class EdgeMatcher[N <: Node : ClassTag](val index: Int) {
    def unapply(e: Edge): Option[N] = (e.target, e.targetArgIndex) match {
      case (n: N, `index`) => Some(n)
      case _ => None
    }
  }

  /**
   *  Abstract IR element.
   *  Represent both value & operation.
   *  <pre>
   *  Any particular Node has the following properties:
   *
   *  $ 1. Node has arguments that are also nodes. Implicitly all arguments fall into one of three categories:
   *
   *  $  -- control arguments. Nodes that explicitly transfer control to this node.
   *        Usual nodes has one or no control argument, but there is a special Node named
   *        [[com.huawei.excelsior.jet.compiler.opt.ir.Nodes.Block Block]] representing
   *        start of a ''Basic Block'' that can have multiple control arguments (control input).
   *
   *        Node has one control argument if its execution explicitly depends on sequential execution of the incoming node.
   *        For instance, two subsequent calls cannot be interchanged because they may have side effects
   *        thus they depends on control of each other so the second node has control argument of the first node.
   *        Nodes that have control arguments: calls, return, throw, clinit, null check, index check, arrstore check,
   *            MonitorEnter/MonitorExit.
   *
   *        Node has no control arguments if its evaluation does not depend explicitly on control
   *        but only on other arguments. It is constants, arithmetic operations etc.
   *
   *  $  -- ordinary or data arguments. It is arguments for a call, left/right operands of binary operations etc.
   *
   *  $  -- global memory argument. Some operations change global memory state and other depends on global memory state,
   *        so the later operations have global memory argument.
   *
   *  // TODO: update this comment with new conception: tags
   *  $ 2. Node may have one or multiple results that are also nodes.
   *
   *     Results also fall into one of three categories: control, data and memory.
   *
   *  $  -- Node that has one control argument also have control result except
   *        some BlockEnd operations
   *        Thus they linearly linked by control in one basic block.
   *        Node can have multiple control results: conditional branches and switches.
   *        Throw, return, goto, branches, switches forms end of Basic Block.
   *
   *  $  -- Node can have data results (usually one data result) and only one global memory result.
   *
   *     If a node has one result the result is the node itself.
   *     If a node has multiple results they are represented by so called projections of the node
   *
   *     Only some BlockEnd nodes have no meaning results that can be consumed by other nodes.
   *
   *     Nodes with control argument and control result forms skeleton of Basic Block (
   *     see [[com.huawei.excelsior.jet.compiler.opt.ir.Nodes.Block Block]] bellow).
   *
   *  $ 3. Node also has a type that usually corresponds to some Java type if a node has only data result.
   *      For multi-results nodes its type is a tuple of types.
   *
   *  $ 4. For convenience nodes also maintain the list of its uses: nodes that have this node as argument.
   *      Thus def-use information is explicitly available.
   *
   *   The internal representation of particular method forms a ''sea of nodes'':
   *   it is a number of nodes representing operations
   *   that are linked together by data and/or control arguments and results.
   *   The (sea of) nodes generated for a particular method are internally in SSA form.
   *   Note, that there is no nodes that correspond to local variables.
   *   However the data result of a node can be imagined as SSA local variable.
   *   So when a node has one result and the result is data result the SSA variable is the node itself.
   *   </pre>
   */
  trait Node extends CodeGenInformation with ResourcesInfo with NodeRef with Position.Owner { self =>

    def proto: Prototype[_ <: Node]

    /** Simple name of the node. */
    final def simpleName: String = {
      // We heavily rely on nodes declaration layout here. :/
      // Most of nodes are declared as inner classes in some other class:
      // e.g. "...nodes.SimpleNodes$Add" and its simpleName is expected to be "Add".
      // Some of them are declared as double nested:
      // e.g. "...nodes.AJNodes$VarArgsList$Builder" and its simpleName is expected to be "VarArgsListBuilder".

      // Note that Class.getSimpleName won't help us because we want to save not only the most inner class name
      // (classOf[AJNodes.VarArgsList.Builder].getSimpleName == "Builder").

      // "...nodes.AJNodes$VarArgsList$Builder"
      val fullName = getClass.getName

      // "AJNodes$VarArgsList$Builder"
      val className = fullName.substring(fullName.lastIndexOf('.') + 1)

      // "VarArgsList$Builder"
      val classNameWithoutFirstOuter = className.substring(className.indexOf('$') + 1)

      // "VarArgsListBuilder"
      classNameWithoutFirstOuter.split('$').mkString // WARNING: replace("$", "") is slower in XScala TODO: investigate
    }

    /** Name of the node (may include some additional node parameters, e.g. constant value, invoke kind, ...). */
    def name: String = simpleName + (proto match {
      case proto: Product => proto.productIterator.mkString("[", ",", "]")
      case _ => ""
    }) + extraNodeNameSuffix(this)

    /** Type of the value the node produces (or type of the node itself). */
    def tpe = proto.tpe

    /** Returns `true` iff node produces floating-point value. */
    def isFP: Boolean = tpe.isFloatingPointType

    /** Tags of this node. */
    def tagsSeq = Tags.seqFromMask(tagsMask)

    def hasTag(tag: Tag) = tag containsInMask tagsMask

    def tagsMask = proto.tagsMask

    /** The number of arguments. */
    def arity = duLinks.length

    /** Node's id; it's unique in the universe for commited node. */
    var id: Int = RAW_ID

    /** Node host scope. */
    var scope: Scope = _

    private[this] var _isCommitted = false

    /** If the node is committed to the Universe. Only committed nodes has id */
    def isCommitted = _isCommitted

    private[ir] def commitImpl(): Unit = {
      assert(!isCommitted)
      makeUses()
      _isCommitted = true
    }

    private[ir] def decommitImpl(): Unit = { //TODO: check/remove DU chains
      assert(isCommitted)
      _isCommitted = false
      setReferent(null)
      nullifyArgs()
      nullifyUses()
      detachFromGroup()
      untie()
    }

    /** source position of the Node */
    var pos: Position = NoPosition

    /** bytecode position of the Node, if any */
    final def bytecodePos: Option[Int] = Position.offset(pos)

    /** inline context of the Node */
    final def inlineContext: InlineContext = Position.inlineContext(pos).orNull

    override def posApproximation = (pos
      orElse { if (!isCommitted) curPos() else NoPosition } /* handle not yet committed nodes during DataFlow/deserialization */
      orElse rootMethodPos /* handle nodes without pos */
      )

    /** Basic block containing the node or null if node is not connected to any basic block. */
    def block: Block

    /** Internal method for compile-time insurance that each node extends either [[FloatingNode]] or [[PinnedNode]]. */
    protected def compileTimeAssertThatNodeExtendsFloatingOrPinnedNode(): Unit

    /** Internal method for compile-time insurance that each node extends either [[ControlNode]] or [[NonControlNode]]. */
    protected def compileTimeAssertThatNodeExtendsControlOrNonControlNode(): Unit

    /** Internal method for compile-time insurance that each node extends either [[ProducesValue]] or [[NotProducesValue]]. */
    protected def compileTimeAssertThatNodeProducesValueOrNotProducesValue(): Unit

    /** Circular double linked list of pinned nodes. */
    private[ir] var prevPinned: Node = _
    private[ir] var nextPinned: Node = _

    private[ir] def tie(p: ControlNode): Unit = {
      assert(this.nextPinned == null && this.prevPinned == null)

      val next = p.nextPinned
      assert(next != null)
      assert(next.prevPinned == p)

      this.nextPinned = next
      this.prevPinned = p
      p.nextPinned = this
      next.prevPinned = this
    }

    private[ir] def untie(): Unit = {
      val next = this.nextPinned
      if (next != null) {
        val prev = this.prevPinned
        assert(prev != null)

        this.nextPinned = null
        this.prevPinned = null
        prev.nextPinned = next
        next.prevPinned = prev

      } else {
        assert(this.prevPinned == null)
      }
    }

    /** Marks a node that persists through implicit (on-commit) optimizations. */
    private var exact = false

    def markExact(): Unit = {
      exact = true
      if (isCommitted) {
        unValueNumber(this)
      }
    }

    def isExact: Boolean = exact

    /** Ordered sequence of arguments. */
    final def args: Iterator[Node] = argsSeq.iterator

    final val argsSeq = argsTail(0)

    final def hasUndefinedArgs: Boolean = args.contains(null)

    /** Return node arguments from `from`'th one to last one */
    // TODO: make it collection.IndexedSeq, because its contents can be modified by external users
    protected final def argsTail(from: Int): Seq[Node] = new IndexedSeq[Node] {
      def length = self.arity - from
      def apply(i: Int) = self.arg(i + from)
    }

    /** Selects an argument by its index. */
    private [ir] final def arg(idx: Int): Node = duLinks(idx).source

    /** Returns the argument if node has only one argument at all. */
    final def arg: Node = { assert(arity == 1); arg(0) }

    /** Replaces argument at given index with a new value.
     *  On any argument update it is also updated uses of the argument with this node.
     */
    final def updateArg(idx: Int, newArg: Node): Unit = { duLinks(idx).source = newArg }

    // Callbacks which are called when an argument/use is changed.
    protected def argChanged(idx: Int): Unit = {}
    protected def useAdded(use: Edge): Unit = {}
    protected def useRemoved(use: Edge): Unit = {}

    protected def duLinks: collection.IndexedSeq[DULink]

    // TODO: refactor me, looks ugly
    private def isArgApplicable(arg: Node, formalType: Type) = {
      // Any arg should be compatible by tag.
      // Non-control & non-memory args should also be compatible by type (in backend we reduce
      // this restriction (see removeNopCasts in Preparation)).
      val tag = formalType.tag
      arg.hasTag(tag) && (tag != Tag.VALUE || Type.lteq(arg.tpe, formalType) || !typeChecksEnabled)
    }

    private def checkArgType(idx: Int, newArg: Node): Node = {
      if (newArg != null) {
        val formalType = proto.argType(idx)
        val arg = withPos(newArg)(Node.convertArg(formalType, newArg))
        if (!isArgApplicable(arg, formalType)) {
          shouldNotReachHere(s"arg#$idx of $name is expected to have tag ${proto.argTag(idx)} & type $formalType" +
            s" but got arg ${arg.name} with tags ${arg.tagsSeq} & type ${arg.tpe}")
        }
        arg
      } else {
        null
      }
    }

    // def-use list entry
    protected final class DULink(var targetArgIndex: Int) extends Edge {
      private[this] var _source: Node = if (targetArgIndex < 0) self else null
      def source: Node = _source

      def target: Node = self
      def sourceLabel = argTag(targetArgIndex)

      def source_=(newSource: Node): Unit = {
        val oldSource = _source
        if (oldSource != newSource) {
          if (target.isCommitted) {
            beforeStructuralChange(target)
          }

          if (oldSource != null) {
            //assert(oldSource.isCommitted) TODO: in spill we create nodes and don't commit them yet - try to refactor not this but that place
            this.remove()
          }
          val checkedSource = target.checkArgType(targetArgIndex, newSource)

          _source = checkedSource

          if (checkedSource != null && target.isCommitted) {
            this.addUse()
          }
          afterStructuralChange(this)
          target.argChanged(targetArgIndex)
        }
      }

      // linked list part; TODO: extract to util trait

      private var _prev: Node#DULink = this
      private var _next: Node#DULink = this

      def prev = _prev
      def next = _next

      private def remove(): Unit = {
        _prev._next = _next
        _next._prev = _prev

        _prev = this
        _next = this

        source.useRemoved(this)
      }

      private def insertBefore(that: Node#DULink): Unit = {
        _prev = that._prev
        _next = that

        _prev._next = this
        _next._prev = this
      }

      def addUse(): Unit = {
        insertBefore(source.defUseHead)
        source.useAdded(this)
      }
    }

    /** Sentinel of this node's def-use list. */
    private[this] var _defUseHead: DULink = _
    private def defUseHead: DULink = {
      if (_defUseHead eq null) _defUseHead = new DULink(-1)
      _defUseHead
    }

    final def nullifyArgs(): Unit = { duLinks foreach { _.source = null } }

    final def replaceArgs(as: Node*): Unit = { replaceArgsBySeq(as) }

    def replaceArgsBySeq(as: Seq[Node]): Unit = {
      var i = 0
      for (arg <- as) {
        duLinks(i).source = arg
        i += 1
      }
      assert(i == arity)
    }

    private def makeUses(): Unit = {
      for (du <- duLinks) {
        val src = du.source
        if (src ne null) {
          du.addUse()
        }
      }
    }

    final def inEdges: Iterator[Edge] = duLinks.iterator

    /** Selects an input edge by its index. */ //TODO: use labels instead of indices
    final def inEdge(idx: Int): Edge = duLinks(idx)

    /** Iterator over all (registered) uses of this Node
      * WARNING: Do not modify source of an iterated edge! Use `replace***` methods for that.
      */
    final def outEdges: Iterator[Edge] = new Iterator[Edge] {
      var u: Node#DULink = defUseHead
      def peekNext = { assert(u.source eq self, s"expected ${u.source} to be eq to $self"); u.next }
      def hasNext = (u ne null) && peekNext.targetArgIndex >= 0
      def next() = if (!hasNext) Iterator.empty.next() else { u = peekNext; u }
    }

    final def outEdgesByTag(tag: Tag) = outEdges filter (_.sourceLabel == tag)
    final def valueOutEdges = outEdgesByTag(Tag.VALUE)

    final def inEdgesByTag(tag: Tag) = inEdges filter (_.sourceLabel == tag)

    def inEdgesByType(tpe: Type) = inEdgesByTag(tpe.tag) filter (_.source.tpe == tpe)

    final def uses: Iterator[Node] = outEdges map (_.target)

    final def usesByTag(tag: Tag) = outEdges collect { case e if e.sourceLabel == tag => e.target }

    final def argsByTag(tag: Tag) = inEdges collect { case e if e.sourceLabel == tag => e.source }

    final def usesWithParamNodes = this match {
      case b: Block => b.uses ++ b.paramNodes
      case n        => n.uses
    }

    final def valueUses = usesByTag(Tag.VALUE)
    final def valueArgs = argsByTag(Tag.VALUE)

    final def hasValueUses = valueUses.nonEmpty

    private[ir] def nullifyUses(): Unit = { replaceUsesBy(null: Node) }

    /** Replace all uses of `this` by `n` and decommit `this`. */
    final def replaceBy(n: Node): Unit = {
      assert(this != n)
      this.moveGroupInfoTo(n)

      replaceUsesBy(n)
      decommit(this)
      this.setReferent(n)
    }

    /** Replace each use of this node by the given node `n`. */
    final def replaceUsesBy(n: Node): Unit = {
      replaceUsesImpl { _ => n }
    }

    private def replaceUsesImpl(f: Edge => Node): Unit = {
      val it = outEdges
      var e = if (it.hasNext) it.next() else null
      while (e ne null) {
        // The order of actions below is crucial due to possible side-effects!
        // First, call user-defined function and obtain new source node for an edge
        val newSrc = f(e)
        // Then, fetch next use from def-use list of this node (`e` must still be an use of `this`)
        assert(e.source eq self)
        val n = if (it.hasNext) it.next() else null
        // And finally, change `e.source` thus moving `e` into def-use list of `newSrc`.
        e.source = newSrc
        e = n
      }
    }

    /** Replace any use that satisfies `pf` by the node returned by `pf`. */
    final def replaceUses(pf: PartialFunction[Edge, Node]): Unit = {
      val oldSrc = { (e: Edge) => e.source }
      replaceUsesImpl { e => pf.applyOrElse(e, oldSrc) }
    }

    final def replaceValueUsesBy(newValue: Node): Unit = {
      replaceUses { case e if e.isValue => newValue }
    }

    final def replaceMemoryUsesBy(newMemory: Node): Unit = {
      replaceUses { case e if e.isMemory => newMemory }
    }

    final def replaceValueUsesByNoValue(): Unit = {
      if (producesValue && valueUses.nonEmpty) {
        replaceValueUsesBy(NoValue())
      }
    }

    private def argTag(idx: Int) = proto.argTag(idx)

    /** Returns single use of this node or throws an exception */
    final def singleUse: Node = ScalaCollections.singleElement(uses)

    /** Returns single use of this node, tagged with given `tag`, or throws an exception */
    final def singleUseByTag(tag: Tag): Node = ScalaCollections.singleElement(usesByTag(tag))

    /** Returns single `VALUE` use of this node or throws an exception */
    final def singleValueUse: Node = singleUseByTag(Tag.VALUE)

    /** Returns single out edge of this node or throws an exception */
    final def singleOutEdge: Edge = ScalaCollections.singleElement(outEdges)

    override def toString = {
      val name = try this.name catch { case NonFatal(_) => this.simpleName }
      try {
        val argsStr = args.map { a => if (a == null) "null" else a.id } mkString ("(", ",", ")")
        s"$id:$name$argsStr"
      } catch {
        case NonFatal(_) => s"$id:$name"
      }
    }

    final def producesValue = hasTag(Tag.VALUE)

    /** Returns type, for which this node argument should be enriched. */
    private[ir] def argEnrichment(argEdge: Edge): Option[symlevel.Type] = None
  }

  object Node {

    /** Creates a copy of given node, partially applying given `argMapping`. */
    def clonePartially(n: Node)(argMapping: PartialFunction[Edge, Node]): Node = withPos(n) {
      val defaultMapping: PartialFunction[Edge, Node] = if (currentScope.isInState) {
        case e if e.isControl => currentCtrl
        case e if e.isMemory => currentMemory
      } else {
        PartialFunction.empty
      }
      withPos(n) {
        n.proto.withExplicitArgs(n.inEdges.toSeq map (argMapping orElse defaultMapping orElse (_.source)): _*)
      }
    }

    /** Creates a copy of given node. */
    def clone(n: Node, argMapping: Node => Node): Node =
      clonePartially(n) { e => argMapping(e.source) }

    /** Creates a copy of given node. */
    def clone(n: Node) =
      clonePartially(n)(PartialFunction.empty)

    /** Creates an exact copy of given node (i.e. a copy that persists through commit optimizations). */
    def cloneExact[N <: Node](n: N): N =
      withPos(n) { n.proto.exact(n.argsSeq: _*).asInstanceOf[N] }

    /** Replaces given node `n` by several exact copies: one for each use.
      *
      * Note: multiple uses in the same node will be replaced by multiple exact copies
      *       (rework if this behavior is undesirable).
      *
      * @return resulting copies including the original node as one of them.
      */
    def rematerializeCompletely[N <: FloatingNode](n: N): Iterator[N] =
      rematerializeImpl(n, e => e)

    /** Replaces given node `n` by several exact copies: one for each use satisfying given condition `cond`
      * and one for the rest of uses that don't satisfy it.
      *
      * Note: multiple uses in the same node will be replaced by multiple exact copies
      *       (rework if this behavior is undesirable).
      *
      * @return resulting copies including the original node as one of them.
      */
    def rematerializeConditionally[N <: FloatingNode](n: N, cond: Edge => Boolean): Iterator[N] =
      rematerializeImpl(n, e => Option.when(cond(e))(e))

    /** Replaces given node `n` by several exact copies: one for each group of uses factorized by given equivalence `eq`
      * (i.e. equivalent uses will have the same copy as their argument).
      *
      * @return resulting copies including the original node as one of them.
      */
    def rematerialize[N <: FloatingNode](n: N, eq: Equiv[Edge]): Iterator[N] = {
      val edges = DisjointSet.from(n.outEdges)(eq)
      rematerializeImpl(n, edges.find)
    }

    /** Replaces given node `n` by several exact copies: one for each group of uses corresponding to each value of `equivClass`
      * (i.e. uses with the same `equivClass` will have the same copy as their argument).
      *
      * @return resulting copies including the original node as one of them.
      */
    private def rematerializeImpl[N <: FloatingNode, C](n: N, equivClass: Edge => C): Iterator[N] = {
      // We can not rematerialize node, if it is in group with some other nodes
      assert(!n.hasGroup)
      // Cannot rematerialize leaf nodes, because there can be only one leaf node for every leaf prototype.
      assert(!n.isInstanceOf[LeafNode[_]])
      n.markExact()
      val rems = mutable.LinkedHashMap.empty[C, N]
      n.replaceUses { case e => rems.getOrElseUpdate(equivClass(e), if (rems.isEmpty) n else cloneExact(n)) }
      rems.valuesIterator
    }

    private var argConverterStack: mutable.Stack[(Type, Node) => Node] = mutable.Stack()

    private[Node] def convertArg(tpe: Type, n: Node) = {
      if (argConverterStack != null && argConverterStack.nonEmpty) {
        // Creating a node during arg conversion may cause this method to be called subsequently.
        // Here we prohibit such conversions in order to avoid infinite recursion.
        val converters = argConverterStack
        argConverterStack = null
        try {
          var v = n
          for (converter <- converters) {
            v = converter(tpe, v)
          }
          v
        } finally {
          argConverterStack = converters
        }
      } else {
        n
      }
    }

    /** Runs given `action` with implicit conversion on each node argument change. */
    def withImplicitArgConversion[T](converter: (Type, Node) => Node)(action: => T): T = {
      argConverterStack.push(converter)
      try {
        action
      } finally {
        argConverterStack.pop()
      }
    }
  }

  /** Prototype of node.
   *  <pre>
   *  Prototype is described by argument types and result types.
   *  So it can be imagined as a type projection (functional or meta-type) of a particular node.
   *  By means of a node prototype we can add type safety of generated nodes and provide uniform way of their
   *  declaration and creation.
   *
   *  Argument types of a prototype are split to fixed arguments and variable arguments (varargs)
   *  (imagine "..." varargs in C or Java or "*" in Scala for the later). All varargs must have the same type,
   *  so the only vararg type is specified for a prototype. Fixed argument types has a fixed (finite) length.
   *
   *  In other words, types of the arguments of node are:
   *
   *    fixedArgTypes(0),
   *    fixedArgTypes(1),
   *    ...,
   *    fixedArgTypes(fixedArgsCount-1),
   *    varArgType,
   *    varArgType,
   *    ...
   *
   *  Node prototype is also can be thought as factory of corresponding node: it has `apply` method that actually
   *  creates corresponding node (or returns already committed one).
   *
   *  Prototype class hierarchy is organized in the way where argument types and result types of a concrete Prototype
   *  is explicitly specified  while extending of a base prototype class as class arguments.
   *  So usually required information about node argument and result types of a node class
   *  can be seen just looking at the declaration of its corresponding Prototype.
   *
   *  Thus Prototype, ProtoGenerator and Node conceptions forms an internal DSL
   *  for describing a particular node class.
   *
   *  For instance, you need a node `MyNode` that may share the same prototype instance between all node instances.
   *  In this case, you declare the node class `MyNode` as follows:
   *  {{{
   *  object MyNode extends SomeProto[MyNode](ArgTypes)(ResultTypes) {
   *    def newInstance() = new MyNode
   *  }
   *
   *  class MyNode extends SomeNode(MyNode)
   *  }}}
   *  and MyNode creation will look like:
   *  {{{
   *  MyNode(arg1, arg2, ..., argN)
   *  }}}
   *  that will call `apply` method of prototype object MyNode that reside in the base Prototype class that may create
   *  new MyNode instance committing it to the universe and automatically updating def-use information,
   *  or may return already committed node if there is such node in the universe (value numbering or CSE occurs).
   *
   *  In the case when a single prototype cannot be reused between all node instances, another scheme is applied using
   *  ProtoGenerator concept:
   *  {{{
   *  object MyNode extends SomeProtoGenerator {
   *    class Proto(protoargs) extends SomeProto[MyNode](ArgTypes)(ResultTypes) {
   *      def newInstance() = new MyNode(this)
   *    }
   *    def apply(protoargs) = new Proto(protoargs)
   *  }
   *
   *  class MyNode(proto: MyNode.Proto) extends SomeNode(proto)
   *  }}}
   *  and MyNode creation will look like:
   *  {{{
   *  MyNode(protoargs)(arg1, arg2, ..., argN)
   *  }}}
   *  that first will call `apply` method of MyNode prototype generator that will create a new node prototype
   *  (or return existing one with the same properties), and
   *  second call of prototype's `apply` method will create MyNode instance (or return already committed node).
   *
   *  Protoargs are usually a type of node operation (at least when ProtoGeneratorWithType is used),
   *  for instance `Add(IntType)(a1, a2)` creates Add node that adds two int arguments and returns int.
   *
   *  Note also, that during creation of a node by means of `apply` method of node prototype, all argument types
   *  are checked according prototype arguments types, thus type safety of node creation is provided.
   *  For instance, a node creation such as `Add(IntType)(a1, a2 ,a3)` fails because Add.Proto prototype declares
   *  only two arguments and  `Add(IntType)(float1, float2)` also fails because `Add(IntType)` will create a node prototype
   *  with all arguments of int type not float.
   *  </pre>
   */
  abstract class Prototype[N <: Node]
    (protected val fixedArgTypes: Type*)
    (_tpe: Type) {

    private[Nodes] def tpe = _tpe

    def tagsMask = tpe.tag.asMask

    private [ir] def fixedArgsCount = fixedArgTypes.size

    def argType(idx: Int): Type

    final def argTag(idx: Int): Tag = argType(idx).tag

    /**
     * Creates node that corresponds to this prototype.
     * Node is created with given args and is appended to the Universe.
     * <p>
     * All args given to `apply` method are separated into two groups:
     * <pre>
     *  arg(0)     ----\
     *  ...             | instance args that are passed to constructor of node
     *  arg(ic-1)  ----/
     *  arg(ic)    --\
     *  ...           | args that are node args in graph
     *  arg(N)     --/
     * </pre>
     * where `ic` is `instanceArgsCount`.
     * </p>
     */
    def apply(args: Node*): Node = {
      commit(makeNode(args))
    }

    /** Creates node that corresponds to this prototype.
      * All prototype args must be explicitly passed to this method,
      * because default state args (e.g. currentCtrl and currentMemory) are not appended.
      */
    def withExplicitArgs(args: Node*): Node = {
      commit(makeNode(args, explicitArgs = true))
    }

    /** Creates node that corresponds to this prototype.
      * Node is created as exact (i.e. it persists through commit optimizations).
      */
    final def exact(args: Node*): N = {
      val n = makeNode(args)
      n.markExact()
      commit(n) ensuring (_ eq n)
      n
    }

    /** Creates node that corresponds to this prototype.
      * Node is created with given args and is __not__ appended to the Universe.
      */
    final def raw(args: Node*): N = {
      makeNode(args, allowUndefinedArgs = true)
    }

    private[Nodes] def makeNode(args: Seq[Node], explicitArgs: Boolean = false, allowUndefinedArgs: Boolean = false): N = {
      val n = newInstance()
      assert(n.tpe == tpe)

      def prepareNodeArgs(nArgs: Seq[Node]) =
        if (explicitArgs) nArgs else getDefaultArgsForNode(n, nArgs)

      val nodeArgs = prepareNodeArgs(args)
      assert(nodeArgs.size >= fixedArgsCount)

      assert(currentScope.inDeserialization || allowUndefinedArgs || !nodeArgs.contains(null), s"${n.name}($nodeArgs)")

      n.replaceArgsBySeq(nodeArgs)

      n.initSelfReference()

      n
    }

    /** Creates node that corresponds to this prototype. Leaves nodeArgs uninitialized. */
    protected def newInstance(): N

    /** Returns true, iff this prototype has control argument */
    def hasControlArg =
      (fixedArgTypes.length >= 1) && (argTag(0) == Tag.CONTROL)

    /** Returns true, iff this prototype has memory argument */
    def hasMemoryArg =
      (fixedArgTypes.length >= 2) && (argTag(1) == Tag.MEMORY)

  }

  object Prototype {
    implicit object PrototypeSetsAndMaps extends Sets.Default[Prototype[_]] with Maps.Default[Prototype[_]]

    // Note: this cache is never cleared or dropped, so if it becomes a problem
    //       implement some kind of cleanup mechanism, such as explicit drop or weak/softref keys and values.
    private val cache = Maps[Prototype[_]].newMMap[Prototype[_]]

    def intern[P <: Prototype[_]](proto: P): P =
      cache.getOrElseUpdate(proto, proto).asInstanceOf[P]
  }
}
