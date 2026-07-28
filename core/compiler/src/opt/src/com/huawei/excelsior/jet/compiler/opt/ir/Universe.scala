/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, NoPosition, Position}
import com.huawei.excelsior.jet.compiler.driver.CompilationMode
import com.huawei.excelsior.jet.compiler.driver.CompilationMode.O1
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.*
import com.huawei.excelsior.jet.compiler.options.StrOption.*
import com.huawei.excelsior.jet.compiler.ir.{BytecodeOffset, InlineContext, LineNumber}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.{CompilerPhase, IRSandbox}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase.PostInline
import com.huawei.excelsior.jet.compiler.opt.Opt
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.MarkedRegions
import com.huawei.excelsior.jet.compiler.opt.middle.{EvacuateAnalysis, GlobalCodeMotion, IdempotentOperationsOptimizer, VarProcessor}
import com.huawei.excelsior.jet.compiler.opt.middle.types.{ContextTypes, TypeAnalysis, VMStates}
import com.huawei.excelsior.jet.compiler.opt.platforms.{PlatformConfig, PlatformDependent}
import com.huawei.excelsior.jet.compiler.opt.util.Callback
import com.huawei.excelsior.jet.compiler.{CodeUnit, CompilerEnvironment, StatsKind, symlevel}
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.util.Sets.Defaults.default
import com.huawei.excelsior.jet.compiler.util.CachedValue
import com.huawei.excelsior.jet.util.graph.ordering.TopSort
import com.huawei.excelsior.jet.util.graph.{Dominators, Loops, ObjectBiGraph}
import com.huawei.excelsior.jet.util.{Closure, ScalaCollections, Worklist}
import xscala.io.TextOutput

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag

/**
 * The IR universe.
 *
 * @author paul
 */
trait Universe extends CompilerEnvironment
                  with Nodes
                  with VarProcessor
                  with UniverseImplicitSetsAndMaps
                  with Types
                  with Identities
                  with NodeGraphs
                  with LinearNodeOrder
                  with DebugPrinters
                  with ConsistencyChecking
                  with NodeReplaceOptimizer
                  with CFGAnalysis
                  with RTStructs
                  with EnrichmentSupport
                  with InlineContextRegions
                  with ValueNumbering
                  with TypeAnalysis
                  with ContextTypes
                  with IdempotentOperationsOptimizer
                  with VMStates
                  with AbstractInterpreterComponent
                  with GlobalCodeMotion
                  with Toolbox
                  with PlatformDependent
                  with EvacuateAnalysis { self =>

  def platformConfig: PlatformConfig

  ///////////////////////
  // Root method under compilation.

  def codeUnit: CodeUnit

  def rootMethod: symlevel.Method = codeUnit.method

  def rootDeclaringClass = rootMethod.getDeclaringClass

  def rootReceiverType = codeUnit.getReceiverType ensuring (_ != null)

  def hostingClass = codeUnit.getHostingClass

  var isDirtyForClassGC = false

  private var _isO1Compiled: Boolean = rootMethod.initialCompilationMode == O1

  def switchToO1(): Unit = {
    _isO1Compiled = true
    isUnstructuredLocking = true
  }

  def isO1Compiled: Boolean = _isO1Compiled

  // TODO-DWARF: this should be switchable for example to turn it off for some methods without debug info
  private val _genDebug: Boolean = env.enabled(GenDebug)

  def genDebug: Boolean = _genDebug

  private var _isUnstructuredLocking = false

  def isUnstructuredLocking = _isUnstructuredLocking

  def isUnstructuredLocking_=(value: Boolean): Unit = {
    if (!_isUnstructuredLocking && value) {
      all[SynchronizedRegion] foreach (_.replaceBy(SynchronizedRegion.noRegion()))
    }
    _isUnstructuredLocking = value
  }

  def isStructuredLocking = !isUnstructuredLocking

  def symbolLinker = env.getSymbolLinker(rootMethod)

  def parent: Universe

  @tailrec
  final def methodAlreadyInCompilation(method: symlevel.Method): Boolean =
    rootMethod == method || (parent != null && parent.methodAlreadyInCompilation(method))

  /** Returns true, iff given `method` has passed front stage. May provoke it's compilation. */
  def passFront(method: Method): Boolean =
    Opt.passFront(method, env, platformConfig, statsGlobal, this)

  object stats {
    def isEnabled(kind: StatsKind): Boolean =
      statsGlobal.isEnabled(kind)

    def count(kind: StatsKind, event: String, pos: Position = rootMethodPos): Unit =
      statsGlobal.count(kind, event, verboseSuffix = s" at ${pos.toString(ignoreNumbers = env.enabled(IgnoreNumbersInPositionsOutput))}")

    def count(kind: StatsKind, event: String, posOwner: Position.Owner): Unit = {
      count(kind, event, posOwner.posApproximation)
    }

    def value(kind: StatsKind, event: String, num: Double): Unit =
      statsGlobal.value(kind, event, num, s" at ${rootMethod.getFullName}")
  }

  /** Optimization Trials mechanics.
    *
    * Provides [[trials.opportunisticOptimizationAllowed]] & [[trials.opportunisticOptimization]] methods
    * to be used to postpone some highly likely (but not certainly) beneficial optimizations for later execution.
    *
    * [[trials.decideAndThen]] runs postponed optimizations & decides whether they were actually beneficial.
    */
  object trials {
    private lazy val allowed = env.enabled(OptimizationTrials) && profile.isPGOHost

    /** Checks whether current Universe should do highly likely beneficial optimizations,
      * and if it can not, registers the intent so that another Universe could do them instead.
      */
    def opportunisticOptimizationAllowed(): Boolean = {
      if (opportunisticUniverse) {
        assert(allowed)
        true
      } else {
        requested = allowed
        false
      }
    }

    /** Braces for highly likely beneficial optimizations. Performs given action iff [[opportunisticOptimizationAllowed()]]. */
    def opportunisticOptimization[T](action: => T): Option[T] = Option.when(opportunisticOptimizationAllowed())(action)

    // following are implementation details

    /** `true` if [[opportunisticOptimization]]s were ever requested for current method. */
    private var requested = false

    /** `true` if current Universe is the one where [[opportunisticOptimization]]s are performed. */
    private def opportunisticUniverse = parent != null && parent.trials.started
    private var started = false

    /** `true` if this Universe wins the trial. */
    private var passed = true

    /** Either creates another Universe in which [[opportunisticOptimization]]s shall be performed,
      * or, if we are already inside such, decides which one of them produced better IR
      * and deserves to execute given `action`.
      */
    def decideAndThen(action: => Unit): Unit = {
      if (trials.opportunisticUniverse) {
        self.trials.passed = self.trials.hotPathWeight < parent.trials.hotPathWeight
        parent.trials.passed = !self.trials.passed

      } else if (trials.requested) {
        trials.started = true
        new IRSandbox(codeUnit, env, statsGlobal) run { sandbox =>
          platformConfig.back(sandbox, parent = self).run()
        }
      }

      if (trials.passed) {
        action
      }
    }

    private def hotPathWeight = withGCM() {
      val cold = findWarmAndColdBlocks()
      ScalaCollections.sumBy(all[Node] filterNot (n => cold(n.block)))(nodeWeight)
    }
  }

  class ProfileInfo {
    private def pgoReady = env.enabled(PGO) && currentPhase >= PostInline

    // Naming is hard.
    // 1. Currently profile requires better optimization only for PGO hosts.
    // 2. In everyday speech and in compiler options everybody uses terminology PGO host.
    def isPGOHost: Boolean =
      pgoReady && (rootMethod.isJCAPGOHost || Profile.blame.requiresBetterOptimizationOf(rootMethod))

    def getHotness(n: Node): MarkedRegions.Hotness = {
      val hotness = if (env.enabled(UseMarkedRegions)) {
        MarkedRegions.hotness(n.pos, Option.when(isPGOHost)(rootMethod))
      } else {
        MarkedRegions.Hotness.Unknown
      }
      n match {
        case cs: Call =>
          val m = cs.targetRef.method
          if (hotness == MarkedRegions.Hotness.Hot || Profile.blame.isHot(cs.pos, Option.unless(m.isAJRTAllocator)(m))) {
            MarkedRegions.Hotness.Hot
          } else if (env.enabled(UseMarkedRegions)) {
            hotness
          } else if (!m.isManaged) {
            MarkedRegions.Hotness.Unknown
          } else {
            MarkedRegions.Hotness.Cold
          }

        case cs: AnyNew =>
          if (hotness == MarkedRegions.Hotness.Hot || Profile.blame.isHot(cs.pos, None)) {
            MarkedRegions.Hotness.Hot
          } else if (env.enabled(UseMarkedRegions)) {
            hotness
          } else {
            MarkedRegions.Hotness.Cold
          }

        case b @ (_: Block | _: BlockEnd) if env.enabled(WorkaroundForJET12487) =>
          // Workaround for absence of inline markers:
          // entry and exit blocks of inlined code often have the same position as the original call node had,
          // so we can check whether that call site were present in inline plan (and hence was hot).
          if (hotness == MarkedRegions.Hotness.Hot || (isPGOHost && plannedMethods(b.pos).nonEmpty)) {
            MarkedRegions.Hotness.Hot
          } else {
            hotness
          }

        case _ => hotness
      }
    }

    def inlinePlanContains(callSitePos: Position, target: symlevel.Method): Boolean =
      pgoReady && Profile.blame.inlinePlan.contains(callSitePos, target)

    /** Returns iterator over methods in descending order of hits,
      * which are planned for inlining at given position.
      */
    def plannedMethods(callSitePos: Position): Iterator[(symlevel.Method, Int)] =
      Profile.blame.inlinePlan.methods(callSitePos)

    /** Returns iterator over methods in descending order of hits,
      * which were called at given position during profiling.
      */
    def calledMethods(callSitePos: Position): Iterator[(symlevel.Method, Int)] =
      Profile.blame.calledMethods(callSitePos)

    /** Returns iterator over methods in descending order of hits,
      * which could be devirtualization targets at given position.
      */
    def devirtTargets(callSitePos: Position): Iterator[(symlevel.Method, Int)] = {
      if (!pgoReady) return Iterator.empty

      val devirt = plannedMethods(callSitePos)
      if (devirt.isEmpty) {
        calledMethods(callSitePos)
      } else {
        devirt
      }
    }

    def markInlined(inlineContext: InlineContext, target: Method, pos: Int): Unit = {
      Profile.blame.inlinePlan.markInlined(inlineContext, target, pos)
    }
  }

  private val realProfileInfo = new ProfileInfo

  def profile: ProfileInfo = realProfileInfo

  def extraNodeNameSuffix(n: Node) = ""

  ///////////////////////
  // Callbacks

  /** Called after commit. */
  val onCommit = new Callback[Node]

  /** Called before decommit. */
  val onDecommit = new Callback[Node]

  /** Called before node structure change. */
  val beforeStructuralChange = new Callback[Node]

  /** Called before node structure change. */
  val afterStructuralChange = new Callback[Edge]

  /** Called before node point needs to be recalculated. */
  val onPointRecalculation = new Callback[FloatingNode]

  ///////////////////////
  // Global universe states

  /** Raw uncommited nodes have id of 0.
   *  Commited nodes have positive ids.
   *  Decommited nodes have negative ids (opposite to their previous positive id).
   */
  protected val RAW_ID = 0

  private var lastId: Int = _

  private var _rootScope: Scope = _
  def rootScope: Scope = _rootScope

  def allScopes: Iterator[Scope] = Closure[Scope](rootScope)(_.inner).iterator

  def initUniverse(): Unit = {
    lastId = RAW_ID
    curScope = new Scope(null, null, Unwind)
    _rootScope = curScope
  }

  /**
   * Decommit all nodes and reset their counter.
   */
  def resetUniverse(): Unit = {
    requireNoGlobalCodeMotion()
    resetValueNumbering()

    assert(curScope.outer == null)
    curScope.drop()
    curScope = null

    initUniverse()
  }

  private var _currentPhase: CompilerPhase = _

  def startPhase(phase: CompilerPhase): Unit = {
    assert(_currentPhase == null || phase == null || _currentPhase < phase)
    _currentPhase = phase
  }

  def currentPhase = _currentPhase

  private var _typeChecksEnabled = true
  def typeChecksEnabled = _typeChecksEnabled
  def disableTypeChecks() = { _typeChecksEnabled = false }

  private var _identityEnabled = true
  def identityEnabled = _identityEnabled
  def disableIdentity() = { _identityEnabled = false }
  def enableIdentity() = { _identityEnabled = true }

  private var _phiIdentityEnabled = true
  def phiIdentityEnabled = _phiIdentityEnabled
  def disablePhiIdentity() = { _phiIdentityEnabled = false }
  def enablePhiIdentity() = { _phiIdentityEnabled = true }

  private var _areDifferentXHandlersInBlockAllowed = false
  def areDifferentXHandlersInBlockAllowed = _areDifferentXHandlersInBlockAllowed
  def allowDifferentXHandlersInBlock[A](action: => A) = {
    assert(!_areDifferentXHandlersInBlockAllowed)

    _areDifferentXHandlersInBlockAllowed = true
    try {
      action
    } finally {
      _areDifferentXHandlersInBlockAllowed = false
    }
  }

  private var _allowMemoryNodeInsertion = true

  def allowMemoryNodeInsertion = _allowMemoryNodeInsertion

  def prohibitMemoryNodeInsertion(): Unit = _allowMemoryNodeInsertion = false

  ///////////////////////
  // Node scopes

  /** Current scope. */
  private var curScope: Scope = _

  def currentScope: Scope = curScope

  def withinScope[A](scope: Scope)(action: => A) = {
    val prev = curScope
    try {
      curScope = scope
      action
    } finally {
      curScope = prev
    }
  }

  /** Creates new temporary scope, performs `action` and drops the scope.
    * Does not modify current scope.
    */
  def inTempScope[T](action: => T) = {
    val (scope, res) = createScope(null, NoPosition, Some(Unwind))(action)
    scope.drop()
    res
  }

  def createScope[T, K <: Position](anchor: SpinalNode, pos: K, xContext: Option[XContext] = Some(Unwind))(action: => T): (Scope, T) = {
    withinScope(new Scope(curScope, anchor,
      xContext match {
        case Some(xC) => xC
        case None =>
          if (anchor.hasXHandler)
            XHandler(anchor.xpoint)
          else
            Unwind
      })) {
      try {
        withPos(pos) {
          (curScope, action)
        }
      } catch {
        case e: Throwable =>
          curScope.drop()
          throw e
      }
    }
  }

  def createScopeWithState[T, K <: Position](anchor: SpinalNode, pos: K, xContext: Option[XContext] = Some(Unwind))(action: => T): (Scope, T) = {
    createScope(anchor, pos, xContext) {
      currentScope.inState(entryBlock, entryMemory) {
        action
      }
    }
  }

  def entryBlock: BBlock = curScope.entryBlock
  def entryMemory: MemoryNode = curScope.entryMemory

  def allNodesCount: Int = curScope.allNodesCount

  /** All nodes of the current scope including `entryBlock`.
    * Addition/removing nodes non-destructively affect this iterator.
   */
  def allNodes: Iterator[Node] = curScope.allNodes

  def all[T <: Node : ClassTag] : Iterator[T] = collect[T](allNodes)
  def collect[T <: Node : ClassTag](xs: Iterator[Node]): Iterator[T] = xs collect { case n: T => n }
  def collect[T <: Node : ClassTag](xs: Iterable[Node]): Iterator[T] = collect[T](xs.iterator)

  def takeWhileInstanceOf[T <: Node : ClassTag](xs: Iterator[Node]): Iterator[T] = {
    collect[T](xs takeWhile { case n: T => true; case _ => false })
  }

  def single[T <: Node : ClassTag] : T = single[T](allNodes)
  def single[T <: Node : ClassTag](xs: Iterator[Node]): T = ScalaCollections.singleElement(collect[T](xs))
  def single[T <: Node : ClassTag](xs: Iterable[Node]): T = ScalaCollections.singleElement(collect[T](xs))

  def cfg = curScope.cfg
  def spinalCFG = curScope.spinalCFG

  /** Used for debug. */
  def nodeByID(id: Int): Option[Node] = {
    def findInScope(s: Scope): Option[Node] = {
      if (s == null) None else {
        s.allNodes find { _.id == id } orElse findInScope(s.outer)
      }
    }
    findInScope(curScope)
  }


  object Scope {

    abstract class State(private [Universe] var ctrl: ControlNode,
                         var memory: NodeRef,
                         val contextTypes: ContextTypesMap = null)
      extends AbstractInterpreter.State {
      protected type This <: State

      private def hasContextTypes = contextTypes != null

      private [Universe] def makeFilter(n: Node) =
        if (hasContextTypes) contextTypes.makeFilter(n) else null

      private def updateContext(f: TypeFilter): Unit = {
        if (f != null) {
          contextTypes.appendFilter(f)
        }
      }

      private def updateControl(n: Node): Unit = n match {
        case block: Block =>
          ctrl = block

        case end: BlockEnd =>
          assert(ctrl == end.inCtrl, "we are trying to add the node which is already in control skeleton")
          ctrl = end

        case sp: SpinalNode =>
          assert(ctrl == sp.inCtrl, "we are trying to add the node which is already in control skeleton")
          ctrl = sp

        case _ =>
      }

      private def updateMemory(n: Node): Unit = n match {
        case end: BlockEnd =>
          assert(end.inMemory == memory)

        case _: Block =>
          memory = n

        case sp: SpinalMemoryNode =>
          assert(memory == sp.inMemory)
          memory = sp

        case _ =>
      }

      private [Universe] def addX(n: Node, f: TypeFilter): Unit = {
        updateContext(f)
        updateControl(n)
        updateMemory(n)
      }

      def add(n: Node): Unit = {
        addX(n, makeFilter(n))
      }

      def rewind(n: Node): Unit = {
        n match {
          case n: SpinalNode if ctrl == n => ctrl = n.inCtrl
          case _ =>
        }

        n match {
          case n: SpinalMemoryNode if memory == n => memory = n.inMemory
          case _ =>
        }

        if (hasContextTypes) {
          contextTypes.remove(n)
        }
      }

      private [Universe] def updateControlArg(node: ContextDependentNode): Unit = {
        if (hasContextTypes) {
          contextTypes.optimizeContextDependentNode(node)
        }
      }


      //// AbstractInterpreter.State implementation

      protected def forkImpl(): This = shouldNotCallThis()

      def makeUnreachableCopy(): This = shouldNotCallThis()

      protected def copyOnWriteImpl(): Unit = shouldNotCallThis()

      def mergeFrom(block: Block, states: Seq[This], identity: Boolean)(mergeFunc: (Type, Seq[Node]) => Node): This = {
        ctrl = block
        memory = if (identity) blockMemoryIdentity(block) else block

        if (hasContextTypes) {
          contextTypes.merge(states map {_.contextTypes}, block, identity)
        }
        this.asInstanceOf[This]
      }

      def foreachPair(that: This)(action: (Node, Node) => Unit): Unit = {}

    }

    def createAnchor(ctrl: UpperPoint): ScopeAnchor = {
      val ctrlAfter = ctrl.outCtrlOrNull
      Block.withoutSpineChangedControlNumInvalidation(ctrl.block) {
        val sa = withPos(ctrl.pos) {
          ScopeAnchor(ctrl)
        }
        if (ctrlAfter != null) {
          ctrlAfter.inCtrl = sa
          Block.tryRefreshBlockControlNums(ctrl, ctrlAfter)
        }
        sa
      }
    }
  }

  def currentCtrl: ControlNode = {
    val state = currentScope.state
    assert(state != null)
    state.ctrl
  }

  def currentMemory: MemoryNode = {
    val state = currentScope.state
    assert(state != null)
    state.memory.deref.asInstanceOf[MemoryNode]
  }

  /** Updates current state with given `ctrl` and it's outgoing memory.
    * TODO: remove this lowering-purpose crutch. */
  def setCurrentControl(ctrl: ControlNode): Unit = {
    val state = currentScope.state
    assert(state != null)
    state.ctrl = ctrl
    state.memory = ctrl match {
      case block: Block if block.blockEnd == null =>
        // Method `memoryAfter` can't properly handle situation when block doesn't have a blockEnd,
        // which is allowed during lowering.
        // In such situation we can conservatively set memory to the block,
        // which will be optimized later during normal optimization cycle.
        block
      case _ => ctrl.memoryAfter
    }
  }

  /** Fails with assert, iff given `x` is current control node. */
  def ensureNotCurrentCtrl(x: ControlNode): Unit = {
    val state = currentScope.state
    assert(state == null || state.ctrl != x)
  }

  /** Appends to given `args` required control and memory arguments from current state. */
  protected def getDefaultArgsForNode(node: Node, args: Seq[Node]): Seq[Node] = {
    val state = currentScope.state
    if (state != null) defaultArgsFor(state.ctrl, state.memory.deref)(node, args) else args
  }

  /** This method used to fill node arguments with some correct control/memory args. They are conservative,
    * but we can commit node with them and after this optimize control with context types information.
    * We do not want to analyze context types information before node commit, because we want use normal
    * access methods to node arguments (e.g. GetField.obj), but they will not work before node commit.
    */
  protected final def defaultArgsFor(ctrl: Node, memory: Node)(node: Node, rawArgs: Seq[Node]): Seq[Node] = {
    var args = rawArgs

    val hasMemIn = node.isInstanceOf[HasInMemory]
    if (hasMemIn) args = memory +: args

    val hasCtrlIn = node match { //TODO
      case _: Phi => false
      case _: HasInControl => true
      case _ => false
    }
    if (hasCtrlIn) args = ctrl +: args

    args
  }

  sealed abstract class XContext(xpoint: XPoint) {
    def xpointOption = Option(xpoint)
  }

  case class XHandler(xpoint: XPoint) extends XContext(xpoint)
  case object Unwind extends XContext(null)

  class Scope(val outer: Scope, val anchor: SpinalNode, val xContext: XContext) { self =>
    def inMemory: MemoryNode = {
      if (anchor != null)
        anchor.memoryBefore
      else
        null
    }

    if (anchor != null) {
      require(anchor.scope == outer)
    }

    val level: Int = if (outer == null) 0 else outer.level + 1

    private [Universe] var state: Scope.State = _ //TODO

    def isInState = state != null

    def inState[T](inCtrl: ControlNode, inMem: MemoryNode)(action: => T): T = {
      class _State extends Scope.State(inCtrl, inMem) {
        protected type This = _State
      }
      inState(new _State)(action)
    }

    def inState[T](state: Scope.State)(action: => T): T = {
      val oldState = swapState(state)
      try {
        action
      } finally {
        swapState(oldState)
      }
    }

    // FIXME: ?!
    def swapState(state: Scope.State) = {
      val old = this.state
      this.state = state
      old
    }

    def hasState = state != null

    private val nodes = Worklist.empty[Node]

    def nodesSize(): Int = nodes.size

    val cfg = new CFG(this)
    val splitUCFG = new SplitUCFG(this)
    val spinalCFG = new SpinalCFG(this)

    val entryBlock = withinScope(this) { BBlock() }
    val entryMemory = withinScope(this) { if (inMemory != null) inMemory else EntryMemory() }

    private var _unreachableBar: BBlock = _

    def hasUnreachableBar: Boolean = _unreachableBar != null

    def unreachableBar: BBlock = {
      if (!hasUnreachableBar) {
        _unreachableBar = withinScope(this) {
          inState(null) {
            val block = BBlock()
            UnreachableBlockEnd.withExplicitArgs(block, block)
            block
          }
        }
      }
      _unreachableBar
    }

    private val _inner = ArrayBuffer.empty[Scope]
    def inner = _inner.iterator

    if (outer != null) {
      outer._inner += this
    }

    /** True, iff basic graph (cfg & splitUCFG) tools (topSort & dominators) are available. */
    private[Universe] var graphToolsAvailable = true

    /** Invalidate graph caches. */
    def invalidateGraphCaches(): Unit = {
      cfg.invalidateCaches()
      splitUCFG.invalidateCaches()
    }

    /** Implement `action` without access to basic graph tools. */
    def withoutGraphTools[T](action: => T): T = {
      assert(graphToolsAvailable)
      graphToolsAvailable = false
      invalidateGraphCaches()
      try {
        action
      } finally {
        graphToolsAvailable = true
      }
    }

    def rewindFromState(n: SpinalNode): Unit = {
      if (state != null) {
        state.rewind(n)
      }
    }

    private var _inDeserialization = false
    def inDeserialization: Boolean = _inDeserialization

    /** TODO: rewrite deserialization. */
    def makeDeserialization(action: => Unit): Unit = {
      cfg.dominators // force evaluation
      splitUCFG.dominators // force evaluation
      _inDeserialization = true
      action
      _inDeserialization = false
      invalidateGraphCaches()
    }

    def refreshDominators(blockEnd: BlockEnd): Unit = {
      splitUCFG.refreshDominators(blockEnd, null)
    }

    def refreshDominators(exit: Branch.Exit): Unit = {
      splitUCFG.refreshDominators(exit, exit.owner)
    }

    def refreshDominators(block: Block): Unit = {
      cfg.refreshDominators(block, null)
      splitUCFG.refreshDominators(block, null)
    }

    def refreshDominatorsForced(block: Block, idomBlock: Block): Unit = {
      assert(inDeserialization)
      cfg.refreshDominators(block, idomBlock)
      splitUCFG.refreshDominators(block, idomBlock)
    }

    private var _exitPoint: Return = _

    def exitPoint = _exitPoint
    private[Universe] def exitPoint_=(ep: Return): Unit = {
      _exitPoint = ep
    }

    def allNodesCount: Int = nodes.size

    /** All nodes of this scope including `entryBlock`.
      * Addition/removing nodes non-destructively affect this iterator.
      */
    def allNodes: Iterator[Node] = nodes.track

    def all[T <: Node : ClassTag]: Iterator[T] = collect[T](allNodes)

    def contains(n: Node) = n.scope == this

    private[Universe] def add(n: Node): Unit = {
      assert(n.scope == null)
      nodes += n
      n.scope = this
    }

    private[Universe] def remove(n: Node): Unit = {
      assert(n.scope == this)
      nodes -= n
      n.scope = null
    }

    /** Create a `Return` with given value and set it in [[Scope.exitPoint]]. */
    def setResult(value: Node): Unit = {
      assert(currentScope != null)
      assert(exitPoint == null)

      val ret = Return(currentCtrl, currentMemory, value)
      exitPoint = ret
    }

    /** Set given `Return` in [[Scope.exitPoint]]. */
    def setResult(ret: Return): Unit = {
      assert(currentScope != null)
      assert(exitPoint == null)

      exitPoint = ret
    }

    /** Add node to `outer` scope and delete it from `nodes` list. */
    private def pushToOuter(n: Node): Unit = {
      n.scope = null
      self.nodes -= n
      outer.add(n)
      if (outer.state != null) {
        outer.state.add(n)
      }
    }

    /** Destroy this scope by merging all nodes into the outer scope and strike out [[anchor]]. */
    def merge(): Unit = {
      assert(inner.isEmpty)

      try {
        val isLinearCase: Boolean = exitPoint != null && all[Block].length == 1

        withinScope(self) {
          assert(Option(exitPoint) == Return.unique)
        }

        var memBefore = anchor.memoryAfter
        // We need to check anchor's block blockEnd because in some unit-tests we use `insertCode` while IR is incomplete.
        // This state of IR prohibits the use of GCM, which is necessary to collect memory uses.
        // In such a situation, memory adjustment is not needed.
        val mem: ArrayBuffer[Node] = if (!anchor.isInstanceOf[MemoryNode] && anchor.block.blockEnd != null && producesNewMemory) {
          withinScope(outer) {
            withIncrementalGCM {
              val memoryRaw = memBefore.outEdgesByTag(Tag.MEMORY).collect {
                case e @ Edge(_, target) if target.scope != self && (anchor strictDominates e.usePoint) => target
              }.to(ArrayBuffer)

              if (isLinearCase && inMemory != outer.entryBlock &&
                memBefore.outEdgesByTag(Tag.MEMORY).exists(_.target.block != anchor.block)) {
                // To adjust the memory in linear case, we need to make it conservative.
                // After this transformation, it will be enough to change the memory only in the block where
                // the anchor located.
                eliminateCrossBlockMemoryUses(memBefore)
                // In some cases (when pred memory node located in different block),
                // the ^transformation may change pred memory. So we need to update it.
                memBefore = inMemory
                memoryRaw filterInPlace (_.block == anchor.block)
              }
              memoryRaw
            }
          }
        } else ArrayBuffer()
        assert(mem.isEmpty || allowMemoryNodeInsertion)

        val unhandledXP: List[XPoint] = if (xContext.isInstanceOf[XHandler]) unhandledXPoints().toList else null
        val newMemory = if (exitPoint != null) exitPoint.inMemory else null

        val tail = if (!isLinearCase && outer.entryBlock.blockEnd != null) Some(Block.splitAfter(anchor)) else None

        if (isLinearCase) {
          specializedLinearMerge()
        } else {
          universalMerge()
        }

        // xControl
        outer.xedgesCount += xedgesCount

        xContext match {
          case XHandler(xpoint) => Block.addEdgesWithTemplate(unhandledXP, xpoint.xEdge)
          case _ =>
        }

        withinScope(outer) {
          if (exitPoint != null) {
            // If merged code has return we do the following:
            // * replace uses of `anchor` with corresponding `exitPoint` args;
            // * connect `tail` to the `exitPoint.block`.
            Node.withImplicitArgConversion(enrichArg(allowTypeMismatch = true)) {
              anchor.replaceUses { e =>
                e.sourceLabel match {
                  case Tag.CONTROL if !isLinearCase => exitPoint.inCtrl
                  case Tag.MEMORY if mem.isEmpty => exitPoint.inMemory
                  case Tag.VALUE => exitPoint.inValue
                }
              }
            }
            strikeOut(anchor)

            memBefore.replaceUses { case MemoryEdge(_, use) if mem.contains(use) => newMemory }

            for (goto <- tail) {
              exitPoint.block.blockEnd = goto
              decommit(exitPoint)
            }
          } else {
            // If merged code never returns we do the following:
            // * replace uses of `replaceableNode` by `NoValue`
            // * remove outgoing edges from `tail` and decommit it too.
            replaceValueUsesByNoValueAndStrikeOut(anchor)
            for (goto <- tail) {
              goto.makeUsesUnreachable()
              decommit(goto)
            }
          }
        }
      } finally {
        self.drop()
      }
    }

    private def producesNewMemory: Boolean = all[BlockEnd] exists (_.inMemory != inMemory)

    /** If all nodes are in one block.
      * This version of merge doesn't need to create new blocks.
      */
    private def specializedLinearMerge(): Unit = {
      val incompleteOuterBlock = anchor.outCtrlOrNull == null
      val inMem = inMemory

      entryBlock.blockRef.invalidate()
      entryBlock.replaceUses { case ControlEdge(_, target) if target != exitPoint =>
        anchor.inCtrl
      }

      nodes.track foreach {
        case sp: SpinalNode if sp.outCtrl == exitPoint =>
          pushToOuter(sp)

          anchor.inCtrl = sp

        case n if n != exitPoint && n != entryBlock =>
          pushToOuter(n)

        case _ =>
      }

      if (incompleteOuterBlock) {
        pushToOuter(exitPoint)
        exitPoint.inCtrl = anchor
        anchor.block.blockEnd = exitPoint

        entryBlock.replaceUses {
          _.sourceLabel match {
            case Tag.MEMORY => inMem
            case Tag.CONTROL => anchor
          }
        }
      } else {
        entryBlock.replaceMemoryUsesBy(inMem)
      }
    }

    private def universalMerge(): Unit = {
      outer.withoutGraphTools {
        withPos(anchor) {

          withinScope(outer) {
            val inMem = inMemory

            require(anchor.inCtrl != null)
            if (anchor.inCtrl.outCtrlOrNull != null) {
              val b = anchor.inCtrl.block
              b.blockEnd = null
              b.refreshBlockRef()
            }

            val goto = Goto(anchor.inCtrl, inMem)
            entryBlock.addArg(goto)

            if (hasUnreachableBar) {
              makeUnreachable(unreachableBar.xSuccBlockEdges)
              withinScope(this) {
                decommit(unreachableBar.blockEnd)
                decommit(unreachableBar)
              }
            }

            nodes.track foreach pushToOuter
          }
          outer.invalidateGraphCaches()
          outer._inner -= this
        }
      }
    }

    /** Destroy this scope by decommiting all nodes. */
    def drop(): Unit = {
      assert(inner.isEmpty)

      withinScope(this) {
        invalidateGraphCaches()
        allNodes foreach decommit
      }
      if (outer != null)
        outer._inner -= this
    }

    def unhandledXPoints() = collect[XPoint](allNodes) filterNot {_.hasHandler}

    private[ir] var xedgesCount = 0

    /** Returns true iff there are exceptional edges in IR. */
    def hasXEdges: Boolean = xedgesCount > 0
  }

  case class RTPartsInfo(isDirtyForClassGC: Boolean)

  trait BasicScopeGraph[N >: Null <: ControlNode] extends ObjectBiGraph[N] {
    protected def scope: Scope

    private val topSortCache = new CachedValue[TopSort[N]](() => super.topSort)
    private val loopsCache = new CachedValue[Loops[N]](() => super.loops)
    private val hasBackwardEdgesCache = new CachedValue[Option[Boolean]](() => Some(super.hasBackwardEdges))
    private val dominatorsCache = new CachedValue[Dominators[N]](() => { assert(!scope.inDeserialization); super.dominators })

    override def topSort: TopSort[N]        = topSortCache.get()
    override def loops: Loops[N]            = loopsCache.get()
    override def hasBackwardEdges: Boolean  = hasBackwardEdgesCache.get().get
    override def dominators: Dominators[N]  = dominatorsCache.get()

    private[Universe] def refreshDominators(node: N, idom: N): Unit = {
      if (dominatorsCache.evaluated() && scope.inDeserialization) {
        dominators.tryUpdateOne(node, idom, strict = false) ensuring { _ == true }
        invalidateCachesExceptDominators()
      } else {
        invalidateCaches()
      }
    }

    private def invalidateCachesExceptDominators(): Unit = {
      topSortCache.invalidate()
      loopsCache.invalidate()
      hasBackwardEdgesCache.invalidate()
    }

    private[Universe] def invalidateCaches(): Unit = {
      invalidateCachesExceptDominators()
      dominatorsCache.invalidate()
    }
  }

  /** Control Flow Graph with factored exception edges and without unreachable edges. */
  final class CFG(val scope: Scope) extends BasicScopeGraph[Block] {
    def start: Block = scope.entryBlock

    def succs(n: Block): Iterator[Block] = n.xSuccBlocks

    def preds(n: Block): Iterator[Block] = n.args collect {
      // null args may appear during deserialization
      case x if x != null && x.block.reachable => x.block
    }
  }

  /** Control Flow Graph with blocks split into two parts and factored exception edges.
    *
    * Every block is split into two graph nodes: block and blockEnd.
    * Branch.Exits explicitly represented in the graph.
    * BlockEnd has only one incoming edge from corresponding block.
    * Block may have factored exception edge.
    * BlockEnd has outgoing edges to successor blocks or branch exits.
    *
    * Note: may contain unreachable code.
    */
  final class SplitUCFG(val scope: Scope) extends BasicScopeGraph[ControlNode] {
    private def iter(x: ControlNode) = if (x != null) Iterator.single(x) else Iterator.empty

    def start: ControlNode = scope.entryBlock

    def succs(n: ControlNode): Iterator[ControlNode] = n match {
      case bb: BBlock     => iter(bb.blockEnd) ++ bb.xHandlers
      case xb: XBlock     => iter(xb.blockEnd)
      case branch: Branch => branch.exits.iterator filter (_.isCommitted)
      case end: BlockEnd  => assert (end.exits.size <= 1); end.succBlocks
      case x: Branch.Exit => collect[BBlock](x.uses)
    }

    def preds(n: ControlNode): Iterator[ControlNode] = n match {
      case bb: BBlock     => collect[ControlNode](bb.args)
      case xb: XBlock     => xb.tryBlocks
      case end: BlockEnd  => iter(end.block)
      case x: Branch.Exit => iter(x.owner)
    }
  }

  /** Skeleton graph (CFG with spinal nodes) with skipped control projections:
    *   - Each edge `(node, node.xpoint)` is replaced by edge `(node, node.xHandler)`
    *   - Each edge `(blockEnd, blockEnd.exit)` is replaced by edge `(blockEnd, blockEnd.exit.target)`
    */
  final class SpinalCFG(val scope: Scope) extends BasicScopeGraph[ControlNode] {
    override def start = scope.entryBlock

    override def succs(n: ControlNode) = n match {
      case n: Block       => Iterator(n.outCtrl)
      case n: SpinalNode  => Iterator(n.outCtrl) ++ n.xHandlerOption
      case n: BlockEnd    => n.succBlocks
    }

    override def preds(n: ControlNode) = n match {
      case n: XBlock      => n.inputs.iterator map { _.owner }
      case n: BBlock      => n.predBlocks map { _.blockEnd }
      case n: LowerPoint  => Iterator(n.inCtrl)
    }
  }

  ///////////////////////
  // Node commit/decommit

  private[this] var _currentInlineContext: InlineContext = _

  def currentInlineContext: InlineContext = _currentInlineContext

  /** Runs given action with given inline context set. */
  def withInlineContext[T](inlineContext: InlineContext)(action: => T): T = {
    assert (inlineContext.isTopLevel)
    val prevInlineContext = _currentInlineContext
    _currentInlineContext = inlineContext
    try {
      action
    } finally {
      _currentInlineContext = prevInlineContext
    }
  }

  private[this] var createNodePosition: () => Position = (() => NoPosition)

  /** Runs given action with given node position factory set. */
  def withPosFactory[T](factory: () => Position)(action: => T): T = {
    val prevFactory = createNodePosition
    createNodePosition = factory
    try {
      action
    } finally {
      createNodePosition = prevFactory
    }
  }

  /** Runs given action with given node position factory set. */
  def withPos[T](posOwner: Position.Owner)(action: => T): T = withPos(posOwner.pos)(action)

  /** Runs given action with given node position factory set. */
  def withPos[T](pos: Position)(action: => T): T = withPosFactory(() => pos)(action)

  /** Creates position specified by [[withPos]]/[[withPosFactory]]. */
  def curPos(): Position = createNodePosition()

  lazy val rootMethodPos =
    BytecodePosition(InlineContext.newRoot(rootMethod))

  /** Whether all on commit optimizations are delayed until this mode is disabled. */
  private var deferredOnCommitOptimizations = false

  private val deferredNodes = Worklist.empty[NonControlNode]

  def withDeferredOnCommitOptimizations[T](action: => T): T = {
    if (deferredOnCommitOptimizations) {
      // Nested case.
      action

    } else {
      assert(deferredNodes.isEmpty)
      deferredOnCommitOptimizations = true
      try {
        action
      } finally {
        deferredOnCommitOptimizations = false
        if (deferredNodes.nonEmpty) bulkReplace {
          deferredNodes.drain foreach { x => if (x.isCommitted) replaceTransitively(x, commit(x)) }
        }
      }
    }
  }

  private var idempotentDominanceEnabled = false
  private def disableIdempotentDominance(): Unit = { idempotentDominanceEnabled = false }
  private def enableIdempotentDominance(): Unit = { idempotentDominanceEnabled = true }
  def withIdempotentDominance(action: => Unit): Unit = { enableIdempotentDominance(); action; disableIdempotentDominance() }

  def commit(n0: Node): Node = {
    // Set position to node (may be used in pre-commit optimizations)
    if (!n0.isCommitted && n0.pos == NoPosition) {
      n0.pos = curPos()
    }

    assert(currentScope.inDeserialization || !n0.hasUndefinedArgs, n0)

    // Do pre-commit optimizations
    val st = curScope.state
    // Pull up controlled value nodes: set inCtrl based on context types info
    if (st != null) n0 match {
      case n0: ContextDependentNode => st.updateControlArg(n0)
      case _ => //skip
    }

    def optimized(n: Node): Node = n match {
      case _: ControlNode => n
      case _: LeafNode[_] => shouldNotReachHere(s"unexpected leaf node: $n")
      case n: NonControlNode if deferredOnCommitOptimizations => deferredNodes += n; n
      case _ if n.hasUndefinedArgs => n
      case _ =>
        val id = n match {
          case phi: Phi => if (phiIdentityEnabled) identity(phi) else phi // Phi-functions created during optimizing backend AI and should be optimized for IR simplification.
          case _ => if (identityEnabled) identity(n) else n
        }
        valueNumber(id)
    }

    // Perform data flow optimizations: algebraic identities & value numbering
    // Note: non-exact node can still be optimized (e.g. value-numbered) into exact one!
    val n = if (n0.isExact) n0 else optimized(n0)

    if (!n.isCommitted) {
      if (n != n0) {
        // `n` may be not equal to `n0` and still be not committed (raw node waiting for commit).
        if (n.pos == NoPosition) {
          n.pos = curPos()
        }

        // Make sure that after optimizations all args are still defined (see JET-13780).
        assert(currentScope.inDeserialization || !n.hasUndefinedArgs, s"$n0 was optimized to $n")
      }

      if (st != null) n match {
        case _: Projection =>
        // - during commit of projection node its owner is still under construction
        // - such nodes are block exit points and should not appear in block state
        case _ =>
          // Do not commit redundant checks based on context types info
          val f = st.makeFilter(n)
          if (f != null && f.isRedundant) {
            assert(!n.isExact) // TODO: support exact filter nodes if needed
            ContextTypesStats.updateOnRedundantFilterRemove(f)
            return null
          }
          // TODO: move idemDom optimization here
          st.addX(n, f)
      }

      // Register in the current scope
      curScope.add(n)
      // Register in the universe and set id
      lastId += 1
      n.id = lastId
      // Perform node-specific actions
      n.commitImpl()

      // Optimize idempotent nodes based on dominance
      if (idempotentDominanceEnabled) {
        n match {
          case n: Idempotent if IdempotentOperationsOptimizer.shouldOptimize(n) =>
            for (idemDom <- IdempotentOperationsOptimizer.findIdempotentDominator(n)) {
              assert(!n.isExact) // TODO: support exact idempotent nodes if needed
              assert(n != idemDom)
              st.rewind(n)
              IdempotentOperationsOptimizer.log(n)
              decommit(n)
              return idemDom
            }
          case _ =>
        }
      }

      // Notify callbacks
      onCommit(n)
    }
    n
  }

  def decommit(n: Node): Unit = {
    if (n.isCommitted) {
      // 1. notify callbacks
      onDecommit(n)
      // 2. perform node-specific actions
      n.decommitImpl()
      // 3. remove from the node's scope
      n.scope.remove(n)
      // 4. set id
      n.id = -n.id
    }
  }

  ///////////////////////
  // Constructor

  initUniverse()
  registerVNInUniverseCallbacks()
  registerTypeCacheCallbacks()


  ///////////////////////
  // Common stuff
  // TODO: move it somewhere

  var dbgPrinter: DebugPrinter = new SilentDebugPrinter

  def createDbgPrinter(kind: LogsKind): Unit = {
    val silent =
        (!env.enabled(DebugIrLogs)) ||
        (env.defined(LogOnlyClass) && !(env.listOf(LogOnlyClass) contains hostingClass.getName)) ||
        (env.defined(LogOnlyProc) && !(env.listOf(LogOnlyProc) contains rootMethod.getName))

    val zip = env.enabled(ZipIrLogs)

    closeDbgPrinter()

    dbgPrinter =
      if (silent) new SilentDebugPrinter
      else if (zip) new ZipIRLogsDebugPrinter(kind)
      else new IRLogsDebugPrinter(kind)
  }

  def closeDbgPrinter(): Unit = dbgPrinter.close()

  def dbgPrintForcedDebugNodes() = dbgPrintForced(_ debugNodes _)

  def dbgPrintForcedDebugGraphs() = dbgPrintForced(_ debugGraphs _)

  /** Perform some debug printing regardless current compile options
    * and producing result as a String.
    *
    * May be used during debugging via evaluating expression,
    * e.g. `dbgPrintForced(_.debugGraphs(_))`.
    */
  def dbgPrintForced(action: (DebugPrinter, String) => Unit): collection.Seq[String] = {
    val buf = new StringBuilder()
    val results = ListBuffer.empty[String]
    action(new TextDebugPrinter {
      override protected def openOut(message: String, extension: String) = TextOutput(buf)
      override protected def closeOut(out: TextOutput): Unit = {
        results += buf.toString
        buf.setLength(0)
      }
    }, "Forced debug printing")
    results
  }

  def sortByNodeID[N <: Node](xs: IterableOnce[N]): Seq[N] = {
    xs.iterator.toSeq.sorted(Ordering.fromLessThan[N](_.id < _.id))
  }

  case class MethodEntryPoint(inCtrl: Node, inMemory: Node, args: Seq[Node])

  type MethodLoader = PartialFunction[(Method, MethodEntryPoint), Return]

  def rootMethodParam(paramIdx: Int): Param = {
    // `this` is always plain, even for interface instance methods
    val generateEop = !rootMethod.getMethodType.isReceiverParameter(paramIdx)
    val symType = rootMethod.getParamType(paramIdx)
    val paramTpe = ValueType.fromSig(symType, eopTypeForInterfaces = generateEop, instantiateRich = true)
    withinScope(rootScope)(Param(paramTpe, paramIdx))
  }

  /** Return sequence of rootMethod parameter nodes. */
  def rootMethodParams: Seq[Param] = (0 until rootMethod.getParamsCount) map rootMethodParam

  /** Returns true, iff `node` may be target of GC. */
  def mayBeTraceableReference(node: Node): Boolean = node.tpe.isTraceableRefType

  def dirtyFramesLogAndStatUpdate(reason: String): Unit = {
    if (env.enabled(LogDirtyFrameReasons)) {
      env.print(s"Frame for root <$codeUnit> is considered to be dirty, reason: $reason\n")
      dirtyFramesStatUpdate(reason)
    }
  }

  def dirtyFramesStatUpdate(reason: String): Unit = {
    stats.count(StatsKind.DirtyFrames, reason)
    stats.count(StatsKind.DirtyFrames, s"$codeUnit: $reason")
  }

  lazy val rootABI = platform.abi(rootMethod)

  def fixMeArm64[A](x: A) = x
}
