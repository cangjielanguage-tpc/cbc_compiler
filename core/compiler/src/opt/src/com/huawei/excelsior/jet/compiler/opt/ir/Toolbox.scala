/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, NoPosition, Position}
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.common.XString.ascii
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo.PGO
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.util.WhileChanged.whileChanged
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, ClassType as SymClassType, Method as SymMethod, Type as SymType}
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.compiler.{PreparationKind, RTConst, RTSProc}
import com.huawei.excelsior.jet.util.graph.ordering.TopSort
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind, Loops, ObjectBiGraph}
import com.huawei.excelsior.jet.util.{ScalaCollections, Worklist}

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.Function.const
import scala.PartialFunction.cond
import scala.collection.mutable.ArrayBuffer

/**
 * General purpose operations on IR.
 *
 * @author paul
 */
trait Toolbox { self: Universe =>

  /** Inserts code (simple sequence of spinal or floating nodes) after given `ctrlBefore`.
    *
    * If `useDefaultHandler` is set, inserted throwing nodes will have no handler in this method.
    * Else if `xContext` may throw exception, inserted code may also be throwing
    * (the handler is set automatically to the `xContext`'s one).
    * Otherwise it is prohibited to insert throwing nodes.
    *
    * Inserted code may redefine memory (i.e. insert memory nodes).
    * Memory inputs of memory nodes below (corresponding to incremental GCM) are updated automatically.
    *
    * All inserted code positions are set to `posContext`'s position.
    * You may override this by including `withPos()` calls into `action`.
    */
  def insertCode[T](ctrlBefore: UpperPoint, xContext: Node, useDefaultHandler: Boolean, posContext: Node)(action: => T): T = {
    replaceByCodeImpl(Scope.createAnchor(ctrlBefore), useDefaultHandler, xContext, posContext) {
      val res = action
      currentScope.setResult(Void())
      res
    }
  }

  /** @see [[insertCode]] */
  def insertCodeBefore[T](point: LowerPoint, useDefaultHandler: Boolean = false)(action: => T): T =
    insertCode(point.inCtrl, point, useDefaultHandler, point)(action)

  /** @see [[insertCode]] */
  def insertCodeAfter[T](point: UpperPoint, useDefaultHandler: Boolean = false)(action: => T): T =
    insertCode(point, point, useDefaultHandler, point)(action)

  def linkNewXPoints(origPoint: SpinalNode, newXPoints: collection.Seq[XPoint]): Unit = {
    val origXEdge = origPoint.xpoint.xEdge
    Block.addEdgesWithTemplate(newXPoints, origXEdge)
  }

  /** Replaces given node by code (simple sequence of spinal or floating nodes).
    *
    * Note: `action` should return the value replacement if needed.
    */
  def replaceByCode[T <: Node](node: SpinalNode, useDefaultHandler: Boolean = false)(action: => T): T = {
    val res = replaceByCodeImpl(node, useDefaultHandler, xContext = node, posContext = node) {
      val result = action
      result match {
        case n: Return => shouldNotReachHere("You should not use Return in replaceByCode.")
        case n: ProducesValue => currentScope.setResult(n)
        case _ => currentScope.setResult(Void())
      }
      result
    }
    node.setReferent(res)
    res
  }

  private def replaceByCodeImpl[T](node: SpinalNode, useDefaultHandler: Boolean = false, xContext: Node, posContext: Node)(action: => T): T = {
    def withoutXPoints[K](action: => K): K = {
      def prohibitXPoints(x: Node): Unit = assert(!x.isInstanceOf[XPoint])

      onCommit.withCallback(prohibitXPoints)(action)
    }
    // TODO: unify with Lowering.lowerNode

    val scopeXContext = xContext match {
      case context: SpinalNode if context.canThrow =>
        if (context.hasXHandler && !useDefaultHandler)
          Some(XHandler(context.xpoint))
        else
          Some(Unwind)
      case _ =>
        if (useDefaultHandler)
          Some(Unwind)
        else
          None
    }

    val (scope, result) = createScopeWithState(node, posContext.pos, Some(scopeXContext.getOrElse(Unwind))) {
      if (scopeXContext.isEmpty)
        withoutXPoints(action)
      else
        action
    }
    scope.merge()

    result
  }

  //////////////////////////////////////////////////////////////////////////////////////////////

  sealed abstract class PredicateConstructor {
    final def &&(that: PredicateConstructor): PredicateConstructor = PredicateConstructor.Conjunction(this, that)
    final def ||(that: PredicateConstructor): PredicateConstructor = PredicateConstructor.Disjunction(this, that)
    final def unary_! = PredicateConstructor.Negation(this)
  }

  object PredicateConstructor {
    def atom(test: => Node): PredicateConstructor = atom(_ => test)
    def atom(test: UpperPoint => Node): PredicateConstructor = Atom(test)

    def nonNull(obj: Node): PredicateConstructor = atom(Cmp(obj.tpe, Condition.NE)(obj, AnyNull(obj.tpe)))

    def instanceOf(obj: Node, klass: SignatureType): PredicateConstructor = atom(
      Cmp(IntType, Condition.NE)(InstanceOf(klass)(obj), IConst(0))
    )

    def equalType(x: Node, y: Node): PredicateConstructor = atom(ctrl =>
      Cmp(AddrType, Condition.EQ)(InstanceDescriptorBy(ctrl, x), InstanceDescriptorBy(ctrl, y))
    )

    def tauTest(guard: Guard, info: TauInfo, obj: Node, rcvType: ReferenceType = null): PredicateConstructor =
      nonNull(obj) && tauTestUnchecked(guard, info, obj, rcvType)

    def tauTestUnchecked(guard: Guard, info: TauInfo, obj: Node, rcvType: ReferenceType = null): PredicateConstructor =
      atom(TauTest(guard, info, _, obj, rcvType))

    case class Atom(test: UpperPoint => Node) extends PredicateConstructor
    case class Negation(predicate: PredicateConstructor) extends PredicateConstructor
    case class Conjunction(left: PredicateConstructor, right: PredicateConstructor) extends PredicateConstructor
    case class Disjunction(left: PredicateConstructor, right: PredicateConstructor) extends PredicateConstructor
  }

  def replaceByPredicate(branch: If, pred: PredicateConstructor): Seq[If] = pred match {
    case PredicateConstructor.Atom(test) =>
      branch.selector = withPos(branch) { test(branch.inCtrl) }
      Seq(branch)

    case PredicateConstructor.Negation(pred) =>
      If.internal.swapExits(branch)
      replaceByPredicate(branch, pred)

    case PredicateConstructor.Conjunction(left, right) =>
      val top = branch
      val bot = makeJunctionAfter(branch, asConjunction = true)
      replaceByPredicate(top, left) ++ replaceByPredicate(bot, right)

    case PredicateConstructor.Disjunction(left, right) =>
      val top = branch
      val bot = makeJunctionAfter(branch, asConjunction = false)
      replaceByPredicate(top, left) ++ replaceByPredicate(bot, right)
  }

  private def makeJunctionAfter(branch: If, asConjunction: Boolean): If = withPos(branch) {
    // These tiny methods unify IR transformation for conjunction/disjunction cases.
    def thenExit(b: If) = if (asConjunction) b.trueExit else b.falseExit
    def elseExit(b: If) = thenExit(b).otherExit

    // Note that code below uses conjunction's terminology for then/else branches.
    // For disjunction case everything is reversed.

    val thenEdge = thenExit(branch).outEdge

    val newBlock = BBlock(thenExit(branch))
    val newBranch = If(newBlock, branch.memoryAfter, branch.selector)

    thenEdge.source = thenExit(newBranch)
    Block.addEdgeWithTemplate(elseExit(newBranch), elseExit(branch).outEdge)

    newBranch
  }

  //////////////////////////////////////////////////////////////////////////////////////////////

  /** Splits block before `point` by inserting empty diamond with given `test` as condition.
    * Returns a triplet of inserted branches in top-down order, `trueBlock` and `falseBlock`.
    */
  def insertEmptyDiamondBefore(point: LowerPoint, pred: PredicateConstructor): (Seq[If], Block, Block) = {
    val proxy = Proxy(ConditionType)(point.block)
    val proxyBranch = insertEmptyDiamondBefore(point, proxy)
    val trueBlock = proxyBranch.trueBlock
    val falseBlock = proxyBranch.falseBlock
    val branches = replaceByPredicate(proxyBranch, pred)
    assert(proxy.uses.isEmpty)
    decommit(proxy)
    (branches, trueBlock, falseBlock)
  }

  private def insertEmptyDiamondBefore(point: LowerPoint, selector: Node): If = withPos(point) {
    val gotoBefore = Block.splitBefore(point)
    val ctrlBefore = gotoBefore.inCtrl
    val memBefore = gotoBefore.inMemory

    val branch = If(ctrlBefore, memBefore, selector)

    val trueBlock = BBlock(branch.trueExit)
    val falseBlock = BBlock(branch.falseExit)

    gotoBefore.target.replaceArgs(Goto(trueBlock, memBefore), Goto(falseBlock, memBefore))

    decommit(gotoBefore)
    branch
  }

  /** Make half-diamond around given node using given predicate.
    * {{{ node()  ~~>  if (pred) { node() } }}}
    * Node must have no value uses, control and memory uses are updated.
    * Returns a triplet of inserted branches in top-down order, `nodeBlock` and `skipBlock`.
    */
  def wrapUnderPredicate(node: SpinalNode, pred: PredicateConstructor): (Seq[If], Block, Block) = {
    require(!node.hasValueUses)
    val joinBlock = splitControlAndMemoryAfter(node).target

    val res @ (_, _, skipBlock) = insertEmptyDiamondBefore(node, pred)
    val skipGoto = skipBlock.blockEnd.asInstanceOf[Goto]
    Block.removeEdge(skipGoto.targetEdge)
    joinBlock.addArg(skipGoto)
    res
  }


  /** Replace spinal node by diamond with itself on backup path and new spinal node created on fast path. */
  def replaceByDiamondWithFastPath[N <: SpinalMemoryNode](node: N)
                                                         (pred: PredicateConstructor)
                                                         (fastPathMaker: => N) = withPos(node) {
    populateFastPaths(node)(fastPathExitOfPredicatedDiamond(node, pred))(() => fastPathMaker)
  }

  /** Replace spinal node by multi-diamond with itself on backup path and new spinal nodes created on fast paths. */
  def replaceByMultiDiamondWithFastPaths[N <: SpinalMemoryNode](node: N, obj: Node, info: TauInfo)
                                                               (guards: Guard*)
                                                               (fastPathMakers: (() => N)*) = withPos(node) {
    val fastPathExits = guards match {
      case Seq(guard) =>
        Seq(fastPathExitOfPredicatedDiamond(node, PredicateConstructor.tauTest(guard, info, obj)))

      case _ =>
        val ctrlBefore: ControlNode = node.inCtrl
        val memBefore = node.inMemory

        // move node to separate block (backup path block)
        val gotoBefore = Block.splitBefore(node)
        val backupPathBlock = node.block
        backupPathBlock.replaceArgs() // remove the input from goto

        val tauTestBranch = TauSwitch(guards, info.asInstanceOf[PGO])(ctrlBefore, memBefore, obj)
        backupPathBlock.addArg(tauTestBranch.defaultExit)

        // it should be decommitted after we create new blockEnd above
        decommit(gotoBefore)
        tauTestBranch.caseExits
    }

    populateFastPaths(node)(fastPathExits: _*)(fastPathMakers: _*)
  }

  private def fastPathExitOfPredicatedDiamond[N <: SpinalMemoryNode](node: N, pred: PredicateConstructor) = {
    splitControlAndMemoryAfter(node)
    val (_, fastBlock, _) = insertEmptyDiamondBefore(node, pred)
    val fastExit = fastBlock.blockEnd.asInstanceOf[Goto]
    Block.removeEdge(fastExit.outEdge)
    fastExit
  }

  private def populateFastPaths[N <: SpinalMemoryNode](node: N)
                                                      (fastPathExits: BlockExit*)
                                                      (fastPathMakers: (() => N)*) = {
    val fastPathNodes = fastPathExits zip fastPathMakers map { case (exit, makeFastPath) => withPos(node) {
      // create fast path block with new node
      val fastPathBlock = BBlock(exit)
      Goto(fastPathBlock, fastPathBlock)
      insertCode(fastPathBlock, node, useDefaultHandler = false, node) {
        makeFastPath()
      }
    }}

    withJoinAfter(node, fastPathNodes)(_.outCtrl) { join =>
      // replace value uses by phi-function
      if (node.producesValue) {
        join(node, n => n)
      }
    }

    fastPathNodes
  }


  def markTauBackupPath[N <: SpinalMemoryNode](point: LowerPoint, obj: Node, guard: Guard, msg: String): Unit = {
    markTauBackupPath(point, obj, Seq(guard), msg)
  }

  def markTauBackupPath[N <: SpinalMemoryNode](point: LowerPoint, obj: Node, guards: Seq[Guard], msg: String): Unit = {
    if (env.enabled(InstrumentTauBackupPath)) {
      val rtsProc = if (ScalaCollections.uniqueValue(guards) contains CHABitGuard) RTSProc.JR_CHABackupPath else RTSProc.JR_TauBackupPath
      insertCodeBefore(point) {
        RTSCall(rtsProc)(obj, AJString.bstr(ascii(s"$msg\n guards=${guards.map(_.name).mkString(",")}.")))
      }
    }
  }

  /** Creates merge point after given `node` and joins control and values in the order of `keys` sequence. */
  def withJoinAfter[T](node: MemoryNode, keys: collection.Seq[T])
                      (controlArg: T => Node)
                      (joinValues: ((Node, T => Node) => Unit) => Unit): Block = {

    // 1. split block after given node
    require(node.memoryUses.nonEmpty)
    val goto = splitControlAndMemoryAfter(node)
    val joinBlock = goto.target
    assert(joinBlock.redefinesMemory)

    // 2. add joined control args
    for (key <- keys) {
      joinBlock.addArg(controlArg(key))
    }

    // 3. join values
    joinValues { (n, f) =>
      val tpe = n.tpe
      val args = joinBlock +: n +: (keys map f)
      val phi = Phi(tpe)(args.toSeq: _*)
      if (phi != n) {
        def replaceable(e: Edge) = e.target match {
          case `phi` if e.targetArgIndex == 0 => false
          case Goto(_, `joinBlock`) => false
          case _ => true
        }
        n.replaceUses { case e if e.isValue && replaceable(e) => phi }
      }
    }

    joinBlock
  }

  /** Split block after given node eliminating cross-block control and memory edges
    * from the node to dependent nodes.
    */
  private def splitControlAndMemoryAfter(point: UpperPoint): Goto = {
    val goto @ Goto(_, newBlock) = Block.splitAfter(point)
    point.replaceUses {
      case e if e.isMemory && e.target != goto => newBlock
    }
    goto
  }

  def replaceByGoto(exit: Branch.Exit): Goto = {
    val branch = exit.owner
    ensureNotCurrentCtrl(branch)
    // We push null state, because we do not know, when this optimization is used - in DataFlow (with state) or in middle.
    val goto = currentScope.inState(null) {
      val goto = Goto(branch.inCtrl, branch.inMemory)
      exit.replaceUsesBy(goto)
      branch.makeUsesUnreachable()
      decommit(branch)
      goto
    }
    goto
  }

  def replaceByHalt(blockEnd: BlockEnd): Halt = {
    blockEnd match {
      case h: Halt => h
      case _ =>
        val block = blockEnd.block
        val h = Halt.explained(s"replaced block end ${blockEnd.simpleName}").withExplicitArgs(blockEnd.inCtrl, blockEnd.inMemory)
        block.blockEnd = h
        blockEnd.makeUsesUnreachable()
        decommit(blockEnd)
        h
    }
  }

  def replaceByReturn(blockEnd: BlockEnd, inValue: Node): Return = {
    blockEnd match {
      case ret: Return if ret.tpe == inValue.tpe =>
        ret.inValue = inValue
        ret

      case _ =>
        val block = blockEnd.block
        val ret = Return(blockEnd.inCtrl, blockEnd.inMemory, inValue)
        block.blockEnd = ret
        blockEnd.makeUsesUnreachable()
        decommit(blockEnd)
        ret
    }
  }

  def replaceByErrorRTSCall(blockEnd: BlockEnd, context: Node, rtsProc: RTSProc, useDefaultHandler: Boolean = false)(args: Node*): Unit = {
    val halt = replaceByHalt(blockEnd)
    insertCode(halt.inCtrl, context, useDefaultHandler, context) {
      ErrorRTSCall(rtsProc)(args: _*)
    }
  }

  def insertErrorRTSCallBefore(point: SpinalNode, rtsProc: RTSProc, useDefaultHandler: Boolean = false)(args: Node*): Unit = {
    val goto = Block.splitBefore(point)
    replaceByErrorRTSCall(goto, point, rtsProc, useDefaultHandler)(args: _*)
  }

  /** Replaces check by error throwing (i.e. check is known to be failed). */
  def replaceCheckByThrow(check: ThrowingPureCheck): Unit = {
    if (check.trusted) {
      replaceCheckByFatal(check)
    } else {
      val (throwProc, throwArgs) = check.throwInfo
      insertErrorRTSCallBefore(check, throwProc)(throwArgs: _*)
    }
    strikeOut(check)
  }

  /** Replaces check by fatal (i.e. check is known to be failed, but it is trusted or non-throwing). */
  def replaceCheckByFatal(check: PureCheck): Unit = {
    val prefix = if (check.trusted) "trusted " else { assert(!check.canThrow); "" }
    val message = AJString.bstr(ascii(s"${prefix}${check.name} failed"))
    insertErrorRTSCallBefore(check, RTSProc.JR_FatalError, useDefaultHandler = true)(message)
  }

  /** Replaces check by error throwing if test passes (i.e. if test passes check is known to be failed). */
  def replaceCheckByThrowIf(check: ThrowingPureCheck, test: PredicateConstructor): Unit = {
    val (_, trueBlock, falseBlock) = insertEmptyDiamondBefore(check, test)

    val Goto(throwBlock, continueBlock) = Block.splitAfter(check)
    throwBlock.replaceArgs(trueBlock.blockEnd)
    continueBlock.addArg(falseBlock.blockEnd)

    // After edges manipulation we should eliminate uses of check.
    check match {
      case check: CheckCast => check.unlinkDependentWeakCasts()
      case _ => assert(!check.hasTag(Tag.VALUE))
    }
    replaceCheckByThrow(check)
  }

  /** Replaces cast by error throwing if casted object is non-null. */
  def replaceCheckCastByThrowIfNonNull(check: CheckCast): Unit =
    replaceCheckByThrowIf(check, PredicateConstructor.nonNull(check.obj))

  def insertHaltAfter(point: UpperPoint): Unit = replaceByHalt(Block.splitAfter(point))
  def insertHaltBefore(point: LowerPoint): Unit = replaceByHalt(Block.splitBefore(point))


  private val nodeReplacer = new NodeReplacer

  /** Decommits node and replaces all uses by corresponding arguments.
   *  Requires that node has the only argument for each resulting tag.
   *
   *  Only nodes with control and memory uses are supported now.
   *  There must be no VALUE uses.
   *  NB: if node has XCONTROL tag the corresponding use is removed and not replaced.
   */
  def strikeOut(node: SpinalNode): Unit = {
    node match {
      case cc: CheckCast => cc.unlinkDependentWeakCasts()
      case _ =>
    }
    require(!node.hasValueUses)
    currentScope.rewindFromState(node)
    Block.withoutSpineChangedControlNumInvalidation(node.block) {
      replaceCompletelyInPartsAndRemoveXPoint(node) {
        case Tag.CONTROL => node.inCtrl
        case Tag.MEMORY => node.asInstanceOf[HasInMemory].inMemory
      }
    }
  }

  def strikeOutWithValueUses(node: SpinalNode, valueReplacement: Node): Unit = {
    node.replaceValueUsesBy(valueReplacement)
    strikeOut(node)
    node.setReferent(valueReplacement)
  }

  def replaceValueUsesByNoValueAndStrikeOut(node: SpinalNode): Unit = {
    if (node.producesValue) {
      node.replaceValueUsesByNoValue()
    }
    strikeOut(node)
  }

  /** Decommits node and replaces all uses by following replacement:
   *  replacement for usage with tag `tag` is `replacements(tag)`.
   *
   *  NB: if node has XCONTROL tag the corresponding use is removed and not replaced.
   */
  def replaceCompletelyInPartsAndRemoveXPoint(node: Node)(replacements: PartialFunction[Tag, Node]): Unit = {
    assert(!replacements.isDefinedAt(Tag.XCONTROL))
    // TODO: we need to guarantee that `replacements` will not create new nodes and they are already created and have appropriate XPoint if necessary
    node match {
      case node: SpinalNode => node.removeXPoint()
      case _ =>
    }
    nodeReplacer.replace(node)(replacements)
  }

  /** Decommits node and replaces all value uses by given replacement.
   *  Requires that node's tags are the subset of replacement's tags.
   */
  def replaceTransitively(node: NonControlNode, replacement: Node): Unit = {
    nodeReplacer.replace(node, replacement)
  }

  def bulkReplace(action: => Unit): Unit = {
    nodeReplacer.bulk(action)
  }


  def unifyReturns(valueType: Type): Return = {
    all[Return].toList match {
      case Nil => null.asInstanceOf[Return]
      case List(exit) => exit
      case exits =>
        val valueArgs = exits.map(_.inValue)

        val block = BBlock(exits map { exit =>
          val goto = Goto(exit.inCtrl, exit.inMemory)
          decommit(exit)
          goto
        }: _*)

        val valuesPhi = Phi(valueType)((block +: valueArgs): _*)

        Return(block, block, valuesPhi)
    }
  }

  def constructSingleHandler(): Throw = {
    val xpoints = currentScope.unhandledXPoints().toList

    if (xpoints.nonEmpty) {
      withPos(rootMethodPos) {
        val xblock = XBlock(xpoints: _*)
        val catchNode = Catch(xblock)
        val goto = Goto(xblock, xblock)
        val throwBlock = BBlock(goto)
        val thrw = Throw(throwBlock, xblock, catchNode)
        Halt.afterThrow("single throw statement generation")(thrw, thrw)
        thrw
      }
    } else {
      null
    }
  }

  object ZeroValueNode {
    /** Return constant zero value node for given `tpe`. */
    def apply(tpe: Type): Node = tpe match {
      case _: StructureType => AnyNull(tpe)
      case LongType         => LConst(0L)
      case IntType          => IConst(0)
      case FloatType        => FConst(0.0f)
      case DoubleType       => DConst(0.0)
    }

    def unapply(node: Node): Boolean = node match {
      case _: AnyNull | LConst(0L) | IConst(0) => true
      case FConst(fc) => floatToRawIntBits(fc) == 0
      case DConst(dc) => doubleToRawLongBits(dc) == 0L
      case _ => false
    }
  }

  /** CFG edge is critical iff its source has more than one successor and its target has more than one predecessor. */
  def isCriticalEdge(edge: Edge): Boolean = cond(edge) {
    case Edge(source: ControlNode, target: Block) => target.predBlocks.size > 1 && source.block.succBlocks.size > 1
  }

  /** This transformation splits all critical edges by inserting new empty blocks on them.
    * After the transformation there are no critical edges in CFG, except exceptional.
    *
    * @see [[isCriticalEdge]]
    * @see [[splitExceptionalCriticalEdges]]
    */
  def splitCriticalEdges(withXHandlers: Boolean = false): Boolean = {
    var changed = false
    for {
      block <- all[BBlock] if block.predBlocks.size > 1
      edge <- block.inEdges if edge.isControl
      pred = edge.source.block
      if (pred.succBlocks.size > 1) || (withXHandlers && (pred.succBlocks.nonEmpty && pred.hasXHandlers))
    } {
      splitControlEdge(edge)
      changed = true
    }
    changed
  }

  /** This transformation splits all exceptional critical edges by inserting new empty [[XBlock]]s on them.
    * After the transformation there are no critical exceptional edges in CFG.
    *
    * @see [[isCriticalEdge]]
    */
  def splitExceptionalCriticalEdges(): Unit = {
    for (xb <- all[XBlock].toList if xb.inputs.size > 1) {
      val xpoints = xb.inputs.toList
      val topXPoints = Sets[XPoint].newQSet(xpoints map (_.block.handledXPoints.next()))

      // TODO: unify XBlock and BBlock and replace the following code with:
      //       xb.replaceArgsBySeq(xgotos)
      // Move contents of original XBlock to newly created BBlock. Don't try to simply `xb replaceBy catchBlock`,
      // maybe introduce proper `replaceBlock` instead.
      val Goto(_, catchBlock) = Block.splitAfter(xb)
      xb.replaceArgsBySeq(Nil)
      val xgotos = xpoints map { xp =>
        val b = XBlock(xp)
        Goto(b, b)
      }
      catchBlock.replaceArgsBySeq(xgotos)
      xb.phies.toList foreach { phi =>
        phi replaceBy Phi(phi.tpe)(catchBlock +: phi.argsSeq: _*)
      }
      assert(xb.redefinesMemory)
      xb.replaceMemoryUsesBy(catchBlock)

      decommit(xb.blockEnd)
      assert(xb.uses.isEmpty)
      decommit(xb)

      // TODO: if (OneXHandlerForABlock)
      for (xp <- xpoints if !topXPoints(xp)) {
        Block.splitBefore(xp.owner)
      }
    }
  }

  /** Splits given CFG edge if it is critical and returns newly created block. */
  def splitCriticalEdge(edge: Edge): Option[BBlock] = {
    Option.when(isCriticalEdge(edge))(splitControlEdge(edge))
  }

  /** This transformation splits back edges of infinite self loops by inserting new empty blocks on them. */
  def splitInfiniteSelfLoops(): Unit = {
    for {
      block <- all[BBlock] if block.succBlocks.size == 1
      edge <- block.inEdges if edge.isControl && edge.source.block == block
    } {
      splitControlEdge(edge)
    }
  }

  def splitControlEdge(edge: Edge): BBlock =  {
    val ControlEdge(ctrl, block: BBlock) = edge
    val newBlock = BBlock(ctrl)
    edge.source = withPos(block) { Goto(newBlock, ctrl.memoryAfter) }
    newBlock
  }

  /** Returns whether `preHeader` block is a valid pre-header for loop `header`.
    * Pre-header is the only forward predecessor of the loop header and header is the only successor of pre-header.
    */
  def isPreHeaderOf(preHeader: Block, header: BBlock) =
    (ScalaCollections.singleton(preHeader.succBlocks) contains header) && (preHeader dominates header)

  /** Returns pre-header for the header of given loop.
    * Supports only reducible loops with [[BBlock]] header.
    * For convenience modifies outer loop bodies if pre-header was created.
    *
    * @see [[isPreHeaderOf]]
    *
    * @return pre-header and flag indicating whether pre-header was created right now
    */
  def getOrCreateLoopPreHeader(loop: Loop[Block]): (Block, Boolean) = {
    require(loop.kind != LoopKind.IRREDUCIBLE)
    val header = loop.header match {
      case x: BBlock => x
      case _: XBlock =>
        // Ask @conwor if you encounter this exception.
        throw new IllegalArgumentException("creation of pre-header for XBlock is currently impossible")
    }

    header.inEdges.filterNot(loop.body contains _.source.block).toList match {
      case List(onlyForwardEdge) if onlyForwardEdge.source.block.succBlocks.size == 1 =>
        // Note that this block may have exceptional edge - it's ok for preheader.
        (onlyForwardEdge.source.block, false)

      case forwardEdges =>
        assert(header != entryBlock, "no back edges to the entry block")
        assert(forwardEdges.nonEmpty, "reducible loop without forward edges must be unreachable and not detected as loop")
        val preHeader = BBlock.extractInputEdges(header, forwardEdges)
        Loops.addToBody(loop.outer, preHeader)
        (preHeader, true)
    }
  }

  /** Returns post-exit for the given loop if it exists.
    * Post-exit is the immediate post-dominator of the loop.
    *
    * Note: currently supported only post-exit which is the target of all exits.
    * TODO: enhance post-exit detection if needed
    */
  def getLoopPostExit(loop: Loop[Block], p: Edge => Boolean = { _ => true }): Option[BBlock] = {
    ScalaCollections.uniqueValue(loopExitEdges(loop) collect { case e if p(e) => e.target.block }) collect { case b: BBlock => b }
  }

  /** Returns exclusive post-exit for the given loop if it exists or can be created.
    * Exclusive post-exit is the one dominated by the loop itself.
    * For convenience modifies outer loop bodies if new post-exit was created.
    *
    * @see [[getLoopPostExit]]
    *
    * @return exclusive post-exit and flag indicating whether new post-exit was created right now
    */
  def getOrCreateExclusiveLoopPostExit(loop: Loop[Block], p: Edge => Boolean = { _ => true }): Option[(Block, Boolean)] = {
    getLoopPostExit(loop, p) map { postExit =>
      val (exitEdges, otherEdges) = postExit.inEdges.partition(e => p(e) && (loop.body contains e.source.block))
      if (otherEdges.isEmpty){
        (postExit, false)
      } else {
        val exclusivePostExit = BBlock.extractInputEdges(postExit, exitEdges.toSeq)
        Loops.addToBody(loop.outer, exclusivePostExit)
        (exclusivePostExit, true)
      }
    }
  }

  /** Returns backward edges of given `loop`. */
  def loopBackwardEdges(loop: Loop[Block]): Iterator[Edge] =
    loop.header.inEdges filter (loop.body contains _.source.block)

  /** Returns enter edges of given `loop`. */
  def loopEnterEdges(loop: Loop[Block]): Iterator[Edge] =
    loop.header.inEdges filterNot (loop.body contains _.source.block)

  /** Returns exit edges of given `loop`. */
  def loopExitEdges(loop: Loop[Block]): Iterator[Edge] =
    loop.exits.iterator flatMap (_.xSuccBlockEdges) filterNot (loop.body contains _.target.block)

  /** Returns true iff given node dominates all backward edges of given loop. */
  def dominatesLoopBackwardEdges(n: ControlNode, loop: Loop[Block]): Boolean = {
    loopBackwardEdges(loop) map (_.source.asInstanceOf[ControlNode]) forall n.dominates
  }

  /** Return true iff given `block` is a target of any exit of given `loop`. */
  def blockIsLoopExitTarget(block: Block, loop: Loop[Block]): Boolean =
    loopExitEdges(loop).exists(_.target == block)

  def eliminateCrossBlockMemoryEdges(ts: TopSort[Block] = cfg.topSort): Boolean = {
    requireGlobalCodeMotion()

    val unprocessedMem = Maps[Block].newQMap[Node]
    for (block <- ts.order if !block.redefinesMemory) {
      unprocessedMem(block) = block.memoryAfter
    }

    if (unprocessedMem.isEmpty) {
      return false
    }

    for (block <- ts.reverse.order if unprocessedMem contains block) {
      val oldMem = unprocessedMem(block)
      oldMem.replaceUses { case e if e.isMemory && e.useBlock == block => block }
    }
    true
  }

  def eliminateCrossBlockMemoryUses(n: MemoryNode): Unit = {
    withIncrementalGCM {
      n.replaceUses {
        case e if e.isMemory && e.useBlock != n.block => e.useBlock
      }
    }
  }

  def optimizeBlockMemory(): Boolean = {
    val ts = cfg.topSort
    whileChanged { changed =>
      for (block <- ts.order if block.redefinesMemory) {
        val replacement = blockMemoryIdentity(block)
        if (replacement != block) {
          block.replaceMemoryUsesBy(replacement)
          changed()
        }
      }
    }
  }

  def blockMemoryIdentity(block: Block) = block match {
    case _ if block.inputs.exists(_.block.unreachable) => block
    case xb: XBlock => xb // TODO: optimize xblock memory
    case bb: BBlock => phiLikeIdentity(bb, bb.reachableMemoriesBefore)
  }

  def phiLikeIdentity(n: Node, inputs: IterableOnce[Node]) = {
    val inputSet = inputs.iterator.toSet
    if (inputSet.size == 1) {
      // All input values are the same
      inputSet.head
    } else if (inputSet.size == 2 && inputSet.contains(n)) {
      // Input values contains only phi function itself and some other same values, so it's cyclic phi function.
      // More powerful cyclic phies optimization performed in SimplifyComponent.eliminateCyclicPhies()
      (inputSet - n).head
    } else {
      n
    }
  }

  /** Eliminates cross-block inCtrl uses within and below given blocks.
    *
    * Ensures that inCtrl argument of controlled nodes is set within their corresponding block.
    */
  def eliminateCrossBlockInCtrlUses(blocks: IterableOnce[Block]): Unit = {
    requireGlobalCodeMotion()

    val xs = Worklist.from(
      for {
        block <- blocks.iterator
        p <- block.points
        x <- collect[ControlledNode](p.controlUses)
        if x.block != block
      } yield x
    )

    for (x <- xs) {
      x.inCtrl = x.block
    }
  }

  /** Returns whether it is OK to merge XPoints of given spinal nodes (e.g. to join these nodes into single one). */
  def safeToMergeXPointsOf(a: SpinalNode, b: SpinalNode): Boolean = {
    val xA = a.xHandlerOption
    val xB = b.xHandlerOption
    // A and B both should have no xHandler, or if they have the same xHandler,
    // they should correspond to the same input edges in all phies of the xHandler for the safe merge.
    xA == xB && (xA.isEmpty || xA.get.phies.forall(phi =>
      phi.phiArg(a.xpoint.xEdge) == phi.phiArg(b.xpoint.xEdge)
    ))
  }

  private def ensurePreparedDefaultPreparationKind: PreparationKind = {
    val ic = Position.inlineContext(curPos()) getOrElse shouldNotReachHere("should be covered by withPos()")
    PreparationKind(ic.method.isManaged, env)
  }

  def ensurePrepared(tpe: SymType, kind: PreparationKind = ensurePreparedDefaultPreparationKind): Unit = {
    if (tpe != null && targetArch != CBC) {
      PreparationCheck(tpe, kind)()
    }
  }

  /** Returns special graph based on CFG without factorized exceptional edges.
    *
    * Please note, that reachability on this graph is not equal to reachability on CFG but implementation uses
    * block.reachable which is CFG reachability. TODO: consider forbidding usage of this graph with unreachable code.
    */
  def cfgWithoutXEdges(): ObjectBiGraph[Block] = if (!currentScope.hasXEdges) cfg else new ObjectBiGraph[Block] {
    val start = entryBlock
    def succs(n: Block): Iterator[Block] = n.succBlocks
    def preds(n: Block): Iterator[Block] = n match {
      case `start` | _: XBlock => Iterator.empty
      case bb: BBlock => bb.predBlocks filter (_.reachable)
    }
  }

  /** Value at block exit. */
  case class ExitValue(exit: BlockExit, value: Node) {
    assert(value == null || value.producesValue)
  }

  implicit class ValueAtExit(value: Node) {
    def at(point: BlockExit) = ExitValue(point, value)
  }

  /** Joins given control paths. */
  def continue(edges: BlockExit*): BBlock = {
    val newBlock = BBlock(edges: _*)
    setCurrentControl(newBlock)
    newBlock
  }

  /** Joins given values. */
  def join(edges: ExitValue*): Node = {
    continue(edges map (_.exit): _*)
    if (edges.size == 1) edges.head.value else {
      makePhi(edges map (_.value): _*)
    }
  }

  /** Creates Phi function with given values. */
  def makePhi(values: Node*): Node = {
    val vtypes = values.map(_.tpe).toSet
    assert(vtypes.size == 1)
    Phi(vtypes.head)(currentCtrl.block +: values: _*)
  }

  /** Generates test for null and returns the state for null edge. */
  def makeNullTest(obj: Node): If.Exit = {
    val checkNull = If(Cmp(obj.tpe, Condition.EQ)(obj, AnyNull(obj.tpe)))
    continue(checkNull.falseExit)
    checkNull.trueExit
  }

  def makeLocationsTagTest(obj: Node, tagMask: Int): If.Exit = {
    // Get TS-word
    val tsWord = GetField(RT.ManagedObj.tsWordConst)(depriveUnsafe(obj))

    val tagsTest = If(
      Cmp(IntType, Condition.EQ)(
        And(tsWord, IConst(RTConst.ObjTags.LOCATION_BITS.intValue)),
        IConst(tagMask))
    )
    continue(tagsTest.trueExit)

    tagsTest.falseExit
  }
}
