/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.jet.compiler.options.BoolOption.{WorkaroundForJET13144, WorkaroundForJET16453}
import com.huawei.excelsior.jet.compiler.symlevel.{MethodReferenceAccessKind, SignatureType, Type as SymType}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.{StatsKind, symlevel}
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.Env.isWorkMode
import com.huawei.excelsior.jet.compiler.opt.ir.{LogsKind, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.types.{Approximation, Guards}
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.opt.middle.types.LoweredReferences.*
import com.huawei.excelsior.jet.compiler.opt.middle.types.LoweredReferences.LoweredReferenceApprox.*
import com.huawei.excelsior.jet.compiler.types.RecordType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.{LoopKind, Loops}

import scala.PartialFunction.cond
import scala.collection.mutable.ArrayBuffer

/**
  * Context type is a type of node narrowed for concrete CFG point. It is based on formal node type and takes
  * into account all checks and conditions dominating this point.
  *
  * @author conwor
  * @author paul
  * @author nastia
  */
trait ContextTypes { self: Universe =>

  type FilterFunc = Approximation => (Approximation, Boolean)

  /** Map of context types for some nodes. */
  class ContextTypesMap(private var types: Map[Node, TypeFilter]) {

    def this() = this(Maps[Node].newImmMap[TypeFilter])

    override def clone(): ContextTypesMap = new ContextTypesMap(types)

    private def getFromMap(k: Node) = {
      assert(k == k.deref)
      val key = typeCacheKeyForNode(k)
      assert(key == key.deref)
      types.get(key)
    }

    private def putToMap(k0: Node, filter: TypeFilter): Unit = {
      val key = typeCacheKeyForNode(k0)
      filter match {
        case null => types -= key
        case _    => types += (key -> filter)
      }
    }

    private def getChain(key: Node): Iterator[TypeFilter] =
      getFromMap(key) map ContextTypesMap.filterChain getOrElse Iterator.empty

    /** Returns topmost point above or equal `point` where context type of `key` assignable to given `tpe`. */
    private def topmostValidPoint(key: Node, point: UpperPoint, tpe: Approximation): ControlNode = {
      assert(key != null)
      if (tpe != null && (tpe >= nodeTypeForContextTypes(key))) {
        return topCtrl(key) ensuring (_.isCommitted)
      }

      val fullChain = getChain(key) dropWhile { f => !(f dominatesExitOf point) }
      val chain = if (tpe == null) {
        // workaround for ArrayGet, returns last check's point, which could be easily broken, see JET-12656
        fullChain take 1
      } else {
        fullChain takeWhile (tpe >= _.outType(key))
      }

      if (chain.nonEmpty) {
        chain.toIndexedSeq.last.point
      } else if (tpe == null) {
        topCtrl(key) ensuring (_.isCommitted)
      } else {
        // E.g., we try to find control argument for GetField from data flow proxy without any type information until resolving
        point
      }
    }

    /** Converts point of context type of some node to appropriate inCtrl argument for context dependent controlled nodes. */
    private def adjustContextTypePointToInCtrlArgument(point: ControlNode): UpperPoint = point match {
      case xp: XPoint => xp.handler

      case exit: Branch.Exit =>
        val target = exit.target
        if (ContextTypesMap.recalculationStage) {
          val loop = ContextTypesMap.loops loopOf target
          if (loop != null && loop.kind != LoopKind.IRREDUCIBLE && loop.header == target) {
            // We can safely return pre-header instead of the header to allow better movement out of loops.
            getOrCreateLoopPreHeader(loop)._1
          } else target
        } else target

      case p: UpperPoint => p

      case _ => shouldNotReachHere(s"unexpected context type point: $point")
    }

    /** Optimize `node` based on context type of it's [[ContextDependentNode.contextKey]].
      * Returns true iff optimization was achieved.
      */
    def optimizeContextDependentNode(node: ContextDependentNode): Boolean = {
      val key = node.contextKey
      if (key != null) {
        val point = topmostValidPoint(key, node.inCtrl, node.requiredKeyType)
        val inCtrl = adjustContextTypePointToInCtrlArgument(point)
        assert(inCtrl dominates node.inCtrl)
        if (inCtrl != node.inCtrl) {
          node.inCtrl = inCtrl
          return true
        }
      }
      false
    }

    private def isStaticCallAndAffectsClinit(n: Call) = {
      n.targetRef.hasMethod && n.targetRef.method.getDeclaringClass.isClassOrInterface && n.targetRef.accessKind == MethodReferenceAccessKind.STATIC
    }

    def makeFilter(n: Node): TypeFilter = n match {
      case n: Call if ContextTypesMap.recalculationStage && isStaticCallAndAffectsClinit(n) =>
        val filter = makeFilter0(n, JVMState(), { inType => clinitFilter(inType, n.targetRef.method.getDeclaringClass) })
        // Filter may be redundant, if so it's important to not make it, since nodes, which produces redundant filters,
        // will be removed during ContextTypesRecalculation, which we can't afford to happen to static call
        if (filter.isRedundant) null else filter

      case tf: TypeFilterNode =>
        makeFilter0(tf, tf.filteredArg, { inType => tf.filterType(inType, tf) })

      case xp @ XPoint(tf: TypeFilterNode) =>
        makeFilter0(xp, tf.filteredArg, { inType => tf.filterType(inType, xp) })

      case xp @ XPoint(n: Call) if ContextTypesMap.recalculationStage && isStaticCallAndAffectsClinit(n) =>
        // We do it only so during abstract interpretation of block with this xpoint
        // this xpoint can have information propagated through to XBlock.
        // See [[ContextTypesRecalculation.interpret]] and [[AbstractInterpreterComponent.addXCtrl]]
        makeFilter0(xp, JVMState(), { inType => (inType, true) })

      case IfFilter(iff) =>
        makeFilter0(iff.point, iff.key, iff.func)

      case TauSwitchFilter(tsf) =>
        makeFilter0(tsf.point, tsf.key, tsf.func)

      case _ => null
    }

    private def makeFilter0(point: ControlNode, key: Node, ff: FilterFunc) =
      if (key.isInstanceOf[ReadVar]) null else new TypeFilter(point, getFromMap(key).orNull, ff, key)

    def appendFilter(filter: TypeFilter): Unit = {
      assert(filter._key != null)
      putToMap(filter._key, filter)
      filter.clearKey()

      if (ContextTypesMap.recalculationStage) {
        filter.outType(filter._key) match {
          case tpe: UpperBounded if !tpe.mayBeNull =>
            val t = tpe.root.symType
            if (t.isClass) {
              val f = makeFilter0(filter.point, JVMState(), { in => clinitFilter(in, t) })
              if (!f.isRedundant) {
                putToMap(JVMState(), f)
              }
            }
          case _ =>
        }
      }
    }

    def remove(n: Node): Unit = n match {
      case p: TypeFilterNode =>
        for (filter <- getFromMap(p.filteredArg)) {
          putToMap(p.filteredArg, filter.idom)
        }

      case XPoint(_: TypeFilterNode) => shouldNotReachHere()

      case _ =>
    }

    /** Merges this map with given `maps` at start of given `block`. */
    def merge(maps: Seq[ContextTypesMap], block: Block, identity: Boolean): Unit = {
      assert(this != ContextTypesMap.Unreachable) // Guaranteed by abstract interpreter.

      var trustedAICToInsert: collection.immutable.Map[Node, (SignatureType, collection.Set[Node])] = Maps[Node].newImmMap[(SignatureType, collection.Set[Node])]

      for ((k, oldF) <- types) {
        // The map can contain decommitted nodes after replacements done by interpretEdge, let's take the valid ones
        val key = typeCacheKeyForNode(k.deref)
        assert(key.isCommitted || key == JVMState())
        assert(typeCacheKeyForNode(k) == k)

        val idomF = this.getChain(key).find(_ dominates block).orNull
        var idomInserted = false

        if (idomF != oldF) {
          lazy val notInIrreducibleLoop = ContextTypesMap.loops.allLoopsOf(block).forall(_.kind != LoopKind.IRREDUCIBLE)
          lazy val keyExistsInAllMaps = maps.forall(x => x.types != null && x.types.contains(key))

          def insertMergeTypeFilterAtBlock(): Unit = {
            val outTypes = maps map (_.types(key).outType(key))
            val mergePointType = outTypes reduce (_ union _)
            val filter = new TypeFilter(block, idomF, { _ weakIntersect mergePointType }, key)
            if (!filter.isRedundant) {
              putToMap(key, idomF)
              idomInserted = true
              appendFilter(filter)
            }
          }

          // If key is an array it may have ArrayIndexCheck filters in merged chains. AIC filters are identity now,
          // so merge type filter for this key will not be inserted. But also AIC is Idempotent node, so if there
          // will be AIC for some `x` below, we could optimize it based on one of two assumptions:
          //   1) Each chain contains AIC for `x`
          //   2) `x` is a phi-function from p1, p2, ..., pn, and all chains contain AIC for corresponding p`s.
          //
          // To trigger this optimization through idempotent operations optimization we create trusted AIC for `x` at
          // the block start.
          def tryToInsertTrustedAIC(): Unit = {

            // Avoid inserting new AIC nodes after lowering, because some AIC might have already been lowered,
            // so this transformation will not make sense.
            // Note: in CBC where real AIC are not lowered this transformation leads to infinite lowering looping:
            //       trusted AIC is lowered, then context types inserts new trusted AIC in place of the first one,
            //       inserted trusted AIC is lowered again, and so on.
            if (currentPhase >= CompilerPhase.Lowering) {
              return
            }

            val anyKeyAIC = this.getChain(key).find(f => f.point.isInstanceOf[ArrayIndexCheck])

            // Check if key has any AIC in any map. Also take reference of array type and length.
            if (anyKeyAIC.nonEmpty) {
              def collectToTrusted(): collection.Set[Node] = {
                // Map from control input to all indices checked in corresponding chain
                val allIndices = Maps[ControlNode].newQMap[Set[Node]]

                // Map from control input to all indices checked in corresponding chain below idom of current block.
                val uniqueIndices = Maps[ControlNode].newQMap[collection.Set[Node]]

                for ((input, map) <- block.inputs zip maps) {
                  val all = new ArrayBuffer[Node]
                  val unique = new ArrayBuffer[Node]
                  var idomFound = false

                  for (f <- map.getChain(key)) {
                    if (f == idomF) idomFound = true
                    f.point match {
                      case aic: ArrayIndexCheck =>
                        all += aic.idx
                        if (!idomFound) unique += aic.idx
                      case _ =>
                    }
                  }

                  if (all.isEmpty) {
                    // Some chain does not have any AIC
                    return Set.empty
                  }

                  allIndices(input) = all.toSet
                  uniqueIndices(input) = Sets[Node].newQSet(unique)
                }

                // Collect all indices which are already trusted at block start (prevent cyclic optimization of context types)
                val alreadyTrusted = (collect[ArrayIndexCheck](block.spineForward) collect {
                  case aic if aic.trusted && EOPConvert.skip(aic.array) == key => aic.idx
                }).toSet

                // Collect phies of block, which could be trusted AIC for key at block start
                def collectPhies(): Sets[Phi]#QSet = {
                  val phies = Sets[Phi].newQSet(block.phies filterNot alreadyTrusted)
                  if (phies.isEmpty) return phies
                  for (input <- block.inputs) {
                    val indices = allIndices(input)
                    phies filterInPlace { phi => indices contains phi.phiInput(input.singleOutEdge).source }
                    if (phies.isEmpty) return phies
                  }
                  phies
                }

                // Collect indices which are checked in all chains
                val repeated = uniqueIndices.values.reduce(_ & _) filterNot alreadyTrusted

                repeated ++ collectPhies()
              }

              val toTrusted = collectToTrusted()
              if (toTrusted.nonEmpty) {
                val check = anyKeyAIC.get.point.asInstanceOf[ArrayIndexCheck]
                trustedAICToInsert += (key -> ((check.arrayType, toTrusted)))
              }
            }
          }

          if (ContextTypesMap.recalculationStage && identity && notInIrreducibleLoop && keyExistsInAllMaps) {
            insertMergeTypeFilterAtBlock()
            if (!env.enabled(WorkaroundForJET16453)) {
              tryToInsertTrustedAIC()
            }
          }
        }

        if (!idomInserted) {
          putToMap(key, idomF)
        }
      }

      if (trustedAICToInsert.nonEmpty) {
        val untouchableControlled = Sets[Node].newQSet
        val last = insertCodeAfter(block) {
          var last: ControlNode = block
          // Note: this sorting is needed for stability, because `types` map is unordered and `trustedAICToInsert`
          // is collected in random order.
          for (key <- trustedAICToInsert.keys.toSeq.sortBy(_.id); (arrayType, indices) = trustedAICToInsert(key)) {
            val plainKey = producesRich(key) match {
              case EnrichmentDecision.Yes(t) => Deprive(t)(key)
              case EnrichmentDecision.No | EnrichmentDecision.DoNotKnow => key
            }
            val length = ArrayLength(arrayType)(plainKey)
            untouchableControlled += length
            for (idx <- indices) {
              last = ArrayIndexCheck(arrayType, trusted = true)(plainKey, idx, length)
            }
          }
          last
        }
        block.replaceUses { case ControlEdge(_, node: FloatingNode) if !untouchableControlled(node) => last }
        ContextTypesMap.trustedAICInserted = true
      }
    }
  }

  object ContextTypesMap {
    private var cache = Maps[Node].newQMap[ContextTypesMap]
    private var backupCache = Maps[Node].newQMap[ContextTypesMap] // results from previous recalculation are used to refine phi functions types
    private val invalidationList = Sets[Node].newQSet
    private var recalculationStage = false // if true, then current phase is not front (see CompilerPhase)
    private var trustedAICInserted = false
    private var loops: Loops[Block] = _

    def inContextTypesRecalculationMode(action: => Unit): Boolean = {
      recalculationStage = true
      loops = cfg.loops
      trustedAICInserted = false
      try {
        action
      } finally {
        recalculationStage = false
        loops = null
        backupCache.clear()
        cleanupAfterContextTypesRecalculation()
      }
      trustedAICInserted
    }

    /**
      * Invalidates nodeType for nodes in cycles in case their context type was used before the chain for it was built
      * and thus further refinement is possible
      * Returns true if further refinement is possible
      */
    def cleanupAfterContextTypesRecalculation(): Boolean = {
      for (n <- invalidationList) {
        invalidateNodeType(n.deref)
      }
      val res = invalidationList.nonEmpty
      invalidationList.clear()
      res
    }

    lazy val Unreachable = new ContextTypesMap(null)

    def setMapAt(point: ControlNode, map: ContextTypesMap): Unit = {
      cache(point) = map.clone()
    }

    def dropCache() : Unit = {
      backupCache.clear()
      cache.clear()
      invalidationList.clear()
    }

    /** For context types recalculation we need to use context types chains built during previous iteration. */
    def resetCache() : Unit = {
      assert(invalidationList.isEmpty)
      backupCache = cache
      cache = Maps[Node].newQMap[ContextTypesMap]
    }

    private def getMostPreciseCacheForPoint(point: ControlNode, cache: Maps[Node]#QMap[ContextTypesMap]): Option[ContextTypesMap] = point match {
      case exit: Branch.Exit => cache.get(point) orElse cache.get(exit.owner)
      case _ => cache.get(point.block.blockEnd)
    }

    /** Returns the Filter chain from the current/latest recalculation */
    private def getFilterChain(node: Node, point: ControlNode): Iterator[TypeFilter] = {
      getMostPreciseCacheForPoint(point, cache) match {
        case Some(cache) => {
          if (cache.getChain(node).isEmpty) {
            // the chain is not yet calculated; but we need the context type of this node;
            // so we need to invalidate the result after the recalculation stage is finished;
            // when asking nodeType or context type again, it may be refined
            if (recalculationStage) {
              invalidationList += node
            }
          }
          cache.getChain(node)
        }
        case _ => {
          if (backupCache.contains(point)) {
            if (recalculationStage) {
              invalidationList += node
            }
          }
          Iterator.empty
        }
      }
    }

    /** Returns the filter chain from the previous recalculation cycle */
    private def getPrevFilterChain(node: Node, point: ControlNode): Iterator[TypeFilter] =
      getMostPreciseCacheForPoint(point, backupCache) match {
        case Some(cache) => cache.getChain(node)
        case _ => Iterator.empty
      }

    private def getContextType(node: Node, point: ControlNode)(acceptable: TypeFilter => Boolean): Option[Approximation] = {
      val chain = getFilterChain(node, point)
      if (chain.nonEmpty) {
        val t1 = chain find acceptable map (_.outType(node))

        // New result should be always better or equal to previous one
        assert({
          val t2 = getPrevFilterChain(node, point) find acceptable map (_.outType(node))
          (t1, t2) match {
            case (Some(t1), Some(t2)) =>
              (t2 >= t1) || {
                if (isWorkMode) {
                  env.forcePrintln("Type calculated at current iteration: " + t1)
                  env.forcePrintln("Type calculated at previous iteration: " + t2)
                  val dbgPrinter = new IRLogsDebugPrinter(LogsKind.CodeGen)
                  dbgPrinter.debugNodes("Context types error", n => s"\t| ${n.pos}")
                  dbgPrinter.debugGraphs("Context types error")
                }
                env.enabled(WorkaroundForJET13144)
              }
            case _ => true
          }
        })

        t1
      } else {
        getPrevFilterChain(node, point) find acceptable map (_.outType(node))
      }
    }

    def getContextTypeAt(node: Node, point: ControlNode): Option[Approximation] =
      getContextType(node, point) { _ dominates point }

    def getContextTypeAfter(node: Node, point: UpperPoint): Option[Approximation] =
      getContextType(node, point) { _ dominatesExitOf point }

    def optimizeContextDependentNode(node: ContextDependentNode): Boolean = {
      getMostPreciseCacheForPoint(node.inCtrl, cache) match {
        case Some(map) => map.optimizeContextDependentNode(node)
        case None => false
      }
    }

    def findInterfaceTypeCheckForWeakCast(obj: Node, itype: symlevel.Type, point: ControlNode): Option[Node] =
      getFilterChain(obj, point.block) collect { case f if f dominates point => f.point } collectFirst {
        case cc: CheckCast if !cc.trusted && cc.obj == obj && cc.targetType.symType == itype => cc
        case IfFilter(IfFilter.IfInstanceOf(_, iof @ InstanceOf(tpe, `obj`), true)) if tpe.symType == itype => iof
      }

    private[ContextTypes] def filterChain(f: TypeFilter): Iterator[TypeFilter] = {
      val wholeChain = ScalaCollections.iterateUntilNull(f)(_.idom)

      // During IR transformations some of filters may be decommitted before context types info recalculated.
      // There is no reason to use these filters in chain (e.g., we cannot call dominance for them).
      wholeChain withFilter { _.point.isCommitted }
    }

    /** Returns mapping from node to its type filter chains.
      * Note that sequence of chains may have duplicates.
      */
    def allTypeFilterChains: Iterator[(Node, Seq[Iterator[TypeFilter]])] = {
      // TODO: implement me better, right now it has too many duplicates
      ScalaCollections.toMultiMap(
        for {
          map <- cache.values
          (n, filter) <- map.types
          if n.isInstanceOf[JVMState] || n.isCommitted
        } yield (n, filterChain(filter))
      ).iterator
    }

    def loweredTypes = currentPhase >= CompilerPhase.Lowering


    /** Lowers control dependency of `node` by `point` and recursively lowers it for all key-arg dependencies of `node`. */
    def lowerControlDependencies(node: ContextDependentNode, point: UpperPoint): Unit = {
      if (node.inCtrl dominates point) {
        node.inCtrl = point
        for (ContextDependency(_, use) <- node.valueOutEdges) {
          lowerControlDependencies(use, point)
        }
      } else {
        assert(point dominates node.inCtrl)
      }
    }

    private val loweredTypesCache = Maps[Node].newMMap[LoweredReferenceApprox]

    def convertContextTypes4Lowering(): Unit = {
      for (n <- allNodes) {
        if (n.tpe.isTraceableRefType) {
          val tpe = fromReferenceApproximation(nodeType(n))
          if (tpe != LoweredRefNullable) {
            loweredTypesCache.put(n, tpe)
          }
        }
      }
    }

    def lowerNode(loweredNode: Node, originalNode: Node): Unit = {
      loweredTypesCache.get(originalNode) match {
        case Some(tpe) =>
          loweredTypesCache.put(loweredNode, tpe)
          loweredTypesCache.remove(originalNode)
        case None =>
      }
    }

    def getLoweredType(node: Node): Approximation = {
      loweredTypesCache.get(node) match {
        case Some(tpe) => tpe
        case None => fromReferenceApproximation(nodeType(node))
      }
    }
  }

  class TypeFilter(val point: ControlNode, val idom: TypeFilter, ff: FilterFunc,

                   // only for temporary use; cleared when filter added to contextTypesMap
                   private[ContextTypes] var _key: Node) {

    private [ContextTypes] def clearKey() = { _key = null; this }

    private var cacheVersion: Int = _
    private var _isRedundant: Boolean = _
    private var _isUnreachable: Boolean = _
    private var _outType: Approximation = _

    point match {
      case _: UpperPoint | _: XPoint | _: Branch.Exit =>
      case _ => shouldNotReachHere(s"unexpected point of TypeFilter: $point")
    }
    recalculate(_key)

    private [ContextTypes] def outType(key: Node) = {
      if (cacheVersion < nodeTypeCacheVersion()) recalculate(key)
      _outType
    }

    private def recalculate(key: Node): Unit = {
      // In run-time some filters may have limited input type (e.g. CHABitTest will crash passed `null`).
      // While creating such nodes we actually guarantee these limits.
      // However our type system is conservative and inType may break these limits (e.g. nullable type for CHABitTest).
      // Current implementation of filters just ignores such situation without limiting input type
      // (e.g. outType for CHABitTest will be nullable).
      // It is done so because filtering it out may be observed as real filtering (which is not logically correct).
      // This problem may be solved by trustedPreFilterFunc which should be called somewhere here.
      // TODO: introduce trustedPreFilterFunc, see Guards.processNullable

      val inType = if (idom != null) {
        idom.outType(key)
      } else {
        assert(key != null)
        nodeTypeForContextTypes(key)
      }

      val (outType, strict) = ff(inType)

      inType.compare(outType) match {
        case CC.Equal =>
          if (strict) {
            _isRedundant = true
          }

        case CC.Greater =>
          if (outType.isEmpty) {
            assert(strict)
            _isUnreachable = true
          }

        case _ =>
          assert(!strict)
          // Non-narrowing filtering is allowed only in case of interface cone filtered to partially equal class cone
          // (e.g. inType = oc(j.i.Serializable), filter = CheckCast(j.u.AbstractCollection)), outType = oc(j.u.AbstractCollection)).
          // It means that in any filter chain there can only be a single link with non-narrowing type transformation.
          // And correctness of some optimizations depends on this fact.
          // Ask      @cypok and @liontiger for more information.
          def knownValidTypes(in: ReferenceType, out: ReferenceType): Boolean = cond((in, out)) {
            case (_: InterfaceType, _: ClassType) => true
            case (JavaReferenceArrayType(inElem), JavaReferenceArrayType(outElem)) => knownValidTypes(inElem, outElem)
          }

          assert(cond(inType, outType) {
            case (OpenCone(in, _), Cone(out, _)) if knownValidTypes(in, out) => true
          })
      }

      if (!_isRedundant && !_isUnreachable && idom != null) {
        point match {
          case point: Idempotent if point.isCommitted =>
            val haveSameCheck = ContextTypesMap.filterChain(idom).exists { f =>
              f.point match {
                case that: Idempotent if that.isCommitted && (that idempotents point) => true
                case _ => false
              }
            }
            if (haveSameCheck) {
              _isRedundant = true
            }
          case _ =>
        }
      }

      _outType = outType
      cacheVersion = nodeTypeCacheVersion()
    }

    /** Returns true iff control flow edge associated with `this` filter dominates `that`. */
    def dominates(that: ControlNode) = point match {
      case point: UpperPoint => point.outCtrl ensuring (_.isCommitted) dominates that
      case _: XPoint | _: Branch.Exit => point dominates that
      case _ => shouldNotReachHere()
    }

    /** Returns true iff control flow edge associated with `this` filter dominates control flow exit edge of node `that`.
      *
      * This method and [[dominates]] method should satisfy the following identity:
      * {{{
      *   dominatesExitOf(that) == dominates(that.outCtrl)
      * }}}
      * But there could be no [[UpperPoint.outCtrl]] for `this` filter point or `that` (normal situation for parsing).
      *
      * TODO: refactor control flow processing in parsing and remove this method (JET-13785).
      */
    def dominatesExitOf(that: UpperPoint) = {
      // This implementation is bugged. Consider filter with NullCheck `point` and some `that` in it's handler. This
      // method will return true for them, which is unpredictable. Implementation below is correct, but we could not
      // use it until JET-13785 not fixed.
      point dominates that

      /*
      point match {
        case _: UpperPoint => (this dominates that) || (point == that)
        case projection: Projection => (this dominates that) || (projection.owner == that)
        case _ => shouldNotReachHere()
      }
       */
    }

    def isRedundant = _isRedundant
    def isUnreachable = _isUnreachable
  }

  private lazy val jvmStateRootType = {
    val rootClass = if (rootMethod.hasReceiverParameter) {
      rootReceiverType ensuring (rootDeclaringClass isAssignableFrom _)
    } else {
      rootDeclaringClass
    }

    val t = new VMStateApprox()

    val withPreparation = if (rootMethod.isManaged) {
      t.withPreparation(rootClass)
    } else {
      t
    }

    if (rootMethod.isPreClinited) {
      withPreparation.withClinit(rootClass)
    } else {
      withPreparation
    }
  }

  private def clinitFilter(in: Approximation, clinitedClass: symlevel.ClassType): (Approximation, Boolean) = {
    in match {
      case vm: VMStateApprox => (vm withClinit clinitedClass, true)
      case _ => shouldNotReachHere()
    }
  }

  private def nodeTypeForContextTypes(x: Node): Approximation = x.tpe match {
    case RecordAddrType(t) => RecordType(t)
    case _ =>
      if (x.isInstanceOf[JVMState]) {
        jvmStateRootType
      } else if (ContextTypesMap.loweredTypes) {
        ContextTypesMap.getLoweredType(x)
      } else {
        nodeType(x)
      }
  }

  /** Control node which must dominate any use of `value`. It may be:
    * - value node itself (e.g. for new, invoke, catch, ...)
    * - some CFG point (e.g. entry block for method arguments)
    */
  def topCtrl(value: Node): UpperPoint = typeCacheKeyForNode(value) match {
    case _: LeafNode[_]         => entryBlock
    case x: SpinalNode          => x
    case x: BlockParamNode      => x.block

    // See [[MemoryOptimizationsSuite]] test with name "ContextTypes.topCtrl for GetMemoryOperation"
    case x: GetMemoryOperation           => ControlNode.lowest(x.inCtrl, x.inMemory)
    case x: LoadMemory.Independent       => x.inCtrl
    case x: ConstString                  => x.inCtrl
    case x: ClassObject                  => x.inCtrl
    case x: GetFlatThin                  => x.inCtrl
    case x: ConcealRef                   => x.inCtrl
    case x: FieldAddr                    => x.inCtrl
    case x: ArrayLength                  => x.inCtrl
    case x: UniversalGeneric.GetField    => x.inCtrl
    case x: UniversalGeneric.GetFieldOHM => x.inCtrl
    case x: LoadFieldSeq                 => x.inCtrl
    case x: LoadStaticFieldSeq           => x.inCtrl

    case x: ControlledNode => shouldNotReachHere("unknown context node source: " + x)

    case x: FloatingNode => x.args map topCtrl reduce ControlNode.lowest[UpperPoint]

    case x => shouldNotReachHere("unknown context node source: " + x)
  }

  sealed abstract class IfFilter {
    def point: If.Exit
    def key: Node
    def func: FilterFunc
    def name: String
  }

  object IfFilter {

    case class IfNull(point: If.Exit, key: Node, isNull: Boolean) extends IfFilter {
      def func =
        if (ContextTypesMap.loweredTypes) {
          intersectOrSubtractLoweredFunc(isNull, LoweredRefNull)
        } else {
          intersectOrSubtractFunc(isNull, RefNull)
        }
      def name = s"IfNull/$isNull"
    }

    case class IfInstanceOf(point: If.Exit, iof: InstanceOf, positive: Boolean) extends IfFilter {
      def key = iof.obj
      def func = {
        assert(!ContextTypesMap.loweredTypes)
        intersectOrSubtractFunc(positive, OpenCone(ReferenceType(asClassType(iof.targetType)), mayBeNull = false))
      }
      def name = s"IfInstanceOf/$positive"
    }

    case class IfInstanceDescriptorEquals(point: If.Exit, objDesc: InstanceDescriptorBy, desc: InstanceDescriptor, positive: Boolean) extends IfFilter {
      def key = objDesc.obj
      def func = intersectOrSubtractFunc(positive, Point(ReferenceType(desc.targetType), mayBeNull = false))
      def name = s"IfInstanceDescriptorEquals/$positive"
    }

    case class IfTauTest(point: If.Exit, key: Node, guard: Guards.Guard, info: TauInfo, guardPassed: Boolean) extends IfFilter {
      def func = {
        assert(!ContextTypesMap.loweredTypes)
        if (guardPassed) ff(guard.intersectWith) else ff(guard.subtractFrom)
      }
      def name = s"If$guard/$guardPassed"
    }

    case class IfInitialized(point: If.Exit, klass: symlevel.ClassType, positive: Boolean) extends IfFilter {
      def key = JVMState()
      def name = s"IfInitialized/$positive"
      def func = {
        if (positive) {
          // Treat as clinited because if class is initialized it is also clinited.
          clinitFilter(_, klass)
        } else {
          // Don't try to filter anything because if class is not initialized it might be clinited.
          { in => (in, false) }
        }
      }
    }

    private def intersectOrSubtractFunc(intersectExit: Boolean, t: ReferenceApprox) =
      if (intersectExit) ff(_ weakIntersect t) else ff(_ subtract t)

    private def ff(typeFunc: ReferenceApprox => (ReferenceApprox, Boolean)): FilterFunc =
      {
        case t: ReferenceApprox => typeFunc(t)
        case t => shouldNotReachHere(t)
      }

    private def intersectOrSubtractLoweredFunc(intersectExit: Boolean, t: LoweredReferenceApprox) =
      if (intersectExit) ffLowered(_ weakIntersect t) else ffLowered(_ subtract t)

    private def ffLowered(typeFunc: LoweredReferenceApprox => (LoweredReferenceApprox, Boolean)): FilterFunc =
      {
        case t: LoweredReferenceApprox => typeFunc(t)
        case t => shouldNotReachHere(t)
      }


    def unapply(exit: If.Exit) = exit.owner.selector match {
      case Cmp(cond @ (Condition.EQ | Condition.NE), l, r) =>
        val isEq = cond == Condition.EQ
        (l, r) match {
          case (obj, _: AnyNull) => Some(IfNull(exit, obj, exit.isTrue == isEq))

          // Filter function for instanceof is tricky to implement correctly on lowered types,
          // and impact of such optimization seems negligible.
          case (iof: InstanceOf, IConst(0)) if !ContextTypesMap.loweredTypes =>
            Some(IfInstanceOf(exit, iof, exit.isTrue != isEq))

          // TODO: account for isJava flag
          case (objDesc: InstanceDescriptorBy, desc: InstanceDescriptor) => Some(IfInstanceDescriptorEquals(exit, objDesc, desc, exit.isTrue == isEq))
          case (desc: InstanceDescriptor, objDesc: InstanceDescriptorBy) => Some(IfInstanceDescriptorEquals(exit, objDesc, desc, exit.isTrue == isEq))

          case _ => None
        }
      case TauTest(guard, info, obj) if !ContextTypesMap.loweredTypes => Some(IfTauTest(exit, obj, guard, info, exit.isTrue))
      case InitializedTest(klass) => Some(IfInitialized(exit, klass, exit.isTrue))
      case _ => None
    }
  }


  final class TauSwitchFilter(val point: TauSwitch.Exit) {
    def tauSwitch = point.owner
    def key = tauSwitch.selector
    def func = (ct: Approximation) => ct match {
      case inType: ReferenceApprox =>
        if (point.isDefault) {
          point.owner.cases.foldLeft((inType, true)) { case ((t, s), g) =>
            val (tn, sn) = g.subtractFrom(t)
            (tn, s && sn)
          }
        } else {
          point.caseValue.intersectWith(inType)
        }
      case _ => shouldNotReachHere(ct)
    }
    def name = s"TauSwitch(${point.caseOption map (_.toString) getOrElse "default"})"
  }

  object TauSwitchFilter {
    def unapply(tse: TauSwitch.Exit): Option[TauSwitchFilter] = Option.unless(ContextTypesMap.loweredTypes)(new TauSwitchFilter(tse))
  }


  object ContextTypesStats {
    private lazy val enabled = stats.isEnabled(StatsKind.ContextTypes)
    private lazy val tauTestLogEnabled = TauTest.log.isEnabled

    def updateOnRedundantFilterRemove(f: TypeFilter): Unit = {
      if (enabled) {
        val name = f.point match {
          case IfFilter(iff) => iff.name
          case TauSwitchFilter(tsf) => tsf.name
          case TauSwitch.Exit(guard) => s"TauSwitch(${guard map (_.toString) getOrElse "default"})"
          case x => x.name
        }
        stats.count(StatsKind.ContextTypes, s"redundant filter $name removed", f.point)
      }

      if (tauTestLogEnabled) {
        f.point match {
          case If.Exit(ifTau @ If(tt: TauTest)) =>
            TauTest.log.inSession("context types", codeUnit) {
              TauTest.log(s"- redundant test ${tt.name} removed", ifTau)
            }
          case _ =>
        }
      }
    }
  }
}
