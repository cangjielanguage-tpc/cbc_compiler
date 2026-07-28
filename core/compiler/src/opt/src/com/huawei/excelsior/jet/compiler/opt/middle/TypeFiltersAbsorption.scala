/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.types.Guards.{Guard, OpenConeGuard}
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.XiTransform
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ClassType, InterfaceType, ReferenceType}
import com.huawei.excelsior.jet.compiler.types.References.{OpenCone, ReferenceApprox, UpperBounded}
import com.huawei.excelsior.jet.compiler.types.{Approximation, CHA}
import com.huawei.excelsior.jet.util.ScalaCollections.groupBy
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.graph.Loops

import scala.PartialFunction.condOpt
import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer

/** Main idea of this optimization: version code with type filters to eliminate them in versioned true-path.
  * Note that this makes sense only in case of two or more filters.
  *
  * The tricky part is to select a type test (''versioning test'') such that filters on true-path become redundant
  * and may be eliminated as always passing.
  *
  *
  * Currently we analyze situation with only two dominating type filters (top and bottom) such that
  * the bottom filter filters strictly more precise type than the top one (e.g. `CheckCast(CharSequence)` & `CheckCast(String)`).
  * In this situation test corresponding to the bottom filter is a suitable versioning test.
  * We say that bottom filter ''absorbs'' top filter
  * (see [[com.huawei.excelsior.jet.compiler.opt.middle.TypeFiltersAbsorption#isAbsorbable isAbsorbable]]).
  *
  * Notice that our filters always filter out null values to unify absorption
  * because some filters reject null values (i.e. InstanceOf), some allow them (i.e. CheckCast)
  * and some assume that input is always non-null (i.e. TauTest).
  * It means that versioning test must reject null values.
  *
  * Also note that in such situation the top filter on false-path remains unchanged
  * but the bottom one becomes...
  * - completely redundant and may be eliminated as always failing if filter rejects null values,
  * - partially redundant and may be replaced by failing one under non-null test if filter allows null values.
  */
trait TypeFiltersAbsorption extends XiTransform { this: Universe =>

  private sealed abstract class Filter {
    def name: String

    def allowNullValues: Boolean

    /** Identification node, bottom point of this filter. */
    def exit: ControlNode

    /** Top point of this filter. */
    def point: LowerPoint

    def pos: Position

    def obj: Node

    def filterFunc: FilterFunc

    def versioningPredicate: PredicateConstructor

    // These actions work after actual versioning and should use original nodes with care.
    def convertToPassing(xi: XiResult): Unit
    def convertToFailing(): Unit
  }

  private abstract class CheckFilter extends Filter {
    override def exit: ThrowingPureCheck
    override def point: ThrowingPureCheck = exit
    override def pos = point.pos

    def convertToPassing(xi: XiResult): Unit =
      strikeOut(xi.copyOf(exit))
  }

  private abstract class TestFilter extends Filter {
    override def exit: If.Exit
    override def point: If = exit.owner
    override def pos = point.selector.pos

    def convertToPassing(xi: XiResult): Unit =
      replaceByGoto(xi.copyOf(exit))
    override def convertToFailing(): Unit =
      replaceByGoto(exit.otherExit)
  }

  private object Filter {
    import PredicateConstructor._
    def create(filter: ControlNode, objNode: Node): Option[Filter] = filter match {

      // TODO: do not use IfFilter explicitly, somehow expose this information in TypeFilter itself
      case exitNode @ IfFilter(ifFilter) => condOpt(ifFilter) {
        case IfFilter.IfInstanceOf(_, InstanceOf(targetType, plainObjNode @ EOPConvert.Skipped(`objNode`)), true) =>
          new TestFilter {
            override def name = "InstanceOfTest"
            override def allowNullValues = false
            override def exit = exitNode
            override def obj = plainObjNode
            override def filterFunc = t =>
              OpenCone(ReferenceType(asClassType(targetType)), mayBeNull = false) weakIntersect t.asInstanceOf[ReferenceApprox]
            override def versioningPredicate = instanceOf(obj, targetType)
          }

        case IfFilter.IfTauTest(_, plainObjNode @ EOPConvert.Skipped(`objNode`), guard, info, true) =>
          new TestFilter {
            override def name = "TauTest"
            override def allowNullValues = false
            override def exit = exitNode
            override def obj = plainObjNode
            override def filterFunc = t =>
              guard intersectWith t.asInstanceOf[ReferenceApprox]
            override def versioningPredicate = tauTest(guard, info, obj)
          }
      }

      // Note that we ignore trusted casts.
      // It's a bad idea to replace two trusted casts (generated as nop) by a single instanceof.
      // It may be a good idea to handle pairs like interface instanceof and trusted class cast,
      // implement if you need it.
      case check @ CheckCast(castedType, plainObjNode @ EOPConvert.Skipped(`objNode`)) if !check.trusted => Some(
        new CheckFilter {
          override def name = "CheckCast"
          override def allowNullValues = true
          override def exit: CheckCast = check
          override def obj = plainObjNode
          override def filterFunc = t =>
            OpenCone(ReferenceType(asClassType(castedType)), mayBeNull = true) weakIntersect t.asInstanceOf[ReferenceApprox]
          override def versioningPredicate = instanceOf(obj, castedType)
          override def convertToFailing(): Unit =
            replaceCheckCastByThrowIfNonNull(exit)
        })

      // Note that we don't analyze NullCheck and NonNullTest.
      // It's useless to absorb them because they could be absorbed only by another NonNullTest.
      // And NonNullTest cannot be versioning test because it only eliminates another NonNullTests.
      //
      // Note that we don't analyze NullTest which can absorb only null-passing filters (e.g. CheckCast).
      // It's useless to absorb another null-passing filters by NullTest
      // because such filters will eventually be generated into null-test
      // and absorption will eliminate only one extra null-test (not so great optimization).
      case _ => None
    }
  }

  def absorbTypeFilters(collectFailStats: Boolean = false): Boolean = {
    if (ContextTypesMap.loweredTypes) {
      return false
    }

    // We limit the scope of optimization to consecutive filters located in the same block
    // (or in consecutive blocks in case of top test filter).
    // Also we ignore filters in different loops to prevent too much versioning (i.e. when bot is in loop)
    // and to prevent execution of extra tests (i.e. when top is in loop).

    lazy val loops = cfg.loops

    def closeEnough(top: Filter, bot: Filter) = {
      (top match {
        case top: CheckFilter => areCloseEnough(top.exit, bot.exit)
        case top: TestFilter => areCloseEnough(top.exit, bot.exit, loops)
      }): @nowarn("msg=unreachable code") // for some reason scalac fails to properly analyse Filter hierarchy
    }

    def suitableTypes(n: Node, top: Filter, bot: Filter) =
      isAbsorbable(nodeTypeAt(n, top.point), top.filterFunc, bot.filterFunc)

    def suitablePair(n: Node, top: Filter, bot: Filter) =
      closeEnough(top, bot) && suitableTypes(n, top, bot)


    def allPairs =
      for {
        (n, chains) <- ContextTypesMap.allTypeFilterChains
        if n.tpe.isStructureType // currently only reference nodes with TypeApproximation types are handled
        chain <- chains
        // It's important to recheck correspondence of ContextType's filter with actual IR
        // because context types might be already ruined, see JET-11234, JET-11370.
        Seq(bot, top) <- (chain flatMap { tf => Filter.create(tf.point, n) }).sliding(2)
        if top.point.block.reachable && bot.point.block.reachable
        if top.exit dominates bot.point // filter chains could be already ruined
      } yield (n, top, bot)


    if (collectFailStats) {
      // Log only distinct pairs, bot.exit is a unique identifier of a pair.
      val failedPairs = allPairs filterNot Function.tupled(suitablePair)
      val distinctPairs = groupBy(failedPairs)(_._3.exit).values.map(_.head)
      for ((n, top, bot) <- distinctPairs) {
        val results = s"close enough = ${closeEnough(top, bot)}, suitable types = ${suitableTypes(n, top, bot)}"
        stats.count(StatsKind.TypeFiltersAbsorption, s"not absorbed pair (${top.name}, ${bot.name}) ($results)", posOf(bot, top))
      }
    }


    // This iterator may have duplicates, handled below because their total count is not so big.
    val suitablePairs = for {
      (n, top, bot) <- allPairs if suitablePair(n, top, bot)
    } yield (top, bot)

    if (suitablePairs.isEmpty) {
      return false
    }

    // Actually absorbed pairs without duplicates.
    val absorbedPairs = ArrayBuffer.empty[(Filter, Filter)]

    // We are not able to version one block twice per transformation session.
    // Conflicting absorbable pairs will be optimized on the next optimization iteration.
    // Note that duplicates in pairs will be filtered out as conflicting.
    val versionedBlocks = Sets[Block].newMSet

    for ((top, bot) <- suitablePairs) {
      val subGraph = versioningSubGraph(top.point, bot.point)
      if (!(subGraph exists versionedBlocks.contains)) {
        absorbedPairs += ((top, bot))
        versionedBlocks ++= subGraph
        stats.count(StatsKind.TypeFiltersAbsorption, s"absorbed pair (${top.name}, ${bot.name})", posOf(bot, top))
      } // else this pair will be handled on next optimization iteration
    }

    assert(absorbedPairs.nonEmpty)

    // Perform versioning of non-overlapping pairs.
    xiTransformAndPostProcess { scheduler =>
      for ((top, bot) <- absorbedPairs) {
        scheduler.version(bot.versioningPredicate, top.point, bot.point)
      }
    } { (xi, _) =>
      // Clean up after versioning.
      for ((top, bot) <- absorbedPairs) {
        // These conversions might be performed by context types
        // but we want to ensure that they do happen, so we convert them manually.
        top.convertToPassing(xi)
        bot.convertToPassing(xi)

        // This conversion is unlikely to be performed by context types
        // (because of unstrict information and weak subtraction support in type system),
        // so we convert it manually.
        // Note that it also must be done to prevent consecutive optimization of the same filters on false-path.
        bot.convertToFailing()
      }
    }

    true
  }

  /** Receives an interface cast (top) and a tau-guard below (bot).
    * Returns some guard which filters the same values as given bot and which absorbs top (maybe the bot itself).
    * Returns `None` if there is no suitable guard.
    */
  def selectGuardForInterfaceCastAbsorption(top: CheckCast, bot: Guard, botPoint: SpinalNode): Option[Guard] = {
    val castType = top.targetType
    require(castType.isInterface)

    if (!areCloseEnough(top, botPoint)) {
      return None
    }

    val inType = nodeTypeAt(top.obj, top)

    def isAbsorbableBy(g: Guard) =
      isAbsorbable(inType,
        top.filterType(_, top),
        t => g.intersectWith(t.asInstanceOf[ReferenceApprox]))

    if (isAbsorbableBy(bot)) {
      return Some(bot)
    }

    // Bot does not absorb top, so we try to find a guard that absorbs it and filters the same type approximation.
    // E.g. if bot is CHABitGuard it's unlikely to absorb anything else
    // but it may be replaced by a more expensive guard with the same filtered type approximation.

    val (topOutType, topStrict) = top.filterType(inType, top)
    if (!topStrict) {
      return None
    }

    // Guard that absorbs top filter.
    val topGuard = topOutType match {
      case topOutType @ UpperBounded(root, _) =>
        assert(root implements castType)
        root match {
          case root: ClassType =>
            OpenConeGuard(root.symType)

          case _: InterfaceType if CHA.isKnownType(root) =>
            topOutType.withoutNull.filterClosed() match {
              case (UpperBounded(rootClosed: ClassType, _), true) =>
                assert(rootClosed implements castType)
                OpenConeGuard(rootClosed.symType)

              case _ => return None
            }
          case _ => return None
        }
      case _ => return None
    }
    assert(isAbsorbableBy(topGuard))

    topGuard.intersectWith(bot, inType) ensuring (_ forall isAbsorbableBy)
  }

  private def areCloseEnough(top: SpinalNode, bot: ControlNode): Boolean = {
    top.block == bot.block
  }

  private def areCloseEnough(top: Branch.Exit, bot: ControlNode, loops: => Loops[Block]): Boolean = {
    top.target == bot.block && loops.inSameLoop(top.block, bot.block)
  }

  private def isAbsorbable(inType: ReferenceApprox,
                           topFunc: Approximation => (Approximation, Boolean),
                           botFunc: Approximation => (Approximation, Boolean)): Boolean = {
    // If top(in) <= bot(in) and top(in) is strict
    // then every object from approximation (in) filtered out in top(in) is also filtered out in bot(in).

    val inTypeNonNull = inType.withoutNull

    val (topOutType, topStrict) = topFunc(inTypeNonNull)
    if (!topStrict) {
      return false
    }

    val (onlyBotOutType, _) = botFunc(inTypeNonNull)

    // We don't need to absorb if the types are equal. This case will be covered by context types
    val absorbable = topOutType > onlyBotOutType
    assert(!absorbable || (topFunc(onlyBotOutType) == (onlyBotOutType, true)),
      "top filter must be redundant under bottom one")
    absorbable
  }

  private def posOf(bot: Filter, top: Filter) =
    bot.pos orElse top.pos orElse rootMethodPos
}
