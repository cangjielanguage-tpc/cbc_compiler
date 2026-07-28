/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.{Stats, StatsKind}
import com.huawei.excelsior.jet.compiler.bytecode.{NoPosition, Position}
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.{CountedLoopsRecognizer, ValueRangeAnalysis}
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.*
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.ScalaCollections.*
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind, Loops}
import xscala.util.MathUtils

import scala.PartialFunction.cond
import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer

/** Loop unrolling optimization.
  *
  * @author liontiger
  */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait LoopUnrolling extends XiTransform with CountedLoopsRecognizer with ValueRangeAnalysis { self: Universe =>

  /** Unrolls loops.
    *
    * Supported unrolling candidates:
    * - Scalar loops -- for better specialization (e.g. Caffeine.Logic);
    * - Array access loops -- for potential [[com.huawei.excelsior.jet.compiler.opt.backend.preparation.RMACombining]] (e.g. Caffeine.Loop).
    *
    * @return `true` if IR was modified, `false` otherwise.
    */
  def unrollLoops(collectFailStats: Boolean = false): Boolean = {
    if (!XiTransform.enabled(LoopUnrolling)) {
      return false
    }

    val loops = cfg.loops
    if (loops.isEmpty) {
      return false
    }
    val countedLoops = detectCountedLoops(loops).toSet

    withIncrementalGCM {
      XiTransform.log.inSession("loop unrolling", codeUnit) {

        def potentialCandidate(loop: Loop[Block]): Option[UnrollingCandidate] = {
          val loopNodes = loop.body flatMap Block.collectNodes

          // TODO: unify with scalar inline predicate (see JET-12142)
          def scalar(n: Node): Boolean = n match {
            case _: BlockEnd => true
            case n: PureCheck => n.trusted
            case _: HasInMemory => false
            case _ => true
          }

          def good(n: Node): Boolean = n match {
            case _: ArrayGet | _: ArrayPut => true // good for RMA combining
            case _: GetField => true // needed for Caffeine.Loop unrolling (without +MoveLoadsOutOfLoops)
            case _: DivisorCheck => true // needed for Caffeine.Sieve unrolling
            case _ => scalar(n)
          }

          if (!rootMethod.isJCAUnrollLoops && !(loopNodes forall good)) {
            return None
          }

          val infos = findInductiveVariables(loop).toSeq flatMap calcUnrollingInfo
          if (infos.isEmpty) {
            return None
          }

          // If the loop contains array operations, then unrolled step is chosen such that
          // all array accesses of a single unrolled iteration will fully cover one or more machine words
          // (e.g. loop with char array access will have unrolled step 4 on 64-bit platform to cover an 8-byte word).
          val arrayWordSize = env.valueOf(ArrayLoopUnrollingWordNum) * AddrType.size
          val arrayElemSizes = loopNodes.iterator collect {
            case n: AnyMemoryAccess with ArrayElementOperation => n.accessType.sizeInBytes
          }

          val arrayUnrolledStep = arrayElemSizes.minOption match {
            case Some(minElemSize) if arrayWordSize > minElemSize => (arrayWordSize / minElemSize) ensuring (_ > 1)
            case _ => 0
          }

          val unrolledStep = Math.max(arrayUnrolledStep, env.valueOf(LoopUnrollingStep))

          Some(UnrollingCandidate(loop, unrolledStep, countedLoops contains loop, loopNodes forall scalar, infos))
        }

        val candidates = loops.iterator flatMap potentialCandidate
        if (candidates.nonEmpty) {
          unrollLoopCandidates(candidates, collectFailStats)
        } else {
          false
        }
      }
    }
  }

  /** Unrolls innermost of given `candidates` if possible.
    *
    * @return `true` if IR was modified, `false` otherwise.
    */
  private def unrollLoopCandidates(candidates: IterableOnce[UnrollingCandidate], collectFailStats: Boolean = false): Boolean = {

    //////////////////////////////////////////////////////////////////////////
    // 0. Filtering and logging.
    //
    // Some candidates cannot be correctly unrolled by current unrolling engine,
    // so such candidates must be filtered and logged before attempting to unroll anything.
    //
    //////////////////////////////////////////////////////////////////////////

    /** Filters candidates with valid reasons for not unrolling (not logged in fail stats). */
    def noUnrolling(candidate: UnrollingCandidate) = {
      candidate.loop.header.unreachable || (candidate.loop.header.spine exists (_.isInstanceOf[NoLoopUnrollingMarker]))
    }

    /** Filters candidates which can be unrolled (failing this predicate will log in fail stats). */
    def unrollable(candidate: UnrollingCandidate): Boolean = withIncrementalGCM {
      val loop = candidate.loop
      def loopInvariant(n: Node) = n.block strictDominates loop.header

      /** Ensure possibility of compensation loop creation. */
      def uniqueInductivePostExit = getLoopPostExit(loop, candidate.isInductiveExit).nonEmpty

      /** Ensure proper compensation loop creation. */
      def noSideEffectsBeforeInductiveExits = {
        val blocks = Closure(loopExitEdges(loop) filter candidate.isInductiveExit map (_.source.block)) { b =>
          if (loop.header == b) Seq() else b.predBlocks
        }
        blocks forall (_.spine forall SpinalNode.sideEffectFree)
      }

      /** Ensure proper number of iterations and unrolled limit calculation. */
      def invariantBounds = candidate.infos forall (info => loopInvariant(info.iv.start) && loopInvariant(info.iv.limit))

      /** Ensure proper predication and simpler predicate.
        * Otherwise, if predicate contains multiple conjuncts, the corresponding exit on failed predicate branch
        * cannot be converted to a failing one (and then it would be versioned, not predicated).
        */
      def noMoreThanOneNonStrictInfo = candidate.infos.count(!_.strict) <= 1

      /** Ensure unrolled loop correctness.
        * This optimization cannot correctly fully unroll loop, because it eliminates unrolled exits,
        * which must be preserved for full unrolling (see FullLoopUnrolling).
        */
      def noFullUnroll = candidate.infos forall (_.range match {
        case range: ConstValueRange => MathUtils.ugeq(range.size, candidate.unrolledStep)
        case _ => true
      })

      uniqueInductivePostExit && noSideEffectsBeforeInductiveExits && invariantBounds && noMoreThanOneNonStrictInfo && noFullUnroll
    }

    val (unrollableCandidates, failedCandidates) = candidates.iterator filterNot noUnrolling partition unrollable

    if (collectFailStats) {
      failedCandidates foreach (_.logFail())
    }

    if (unrollableCandidates.isEmpty) {
      return false
    }

    // Collect only innermost candidates to guarantee that they do not overlap (needed for the following steps).
    // Note that other candidates will still be optimized on the following optimization iterations.
    val innermostCandidates = minimalElements(unrollableCandidates)(partialOrderingBy(_.loop isInnerOf _.loop))
    if (innermostCandidates.isEmpty) {
      return false
    }

    innermostCandidates foreach (_.logSuccess())

    //////////////////////////////////////////////////////////////////////////
    // 1. Preparation.
    //
    // Previously collected UnrollingInfo will be invalidated in the following steps.
    // So here we must perform all necessary IR preparation and collect all additional info
    // that relies on still valid UnrollingInfo.
    //
    //////////////////////////////////////////////////////////////////////////

    class UnrollingTarget(candidate: UnrollingCandidate) {

      // Since loop and infos will be invalidated soon, we don't want to make them public.
      private val UnrollingCandidate(loop, _, _, _, infos) = candidate

      val UnrollingCandidate(_, unrolledStep, counted, scalar, _) = candidate
      val header = loop.header

      // Note: since we unroll only the innermost loops per optimization, we can safely create pre-header and post-exit
      //       without corrupting other unrolled loops infos.
      private val (preHeader, _) = getOrCreateLoopPreHeader(loop)
      private val (inductivePostExit, _) = getOrCreateExclusiveLoopPostExit(loop, candidate.isInductiveExit).get

      // Compensation loop will immediately follow inductive post-exit of original loop.
      // Note that non-inductive exits should not go to compensation loop (see JET-12149).
      val compensationLoopEntry = Block.splitAfter(inductivePostExit)

      private val startValueReads = ArrayBuffer.empty[ReadVar]

      {
        // In order to continue iteration with the unrolled variables in compensation loop,
        // we must ensure that start value assignment won't be copied to compensation loop.
        // So we manually break def-use edges between start values and phies before actual xiTransform,
        // while keeping inserted reads as anchors, which will be reused in compensation loop.

        val enterEdge = singleElement(loopEnterEdges(loop))
        val assignPoint = preHeader.blockEnd.inCtrl
        for (phi <- header.phies) {
          withNewVar(phi.tpe) { (assignAt, readAt) =>
            val startValue = phi.phiArg(enterEdge)
            assignAt(assignPoint, startValue)
            val read = readAt(preHeader.blockEnd.inCtrl)
            startValue.replaceUses { case e if e.isValue && e.target == phi => read }
            startValueReads += read
            assignAt(header, phi)
          }
        }
      }

      // These anchors will be used for both predication and compensation loop creation steps.
      // Note: in order to properly create compensation using xiTransform copying mechanism,
      //       we must include both startValueReads and compensationLoopEntry as anchors.
      val anchors = startValueReads.toSeq ++ Seq(preHeader.blockEnd, header.outCtrl, compensationLoopEntry)

      // Strict predicate is the one that guarantees strictness of all value ranges.
      // Note: it is ok to leave predicate as-is (without their materialization into nodes),
      //       because predication step is the first xiTransform one,
      //       so the IR will still be valid up to predicate's first and only use.
      // Note: currently only a single non-strict value range is allowed to avoid bloating the predicate.
      val strictPredicate = infos collectFirst {
        case UnrollingInfo(iv, range, strict) if !strict =>
          val pred = range match {
            case _: HalfSymbolicValueRange | _: SymbolicValueRange =>
              val (from, to) = ValueRange.bounds(range)
              PredicateConstructor.atom(Cmp(range.tpe, Condition.LE)(from, to))

            case _: ConstValueRange | _: EmptyValueRange => shouldNotReachHere(s"non-strict range: $range")
          }
          (iv.continueEdge, pred)
      }

      // These limit tests will replace original ones in the unrolled loop.
      // Note: the tests will have no uses (dead code) until unrolling step, which will replace the original limit tests
      //       with these new ones.
      val limitTests = Maps[If.Exit].newQMap[Node]

      // Current ValueRangeAnalysis cannot re-calculate value ranges of inductive variables after loop unrolling.
      // So we insert these helper value range filters to preserve roughly the same ranges after unrolling.
      // Note that any copies of these filters must be eliminated after each xiTransform.
      private val indexFilters = ArrayBuffer.empty[RawValueRangeFilter]
      def eliminateCopiedIndexFilters(xi: XiResult): Unit = {
        for (marker <- indexFilters; copy <- xi.copiesOf(marker)) {
          strikeOut(copy)
        }
      }

      for (UnrollingInfo(iv, range, _) <- infos) {
        val unrolledRange = calcUnrolledValueRange(range, iv.step, unrolledStep, iv.continueEdge.owner.block)
        val (from, to) = ValueRange.bounds(unrolledRange)

        // In order to correctly insert filter, we need to split critical continue edge if it is such.
        splitCriticalEdge(iv.continueEdge.outEdge) foreach (Loops.addToBody(loop, _))

        indexFilters += insertCodeAfter(iv.continueEdge.target)(RawValueRangeFilter(iv.index, from, to))

        val limitTest = {
          val cmp = calcLimitTest(iv.index, from, to, iv.step > 0)
          if (iv.continueEdge.isTrue) cmp else Not(cmp)
        }

        limitTests(iv.continueEdge) = limitTest
      }

    }

    val targets = innermostCandidates map (new UnrollingTarget(_))

    //////////////////////////////////////////////////////////////////////////
    // 2. Predication.
    //
    // Loops with non-strict value ranges, must be predicated to guarantee their strictness.
    // The corresponding non-strict limit tests will be replaced by failing ones on failed predication branch,
    // because the loop on failed branch will not iterate at all (since it failed strict predicate).
    //
    // Note: currently only a single non-strict value range is allowed to avoid bloating the predicate.
    //
    //////////////////////////////////////////////////////////////////////////

    if (targets exists (_.strictPredicate.nonEmpty)) {
      xiTransformAndPostProcess { scheduler =>
        for (target <- targets; (_, pred) <- target.strictPredicate) {
          // Note: the predicate is negated to preserve original loop (with original anchors and markers) as true version
          //       (see XiScheduler.version).
          scheduler.version(!pred, target.anchors: _*)
        }
      } { (xi, _) =>
        for (target <- targets; (continueEdge, _) <- target.strictPredicate) {
          target.eliminateCopiedIndexFilters(xi)
          // Copied loop will not iterate at all.
          // So copied non-strict limit test will always fail and can be safely replaced by failing one.
          replaceByGoto(xi.copyOf(continueEdge).otherExit)
        }
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // 3. Compensation loop creation.
    //
    // If the number of iterations of the loop was not divisible by unrolled step,
    // then the remainder of iterations must be performed by a non-unrolled compensation loop
    // immediately following the unrolled one.
    // The compensation loop continues iteration with the same inductive variable from where the unrolled loop left off.
    //
    //////////////////////////////////////////////////////////////////////////

    // Counted compensation loops with unrolled step 2 will run zero or one iteration at most,
    // so such loops can be fully unrolled and effectively flattened into a single compensation branch.
    // Note: this flattening may not always lead to better performance, so it must be enabled with caution!
    val compensationHeadersToUnroll = Maps[Block].newQMap[Seq[If.Exit]]
    def fullyUnrollCompensationLoop(target: UnrollingTarget): Boolean = {
      (env.enabled(UnrollCompensationLoop) || (env.enabled(UnrollScalarCompensationLoop) && target.scalar)) &&
        target.unrolledStep == 2 &&
        target.counted
    }

    xiTransformAndPostProcess { scheduler =>
      for (target <- targets) {
        scheduler.unsafe.copy(target.compensationLoopEntry.targetEdge, target.anchors: _*)
      }
    } { (xi, _) =>
      for (target <- targets) {
        target.eliminateCopiedIndexFilters(xi)

        val compensationHeader = xi.copyOf(target.header)
        if (fullyUnrollCompensationLoop(target)) {
          // Collect compensation headers and continue edges for subsequent full unrolling.
          compensationHeadersToUnroll(compensationHeader) = target.limitTests.keysIterator.map(xi.copyOf).toSeq

        } else {
          if (target.counted) {
            // If the original loop was counted, then compensation loop remains counted as well.
            insertCodeAfter(compensationHeader)(CountedLoopMarker())
          }
          // The unrolled loop will not be unrolled twice because it will have step more than 1.
          // But compensation loop still has step 1 and should be marked to ensure that it won't be unrolled further.
          insertCodeAfter(compensationHeader)(NoLoopUnrollingMarker())
        }
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // 4. Unrolling.
    //
    // Original loops are unrolled with adjusted new limit.
    // Collected compensation loops are also fully unrolled at the same time.
    //
    //////////////////////////////////////////////////////////////////////////

    // Note that loops must be recollected again because xiTransform significantly changes CFG.
    val loops = cfg.loops

    xiTransformAndPostProcess { scheduler =>
      // Unroll original loops.
      for (target <- targets) {
        val loop = loops loopOf target.header
        scheduler.unroll(loop, Math.toIntExact(target.unrolledStep) - 1)
      }
      // Fully unroll collected compensation loops.
      for (header <- compensationHeadersToUnroll.keys) {
        val loop = loops loopOf header
        scheduler.unroll(loop, 1)
      }
    } { (xi, _) =>
      // Post-process original loops.
      for (target <- targets) {
        target.eliminateCopiedIndexFilters(xi)

        // Unrolled loop is always counted!
        // Because in order to unroll the loop we must first calculate its number of iteration,
        // which is either finite positive number or guaranteed to be such via predication.
        insertCodeAfter(target.header)(CountedLoopMarker())

        for ((continueEdge, limitTest) <- target.limitTests) {
          // All copied limit tests a replaced by passing ones,
          // so that there is only a single remaining limit test per unrolled iteration.
          xi.copiesOf(continueEdge) foreach replaceByGoto

          // That remaining limit test is updated with adjusted limit for the unrolled loop.
          val branch = continueEdge.owner
          branch.selector = limitTest
        }
      }
      // Post-process collected compensation loops in order to fully unroll them into a single branch.
      for ((_, continueEdges) <- compensationHeadersToUnroll; continueEdge <- continueEdges) {
        // The second iteration's limit test will always fail, so we can safely replace it by failing one.
        replaceByGoto(xi.copyOf(continueEdge).otherExit)
      }
    }

    //////////////////////////////////////////////////////////////////////////

    true
  }

  /** Represents potential loop candidate for unrolling. */
  case class UnrollingCandidate(loop: Loop[Block], unrolledStep: Long, counted: Boolean, scalar: Boolean, infos: Seq[UnrollingInfo]) {
    require(loop.kind != LoopKind.IRREDUCIBLE)
    require(unrolledStep > 1)
    require(infos.nonEmpty)

    def isInductiveExit(e: Edge): Boolean = cond(e) {
      case ControlEdge(exit, _) if infos exists (_.iv.continueEdge.otherExit == exit) => true
    }

    def logFail(): Unit = log(loop, success = false)

    def logSuccess(): Unit = log(loop, success = true)

    private def log(loop: Loop[Block], success: Boolean): Unit = {
      val unrolled = if (success) "unrolled" else "not unrolled"
      stats.count(StatsKind.XiTransformations, s"loops $unrolled", loop.header)
      if (XiTransform.log.isEnabled) {
        XiTransform.log(s"- loop $unrolled", loop.header)
        XiTransform.log(s"  with step = $unrolledStep [counted = $counted, scalar = $scalar]")
        for (UnrollingInfo(iv, range, strict) <- infos) {
          XiTransform.log(s"  with info:")
          XiTransform.log(s"    inductive variable = ${iv.description}")
          XiTransform.log(s"    range = ${rangeStr(range)}", rangePos(range))
        }
      }
    }

    private def rangeStr(range: ValueRange): String = {
      def str(n: Node, addend: Long) = {
        val nodeStr = "<" + n.simpleName + ">"
        val addendStr = addend.sign match {
          case  0 => ""
          case  1 => s"+$addend"
          case -1 => s"-${-addend}"
        }
        nodeStr + addendStr
      }

      range match {
        case _: EmptyValueRange =>
          "[]"

        case ConstValueRange(_, from, to, _) =>
          s"[$from, $to]"

        case HalfSymbolicValueRange(_, from, to, toAddend, _) =>
          s"[$from, ${str(to, toAddend)}]"

        case SymbolicValueRange(_, from, fromAddend, to, toAddend, _) =>
          s"[${str(from, fromAddend)}, ${str(to, toAddend)}]"
      }
    }

    private def rangePos(range: ValueRange): Position =
      if (range.evidence == null) NoPosition else range.evidence.pos
  }

  /** Calculates unrolling info for given inductive variable if possible. */
  def calcUnrollingInfo(iv: InductiveVariable): Option[UnrollingInfo] = {
    if (iv.step.abs == 1) {
      calcValueRangeOfInductiveVariable(iv) collect {
        case r: ConstValueRange => UnrollingInfo(iv, r, strict = true)
        case r: HalfSymbolicValueRange if iv.cond != Condition.NE => UnrollingInfo(iv, r, strict = false)
        case r: SymbolicValueRange if iv.cond != Condition.NE => UnrollingInfo(iv, r, strict = false)
      }
    } else {
      None
    }
  }

  /** Encapsulates basic information for unrolling of the loop containing given inductive variable.
    *
    * Note: `strict` flag indicates that `range` is guaranteed to be non-empty
    *       (e.g. strict `[from, to]` range means that `from <= to`).
    */
  case class UnrollingInfo(iv: InductiveVariable, range: ValueRange, strict: Boolean) {
    require(iv.step.abs == 1)
    require(strict || iv.cond != Condition.NE, "cannot create strict predicate for variable with NE condition")
  }

  /** Calculates value range of the unrolled loop with given `range` of inductive variable. */
  def calcUnrolledValueRange(range: ValueRange, step: Long, unrolledStep: Long, inCtrl: UpperPoint): ValueRange = {
    require(step.abs == 1)
    require(unrolledStep > 1)

    lazy val compensationIterNumConst: Long = {
      val iterNum = range.asInstanceOf[ConstValueRange].size
      MathUtils.urem(iterNum, unrolledStep)
    }

    def compensationIterNum: Node = {
      URem(range.tpe)(inCtrl, ValueRange.size(range), IntegralConst(range.tpe)(unrolledStep))
    }

    range match {
      case ConstValueRange(tpe, from, to, evidence) if to - from >= compensationIterNumConst =>
        if (step > 0) {
          ConstValueRange(tpe, from, to - compensationIterNumConst, evidence)
        } else {
          ConstValueRange(tpe, from + compensationIterNumConst, to, evidence)
        }

      case HalfSymbolicValueRange(tpe, from, to, toAddend, evidence) =>
        if (step > 0) {
          HalfSymbolicValueRange(tpe, from, Sub(to, compensationIterNum), toAddend, evidence)
        } else {
          SymbolicValueRange(tpe, compensationIterNum, from, to, toAddend, evidence)
        }

      case SymbolicValueRange(tpe, from, fromAddend, to, toAddend, evidence) =>
        if (step > 0) {
          SymbolicValueRange(tpe, from, fromAddend, Sub(to, compensationIterNum), toAddend, evidence)
        } else {
          SymbolicValueRange(tpe, Add(from, compensationIterNum), fromAddend, to, toAddend, evidence)
        }

      case _ => EmptyValueRange(range.tpe)
    }
  }

  /** Calculates a valid limit test for limiting given `value` in the given range `[from, to]`. */
  def calcLimitTest(value: Node, from: Node, to: Node, positiveIncrement: Boolean): Node = {
    val tpe = value.tpe
    val limit = if (positiveIncrement) {
      Add(to, IntegralConst(tpe)(1))
    } else {
      Sub(from, IntegralConst(tpe)(1))
    }
    Cmp(tpe, Condition.NE)(value, limit)
  }
}
