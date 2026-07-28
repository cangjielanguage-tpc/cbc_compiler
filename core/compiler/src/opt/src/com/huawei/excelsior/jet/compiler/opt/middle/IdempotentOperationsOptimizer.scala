/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.types.Guards.PointGuard
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.util._
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.util.ScalaCollections.{groupBy, singleton}
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.Worklist

import scala.annotation.nowarn
import scala.reflect.ClassTag

/**
 * Optimization for idempotent operations (NullChecks, ClinitChecks, ...).
 * If two idempotent operations are equals in terms of Idempotent.idem_== with control matcher
 * based on dominance relation, then we could remove latest operation.
 *
 * @author conwor
 * @author paul
 */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait IdempotentOperationsOptimizer { self: Universe =>

  val enabledIOO = true // this flag is here to mark optimizations that rely on IOO to work (introduce compile option if needed)

  def optimizeDuplicateIfs(): Boolean = {
    enum ComparisonResult {
      case EQ, NEQ
    }
    import ComparisonResult._

    def compare(high: Node, low: Node): Option[ComparisonResult] = {
        (high, low) match {
          case _ if high == low =>
            Some(EQ)

          case (_: TauTest, _: TauTest) =>
            Some(EQ)

          case (_, Not(_)) | (Not(_), _) if env.enabled(BoolOption.NegDuplicateIfs) =>
            Some(NEQ)

          case (high @ Cmp(highOp, _, _), Cmp(lowOp, _, _))
            if highOp.negate(high.keyType.isFloatingPointType) == lowOp &&
              env.enabled(BoolOption.NegDuplicateIfs) =>
            Some(NEQ)

          case _ => None
      }
    }

    def optimizeReusedCondition(ifs: collection.Seq[If]): Boolean = {
      val reachableIfs = ifs filter { _.block.reachable }
      val worklist = Worklist.from(reachableIfs sortBy { i => cfg.dominators.depth(i.block) }) // less control-constrained nodes come first
      val replaced = Maps[If].newQMap[Node]

      for {
        high <- worklist.drain
        low <- worklist.snapshot
        res <- compare(high.selector, low.selector)
      } {
        if (high.trueExit dominates low) {
          replaced(low) = res match {
            case EQ => True()
            case NEQ => False()
          }
          worklist -= low
        } else if (high.falseExit dominates low) {
          replaced(low) = res match {
            case EQ => False()
            case NEQ => True()
          }
          worklist -= low
        }
      }

      for ((n, condition) <- replaced) {
        n.selector = condition
      }
      replaced.nonEmpty
    }

    def selectorKey(n: Node): Any = n match {
      case test: TauTest => test.key
      case Cmp(_, l, r) => (l, r)
      case Not(s) => selectorKey(s)
      case _ => n
    }

    // TODO: when check edges are done, `groupBy (_.selector)` should be enough
    var wasChanged = false
    for ((_, ifs) <- groupBy(all[If])(x => selectorKey(x.selector)) if ifs.size > 1) {
      wasChanged |= optimizeReusedCondition(ifs)
    }
    wasChanged
  }

  /** Diamond dust optimization (JET-9338).
    *
    * Sometimes profile guided devirtualization can narrow down the type of object to a Type Point.
    * In IR it is represented as a TauTest node with PointGuard and with given object.
    * In this case we can transform other non-point tests into point ones using this refined type information.
    */
  def optimizeDiamondDust(): Boolean = {
    if (!env.enabled(BoolOption.DiamondDust)) {
      return false
    }

    TauTest.log.inSession("diamond dust", codeUnit) {

      def optimizeToPoint(tests: collection.Seq[TauTest]): Boolean = {
        val (points, nonPoints) = tests partition (_.guard.isInstanceOf[PointGuard])
        if (points.isEmpty) return false

        def mayBeSafelyReplacedBy(x: TauTest, p: TauTest) = {
          val pTpe = p.guard.asInstanceOf[PointGuard].typeAppr
          x.guard.intersectWith(pTpe) == (pTpe, true)
        }

        var wasChanged = false
        for {
          x <- nonPoints
          TauTest(g: PointGuard, _, obj) <- points find (p => mayBeSafelyReplacedBy(x, p))
        } {
          assert(obj == x.obj)
          val replacement = withPos(x) { TauTest(g, TauInfo.Unknown, x.inCtrl, obj) }
          TauTest.log(s"- replaced ${x.name} -> ${replacement.name}", x)
          replaceTransitively(x, replacement)
          wasChanged = true
        }
        wasChanged
      }

      var wasChanged = false
      for ((_, tests) <- groupBy(all[TauTest].filter(_.canBeUsedInDiamondDust))(_.obj) if tests.size > 1) {
        wasChanged |= optimizeToPoint(tests)
      }
      wasChanged
    }
  }

  def optimizeConsecutiveMemBarriers(): Boolean = {
    var wasChanged = false
    val worklist = Worklist.from(all[MemBarrier])
    for (upper <- worklist.drain;
         lower @ (_x: MemBarrier) <- singleton(upper.memoryUses))
    {
      val merged = replaceByCode(upper) { MemBarrier(upper.kinds | lower.kinds)() }
      strikeOut(lower)
      worklist -= lower
      worklist += merged
      wasChanged |= true
    }
    wasChanged
  }

  def optimizeIdempotentOperations(useFilter: Boolean = true): Boolean = {

    /** Map from node to its idempotent dominator. */
    val idomMap = Maps[Idempotent].newQMap[Idempotent]

    val worklist = Worklist.from(all[Idempotent] filter (n => !useFilter || IdempotentOperationsOptimizer.shouldOptimize(n)))
    for (n <- worklist.track) {

      val idempotentNodes = Sets[Idempotent].newQSet(potentialIdempotents(n, Some(worklist)))
      worklist --= idempotentNodes
      val mutuallyNonDominated = Sets[Idempotent].newQSet

      // We want to find subset of all idempotent nodes that are mutually non-dominated (MND set).
      // We consider each node and if it is dominated by one of already found MND nodes, then
      // it is not included in this set. Otherwise, we exclude from MND set all nodes that are
      // dominated by this node and include it in MND set.
      for (idem <- idempotentNodes) {
        mutuallyNonDominated.find { x => x idempotents idem } match {
          case Some(x) =>
            idomMap(idem) = x

          case None =>
            val dominated = mutuallyNonDominated filter { x => idem idempotents x }
            dominated foreach { x => idomMap(x) = idem }
            mutuallyNonDominated --= dominated
            mutuallyNonDominated += idem
        }
      }

      // TODO: optimize mutually non dominated nodes with such pattern:
      // if two or more nodes have point in program, which dominates all of them and
      // they together post dominates this point and
      // there are no control nodes between this point and each of this nodes,
      // then move one of them in this point and eliminate other.

      // TODO: optimize mutually non dominated nodes with such pattern:
      // if one node have group of nodes dominate it together, then eliminate this node.
    }

    bulkReplace {
      for ((n, dom) <- idomMap) {
        IdempotentOperationsOptimizer.log(n)
        strikeOutWithValueUses(n, dom)
      }
    }
    idomMap.nonEmpty
  }

  private def potentialIdempotents(n: Idempotent, hint: Option[Worklist[Idempotent]]) = {

    def byKey[N <: Idempotent : ClassTag](key: Node) = {
      val candidates = collect[N](key.uses)
      hint match {
        case Some(worklist) => candidates filter worklist.contains
        case None => candidates
      }
    }

    def byAny[N <: Idempotent : ClassTag](n: N, p: N => Any) = {
      val pn = p(n)
      val candidates = hint match {
        case Some(worklist) => worklist.iterator
        case None => allNodes
      }
      candidates collect { case x: N if p(x) == pn => x }
    }

    n match {
      // common Java operations
      case n: Clinit            => byAny [Clinit]           (n, _.klass)
      case n: NullCheck         => byKey [NullCheck]        (n.obj)
      case n: CheckCast         => byKey [CheckCast]        (n.obj)
      case n: ArrayStoreCheck   => byKey [ArrayStoreCheck]  (n.array)
      case n: ArrayIndexCheck   => byKey [ArrayIndexCheck]  (n.array)

      // common AJ operations
      case n: ThinNullCheck     => byKey [ThinNullCheck]    (n.obj)
      case n: ThinCheckCast     => byKey [ThinCheckCast]    (n.obj)
      case n: GetFlatThinCheck  => byKey [GetFlatThinCheck] (n.base)

      // common Cangjie operations
      case n: PackageInit       => byAny [PackageInit]      (n, _.klass)
      case n: PackageInitCheck  => byAny [PackageInitCheck] (n, _.klass)

      // rare Java operations
      case n: DivisorCheck      => byKey [DivisorCheck]     (n.divisor)

      case _ => shouldNotReachHere(s"unexpected Idempotent: $n")
    }
  }

  object IdempotentOperationsOptimizer {

    def log(n: Idempotent): Unit = {
      stats.count(StatsKind.IdempotentOperations, s"idempotent operation ${n.name} removed", n)
    }

    def shouldOptimize(n: Idempotent): Boolean = n match {
      case _: TypeFilterNode => false // optimized by ContextTypes
      case _: DivisorCheck | _: GetFlatThinCheck | _: PackageInit | _: PackageInitCheck => true
      case _ => shouldNotReachHere(s"unexpected Idempotent node: $n")
    }

    /** Checks that nodes ignored above are completely optimized by corresponding optimization. */
    def checkCoverage(): Unit = {
      val changed = optimizeIdempotentOperations(useFilter = false)
      assert(!changed)
    }

    /** Try to find node (idempotent dominator) which idempotents `n`:
      *  they should be structurally equal except of control argument,
      *  idempotent dominator should strictly dominate `n` by control.
      */
    def findIdempotentDominator(n: Idempotent): Option[Idempotent] =
      potentialIdempotents(n, None) find { _ idempotents n }
  }

}
