/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.LoopsNormalizer
import com.huawei.excelsior.jet.compiler.options.BoolOption.InductiveVariablesWithInductiveCmp
import com.huawei.excelsior.jet.util.ScalaCollections.groupBy
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind, Loops}
import xscala.util.MathUtils

import scala.PartialFunction.condOpt
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
  * Counted loops detection.
  * Implementation based on specification by @ikireev & @dbg,
  *   "Safepoints reducing by counted loops detection".
  *
  * Searching for loops like that:
  *
  * LOOP:
  *   index = phi(start, increment)
  *   ...
  *   increment = index +/- step
  *   ...
  *   if cond(index/increment, limit) goto LOOP
  *
  * @author dbg
  * @author ikireev
  * @author conwor
  * @author cypok
  */
trait CountedLoopsRecognizer extends ValueRangeAnalysis with LoopsNormalizer { self: Universe =>

  /** Inductive variable of a loop is a phi-function (`index`), defined in the loop header,
    * incremented in the loop body (`increment`) and used (itself or as the increment)
    * to continue loop iteration by comparing with `limit` using `cond`.
    * Loop continuation goes by `continueEdge`.
    */
  case class InductiveVariable(index: Phi, start: Node, step: Long, incrementIsCompared: Boolean, cond: Condition, limit: Node, continueEdge: If.Exit) {
    assert(step != 0, "add with zero cannot exist in our IR")

    def tpe = index.tpe

    private[CountedLoopsRecognizer] def loopHeader: Block = index.block
    private[CountedLoopsRecognizer] def branch: If = continueEdge.owner

    def description: String = {
      def valueOf(n: Node): String = n match {
        case IntegralConst(x) => x.toString
        case _ => "<" + n.simpleName + ">"
      }

      val incrementSuffix = if (incrementIsCompared) " (increment is compared)" else ""
      s"for (i = ${valueOf(start)}; i $cond ${valueOf(limit)}; i += $step$incrementSuffix)"
    }

    override def toString = s"InductiveVariable ${index.id}: $description"
  }

  def findInductiveVariables(loop: Loop[Block], allowInductiveCmp: Boolean = false): collection.Set[InductiveVariable] = {
    // Ignore non-normalized loops
    if (!isNormalizedLoop(loop)) {
      return Set.empty
    }

    val header = loop.header

    // Normalized loop has exactly two inputs and the second is the backward one
    val backwardBranch = header.inputs(1)

    def inLoop(b: Block) = loop.body contains b

    object InductivePhi {
      /** Returns pair of value from forward branch and value from backward branch. */
      def unapply(phi: Phi): Option[(Node, Node)] = condOpt(phi) {
        case Phi(`header`, fv, bv) => (fv, bv)
      }
    }

    // Find inductive variable for each suitable loop exit

    // TODO: Consider moving this check into isCountedVariable().
    //       This property is not required for ArrayIndexChecksOptimizer,
    //       see motivational example in fun-test optimizations/aic method notCounted().
    def isCheckedOnEveryIteration(branch: If) = branch dominates backwardBranch

    // JET-12784: sometimes block with handled throwing node and a branch at the end may be treated
    // as a valid site/use of inductive variable (see code example in the issue), so we need
    // to perform the following check to calculate inductive variable and its continue edge properly
    // (see LoopsRecognizer.calculateLoopExits for more context).
    def hasExitEdge(branch: If) = !inLoop(branch.trueBlock) || !inLoop(branch.falseBlock)

    object ConditionPattern {
      def unapply(n: Node) = n match {
        // Ordinary cmp case
        case c: Cmp => Some(c)
        // Inductive cmp case -- cmp is calculated on the previous iteration, but used on the next one via inductive phi.
        // TODO: create a different kind of inductive variable here and support it in all required optimizations
        case InductivePhi(_, c: Cmp) if allowInductiveCmp && env.enabled(InductiveVariablesWithInductiveCmp) => Some(c)
        case _ => None
      }
    }

    loop.exits flatMap { exit => exit.blockEnd match {
      case branch @ If(ConditionPattern(cmp @ Cmp(cond, left, right))) if cmp.keyType.isIntegralType && hasExitEdge(branch) && isCheckedOnEveryIteration(branch) =>

        def continueOnTrue = inLoop(branch.trueBlock)
        def continueEdge = branch.exit(continueOnTrue)

        def find(limitedValue: Node, limit: Node, reversed: Boolean): Option[InductiveVariable] = {
          def normalizedCondition = {
            def normalizeByExitsOrder(c: Condition) = if (!continueOnTrue) c.negate(isFP = false) else c
            def normalizeByInputsOrder(c: Condition) = if (reversed) c.swap else c
            normalizeByExitsOrder(normalizeByInputsOrder(cond))
          }

          condOpt(limitedValue) {
            // Common part:
            //   index = phi(start, increment)
            //   increment = index +/- step

            // "pre-increment" case:
            //   if (increment cond limit) continue loop
            case AddWithIntegralConst(index @ InductivePhi(start, `limitedValue`), step) =>
              InductiveVariable(index, start, step, incrementIsCompared = true, normalizedCondition, limit, continueEdge)

            // "post-increment" case:
            //   if (index cond limit) continue loop
            case index @ InductivePhi(start, AddWithIntegralConst(`limitedValue`, step)) =>
              InductiveVariable(index, start, step, incrementIsCompared = false, normalizedCondition, limit, continueEdge)
          }
        }

        find(left, right, reversed = false) ++
          find(right, left, reversed = true)

      case _ => Iterable.empty
    }}
  }

  /** Returns true iff this variable is guaranteed to stop loop after some counted number of incrementations. */
  private def isCountedVariable(iv: InductiveVariable, outerIVs: Phi => Iterable[InductiveVariable]): Boolean = {
    // Note that there are more cases when the loop is counted, feel free to add them if you have motivation.

    import Condition._

    object Invariant {
      def unapply(n: Node): Boolean = n match {
        // These popular nodes are invariant for sure, no need to fire up GCM.
        case _: Constant | _: Param => true

        // Feel free to expand logic "node with invariant value arguments is also invariant".
        // Consider more non-spinal-non-memory nodes and others.
        case ArrayLength(Invariant()) |
             Neg(Invariant()) |
             BinaryOp(Invariant(), Invariant())
           => true

        case _ =>
          assert(n.block != null) // GCM must be already done
          (n.block != iv.loopHeader) && (n.block dominates iv.loopHeader)
      }
    }

    val cond = iv.cond

    // Eventually (after 2^32 iterations) index covers all values X
    // such that GCD of step and range length divides |X - startValue|.
    // I.e. ..., startValue-2*stepGcd, startValue-stepGcd, startValue, startValue+stepGcd, ...
    val stepGcd = iv.tpe match {
      case LongType => 1L << java.lang.Long.numberOfTrailingZeros(iv.step.abs)
      case IntType => 1L << java.lang.Integer.numberOfTrailingZeros(iv.step.abs.toInt)
      case x => shouldNotReachHere(x)
    }

    if (cond != NE && cond != EQ) {
      // less/greater case

      val limitAdjustment = cond match {
        case LT | GT | ULT | UGT => 0
        case LE | GE | ULE | UGE => 1
        case _ => shouldNotReachHere(cond)
      }

      // Loop stops if we could guarantee that index ever comes to the "bad" range.
      // It is guaranteed that index comes to that range if stepGcd is less than its length.
      //
      // For less case the "bad" range is [(limit + limitAdj) .. MaxValue]:
      //   stepGcd <= (MaxValue - limit - limitAdj + 1)
      // so limit should be:
      //   limit <= MaxValue - stepGcd - limitAdj + 1 = MaxValue - stepAdj
      //
      // For greater case the "bad" range is [MinValue .. (limit - limitAdj)]:
      //   stepGcd <= (limit - limitAdj - MinValue + 1)
      // so limit should be:
      //   limit >= MinValue + stepGcd + limitAdj - 1 = MinValue + stepAdj
      //
      // TODO: use startValue for more precise detection,
      //       e.g. start = 0, cond = GE, limit = Min+1, step = -2
      //       in this case length of bad range is 1 and it is less than step (2),
      //       but we can guarantee that index comes to that range because it's always even.

      val stepAdjusted = stepGcd + limitAdjustment - 1
      if (stepAdjusted == 0) {
        // In this case any limit value is good (step is equal to 1 and comparision is strict).
        true

      } else {
        calcConstValueRanges(iv.limit, iv.branch, outerIVs) exists { case ConstValueRange(tpe, endValueFrom, endValueTo, _) =>
          import MathUtils.*
          cond match {
            case LT | LE => endValueTo   <= maxValue(tpe) - stepAdjusted
            case GT | GE => endValueFrom >= minValue(tpe) + stepAdjusted
            case ULT | ULE => uleq(endValueTo,   unsignedMaxValue(tpe) - stepAdjusted)
            case UGT | UGE => ugeq(endValueFrom, unsignedMinValue(tpe) + stepAdjusted)
            case _ => shouldNotReachHere(cond)
          }
        }
      }

    } else {
      // equality case

      iv.limit match {
        case Invariant() =>
          if (cond == EQ) {
            // Index will stop the loop in 2 iterations because it cannot be equal to the same limit after increment.
            true

          } else {
            assert(cond == NE)
            if (stepGcd == 1) {
              // If step is coprime with integral range length
              // index will stop the loop in 2^32 iterations because it covers the whole integral range.
              true

            } else {
              (iv.start, iv.limit) match {
                case (IntegralConst(startValue), IntegralConst(endValue)) =>
                  // Note that stepGcd always divides range length so we do not care
                  // about integral overflow while calculating (end - start).
                  (endValue - startValue) % stepGcd == 0

                case _ => false
              }
            }
          }

        case _ => false
      }
    }
  }

  def detectCountedLoops(loops: Loops[Block], collectStats: Boolean = false): Seq[Loop[Block]] = {
    if (loops.isEmpty) return Nil // fast path

    val result = ListBuffer.empty[Loop[Block]]
    withIncrementalGCM {
      val allIVs = mutable.Map.empty[Phi, Iterable[InductiveVariable]] withDefaultValue Nil
      val marked = all[CountedLoopMarker].map(m => loops.loopOf(m.block)).toSet
      for (loop <- loops) {
        if (collectStats) {
          stats.count(StatsKind.CountedLoops, "All loops count")
          stats.count(StatsKind.CountedLoops, s"codeUnit[$codeUnit] - all")
        }
        val isCounted = if (marked(loop)) {
          if (collectStats) {
            stats.count(StatsKind.CountedLoops, "Counted loops count")
            stats.count(StatsKind.CountedLoops, s"codeUnit[$codeUnit] - counted (marked)")
          }
          true
        } else if (loop.kind == LoopKind.IRREDUCIBLE) {
          if (collectStats) {
            stats.count(StatsKind.CountedLoops, "IRREDUCIBLE loops count")
          }
          false
        } else {
          val ivs = findInductiveVariables(loop, allowInductiveCmp = true)
          val isCounted = ivs exists (isCountedVariable(_, allIVs))
          allIVs ++= groupBy(ivs)(_.index) // note that different loops cannot have common indexes
          if (collectStats) {
            if (stats.isEnabled(StatsKind.CountedLoops)) {
              if (isCounted) {
                stats.count(StatsKind.CountedLoops, "Counted loops count")
                stats.count(StatsKind.CountedLoops, s"codeUnit[$codeUnit] - counted")
              } else {
                if (ivs.isEmpty) {
                  stats.count(StatsKind.CountedLoops, "Not recognized: no inductive variables")
                } else {
                  for (iv <- ivs) {
                    stats.count(StatsKind.CountedLoops,
                      s"Not recognized: bad inductive variable: ${iv.description})",
                      iv.continueEdge)
                  }
                }
              }
            }
          }
          isCounted
        }
        if (isCounted) result += loop
      }
    }
    result.toList
  }
}
