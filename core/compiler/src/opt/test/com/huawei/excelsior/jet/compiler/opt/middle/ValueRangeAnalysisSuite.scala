/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.types.References.OpenCone
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{ArithNodesDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.util.ScalaCollections.groupBy
import com.huawei.excelsior.jet.util.graph.Loops
import org.scalactic.source

import scala.annotation.nowarn

@nowarn("msg=match may not be exhaustive")
class ValueRangeAnalysisSuite
  extends CompilerSuite
    with GlobalNodesBuilder
    with ArithNodesDSL
    with ValueRangeAnalysis
    with CountedLoopsRecognizer {

  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("arr")({
        case Seq() => addPinnedObjNode(OpenCone(tI1D, mayBeNull = false))
      }),

      new SimpleAttribute("len")({
        case Seq(array) => JavaArrayLength(array)
      }),

      new SimpleAttribute("aic")({
        case Seq(array, idx, len) => ArrayIndexCheck(sigI1D, array, idx, len)
      }),

      new SimpleAttribute("inc")({
        case Seq(value) => Add(value, IConst(1))
      }),

      new SimpleAttribute("dec")({
        case Seq(value) => Add(value, IConst(1))
      }),

      new SimpleAttribute("filter_const_0_10")({
        case Seq(value) => RawValueRangeFilter(value, IConst(0), IConst(10))
      }),

      new SimpleAttribute("filter_const_min_max")({
        case Seq(value) => RawValueRangeFilter(value, IConst(Int.MinValue), IConst(Int.MaxValue))
      }),

      new SimpleAttribute("filter_const_1_10")({
        case Seq(value) => RawValueRangeFilter(value, IConst(1), IConst(10))
      }),

      new SimpleAttribute("filter_half_0")({
        case Seq(value, to) => RawValueRangeFilter(value, IConst(0), to)
      }),

      new SimpleAttribute("filter_sym")({
        case Seq(value, from, to) => RawValueRangeFilter(value, from, to)
      }),

      new SimpleAttribute("idx_const_0_100")({
        case Seq() => addInductiveVariable(IConst(0), Condition.LT, IConst(100), IConst(1))
      }),

      new SimpleAttribute("idx_half_0")({
        case Seq(to) => addInductiveVariable(IConst(0), Condition.LT, to, IConst(1))
      }),

      new SimpleAttribute("idx_sym")({
        case Seq(from, to) => addInductiveVariable(from, Condition.LT, to, IConst(1))
      }),

    ) ++ super.parsableAttributes()
  }

  def e(implicit tpe: Type) = lazyValue("empty", EmptyValueRange(tpe))
  def c(from: Long, to: Long)(implicit tpe: Type) = lazyValue(s"[$from .. $to]", ConstValueRange(tpe, from, to, null))
  def h(from: Long, to: LazyValue[Node], toAddend: Long)(implicit tpe: Type) = lazyValue(s"[$from, $to + $toAddend]", HalfSymbolicValueRange(tpe, from, to(), toAddend, null))
  def s(from: LazyValue[Node], fromAddend: Long, to: LazyValue[Node], toAddend: Long)(implicit tpe: Type) = lazyValue(s"[$from + $fromAddend, $to + $toAddend]", SymbolicValueRange(tpe, from(), fromAddend, to(), toAddend, null))

  def c(tpe: Type, from: Long, to: Long, evidence: ControlNode) = ConstValueRange(tpe, from, to, evidence)
  def h(from: Long, to: Node, toAddend: Long, evidence: ControlNode) = HalfSymbolicValueRange(to.tpe, from, to, toAddend, evidence)
  def s(from: Node, fromAddend: Long, to: Node, toAddend: Long, evidence: ControlNode) = SymbolicValueRange(from.tpe, from, fromAddend, to, toAddend, evidence)

  val I = IntType
  val L = LongType

  def c(c: Long)(implicit tpe: Type) = lazyValue(c.toString, IntegralConst(tpe)(c))
  def x(implicit tpe: Type) = lazyValue("x", Param(tpe, 1))
  def y(implicit tpe: Type) = lazyValue("y", Param(tpe, 2))
  def add(l: LazyValue[Node], r: LazyValue[Node]) = lazyValue(s"($l + $r)", Add(l(), r()))
  def sub(l: LazyValue[Node], r: LazyValue[Node]) = lazyValue(s"($l - $r)", Sub(l(), r()))


  for (((range, from, to, size), pos) <- (Seq(I, L) flatMap { implicit tpe: Type =>
    Seq(
      tp(c(0, 10),                        c(0),             c(10),            c(11)),
      tp(c(1, 10),                        c(1),             c(10),            c(10)),
      tp(c(-1, 1),                        c(-1),            c(1),             c(3)),
      tp(c(0, 0),                         c(0),             c(0),             c(1)),
      tp(c(0, Int.MaxValue),              c(0),             c(Int.MaxValue),  c(Int.MaxValue.toLong + 1)),

      tp(h(0,             y,            0),             c(0),             y,                        add(y, c(1))),
      tp(h(1,             y,            1),             c(1),             add(y, c(1)),             add(y, c(1))),
      tp(h(Int.MaxValue,  y,            Int.MaxValue),  c(Int.MaxValue),  add(y, c(Int.MaxValue)),  add(y, c(1))),
      tp(h(Int.MinValue,  y,            Int.MinValue),  c(Int.MinValue),  add(y, c(Int.MinValue)),  add(y, c(1))),
      tp(h(1,             y,            0),             c(1),             y,                        y),
      tp(h(0,             y,            -1),            c(0),             sub(y, c(1)),             y),
      tp(h(0,             sub(y, c(1)), 0),             c(0),             sub(y, c(1)),             y),
      tp(h(Int.MinValue,  y,            0),             c(Int.MinValue),  y,                        sub(y, c(Int.MinValue.toLong - 1))),
      tp(h(Int.MaxValue,  y,            Int.MinValue),  c(Int.MaxValue),  add(y, c(Int.MinValue)),  sub(y, c(Int.MaxValue.toLong - Int.MinValue.toLong - 1))),

      tp(s(x,            0,             y,            0),             x,                        y,                        add(sub(y, x), c(1))),
      tp(s(x,            1,             y,            1),             add(x, c(1)),             add(y, c(1)),             add(sub(y, x), c(1))),
      tp(s(x,            Int.MaxValue,  y,            Int.MaxValue),  add(x, c(Int.MaxValue)),  add(y, c(Int.MaxValue)),  add(sub(y, x), c(1))),
      tp(s(x,            Int.MinValue,  y,            Int.MinValue),  add(x, c(Int.MinValue)),  add(y, c(Int.MinValue)),  add(sub(y, x), c(1))),
      tp(s(x,            1,             y,            0),             add(x, c(1)),             y,                        sub(y, x)),
      tp(s(add(x, c(1)), 0,             y,            0),             add(x, c(1)),             y,                        sub(y, x)),
      tp(s(x,            0,             y,            -1),            x,                        sub(y, c(1)),             sub(y, x)),
      tp(s(x,            0,             sub(y, c(1)), 0),             x,                        sub(y, c(1)),             sub(y, x)),
      tp(s(x,            0,             y,            Int.MaxValue),  x,                        add(y, c(Int.MaxValue)),  add(sub(y, x), c(Int.MaxValue.toLong + 1))),
      tp(s(x,            Int.MinValue,  y,            0),             add(x, c(Int.MinValue)),  y,                        add(sub(y, x), c(-Int.MinValue.toLong + 1))),
      tp(s(x,            Int.MaxValue,  y,            Int.MinValue),  add(x, c(Int.MaxValue)),  add(y, c(Int.MinValue)),  add(sub(y, x), c(Int.MinValue.toLong - Int.MaxValue.toLong + 1))),
    )
  }) ++ {
    implicit def tpe: Type = L
    Seq(
      tp(c(   Long.MinValue,    Long.MaxValue), c(Long.MinValue),         c(Long.MaxValue),         c(Long.MaxValue - Long.MinValue + 1)),
      tp(h(   Long.MinValue, y, Long.MaxValue), c(Long.MinValue),         add(y, c(Long.MaxValue)), sub(y, c(Long.MinValue - Long.MaxValue - 1))),
      tp(s(x, Long.MinValue, y, Long.MaxValue), add(x, c(Long.MinValue)), add(y, c(Long.MaxValue)), add(sub(y, x), c(Long.MaxValue - Long.MinValue + 1))),
    )
  }) {

    test(s"value range API: ${range()}") {
      ValueRange.bounds(range()) shouldBe (from(), to())
      ValueRange.bounds(ValueRange(from(), to(), null)) shouldBe (from(), to())
      ValueRange.size(range()) shouldBe size()
    }
  }

  import Condition._

  for {
    ((tpe, start, step, incrementIsCompared, limit, range), pos) <- Seq(I, L) flatMap { implicit tpe: Type =>
      // Important: scalac cannot infer this type in a reasonable time, so we help it by explicitly specifying the type here.
      Seq[((Type, LazyValue[Node], Int, Boolean, LazyValue[Node], Condition => Option[LazyValue[ValueRange]]), source.Position)](

        // ConstValueRange

        tp(tpe, c(0), 1, false, c(100), {
          case LT | NE => Some(c(0, 99))
          case LE => Some(c(0, 100))
          case GT | GE => Some(e)
        }),

        tp(tpe, c(0), 1, true, c(100), {
          case LT | NE => Some(c(0, 98))
          case LE => Some(c(0, 99))
          case GT | GE => Some(e)
        }),

        tp(tpe, c(0), 1, false, c(0), {
          case LE => Some(c(0, 0))
          case GE => None
          case GT | LT | NE => Some(e)
        }),

        tp(tpe, c(0), 1, true, c(0), {
          case LT | LE => Some(e)
          case GT | GE | NE => None
        }),

        tp(tpe, c(0), 1, false, c(1), {
          case LT | NE => Some(c(0, 0))
          case LE => Some(c(0, 1))
          case GT | GE => Some(e)
        }),

        tp(tpe, c(0), 1, true, c(1), {
          case LT | NE | GT => Some(e)
          case LE => Some(c(0, 0))
          case GE => None
        }),

        tp(tpe, c(0), 2, false, c(4), {
          case LT | NE => Some(c(0, 2))
          case LE => Some(c(0, 4))
          case GT | GE => Some(e)
        }),

        tp(tpe, c(0), 2, true, c(4), {
          case LT | NE => Some(c(0, 0))
          case LE => Some(c(0, 2))
          case GT | GE => Some(e)
        }),

        tp(tpe, c(0), 3, false, c(4), {
          case LT | LE => Some(c(0, 3))
          case GT | GE => Some(e)
          case NE => None
        }),

        tp(tpe, c(0), 3, true, c(4), {
          case LT | LE => Some(c(0, 0))
          case GT | GE => Some(e)
          case NE => None
        }),

        tp(tpe, c(0), 1, false, c(Int.MaxValue - 1), {
          case LT | NE => Some(c(0, Int.MaxValue - 2))
          case LE => Some(c(0, Int.MaxValue - 1))
          case GT | GE => Some(e)
        }),

        tp(tpe, c(0), 1, false, c(Int.MaxValue), {
          case LT | NE => Some(c(0, Int.MaxValue - 1))
          case GT | GE => Some(e)
          case LE if tpe == L => Some(c(0, Int.MaxValue))
          case LE => None
        }),

        tp(tpe, c(Int.MaxValue), 1, false, c(Int.MaxValue), {
          case LT | NE | GT => Some(e)
          case LE if tpe == L => Some(c(Int.MaxValue, Int.MaxValue))
          case LE | GE => None
        }),

        tp(tpe, c(Int.MaxValue - 1), 1, false, c(Int.MaxValue), {
          case LT | NE => Some(c(Int.MaxValue - 1, Int.MaxValue - 1))
          case GT | GE => Some(e)
          case LE if tpe == L => Some(c(Int.MaxValue - 1, Int.MaxValue))
          case LE => None
        }),

        tp(tpe, c(Int.MaxValue - 1), 2, false, c(Int.MaxValue), {
          case GT | GE => Some(e)
          case LT | LE if tpe == L => Some(c(Int.MaxValue - 1, Int.MaxValue - 1))
          case LT | LE | NE => None
        }),

        tp(tpe, c(Int.MaxValue - 2), 2, false, c(Int.MaxValue), {
          case LT | NE => Some(c(Int.MaxValue - 2, Int.MaxValue - 2))
          case GT | GE => Some(e)
          case LE if tpe == L => Some(c(Int.MaxValue - 2, Int.MaxValue))
          case LE => None
        }),

        tp(tpe, c(Int.MinValue), 1, false, c(Int.MaxValue), {
          case LT | NE => Some(c(Int.MinValue, Int.MaxValue - 1))
          case GT | GE => Some(e)
          case LE if tpe == L => Some(c(Int.MinValue, Int.MaxValue))
          case LE => None
        }),

        tp(tpe, c(Int.MinValue), 2, false, c(Int.MaxValue), {
          case GT | GE => Some(e)
          case LT | LE if tpe == L => Some(c(Int.MinValue, Int.MaxValue - 1))
          case LT | LE | NE => None
        }),

        tp(tpe, c(Int.MinValue), 5, false, c(Int.MaxValue), {
          case LT | NE => Some(c(Int.MinValue, Int.MaxValue - 5))
          case GT | GE => Some(e)
          case LE if tpe == L => Some(c(Int.MinValue, Int.MaxValue))
          case LE => None
        }),

        tp(tpe, c(Int.MinValue), 7, false, c(Int.MaxValue), {
          case GT | GE => Some(e)
          case LT | LE if tpe == L => Some(c(Int.MinValue, Int.MaxValue - 3))
          case LT | LE | NE => None
        }),

        tp(tpe, c(0), 1, false, c(minValue(tpe)), {
          case LE => Some(e)
          case LT => None // could be Some(e)
          case NE => None
          case GT | GE => None
        }),

        tp(tpe, c(0), 1, true, c(minValue(tpe)), {
          case LT | LE => None // could be Some(e)
          case NE => None
          case GT | GE => None
        }),

        tp(tpe, c(100), -1, false, c(0), {
          case GT | NE => Some(c(1, 100))
          case GE => Some(c(0, 100))
          case LT | LE => Some(e)
        }),

        tp(tpe, c(100), -1, true, c(0), {
          case GT | NE => Some(c(2, 100))
          case GE => Some(c(1, 100))
          case LT | LE => Some(e)
        }),

        tp(tpe, c(0), -1, false, c(0), {
          case GT | NE | LT => Some(e)
          case GE => Some(c(0, 0))
          case LE => None
        }),

        tp(tpe, c(0), -1, true, c(0), {
          case GT | GE => Some(e)
          case LT | LE | NE => None
        }),

        tp(tpe, c(1), -1, false, c(0), {
          case GT | NE => Some(c(1, 1))
          case GE => Some(c(0, 1))
          case LT | LE => Some(e)
        }),

        tp(tpe, c(1), -1, true, c(0), {
          case GT | NE | LT => Some(e)
          case GE => Some(c(1, 1))
          case LE => None
        }),

        tp(tpe, c(4), -2, false, c(0), {
          case GT | NE => Some(c(2, 4))
          case GE => Some(c(0, 4))
          case LT | LE => Some(e)
        }),

        tp(tpe, c(4), -2, true, c(0), {
          case GT | NE => Some(c(4, 4))
          case GE => Some(c(2, 4))
          case LT | LE => Some(e)
        }),

        tp(tpe, c(1), -2, false, c(0), {
          case GT | GE => Some(c(1, 1))
          case NE => None
          case LT | LE => Some(e)
        }),

        tp(tpe, c(1), -2, true, c(0), {
          case GT | GE => Some(e)
          case LT | LE | NE => None
        }),

        tp(tpe, c(4), -3, false, c(0), {
          case GT | GE => Some(c(1, 4))
          case LT | LE => Some(e)
          case NE => None
        }),

        tp(tpe, c(4), -3, true, c(0), {
          case GT | GE => Some(c(4, 4))
          case LT | LE => Some(e)
          case NE => None
        }),

        // HalfSymbolicValueRange

        tp(tpe, c(0), 1, false, y, {
          case LT | NE => Some(h(0, y, -1))
          case LE => Some(h(0, y, 0))
          case GT | GE => None
        }),

        tp(tpe, c(0), 1, true, y, {
          case LT | NE => Some(h(0, y, -2))
          case LE => Some(h(0, y, -1))
          case GT | GE => None
        }),

        tp(tpe, c(0), -1, false, x, {
          case GT | NE => Some(s(x, 1, c(0), 0))
          case GE => Some(s(x, 0, c(0), 0))
          case LT | LE => None
        }),

        tp(tpe, c(0), -1, true, x, {
          case GT | NE => Some(s(x, 2, c(0), 0))
          case GE => Some(s(x, 1, c(0), 0))
          case LT | LE => None
        }),

        tp(tpe, c(0), 2, false, y, {
          case _ => None
        }),

        tp(tpe, c(Int.MaxValue), 1, false, y, {
          case LT | NE if tpe == L => Some(h(Int.MaxValue, y, -1))
          case LE if tpe == L => Some(h(Int.MaxValue, y, 0))
          case LT | LE | GT | GE | NE => None
        }),

        tp(tpe, c(Int.MaxValue - 1), 1, false, y, {
          case LT | NE => Some(h(Int.MaxValue - 1, y, -1))
          case LE => Some(h(Int.MaxValue - 1, y, 0))
          case GT | GE => None
        }),

        // SymbolicValueRange

        tp(tpe, x, 1, false, y, {
          case LT | NE => Some(s(x, 0, y, -1))
          case LE => Some(s(x, 0, y, 0))
          case GT | GE => None
        }),

        tp(tpe, x, 1, true, y, {
          case LT | NE => Some(s(x, 0, y, -2))
          case LE => Some(s(x, 0, y, -1))
          case GT | GE => None
        }),

        tp(tpe, y, -1, false, x, {
          case GT | NE => Some(s(x, 1, y, 0))
          case GE => Some(s(x, 0, y, 0))
          case LT | LE => None
        }),

        tp(tpe, y, -1, true, x, {
          case GT | NE => Some(s(x, 2, y, 0))
          case GE => Some(s(x, 1, y, 0))
          case LT | LE => None
        }),

        tp(tpe, x, 2, false, y, {
          case _ => None
        }),

      )
    }
    cond <- Seq(LT, LE, NE, GT, GE)
  } {
    test(s"inductive variable range: ($tpe, $start, $step, $incrementIsCompared, $cond, $limit)") {
      makeCFG(0)
      val idx = Phi.raw(tpe)(b(0)) // Dummy phi function
      val iv = InductiveVariable(idx, start(), step, incrementIsCompared, cond, limit(), null)

      calcValueRangeOfInductiveVariable(iv) shouldBe range(cond).map(_.apply())
    }
  }

  def checkValueRanges(node: Node, point: LowerPoint)(ranges: ValueRange*)(extendedRanges: ValueRange*): Unit = {
    val inductiveVariables: collection.SeqMap[Phi, Seq[InductiveVariable]] = {
      val inductive = cfg.loops.iterator flatMap (findInductiveVariables(_))
      groupBy(inductive)(_.index)
    }

    calcValueRanges(node, point, inductiveVariables, extendedRanges = false).toSet shouldBe ranges.toSet
    calcValueRanges(node, point, inductiveVariables, extendedRanges = true).toSet shouldBe extendedRanges.toSet
  }

  def continueEdge(b: Block) = b.blockEnd.asInstanceOf[If].trueExit
  def exitEdge(b: Block) = b.blockEnd.asInstanceOf[If].falseExit

  def cn(str: String) = n(str).asInstanceOf[LowerPoint]

  test("const ranges") {
    makeCFG(0@@("a=arr()", "l=len(a)") -> wd(1@@("i=idx_const_0_100()", "i++=inc(i)") -> 2@@("ci=aic(a,i,l)", "ci++=aic(a,i++,l)")) -> 9)

    checkValueRanges("i", 2.blockEnd)(
      c(I, 0, 99, continueEdge(1)),
    )(
      c(I, 0, 99, continueEdge(1)),
      h(0, "l", -1, cn("ci++")),
    )

    checkValueRanges("i", 1.blockEnd)(
      // empty
    )(
      //c(I, 0, Int.MaxValue, 1),
    )

    checkValueRanges("i", exitEdge(1).target.blockEnd)(
      //c(I, 100, Int.MaxValue, exitEdge(1)),
    )(
      //c(I, 100, Int.MaxValue, exitEdge(1)),
    )

    checkValueRanges("i++", 2.blockEnd)(
      c(I, 1, 100, continueEdge(1)),
    )(
      c(I, 1, 100, continueEdge(1)),
      h(0, "l", -1, 2.blockEnd),
      h(1, "l", 0, cn("ci++")),
    )
  }

  test("consecutive loops") {
    makeCFG(0@@("a=arr()", "l=len(a)") -> wd(1@@("i=idx_half_0(l)", "i++=inc(i)") -> 2) -> 3@@("fi=filter_const_0_10(i)") ->
      (wd(4@@("j=idx_sym(i,l)") -> 5) || wd(6@@("k=idx_sym(i++,l)") -> 7)) -> 9)

    checkValueRanges("i", 2.blockEnd)(
      h(0, "l", -1, continueEdge(1)),
    )(
      h(0, "l", -1, continueEdge(1)),
    )

    checkValueRanges("j", 5.blockEnd)(
      s("i", 0, "l", -1, continueEdge(4)),
    )(
      s("i", 0, "l", -1, continueEdge(4)),
      h(0, "l", -1, continueEdge(4)), // using "fi"
    )

    checkValueRanges("k", 7.blockEnd)(
      s("i++", 0, "l", -1, continueEdge(6)),
    )(
      s("i++", 0, "l", -1, continueEdge(6)),
      h(1, "l", -1, continueEdge(6)), // using "fi"
    )
  }

  test("consecutive filters") {
    makeCFG(0@@("s", "end1", "end2", "i") -> 2@@("f1=filter_sym(i,s,end1)", "f2=filter_half_0(i,end2)", "f3=filter_const_1_10(i)") -> 9)

    checkValueRanges("i", 0.blockEnd)(
      // empty
    )(
      // empty
    )

    checkValueRanges("i", cn("f1"))(
      // empty
    )(
      s("s", 0, "end1", 0, cn("f1")),
    )

    checkValueRanges("i", cn("f2"))(
      // empty
    )(
      s("s", 0, "end1", 0, cn("f1")),
      h(0, "end2", 0, cn("f2")),
      //h(0, "end1", 0, cn("f2")), // using "f1" and "f2"
    )

    checkValueRanges("i", cn("f3"))(
      // empty
    )(
      s("s", 0, "end1", 0, cn("f1")),
      h(0, "end2", 0, cn("f2")),
      c(I, 1, 10, cn("f3")),
      //h(0, "end1", 0, cn("f2")), // using "f1" and "f2"
      //h(1, "end1", 0, cn("f3")), // using "f1" and "f3"
      //h(1, "end2", 0, cn("f3")), // using "f2" and "f3"
    )
  }

  test("add with const overflow") {
    makeCFG(0@@("i", "i++=inc(i)", "i--=dec(i)") -> 2@@("f=filter_const_min_max(i)") -> 9)

    checkValueRanges("i", 2.blockEnd)(
      // empty
    )(
      c(I, Int.MinValue, Int.MaxValue, cn("f")),
    )

    checkValueRanges("i++", 2.blockEnd)(
      // empty
    )(
      // empty (overflow MaxValue)
    )

    checkValueRanges("i--", 2.blockEnd)(
      // empty
    )(
      // empty (overflow MinValue)
    )
  }

  test("empty filtered value range") {
    makeCFG(0 -> 1)
    val aic = makeNodes { at =>
      at(0)
      ArrayIndexCheck(sigI1D, addObjNode(), addNode(), ic(0))
    }
    aic.filteredValueRange shouldBe empty
  }

  test("regression test for infinite recursion") {
    makeCFG(0 -> wd(1 -> 2) -> 3)
    makeNodes { at =>
      at(1)
      val limit = Proxy(IntType)(1)
      n("i") = addInductiveVariable(IConst(0), Condition.GT, limit, IConst(-1))
      n("i--") = Add(n("i"), IConst(-1))
      limit replaceBy n("i--")
    }

    checkValueRanges("i", 2.blockEnd)(
      s(n("i--"), 1, IConst(0), 0, continueEdge(1))
    )(
      s(n("i--"), 1, IConst(0), 0, continueEdge(1))
    )
  }

  test("correct filtered value ctrl") {
    makeCFG(0@@("a=arr()", "l=len(a)", "x") -> 1@@"aic1=aic(a,x,l)" -> xb(2)@@"aic2=aic(a,x,l)")
    removeHandlerAnchors()

    checkValueRanges("x", 1.blockEnd)(
      // empty
    )(
      h(0, "l", -1, 1.blockEnd),
    )

    checkValueRanges("x", cn("aic2"))(
      // empty
    )(
      // empty
    )
  }

  test("correct filtered value ctrl with const") {
    makeCFG(0@@("a=arr()", "x=ic(42)") -> 1@@"aic1=aic(a,x,x)" -> xb(2)@@"aic2=aic(a,x,x)")
    removeHandlerAnchors()

    checkValueRanges("x", 1.blockEnd)(
      c(I, 42, 42, null),
    )(
      c(I, 42, 42, null),
      c(I, 0, 41, 1.blockEnd), // 1.blockEnd is unreachable, so this impossible range is ok
    )

    checkValueRanges("x", cn("aic2"))(
      c(I, 42, 42, null),
    )(
      c(I, 42, 42, null),
    )
  }

  test("regression test for JET-12784") {
    makeCFG(0 -> wd(1@@("i=idx_const_0_100()", "i++=inc(i)") -> 2@@"xspinal()" -> (3 || 4) -> 5) -> 9 |>| 2 -> xb(6) -> 9)
    addCondition(b(2), "i", "i++", Condition.LT)
    removeHandlerAnchors()

    checkValueRanges("i", 3.blockEnd)(
      c(I, 0, 99, continueEdge(1))
    )(
      c(I, 0, 99, continueEdge(1))
    )
  }

}
