/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.post

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase

/**
  * Tests for LiveRangesComponent.
  *
  * @author conwor
  */
class LiveRangesComponentSuite extends PostProcessSuite with LiveRangesComponent {

  startPhase(CompilerPhase.BackEnd)

  override def beforeEach(): Unit = {
    super.beforeEach()
    tieNodesInBackendOrder = true
    allTestNodes = null
  }

  private var allTestNodes: Seq[Node] = _
  private var allTestSSARanges: Seq[SSALiveRange] = _

  private def calcAllSeqs(): Unit = {
    if (allTestNodes == null) {
      allTestNodes = allNodes.toSeq filterNot
        { x => x.isInstanceOf[Fake] && (x.uses.size == 1) && x.singleUse.isInstanceOf[If] } filterNot
        { x => x.isInstanceOf[Branch.Exit] }

      allTestSSARanges = allTestNodes filterNot
        { x => x.isInstanceOf[Block] || x.isInstanceOf[BlockEnd]} filter
        { case N(_) => true; case _ => false } map
        { x => LiveRanges.ssa(x) }
    }
  }

  private def checkRangeContent(ssaRange: SSALiveRange, content: Set[Node]): Unit = {
    calcAllSeqs()
    for (node <- LiveRanges.backendCodeOrder) {
      val delegate = node match {
        case c: Constraints => c.owner
        case _ => node
      }
      (ssaRange contains node) should be (content(delegate))
    }
    val web = LiveRanges.web(ssaRange.value)
    for (node <- content) {
      (web contains node) should be (true)
    }
  }

  private def checkIntersection(ssaRange: SSALiveRange, intersected: Set[SSALiveRange], touched: Set[SSALiveRange]): Unit = {
    calcAllSeqs()
    for (x <- allTestSSARanges if x != ssaRange) {
      (ssaRange intersects x) should be (intersected(x))
      //(ssaRange touches x) should be (touched(x))
      //(ssaRange intersectsOrTouches x) should be (intersected(x) || touched(x))
    }
  }

  private def testWithRanges(name: String)(init: => Unit)(checks: => Unit): Unit = {
    test(name) {
      init
      LiveRanges.enableFor {
        checks
      }
    }
  }


  testWithRanges("one-block ranges check") {
    makeCFG(0 @@ ("a=s()", "b=s()", "c=s()", "d=s(b,c)", "e=s()", "f=s(a,c)", "g=s()", "h=s(f,g)"))
  } {
    val (a, b, c, d, e, f, g, h) = ("a": Node, "b": Node, "c": Node, "d": Node, "e": Node, "f": Node, "g": Node, "h": Node)
    val (ar, br, cr, dr, er, fr, gr, hr) = (LiveRanges.ssa(a), LiveRanges.ssa(b), LiveRanges.ssa(c), LiveRanges.ssa(d), LiveRanges.ssa(e), LiveRanges.ssa(f), LiveRanges.ssa(g), LiveRanges.ssa(h))

    checkRangeContent(ar, Set(a, b, c, d, e))
    checkRangeContent(br, Set(b, c))
    checkRangeContent(cr, Set(c, d, e))
    checkRangeContent(dr, Set(d))
    checkRangeContent(er, Set(e))
    checkRangeContent(fr, Set(f, g))
    checkRangeContent(gr, Set(g))
    checkRangeContent(hr, Set(h))

    checkIntersection(ar, Set(br, cr, dr, er),  Set(fr))
    checkIntersection(br, Set(ar, cr),          Set(dr))
    checkIntersection(cr, Set(ar, br, dr, er),  Set(fr))
    checkIntersection(dr, Set(ar, cr),          Set(br))
    checkIntersection(er, Set(ar, cr),          Set())
    checkIntersection(fr, Set(gr),              Set(ar, cr, hr))
    checkIntersection(gr, Set(fr),              Set(hr))
    checkIntersection(hr, Set(),                Set(fr, gr))
  }

  testWithRanges("node used in block end") {
    makeCFG(0 @@ ("a=s()", "b=s()", "c=s()", "ret(b)"))
  } {
    val (a, b, c) = ("a": Node, "b": Node, "c": Node)
    val (ar, br, cr) = (LiveRanges.ssa(a), LiveRanges.ssa(b), LiveRanges.ssa(c))

    checkRangeContent(ar, Set(a))
    checkRangeContent(br, Set(b, c))
    checkRangeContent(cr, Set(c))

    checkIntersection(ar, Set(),    Set())
    checkIntersection(br, Set(cr),  Set())
    checkIntersection(cr, Set(br),  Set())
  }

  testWithRanges("two-blocks ranges check") {
    makeCFG(0 @@ ("a=s()", "b=s()", "c=s()", "d=s(b,c)") -> 1 @@ ("e=s()", "f=s(a,c)", "g=s()", "h=s(f,g)"))
  } {
    val (a, b, c, d, e, f, g, h) = ("a": Node, "b": Node, "c": Node, "d": Node, "e": Node, "f": Node, "g": Node, "h": Node)
    val (b0, b1) = (0: Block, 1: Block)
    val (ar, br, cr, dr, er, fr, gr, hr) = (LiveRanges.ssa(a), LiveRanges.ssa(b), LiveRanges.ssa(c), LiveRanges.ssa(d), LiveRanges.ssa(e), LiveRanges.ssa(f), LiveRanges.ssa(g), LiveRanges.ssa(h))

    checkRangeContent(ar, Set(a, b, c, d, b0.blockEnd, b1, e))
    checkRangeContent(br, Set(b, c))
    checkRangeContent(cr, Set(c, d, b0.blockEnd, b1, e))
    checkRangeContent(dr, Set(d))
    checkRangeContent(er, Set(e))
    checkRangeContent(fr, Set(f, g))
    checkRangeContent(gr, Set(g))
    checkRangeContent(hr, Set(h))

    checkIntersection(ar, Set(br, cr, dr, er),  Set(fr))
    checkIntersection(br, Set(ar, cr),          Set(dr))
    checkIntersection(cr, Set(ar, br, dr, er),  Set(fr))
    checkIntersection(dr, Set(ar, cr),          Set(br))
    checkIntersection(er, Set(ar, cr),          Set())
    checkIntersection(fr, Set(gr),              Set(ar, cr, hr))
    checkIntersection(gr, Set(fr),              Set(hr))
    checkIntersection(hr, Set(),                Set(fr, gr))
  }

  testWithRanges ("on diamond") {
    makeCFG(0 @@ ("a=s()", "b=s()", "c=s()") -> (1 @@ ("x=s(a,b)", "d=s()") || 2 @@ ("e=s()", "y=s(a,c)")) -> 3 @@ ("z=s(a,a)", "f=s()"))
    val (a, b, c, d, e, f, x, y, z) = ("a": Node, "b": Node, "c": Node, "d": Node, "e": Node, "f": Node, "x": Node, "y": Node, "z": Node)
    val (b0, b1, b2, b3) = (0: Block, 1: Block, 2: Block, 3: Block)

    b0.blockEnd.addConstraints() += a
    b1.blockEnd.addConstraints() += a
    b2.blockEnd.addConstraints() += a

  } {
    val (a, b, c, d, e, f, x, y, z) = ("a": Node, "b": Node, "c": Node, "d": Node, "e": Node, "f": Node, "x": Node, "y": Node, "z": Node)
    val (b0, b1, b2, b3) = (0: Block, 1: Block, 2: Block, 3: Block)
    val (ar, br, cr, dr, er, fr, xr, yr, zr) = (LiveRanges.ssa(a), LiveRanges.ssa(b), LiveRanges.ssa(c), LiveRanges.ssa(d), LiveRanges.ssa(e), LiveRanges.ssa(f), LiveRanges.ssa(x), LiveRanges.ssa(y), LiveRanges.ssa(z))

    checkRangeContent(ar, Set(a, b, c, d, e, x, y, b0.blockEnd, b1, b2, b3))
    checkRangeContent(br, Set(b, c, b0.blockEnd, b1))
    checkRangeContent(cr, Set(c, e, b0.blockEnd, b2))
    checkRangeContent(dr, Set(d))
    checkRangeContent(er, Set(e))
    checkRangeContent(fr, Set(f))
    checkRangeContent(xr, Set(x))
    checkRangeContent(yr, Set(y))
    checkRangeContent(zr, Set(z))

    checkIntersection(ar, Set(br, cr, dr, er, xr, yr),  Set(zr))
    checkIntersection(br, Set(ar, cr),                  Set(xr))
    checkIntersection(cr, Set(ar, br, er),              Set(yr))
    checkIntersection(dr, Set(ar),                      Set())
    checkIntersection(er, Set(ar, cr),                  Set())
    checkIntersection(fr, Set(),                        Set())
    checkIntersection(xr, Set(ar),                      Set(br))
    checkIntersection(yr, Set(ar),                      Set(cr))
    checkIntersection(zr, Set(),                        Set(ar))
  }

  testWithRanges("phi on diamond") {
    makeCFG(0 @@ "a=s()" -> (1 @@ "b=s()" || 2 @@ "c=s()") -> 3 @@ ("p=phi(b,c)", "x=s(a,p)"))
    val (a, b, c, p, x) = ("a": Node, "b": Node, "c": Node, "p": Node, "x": Node)
    val (b0, b1, b2, b3) = (0: Block, 1: Block, 2: Block, 3: Block)

    b0.blockEnd.addConstraints() += a
    b1.blockEnd.addConstraints() += (a, b)
    b2.blockEnd.addConstraints() += (a, c)

  } {
    val (a, b, c, p, x) = ("a": Node, "b": Node, "c": Node, "p": Node, "x": Node)
    val (b0, b1, b2, b3) = (0: Block, 1: Block, 2: Block, 3: Block)
    val (ar, br, cr, pr, xr) = (LiveRanges.ssa(a), LiveRanges.ssa(b), LiveRanges.ssa(c), LiveRanges.ssa(p), LiveRanges.ssa(x))

    checkRangeContent(ar, Set(a, b, c, p, b0.blockEnd, b1, b2, b3))
    checkRangeContent(br, Set(b))
    checkRangeContent(cr, Set(c))
    checkRangeContent(pr, Set(p))
    checkRangeContent(xr, Set(x))

    checkIntersection(ar, Set(br, cr, pr),  Set(xr))
    checkIntersection(br, Set(ar),          Set())
    checkIntersection(cr, Set(ar),          Set())
    checkIntersection(pr, Set(ar),          Set(xr))
    checkIntersection(xr, Set(),            Set(ar, pr))

    LiveRanges.web(a).values.toSet should be (Set(a))
    LiveRanges.web(b).values.toSet should be (Set(b, c, p))
    LiveRanges.web(c).values.toSet should be (Set(b, c, p))
    LiveRanges.web(p).values.toSet should be (Set(b, c, p))
    LiveRanges.web(x).values.toSet should be (Set(x))

    LiveRanges.coverageOf(a, b, c, x) should be (Set(LiveRanges.web(a), LiveRanges.web(b), LiveRanges.web(x)))
  }

  testWithRanges("in loop") {
    makeCFG(0 @@ ("a=s()", "b=s()", "c=s()") -> dw(1 @@ ("d=s()", "x=s(b,d)") -> 2 @@ "y=s(x,c)") -> 3 @@ "z=s(a,y)")
    val (a, b, c, d, x, y, z) = ("a": Node, "b": Node, "c": Node, "d": Node, "x": Node, "y": Node, "z": Node)
    val (b0, b1, b2, b3) = (0: Block, 1: Block, 2: Block, 3: Block)

    b0.blockEnd.addConstraints() += (a, b, c)
    b1.blockEnd.addConstraints() += (a, b, c, x)
    b2.blockEnd.addConstraints() += (a, b, c, y)

  } {
    val (a, b, c, d, x, y, z) = ("a": Node, "b": Node, "c": Node, "d": Node, "x": Node, "y": Node, "z": Node)
    val (b0, b1, b2, b3) = (0: Block, 1: Block, 2: Block, 3: Block)
    val (ar, br, cr, dr, xr, yr, zr) = (LiveRanges.ssa(a), LiveRanges.ssa(b), LiveRanges.ssa(c), LiveRanges.ssa(d), LiveRanges.ssa(x), LiveRanges.ssa(y), LiveRanges.ssa(z))

    checkRangeContent(ar, Set(a, b, c, d, x, y, b2.blockEnd, b0.blockEnd, b1.blockEnd, b1, b2, b3))
    checkRangeContent(br, Set(b, c, d, x, y, b0.blockEnd, b1.blockEnd, b1, b2))
    checkRangeContent(cr, Set(c, d, x, y, b0.blockEnd, b1.blockEnd, b1, b2))
    checkRangeContent(dr, Set(d))
    checkRangeContent(xr, Set(x, b1.blockEnd, b2))
    checkRangeContent(yr, Set(y, b2.blockEnd, b3))
    checkRangeContent(zr, Set(z))

    checkIntersection(ar, Set(br, cr, dr, xr, yr),  Set(zr))
    checkIntersection(br, Set(ar, cr, dr, xr, yr),  Set())
    checkIntersection(cr, Set(ar, br, dr, xr, yr),  Set())
    checkIntersection(dr, Set(ar, br, cr),          Set(xr))
    checkIntersection(xr, Set(ar, br, cr),          Set(dr, yr))
    checkIntersection(yr, Set(ar, br, cr),          Set(xr, zr))
    checkIntersection(zr, Set(),                    Set(ar, yr))
  }

  testWithRanges("phi in loop") {
    makeCFG(0 @@ ("a=s()", "b=s()") -> dw(1 @@ ("p1=phi(a,x)", "p2=phi(b,y)", "x=s(p1,p1)", "y=s(x,x)") -> 2 @@ "t1=s(p2,p2)") -> 3 @@ "t2=s(t1,t1)")
    val (a, b, x, y, p1, p2, t1, t2) = ("a": Node, "b": Node, "x": Node, "y": Node, "p1": Node, "p2": Node, "t1": Node, "t2": Node)
    val (b0, b1, b2, b3) = (0: Block, 1: Block, 2: Block, 3: Block)

    b0.blockEnd.addConstraints() += (a, b)
    b1.blockEnd.addConstraints() += (p2, x, y)
    b2.blockEnd.addConstraints() += (x, y, t1)

  } {
    val (a, b, x, y, p1, p2, t1, t2) = ("a": Node, "b": Node, "x": Node, "y": Node, "p1": Node, "p2": Node, "t1": Node, "t2": Node)
    val (b0, b1, b2, b3) = (0: Block, 1: Block, 2: Block, 3: Block)
    val (ar, br, xr, yr, p1r, p2r, t1r, t2r) = (LiveRanges.ssa(a), LiveRanges.ssa(b), LiveRanges.ssa(x), LiveRanges.ssa(y), LiveRanges.ssa(p1), LiveRanges.ssa(p2), LiveRanges.ssa(t1), LiveRanges.ssa(t2))

    checkRangeContent(ar,   Set(a, b))
    checkRangeContent(br,   Set(b))
    checkRangeContent(xr,   Set(x, y, t1, b1.blockEnd, b2))
    checkRangeContent(yr,   Set(y, t1, b1.blockEnd, b2))
    checkRangeContent(p1r,  Set(p1, p2))
    checkRangeContent(p2r,  Set(p2, x, y, b1.blockEnd, b2))
    checkRangeContent(t1r,  Set(t1, b2.blockEnd, b3))
    checkRangeContent(t2r,  Set(t2))

    checkIntersection(ar,   Set(br),            Set())
    checkIntersection(br,   Set(ar),            Set())
    checkIntersection(xr,   Set(yr, p2r, t1r),  Set(p1r))
    checkIntersection(yr,   Set(xr, p2r, t1r),  Set())
    checkIntersection(p1r,  Set(p2r),           Set(xr))
    checkIntersection(p2r,  Set(xr, yr, p1r),   Set(t1r))
    checkIntersection(t1r,  Set(xr, yr),        Set(p2r, t2r))
    checkIntersection(t2r,  Set(),              Set(t1r))

    LiveRanges.web(a).values.toSet should be (Set(a, p1, x))
    LiveRanges.web(b).values.toSet should be (Set(b, p2, y))
    LiveRanges.web(x).values.toSet should be (Set(a, p1, x))
    LiveRanges.web(y).values.toSet should be (Set(b, p2, y))
    LiveRanges.web(p1).values.toSet should be (Set(a, p1, x))
    LiveRanges.web(p2).values.toSet should be (Set(b, p2, y))
    LiveRanges.web(t1).values.toSet should be (Set(t1))
    LiveRanges.web(t2).values.toSet should be (Set(t2))

    LiveRanges.coverageOf(a, x, b, y) should be (Set(LiveRanges.web(a), LiveRanges.web(b)))
    LiveRanges.coverageOf(a, p1, t1) should be (Set(LiveRanges.web(a), LiveRanges.web(t1)))
    LiveRanges.coverageOf(a, b, t1, t2) should be (Set(LiveRanges.web(a), LiveRanges.web(b), LiveRanges.web(t1), LiveRanges.web(t2)))
  }

  testWithRanges("exception block") {
    makeCFG(0 @@ "a=s()" -> 1 @@ ("b=s()", "n=xs()", "c=s()") -> (2 @@ "x=s(a,b)" || 3 @@ "y=s(c,c)")
      |>| 1 -> xb(4) -> 2)
    removeHandlerAnchors()

    val Seq(a, b, c, x, y, n) = Seq("a": Node, "b": Node, "c": Node, "x": Node, "y": Node, "n": Node)
    val (b0, b1, b2, b3, b4) = (0: Block, 1: Block, 2: Block, 3: Block, 4: Block)

    b0.blockEnd.addConstraints() += a
    b1.blockEnd.addConstraints() += (a, b, c)
    b4.blockEnd.addConstraints() += (a, b)

  } {
    val Seq(a, b, c, x, y, n) = Seq("a": Node, "b": Node, "c": Node, "x": Node, "y": Node, "n": Node)
    val (b0, b1, b2, b3, b4) = (0: Block, 1: Block, 2: Block, 3: Block, 4: Block)
    val (ar, br, cr, xr, yr) = (LiveRanges.ssa(a), LiveRanges.ssa(b), LiveRanges.ssa(c), LiveRanges.ssa(x), LiveRanges.ssa(y))

    allTestNodes = Seq(a, b, c, x, y, b0, b1, b2, b3, b0.blockEnd, b1.blockEnd, n, b4, b4.blockEnd)
    allTestSSARanges = Seq(ar, br, cr, xr, yr)

    checkRangeContent(ar, Set(a, b, c, b1, b2, b0.blockEnd, n, b4))
    checkRangeContent(br, Set(b, c, b2, n, b4))
    checkRangeContent(cr, Set(c, b1.blockEnd, b3))
  }

  testWithRanges("exception block - 2") {
    makeCFG(0 @@ ("a=s()", "b=s()", "n=xs()", "c=s()", "x=s(a,a)") -> xb(1) @@ ("xAdd=s(a,a)"))
    removeHandlerAnchors()
  } {
    val Seq(a, b, c, x, n, xAdd) = Seq("a": Node, "b": Node, "c": Node, "x": Node, "n": Node, "xAdd": Node)
    val (b0, b1) = (0: Block, 1: Block)
    val (ar, br, cr, xr, xAddR) = (LiveRanges.ssa(a), LiveRanges.ssa(b), LiveRanges.ssa(c), LiveRanges.ssa(x), LiveRanges.ssa(xAdd))

    allTestNodes = Seq(a, b, c, x, b0, n, b1, xAdd)
    allTestSSARanges = Seq(ar, br, cr, xr, xAddR)

    checkRangeContent(ar, Set(a, b, c, n, b1))
  }

}