/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeMethod
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.Guards._
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes._
import com.huawei.excelsior.jet.compiler.types.References._

class ReferencesSuite extends CompilerSuite with TypesToolbox {

  import TypeApproximationBuildingHelperNonStrict._
  import com.huawei.excelsior.jet.compiler.types.Approximation.CC._

  test("strict building") {
    import TypeApproximationBuildingHelperStrict._
    resetCHA()
    intercept[ClassCastException] {
      // expecting open cone
      c(tCF)
    }
    intercept[ClassCastException] {
      // expecting open cone
      w(tCF, e)
    }
    intercept[ClassCastException] {
      // it cannot be widened
      w(p(tCF), e)
    }
  }

  test("interface point") {
    intercept[IllegalArgumentException] {
      pn(tI)
    }
  }

  test("negative height closed cone") {
    resetCHA(tB, tCF)
    cc(tC)
    intercept[IllegalArgumentException] {
      cc(tC, -1)
    }
    ccl(tB, 5)
    intercept[IllegalArgumentException] {
      ccl(tB, 4)
    }
  }

  test("widened and not widened") {
    resetCHA(tB, tC)

    e.withProbableType(e).hasRefinedProbableType should be (false)
    n.withProbableType(e).hasRefinedProbableType should be (false)
    p(tA).withProbableType(e).hasRefinedProbableType should be (false)
    c(tA).withProbableType(e).hasRefinedProbableType should be (true)
    cc(tA).withProbableType(e).hasRefinedProbableType should be (true)

    (w(tA, e) equals         c(tA)) should be (true)
    (w(tA, e) compare        c(tA)) should be (Equal)
    (w(tA, e) equalsWidened  c(tA)) should be (false)
    (w(tA, e) compareWidened c(tA)) should be (Less)
  }

  test("widened with and without null") {
    wn(tA, c(tA)).withoutNull.hasRefinedProbableType should be (false)
    wn(tA, c(tA)).withNull.hasRefinedProbableType should be (false)
  }

  test("too probable") {
    (c(tObj) withProbableType c(tA)) should beTA (w(tObj, c(tA)))
    intercept[IllegalArgumentException] {
      c(tObj) withProbableType w(tA, c(tB))
    }
  }

  test("canonicalization") {
    c(tCF) should beTA (p(tCF))
    c(tInt1D) should beTA (p(tInt1D))

    resetCHA(tIB, tC)
    cc(tA) should beTA (cc(tA, 3))
    cc(tA, 99) should beTA (cc(tA, 3))
    cc(tC) should beTA (p(tC))
    cc(tObj) should beTA (cc(tObj, 4))
    cc(tObj, 1) should beTA (p(tObj))
    cc(tObj, 2) should beTA (cc(tObj, 2))
    ccl(tB, 6) should beTA (cc(tB, 2))

    resetCHA(tObj)
    cc(tObj) should beTA (p(tObj))

    {
      def withAbstract(types: ReferenceType*)(action: => Unit): Unit = {
        types foreach (_.setAbstractClass(true))
        try {
          action
        } finally {
          types foreach (_.setAbstractClass(false))
        }
      }

      resetCHA(tIB, tC)

      withAbstract(tC) {
        cc(tC) should beTA (e)
      }

      withAbstract(tA, tIB, tC) {
        cc(tA, 2) should beTA (p(tB))
        cc(tA) shouldNot beTA (p(tB)) // only root is refined, max level is not refined
      }

      withAbstract(tA, tB, tIB) {
        cc(tA) should beTA (p(tC))
      }

      withAbstract(tA, tB, tC) {
        cc(tA, 2) should beTA (e)
        cc(tA, 3) should beTA (p(tIB))
      }
    }
  }

  test("closed types interface subcones") {
    resetCHA(tIBB, tJB, tC)
    cc(tObj).asInstanceOf[ClosedCone].subConeImplementingInterface(tI) should be (Some(tObj)) // could be Some(tB)
    cc(tA).asInstanceOf[ClosedCone].subConeImplementingInterface(tI) should be (Some(tB))
    cc(tA, 2).asInstanceOf[ClosedCone].subConeImplementingInterface(tI) should be (None)
    cc(tB, 2).asInstanceOf[ClosedCone].subConeImplementingInterface(tI) should be (Some(tB))
    cc(tB, 2).asInstanceOf[ClosedCone].subConeImplementingInterface(tJ) should be (Some(tJB))
    cc(tB, 3).asInstanceOf[ClosedCone].subConeImplementingInterface(tI) should be (Some(tB))
    cc(tIB).asInstanceOf[ClosedCone].subConeImplementingInterface(tI) should be (Some(tIB))
  }

  val unionInfo = withCHA()(Seq(
     tp(e,          n,          n)
    ,tp(e,          cn(tA),     cn(tA))
    ,tp(e,          pn(tA),     pn(tA))
    ,tp(n,          cn(tA),     cn(tA))
    ,tp(n,          c(tA),      cn(tA))
    ,tp(n,          pn(tA),     pn(tA))
    ,tp(n,          p(tA),      pn(tA))

    ,tp(cn(tB),     cn(tC),     cn(tA))
    ,tp(c(tB),      cn(tC),     cn(tA))
    ,tp(c(tB),      c(tC),      c(tA))

    ,tp(pn(tB),     pn(tC),     cn(tA))
    ,tp(p(tB),      pn(tC),     cn(tA))
    ,tp(p(tB),      p(tC),      c(tA))

    ,tp(p(tB),      cn(tC),     cn(tA))
    ,tp(c(tB),      pn(tC),     cn(tA))
    ,tp(c(tB),      p(tC),      c(tA))

    ,tp(p(tA),      pn(tA),     pn(tA))

    ,tp(p(tObj1D),  p(tInt1D),  c(tObj))

    ,tp(p(tA),      p(tD),      c(tObj))
    ,tp(p(tTX),     p(tTY),     c(tThinType))

  )) ++ withCHA(tIB, tCF)(Seq(
     tp(p(tIB),     p(tC),      cc(tA, 3))
    ,tp(cc(tB, 2),  cc(tC, 2),  cc(tA, 3))
    ,tp(cc(tB, 2),  cc(tA, 2),  cc(tA, 3))
    ,tp(p(tC),      cc(tB),     cc(tA))

    ,tp(p(tB1D),    p(tCF1D),   c(tA1D))

    ,tp(p(tObj1D),  p(tObj2D),  c(tObj1D))

    ,tp(p(tI1D),    p(tJ1D),    c(tI1D))
    ,tp(p(tI1D),    p(tK1D),    c(tObj1D))
  ))

  for ((((t1, t2, tUnion), pos), chaTypes) <- unionInfo) {
    test(s"union: $t1 & $t2 (cha: $chaTypes)") {
      resetCHASeq(chaTypes)

      (t1 union t1) should beTA (t1)
      (t2 union t2) should beTA (t2)
      (t1 union t2) should beTA (tUnion)
      (t2 union t1) should beTA (tUnion)
      (t1 union tUnion) should beTA (tUnion)
      (t2 union tUnion) should beTA (tUnion)

      (t1 compare tUnion) should (be (Less) or be (Equal))
      (t2 compare tUnion) should (be (Less) or be (Equal))
    }
  }

  val unionOnEdgesInfo = withCHA()(Seq(
    /*   (approx1,  isColdEdge1) | (approx2,    isColdEdge2) | union     */
     tp((cn(tA),          false), (n,                 false), cn(tA))
    ,tp((cn(tA),          true),  (n,                 false), wn(tA, n))
    ,tp((cn(tA),          false), (n,                 true),  cn(tA))
    ,tp((cn(tA),          true),  (n,                 true),  wn(tA, e))

    ,tp((cn(tA),          false), (e,                 false), cn(tA))
    ,tp((cn(tA),          true),  (e,                 false), wn(tA, e))
    ,tp((cn(tA),          false), (e,                 true),  cn(tA))
    ,tp((cn(tA),          true),  (e,                 true),  wn(tA, e))

    ,tp((cn(tA),          false), (p(tB),             false), cn(tA))
    ,tp((cn(tA),          true),  (p(tB),             false), wn(tA, p(tB)))
    ,tp((cn(tA),          false), (p(tB),             true),  cn(tA))
    ,tp((cn(tA),          true),  (p(tB),             true),  wn(tA, e))

    ,tp((cn(tC),          false), (wn(tB, cn(tIB)),   false), cn(tA))
    ,tp((cn(tC),          true),  (wn(tB, cn(tIB)),   false), wn(tA, cn(tIB)))
    ,tp((cn(tC),          false), (wn(tB, cn(tIB)),   true),  wn(tA, cn(tC)))
    ,tp((cn(tC),          true),  (wn(tB, cn(tIB)),   true),  wn(tA, e))

    ,tp((wn(tA, cn(tB)),  false), (wn(tA, cn(tC)),    false), cn(tA))
    ,tp((wn(tA, cn(tB)),  true),  (wn(tA, cn(tC)),    false), wn(tA, cn(tC)))
    ,tp((wn(tA, cn(tB)),  false), (wn(tA, cn(tC)),    true),  wn(tA, cn(tB)))
    ,tp((wn(tA, cn(tB)),  true),  (wn(tA, cn(tC)),    true),  wn(tA, e))

    ,tp((wn(tA, cn(tB)),  false), (wn(tObj, cn(tD)),  false), cn(tObj))
    ,tp((wn(tA, cn(tB)),  true),  (wn(tObj, cn(tD)),  false), wn(tObj, cn(tD)))
    ,tp((wn(tA, cn(tB)),  false), (wn(tObj, cn(tD)),  true),  wn(tObj, cn(tB)))
    ,tp((wn(tA, cn(tB)),  true),  (wn(tObj, cn(tD)),  true),  wn(tObj, e))

    ,tp((wn(tB, n),       false), (wn(tC, c(tC)),     false), wn(tA, cn(tC)))
    ,tp((wn(tB, n),       true),  (wn(tC, c(tC)),     false), wn(tA, c(tC)))
    ,tp((wn(tB, n),       false), (wn(tC, c(tC)),     true),  wn(tA, n))
    ,tp((wn(tB, n),       true),  (wn(tC, c(tC)),     true),  wn(tA, e))

    ,tp((w(tI, c(tIB)),   false), (w(tI, c(tJB)),     false), c(tI))
    ,tp((w(tI, c(tIBB)),  false), (w(tI, c(tJIB)),    false), w(tI, c(tIB)))

  )) ++ withCHA(tB, tC)(Seq(
     tp((pn(tB),          false), (c(tC),             true),  wn(tA, pn(tB)))
    ,tp((p(tB),           false), (p(tC),             true),  wc(tA, p(tB)))
  ))

  for ((((t1, t2, tUnion), pos), chaTypes) <- unionOnEdgesInfo) {
    test(s"unionOnEdges: $t1 & $t2 (cha: $chaTypes)") {
      resetCHASeq(chaTypes)

      val toe1 = TypeOnEdge(t1._1, t1._2)
      val toe2 = TypeOnEdge(t2._1, t2._2)

      (toe1 union toe2).tpe should beTA (tUnion)
      (toe2 union toe1).tpe should beTA (tUnion)
    }
  }

  val compareInfo = withCHA()(Seq(
     tp(e,            e,                  Equal)
    ,tp(e,            n,                  Less)
    ,tp(e,            cn(tA),             Less)
    ,tp(e,            pn(tA),             Less)

    ,tp(n,            cn(tA),             Less)
    ,tp(n,            c(tA),              Incomparable)
    ,tp(n,            pn(tA),             Less)
    ,tp(n,            p(tA),              Incomparable)

    ,tp(pn(tB),       pn(tC),             PartiallyEqual)
    ,tp(pn(tB),       p(tC),              Incomparable)
    ,tp(p(tB),        p(tC),              Incomparable)
    ,tp(pn(tA),       pn(tB),             PartiallyEqual)
    ,tp(pn(tA),       p(tA),              Greater)

    ,tp(cn(tB),       cn(tC),             PartiallyEqual)
    ,tp(cn(tB),       c(tC),              Incomparable)
    ,tp(c(tB),        c(tC),              Incomparable)

    ,tp(cn(tA),       cn(tB),             Greater)
    ,tp(c(tA),        cn(tB),             PartiallyEqual)
    ,tp(cn(tA),       c(tB),              Greater)
    ,tp(c(tA),        c(tB),              Greater)
    ,tp(cn(tA),       c(tA),              Greater)

    ,tp(cn(tI),       cn(tC),             PartiallyEqual)
    ,tp(cn(tI),       c(tC),              PartiallyEqual)
    ,tp(c(tI),        cn(tC),             PartiallyEqual)
    ,tp(c(tI),        c(tC),              PartiallyEqual)

    ,tp(cn(tB),       pn(tC),             PartiallyEqual)
    ,tp(cn(tB),       p(tC),              Incomparable)
    ,tp(c(tB),        pn(tC),             Incomparable)
    ,tp(c(tB),        p(tC),              Incomparable)

    ,tp(cn(tA),       pn(tB),             Greater)
    ,tp(c(tA),        pn(tB),             PartiallyEqual)
    ,tp(cn(tA),       p(tB),              Greater)
    ,tp(c(tA),        p(tB),              Greater)

    ,tp(cn(tA),       pn(tA),             Greater)
    ,tp(c(tA),        pn(tA),             PartiallyEqual)
    ,tp(cn(tA),       p(tA),              Greater)
    ,tp(c(tA),        p(tA),              Greater)

    ,tp(cn(tI),       pn(tA),             PartiallyEqual)
    ,tp(cn(tI),       p(tA),              Incomparable)
    ,tp(c(tI),        pn(tA),             Incomparable)
    ,tp(c(tI),        p(tA),              Incomparable)

    ,tp(cn(tI),       pn(tIB),            Greater)
    ,tp(c(tI),        pn(tIB),            PartiallyEqual)
    ,tp(c(tI),        pn(tInt1D),         Incomparable)
    ,tp(cn(tAI),      pn(tInt1D),         Greater)
    ,tp(c(tI1D),      p(tIB1D),           Greater)
    ,tp(c(tI1D),      p(tA1D),            Incomparable)

    ,tp(wn(tB, cn(tJB)), wn(tB, cn(tIB)), Equal)
    ,tp(cn(tB),       wn(tB, cn(tIB)),    Equal)
    ,tp(n,            wn(tB, cn(tJB)),    Less)
    ,tp(cn(tObj),     wn(tA, cn(tC)),     Greater)
    ,tp(cn(tC),       wn(tB, e),          PartiallyEqual)
    ,tp(c(tB),        wn(tC, n),          Incomparable)

  )) ++ withCHA(tB, tCF)(Seq(
     tp(p(tObj),      cc(tA),             Incomparable)
    ,tp(p(tC),        cc(tA, 1),          Incomparable)
    ,tp(p(tB),        cc(tC, 2),          Incomparable)
    ,tp(p(tC),        cc(tA, 2),          Less)

  )) ++ withCHA(tIBB, tJB, tCF)(Seq(
     tp(cc(tA, 2),    cc(tIB, 2),         Incomparable)
    ,tp(cc(tA, 3),    cc(tIB, 2),         PartiallyEqual)
    ,tp(cc(tA, 4),    cc(tB, 2),          Greater)
    ,tp(cc(tB),       cc(tC),             Incomparable)
    ,tp(cc(tA, 2),    cc(tA),             Less)

    ,tp(c(tCF),       cc(tA, 2),          Incomparable)
    ,tp(c(tB),        cc(tA, 3),          PartiallyEqual)
    ,tp(c(tC),        cc(tB),             Incomparable)
    ,tp(c(tObj),      cc(tA, 2),          Greater)
    ,tp(c(tI),        cc(tA, 2),          Incomparable)
    ,tp(c(tI),        cc(tA, 3),          PartiallyEqual)

    ,tp(c(tI),        cc(tObj),           PartiallyEqual)

  )) ++ withCHA(tB, tC)(Seq(
     tp(c(tC),        cc(tA, 2),          PartiallyEqual)
  ))

  for ((((t1, t2, c), pos), chaTypes) <- compareInfo) {
    test(s"compare: $t1 & $t2 (cha: $chaTypes)") {
      resetCHASeq(chaTypes)

      (t1 compare t1) should be (Equal)
      (t2 compare t2) should be (Equal)
      (t1 compare t2) should be (c)
      (t2 compare t1) should be (c.inverse)

      val tUnion = t1 union t2
      (t1 compare tUnion) should (be (Less) or be (Equal))
      (t2 compare tUnion) should (be (Less) or be (Equal))

      c match {
        case Equal =>
          tUnion.safeType should beTA (t1.safeType)
          tUnion.safeType should beTA (t2.safeType)
        case Greater =>
          tUnion.safeType should beTA (t1.safeType)
          tUnion.safeType shouldNot beTA (t2.safeType)
        case Less =>
          tUnion.safeType shouldNot beTA (t1.safeType)
          tUnion.safeType should beTA (t2.safeType)
        case _ =>
          tUnion.safeType shouldNot beTA (t1.safeType)
          tUnion.safeType shouldNot beTA (t2.safeType)
      }
    }
  }

  val intersectInfo = ((withCHA()(Seq(
     tp(e,                  e,                e)
    ,tp(e,                  n,                e)
    ,tp(e,                  cn(tA),           e)
    ,tp(e,                  pn(tA),           e)

    ,tp(n,                  cn(tA),           n)
    ,tp(n,                  c(tA),            e)
    ,tp(n,                  pn(tA),           n)
    ,tp(n,                  p(tA),            e)

    ,tp(pn(tB),             pn(tC),           n)
    ,tp(pn(tB),             p(tC),            e)
    ,tp(p(tB),              p(tC),            e)
    ,tp(pn(tA),             pn(tB),           n)
    ,tp(pn(tA),             p(tA),            p(tA))

    ,tp(cn(tB),             cn(tC),           n)
    ,tp(cn(tB),             c(tC),            e)
    ,tp(c(tB),              c(tC),            e)

    ,tp(cn(tA),             cn(tB),           cn(tB))
    ,tp(c(tA),              cn(tB),           c(tB))
    ,tp(cn(tA),             c(tB),            c(tB))
    ,tp(c(tA),              c(tB),            c(tB))
    ,tp(cn(tA),             c(tA),            c(tA))

    ,tp(cn(tB),             pn(tC),           n)
    ,tp(cn(tB),             p(tC),            e)
    ,tp(c(tB),              pn(tC),           e)
    ,tp(c(tB),              p(tC),            e)

    ,tp(cn(tA),             pn(tB),           pn(tB))
    ,tp(c(tA),              pn(tB),           p(tB))
    ,tp(cn(tA),             p(tB),            p(tB))
    ,tp(c(tA),              p(tB),            p(tB))

    ,tp(cn(tA),             pn(tA),           pn(tA))
    ,tp(c(tA),              pn(tA),           p(tA))
    ,tp(cn(tA),             p(tA),            p(tA))
    ,tp(c(tA),              p(tA),            p(tA))

    ,tp(cn(tI),             pn(tA),           n)
    ,tp(cn(tI),             p(tA),            e)
    ,tp(c(tI),              pn(tA),           e)
    ,tp(c(tI),              p(tA),            e)
    ,tp(c(tI),              c(tObj),          c(tI))
    ,tp(c(tI),              c(tJ),            c(tJ))

    ,tp(cn(tI),             pn(tIB),          pn(tIB))
    ,tp(c(tI),              pn(tIB),          p(tIB))
    ,tp(c(tI),              pn(tInt1D),       e)
    ,tp(cn(tAI),            pn(tInt1D),       pn(tInt1D))
    ,tp(c(tI1D),            p(tIB1D),         p(tIB1D))
    ,tp(c(tI1D),            p(tA1D),          e)

    ,tp(wn(tA, pn(tB)),     wn(tA, pn(tC)),   wn(tA, n))
    ,tp(wn(tA, p(tB)),      wn(tA, pn(tC)),   wn(tA, e))
    ,tp(wn(tA, pn(tCF)),    cn(tC),           wn(tC, pn(tCF)))
    ,tp(wn(tA, pn(tB)),     cn(tC),           wn(tC, n))
    ,tp(wn(tA, p(tB)),      cn(tC),           wn(tC, e))
    ,tp(wn(tA, pn(tB)),     pn(tB),           pn(tB))

    ,tp(wn(tObj, cn(tI)),   cn(tK),           cn(tK))
    ,tp(wn(tObj, cn(tJ)),   cn(tI),           wn(tI, cn(tJ)))

  )) ++ withCHA(tIBB, tCF, tD)(Seq(
     tp(cc(tA, 3),          cc(tB, 3),        cc(tB, 2))
    ,tp(cc(tA, 2),          cc(tC, 2),        p(tC))
    ,tp(cc(tA, 2),          cc(tCF),          e)
    ,tp(cc(tA),             c(tJ),            e)
    ,tp(cc(tA, 2),          c(tI),            e)
    ,tp(cc(tA, 3),          c(tI),            p(tIB))
    ,tp(cc(tA, 4),          c(tI),            cc(tIB))
    ,tp(cc(tA, 2),          c(tB),            p(tB))

  )) ++ withCHA(tIBB, tJB)(Seq(
     tp(cc(tA),             c(tJ),            p(tJB))

  ))) map { case (((t1, t2, i), pos), chaTypes) => (((Seq(t1, t2), i), pos), chaTypes) }) ++ withCHA()(Seq(
     tp(Seq(c(tObj), c(tA), c(tB)),           c(tB))
    ,tp(Seq(c(tObj), c(tB), c(tC)),           e)
    ,tp(Seq(c(tI), c(tA), p(tJBF)),           p(tJBF))
    ,tp(Seq(c(tI), c(tK), c(tIKB)),           c(tIKB))

  )) ++ withCHA(tIKB, tJB)(Seq(
     tp(Seq(c(tI),   c(tK),   cc(tB)),        cc(tIKB))
  ))

  def checkIntersect(ts: Seq[ReferenceApprox], expectedResultOpt: Option[ReferenceApprox], expectedStrict: Boolean): Unit = {
    val (r, s) = ReferenceApprox.weakIntersect(ts: _*)
    for (expectedResult <- expectedResultOpt) {
      r should beTA (expectedResult)
    }
    s should be (expectedStrict)

    if (s) {
      ts forall { _ compare r match { case Greater | Equal => true; case _ => false } } should be (true)
    } else {
      ts forall { _ compare r match { case Greater | Equal | PartiallyEqual => true; case _ => false } } should be (true)
      ts exists { _ compare r match { case PartiallyEqual => true; case _ => false } } should be (true)
    }
  }

  for ((((ts, i), pos), chaTypes) <- intersectInfo) {
    test(s"intersect: $ts (cha: $chaTypes)") {
      resetCHASeq(chaTypes)

      for (t <- ts) {
        checkIntersect(Seq(t, t), Some(t), true)
      }
      for (tsp <- ts.permutations) {
        checkIntersect(tsp, Some(i), true)
      }
    }
  }

  val intersectNonStrict = ((withCHA()(Seq(
     tp(cn(tI),   cn(tC),   cn(tC))
    ,tp(cn(tI),   c(tC),    c(tC))
    ,tp(c(tI),    cn(tC),   c(tC))
    ,tp(cn(tC),   c(tI),    c(tC))
    ,tp(c(tI),    c(tC),    c(tC))
    ,tp(c(tI),    c(tK),    c(tI))
    ,tp(c(tK),    c(tI),    c(tK))
    ,tp(c(tI1D),  c(tA1D),  c(tA1D))
    ,tp(c(tA1D),  c(tI1D),  c(tA1D))
    ,tp(c(tI1D),  c(tK1D),  c(tI1D))
    ,tp(c(tK1D),  c(tI1D),  c(tK1D))

  )) ++ withCHA(tIBB, tJB)(Seq(
     tp(cc(tA),   c(tI),    cc(tB)) // strict result: cc(tIB) union p(tJB)
    ,tp(cc(tB),   c(tI),    cc(tB)) // strict result: cc(tIB) union p(tJB)

  ))) map { case (((t1, t2, i), pos), chaTypes) => (((Seq(t1, t2), i), pos), chaTypes) }) ++ withCHA()(Seq(
     tp(Seq(c(tI), c(tA), c(tK)),    c(tA))
    ,tp(Seq(c(tI), c(tK), c(tIB)),   c(tIB))
    ,tp(Seq(c(tI), c(tK), c(tJ)),    c(tJ))
    ,tp(Seq(c(tK), c(tI), c(tJ)),    c(tK))
    ,tp(Seq(c(tJ), c(tK), c(tI)),    c(tJ))
  ))

  for ((((ts, i), pos), chaTypes) <- intersectNonStrict) {
    test(s"intersect non strict: $ts (cha: $chaTypes)") {
      resetCHASeq(chaTypes)

      for (t <- ts) {
        checkIntersect(Seq(t, t), Some(t), true)
      }
      checkIntersect(ts, Some(i), false)
      for (tsp <- ts.permutations) {
        // They must be non-strict but result might differ.
        checkIntersect(tsp, None, false)
      }
    }
  }

  val subtractStrict = withCHA()(Seq(
     tp(c(tA),          e,        c(tA))
    ,tp(c(tA),          n,        c(tA))
    ,tp(cn(tA),         n,        c(tA))
    ,tp(cn(tA),         c(tA),    n)
    ,tp(c(tA),          c(tA),    e)
    ,tp(c(tB),          c(tA),    e)
    ,tp(cn(tI),         c(tObj),  n)
    ,tp(pn(tB),         cn(tA),   e)
    ,tp(p(tB),          c(tA),    e)
    ,tp(p(tIB),         c(tI),    e)
    ,tp(p(tIB),         c(tJ),    p(tIB))
    ,tp(p(tB),          p(tC),    p(tB))
    ,tp(p(tB),          pn(tC),   p(tB))
    ,tp(pn(tB),         p(tC),    pn(tB))
    ,tp(e,              c(tA),    e)

    ,tp(wn(tA, p(tB)),  c(tA),    n)

  )) ++ withCHA(tB)(Seq(
     tp(cc(tA, 2),      c(tA),    e)
  ))

  for ((((t1, t2, s), pos), chaTypes) <- subtractStrict) {
    test(s"subtract strict: $t1 & $t2 (cha: $chaTypes)") {
      resetCHASeq(chaTypes)
      (t1 subtract t2) should beTA ((s, true))
      (t1 subtract t1) should beTA ((e, true))
      (t2 subtract t2) should beTA ((e, true))

      // X sub Y == X sub (Y int X)
      val (i, istrict) = (t1 weakIntersect t2)
      istrict should be (true)
      (t1 subtract i) should beTA ((s, true))
    }
  }

  val subtractNotStrict = withCHA()(Seq(
     tp(c(tA),          c(tB),      c(tA))
    ,tp(c(tA),          p(tB),      c(tA))
    ,tp(c(tI),          c(tA),      c(tI))
    ,tp(wn(tA, p(tB)),  p(tB),      wn(tA, e))
    ,tp(wn(tA, pn(tB)), p(tB),      wn(tA, n))

  )) ++ withCHA(tIBB)(Seq(
     tp(cc(tB, 2),      c(tIB),     cc(tB, 2)) // might be p(tB)
    ,tp(cc(tB, 2),      cc(tA, 2),  cc(tB, 2)) // might be p(tIB)
  ))

  for ((((t1, t2, s), pos), chaTypes) <- subtractNotStrict) {
    test(s"subtract not strict: $t1 & $t2 (cha: $chaTypes)") {
      resetCHASeq(chaTypes)
      (t1 subtract t2) should beTA ((s, false))
      (t1 subtract t1) should beTA ((e, true))
      (t2 subtract t2) should beTA ((e, true))
    }
  }

  object CHABit extends (ReferenceApprox => (ReferenceApprox, Boolean)) {
    override def toString = "CHABit"
    def apply(t: ReferenceApprox) = t.filterClosed()
  }
  case class Level(l: Int) extends (ReferenceApprox => (ReferenceApprox, Boolean)) {
    override def toString = s"Level($l)"
    def apply(t: ReferenceApprox) = t.filterLevel(l)
  }

  val filteringBits = withCHA(tIB)(Seq(
    // because of arrays
     tp(CHABit,    c(tObj1D),      c(tObj1D),         false)
    ,tp(Level(9),  c(tObj1D),      c(tObj1D),         false)
    ,tp(CHABit,    p(tInt1D),      p(tInt1D),         false)
    ,tp(Level(9),  p(tInt1D),      p(tInt1D),         false)

    // despite of arrays
    ,tp(CHABit,    c(tObj),        cc(tObj),          true)
    ,tp(Level(9),  c(tObj),        cc(tObj),          true)

  )) ++ withCHA(tObj)(Seq(
     tp(CHABit,    p(tObj),        p(tObj),           true)
    ,tp(Level(1),  p(tObj),        e,                 true)
    ,tp(Level(3),  p(tObj),        p(tObj),           true)

  )) ++ withCHA(tIB, tJB, tC)(Seq(
     tp(CHABit,    c(tIB),         cc(tIB),           true)
    ,tp(CHABit,    c(tB),          cc(tB),            true)
    ,tp(CHABit,    p(tB),          p(tB),             true)
    ,tp(CHABit,    e,              e,                 true)
    ,tp(CHABit,    c(tA),          cc(tA),            true)

    ,tp(CHABit,    c(tI),          cc(tB),            false)

  )) ++ withCHA(tIBB, tJB, tC)(Seq(
     tp(Level(6),  c(tA),          cc(tA, 3),         true)
    ,tp(Level(4),  c(tA),          p(tA),             true)
    ,tp(Level(5),  c(tB),          p(tB),             true)
    ,tp(Level(5),  c(tIB),         e,                 true)
    ,tp(CHABit,    c(tJB),         p(tJB),            true)

    ,tp(CHABit,    c(tI),          cc(tB),            false)
    ,tp(Level(6),  c(tI),          cc(tB, 2),         false)

    ,tp(CHABit,    w(tA, c(tJB)),  wc(tA,  cc(tJB)),  true)
  ))

  for ((((f, t, r, s), pos), chaTypes) <- filteringBits) {
    test(s"filtering: $f filters $t (cha: $chaTypes)") {
      resetCHASeq(chaTypes)
      f(t) should beTA (r, s)
    }
  }

  def methodGuardByHost(targetHost: ReferenceType) = {
    val m = makeSymMethod("bar", targetHost).setStatic(false)
    MethodGuard(m.getMethodReference, m)
  }

  val guardsFiltering = withCHA(tIBB, tJB, tC)(Seq(
    // ignore nulls
     tp(CHABitGuard,  cn(tIB),  (cc(tIB).withNull, true),  (cn(tIB), false))
    ,tp(CHABitGuard,  pn(tIB),  (pn(tIB).withNull, true),  (n,       true ))

    ,tp(CHABitGuard,  p(tIB),          (p(tIB),          true ),  (e,              true ))
    ,tp(CHABitGuard,  c(tIB),          (cc(tIB),         true ),  (c(tIB),         false))
    ,tp(CHABitGuard,  c(tB),           (cc(tB),          true ),  (c(tB),          false))
    ,tp(CHABitGuard,  cc(tB),          (cc(tB),          true ),  (e,              true ))
    ,tp(CHABitGuard,  c(tObj),         (cc(tObj),        true ),  (c(tObj),        false))
    ,tp(CHABitGuard,  w(tA, c(tIB)),   (wc(tA, cc(tIB)), true ),  (w(tA, c(tIB)),  false))

    ,tp(PointGuard(tB),    p(tA),           (e,       true ),  (p(tA),         true ))
    ,tp(PointGuard(tB),    p(tB),           (p(tB),   true ),  (e,             true ))
    ,tp(PointGuard(tB),    c(tA),           (p(tB),   true ),  (c(tA),         false))
    ,tp(PointGuard(tIBB),  cc(tIB),         (p(tIBB), true ),  (cc(tIB),       false)) // imperfect subtraction
    ,tp(PointGuard(tIBB),  w(tA, p(tJB)),   (p(tIBB), true ),  (w(tA, p(tJB)), false)) // point ignores probable part

    ,tp(MaxClosedConeGuard(tB),  c(tIB),    (cc(tIB), true ),  (c(tIB),        false))

  )) ++ withCHA(tIBB, tJB, tCF)(Seq(
     tp(LevelGuard(4),  p(tA),           (p(tA),             true ),  (e,             true ))
    ,tp(LevelGuard(5),  p(tA),           (p(tA),             true ),  (e,             true ))
    ,tp(LevelGuard(5),  p(tC),           (p(tC),             true ),  (e,             true ))
    ,tp(LevelGuard(7),  c(tC),           (cc(tC),            true ),  (c(tC),         false))
    ,tp(LevelGuard(6),  c(tIB),          (p(tIB),            true ),  (c(tIB),        false))
    ,tp(LevelGuard(6),  cc(tB),          (cc(tB, 2),         true ),  (cc(tB),        false)) // imperfect subtraction
    ,tp(LevelGuard(6),  c(tA),           (cc(tA, 3),         true ),  (c(tA),         false))
    ,tp(LevelGuard(6),  w(tB, c(tIB)),   (wc(tB, 2, p(tIB)), true ),  (w(tB, c(tIB)), false))

    ,tp(MaxClosedConeGuard(tB),  c(tA),         (cc(tB),    true ),  (c(tA),        false))
    ,tp(MaxClosedConeGuard(tB),  cc(tB),        (cc(tB),    true ),  (e,            true ))
    ,tp(MaxClosedConeGuard(tA),  c(tObj),       (cc(tA),    true ),  (c(tObj),      false))
    ,tp(MaxClosedConeGuard(tB),  c(tObj),       (cc(tB),    true ),  (c(tObj),      false))
    ,tp(MaxClosedConeGuard(tB),  c(tC),         (e,         true ),  (c(tC),        true ))
    ,tp(MaxClosedConeGuard(tB),  c(tIB),        (cc(tIB),   true ),  (c(tIB),       false))
    ,tp(MaxClosedConeGuard(tB),  w(tA, p(tC)),  (wc(tB, e), true ),  (w(tA, p(tC)), false))
    ,tp(MaxClosedConeGuard(tB),  w(tA, c(tB)),  (cc(tB),    true ),  (w(tA, c(tB)), false))
    ,tp(MaxClosedConeGuard(tC),  w(tA, p(tC)),  (wc(tC, p(tC)), true),  (w(tA, e),  false))

    ,tp(OpenConeGuard(tB),  c(tA),         (c(tB),         true ),  (c(tA),         false))
    ,tp(OpenConeGuard(tB),  c(tB),         (c(tB),         true ),  (e,             true ))
    ,tp(OpenConeGuard(tB),  cc(tA),        (cc(tB),        true ),  (cc(tA),        false))
    ,tp(OpenConeGuard(tB),  cc(tB),        (cc(tB),        true ),  (e,             true ))
    ,tp(OpenConeGuard(tA),  c(tObj),       (c(tA),         true ),  (c(tObj),       false))
    ,tp(OpenConeGuard(tB),  c(tObj),       (c(tB),         true ),  (c(tObj),       false))
    ,tp(OpenConeGuard(tB),  c(tC),         (e,             true ),  (c(tC),         true ))
    ,tp(OpenConeGuard(tB),  c(tIB),        (c(tIB),        true ),  (e,             true ))
    ,tp(OpenConeGuard(tB),  wc(tA, p(tC)), (wc(tB, e),     true ),  (wc(tA, p(tC)), false))
    ,tp(OpenConeGuard(tB),  w(tA, c(tB)),  (c(tB),         true ),  (w(tA, e),      false))
    ,tp(OpenConeGuard(tB),  w(tA, cc(tB)), (w(tB, cc(tB)), true ),  (w(tA, e),      false))

  )) ++ withCHA()(Seq(
     tp(methodGuardByHost(tA),   c(tObj),  (c(tA),  false),  (c(tObj), false))
    ,tp(methodGuardByHost(tI),   c(tObj),  (c(tI),  false),  (c(tObj), false))
    ,tp(methodGuardByHost(tB),   c(tC),    (e,      true),   (c(tC),   true))

  )) ++ withCHA(tIBB, tC)(Seq(
     tp(CHABitGuard,    c(tI),       (cc(tIB),   true),    (c(tI), false))
    ,tp(LevelGuard(6),  c(tI),       (p(tIB),    true),    (c(tI), false))
  )) ++ withCHA(tIBB, tJB, tC)(Seq(
     tp(CHABitGuard,    c(tI),       (cc(tB),    false),   (c(tI), false))
    ,tp(LevelGuard(6),  c(tI),       (cc(tB, 2), false),   (c(tI), false))
  )) ++ withCHA(tIBB, tC, tIX)(Seq(
     tp(CHABitGuard,    c(tI),       (cc(tObj),    false),   (c(tI), false))
    ,tp(LevelGuard(6),  c(tI),       (cc(tObj, 4), false),   (c(tI), false))
  ))

  for ((((g, t, i, s), pos), chaTypes) <- guardsFiltering) {
    test(s"guards filtering: $g filters $t (cha: $chaTypes)") {
      resetCHASeq(chaTypes)
      g.intersectWith(t) should beTA (i)
      g.subtractFrom(t) should beTA (s)
    }
  }

  val guardsIntersection = withCHA(tIBB, tJB, tCF)(Seq(
    // context free
     tp(CHABitGuard, PointGuard(tIB),         c(tObj), Some(PointGuard(tIB)))
    ,tp(CHABitGuard, MaxClosedConeGuard(tIB), c(tObj), Some(MaxClosedConeGuard(tIB)))
    ,tp(CHABitGuard, OpenConeGuard(tIB),      c(tObj), Some(MaxClosedConeGuard(tIB)))
    ,tp(CHABitGuard, OpenConeGuard(tIBB),     c(tObj), Some(PointGuard(tIBB)))

    ,tp(LevelGuard(7), PointGuard(tC),          c(tObj), Some(PointGuard(tC)))
    ,tp(LevelGuard(7), LevelGuard(6),           c(tObj), Some(LevelGuard(6)))
    ,tp(LevelGuard(7), MaxClosedConeGuard(tC),  c(tObj), Some(MaxClosedConeGuard(tC)))
    ,tp(LevelGuard(7), OpenConeGuard(tC),       c(tObj), Some(MaxClosedConeGuard(tC)))
    ,tp(LevelGuard(7), MaxClosedConeGuard(tB),  c(tObj), Some(MaxClosedConeGuard(tB)))
    ,tp(LevelGuard(7), OpenConeGuard(tB),       c(tObj), Some(MaxClosedConeGuard(tB)))
    ,tp(LevelGuard(6), MaxClosedConeGuard(tIB), c(tObj), Some(PointGuard(tIB)))
    ,tp(LevelGuard(6), OpenConeGuard(tIB),      c(tObj), Some(PointGuard(tIB)))
    ,tp(LevelGuard(5), PointGuard(tIB),         c(tObj), None)
    ,tp(LevelGuard(5), MaxClosedConeGuard(tIB), c(tObj), None)
    ,tp(LevelGuard(5), OpenConeGuard(tIB),      c(tObj), None)

    ,tp(MaxClosedConeGuard(tA), PointGuard(tB),         c(tObj), Some(PointGuard(tB)))
    ,tp(MaxClosedConeGuard(tA), MaxClosedConeGuard(tB), c(tObj), Some(MaxClosedConeGuard(tB)))
    ,tp(MaxClosedConeGuard(tA), OpenConeGuard(tB),      c(tObj), Some(MaxClosedConeGuard(tB)))
    ,tp(MaxClosedConeGuard(tB), PointGuard(tC),         c(tObj), None)
    ,tp(MaxClosedConeGuard(tB), MaxClosedConeGuard(tC), c(tObj), None)
    ,tp(MaxClosedConeGuard(tB), OpenConeGuard(tC),      c(tObj), None)
    ,tp(MaxClosedConeGuard(tC), OpenConeGuard(tCF),     c(tObj), Some(PointGuard(tCF)))

    ,tp(OpenConeGuard(tA), PointGuard(tB),         c(tObj), Some(PointGuard(tB)))
    ,tp(OpenConeGuard(tA), MaxClosedConeGuard(tB), c(tObj), Some(MaxClosedConeGuard(tB)))
    ,tp(OpenConeGuard(tA), OpenConeGuard(tB),      c(tObj), Some(OpenConeGuard(tB)))
    ,tp(OpenConeGuard(tB), PointGuard(tC),         c(tObj), None)
    ,tp(OpenConeGuard(tB), OpenConeGuard(tC),      c(tObj), None)

    ,tp(PointGuard(tA), PointGuard(tB), c(tObj), None)

    // context dependent
    ,tp(LevelGuard(7),     MaxClosedConeGuard(tA), c(tB),  Some(LevelGuard(7)))
    ,tp(LevelGuard(6),     CHABitGuard,            c(tIB), Some(PointGuard(tIB)))
    ,tp(LevelGuard(6),     MaxClosedConeGuard(tB), c(tIB), Some(PointGuard(tIB)))
    ,tp(LevelGuard(6),     OpenConeGuard(tA),      c(tIB), Some(PointGuard(tIB)))
    ,tp(CHABitGuard,       OpenConeGuard(tIB),     c(tIB), Some(CHABitGuard))
    ,tp(CHABitGuard,       OpenConeGuard(tA),      c(tIB), Some(CHABitGuard))
    ,tp(OpenConeGuard(tA), MaxClosedConeGuard(tB), c(tIB), Some(MaxClosedConeGuard(tB)))

    // choosing simplest guard
    ,tp(LevelGuard(7),     CHABitGuard,             c(tIB), Some(CHABitGuard))
    ,tp(CHABitGuard,       MaxClosedConeGuard(tB),  c(tIB), Some(CHABitGuard))
    ,tp(OpenConeGuard(tA), MaxClosedConeGuard(tB),  cc(tB), Some(MaxClosedConeGuard(tB)))

    // perfections (were imperfections)
    ,tp(LevelGuard(7), MaxClosedConeGuard(tIB), c(tIB),   Some(LevelGuard(7)))
    ,tp(LevelGuard(7), MaxClosedConeGuard(tA), c(tObj),   Some(MaxClosedConeGuard(tA)))
    ,tp(LevelGuard(7), OpenConeGuard(tA),      c(tObj),   Some(MaxClosedConeGuard(tA)))
    ,tp(CHABitGuard,   LevelGuard(6),          cc(tA, 3), Some(CHABitGuard))
    ,tp(CHABitGuard,   MaxClosedConeGuard(tA), cc(tA, 3), Some(CHABitGuard))
    ,tp(LevelGuard(6), MaxClosedConeGuard(tA), cc(tA, 3), Some(LevelGuard(6)))

    ,{
      val m = methodGuardByHost(tA)
      tp(m, LevelGuard(6),  cc(tA, 3), Some(m))
    }

    // imperfections
    ,tp(methodGuardByHost(tA), CHABitGuard,             cc(tB, 2), None)
    ,tp(methodGuardByHost(tA), MaxClosedConeGuard(tIB), cc(tA, 3), None)
    ,tp(methodGuardByHost(tA), OpenConeGuard(tIB),      cc(tA, 3), None)
  ))

  for ((((g1, g2, t, r), pos), chaTypes) <- guardsIntersection) {
    test(s"guards intersection: $g1 intersect $g2 with type $t (cha: $chaTypes)") {
      resetCHASeq(chaTypes)
      shouldIntersect(g1, g2, t, r)
    }
  }

  test("method guard intersection with point guard") {
    try {
      resetCHA(tIB)

      val m = makeSymMethod("foo", tA).setStatic(false).setAbstract(false)

      val mt = MethodGuard(m.getMethodReference, m)
      val pg = PointGuard(tIB)

      shouldIntersect(mt, pg, c(tObj), Some(pg))

      val mOverride = makeSymMethod("foo", tIB).setStatic(false)

      shouldIntersect(mt, pg, c(tObj), None)

    } finally {
      tIB.clearMethods()
      tA.clearMethods()
    }
  }

  private def shouldIntersect(g1: Guard, g2: Guard, inType: ReferenceApprox, res: Option[Guard]): Unit = {
    g1.intersectWith(g2, inType) shouldBe res
    g2.intersectWith(g1, inType) shouldBe res
  }

  for {
    (name, methods /*(declClass, isAbstract)*/, originHost, targetHost, testCases) <- Seq(
      (
        "virtual normal",
        Seq(
          (tA, true),
          (tB, false),
        ),
        tA, tB,
        withCHA(tIBB)(Seq(
          tp(p(tIB),    p(tIB),    true),
          tp(c(tIB),    c(tIB),    false),
          tp(c(tObj),   c(tB),     false),
          tp(c(tI),     c(tB),     false),
          tp(c(tB),     c(tB),     false),
          tp(cc(tA, 3), cc(tB, 2), false), // could be strict = true
        ))
      ),

      (
        "virtual override",
        Seq(
          (tA,  true),
          (tB,  false),
          (tIB, false),
        ),
        tA, tB,
        withCHA(tIBB)(Seq(
          tp(p(tIB),    e,         true),
          tp(c(tIB),    e,         true),
          tp(c(tObj),   c(tB),     false),
          tp(c(tI),     c(tB),     false),
          tp(c(tB),     c(tB),     false),
          tp(cc(tA, 2), p(tB),     true),
          tp(cc(tA, 3), cc(tB, 2), false), // could be (p(tB), true)
        ))
      ),

      (
        "interface normal",
        Seq(
          (tI,  true),
          (tB,  false),
          (tIB, false),
        ),
        tI, tIB,
        withCHA(tIBB)(Seq(
          tp(c(tObj),   c(tIB), false),
          tp(c(tI),     c(tIB), false),
          tp(c(tA),     c(tIB), false),
          tp(cc(tA, 3), p(tIB), true),
        ))
      ),

      (
        "interface normal inherited",
        Seq(
          (tI,  true),
          (tB,  false),
        ),
        tI, tB,
        withCHA(tIBB)(Seq(
          tp(c(tObj),   c(tB),     false),
          tp(c(tI),     c(tB),     false),
          tp(c(tA),     c(tB),     false),
          tp(cc(tA, 3), cc(tB, 2), false),
        ))
      ),

      (
        "interface abnormal",
        Seq(
          (tI,  true),
          (tB,  false),
          (tIB, false),
        ),
        tI, tB,
        withCHA(tIBB)(Seq(
          tp(c(tObj),   c(tB),     false),
          tp(c(tI),     c(tB),     false),
          tp(c(tA),     c(tB),     false),
          tp(c(tIB),    e,         true),
          tp(cc(tA, 3), cc(tB, 2), false), // could be (e, true)
        ))
      ),

      (
        "interface override",
        Seq(
          (tI,   true),
          (tJ,   false),
          (tB,   true),
          (tIB,  false),
          (tJB2, false),
        ),
        tI, tJ,
        withCHA(tIBB, tJB, tJB2)(Seq(
          tp(c(tObj),    c(tJ),     false),
          tp(c(tI),      c(tJ),     false),
          tp(c(tA),      c(tA),     false),
          tp(c(tIB),     c(tIB),    false), // could be (e, true), because tJ could not override tIB impl
          tp(cc(tIB, 2), e,         true),
          tp(cc(tA, 3),  cc(tB, 2), false), // could be (p(tJB), true)
        ))
      ),

      (
        "interface override by super method",
        Seq(
          (tI, false),
          (tJ, false),
          (tB, false),
        ),
        tI, tI,
        withCHA(tIBB)(Seq(
          tp(c(tObj),   c(tI), false),
          tp(c(tJ),     e,     true),
          tp(c(tA),     c(tA), false),
          tp(c(tIB),    e,     true),
          tp(cc(tA, 3), e,     true),
          tp(cc(tB, 2), e,     true),
        ))
      ),
    )

    (((tIn, tOut, strict), pos), chaTypes) <- testCases
  } {
    test(s"method guard intersection [$name]: tIn = $tIn") {
      try {
        var origin: FakeMethod = null
        var target: FakeMethod = null
        for ((host, isAbstract) <- methods) {
          val m = makeSymMethod("foo", host).setStatic(false).setAbstract(isAbstract)
          if (host == originHost) origin = m
          if (host == targetHost) target = m
        }
        resetCHASeq(chaTypes)

        val mg = MethodGuard(origin.getMethodReference, target)
        mg.intersectWith(tIn) shouldBe (tOut, strict)
      } finally {
        methods foreach (_._1.clearMethods())
      }
    }
  }

}

