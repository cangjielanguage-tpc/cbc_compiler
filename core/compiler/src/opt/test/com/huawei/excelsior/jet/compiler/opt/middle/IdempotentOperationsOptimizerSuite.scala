/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.codeemitter.BarrierKind.{LOAD_LOAD, LOAD_STORE, STORE_LOAD, STORE_STORE}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.types.References.ClosedCone
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, TypeKind}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.Primitive
import com.huawei.excelsior.jet.compiler.{CompilerSuite, symlevel}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeField, FakeMethod, FakeType}
import com.huawei.excelsior.jet.compiler.types.TypesToolbox

import scala.collection.mutable.ArrayBuffer

/**
 * Tests for IdempotentOperationsOptimizer class.
 */
class IdempotentOperationsOptimizerSuite extends CompilerSuite
                                            with GlobalNodesBuilder
                                            with TypesToolbox
                                            with IdempotentOperationsOptimizer {

  startPhase(CompilerPhase.PostInline)

  override def afterEach(): Unit = {
    typeProvider.getAllClasses foreach (_.asInstanceOf[FakeType].clearMethods())
    super.afterEach()
  }

  def clinit(b: Block) = makeNodes { at =>
    at(b)
    Clinit(symK)()
  }
  
  def nullCheck(b: Block, obj: Node) = makeNodes { at =>
    at(b)
    NullCheck(obj)
  }

  def packageInitCheck(b: Block) = makeNodes { at =>
    at(b)
    PackageInitCheck(symK)()
  }

  def idivxCheck(b: Block, divisor: Node) = makeNodes { at =>
    at(b)
    DivisorCheck()(divisor)
  }

  def putStatic(b: Block, field: symlevel.Field, value: Node) = makeNodes { at =>
    at(b)
    PutStatic(field)(value)
  }

  def memBarrier(b: Block, kind: BarrierKind) = makeNodes { at =>
    at(b)
    MemBarrier(Set(kind))()
  }

  def addMethodTo(host: FakeType) = makeSymMethod("foo", host).setStatic(false)

  test("simple clinit elimination") {
    makeCFG(0 -> 1)
    val nc0 = clinit(0)
    val nc1 = clinit(1)
    optimizeIdempotentOperations(useFilter = false)
    nc1.isCommitted should be (false)
    nc0.isCommitted should be (true)
  }

  test("simple null checks elimination") {
    makeCFG(0 -> 1)
    val obj = addObjNode()
    val nc0 = nullCheck(0, obj)
    val nc1 = nullCheck(1, obj)
    optimizeIdempotentOperations(useFilter = false)
    nc1.isCommitted should be (false)
    nc0.isCommitted should be (true)
  }

  test("simple package init checks elimination") {
    makeCFG(0 -> 1)
    val nc0 = packageInitCheck(0)
    val nc1 = packageInitCheck(1)
    optimizeIdempotentOperations(useFilter = false)
    nc1.isCommitted should be (false)
    nc0.isCommitted should be (true)
  }

  test("different key objects and idempotent types") {
    makeCFG(0 -> 1)
    val dead = new ArrayBuffer[Node]
    val live = new ArrayBuffer[Node]
    for (i <- 0 until 10) {
      val obj = addObjNode()
      live += nullCheck(0, obj)
      dead += nullCheck(1, obj)
    }
    for (i <- 0 until 10) {
      val obj = addObjNode()
      live += nullCheck(0, obj)
      dead += nullCheck(1, obj)
    }
    for (i <- 0 until 10) {
      live += nullCheck(1, addObjNode())
    }
    optimizeIdempotentOperations(useFilter = false)
    live foreach { _.isCommitted should be (true) }
    dead foreach { _.isCommitted should be (false) }
  }

  test("elimination on diamond") {
    makeCFG(0 -> (1 || 2) -> 3)

    val obj = addObjNode()
    val nc0 = nullCheck(0, obj)
    val nc1 = nullCheck(1, obj)
    val nc2 = nullCheck(2, obj)
    val nc3 = nullCheck(3, obj)
    optimizeIdempotentOperations(useFilter = false)
    nc1.isCommitted should be (false)
    nc2.isCommitted should be (false)
    nc3.isCommitted should be (false)
    nc0.isCommitted should be (true)
  }

  test("transitive domination") {
    makeCFG(0 -> 1 -> 2)
    val l = addNode(0)
    val divisor = addNode(0)
    val n2 = idivxCheck(2, divisor)
    val n1 = idivxCheck(1, divisor)
    val n0 = idivxCheck(0, divisor)
    val use = IDiv(IntType)(n2, l, divisor)
    optimizeIdempotentOperations(useFilter = false)
    n2.isCommitted should be (false)
    n1.isCommitted should be (false)
    n0.isCommitted should be (true)

    // now it is conservative control replacement, but context types should fix this
    use.asInstanceOf[HasInControl].inCtrl should be (2.block)
  }

  test("consecutive mem barriers") {
    makeCFG(0)
    val m0 = memBarrier(0, LOAD_LOAD)
    val m1 = memBarrier(0, LOAD_STORE)
    val m2 = memBarrier(0, STORE_LOAD)
    val m3 = memBarrier(0, STORE_STORE)
    optimizeConsecutiveMemBarriers()
    m3.isCommitted should be (false)
    m2.isCommitted should be (false)
    m1.isCommitted should be (false)
    m0.isCommitted should be (false)
    val barriers = all[MemBarrier].toList
    barriers.size should be (1)
    barriers.head.kinds should be (Set(LOAD_LOAD, LOAD_STORE, STORE_LOAD, STORE_STORE))
  }

  test("consecutive mem barriers with redefining mem field op in the middle") {
    makeCFG(0)
    val m0 = memBarrier(0, LOAD_LOAD)
    val m1 = memBarrier(0, LOAD_STORE)
    val m2 = memBarrier(0, STORE_LOAD)
    val f = new FakeField(`type` = Primitive(TypeKind.INT))
    val ps = putStatic(0, f, addNode())
    val m3 = memBarrier(0, STORE_STORE)
    optimizeConsecutiveMemBarriers()
    m3.isCommitted should be (true)
    m2.isCommitted should be (false)
    m1.isCommitted should be (false)
    m0.isCommitted should be (false)
    all[MemBarrier].size should be (2)
    ps.inMemory.asInstanceOf[MemBarrier].kinds should be (Set(LOAD_LOAD, LOAD_STORE, STORE_LOAD))
  }

  test("consecutive mem barriers with not redefining mem field op in the middle") {
    makeCFG(0)
    val m0 = memBarrier(0, LOAD_LOAD)
    val m1 = memBarrier(0, LOAD_STORE)
    val m2 = memBarrier(0, STORE_LOAD)
    val f = new FakeField(`type` = Primitive(TypeKind.INT))
    val gs = makeNodes { at =>
      at(0)
      GetStatic(f).asInstanceOf[GetStatic]
    }
    val m3 = memBarrier(0, STORE_STORE)
    optimizeConsecutiveMemBarriers()
    m3.isCommitted should be (true)
    m2.isCommitted should be (false)
    m1.isCommitted should be (false)
    m0.isCommitted should be (false)
    all[MemBarrier].size should be (2)
    gs.inMemory.asInstanceOf[MemBarrier].kinds should be (Set(LOAD_LOAD, LOAD_STORE, STORE_LOAD))
  }

  //////// Diamond dust ////////

  test("no diamond dust with other points") {
    makeCFG(0)
    val obj = addObjNode()
    val points = Set(tA, tB, tIB, tIBB) map (t => TauTest(PointGuard(t), TauInfo.Unknown, b(0), obj))

    optimizeDiamondDust()
    all[TauTest].toSet should be (points)
  }

  test("correct diamond dust") {
    resetCHA(tB, tD)
    val m = addMethodTo(tA)

    makeCFG(0)
    val obj = addObjNode(ClosedCone.max(tA, mayBeNull = false))
    val dustPoint = PointGuard(tB)
    val roguePoint = PointGuard(tD)
    for (g <- Seq(roguePoint, CHABitGuard, dustPoint, LevelGuard(4), MaxClosedConeGuard(tA), OpenConeGuard(tA), MethodGuard(m.getMethodReference, m))) {
      TauTest(g, TauInfo.Unknown, b(0), obj)
    }

    optimizeDiamondDust()
    for (t <- all[TauTest]) {
      t.obj should be (obj)
      t.guard should (be (roguePoint) or
                      matchPattern { case PointGuard(klass) if klass == tB.symType => })
    }
  }

  test("no diamond dust for different objs") {
    resetCHA(tB, tD)
    val m = addMethodTo(tA)

    makeCFG(0)
    val obj = addObjNode(ClosedCone.max(tA, mayBeNull = false))
    val roguePoint = TauTest(PointGuard(tA), TauInfo.Unknown, b(0), addObjNode())
    val guards = Seq(CHABitGuard, LevelGuard(4), MaxClosedConeGuard(tA), OpenConeGuard(tA), MethodGuard(m.getMethodReference, m)) map (TauTest(_, TauInfo.Unknown, b(0), obj))

    optimizeDiamondDust()
    all[TauTest].toSet should be (guards.toSet + roguePoint)
  }

  test("no diamond dust for incompatible tests") {
    resetCHA(tIB, tDD)
    val m = addMethodTo(tA)
    addMethodTo(tB)

    makeCFG(0)
    val obj = addObjNode(ClosedCone.max(tA, mayBeNull = false))
    val roguePoint = TauTest(PointGuard(tB), TauInfo.Unknown, b(0), obj)
    val guards = Seq(LevelGuard(3), MaxClosedConeGuard(tD), OpenConeGuard(tD), MethodGuard(m.getMethodReference, m)) map (TauTest(_, TauInfo.Unknown, b(0), obj))

    optimizeDiamondDust()
    all[TauTest].toSet should be (guards.toSet + roguePoint)
  }

  test("random diamond dust with multiple compatible points (JET-9338)") {
    resetCHA(tB)
    val m = addMethodTo(tA)

    makeCFG(0)
    val obj = addObjNode(ClosedCone.max(tA, mayBeNull = false))
    val aPoint = PointGuard(tA)
    val bPoint = PointGuard(tB)
    for (g <- Seq(aPoint, bPoint, LevelGuard(4), MaxClosedConeGuard(tA), OpenConeGuard(tA), MethodGuard(m.getMethodReference, m))) {
      TauTest(g, TauInfo.PGO(1, 1), b(0), obj)
    }

    optimizeDiamondDust()
    for (t <- all[TauTest]) {
      t.obj should be (obj)
      t.guard should matchPattern { case PointGuard(klass) if klass == tA.symType || klass == tB.symType => }
    }

    val (aPoints, bPoints) = all[TauTest].map(_.guard.asInstanceOf[PointGuard]).toSeq partition (_.root == tA.symType)
    // in current implementation a random compatible point (either tA or tB) will be chosen for diamond dust
    (aPoints, bPoints) should matchPattern { case (Seq(`aPoint`), _) | (_, Seq(`bPoint`)) => }
  }

  test("bad diamond dust for level and CHA-bit tests with incompatible point") {
    resetCHA(tB, tD)

    makeCFG(0)
    val obj = addObjNode(ClosedCone.max(tA, mayBeNull = false))
    TauTest(PointGuard(tD), TauInfo.Unknown, b(0), obj)
    TauTest(LevelGuard(4), TauInfo.PGO(1, 1), b(0), obj)
    TauTest(CHABitGuard, TauInfo.PGO(1, 1), b(0), obj)

    // Note: the type point D is not from closed cone filtered by level test despite having appropriate level,
    // however it is safe to perform diamond dust, provided that the level test was inserted correctly:
    // This means that compiler guarantees that obj cannot be of type D at run time
    // (otherwise it would pass initial level test), so we can safely replace level test with point test,
    // although it would always go to the backup path.

    optimizeDiamondDust()
    for (t <- all[TauTest]) {
      t.obj should be (obj)
      t.guard should matchPattern { case PointGuard(root) if root == tD.symType => }
    }
  }
}
