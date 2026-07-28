/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.devirtualization

import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, Position, MethodAccessKind as MAK}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.compiler.types.References._
import com.huawei.excelsior.jet.compiler.symlevel.MethodSearchError
import com.huawei.excelsior.jet.compiler.symlevel.MethodSearchError.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethod, FakeSymbol}
import com.huawei.excelsior.jet.compiler.symlevel.{Method, SignatureType}

import scala.Function.const
import scala.collection.mutable
import scala.language.implicitConversions

class CallTargetInfosSuite extends CommonDevirtualizationSuite {

  import CallTargetSearchResults._
  import TauInfo._
  import TypeApproximationBuildingHelperStrict._

  startPhase(CompilerPhase.PostInline)

  override def beforeEach(): Unit = {
    super.beforeEach()
    assert(currentProfileTargets.isEmpty)
    assert(_pgiMaxTargetsCount == DefaultPgiMaxTargetsCount)
    assert(_pgiHitsCoverageThreshold == DefaultPgiHitsCoverageThreshold)
  }

  override def afterEach(): Unit = {
    assert(currentProfileTargets.isEmpty)

    assert(_pgiMaxTargetsCount == DefaultPgiMaxTargetsCount)
    assert(_pgiHitsCoverageThreshold == DefaultPgiHitsCoverageThreshold)

    super.afterEach()
  }

  // TODO: resurrect unit-tests that use this method
  def impossibleMTFor(method: FakeMethod) = {
    assert(MethodTest.canBeGeneratedFor(method))

    // Some impossible modification should be here.

    assert(!MethodTest.canBeGeneratedFor(method))
  }


  private var currentProfileTargets = Seq.empty[(Method, Int)]

  private val DefaultPgiMaxTargetsCount = 10
  private val DefaultPgiHitsCoverageThreshold = 100
  private var _pgiMaxTargetsCount = DefaultPgiMaxTargetsCount
  private var _pgiHitsCoverageThreshold = DefaultPgiHitsCoverageThreshold
  override def pgiMaxTargetsCount = _pgiMaxTargetsCount
  override def pgiHitsCoverageThreshold = _pgiHitsCoverageThreshold

  override def plannedMethods(callSitePos: Position) = {
    assert(Position.offset(callSitePos) contains testBCPos)
    currentProfileTargets.iterator
  }

  override def calledMethods(callSitePos: Position) = plannedMethods(callSitePos)
  override def devirtTargets(callSitePos: Position) = plannedMethods(callSitePos)

  private def findImpl(refClass: ReferenceType, original: Method, akind: MAK, rcvType: ReferenceApprox, guardMode: GuardMode, thisRcv: Boolean, rootClass: ReferenceType): CallTargetSearchResult = {

    // This is a safe place to share our struggles and frustrations with JET, devirtualization and more:
    //
    // [2017] cypok & liontiger: We are naive and stupid and the thought that refClass and declClass are not the same
    //   has never crossed our minds. Plus everyone uses aliens anyway, so there are no problems right now.
    //
    // [2018] cypok & liontiger: Today we started a revolution! Because we are now much smarter and understand
    //   that refClass != declClass, we are now using refClass instead of original.getDeclaringClass in CallTargetInfos.
    //   However, there are a lot of tests already written here which assume that refClass == declClass.
    //   There are deadlines for JIT coming up and in this dev period we will not fix such issues as JET-7343.
    //   So basically due to our laziness we don't want to support all the different combinations of refClass/declClass,
    //   but we strongly encourage you to fix our mistakes and empower unit-tests as you see fit.
    //
    // [2020] liontiger: Today I eliminated aliens from Java part of compiler! However, this involved reworking
    //   isCompatibleTarget mechanism, which was relying on aliens for the virtual call of default interface method case.
    //   The initial mindless refactoring produced undesirable code in this method, which rendered all such cases as
    //   incompatible. But thankfully this was quickly caught during review and it has come that time again to write
    //   missing unit-tests and to struggle with narrowness of our testing system. As you can see above, it assumes that
    //   refClass == declClass, which is not true when interface method is called virtually (like in our case here).
    //   So I initially supported this functionality only for virtual invokes. But it was not enough. Method test was
    //   also assuming that refClass == declClass, and fixing that required a rework of _all_ RHS.
    //   At least it did not require any fundamental rework of the framework which says a lot about its robustness.
    //   I hope that we won't ever need a complete rewrite of the framework, because checking its correctness and
    //   consistency would be a gigantic task.

    val invoke = createInvoke(refClass, original, akind)

    val oldRootMethod = _rootMethod
    if (rootClass != null) {
      _rootMethod = from(rootClass)
    }
    val rcv = if (thisRcv) {
      ReceiverParam()
    } else {
      null
    }
    val res = try {
      findTargetMethod(invoke, rcv, rcvType, guardMode)
    } finally {
      _rootMethod = oldRootMethod
    }

    checkSmartBookkeepingRequirements(res)

    res
  }

  // Keep synchronized with SmartBookkeeping.registerGuardedDevirtualization().
  private def checkSmartBookkeepingRequirements(res: CallTargetSearchResult): Unit = res match {
    case OneGuardedTarget(_, guard, _, refinedRcvType) => guard match {
      case MagicGuard | _: MethodGuard =>
      case _ =>
        val filtered = guard.intersectWith(refinedRcvType)._1
        filtered match {
          case _: ClosedUpperBounded => // ok
          case OpenCone(_, _) => // ok
          case t => fail(s"unexpected type filtered by guard: $res filters $t")
        }
    }
    case _ =>
  }

  private def invoke(refClass: ReferenceType, akind: MAK, rcvType: ReferenceApprox, thisRcv: Boolean = false, rootClass: ReferenceType = null, method: FakeMethod = null): InvokeResult = {
    val original = if (method == null) {
      val declClass = if (refClass.isJavaArray) ReferenceType.javaLangObject else refClass
      from(declClass)
    } else {
      method
    }
    InvokeResult(refClass, original, rcvType, findImpl(refClass, original, akind, rcvType, _, thisRcv, rootClass))
  }

  def invokeVirtual(refClass: ReferenceType, method: FakeMethod, rcvType: ReferenceApprox): InvokeResult =
    invoke(refClass, MAK.VIRTUAL, rcvType, method = method)

  def invokeVirtual(refClass: ReferenceType, rcvType: ReferenceApprox): InvokeResult =
    invoke(refClass, MAK.VIRTUAL, rcvType)

  def invokeVirtualFromThisAt(refClass: ReferenceType, rcvType: ReferenceApprox, rootClass: ReferenceType): InvokeResult =
    invoke(refClass, MAK.VIRTUAL, rcvType, thisRcv = true, rootClass)

  def invokeVirtualRecursive(refClass: ReferenceType, rcvType: ReferenceApprox): InvokeResult =
    invokeVirtualFromThisAt(refClass, rcvType, refClass)

  def invokeInterface(refClass: ReferenceType, rcvType: ReferenceApprox): InvokeResult =
    invoke(refClass, MAK.INTERFACE, rcvType)


  trait RHS extends ((ReferenceType/*refClass*/, FakeMethod/*original*/, ReferenceApprox/*rcvType*/) => CallTargetSearchResult)

  case class RHSConst(result: CallTargetSearchResult) extends RHS {
    def apply(refClass: ReferenceType, original: FakeMethod, rcvType: ReferenceApprox) = result
  }

  def unknown = RHSConst(UnknownTarget)
  def probableNothing = RHSConst(ProbableNoTarget)
  def unreachable = RHSConst(UnreachableCall)
  def erroneous(error: MethodSearchError) = RHSConst(ErroneousCall(error))


  implicit def methodAsDirectRHS(target: Method): RHSDirect = RHSDirect(target)

  case class RHSDirect(target: Method) extends RHS {
    def apply(refClass: ReferenceType, original: FakeMethod, rcvType: ReferenceApprox) = OneDirectTarget(target)

    protected def simple(guard: Guard) = RHSOneGuarded(target, { (_, _) => guard })

    def magic = simple(MagicGuard)
    def cha = simple(CHABitGuard)
    def point(klass: ReferenceType) = simple(PointGuard(klass))
    def level(level: Int) = simple(LevelGuard(level))
    def maxcc(klass: ReferenceType) = simple(MaxClosedConeGuard(klass))

    def mt = RHSOneGuarded(target, { (_, original) => MethodGuard(original.getMethodReference, target) })
  }

  sealed abstract class RHSOneOrMultipleGuarded extends RHS {
    final def apply(refClass: ReferenceType, original: FakeMethod, rcvType: ReferenceApprox) = {
      create(refClass, original, rcvType match {
        case _ if explicitRcvType != null => explicitRcvType
        case rcvType: Cone => rcvType
        case _ => shouldNotReachHere("guarded devirtualization should not be expected when receiver type is not cone")
      })
    }

    protected def explicitRcvType: Cone
    protected def create(refClass: ReferenceType, original: FakeMethod, rcvTypeCone: Cone): OneOrMultipleGuardedTargets
  }

  case class RHSOneGuarded(target: Method, guard: (ReferenceType/*refClass*/, FakeMethod/*original*/) => Guard, info: TauInfo = Static, explicitRcvType: Cone = null) extends RHSOneOrMultipleGuarded {
    protected def create(refClass: ReferenceType, original: FakeMethod, rcvTypeCone: Cone) =
      OneGuardedTarget(target, guard(refClass, original), info, rcvTypeCone)

    def jca = { assert(info == Static); copy(info = JCA) }
    def pgo(x: Int, y: Int) = { assert(info == Static); copy(info = PGO(x, y)) }
    def receiving(rcvType: Cone) = { assert(explicitRcvType == null); copy(explicitRcvType = rcvType) }
  }

  case class RHSMultipleGuarded(guardedTargets: Seq[(Method, (ReferenceType/*refClass*/, FakeMethod/*original*/) => Guard)], info: PGO) extends RHSOneOrMultipleGuarded {
    protected def create(refClass: ReferenceType, original: FakeMethod, rcvTypeCone: Cone) =
      MultipleGuardedTargets(guardedTargets map { case (m, g) => (m, g(refClass, original)) }, info, rcvTypeCone)

    def explicitRcvType: Cone = null

    def and(another: RHSMultipleGuarded): RHSMultipleGuarded =
      RHSMultipleGuarded(guardedTargets ++ another.guardedTargets, info ++ another.info)
  }

  object RHSMultipleGuarded {
    def apply(rhs: RHSOneGuarded): RHSMultipleGuarded = {
      require(rhs.explicitRcvType == null)
      require(rhs.info.isInstanceOf[PGO])
      RHSMultipleGuarded(Seq((rhs.target, rhs.guard)), rhs.info.asInstanceOf[PGO])
    }
  }

  implicit def oneToMultipleGuarded(rhs: RHSOneGuarded): RHSMultipleGuarded = RHSMultipleGuarded(rhs)

  case class InvokeResult(refClass: ReferenceType, original: FakeMethod, rcvType: ReferenceApprox, func: (GuardMode => CallTargetSearchResult)) {
    def shouldFind(rhs: RHS): Unit = {
      val exp = rhs(refClass, original, rcvType)
      exp match {
        case _: OneDirectTarget | UnknownTarget | _: NoTarget =>
          func(GuardMode.NoGuards) should be (exp)
          func(GuardMode.RealGuards) should be (exp)
          func(GuardMode.AnyGuards) should be (exp)

        case ProbableNoTarget =>
          func(GuardMode.NoGuards) should be (UnknownTarget)
          func(GuardMode.RealGuards) should be (exp)
          func(GuardMode.AnyGuards) should be (exp)

        case exp: OneGuardedTarget =>
          func(GuardMode.NoGuards) should be (UnknownTarget)
          if (exp.guard == MagicGuard) {
            // If we test magic guard, there should be no real guard.
            func(GuardMode.RealGuards) should be (UnknownTarget)
            func(GuardMode.AnyGuards) should be (exp)
          } else {
            // If we test real guard, there may be real or magic guard.
            func(GuardMode.RealGuards) should be (exp)
            func(GuardMode.AnyGuards) should (be (exp) or be (exp.copy(guard = MagicGuard)))
          }

        case exp: MultipleGuardedTargets =>
          assert(exp.guardedTargets forall (_._2 != MagicGuard),
            "it's hard to test partially magic guards, so completely ignore them")
          func(GuardMode.NoGuards) should be (UnknownTarget)
          func(GuardMode.RealGuards) should be (exp)
      }
    }

    def shouldNotPGO(target: Method): Unit =
      shouldPGO(DefaultPgiMaxTargetsCount, DefaultPgiHitsCoverageThreshold, Seq((target, 1)), unknown)

    def shouldPGO(target: Method, rhs: RHSDirect => RHSOneGuarded): Unit =
      shouldPGOs(Seq(target), rhs(target).pgo(1, 0))

    def shouldPGOs(targets: Seq[Method], rhs: RHS): Unit =
      shouldPGOs(targets.size, DefaultPgiHitsCoverageThreshold, targets map { x => (x, 1) }, rhs)

    def shouldPGOs(maxTargets: Int, coverageThreshold: Int, targets: Seq[(Method, Int)], rhs: RHS): Unit =
      shouldPGO(maxTargets, coverageThreshold, targets, rhs)

    def shouldPGO(maxTargets: Int, coverageThreshold: Int, targets: Seq[(Method, Int)], rhs: RHS): Unit = {
      _pgiMaxTargetsCount = maxTargets
      _pgiHitsCoverageThreshold = coverageThreshold
      currentProfileTargets = targets
      try {
        shouldFind(rhs)
      } finally {
        _pgiMaxTargetsCount = DefaultPgiMaxTargetsCount
        _pgiHitsCoverageThreshold = DefaultPgiHitsCoverageThreshold
        currentProfileTargets = Seq.empty
      }
    }
  }

  //////////////////////////
  // Unguarded devirtualization

  test("final point") {
    resetCHA()
    abstractIn(tA)
    finalIn(tC)
    invokeVirtual(tA, p(tC)) shouldFind from(tC)
  }

  test("final point easy") {
    resetCHA()
    finalIn(tC)
    invokeVirtual(tC, p(tC)) shouldFind from(tC)
  }

  test("point") {
    resetCHA()
    abstractIn(tA)
    in(tC)
    invokeVirtual(tA, p(tC)) shouldFind from(tC)
  }

  test("point easy") {
    resetCHA()
    in(tC)
    invokeVirtual(tC, p(tC)) shouldFind from(tC)
  }

  test("closed cone success") {
    resetCHA(tB, tC)
    in(tA)
    invokeVirtual(tA, p(tA) union p(tB)) shouldFind from(tA) // cc(tA, 2)
  }

  test("closed cone failure") {
    resetCHA(tB, tC)
    in(tA, tC)
    invokeVirtual(tA, p(tA) union p(tB)) shouldFind unknown // cc(tA, 2)
  }

  test("closed cone no target") {
    resetCHA(tIB)
    in(tA)
    abstractIn(tB)
    invokeVirtual(tA, cc(tB, 2)) shouldFind erroneous(ABSTRACT_METHOD)
  }

  test("interface error ICCE") {
    resetCHA(tIB)
    abstractIn(tI)
    invokeInterface(tI, p(tB)) shouldFind erroneous(INCOMPATIBLE_CLASS_CHANGE)
  }

  test("interface error ICCE (array)") {
    abstractIn(tI)
    invokeInterface(tI, p(tA1D)) shouldFind erroneous(INCOMPATIBLE_CLASS_CHANGE)
  }

  test("interface possible error ICCE") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tIB, tIBB)
    invokeInterface(tI, cc(tB)) shouldFind unknown
  }

  test("interface error IAE") {
    resetCHA(tIB)
    abstractIn(tI)
    privateIn(tIB)
    invokeInterface(tI, p(tIB)) shouldFind erroneous(ILLEGAL_ACCESS)
  }

  test("interface possible error IAE") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tIB)
    privateIn(tIBB)
    invokeInterface(tI, cc(tIB)) shouldFind unknown
  }

  test("interface from closed cone") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tIB, tIBB)
    invokeInterface(tI, cc(tIB)) shouldFind unknown // virtualFrom(tIB), see JET-9901
  }

  test("final cone") {
    resetCHA()
    finalIn(tA)
    invokeVirtual(tA, p(tA)) shouldFind from(tA)
  }

  test("final cone 2") {
    resetCHA()
    finalIn(tA)
    invokeVirtual(tA, p(tB)) shouldFind from(tA)
  }

  test("final cone and abstract root") {
    resetCHA(tCF)
    in(tObj)
    finalIn(tA)
    abstractClass(tC)
    invokeVirtual(tObj, c(tC)) shouldFind from(tA)
  }

  test("cone") {
    resetCHA()
    in(tA)
    invokeVirtual(tA, c(tA)) shouldFind unknown
  }

  test("array point from j.l.Object") {
    resetCHA()
    in(tObj)
    invokeVirtual(tObj, p(tObj1D)) shouldFind from(tObj)
  }

  test("array point from array") {
    resetCHA()
    in(tObj)
    invokeVirtual(tObj1D, p(tObj1D)) shouldFind from(tObj)
  }

  test("array point from greater array") {
    resetCHA()
    in(tObj)
    invokeVirtual(tA1D, p(tObj1D)) shouldFind unreachable
  }

  test("array cone from j.l.Object") {
    resetCHA()
    in(tObj)
    invokeVirtual(tObj, c(tObj1D)) shouldFind from(tObj)
  }

  test("array cone from array") {
    resetCHA()
    in(tObj)
    invokeVirtual(tObj1D, c(tObj1D)) shouldFind from(tObj)
  }

  test("virtual call from array of interfaces") {
    resetCHA()
    in(tObj)
    invokeVirtual(tI1D, w(tObj1D, c(tI1D))) shouldFind from(tObj)
  }

  test("array point incompatible class") {
    in(tA)
    invokeVirtual(tA, p(tObj1D)) shouldFind unreachable
  }

  test("private as virtual") {
    resetCHA()
    privateIn(tA)
    invokeVirtual(tA, c(tA)) shouldFind from(tA)
  }

  test("raw interface call") {
    resetCHA()
    abstractIn(tI)
    invokeInterface(tI, c(tI)) shouldFind unknown
  }

  test("virtualized interface call") {
    resetCHA()
    abstractIn(tI)
    in(tIB)
    invokeInterface(tI, c(tIB)) shouldFind unknown // OneVirtualTarget(from(tIB)) - not implemented yet
  }

  test("final devirtualized interface call") {
    resetCHA()
    abstractIn(tI)
    finalIn(tIB)
    invokeInterface(tI, c(tIB)) shouldFind from(tIB)
  }

  test("devirtualized interface call") {
    resetCHA()
    abstractIn(tI)
    in(tIB)
    invokeInterface(tI, p(tIB)) shouldFind from(tIB)
  }

  test("final in super") {
    resetCHA()
    finalIn(tB)
    abstractIn(tI)
    invokeInterface(tI, c(tIB)) shouldFind from(tB)
  }

  test("default method") {
    resetCHA()
    abstractIn(tI)
    defaultIn(tJ)
    invokeInterface(tI, p(tJB)) shouldFind from(tJ)
  }

  test("non-default method") {
    resetCHA()
    abstractIn(tI)
    defaultIn(tJ)
    in(tA)
    invokeInterface(tI, p(tJB)) shouldFind from(tA)
  }

  test("abstract") {
    resetCHA()
    abstractIn(tA)
    invokeVirtual(tA, p(tA)) shouldFind erroneous(ABSTRACT_METHOD)
  }

  test("intersected empty") {
    resetCHA()
    in(tB)
    invokeVirtual(tB, c(tC)) shouldFind unreachable
  }

  test("thin final point") {
    resetCHA(tTXX)
    abstractIn(tTX)
    finalIn(tTXX)
    invokeVirtual(tTX, p(tTXX)) shouldFind from(tTXX)
  }

  test("thin point") {
    resetCHA(tTXX)
    abstractIn(tTX)
    in(tTXX)
    invokeVirtual(tTX, p(tTXX)) shouldFind from(tTXX)
  }

  test("thin point easy") {
    resetCHA(tTXX)
    in(tTXX)
    invokeVirtual(tTXX, p(tTXX)) shouldFind from(tTXX)
  }

  test("thin closed cone") {
    resetCHA(tTXX, tTXXF)
    in(tTX)
    invokeVirtual(tTX, p(tTX) union p(tTXX)) shouldFind from(tTX)
  }

  test("thin 3 classes 3 impls") {
    resetCHA(tTXX, tTXXF)
    in(tTX, tTXX, tTXXF)
    invokeVirtual(tTX, cc(tTX)) shouldFind unknown
  }

  // Unguarded devirtualization
  //////////////////////////

  //////////////////////////
  // Probable types (& CHA)

  test("cha 1 class") {
    resetCHA(tA)
    in(tA)
    invokeVirtual(tA, c(tA)) shouldFind from(tA).cha
  }

  test("cha 2 classes 1 impl") {
    resetCHA(tB)
    in(tA)
    invokeVirtual(tA, c(tA)) shouldFind from(tA).cha
  }

  test("cha 2 classes 2 impl") {
    resetCHA(tB)
    in(tA, tB)
    invokeVirtual(tA, c(tA)) shouldFind unknown
  }

  test("cha 2 classes 2 impls 1 abstract") {
    resetCHA(tB)
    in(tA, tB)
    abstractClass(tB)
    invokeVirtual(tA, c(tA)) shouldFind from(tA).cha
  }

  test("cha 3 classes 2 impls 1 abstract") {
    resetCHA(tIB)
    in(tA, tB)
    abstractClass(tB)
    invokeVirtual(tA, c(tA)) shouldFind unknown
  }

  test("cha 1 abstract") {
    resetCHA(tA)
    in(tA)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldFind probableNothing
  }

  test("cha 1 abstract 2 subclasses") {
    resetCHA(tB, tC)
    in(tA)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldFind from(tA).cha
  }

  test("cha 2 impls 1 abstract 1 common impl") {
    resetCHA(tIB)
    in(tA, tB)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldFind from(tB).cha
  }

  test("cha 2 impls 1 abstract 1 non-common impl") {
    resetCHA(tB, tC)
    in(tA, tB)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldFind unknown
  }

  test("cha impl in super") {
    resetCHA(tIB)
    in(tA)
    invokeVirtual(tA, c(tB)) shouldFind from(tA).cha
  }

  test("cha impl in abstract super") {
    resetCHA(tIB)
    in(tA)
    abstractClass(tA)
    invokeVirtual(tA, c(tB)) shouldFind from(tA).cha
  }

  test("cha 1 abstract 1 override") {
    resetCHA(tB)
    abstractIn(tA)
    in(tB)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldFind from(tB).cha
  }

  test("interface with two impls: level") {
    resetCHA(tIBBB, tIX)
    in(tObj, tIBBB)
    invokeVirtual(tObj, c(tI)) shouldFind unknown // no single implementation
    invokeVirtual(tObj, w(tI, cc(tIB, 2))) shouldFind from(tObj).level(7)
  }

  test("interface with two impls: cha") {
    resetCHA(tIB, tIX)
    in(tObj)
    invokeVirtual(tObj, c(tI)) shouldFind from(tObj).cha
    invokeVirtual(tObj, w(tI, p(tIB))) shouldFind from(tObj).cha
  }

  test("object cone: no level") {
    resetCHA(tCF)
    in(tObj, tCF)
    invokeVirtual(tObj, c(tObj)) shouldFind unknown // no single implementation
    invokeVirtual(tObj, w(tObj, cc(tA, 2))) shouldFind from(tObj).mt // never ever .level(2)
  }

  test("object cone: no cha") {
    resetCHA(tC)
    in(tObj)
    invokeVirtual(tObj, c(tObj)) shouldFind from(tObj).maxcc(tObj)
    invokeVirtual(tObj, w(tObj, p(tA))) shouldFind from(tObj).point(tA) // never ever .cha
  }

  test("cha with refined type") {
    resetCHA(tB)
    in(tA, tB)
    invokeVirtual(tB, c(tA)) shouldFind from(tB).cha.receiving(c(tB))
  }

  test("hard partial refinement") {
    resetCHA(tB, tI)
    in(tA)
    invokeVirtual(tA, c(tObj)) shouldFind from(tA).cha.receiving(c(tA))
    invokeVirtual(tA, c(tI)) shouldFind from(tA).cha.receiving(c(tA))
  }

  test("intersected closed cone") {
    resetCHA(tIB, tJB)
    abstractIn(tJ)
    in(tIB, tJB)
    invokeInterface(tJ, cc(tB, 2)) shouldFind from(tJB).point(tJB) // no direct because of potential ICCE
  }

  test("probable non-final point in open cone without cha") {
    resetCHA()
    in(tA)
    invokeVirtual(tA, w(tA, p(tC))) shouldFind from(tA).point(tC)
  }

  test("probable final point in open cone without cha") {
    resetCHA()
    in(tA)
    invokeVirtual(tA, w(tA, p(tCF))) shouldFind from(tA).point(tCF)
  }

  test("probable point in open cone without cha bit") {
    resetCHA(tIB)
    in(tA, tB)
    invokeVirtual(tA, w(tA, p(tIB))) shouldFind from(tB).point(tIB)
  }

  test("probable point in open cone under cha test") {
    resetCHA(tIB)
    in(tA, tB)
    invokeVirtual(tA, w(tA, p(tIB))) shouldFind from(tB).point(tIB)
  }

  test("probable point in open cone under point test") {
    resetCHA(tIBB)
    in(tA, tB, tIBB)
    invokeVirtual(tA, w(tA, p(tIB))) shouldFind from(tB).point(tIB)
  }

  test("probable point in interface open cone") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tB, tIBB)
    invokeInterface(tI, w(tI, p(tIB))) shouldFind from(tB).point(tIB)
  }

  test("probable point in closed cone") {
    resetCHA(tIB)
    in(tA, tB)
    invokeVirtual(tA, wc(tA, p(tIB))) shouldFind from(tB).point(tIB)
  }

  test("probable non-chabit test with chabit probable root") {
    resetCHA(tIB, tJB)
    in(tA, tB)
    for (tpe <- Seq(c(tB), cc(tB))) {
      invokeVirtual(tA, w(tObj, tpe)) shouldFind from(tB).maxcc(tB).receiving(c(tA))
    }
  }

  test("probable point test") {
    resetCHA(tB)
    in(tA, tB)
    for (tpe <- Seq(c(tB), p(tB))) {
      invokeVirtual(tA, w(tA, tpe)) shouldFind from(tB).point(tB)
    }
  }

  test("probable maxcc test") {
    resetCHA(tIBB, tCF)
    in(tA, tC)
    for (tpe <- Seq(c(tC), cc(tC))) {
      invokeVirtual(tA, w(tA, tpe)) shouldFind from(tC).maxcc(tC)
    }
  }

  test("probable maxcc test (abstract root)") {
    resetCHA(tIBB)
    in(tA, tB)
    abstractClass(tB)
    invokeVirtual(tA, w(tA, c(tB))) shouldFind from(tB).maxcc(tIB)
  }

  test("probable maxcc test (abstract inheritor)") {
    resetCHA(tIBB)
    in(tA, tB, tIBB)
    abstractClass(tIBB)
    invokeVirtual(tA, w(tA, c(tB))) shouldFind from(tB).maxcc(tB)
  }

  test("probable maxcc test with interface rcv root") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tB)
    invokeInterface(tI, w(tObj, c(tIB))) shouldFind from(tB).maxcc(tIB)
  }

  test("probable point test (abstract class without CC bit)") {
    resetCHA(tIB)
    in(tA, tB)
    abstractClass(tB)
    invokeVirtual(tA, w(tA, c(tB))) shouldFind from(tB).point(tIB)
  }

  test("probable level test (or maxcc) (abstract class without CC bit)") {
    resetCHA(tIBB, tJB)
    in(tA, tB)
    abstractClass(tB)

    invokeVirtual(tA, w(tA, c(tB))) shouldFind from(tB).maxcc(tB)
  }

  test("probable chabit test with same root") {
    resetCHA(tIBB, tJB)
    in(tA, tB)
    abstractClass(tB)

    invokeVirtual(tA, c(tB)) shouldFind from(tB).cha
  }

  test("probable level test") {
    resetCHA(tIBB)
    in(tA, tIBB)
    invokeVirtual(tA, w(tB, cc(tB, 2))) shouldFind from(tA).level(6)
  }

  test("probable level test (with abstract inheritor)") {
    resetCHA(tIBB, tC)
    in(tA, tIBB)
    abstractClass(tB)
    abstractClass(tIB)
    invokeVirtual(tA, w(tA, cc(tA, 3))) shouldFind from(tA).level(5) // probable without tIBB
  }

  test("probable level test (different root)") {
    resetCHA(tIBB)
    in(tA, tIBB)
    invokeVirtual(tA, w(tA, cc(tB, 2))) shouldFind from(tA).level(6)
  }

  test("probable maxcc test with cc safe") {
    resetCHA(tIBB)
    in(tA, tB, tIBB)
    invokeVirtual(tA, wc(tA, 3, cc(tB, 2))) shouldFind from(tB).maxcc(tB)
  }

  test("probable needs level + maxcc test") {
    resetCHA(tIBB, tC)
    in(tA, tIBB, tC)
    invokeVirtual(tA, w(tA, cc(tB, 2))) shouldFind from(tA).mt // combo is needed: maxcc(tB) & level(3)
  }

  test("probable needs array bit test") {
    resetCHA()
    in(tObj)
    invokeVirtual(tObj, w(tObj, c(tObj1D))) shouldFind from(tObj).magic
  }

  test("probable array cold") {
    resetCHA(tI)
    abstractIn(tI)
    invokeInterface(tI, w(tObj, c(tObj1D))) shouldFind probableNothing
  }

  test("probable no-point test (abstract class)") {
    resetCHA(tB)
    in(tA, tB)
    abstractClass(tB)
    invokeVirtual(tA, w(tA, c(tB))) shouldFind probableNothing
  }

  test("probable unknown") {
    resetCHA(tIB)
    in(tA, tB, tIB)
    for (tpe <- Seq(c(tB), cc(tB))) {
      invokeVirtual(tA, w(tA, tpe)) shouldFind unknown
    }
  }

  test("probable unknown for interface") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tIB, tIBB)
    invokeInterface(tI, w(tObj, c(tI))) shouldFind unknown
  }

  test("probable interface call from object") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tA)
    invokeInterface(tI, c(tObj)) shouldFind from(tA).maxcc(tIB)
  }

  test("probable interface call from probable interface") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tA)
    invokeInterface(tI, w(tObj, c(tI))) shouldFind from(tA).maxcc(tIB)
  }

  test("probable interface call from class") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tA)
    invokeInterface(tI, c(tA)) shouldFind from(tA).maxcc(tIB)
  }

  test("probable interface call from interface") {
    resetCHA(tIBB)
    abstractIn(tI)
    in(tA)
    invokeInterface(tI, c(tI)) shouldFind from(tA).cha
  }

  test("probable interface call from classes without suitable common super") {
    resetCHA(tIBB, tJB)
    defaultIn(tI)
    invokeInterface(tI, c(tA)) shouldFind unknown
  }

  test("probable interface call from interface without suitable common super") {
    resetCHA(tIBB, tJB)
    defaultIn(tI)
    invokeInterface(tI, c(tI)) shouldFind unknown // from(tI).cha might be here
  }

  test("probable interface call from classes with hard suitable common super") {
    resetCHA(tIBB, tJB)
    abstractIn(tI)
    in(tB)
    abstractClass(tB)
    invokeInterface(tI, c(tA)) shouldFind unknown // from(tB).maxcc(tB) might be here
    // findCHAImplementation currently fails to analyze classes which does not inherit original method host
  }

  test("probable interface call from interface with hard suitable common super") {
    resetCHA(tIBB, tJB)
    abstractIn(tI)
    in(tB)
    abstractClass(tB)
    invokeInterface(tI, c(tI)) shouldFind unknown // from(tB).cha might be here
    // findCHAImplementation currently fails to analyze classes which does not inherit original method host
  }

  test("probable interface call as cha from non-strict interface") {
    resetCHA(tIB)
    abstractIn(tI)
    in(tA)
    invokeInterface(tI, c(tObj)) shouldFind from(tA).point(tIB) // from(tA).cha might be here
    // findCHAImplementation currently fails to analyze classes which does not inherit original method host
  }

  test("probable interface call not cha from non-strict interface") {
    resetCHA(tIB)
    abstractIn(tI)
    in(tA)
    invokeInterface(tI, c(tObj)) shouldFind from(tA).point(tIB)
  }

  test("probable interface call with small hierarchy of rcvType") {
    resetCHA(tIBB, tJB, tJB2)
    abstractIn(tI)
    in(tB)
    abstractClass(tB)
    invokeInterface(tI, c(tJ)) shouldFind unknown // from(tB).cha might be here
    // findCHAImplementation currently fails to analyze classes which does not inherit original method host
  }

  test("probable interface call with unstrict closing and two impls") {
    resetCHA(tIBB, tJB)
    abstractIn(tI)
    in(tIB, tJB)
    invokeInterface(tI, w(tObj, c(tB))) shouldFind unknown
  }

  test("probable interface call with unstrict closing and one impl") {
    resetCHA(tIBB, tJB)
    abstractIn(tI)
    in(tB)
    // we cannot do maxcc(tB) because interfacecall(tB) must throw ICCE
    invokeInterface(tI, w(tObj, c(tB))) shouldFind unknown
  }

  test("probable interface call with unstrict closing and one impl and abstract root") {
    resetCHA(tIBB, tJB)
    abstractIn(tI)
    in(tB)
    abstractClass(tB)
    invokeInterface(tI, w(tObj, c(tB))) shouldFind unknown // from(tB).maxcc(tB) might be here
    // findCHAImplementation currently fails to analyze classes which does not inherit original method host
  }

  test("probable abstract in point") {
    resetCHA()
    abstractIn(tI, tIB)
    invokeInterface(tI, w(tObj, p(tIB))) shouldFind probableNothing
  }

  test("probable erroneous in closed cone") {
    resetCHA(tIBB)
    abstractIn(tI)
    privateIn(tIB) // IAE
    privateIn(tIBB) // IAE
    invokeInterface(tI, w(tIB, cc(tIB))) shouldFind probableNothing
  }

  test("probable different erroneous in closed cone") {
    resetCHA(tIBB)
    abstractIn(tI)
    privateIn(tIB) // IAE
    abstractIn(tIBB) // AME
    invokeInterface(tI, w(tIB, cc(tIB))) shouldFind unknown // probableNothing might be, but different errors breaks current implementation
  }

  test("probable erroneous and normal in closed cone") {
    resetCHA(tIBB)
    abstractIn(tI)
    privateIn(tIB) // IAE
    in(tIBB) // ok
    invokeInterface(tI, w(tIB, cc(tIB))) shouldFind unknown // error or ok, everything could happen
  }

  test("probable incompatible point") {
    resetCHA()
    abstractIn(tI)
    invokeInterface(tI, w(tObj, p(tB))) shouldFind probableNothing
  }

  test("probable abstract cone") {
    resetCHA(tA)
    in(tA)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldFind probableNothing
  }

  // Probable types (& CHA)
  //////////////////////////

  //////////////////////////
  // Profile guard choosing

  test("choose no test from incompatible receiver") {
    resetCHA()
    in(tA, tC)
    invokeVirtual(tA, c(tB)) shouldNotPGO (from(tC))
  }

  test("choose no test from incompatible closed receiver") {
    resetCHA(tIB)
    in(tA, tB, tIB)
    invokeVirtual(tA, cc(tA, 2)) shouldNotPGO (from(tIB))
  }

  test("choose no test from greater receiver") {
    resetCHA()
    in(tA, tB)
    invokeVirtual(tA, c(tIB)) shouldNotPGO (from(tA))
  }

  test("choose no test for non-public target via interface call") {
    resetCHA()
    abstractIn(tI)
    privateIn(tIB)
    in(tIBB)
    invokeInterface(tI, c(tIB)) shouldNotPGO (from(tIB))
  }

  test("choose no test for incompatible target (final class) and original (interface)") {
    resetCHA()
    abstractIn(tI)
    in(tA, tCF)
    invokeInterface(tI, c(tA)) shouldNotPGO (from(tCF))
  }

  test("choose no test for incompatible target (class) and original (class)") {
    resetCHA()
    abstractIn(tI, tC)
    in(tIB)
    invokeVirtual(tC, c(tI)) shouldNotPGO (from(tIB))
  }

  test("choose no test for overriden default method via virtual call") {
    resetCHA()
    defaultIn(tI)
    in(tIB)
    invokeVirtual(tIBB, from(tIB), c(tB)) shouldNotPGO (from(tI))
  }

  test("choose method test for non-overriden default method via virtual call") {
    resetCHA()
    defaultIn(tI)
    in(tIBB)
    invokeVirtual(tIB, from(tI), c(tIB)) shouldPGO (from(tI), _.mt)
  }

  test("choose not first target (skip incompatible)") {
    resetCHA()
    in(tA, tC)
    invokeVirtual(tA, c(tB)) shouldPGOs (Seq(from(tC), from(tA)), from(tA).mt.pgo(1, 0))
  }

  test("choose point test from greater receiver") {
    resetCHA(tIB, tJB)
    in(tA, tIB, tJB)
    invokeVirtual(tA, c(tB)) shouldPGO (from(tA), _.point(tB))
  }

  test("choose maxcc test from greater receiver (with abstract super)") {
    resetCHA(tIB, tC)
    in(tA, tC)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldPGO (from(tA), _.mt) // maxcc(tB) might be here (current implementation limitation)
  }

  test("choose level test from greater receiver") {
    resetCHA(tIB, tJB)
    in(tA, tIB, tJB)
    invokeVirtual(tA, c(tA)) shouldPGO (from(tA), _.level(5))
  }

  test("choose method test from greater receiver") {
    resetCHA(tIB, tC)
    in(tA, tC, tIB)
    invokeVirtual(tA, c(tA)) shouldPGO (from(tA), _.mt)
  }

  test("choose non-method test from greater receiver (with abstract bad classes)") {
    resetCHA(tIB, tC)
    in(tA, tC, tIB)
    abstractClass(tC)
    invokeVirtual(tA, c(tA)) shouldPGO (from(tA), _.level(5))
  }

  test("choose maxcc test from lesser receiver") {
    resetCHA(tIB, tC)
    in(tA, tB)
    invokeVirtual(tA, c(tA)) shouldPGO (from(tB), _.maxcc(tB))
  }

  test("choose maxcc test from lesser receiver (abstract inheritor)") {
    resetCHA(tIBB, tC)
    in(tA, tB, tIBB)
    abstractClass(tIBB)
    invokeVirtual(tA, c(tA)) shouldPGO (from(tB), _.maxcc(tB))
  }

  test("choose maxcc test from lesser interface receiver") {
    resetCHA(tIBB, tJB)
    abstractIn(tI)
    in(tIB, tJB)
    invokeInterface(tI, c(tI)) shouldPGO (from(tIB), _.maxcc(tIB))
  }

  test("choose method test from lesser interface receiver (strict interface)") {
    resetCHA(tIB, tJB)
    abstractIn(tI)
    in(tB)
    invokeInterface(tI, c(tI)) shouldPGO (from(tB), _.mt) // maxcc(tB) may be used here
  }

  test("choose maxcc test from partially equal interface receiver (probable interface)") {
    resetCHA(tIBB, tJB)
    abstractIn(tI)
    in(tIB)
    invokeInterface(tI, w(tObj, c(tI))) shouldPGO (from(tIB), _.maxcc(tIB))
  }

  test("choose method test from partially equal interface receiver (probable interface)") {
    resetCHA(tIB, tJB)
    abstractIn(tI)
    in(tB)
    invokeInterface(tI, w(tObj, c(tI))) shouldPGO (from(tB), _.mt)
  }

  test("choose not-maxcc test from partially equal class receiver (strict interface)") {
    resetCHA(tIB, tJB)
    abstractIn(tI)
    in(tA, tB)
    abstractClass(tB)
    invokeVirtual(tA, c(tI)) shouldPGO (from(tB), _.maxcc(tB).receiving(c(tA)))
  }

  test("choose point test from intersection of receiver and target (intersection implements original)") {
    resetCHA(tIB, tIKBB)
    abstractIn(tI)
    defaultIn(tK)
    invokeInterface(tI, cc(tB, 3)) shouldPGO (from(tK), _.maxcc(tIKB))
  }

  test("choose point test from intersection with point target") {
    resetCHA(tIB, tJBF)
    abstractIn(tI)
    in(tJBF)
    invokeInterface(tI, c(tB)) shouldPGO (from(tJBF), _.point(tJBF))
  }

  test("choose method test from intersection of receiver and target (receiver implements original)") {
    resetCHA(tIB, tJB)
    abstractIn(tI)
    in(tB)
    invokeInterface(tI, c(tI)) shouldPGO (from(tB), _.mt)
  }

  test("choose not-maxcc test from greater class receiver (strict interface)") {
    resetCHA(tIBB, tJB)
    abstractIn(tI)
    in(tIB, tIBB)
    invokeVirtual(tIB, c(tI)) shouldPGO (from(tIB), _.point(tIB).receiving(c(tIB)))
  }

  test("choose point test from lesser receiver") {
    resetCHA(tIB, tJB, tC)
    in(tA, tB, tIB, tJB)
    invokeVirtual(tB, c(tB)) shouldPGO (from(tB), _.point(tB))
  }

  test("choose method test from lesser receiver") {
    resetCHA(tIB, tJB, tC)
    in(tA, tB, tIB)
    invokeVirtual(tB, c(tB)) shouldPGO (from(tB), _.mt)
  }

  test("choose method test from lesser receiver (need maxcc + level)") {
    resetCHA(tIB, tJB, tC)
    in(tObj, tA, tIB, tJB)
    invokeVirtual(tObj, c(tObj)) shouldPGO (from(tA), _.mt) // maxcc(tA) + level(2)
  }

  test("choose level test from lesser receiver (abstract root)") {
    resetCHA(tIBB)
    in(tB, tIBB)
    abstractClass(tA)
    invokeVirtual(tB, c(tB)) shouldPGO (from(tB), _.level(6))
  }

  test("choose level test from lesser receiver (normal root)") {
    resetCHA(tIBB, tJB)
    in(tB, tIBB)
    invokeVirtual(tB, c(tB)) shouldPGO (from(tB), _.level(6))
  }

  test("choose point test from lesser receiver (final class)") {
    resetCHA()
    in(tC, tCF)
    invokeVirtual(tC, c(tC)) shouldPGO (from(tCF), _.point(tCF))
  }

  test("choose method test from lesser receiver (non-final class)") {
    resetCHA()
    in(tA, tB)
    invokeVirtual(tA, c(tA)) shouldPGO (from(tB), _.mt)
  }

  test("choose never passing method test for inherited interfaces (weak implementation)") {
    resetCHA()
    defaultIn(tI, tJ)
    invokeInterface(tI, c(tJ)) shouldPGO (from(tI), _.mt) // This is a never passing method test. ;(
  }

  // TODO: resurrect unit-test
  ignore("choose magic test if method test is not possible") {
    resetCHA()
    in(tA)
    impossibleMTFor(from(tA))
    invokeVirtual(tA, c(tB)) shouldPGO (from(tA), _.magic)
  }

  test("choose method test for interface cone") {
    resetCHA(tIB, tIX)
    defaultIn(tI)
    invokeInterface(tI, c(tI)) shouldPGO (from(tI), _.mt)
  }

  test("choose method test for abstract cone") {
    resetCHA(tA)
    in(tA)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldPGO (from(tA), _.mt)
  }

  test("choose method test for abstract class with overrides in non-abstract inheritors") {
    resetCHA(tB, tC)
    in(tA, tB, tC)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldPGO (from(tA), _.mt)
  }

  test("choose for pgo respects probable type") {
    resetCHA(tIB, tC)
    in(tA, tIB, tC)
    invokeVirtual(tA, w(tA, c(tB))) shouldPGO (from(tA), _.point(tB))
  }

  test("choose point test if probable type is incompatible but target is final") {
    resetCHA(tIB, tCF)
    in(tA, tIB, tCF)
    invokeVirtual(tA, w(tA, c(tB))) shouldPGO (from(tCF), _.point(tCF))
  }

  test("interface call with abstract root non-implementing interface") {
    resetCHA(tIBB, tJB, tIX)
    abstractIn(tI)
    in(tB)
    abstractClass(tB)
    invokeInterface(tI, c(tObj)) shouldPGO (from(tB), _.mt) // _.maxcc(tB) might be used
  }

  test("simple multi") {
    resetCHA(tB)
    in(tA, tB)
    invokeVirtual(tA, c(tA)) shouldPGOs (
      Seq(from(tB), from(tA)),
      from(tB).point(tB).pgo(1, 1) and from(tA).point(tA).pgo(1, 0))
  }

  test("multi respects maxTargets") {
    resetCHA(tB, tC)
    in(tA, tB, tC)
    invokeVirtual(tA, c(tA)) shouldPGOs (maxTargets = 2, coverageThreshold = 100,
      Seq((from(tB), 10), (from(tA), 9), (from(tC), 8)),
      from(tB).point(tB).pgo(10, 17) and from(tA).point(tA).pgo(9, 8))
  }

  test("multi with incompatible targets") {
    resetCHA(tB, tCF)
    in(tA, tB, tC, tCF)
    invokeVirtual(tC, c(tC)) shouldPGOs (maxTargets = DefaultPgiMaxTargetsCount, coverageThreshold = DefaultPgiHitsCoverageThreshold,
      Seq((from(tA), 35)/*incompatible*/, (from(tCF), 25), (from(tB), 20)/*incompatible*/, (from(tC), 20)),
      from(tCF).point(tCF).pgo(25, 20) and from(tC).point(tC).pgo(20, 0))
  }

  test("multi respects coverageThreshold") {
    resetCHA(tB, tCF)
    in(tA, tB, tC, tCF)
    invokeVirtual(tA, c(tA)) shouldPGOs (maxTargets = 4, coverageThreshold = 50,
      Seq((from(tB), 35), (from(tA), 25), (from(tC), 20), (from(tCF), 20)),
      from(tB).point(tB).pgo(35, 65) and from(tA).point(tA).pgo(25, 40))
  }

  // TODO: resurrect unit-test
  ignore("multi with impossible guard") {
    resetCHA()
    in(tA, tB, tC, tCF)
    impossibleMTFor(from(tC))
    invokeVirtual(tA, c(tA)) shouldPGOs (
      Seq(from(tB), from(tA), from(tC), from(tCF)),
      from(tB).mt.pgo(1, 3) and from(tA).mt.pgo(1, 2))
  }

  // Profile guard choosing
  //////////////////////////

  //////////////////////////
  // Recursive & JCA

  test("recursive under CHA") {
    resetCHA(tB)
    in(tA)
    invokeVirtualRecursive(tA, c(tA)) shouldFind from(tA).cha
  }

  test("recursive under point test") {
    resetCHA(tB)
    in(tA, tB)
    invokeVirtualRecursive(tA, c(tA)) shouldFind from(tA).point(tA)
  }

  test("recursive under method test") {
    resetCHA()
    in(tA)
    in(tB)
    invokeVirtualRecursive(tA, c(tA)) shouldFind from(tA).mt
  }

  test("recursive under level test") {
    resetCHA(tIB, tC)
    in(tA, tIB)
    invokeVirtualRecursive(tA, c(tA)) shouldFind from(tA).level(5)
  }

  test("recursive not devirtualized") {
    resetCHA()
    in(tA)
    in(tB)
    invokeVirtualRecursive(tA, c(tB)) shouldFind unknown
  }

  test("recursive not devirtualized because abstract") {
    resetCHA()
    in(tA)
    abstractIn(tB)
    invokeVirtualRecursive(tA, c(tB)) shouldFind unknown
  }

  test("recursive abstract class") {
    resetCHA(tA)
    in(tA)
    abstractClass(tA)
    invokeVirtualRecursive(tA, c(tA)) shouldFind from(tA).mt
  }

  test("recursive abstract class with overrides in non-abstract inheritors") {
    resetCHA(tB, tC)
    in(tA, tB, tC)
    abstractClass(tA)
    invokeVirtualRecursive(tA, c(tA)) shouldFind from(tA).mt
  }

  test("jca inline") {
    resetCHA()
    in(tA)
    from(tA).setJCAInlined(true)
    invokeVirtual(tA, c(tB)) shouldFind from(tA).mt.jca
  }

  test("jca inline non-public interface call") {
    resetCHA()
    abstractIn(tI)
    privateIn(tIB)
    from(tIB).setJCAInlined(true)
    invokeInterface(tI, c(tIB)) shouldFind unknown
  }

  test("jca inline abstract class") {
    resetCHA(tA)
    in(tA)
    from(tA).setJCAInlined(true)
    abstractClass(tA)
    invokeVirtual(tA, c(tA)) shouldFind from(tA).mt.jca
  }

  // Recursive & JCA
  //////////////////////////

  //////////////////////////
  // Regression tests

  test("this arg may be casted to some random class, JET-10244") {
    resetCHA()
    in(tA)
    defaultIn(tI)
    invokeVirtualFromThisAt(tA, c(tA), rootClass = tI) shouldFind unknown
  }

  test("jet-11003, 1") {
    // I = Collection
    // J = List
    // IB = AbstractCollection
    // IBB = LinkedHashSet
    resetCHA()
    abstractIn(tI, tJ)
    in(tIB)
    // there might be tX extends tIBB implements tJ
    invokeInterface(tJ, c(tIBB)) shouldPGO (from(tIB), _.mt)
  }

  test("jet-11003, 2") {
    resetCHA()
    abstractIn(tI, tJ)
    in(tIB, tIBB)
    // always failing guard, should be unknown, tIBB overrides method from tIB
    invokeInterface(tJ, c(tIBB)) shouldPGO (from(tIB), _.mt)
  }

  test("jet-11003, 3") {
    resetCHA()
    abstractIn(tI, tJ)
    in(tIB, tJB)
    // tIBB is incompatible with tJB
    invokeInterface(tJ, c(tIBB)) shouldNotPGO (from(tJB))
  }

  test("jet-11009, empty CHA intersection, not maxcc") {
    resetCHA(tIBB, tJB, tJB2)
    abstractIn(tI)
    in(tIB)
    invokeInterface(tI, c(tJ)) shouldPGO (from(tIB), _.mt)
  }

  test("jet-11009, empty CHA intersection, not point") {
    resetCHA(tIB, tJB, tJB2)
    abstractIn(tI)
    in(tIB)
    invokeInterface(tI, c(tJ)) shouldPGO (from(tIB), _.mt)
  }

  test("jet-11009, non empty CHA intersection") {
    resetCHA(tJIB, tJB, tJB2)
    abstractIn(tI)
    in(tIB)
    invokeInterface(tI, c(tJ)) shouldPGO (from(tIB), _.point(tJIB))
  }

  // Regression tests
  //////////////////////////

}
