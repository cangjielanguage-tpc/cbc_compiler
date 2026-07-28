/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.lowering.amd64.LoweringAmd64
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.ScalesAmd64
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.CLASS
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethod, FakeMethodType, FakeType}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{InterfaceType, ReferenceType}
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.compiler.types.TypesToolbox
import com.huawei.excelsior.jet.compiler.{CompilerSuite, RTSProc}


/**
 * Tests for lowering of ArrayStoreCheck nodes.
 */
class ArrayStoreCheckLoweringSuite extends CompilerSuite
        with GlobalNodesBuilder
        with TypesToolbox
        with Optimize
        with ScalesAmd64
        with LoweringAmd64
        with UnnecessaryOperationsElimination {

  import TypeApproximationBuildingHelperNonStrict.*
  startPhase(CompilerPhase.Lowering)

  val jrCheckArrayStoreOpt = new FakeMethod("JR_CheckArrayStoreOpt", FakeMethodType.create(TypeKind.INT, TypeKind.CLASS, TypeKind.CLASS))
  val jrCheckArrayStoreNotNull = new FakeMethod("JR_CheckArrayStoreNotNull", FakeMethodType.create(TypeKind.INT, TypeKind.CLASS, TypeKind.CLASS))

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.disable(BoolOption.IdescHigh16BitsCleaning)

    val ajStandardException = FakeType("com/huawei/excelsior/jet/runtime/excepts/AJStandardExceptions", CLASS, null)
    val javaStandardException = FakeType("com/huawei/excelsior/jet/runtime/excepts/JavaStandardExceptions", CLASS, null)
    env.registerFake(ajStandardException, javaStandardException)

    env.setRtsProc(RTSProc.JR_CheckArrayStoreOpt, jrCheckArrayStoreOpt)
    env.setRtsProc(RTSProc.JR_CheckArrayStoreNotNull, jrCheckArrayStoreNotNull)
  }

  object LoweredGetField {
    def unapply(lm: LoadMemory) = lm.addr match {
      case lea @ Lea.Base(base, offset) => Some((base, offset))
      case _ => None
    }
  }

  def checkObjCmp(cmp: Cmp, arrObj: Node): Unit = {
    val objectArrayDesc = RawInstanceDescriptor(typeProvider.getArrayType(typeProvider.getObjectType, 1))
    cmp.l should be (objectArrayDesc)
    cmp.r should matchPattern { case LoweredGetField(`arrObj`, offs) if offs == RT.ManagedObj.td.getInstanceFieldOffset => }
  }

  def checkElemCmp(cmp: Cmp, arrObj: Node, elemType: ReferenceType): Unit = {
    cmp.l should be (TypeHandle(elemType))
    cmp.r should matchPattern { case LoweredGetField(LoweredGetField(`arrObj`, tdOffs), baseOffs)
      if tdOffs == RT.ManagedObj.td.getInstanceFieldOffset && baseOffs == RT.JavaInstanceDescriptor.arrayBaseType.getInstanceFieldOffset =>
    }
  }

  def checkRichCmp(cmp: Cmp, valObj: Node, valType: ReferenceType): Unit = {
    val symType = valType.symType
    val possiblyRichNode = valObj match {
      case Deprive(`symType`, x) => x
      case _ => null
    }

    cmp.l should matchPattern { case BitFieldExtract(_, _, false, ConcealRef(`possiblyRichNode`)) => }
    cmp.r should be (IntegralConst(AddrType)(0))
  }

  def checkFalseExitRTSlowPath(branch: If): Unit = {
    val cmp = branch.falseBlock.blockEnd.asInstanceOf[If].selector.asInstanceOf[Cmp]
    cmp.l should matchPattern { case AnyDirectCall(`jrCheckArrayStoreOpt` | `jrCheckArrayStoreNotNull`) => }
  }

  def checkTrueExitSuccessRet(branch: If): Unit = {
    branch.trueBlock.blockEnd should matchPattern { case _: Return => }
  }

  def checkFastPath(asc: ArrayStoreCheck, fastPath: FP): Unit = {
    val arrObj = asc.array
    val valObj = asc.value

    doLowering()

    val testNullBranch = entryBlock.blockEnd.asInstanceOf[If]
    val fpBranch = testNullBranch.falseBlock.blockEnd.asInstanceOf[If]
    val fpCmp = fpBranch.selector.asInstanceOf[Cmp]

    fastPath match {
      case NoFP =>
        checkFalseExitRTSlowPath(testNullBranch)
        checkTrueExitSuccessRet(testNullBranch)

      case Obj1DFP =>
        checkObjCmp(fpCmp, arrObj)
        checkFalseExitRTSlowPath(fpBranch)
        checkTrueExitSuccessRet(fpBranch)

      case StrictFP(elemType) =>
        checkElemCmp(fpCmp, arrObj, elemType)
        checkFalseExitRTSlowPath(fpBranch)
        checkTrueExitSuccessRet(fpBranch)

      case CheckRichFP(valType) =>
        checkRichCmp(fpCmp, valObj, valType)
        checkFalseExitRTSlowPath(fpBranch)
        checkTrueExitSuccessRet(fpBranch)

      case RelaxedFP(elemType, valType) =>
        checkElemCmp(fpCmp, arrObj, elemType)
        checkFalseExitRTSlowPath(fpBranch)

        val richBranch = fpBranch.trueBlock.blockEnd.asInstanceOf[If]
        val richCmp = richBranch.selector.asInstanceOf[Cmp]
        checkRichCmp(richCmp, valObj, valType)
        checkFalseExitRTSlowPath(richBranch)
        checkTrueExitSuccessRet(richBranch)
    }
  }

  private def addDeprivedRichNode(appr: ReferenceApprox) = {
    val UpperBounded(interf: InterfaceType, _) = appr.probableType
    Deprive(interf.symType)(addRichObjNode(interf, appr))
  }

  def createASC(arrObjTA: ReferenceApprox, valueObjTA: ReferenceApprox): ArrayStoreCheck = {
    makeCFG(0)

    makeNodes { at =>
      at(0)
      val arrObj = addObjNode(arrObjTA)

      val valueObj = valueObjTA.probableType match {
        case UpperBounded(_: InterfaceType, _) =>
          addDeprivedRichNode(valueObjTA)
        case _ =>
          addObjNode(valueObjTA)
      }

      val UpperBounded(arrType, _) = arrObjTA

      ArrayStoreCheck(sig(arrType), arrObj, valueObj)
    }
  }

  val strikeOutCases = Seq(
    tp(p(tA1D), p(tA)),
    tp(p(tA1D), p(tB)),
    tp(p(tA1D), c(tB)),
    tp(p(tA1D), c(tIB)),
    tp(p(tA1D), RefNull),
    tp(w(tA1D, p(tB1D)), RefNull),
    tp(p(tInt2D), RefNull),
    tp(p(tI1D), c(tI)),
    tp(p(tI1D), c(tJ)),
    tp(p(tObj1D), c(tObj))
  )

  for (((arrObjTA, valueObjTA), pos) <- strikeOutCases) {
    test(s"strike out: store $valueObjTA into array of $arrObjTA") {
      val asc = createASC(arrObjTA, valueObjTA)
      eliminateUnnecessaryOperations()

      asc.isCommitted should be (false)
      b(0).spine.isEmpty should be (true)
    }
  }

  val throwCases = Seq(
    tp(p(tB1D), p(tC)),
    tp(p(tB1D), p(tA)),
    tp(p(tJ1D), p(tIB)),
    tp(c(tB1D), p(tC)),
    tp(c(tB1D), p(tA)),
    tp(c(tJ1D), p(tIB))
  )

  for (((arrObjTA, valueObjTA), pos) <- throwCases) {
    test(s"throw: store $valueObjTA into array of $arrObjTA") {
      createASC(arrObjTA, valueObjTA)
      eliminateUnnecessaryOperations()

      b(0).spine.toSeq should matchPattern { case Seq(_: ErrorRTSCall) => }
    }
  }

  trait FP
  trait CheckType extends FP {
    def elemType: ReferenceType
  }
  trait CheckRich extends FP {
    def valType: InterfaceType
  }
  case class StrictFP(elemType: ReferenceType) extends CheckType // array element check
  case class CheckRichFP(valType: InterfaceType) extends CheckRich // value enrichment check
  case class RelaxedFP(elemType: ReferenceType, valType: InterfaceType) extends CheckType with CheckRich // array element check & value enrichment check
  case object Obj1DFP extends FP
  case object NoFP extends FP

  val fastPathCases = Seq(
    // array type appr, value type appr, fastpath
    tp(c(tA1D), p(tB), StrictFP(tA)),
    tp(c(tObj1D), p(tB), Obj1DFP),
    tp(c(tA2D), p(tB1D), StrictFP(tA)),
    tp(c(tObj2D), p(tObj1D), NoFP),
    tp(w(tObj2D, c(tAI2D)), p(tObj2D), NoFP),
    tp(w(tObj2D, c(tAI2D)), w(tObj1D, c(tAI1D)), NoFP),
    tp(w(tA1D, p(tB1D)), p(tB), StrictFP(tB)),

    tp(w(tObj1D, c(tI1D)), w(tObj, c(tI)), RelaxedFP(tI, tI)),
    tp(w(tObj1D, c(tI1D)), w(tObj, c(tJ)), RelaxedFP(tI, tJ)),
    tp(w(tObj1D, c(tI1D)), w(tObj, c(tK)), NoFP),
    tp(w(tObj1D, c(tI1D)), c(tI), StrictFP(tI)),
    tp(w(tObj1D, c(tI1D)), c(tJ), StrictFP(tI)),
    tp(w(tObj1D, c(tI1D)), c(tK), NoFP),
    tp(w(tObj1D, c(tI1D)), c(tIB), StrictFP(tI)),
    tp(w(tObj1D, c(tI1D)), c(tJB), StrictFP(tI)),

    tp(w(tObj1D, p(tI1D)), w(tObj, c(tI)), RelaxedFP(tI, tI)),
    tp(w(tObj1D, p(tI1D)), w(tObj, c(tJ)), RelaxedFP(tI, tJ)),
    tp(w(tObj1D, p(tI1D)), w(tObj, c(tK)), NoFP),
    tp(w(tObj1D, p(tI1D)), c(tI), StrictFP(tI)),
    tp(w(tObj1D, p(tI1D)), c(tJ), StrictFP(tI)),
    tp(w(tObj1D, p(tI1D)), c(tK), NoFP),
    tp(w(tObj1D, p(tI1D)), c(tIB), StrictFP(tI)),
    tp(w(tObj1D, p(tI1D)), c(tJB), StrictFP(tI)),

    tp(c(tI1D), w(tObj, c(tI)), RelaxedFP(tI, tI)),
    tp(c(tI1D), w(tObj, c(tJ)), RelaxedFP(tI, tJ)),
    tp(c(tI1D), w(tObj, c(tK)), NoFP),
    tp(c(tJ1D), w(tObj, c(tI)), NoFP),
    tp(c(tJ1D), w(tObj, c(tJ)), RelaxedFP(tJ, tJ)),
    tp(c(tJ1D), w(tObj, c(tK)), NoFP),
    tp(c(tJ1D), c(tI), NoFP),
    tp(c(tJ1D), c(tK), NoFP),
    tp(c(tJ1D), c(tIB), NoFP),

    tp(p(tI1D), w(tObj, c(tI)), CheckRichFP(tI)),
    tp(p(tI1D), w(tObj, c(tJ)), CheckRichFP(tJ)),
    tp(p(tI1D), w(tObj, c(tK)), NoFP),
    tp(p(tJ1D), w(tObj, c(tI)), NoFP),
    tp(p(tJ1D), w(tObj, c(tJ)), CheckRichFP(tJ)),
    tp(p(tJ1D), w(tObj, c(tK)), NoFP),
    tp(p(tJ1D), c(tI), NoFP),
    tp(p(tJ1D), c(tK), NoFP),
    tp(p(tJ1D), c(tIB), NoFP)
  )

  for (((arrObjTA, valueObjTA, fastPathInfo), pos) <- fastPathCases) {
    test(s"fastpath: store $valueObjTA into array of $arrObjTA") {
      val asc = createASC(arrObjTA.withNull, valueObjTA.withNull)
      eliminateUnnecessaryOperations()
      checkFastPath(asc, fastPathInfo)
    }
  }

  test("failed fastpath generation by depriving other type") {
    // create normal configuration and apply ASC optimization
    val asc = createASC(wn(tObj1D, c(tI1D)), wn(tObj, c(tI)))
    asc.hasFastPathInfo should be (false)

    eliminateUnnecessaryOperations()
    asc.hasFastPathInfo should be (true)

    // substitute type of storing value after optimization
    asc.updateArg(3, addDeprivedRichNode(wn(tObj, c(tJ))))

    eliminateUnnecessaryOperations()
    asc.hasFastPathInfo should be (true)

    // check failed relaxed fastpath generation
    checkFastPath(asc, NoFP)
  }

}

