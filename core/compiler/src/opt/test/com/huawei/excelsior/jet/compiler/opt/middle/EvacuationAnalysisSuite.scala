/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.middle.escape.StackAllocOptimization
import com.huawei.excelsior.jet.compiler.opt.serialization.TestExtraInfo
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.options.BoolOption.Evacuation
import com.huawei.excelsior.jet.compiler.symlevel.{MethodSignature, MethodType, SignatureType, TypeKind as TKind}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{Int32, Int8, Reference, javaLangString}
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.{CLASS, INT, VOID}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethod, FakeMethodReference, FakeMethodType, FakeType}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ClassType
import com.huawei.excelsior.jet.compiler.types.References.OpenCone
import com.huawei.excelsior.jet.util.ScalaCollections

class EvacuationAnalysisSuite extends CompilerSuite with GlobalNodesBuilder with StackAllocOptimization with TestExtraInfo {

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.enable(Evacuation)
  }

  //        symObj
  //          |
  //   EI*  ERoot*
  //     \  /   \
  //      EA*   NEA
  //
  // All * classes are marked as evacuated types.

  val evacuatableRootClass = makeSymClass("ERoot", symObj).markAsEvacuatedType()
  val evacuatableInterface = makeSymInterface("EI")
  val evacuatableClass = makeSymClass("EA", evacuatableRootClass).markAsEvacuatedType().setRawObjectSize(1)

  // Class is non-evacuatable, but his superclass is.
  val nonEvacuatableClass = makeSymClass("NEA", evacuatableClass).setRawObjectSize(1)

  def createFakeMethodTypeBySignatureType(returnTypeKind: SignatureType, paramTypeKinds: SignatureType*): MethodType =
    FakeMethodType.create().changeReturnType(returnTypeKind).changeParameters(paramTypeKinds)

  val nonEvacuateMethodRef = new FakeMethodReference(new FakeMethod(createFakeMethodTypeBySignatureType(Reference(evacuatableRootClass), Reference(evacuatableRootClass))))
  val nonEvacuateWithEvacuatedParamMethodRef = new FakeMethodReference(new FakeMethod(createFakeMethodTypeBySignatureType(Reference(evacuatableRootClass), Reference(evacuatableRootClass))))
  addMethodInfo(nonEvacuateWithEvacuatedParamMethodRef.method, _.copy(alwaysEvacuatedParams = Set(0)))

  val evacuateMethodRef = new FakeMethodReference(new FakeMethod(createFakeMethodTypeBySignatureType(Reference(symObj), Reference(symObj))))
  val evacuateInterfaceMethodRef = new FakeMethodReference(new FakeMethod(createFakeMethodTypeBySignatureType(Reference(symObj), Reference(evacuatableInterface))))

  private def callNonEvacuateMethod(value: Node) = DirectCall(nonEvacuateMethodRef)(value)
  private def callNonEvacuateMethodWithAlwaysEvacuatedParam(value: Node) = DirectCall(nonEvacuateWithEvacuatedParamMethodRef)(value)

  private def callEvacuateMethod(value: Node) = DirectCall(evacuateMethodRef)(value)
  private def callEvacuateInterfaceMethod(value: Node) = DirectCall(evacuateInterfaceMethodRef)(value)

  override def parsableAttributes(): Seq[Attribute] = Seq(
    new SimpleAttribute("newE")({ case Seq() => New(sig(evacuatableClass))() }),
    new SimpleAttribute("newNE")({ case Seq() => New(sig(nonEvacuatableClass))() }),
    new SimpleAttribute("enrich")({ case Seq(obj, wc) => Enrich(evacuatableInterface)(obj, wc) }),
    new SimpleAttribute("nonEvacuateCall")({ case Seq(obj) => callNonEvacuateMethod(obj) }),
    new SimpleAttribute("nonEvacuateCallWAEP")({ case Seq(obj) => callNonEvacuateMethodWithAlwaysEvacuatedParam(obj) }),
    new SimpleAttribute("evacuateCall")({ case Seq(obj) => callEvacuateMethod(obj) }),
    new SimpleAttribute("evacuateInterfaceCall")({ case Seq(obj) => callEvacuateInterfaceMethod(obj) })
  ) ++ super.parsableAttributes()

  def checkEvacuationAt(evacuations: (Seq[String], Option[String])*): Unit = {
    val evacuationCount = ScalaCollections.sumBy(evacuations)(_._1.length)

    allocateObjectsOnStack() shouldBe evacuations.count(_._2.nonEmpty) > 0
    placeEvacuation() shouldBe evacuationCount > 0

    for ((places, id) <- evacuations) {
      id.foreach(n(_) shouldBe a[NewStackAllocated])
      places.foreach(n(_).asInstanceOf[HasInControl].inCtrl shouldBe an[Evacuate])
    }

    all[Evacuate].length shouldBe evacuationCount
  }

  startPhase(CompilerPhase.Lowering)

  test("Stack-allocated by EvacuateAnalysis") {
    makeCFG(1@@("x=newE()", "y=newNE()") -> 2@@("xc=nonEvacuateCall(x)", "yc=nonEvacuateCall(y)"))

    checkEvacuationAt((Seq(), Some("x")))
    n("y") shouldBe a[New]
  }

  test("Multiple evacuation") {
    makeCFG(1@@"x=newE()" -> 2@@"c2=evacuateCall(x)" -> (3@@"c3=evacuateCall(x)" || 4@@"c4=evacuateCall(x)") -> 
      5@@("c5=evacuateCall(x)", "nonEvacuateCall(x)"))

    checkEvacuationAt((Seq("c2", "c3", "c4", "c5"), Some("x")))
  }

  test("Repeated evacuation with various source type") {
    makeCFG(1@@"x=newE()" -> 3@@("w=wc(I,x)", "e=enrich(x,w)", "c1=evacuateInterfaceCall(e)", "c2=evacuateCall(x)") -> 4@@"nonEvacuateCall(x)")

    checkEvacuationAt((Seq("c1", "c2"), Some("x")))
  }

  test("Optimized evacuation count") {
    makeCFG(1@@"x=newE()" -> 2@@("e1=evacuateCall(x)", "e2=evacuateCall(x)") -> 3@@"c=nonEvacuateCall(x)")

    checkEvacuationAt((Seq("e1"), Some("x")))
    all[Evacuate].next().valueUses.length shouldBe 2
  }

  test("Evacuate after call") {
    makeCFG(1@@"x=newE()" -> 2@@"xc=nonEvacuateCall(x)" -> 3@@"r=ret(xc)")

    checkEvacuationAt((Seq("r"), Some("x")))
  }

  test("Evacuate point") {
    makeCFG(1@@"x=newE()" -> (2@@"e1=evacuateCall(x)" || 3@@"e2=evacuateCall(x)") -> 4@@"c=nonEvacuateCall(x)")

    checkEvacuationAt((Seq("e1", "e2"), Some("x")))
  }

  test("Phi-function with difference types") {
    makeCFG(1 -> (2@@"x=newE()" || 3@@"y=newNE()") -> 4@@("z=phi(x,y)", "nonEvacuateCall(z)", "c=evacuateCall(z)"))

    checkEvacuationAt((Seq("c"), Some("x")))
  }

  test("Phi-function with one evacuated type") {
    makeCFG(1 -> (2@@"x=newE()" || 3@@"y=newE()") -> 4@@("z=phi(x,y)", "nonEvacuateCall(z)", "c=evacuateCall(z)"))

    checkEvacuationAt((Seq("c"), Some("x")), (Seq(), Some("y")))  // Evacuation that placed before Call is
  }

  test("Phi-function in loop") {
    makeCFG(1@@"x=newE()" -> 2@@"p=phi(x,y)" -> (3@@"y=nonEvacuateCall(p)" || 4) |>| 3 -> 2)

    checkEvacuationAt((Seq(), Some("x")))
  }

  test("Param with same type as return type") {
    makeCFG(1 -> 2)

    rootMethod.setReturnType(Reference(evacuatableClass))
    replaceByReturn((2: Block).blockEnd, addParam(ClassType(evacuatableClass), OpenCone(ClassType(evacuatableClass), mayBeNull = false)))

    checkEvacuationAt()
  }

  test("Return with non-evacuatable type") {
    makeCFG(1 -> 2)

    rootMethod.setReturnType(Reference(symObj))

    n("p") = addParam(ClassType(evacuatableClass), OpenCone(ClassType(evacuatableClass), mayBeNull = false))
    n("r") = replaceByReturn((2: Block).blockEnd, n("p"))

    checkEvacuationAt((Seq("r"), None))
  }

  test("Evacuated use with RequiredEvacuation use (New will not be stack-allocated)") {
    makeCFG(1@@"x=newE()" -> 2@@("c=evacuateCall(x)", "nonEvacuateCallWAEP(x)"))

    checkEvacuationAt((Seq(), None))
  }

  test("RequiredEvacuation use without evacuation") {
    makeCFG(1@@"x=newE()" -> 2@@("c=nonEvacuateCall(x)", "cWAEP=nonEvacuateCallWAEP(x)", "cE=evacuateCall(x)"))

    checkEvacuationAt((Seq("cE"), Some("x")))
  }

  test("paramHasUnconditionalEscape results") {
    makeCFG(1@@("t1=newE()", "t2=newE()", "evacuateCall(t1)") -> (2@@"evacuateCall(t2)" || 3@@"evacuateCall(t2)") -> 4)
    
    n("p1") = addParam(ClassType(evacuatableClass), OpenCone(ClassType(evacuatableClass), mayBeNull = false))
    n("p2") = addParam(ClassType(evacuatableClass), OpenCone(ClassType(evacuatableClass), mayBeNull = false))
    
    n("t1").replaceValueUsesBy(n("p1"))
    n("t2").replaceValueUsesBy(n("p2"))

    paramHasUnconditionalEscape(n("p1").asInstanceOf[Param]) shouldBe true
    paramHasUnconditionalEscape(n("p2").asInstanceOf[Param]) shouldBe false
  }

  test("Return should not effect the result of paramHasUnconditionalEscape") {
    makeCFG(1 -> (2 || 3) -> 4)

    rootMethod.setReturnType(Reference(evacuatableClass))

    n("p") = addParam(ClassType(evacuatableClass), OpenCone(ClassType(evacuatableClass), mayBeNull = false))
    n("r") = replaceByReturn((2: Block).blockEnd, n("p"))

    paramHasUnconditionalEscape(n("p").asInstanceOf[Param]) shouldBe false
  }
}
