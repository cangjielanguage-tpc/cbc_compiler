/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind
import com.huawei.excelsior.jet.compiler.{CompilerSuite, symlevel}
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.symlevel.MethodSignature
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{Int32, javaLangString}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethodReference, FakeType}

import scala.collection.immutable.Seq

class SpecializedMergeSuite extends CompilerSuite
  with GlobalNodesBuilder
  with CFGTransformationDSL {

  override def transformation(): Unit = {}

  override def makeDebug: Boolean = false

  private def addToStringCall(value: Node) = {
    val refClass = FakeType.create(classOf[Integer])
    val methodRef = new FakeMethodReference(
      refClass.findMethodOrNull(XString("toString"), MethodSignature(Int32)(javaLangString)),
      MethodAccessKind.STATIC,
      refClass
    )
    DirectCall(methodRef)(value)
  }

  override def parsableAttributes() = Seq(
    new SimpleAttribute("toString")({ case Seq(v) => addToStringCall(v) }),
    new SimpleAttribute("mergePoint")({ case Seq() => ScopeAnchor() })
  ) ++ super.parsableAttributes()

  test("Dataflow merge") {
    makeCFG(1 -> 2)

    val (scope, ic) = createScopeWithState(Scope.createAnchor(1: Block), (1: Block).pos) {
      val ic = IConst(42)
      currentScope.setResult(Void())
      ic
    }
    scope.merge()

    ic.scope shouldBe rootScope
  }

  test("Linear merge") {
    makeCFG(1 -> 2)

    val (scope, toStringCall) = createScopeWithState(Scope.createAnchor(1: Block), (1: Block).pos) {
      val call = addToStringCall(IConst(42))
      currentScope.setResult(Void())
      call
    }

    scope.merge()

    (1: Block).outCtrl shouldBe toStringCall

    val ic = toStringCall.argsByTag(Tag.VALUE).find(_.isInstanceOf[IConst]).get
    ic.scope shouldBe rootScope
  }

  test("Insert temp `ScopeAnchor` node") {
    makeCFG(1@@"x=spinal()" -> 2)

    val xSpinal = n("x").asInstanceOf[SpinalNode]
    val sa = Scope.createAnchor(xSpinal)

    xSpinal.outCtrl shouldBe sa
  }

  test("Insert if by using insertCode") {
    beforeWithPost(1@@("c1=ic(4)", "c2=ic(2)", "x=spinal()") -> 2, {
      insertCodeAfter(n("x").asInstanceOf[SpinalNode], useDefaultHandler = true) {
        val _if = If(Cmp(IntType, Condition.LT)(n("c1"), n("c2")))

        continue(_if.trueExit)
        val g1 = Goto()

        continue(_if.falseExit)
        val g2 = Goto()

        val phi = join(IConst(42) at g1, IConst(-42) at g2)
        addToStringCall(phi)
      }
    })

    after(1 -> 2 -> (3 || 4) -> 5 -> 6 -> 7)
  }

  test("Create scope with replaceable node") {
    makeCFG(1@@"x=spinal()" -> 2)

    val xSpinal = n("x").asInstanceOf[SpinalNode]

    val (scope, toStringCall) = createScopeWithState(xSpinal, (1: Block).pos) {
      val call = addToStringCall(IConst(42))
      currentScope.setResult(Void())
      call
    }
    scope.merge()

    xSpinal.isCommitted shouldBe false
    (1: Block).outCtrl shouldBe toStringCall
    (1: Block).blockEnd.inMemory shouldBe toStringCall
  }

  test("Use strikeOut from inner Scope") {
    makeCFG(1@@("c1=ic(4)", "c2=ic(2)", "x=spinal()") -> 2)

    val x = n("x").asInstanceOf[SpinalNode]
    insertCodeAfter((1: Block), useDefaultHandler = true) {
      val _if = If(Cmp(IntType, Condition.LT)(n("c1"), n("c2")))

      continue(_if.trueExit)
      val g1 = Goto()

      continue(_if.falseExit)
      val g2 = Goto()

      val phi = join(IConst(42) at g1, IConst(-42) at g2)
      addToStringCall(phi)

      strikeOut(x)
    }

    x.isCommitted shouldBe false
  }
  
  test("Complex memory-graph repair") {
    makeCFG(1@@"data=write()" -> (2 || 3 -> 4) -> 5@@"data5=write()")

    insertCodeBefore(3.blockEnd, useDefaultHandler = true) {FakeSpinalX(IntType)()}
    checkIRConsistency(CheckLevels.Important)
  }
}
