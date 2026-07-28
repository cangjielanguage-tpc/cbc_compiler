/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.ir.Tag
import com.huawei.excelsior.jet.compiler.opt.middle.explosion.ReconPlacement
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.Primitive
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind as TKind
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeField, FakeType}
import com.huawei.excelsior.jet.util.ScalaCollections

class ReconPlacementSuite extends CompilerSuite with GlobalNodesBuilder with ReconPlacement with IRTransformationsCollection {

  def singleExit(b: Block): ControlNode = ScalaCollections.singleElement(b.blockEnd.exits)
  def singleEnter(b: Block): ControlNode = ScalaCollections.singleElement(b.inputs)

  def someCtrl = GCPoint()
  def newObj = New(sig(ReferenceType.javaLangObject.symType))()

  def refUse(obj: SpinalNode): Node = {
    val field = new FakeField(`type` = Primitive(TKind.INT))
    val u = PutField(field)(obj, IConst(0))
    refUseAt(u, obj)
    u
  }

  def refUseAt(point: ControlNode, obj: SpinalNode): Unit = {
    refUses ++= Map(obj -> (refUses.getOrElse(obj, Set.empty) ++ Set(point)))
  }

  var recons: Map[SpinalNode, Set[ControlNode]] = _
  var refUses: Map[SpinalNode, Set[ControlNode]] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    recons = Map.empty
    refUses = Map.empty
  }

  def reconAfter(after: ControlNode, obj: SpinalNode): Unit = {
    recons ++= Map(obj -> (recons.getOrElse(obj, Set.empty) ++ Set(after)))
  }

  def recon(obj: SpinalNode): Unit = {
    val lastCtrl = someCtrl
    reconAfter(lastCtrl.asInstanceOf[UpperPoint], obj)
  }

  def checkPlacement(): Unit = {
    for ((obj, exps) <- recons) {
      findReconPoints(obj, refUses.getOrElse(obj, Set.empty)).toSet should be (exps map (after => ScalaCollections.singleElement(after.outEdgesByTag(Tag.CONTROL))))
    }
  }


  test("single immediate escape") {
    makeCFG(0)

    makeNodes { at =>
      at(0)
      val obj = newObj
      someCtrl
      recon(obj)
      refUse(obj)
    }

    checkPlacement()
  }

  test("single block") {
    makeCFG(0)

    makeNodes { at =>
      at(0)
      val obj = newObj
      someCtrl
      recon(obj)
      refUse(obj)
    }

    transform(BlocksConnectionTransformation)

    checkPlacement()
  }

  test("dominated escape") {
    makeCFG(0)

    makeNodes { at =>
      at(0)
      val obj = newObj
      recon(obj)
      refUse(obj)
      refUse(obj)
    }

    checkPlacement()
  }

  test("dominated escape branch") {
    makeCFG(0 -> (1 || 2))

    makeNodes { at =>
      at(0)
      val obj = newObj
      recon(obj)
      refUse(obj)

      at(1)
      refUse(obj)

      at(2)
      someCtrl
    }

    checkPlacement()
  }

  test("escape on some branches") {
    makeCFG(0 -> (1 || 2 || 3))

    makeNodes { at =>
      at(0)
      val obj = newObj

      at(1)
      recon(obj)
      refUse(obj)

      at(2)
      recon(obj)
      refUse(obj)

      at(3)
      someCtrl
    }

    checkPlacement()
  }

  test("escape on branch with merge") {
    makeCFG(0 -> (1 || 2) -> 3)

    makeNodes { at =>
      at(0)
      val obj = newObj

      at(1)
      recon(obj)
      refUse(obj)

      at(2)
      someCtrl
      reconAfter(singleExit(2), obj)

      at(3)
      refUse(obj)
    }

    checkPlacement()
  }

  test("escape at block point") {
    makeCFG(0 -> (1 || 2) -> 3)

    makeNodes { at =>
      at(0)
      val obj = newObj

      at(1)
      someCtrl
      reconAfter(singleExit(1), obj)

      at(2)
      someCtrl
      reconAfter(singleExit(2), obj)

      at(3)
      refUseAt(3, obj)
      refUse(obj)
    }

    checkPlacement()
  }

  test("escape in phi") {
    makeCFG(0 -> (1 || 2) -> 3)

    makeNodes { at =>
      at(0)

      at(1)
      val obj = newObj
      someCtrl
      reconAfter(singleExit(1), obj)

      at(2)
      someCtrl

      at(3)
      val phi = Phi(TRefType)(3, obj, Null())
      refUseAt(upperPoint(phi), obj)
    }

    checkPlacement()
  }

  test("escape on two parallel edges") {
    makeCFG(0 -> 1 -> 2 |>| 1 -> 2)

    makeNodes { at =>
      at(0)
      val obj = newObj

      at(1)
      someCtrl
      reconAfter(1.blockEnd.asInstanceOf[If].trueExit, obj)
      reconAfter(1.blockEnd.asInstanceOf[If].falseExit, obj)

      at(2)
      refUseAt(2, obj)
      refUse(obj)
    }

    checkPlacement()
  }

  test("escape in loop") {
    makeCFG(0 -> wd(1) -> 2 )

    makeNodes { at =>
      at(0)
      val placeholder = newObj

      at(1)
      val phi = Phi(TRefType)(1, Null(), placeholder)
      val obj = newObj
      placeholder.replaceValueUsesBy(obj)
      refUseAt(upperPoint(phi), obj)
      someCtrl
      reconAfter(1.blockEnd.asInstanceOf[If].exits.collectFirst{case e if e.target == b(1) => e}.get, obj)

      at(2)
      someCtrl
    }

    checkPlacement()
  }
}
