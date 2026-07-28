/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.middle.ContextTypesRecalculation
import com.huawei.excelsior.jet.compiler.types.Guards.{Guard, OpenConeGuard, PointGuard}
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo.PGO
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.types.TypesToolbox

class TauSwitchSuite extends CompilerSuite with TypesToolbox with GlobalNodesBuilder with ContextTypesRecalculation with IRTransformationsCollection {

  import TypeApproximationBuildingHelperStrict._

  override def afterEach(): Unit = {
    rootMethod.setReturnType(SignatureType.javaLangObject)
    super.afterEach()
  }

  startPhase(CompilerPhase.PostInline)

  def replaceByTauSwitch(sw: Switch, obj: Node, guards: Seq[Guard]) = {
    val ts = TauSwitch(guards, PGO(Seq.tabulate(guards.size)(_ + 1), guards.size + 1))(obj)
    assert(sw.exits.indexOf(sw.defaultExit) == ts.exits.indexOf(ts.defaultExit))
    for ((swe, tse) <- sw.exits zip ts.exits) {
      swe replaceUsesBy tse
    }
    decommit(sw)
    ts
  }

  test("drop exit") {
    makeCFG(0 -> (2 || 2 || 3 || 5))

    makeNodes { at =>
      at(0)
      val obj = addObjNode(c(tObj))
      replaceByTauSwitch(b(0).blockEnd.asInstanceOf[Switch], obj, Seq(PointGuard(tB), PointGuard(tC), PointGuard(tD)))
    }

    def ts = b(0).blockEnd.asInstanceOf[TauSwitch]

    while (b(0).succBlocks.size > 1) {
      val toDrop = ts.caseExits.head
      val info = ts.info
      val expectedSuccs = b(0).succBlocks.filter(_ != toDrop.target).toSeq
      AnySwitch.dropExits(toDrop)
      b(0).succBlocks.toSeq should be (expectedSuccs)
      ts.info shouldBe PGO(info.trueWeights.tail, info.trueWeights.head + info.falseWeight)
    }

    an[AssertionError] should be thrownBy AnySwitch.dropExits(ts.defaultExit)

  }

  test("exit filters") {
    makeCFG(0 -> (1 || 2 || 3 || 4))

    val obj = makeNodes { at =>
      at(0)
      val obj = addObjNode(c(tObj))
      val sw = b(0).blockEnd.asInstanceOf[Switch]
      replaceByTauSwitch(sw, obj, Seq(PointGuard(tA), PointGuard(tB), OpenConeGuard(tD)))

      obj
    }

    recalculateContextTypes()
    recalculateContextTypes() // recalculate ContextTypes after switch replacement

    nodeTypeAt(obj, b(1)) should be (c(tObj)) // default
    nodeTypeAt(obj, b(2)) should be (p(tA))
    nodeTypeAt(obj, b(3)) should be (p(tB))
    nodeTypeAt(obj, b(4)) should be (c(tD))
  }

  test("redundant exit") {
    makeCFG(0 -> ((1 -> (3 || 4 || 5 || 6)) || 2))

    val obj = makeNodes { at =>
      at(0)
      val obj = addObjNode(c(tObj))
      b(0).blockEnd.asInstanceOf[If].selector = TypeTest(OpenConeGuard(tA), TauInfo.Unknown)(obj)

      at(1)
      GCPoint()
      val sw = b(1).blockEnd.asInstanceOf[Switch]
      replaceByTauSwitch(sw, obj, Seq(PointGuard(tB), PointGuard(tC), PointGuard(tD)))

      obj
    }

    recalculateContextTypes()
    recalculateContextTypes()

    nodeTypeAt(obj, b(1)) should be (c(tA))
    nodeTypeAt(obj, b(2)) should be (c(tObj))

    b(1).succBlocks.toSeq should be (Seq(b(3), b(4), b(5)))
  }

}
