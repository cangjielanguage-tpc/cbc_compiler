/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.devirtualization

import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind
import com.huawei.excelsior.jet.compiler.types.Guards.OpenConeGuard
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ClassType, InterfaceType, ReferenceType}
import com.huawei.excelsior.jet.compiler.types.References._
import com.huawei.excelsior.jet.compiler.types.TypesToolbox

class LightInterfCallsSuite extends CommonDevirtualizationSuite with TypesToolbox {

  import LightInterfCalls._
  import TypeApproximationBuildingHelperStrict._

  private def find(refClass: InterfaceType, rcvTypeAppr: ReferenceApprox): Result = {
    find(refClass, rcvTypeAppr, GuardMode.RealGuards)
  }

  private def find(refClass: InterfaceType, rcvTypeAppr: ReferenceApprox, guardMode: GuardMode): Result = {
    val declClass = if (refClass.isJavaArray) ReferenceType.javaLangObject else refClass
    val call = createInvoke(refClass, from(declClass), MethodAccessKind.INTERFACE)
    call.target match {
      case _: InvokeInterfaceTarget => findLightInterfCallResult(call, rcvTypeAppr, guardMode)
      case _ => Unknown
    }
  }

  private def shouldBeUnknown(refClass: InterfaceType, rcvTypeAppr: Cone) = {
    find(refClass, rcvTypeAppr, GuardMode.RealGuards) should be (Unknown)
    find(refClass, rcvTypeAppr, GuardMode.NoGuards) should be (Unknown)
  }

  private def shouldBeInferred(refClass: InterfaceType, rcvTypeAppr: Cone, klass: ClassType) = {
    val inferred = Inferred(klass)
    find(refClass, rcvTypeAppr, GuardMode.RealGuards) should be (inferred)
    find(refClass, rcvTypeAppr, GuardMode.NoGuards) should be (inferred)
  }

  private def shouldBeGuarded(refClass: InterfaceType, rcvTypeAppr: Cone, klass: ClassType) = {
    val guarded = Guarded(OpenConeGuard(klass), rcvTypeAppr)
    find(refClass, rcvTypeAppr, GuardMode.RealGuards) should be (guarded)
    find(refClass, rcvTypeAppr, GuardMode.NoGuards) should be (Unknown)
  }

  ////////////////////
  // Inferred

  test("unknown from array cone") {
    resetCHA(tI)
    defaultIn(tI)
    shouldBeUnknown(tI, c(tI1D))
  }

  test("error from non-cone approximations") {
    resetCHA(tIBB)
    defaultIn(tI)
    an [Error] should be thrownBy find(tI, RefEmpty)
    an [Error] should be thrownBy find(tI, RefNull)
    an [Error] should be thrownBy find(tI, p(tIB))
  }

  test("inferred from implementing class cone") {
    resetCHA(tIBB)
    in(tA)
    defaultIn(tI)
    shouldBeInferred(tI, c(tIB), tIB)
  }

  // Inferred
  ////////////////////

  ////////////////////
  // Guarded

  {
    def init() = {
      resetCHA(tIB, tJB, tIKB, tC)
      in(tA)
      defaultIn(tI)
      defaultIn(tJ)
      defaultIn(tK)
    }

    val tests = Seq(
      // interface cone
       tp(tJ, c(tJ),  Some(tJB))
      ,tp(tI, c(tK),  Some(tIKB))
      ,tp(tI, c(tJ),  Some(tJB))
      ,tp(tJ, c(tI),  Some(tJB))
      ,tp(tJ, c(tK),  None)

      // probable interface cone
      ,tp(tJ, w(tObj, c(tJ)),  Some(tJB))
      ,tp(tI, w(tObj, c(tK)),  Some(tIKB))
      ,tp(tI, w(tObj, c(tJ)),  Some(tJB))
      ,tp(tJ, w(tObj, c(tI)),  Some(tJB))
      ,tp(tJ, w(tObj, c(tK)),  None)

      // strict partially equal
      ,tp(tJ, c(tB),           Some(tJB))
      ,tp(tJ, w(tObj, c(tB)),  Some(tJB))

      // non-strict partially equal
      ,tp(tI, c(tB),           None)
      ,tp(tI, w(tObj, c(tB)),  None)

      // incompatible
      ,tp(tI, c(tC),           None)
      ,tp(tI, w(tObj, c(tC)),  None)
    )

    for (((refClass, rcvType, klassOption), pos) <- tests) {
      klassOption match {
        case Some(klass) =>
          test(s"guarded ($refClass, $rcvType, $klass)") {
            init()
            shouldBeGuarded(refClass, rcvType, klass)
          }

        case None =>
          test(s"unknown ($refClass, $rcvType)") {
            init()
            shouldBeUnknown(refClass, rcvType)
          }
      }
    }
  }

  // Guarded
  ////////////////////
}
