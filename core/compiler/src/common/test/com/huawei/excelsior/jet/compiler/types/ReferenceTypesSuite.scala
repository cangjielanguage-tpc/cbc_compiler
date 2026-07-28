/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.Approximation.CC.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.types.References.*

class ReferenceTypesSuite extends CompilerSuite with TypesToolbox {

  implicit class impl(clazz: ReferenceType) {
    def implements(interf: ReferenceType): Boolean = interf >= clazz
  }

  private val finalClass = Symbol("final")

  test("final class") {
    tC should not be finalClass
    tCF should be (finalClass)
  }

  test("final interface") {
    tI should not be finalClass
  }

  test("final array") {
    tA1D should not be finalClass
    tCF1D should be (finalClass)
  }

  test("final primitive array") {
    tInt1D should be (finalClass)
  }


  test("class implements") {
    (tB implements tI) should be (false)
    (tIB implements tI) should be (true)
  }

  test("class implements hard") {
    (tJB implements tJ) should be (true)
    (tJB implements tI) should be (true)
  }

  test("interface implements") {
    (tJ implements tI) should be (true)
    (tI implements tJ) should be (false)
  }

  test("array implements") {
    (tInt1D implements tI) should be (false)
    (tInt1D implements ReferenceType.javaLangCloneable) should be (true)
    (tInt1D implements ReferenceType.javaIOSerializable) should be (true)
  }


  private def checkSupers(types: ReferenceType*): Unit = {
    for ((t: CohenReferenceType, level) <- types.reverse.zipWithIndex) {
      t.cohenLevel should be (level)
    }

    def nextSuper(t: ReferenceType) = t match {
      case t: ClassOrInterface => t.superclass
      case t: ArrayType => t.cohenSuper
    }
    for (Seq(t, st) <- types.sliding(2)) {
      nextSuper(t) should be (st)
    }
    nextSuper(types.last) should be (null)
  }

  test("simple super") {
    checkSupers(tB, tA, tObj, tJavaType, aLObj, aObj)
  }

  test("interface super") {
    checkSupers(tI, tObj, tJavaType, aLObj, aObj)
  }

  test("simple array creation") {
    tB2D.arrayElement should be (tB1D)
    tInt2D.arrayElement should be (tInt1D)
    tInt1D.arrayElement should be (PrimitiveType(symInt))
    tI2D.arrayElement should be (tI1D)
    tI1D.arrayElement should be (tI)
  }

  test("array super") {
    checkSupers(tB2D, tA2D, tObj2D, tObj1D, tObj, tJavaType, aLObj, aObj)
  }

  test("primitive 2d array super") {
    checkSupers(tInt2D, tObj1D, tObj, tJavaType, aLObj, aObj)
  }

  test("primitive 1d array super") {
    checkSupers(tInt1D, tObj, tJavaType, aLObj, aObj)
  }

  test("interface array super") {
    checkSupers(tI2D, tObj2D, tObj1D, tObj, tJavaType, aLObj, aObj)
  }

  test("thin super") {
    checkSupers(tTXX, tTX, tThinType)
    tThinType.superclass should be (null)
  }

  test("thin final") {
    tTX should not be finalClass
    tTY should not be finalClass
    tTXX should not be finalClass
    tTXXF should be (finalClass)
  }

  test("aj managed super") {
    checkSupers(aLObj, aObj)
    aObj.superclass should be (null)
  }

  def nn(t: ReferenceType): ReferenceType = ReferenceType(SignatureType.NonNullableWrapper(t.sigType.asInstanceOf[SignatureType.NonNullableWrapper.Base]))

  val javaTypePairsInfo = Seq(
  //  t1          t2      lcm(t1, t2)   cmp(t1, t2)
     tp(tA,         tObj,   tObj,         Less)
    ,tp(tA,         tB,     tA,           Greater)
    ,tp(tB,         tC,     tA,           Incomparable)
    ,tp(tB,         tD,     tObj,         Incomparable)

    ,tp(tI,         tI,     tI,           Equal)
    ,tp(tI,         tA,     tObj,         PartiallyEqual)
    ,tp(tI,         tIB,    tI,           Greater)
    ,tp(tI,         tJB,    tI,           Greater)
    ,tp(tJ,         tIB,    tObj,         PartiallyEqual)
    ,tp(tJ,         tJB,    tJ,           Greater)
    ,tp(tI,         tCF,    tObj,         Incomparable)
    ,tp(tJ,         tJ2,    tObj,         PartiallyEqual)

    ,tp(tB2D,       tObj2D, tObj2D,       Less)
    ,tp(tB2D,       tObj1D, tObj1D,       Less)
    ,tp(tA1D,       tA2D,   tObj1D,       Incomparable)
    ,tp(tA1D,       tB1D,   tA1D,         Greater)
    ,tp(tA,         tA1D,   tObj,         Incomparable)

    ,tp(tInt1D,     tObj1D, tObj,         Incomparable)
    ,tp(tInt1D,     tInt2D, tObj,         Incomparable)
    ,tp(tInt2D,     tObj1D, tObj1D,       Less)
    ,tp(tInt2D,     tB1D,   tObj1D,       Incomparable)

    ,tp(tI1D,       tJ1D,   tI1D,         Greater)
    ,tp(tI1D,       tK1D,   tObj1D,       PartiallyEqual)
    ,tp(tI1D,       tIB1D,  tI1D,         Greater)
    ,tp(tJ1D,       tJ21D,  tObj1D,      PartiallyEqual) // common super could be tI1D
    ,tp(tIB1D,      tIX1D,  tObj1D,       Incomparable) // common super could be tI1D
    ,tp(tAI1D,      tIB1D,  tObj1D,       PartiallyEqual)
    ,tp(tI1D,       tCF1D,  tObj1D,       Incomparable)
    ,tp(tAI1D,      tCF1D,  tObj1D,       Incomparable)
    ,tp(tI1D,       tB1D,   tObj1D,       PartiallyEqual)
    ,tp(tI1D,       tB2D,   tObj1D,       Incomparable)
    ,tp(tAI1D,      tB2D,   tAI1D,        Greater)
    ,tp(tI1D,       tInt2D, tObj1D,       Incomparable)
    ,tp(tAI1D,      tInt2D, tAI1D,        Greater)

    ,tp(tI,         tB2D,   tObj,         Incomparable)
    ,tp(tI,         tI1D,   tObj,         Incomparable)
    ,tp(tAI,        tB2D,   tAI,          Greater)
  )

  val ajTypePairsInfo = Seq(
     tp(tTX,        tTY,    tThinType,    Incomparable)
    ,tp(tTX,        tTXX,   tTX,          Greater)
    ,tp(tTXXF,      tTY,    tThinType,    Incomparable)

    ,tp(aObj,       aLObj,  aObj,         Greater)
  )

  for (((t1, t2, tLCM, _), pos) <- javaTypePairsInfo ++ ajTypePairsInfo) {
    test(s"common super: $t1 & $t2") {
      (t1 commonSuper t1) should be (t1)
      (t2 commonSuper t2) should be (t2)
      (t1 commonSuper t2) should be (tLCM)
      (t2 commonSuper t1) should be (tLCM)
      (t1 commonSuper tLCM) should be (tLCM)
      (t2 commonSuper tLCM) should be (tLCM)
      (nn(t1) commonSuper t1) should be (t1)
      (nn(t2) commonSuper t2) should be (t2)
      (nn(t1) commonSuper t2) should be (tLCM)
      (nn(t2) commonSuper t1) should be (tLCM)
      (nn(t1) commonSuper tLCM) should be (tLCM)
      (nn(t2) commonSuper tLCM) should be (tLCM)
    }
  }

  for (((t1, t2, _, cc), pos) <- javaTypePairsInfo ++ ajTypePairsInfo) {
    test(s"compare: $t1 & $t2") {
      (t1 compare t1) should be (Equal)
      (t2 compare t2) should be (Equal)
      (t1 compare t2) should be (cc)
      (t2 compare t1) should be (cc.inverse)
      (nn(t1) compare t1) should be (Equal)
      (nn(t2) compare t2) should be (Equal)
      (nn(t1) compare t2) should be (cc)
      (nn(t2) compare t1) should be (cc.inverse)
    }
  }

  for (((t1, t2, _, _), pos) <- javaTypePairsInfo ++ ajTypePairsInfo) {
    test(s"common super & compare consistency: $t1 & $t2") {
      val tLCM = t1 commonSuper t2
      (t1 compare tLCM) should (be (Less) or be (Equal))
      (t2 compare tLCM) should (be (Less) or be (Equal))
      (nn(t1) compare tLCM) should (be (Less) or be (Equal))
      (nn(t2) compare tLCM) should (be (Less) or be (Equal))
      (t1 compare nn(tLCM)) should (be (Less) or be (Equal))
      (t2 compare nn(tLCM)) should (be (Less) or be (Equal))
      (nn(t1) compare nn(tLCM)) should (be (Less) or be (Equal))
      (nn(t2) compare nn(tLCM)) should (be (Less) or be (Equal))
    }
  }

  for (((t1, t2, _, _), pos) <- javaTypePairsInfo) {
    // ajTypePairsInfo is not included
    // because open cones of Thin types require CHA initialization
    test(s"ReferenceType.compare & ReferenceApprox.compare consistency: $t1 & $t2") {
      val ta1 = OpenCone(t1, mayBeNull = false)
      val ta2 = OpenCone(t2, mayBeNull = false)
      (t1 compare t2) should be (ta1 compare ta2)
      (nn(t1) compare t2) should be (ta1 compare ta2)
      (t1 compare nn(t2)) should be (ta1 compare ta2)
      (nn(t1) compare nn(t2)) should be (ta1 compare ta2)
    }
  }


}
