/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeField, FakeMethod, FakeType}
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, Type, TypeKind, ClassType as SymClassType}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.options.BoolOption.{DoCHA, GlobalInitFieldsAnalysis, NoFieldsTypeAnalysis}
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.compiler.testutils.EnvProvider
import org.scalatest.matchers.{MatchResult, Matcher}
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.Closure

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.chaining.scalaUtilChainingOps


trait TypesToolbox extends EnvProvider { self: CompilerSuite =>

  resetCHA()

  override def beforeEach(): Unit = {
    resetCHA()
    resetEnvironment()
    env.enable(GlobalInitFieldsAnalysis)
    env.disable(NoFieldsTypeAnalysis)
  }

  private val symTypes = mutable.HashMap.empty[String, FakeType]
  private def makeSym(name: String, t: FakeType) = t tap (symTypes(name) = _)
  def sym(name: String) = symTypes(name)
  def sig(name: String): SignatureType = sig(sym(name))
  def sig(tpe: Type): SignatureType = SignatureType.fromSymType(tpe)
  def t(name: String) = ReferenceType(sym(name))

  //     aObj
  //       |
  //     aLObj
  //       |
  //      Obj -------------
  //      / \  \  \   \    \
  //     A   D  |  I   K    X,XX(deferred)
  //    / \  |  |  |\
  //   B   C DD |  J J2
  //   |   |    |
  //   |   |    IX(implements I)
  //   |   |
  //   |  CF(final)
  //   |\
  //   | IB(implements I)
  //   | |\
  //   | | JIB(implements J)
  //   | IBB
  //   | |
  //   | IBBB
  //   |\
  //   | JB(implements J)
  //   | |
  //   | JBF(final)
  //   |\
  //   | JB2(implements J)
  //   |
  //   IKB(implements I,K)
  //   |
  //   IKBB

  //      ThinType
  //      /  \
  //     TX  TY
  //    /  \
  //   TXX TXXF

  val symInt = makeSym("Int", env.getPrimitiveType(TypeKind.INT))
  val symLong = makeSym("Long", env.getPrimitiveType(TypeKind.LONG))
  val symByte = makeSym("Byte", env.getPrimitiveType(TypeKind.BYTE))

  def makeSymClass(name: String, superClass: FakeType, implementedInterfaces: FakeType*) =
    makeSym(name, FakeType(name, TypeKind.CLASS, superClass, implementedInterfaces*).withClinit(true))

  def makeSymRecord(name: String, implementedInterfaces: FakeType*) =
    makeSym(name, FakeType(name, TypeKind.RECORD, null, implementedInterfaces*).withClinit(true))

  val symObj = makeSym("Obj", env.getObjectType)
  val symIter = makeSym("Iter", env.getIteratorType)

  val symA = makeSymClass("A", symObj)
  val sigA = sig(symA)
  val symB = makeSymClass("B", symA)
  val symC = makeSymClass("C", symA)
  val symD = makeSymClass("D", symObj)

  val symCF = makeSymClass("CF", symC).setFinal(true)
  val symDD = makeSymClass("DD", symD)

  def makeSymInterface(name: String, implementedInterfaces: FakeType*) =
    makeSym(name, FakeType(name, TypeKind.INTERFACE, null, implementedInterfaces*))

  val symI = makeSymInterface("I")
  val symJ = makeSymInterface("J", symI)
  val symK = makeSymInterface("K")
  val symJ2= makeSymInterface("J2", symI)

  val symIB = makeSymClass("IB", symB, symI)
  val symIBB = makeSymClass("IBB", symIB)
  val symIBBB = makeSymClass("IBBB", symIBB)
  val symJIB = makeSymClass("JIB", symIB, symJ)
  val symJB = makeSymClass("JB", symB, symJ)
  val symJBF = makeSymClass("JBF", symJB).setFinal(true)
  val symJB2 = makeSymClass("JB2", symB, symJ)
  val symIKB = makeSymClass("IKB", symB, symI, symK)
  val symIKBB = makeSymClass("IKBB", symIKB)

  val symIX = makeSymClass("IX", symObj, symI)

  val symX = makeSymClass("X", symObj).setDeferred()
  val symXX = makeSymClass("XX", symObj).setDeferred()

  def makeSymThinClass(name: String, superClass: FakeType) =
    makeSym(name, FakeType(name, TypeKind.CLASS, superClass).markAsThinClass())

  val symThinType = makeSym("ThinType", env.getThinTypeType)
  val symTX = makeSymThinClass("TX", symThinType)
  val symTY = makeSymThinClass("TY", symThinType)
  val symTXX = makeSymThinClass("TXX", symTX)
  val symTXXF = makeSymThinClass("TXXF", symTX).setFinal(true)

  val symIterImpl = makeSymClass("IterImpl", symObj, symIter)

  def makeSymArray(name: String, base: FakeType, dim: Int) =
    makeSym(name, env.getArrayType(base, dim))

  val tObj = ReferenceType.javaLangObject
  val tA = ClassType(symA)
  val tB = ClassType(symB)
  val tIB = ClassType(symIB)
  val tIBB = ClassType(symIBB)
  val tIBBB = ClassType(symIBBB)
  val tJB = ClassType(symJB)
  val tJBF = ClassType(symJBF)
  val tJB2 = ClassType(symJB2)
  val tJIB = ClassType(symJIB)
  val tIKB = ClassType(symIKB)
  val tIKBB = ClassType(symIKBB)
  val tC = ClassType(symC)
  val tCF = ClassType(symCF)
  val tD = ClassType(symD)
  val tDD = ClassType(symDD)
  val tIX = ClassType(symIX)

  val tI = InterfaceType(symI)
  val tJ = InterfaceType(symJ)
  val tK = InterfaceType(symK)
  val tJ2 = InterfaceType(symJ2)

  val tAI = ReferenceType.javaIOSerializable

  def makeArray(name: String, base: FakeType, dim: Int) =
    JavaArrayType(sig(makeSymArray(name, base, dim)))

  val tObj1D = makeArray("Obj1D", symObj, 1)
  val tObj2D = makeArray("Obj2D", symObj, 2)
  val tA1D = makeArray("A1D", symA, 1)
  val tA2D = makeArray("A2D", symA, 2)
  val tB1D = makeArray("B1D", symB, 1)
  val tB2D = makeArray("B2D", symB, 2)
  val tCF1D = makeArray("CF1D", symCF, 1)
  val tI1D = makeArray("I1D", symI, 1)
  val tI2D = makeArray("I2D", symI, 2)
  val tJ1D = makeArray("J1D", symJ, 1)
  val tK1D = makeArray("K1D", symK, 1)
  val tJ21D = makeArray("J21D", symJ2, 1)

  val sigI1D = sig(tI1D)

  val tIB1D = makeArray("IB1D", symIB, 1)
  val tIX1D = makeArray("IX1D", symIX, 1)

  val tAI1D = makeArray("AI1D", env.getSerializableType, 1)
  val tAI2D = makeArray("AI2D", env.getSerializableType, 2)

  val tInt1D = makeArray("Int1D", symInt, 1)
  val sigInt1D = sig(tInt1D)
  val tInt2D = makeArray("Int2D", symInt, 2)
  val tInt99D = makeArray("Int99D", symInt, 99)

  val tThinType = ReferenceType.ajLangThinType
  val tTX = ClassType(symTX)
  val tTY = ClassType(symTY)
  val tTXX = ClassType(symTXX)
  val tTXXF = ClassType(symTXXF)

  val aObj = ReferenceType.ajLangAJObject
  val aLObj = ReferenceType.ajLangLockableAJObject

  val tJavaType = ReferenceType.javaRefType
  val tScalaType = ReferenceType.scalaRefType
  val tCangjieType = ReferenceType.cangjieRefType

  val tIter = ReferenceType.javaUtilIterator
  val tIterImpl = ClassType(symIterImpl)

  implicit def jt2st(jt: ReferenceType): FakeType = jt.symType.asInstanceOf[FakeType]

  def resetCHA(types: ReferenceType*): Unit = {
    require(types == types.distinct, "no duplicates, please")

    for (t1 <- types; t2 <- types if t1 != t2) {
      require(!(t1 >= t2), s"$t1 is redundant, because of $t2")
      require(!(t2 >= t1), s"$t2 is redundant, because of $t1")
    }

    implicit object SetsAndMaps extends Sets.Default[SymClassType] with Maps.Default[SymClassType]
    def declaredSupers(t: SymClassType) = {
      if (t == symObj) Iterator.single(tJavaType.symType)
      else t.getDeclaredSuperTypes
    }

    val allTypes = Closure(types.map(_.symType))(declaredSupers)

    ProjectLogic.setEnvForUnitTests(env)
    env.chaEnabled = allTypes.nonEmpty
    env.setAllClasses(allTypes.toSeq)
    env.enable(DoCHA)
    CHA.reset(env)
  }

  def withCHA[A](chaTypes: ReferenceType*)(infos: => Seq[A]): Seq[(A, Seq[ReferenceType])] = {
    resetCHASeq(chaTypes)
    infos map { (_, chaTypes) }
  }

  def resetCHASeq(chaTypes: Seq[ReferenceType]): Unit = {
    resetCHA(chaTypes*)
  }

  sealed class TypeApproximationBuildingHelperBase private[TypesToolbox] {
    def e = RefEmpty
    def p(t: ReferenceType): Point = Point(t, mayBeNull = false)
    def c(t: ReferenceType): ReferenceApprox = OpenCone(t, mayBeNull = false)
    def cc(t: ClassType): ReferenceApprox = ClosedCone.max(t, mayBeNull = false)
    def cc(t: ClassType, height: Int): ReferenceApprox = ClosedCone.withHeight(t, mayBeNull = false, height)
    def ccl(t: ClassType, maxLevel: Int): ReferenceApprox = ClosedCone.withMaxLevel(t, mayBeNull = false, maxLevel)

    def n = RefNull
    def cn(t: ReferenceType): ReferenceApprox = c(t).withNull
    def pn(t: ReferenceType): Point = p(t).withNull.asInstanceOf[Point]

    def w(safe: ReferenceApprox, probable: ReferenceApprox): ReferenceApprox = safe.withProbableType(probable)

    def w(t: ReferenceType, p: ReferenceApprox): ReferenceApprox = w(c(t), p)
    def wn(t: ReferenceType, p: ReferenceApprox): ReferenceApprox = w(cn(t), p)
    def wc(t: ClassType, p: ReferenceApprox): ReferenceApprox = w(cc(t), p)
    def wc(t: ClassType, height: Int, p: ReferenceApprox): ReferenceApprox = w(cc(t, height), p)
  }

  /** Import it to use short names for building type approximations.
    * This version allows canonicalization to other types
    * (e.g. `c(final class)` returns `TypePoint`).
    */
  object TypeApproximationBuildingHelperNonStrict extends TypeApproximationBuildingHelperBase

  /** Import it to use short names for building type approximations.
    * This version prohibits any canonicalization
    * (e.g. `c(final class)` throws an error).
    */
  object TypeApproximationBuildingHelperStrict extends TypeApproximationBuildingHelperBase {
    override def c(t: ReferenceType): OpenCone = super.c(t).asInstanceOf[OpenCone]
    override def cc(t: ClassType): ClosedCone = super.cc(t).asInstanceOf[ClosedCone]
    override def cc(t: ClassType, height: Int): ClosedCone = super.cc(t, height).asInstanceOf[ClosedCone]
    override def ccl(t: ClassType, maxLevel: Int): ClosedCone = super.ccl(t, maxLevel).asInstanceOf[ClosedCone]

    override def cn(t: ReferenceType): OpenCone = super.cn(t).asInstanceOf[OpenCone]

    override def w(safe: ReferenceApprox, probable: ReferenceApprox): WidenedUpperBounded = super.w(safe.asInstanceOf[WidenedUpperBounded], probable).asInstanceOf[WidenedUpperBounded].ensuring(_.hasRefinedProbableType)

    override def w(t: ReferenceType, p: ReferenceApprox): OpenCone = super.w(t, p).asInstanceOf[OpenCone]
    override def wn(t: ReferenceType, p: ReferenceApprox): OpenCone = super.wn(t, p).asInstanceOf[OpenCone]
    override def wc(t: ClassType, p: ReferenceApprox): ClosedCone = super.wc(t, p).asInstanceOf[ClosedCone]
    override def wc(t: ClassType, height: Int, p: ReferenceApprox): ClosedCone = super.wc(t, height, p).asInstanceOf[ClosedCone]
  }

  /** Match type approximation considering safe and probable types. */
  def beTA(expected: ReferenceApprox) = new Matcher[ReferenceApprox] {
    def apply(actual: ReferenceApprox) = {
      MatchResult(
        actual.equalsWidened(expected),
        "{0} was not equal to {1}",
        "{0} was equal to {1}",
        Vector(actual, expected)
      )
    }
  }

  /** Match strict type approximation considering safe and probable types. */
  def beTA(expected: (ReferenceApprox, Boolean)) = new Matcher[(ReferenceApprox, Boolean)] {
    def apply(actual: (ReferenceApprox, Boolean)) = {
      MatchResult(
        actual._1.equalsWidened(expected._1) && actual._2 == expected._2,
        "{0} was not equal to {1}",
        "{0} was equal to {1}",
        Vector(actual, expected)
      )
    }
  }

  def makeSymField(name: String, `type`: SignatureType, declaringClass: FakeType): FakeField =
    new FakeField(name, `type`) tap declaringClass.addField

  def makeSymField(name: String, `type`: FakeType, declaringClass: FakeType): FakeField =
    makeSymField(name, sig(`type`), declaringClass)

  def makeSymMethod(name: String, declaringClass: FakeType): FakeMethod =
    new FakeMethod(name) tap declaringClass.addMethod

}
