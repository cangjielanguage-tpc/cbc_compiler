/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.layout

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.layout.MethodTables.*
import com.huawei.excelsior.jet.compiler.layout.MethodTablesScala.RefMap
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeEnvironment, FakeMethod, FakeType}
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, FindMethodImplResult, Method, Type}
import com.huawei.excelsior.jet.compiler.types.TypesToolbox

import scala.language.implicitConversions

class MethodTablesSuite extends CompilerSuite with TypesToolbox {

  import MethodInfo.Mods as M

  object MethodInfo {
    enum Mods {
      case N, // None
           P, // Public
           H, // Hidden (private)
           L, // Local (package private)
           A, // Abstract, public
           S, // Static, public
           F, // Final, public
           f, // Final, package private
           C  // Constructor, public
    }
  }

  case class MethodInfo(name: String, mods: M*) {
    for ((c, mod) <- classes zip mods if mod != M.N) {
      val m = new FakeMethod(name).setStatic(false)
      mod match {
        case M.P => m.setPublic(true)
        case M.H => m.setPublic(false).setPrivate(true)
        case M.L => m.setPublic(false) // package private
        case M.A => m.setPublic(true).setAbstract(true)
        case M.S => m.setPublic(true).setStatic(true)
        case M.F => m.setPublic(true).setFinal(true)
        case M.f => m.setPublic(false).setFinal(true) // package private
        case M.C => m.setPublic(true).setConstructor(true)
        case _ => shouldNotReachHere()
      }
      c.addMethod(m)
    }

    def definedIn(c: FakeType) = c.hasMethod(name)
    def in(c: FakeType) = c.method(name)
  }

  private implicit def method2FromRefClass(method: Method): FromRefClass = FromRefClass(method)
  private case class FromRefClass(method: Method) { selfWrapper =>
    // Note: it is important that RefImpl is not Ref.Comparable,
    //       because VNumMap and Layout should be able to work with any Ref
    //       disregarding its equals and hashCode implementation.
    def from(refClass: ClassType): Ref = RefImpl(method, refClass)
    private case class RefImpl(method: Method, refClass: ClassType) extends Ref
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////

  val A    = FakeType.create(classOf[com.huawei.excelsior.jet.compiler.layout.classes.a.A])
  val B    = FakeType.create(classOf[com.huawei.excelsior.jet.compiler.layout.classes.b.B]) // extends A implements I, J
  val C    = FakeType.create(classOf[com.huawei.excelsior.jet.compiler.layout.classes.a.C]) // extends B implements K
  val I    = FakeType.create(classOf[com.huawei.excelsior.jet.compiler.layout.classes.a.I])
  val J    = FakeType.create(classOf[com.huawei.excelsior.jet.compiler.layout.classes.b.J])
  val K    = FakeType.create(classOf[com.huawei.excelsior.jet.compiler.layout.classes.a.K]) // extends I, J

  val classes = Seq        (/*name*/       A,     B,     C,     I,     J,     K)
  val foo     = MethodInfo ( "foo",      M.H,   M.N,   M.P,   M.P,   M.P,   M.P)
  val bar     = MethodInfo ( "bar",      M.S,   M.P,   M.N,   M.N,   M.N,   M.N)
  val baz     = MethodInfo ( "baz",      M.F,   M.N,   M.N,   M.A,   M.N,   M.N)
  val qux     = MethodInfo ( "qux",      M.L,   M.L,   M.P,   M.N,   M.N,   M.A)
  val wex     = MethodInfo ( "wex",      M.L,   M.N,   M.P,   M.P,   M.P,   M.N)
  val fus     = MethodInfo ( "fus",      M.L,   M.P,   M.P,   M.N,   M.N,   M.N)
  val con     = MethodInfo ( "con",      M.C,   M.C,   M.N,   M.N,   M.N,   M.N)
  val methods = Seq(foo, bar, baz, qux, wex, fus, con)

  val extraClasses = Seq(
    FakeType.create(classOf[java.lang.Object]),
    FakeType.create(classOf[java.lang.String]),
    FakeType.create(classOf[java.lang.Class[_]]),
    FakeType.create(classOf[java.lang.reflect.Method]),
    FakeType.create(classOf[java.util.ArrayList[_]]),
    FakeType.create(classOf[java.util.HashSet[_]]),
    FakeType.create(classOf[java.util.LinkedHashMap[_, _]]),
    FakeType.create(classOf[java.lang.Enum[_]]),
    FakeType.create(classOf[java.util.EnumSet[_]]),
    FakeType.create(classOf[Set[_]]),
    FakeType.create(classOf[scala.collection.mutable.LinkedHashMap[_, _]]),
    FakeType.create(classOf[Type]),
    FakeType.create(classOf[Method])
  )

  ///////////////////////////////////////////////////////////////////////////////////////////////

  abstract class AbstractLayoutBuilder {
    def apply(t: ClassType): Layout
  }

  case object TrivialLayoutBuilder extends AbstractLayoutBuilder {
    def apply(t: ClassType) = MethodTables.buildTrivialMTLayoutStraight(t, new FakeEnvironment)
  }

  case object OptimizedLayoutBuilder extends AbstractLayoutBuilder {
    def apply(t: ClassType) = MethodTablesScala.buildMTLayout(t, new FakeEnvironment)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////

  for (vnum <- Seq(new VNumMapDefault, new RefMap)) {
    test(s"VNumMap.Builder consistency: ${vnum.getClass.getSimpleName}") {

      // populate with fake refs
      var size = 0
      for (c <- classes; m <- methods if m definedIn c) {
        val ref = m in c from c
        if (canBeInMethodTable(m in c) && !vnum.contains(ref)) {
          vnum.update(ref, size)
          size += 1
        }
      }

      for (ref <- vnum.refs) {
        withClue(s"Ref(${ref.refClass}, ${ref.method}), vnum: ${vnum(ref)}") {
          val _ref = ref.method from ref.refClass
          (vnum contains _ref) shouldBe true
          vnum(_ref) shouldBe vnum(ref)
        }
      }
    }
  }

  for (build <- Seq(TrivialLayoutBuilder, OptimizedLayoutBuilder); tpe <- classes ++ extraClasses) {
    test(s"$build correctness for $tpe") {
      val layout = build(tpe)
      val size = layout.size
      val allRefs = for {
        c <- tpe +: tpe.getSuperClasses ++: tpe.allSuperInterfaces.toSeq
        m <- c.getDeclaredMethods if canBeInMethodTable(m)
      } yield m from c
      for (ref <- allRefs) {
        val vnum = layout.vnum(ref)
        vnum should (be < size)

        if (vnum == NO_VNUM) {
          for (_ref <- allRefs if Ref.equals(_ref, ref)) {
            layout.vnum(_ref) shouldEqual NO_VNUM
          }

        } else {
          for (_ref <- allRefs if !Ref.equals(_ref, ref)) {
            layout.vnum(_ref) should not equal vnum
          }
        }

        tpe.findMethodImplementation(ref) match {
          case r: FindMethodImplResult.Found if !r.result.isFinal =>
            vnum should not equal NO_VNUM
          case _ =>
        }
      }

      if (tpe.isClass) {
        val inums = layout.inums
        for ((interf, idx) <- tpe.allSuperInterfaces.zipWithIndex) {
          val inum = inums(idx)
          val interfLayout = build(interf)
          inum should not equal NO_INUM
          (inum + interfLayout.size - 1) should (be < size) // check last slot of IMT
        }
      }
    }
  }

  for {
    (l, r, res) <- Seq(
      // normal equality (without package private)
       (foo in C from C,   foo in C from C,   true)  // same methods
      ,(foo in C from C,   qux in C from C,   false) // different methods

      ,(foo in C from C,   foo in I from I,   true)
      ,(foo in C from B,   foo in C from C,   true) // different refClass
      ,(foo in C from B,   foo in I from A,   true)
      ,(foo in J from A,   foo in I from B,   true)

      // package private equality
      ,(qux in A from A,   qux in B from B,   false)
      ,(qux in B from B,   qux in C from C,   false)
      ,(qux in B from B,   qux in K from K,   false)
      ,(qux in A from A,   qux in C from C,   true)
      ,(qux in A from A,   qux in K from K,   true)
      ,(qux in C from C,   qux in K from K,   true)
      ,(qux in A from C,   qux in C from K,   true)

      ,(wex in A from A,   wex in I from I,   true)
      ,(wex in A from A,   wex in I from B,   false)
      ,(wex in A from A,   wex in J from J,   false)
      ,(wex in C from C,   wex in J from J,   true)
      ,(wex in C from C,   wex in I from I,   true)
      ,(wex in C from C,   wex in I from B,   true)

      ,(fus in A from A,   fus in B from B,   false)
      ,(fus in C from C,   fus in B from B,   true)
      ,(fus in C from C,   fus in A from A,   true)

      // Weird cases
      ,(bar in A from A,   bar in A from B,   false) // same static method
      ,(bar in A from A,   bar in B from B,   false) // different static method
      ,(con in A from A,   con in A from B,   false) // same constructor
      ,(con in A from A,   con in B from B,   false) // different constructor

      // JLS and Verifier shield us from such refs, but equality currently handles them somehow
      ,(foo in A from A,   foo in C from C,   false) // private method
      ,(foo in A from C,   foo in C from C,   false) // private method from another class
      ,(qux in A from B,   qux in B from B,   true) // package private method from refClass in different package
      ,(qux in A from A,   qux in B from A,   true) // package private method from refClass in different package
    )
  } {
    test(s"Ref equality of $l and $r") {
      Ref.equals(l, r) shouldBe res
      Ref.equals(r, l) shouldBe res
    }
  }

}
