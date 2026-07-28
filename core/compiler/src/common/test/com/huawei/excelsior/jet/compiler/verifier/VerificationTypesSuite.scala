/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeEnvironment, FakeMethod, FakeType, FakeVerifiableMethod, FakeVerifiableType}
import com.huawei.excelsior.jet.compiler.symlevel.Type as SymType

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.control.NoStackTrace

class VerificationTypesSuite extends CompilerSuite {

  private implicit def str2xstr(str: String): XString = XString.ascii(str)

  private type Type = VerificationTypes#VerificationType
  private type LazyType = (VerificationTypes, FakeEnvironment) => Type

  /** Lazy wrapper for simple type. */
  private def t(lazyType: VerificationTypes => Type): LazyType =
    (types, _) => {
      lazyType(types)
    }

  /** Lazy wrapper for type with extra actions. */
  private def m(lazyType: LazyType)(actions: (VerificationTypes, FakeEnvironment, Type) => Type): LazyType =
    (types, env) => {
      actions(types, env, lazyType(types, env))
    }

  /** Lazy wrapper for type with resolution. */
  private def r(lazyType: LazyType, resolution: SymType): LazyType =
    m(lazyType) { (_, env, tpe) =>
      assert(tpe.isClassOrArrayOrNull, "required to use #toString() as #getClassName()")
      val className = tpe.toString
      assert(className == resolution.getName)
      env.typesResolution.put(className, asClassType(resolution))
      tpe
    }


  /** Class or array of classes. */
  private def c(name: String, dimNum: Int) = t(_.classOf(name, dimNum))
  /** Class or array of classes. */
  private def c(sig: String) = t(_.classOf(sig))

  private def c(klass: Class[?]): (LazyType, LazyType) = {
    val symType = FakeType.create(klass)
    val nonResolvable = c(symType.getName)
    val resolvable = r(nonResolvable, symType)
    (nonResolvable, resolvable)
  }

  /** Array of other types. */
  private def a(ltpe: LazyType, dimNum: Int) = m(ltpe) { (types, _, tpe) =>
    types.array(tpe, dimNum)
  }


  object Sandbox {
    class Foo
    class Bar extends Foo
    class Baz extends Foo
    final class Qux extends Bar

    class RefClass

    trait Interf

    class Dfr
    class Dfr2
  }
  import Sandbox.*
  FakeType.create(classOf[Dfr]).setDeferred()
  FakeType.create(classOf[Dfr2]).setDeferred()


  private val top = t(_.TOP)

  private val refOrRet = t(_.REFERENCE_OR_RETURN_ADDRESS)
  private val ref = t(_.REFERENCE)
  private val null_ = t(_.NULL)

  private val ret = t(_.RETURN_ADDRESS)

  private val (obj, _) = c(classOf[Object])
  private val obj1D = a(obj, 1)

  private val (foo, fooR) = c(classOf[Foo])
  private val foo1D = a(foo, 1)
  private val foo1DR = a(fooR, 1)
  private val foo2DR = a(fooR, 2)

  private val (_, barR) = c(classOf[Bar])
  private val bar1DR = a(barR, 1)

  private val (qux, quxR) = c(classOf[Qux])

  private val (_, bazR) = c(classOf[Baz])
  private val baz1DR = a(bazR, 1)

  private val (interf, interfR) = c(classOf[Interf])
  private val interf1DR = a(interfR, 1)
  private val interf2DR = a(interfR, 2)

  private val (_, dfr) = c(classOf[Dfr]) // may be some class, may be some interface, ...
  private val dfr1D = a(dfr, 1)
  private val dfr2D = a(dfr, 2)

  private val (_, dfr2) = c(classOf[Dfr2]) // may be some class, may be some interface, ...

  private val int = t(_.INT)
  private val int1D = a(int, 1)
  private val int2D = a(int, 2)

  private val long = t(_.LONG)
  private val long2D = a(long, 2)

  private val uninitThis = t(_.thisType(false))
  private val uninitFoo = t(_.uninitialized(3))
  private val uninitBar = t(_.uninitialized(5))



  private class VerificationInfoBuilder extends AbstractVerifier.InfoBuilder {
    val pairs = mutable.HashSet.empty[(SymType, SymType)]

    override def addVerificationPairImpl(from: SymType, to: SymType, message: XString): Unit = pairs += ((to, from))
    override def getObjectType: VerifiableType = new FakeVerifiableType(FakeType.create(classOf[Object]))
  }

  private def isAssignable(to: Type, from: Type) =
    to.isAssignableFrom(from, new FakeVerifiableMethod(new FakeMethod("fakeContext")))

  private def testBasics(t: Type): Unit = {
    (t equals t) should be (true)
    isAssignable(t, t) should be (true)
    (t merge t) should be (t)
  }

  private def test(name: String, lazyTypes: Seq[LazyType], expectedPairs: Option[(Class[?], Class[?])] = None)
                  (testFun: Seq[Type] => Unit): Unit = {
    registerTest(name) {
      val env = new FakeEnvironment
      val builder = new VerificationInfoBuilder
      val refClass = new FakeVerifiableType(FakeType.create(classOf[RefClass]))
      val types = new VerificationTypes(env.getTypeProvider, builder, refClass)
      val ts = lazyTypes map (_(types, env))
      try {
        ts foreach testBasics

        testFun(ts)

        refClass.verificationImports should be (env.typesResolution.values.toSet filter (_.hasVerificationInfo))

        val expectedPairsSet = expectedPairs.toSet // make Scala type checker God happy
        builder.pairs should be (expectedPairsSet map { case (t, f) => (FakeType.create(t), FakeType.create(f)) })
      } catch {
        case e: Throwable =>
          throw new Throwable(s"$name failed on ${ts.mkString("(", ", ", ")")}", e) with NoStackTrace {
            override def toString: String = getMessage
          }
      }
    }
  }


  private val similarArrays = Seq(
     c("[[Lfoo/Bar;")
    ,c("foo/Bar", 2)
    ,a(c("foo/Bar"), 2)
    ,a(a(c("foo/Bar"), 1), 1)
    ,a(c("[Lfoo/Bar;"), 1)
  )
  test(s"array factories", similarArrays) { arrayTypes =>
    val standard = arrayTypes.head
    for (t <- arrayTypes) {
      t should be (standard)
    }
  }


  private val assignableFromCases = Seq(
     (top,        int1D,      top)

    ,(refOrRet,   int1D,      top)
    ,(refOrRet,   null_,      top)
    ,(refOrRet,   ret,        top)

    ,(ref,        int1D,      top)
    ,(ref,        uninitFoo,  top)
    ,(ref,        null_,      top)

    ,(obj,        int1D,      obj)
    ,(obj,        obj1D,      obj)
    ,(obj,        foo,        obj)
    ,(obj1D,      int2D,      obj1D)
    ,(obj1D,      foo1D,      obj1D)
    ,(fooR,       barR,       foo)
    ,(foo1D,      bar1DR,     foo1DR)

    ,(obj,        interf,     obj)
    ,(interfR,    obj,        obj)
    ,(interfR,    int1D,      obj)
    ,(interf1DR,  interf2DR,  obj1D)
    ,(interf1DR,  foo1DR,     obj1D)


    ,(null_,      null_,      null_)
    ,(int1D,      null_,      int1D)
    ,(qux,        null_,      qux)
  )
  for (((leftL, rightL, mergedL), i) <- assignableFromCases.zipWithIndex) {
    test(s"isAssignableFrom positive #$i", Seq(leftL, rightL, mergedL)) { case Seq(left, right, merged) =>
      isAssignable(left, right) should be (true)
      left merge right should be (merged)
      right merge left should be (merged)
    }
  }

  private val nonAssignableCases = Seq(
     (int,        long,       top)
    ,(obj,        int,        top)
    ,(int,        obj,        top)

    ,(int,        int1D,      top)
    ,(int1D,      int,        top)
    ,(int2D,      long2D,     obj1D)
    ,(int1D,      long2D,     obj)
    ,(int1D,      int2D,      obj)

    ,(barR,       obj,        obj)
    ,(barR,       fooR,       fooR)
    ,(barR,       bazR,       fooR)
    ,(bar1DR,     foo1DR,     foo1D)
    ,(foo1DR,     foo2DR,     obj1D)
    ,(bar1DR,     baz1DR,     foo1DR)
    ,(obj1D,      int1D,      obj)
    ,(foo1DR,     int2D,      obj1D)

    ,(null_,      obj,        obj)

    ,(uninitThis, uninitFoo,  top)
    ,(uninitFoo,  uninitBar,  top)

    ,(ref,        int,        top)
    ,(ref,        ret,        top)
    ,(refOrRet,   int,        top)

    ,(ret,        obj,        top)
  )
  for (((leftL, rightL, mergedL), i) <- nonAssignableCases.zipWithIndex) {
    test(s"isAssignableFrom negative #$i", Seq(leftL, rightL, mergedL)) { case Seq(left, right, merged) =>
      isAssignable(left, right) should be (false)
      left merge right should be (merged)
      right merge left should be (merged)
    }
  }


  private val deferredAssignableCases = Seq(
     (dfr,     fooR,    true,   Some(classOf[Dfr], classOf[Foo]))
    ,(fooR,    dfr,     true,   Some(classOf[Foo], classOf[Dfr]))
    ,(foo1DR,  dfr1D,   true,   Some(classOf[Foo], classOf[Dfr]))
    ,(dfr,     null_,   true,   None)

    ,(dfr,     obj1D,   true,   Some(classOf[Dfr], classOf[Object]))
    ,(dfr1D,   int2D,   true,   Some(classOf[Dfr], classOf[Object]))
    ,(dfr1D,   dfr2D,   true,   Some(classOf[Dfr], classOf[Object]))

    ,(quxR,    dfr,     true,   Some(classOf[Qux], classOf[Dfr])) // Qux is final but we are not so smart.

    ,(int,     dfr,     false,  None)
    ,(int1D,   dfr,     false,  None)
    ,(obj1D,   dfr,     false,  None)
  )
  for (((leftL, rightL, assignable, pairs), i) <- deferredAssignableCases.zipWithIndex) {
    test(s"deferred assignable #$i", Seq(leftL, rightL), pairs) { case Seq(left, right) =>
      isAssignable(left, right) should be (assignable)
    }
  }

  private val deferredMergeCases = Seq(
     (dfr,     fooR,    fooR)
    ,(fooR,    dfr,     fooR)
    ,(foo1DR,  dfr1D,   foo1DR)
    ,(dfr,     null_,   dfr)

    ,(dfr,     dfr2,    dfr)

    ,(quxR,    dfr,     quxR)

    ,(int,     dfr,     top)
    ,(int1D,   dfr,     obj)
    ,(obj1D,   dfr,     obj)
    ,(dfr1D,   int2D,   obj1D)
    ,(dfr1D,   dfr2D,   obj1D)
  )
  for (((leftL, rightL, mergedL), i) <- deferredMergeCases.zipWithIndex) {
    test(s"deferred merge #$i", Seq(leftL, rightL, mergedL)) { case Seq(left, right, merged) =>
      left merge right should be (merged)
    }
  }

}

