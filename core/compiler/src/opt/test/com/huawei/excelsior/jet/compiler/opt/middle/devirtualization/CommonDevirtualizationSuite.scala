/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.devirtualization

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, MethodAccessKind as MAK}
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodReference}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethod, FakeMethodReference, FakeType}
import com.huawei.excelsior.jet.compiler.types.TypesToolbox
import org.junit.Ignore

import scala.collection.mutable

@Ignore
abstract class CommonDevirtualizationSuite extends CompilerSuite with GlobalNodesBuilder with TypesToolbox {

  val METHOD_NAME = "foo"

  // Method declaration DSL
  private val klassesWithMethods = mutable.Set.empty[FakeType]
  private def inImpl(klasses: Seq[ReferenceType], methodModifier: FakeMethod => FakeMethod) = {
    for (klass <- klasses) yield {
      val symKlass = klass.symType.asInstanceOf[FakeType]
      val m = methodModifier(makeSymMethod(METHOD_NAME, symKlass).setPublic(true).setStatic(false))
      symKlass.addMethod(m)
      klassesWithMethods += symKlass
      m
    }
  }
  def in(klasses: ReferenceType*) = inImpl(klasses, { m => assert(m.getDeclaringClass.isClass); m })
  def defaultIn(klasses: ReferenceType*) = inImpl(klasses, { m => assert(m.getDeclaringClass.isInterface); m })
  def abstractIn(klasses: ReferenceType*) = inImpl(klasses, { m => m.setAbstract(true) })
  def finalIn(klasses: ReferenceType*) = inImpl(klasses, { m => m.setFinal(true) })
  def privateIn(klasses: ReferenceType*) = inImpl(klasses, { m => m.setPublic(false).setPrivate(true) })

  def from(klass: ReferenceType): FakeMethod = {
    klass.symType.asInstanceOf[FakeType].method(METHOD_NAME)
  }


  override def beforeEach(): Unit = {
    super.beforeEach()
    makeCFG(0)
    assert(klassesWithMethods.isEmpty)
    assert(abstractClasses.isEmpty)
  }

  override def afterEach(): Unit = {
    klassesWithMethods foreach (_.clearMethods())
    klassesWithMethods.clear()

    abstractClasses foreach (_.setAbstractClass(false))
    abstractClasses.clear()

    super.afterEach()
  }


  private val abstractClasses = mutable.Set.empty[ReferenceType]

  def abstractClass(klass: ReferenceType): Unit = {
    assert(!klass.isAbstractClass)
    klass.setAbstractClass(true)
    abstractClasses += klass
  }

  val testBCPos = 37

  def createInvoke(refClass: ReferenceType, original: Method, akind: MAK): Call = {
    assert((refClass.isInterface && akind == MAK.INTERFACE) || ((refClass.isClass || refClass.isJavaArray) && akind == MAK.VIRTUAL))

    makeNodes { at =>
      at(0)
      val methodRef = new FakeMethodReference(original, akind, refClass)
      withPos(BytecodePosition(testBCPos, InlineContext.newRoot(rootMethod))) {
        val receiver = Fake(ValueType(refClass.symType))
        val args = if (methodRef.isInterfCall) {
          Seq(InvokeInterfaceTarget(methodRef)(receiver, Fake(LongType)), receiver)
        } else if (methodRef.hasNonRecordReceiverParameter) {
          Seq(InvokeTarget.instance(methodRef)(receiver), receiver)
        } else {
          Seq(InvokeTarget.static(methodRef))
        }
        Call(methodRef)(args*)
      }
    }
  }

}
