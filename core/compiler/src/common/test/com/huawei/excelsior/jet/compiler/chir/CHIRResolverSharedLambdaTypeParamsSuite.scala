/*
 *   Copyright (C) 2026 Huawei Technologies Co., Ltd.
 *   All rights reserved.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.chir.CHIR.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeEnvironment
import org.scalatest.funsuite.AnyFunSuite

/** Regression test: cjc may create a generic lambda function as a global
  * function whose genericTypeParams are the shared GenericType objects of the
  * lambda class. Those are already registered as class type variables; they
  * must not be re-registered as local ones (which asserts on the mismatch).
  */
class CHIRResolverSharedLambdaTypeParamsSuite extends AnyFunSuite {

  private class TestPackage(typeDefsSeq: Seq[CustomTypeDef], valuesSeq: Seq[Value]) extends CHIR.Package {
    override def name: String                                     = "test"
    override def typeDefs: Iterator[CustomTypeDef]                = typeDefsSeq.iterator
    override def values: Iterator[Value]                          = valuesSeq.iterator
    override def function(idx: Int): Func                         = throw new UnsupportedOperationException
    override def packageInitFunc: Func                            = throw new UnsupportedOperationException
    override def packageInitLiteralFunc: Func                     = throw new UnsupportedOperationException
    override def getFunc(identifier: String): Option[Func]        = None
    override def getDef(identifier: String): Option[CustomTypeDef] = None
  }

  private class TestGenericType extends GenericType {
    override def identifier: String     = "T"
    override def upperBounds: Seq[Type] = Nil
  }

  private class TestCustomType(tpeDef: CustomTypeDef, params: Seq[Type]) extends CustomType {
    override def typeDef: CustomTypeDef                          = tpeDef
    override def genericTypeParams: Seq[Type]                    = params
    protected override def equalsImpl(that: CustomType): Boolean = this eq that
  }

  private class TestCustomTypeDef(tpeValue: => Type) extends CustomTypeDef {
    override def tpe: Type                                       = tpeValue
    override def packageName: String                             = "std.core"
    override def identifier: String                              = "std.core:Lambda1"
    override def srcCodeIdentifier: String                       = "Lambda1"
    override def instanceVars: Seq[InstanceVar]                  = Nil
    override def staticVars: Seq[GlobalVar]                      = Nil
    override def methods: Seq[Func]                              = Nil
    override def vtables: Seq[VTable]                            = Nil
    override def implementedInterfaces: Seq[ClassType]           = Nil
    override def attributes: Seq[Attribute]                      = Nil
    override def annotations: Seq[Annotation]                    = Nil
    protected override def equalsImpl(that: CustomTypeDef): Boolean = this eq that
  }

  private class TestFunc(genericParams: Seq[GenericType]) extends Func {
    override def tpe: FuncType                       = throw new UnsupportedOperationException
    override def id: Long                            = 1
    override def identifier: String                  = "std.core:lambda$apply"
    override def srcCodeIdentifier: String           = "lambda$apply"
    override def packageName: String                 = "std.core"
    override def kind: Func.Kind                     = Func.Kind.Default
    override def genericTypeParams: Seq[GenericType] = genericParams
    override def body: Option[BlockGroup]            = None
    override def params: Seq[Parameter]              = Nil
    override def retVal: Option[LocalVar]            = None
    override def sourceFile: Option[String]          = None
    override def genericDeclaration: Option[Func]    = None
    override def declaringDef: Option[CustomTypeDef] = None
    override def attributes: Seq[Attribute]          = Nil
    override def annotations: Seq[Annotation]        = Nil
  }

  test("shared lambda type parameters are not re-registered as local") {
    val g = new TestGenericType

    // Lambda class and its CustomType reference each other; wire via a var.
    var tpeRef: Type = null
    val lambdaClass  = new TestCustomTypeDef(tpeRef)
    val customTpe    = new TestCustomType(lambdaClass, Seq(g))
    tpeRef = customTpe

    // Global generic lambda function sharing the lambda class parameter object.
    val globalFunc = new TestFunc(Seq(g))
    val pkg        = new TestPackage(Seq(lambdaClass), Seq(globalFunc))

    val resolver = new CHIRResolver(pkg, new FakeEnvironment)

    // The shared parameter resolves to the class type variable registered by
    // the lambda class, and the resolver does not assert on re-registration.
    assert(resolver.typeSig(g) == SignatureType.ClassTypeVariable(0))
  }
}
