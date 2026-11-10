/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.chir.dsl.CHIRDSL

/** Fake CHIR package with intrinsics.
 *
 * Everything those intrinsics refer to should be referred to as imported AOT CHIR entities
 */
private final class IntrinsicsPackage extends CHIR.Package {
  private class CHIRThrowFuncGenerator(_pkg: CHIR.Package, _name: String, _exceptionMsg: String) extends CHIRFuncGenerator {
    implicit val pkg: CHIR.Package = _pkg

    def generateFunc(_id: Int): Option[CHIR.Func] = {
      Option(new CHIR.Func {
        def tpe: CHIR.FuncType = new CHIR.FuncType {
          def paramTypes: Seq[CHIR.Type] = Seq.empty

          def paramTypesWithoutReceiver: Seq[CHIR.Type] = Seq.empty

          def receiverType: CHIR.Type = shouldNotCallThis(s"receiver type is not expected for internal throw helper")

          def returnType: CHIR.Type = CHIR.BuiltinType.Nothing

          def isC: Boolean = false

          def hasVarArg: Boolean = false
        }

        def id: Long = _id

        def identifier: String = _name

        def srcCodeIdentifier: String = _name

        def packageName: String = IntrinsicsPackage.intrinsicsPackageName

        def kind: CHIR.Func.Kind = CHIR.Func.Kind.Default

        def genericTypeParams: Seq[CHIR.GenericType] = Seq.empty

        def sourceFile: Option[String] = Option.empty

        def body: Option[CHIR.BlockGroup] = Option(CHIRDSL.genBlockGroup(_pkg) { gen =>
          val exceptionType: CHIR.Type = AOTDefs.Exception
          val exceptionInitFunc: CHIR.Func = AOTDefs.initException
          gen.startBlock(gen.entryBlock)
          val exception = gen.local(exceptionType, gen.alloc(exceptionType))

          val exceptionMsg = gen.local(AOTDefs.String, gen.const(AOTDefs.String, _exceptionMsg))

          gen.apply(exceptionInitFunc, Some(exceptionType), Some(exception), exception, exceptionMsg)

          gen.local(CHIR.BuiltinType.Void, gen.apply(exceptionInitFunc, Some(exceptionType), Some(exception), exception, exceptionMsg))

          gen.local(CHIR.BuiltinType.Nothing, gen.raise(exception, Option.empty))
        })

        def params: Seq[CHIR.Parameter] = Seq.empty

        def retVal: Option[CHIR.LocalVar] = shouldNotCallThis("should not be called")

        def annotations: Seq[CHIR.Annotation] = Seq.empty

        def attributes: Seq[CHIR.Attribute] = Seq.empty

        def declaringDef: Option[CHIR.CustomTypeDef] = Option.empty
      })
    }
  }


  private val objectDef: CHIR.ClassDef = new FakeClassDef(
    packageName = "std.core",
    identifier = "@_CNat6ObjectE",
    srcCodeIdentifier = "Object",
    attributes = Seq(CHIR.Attribute.Public, CHIR.Attribute.Virtual, CHIR.Attribute.Imported),
    superClass = None,
    methods0 = Seq.empty
  )

  private val stringDef: CHIR.StructDef = new FakeStructDef(
    packageName = "std.core",
    identifier = "@_CNat6StringE",
    srcCodeIdentifier = "String",
    attributes = Seq(CHIR.Attribute.Public, CHIR.Attribute.Imported),
    instanceVars = Seq.empty
  )

  private lazy val exceptionDef: CHIR.ClassDef = new FakeClassDef(
    packageName = "std.core",
    identifier = "@_CNat9ExceptionE",
    srcCodeIdentifier = "Exception",
    attributes = Seq(CHIR.Attribute.Public, CHIR.Attribute.Virtual, CHIR.Attribute.Imported),
    superClass = Some(objectDef.tpe),
    methods0 = Seq(exceptionInitFunc)
  )

  private lazy val exceptionInitFunc: CHIR.Func = new FakeFunc(
    identifier = "@_CNat9Exception6<init>HRNat6StringE",
    srcCodeIdentifier = "init",
    packageName = "std.core",
    tpe = new CHIR.FuncType {
      def receiverType: CHIR.Type = FakeRefType(exceptionDef.tpe)
      def paramTypes: Seq[CHIR.Type] = Seq(receiverType, stringDef.tpe)
      def paramTypesWithoutReceiver: Seq[CHIR.Type] = Seq(stringDef.tpe)
      def returnType: CHIR.Type = CHIR.BuiltinType.Void
      def isC: Boolean = false
      def hasVarArg: Boolean = false
    },
    kind = CHIR.Func.Kind.ClassCtor,
    attributes = Seq(CHIR.Attribute.Public, CHIR.Attribute.Imported),
    declaringDef = Some(exceptionDef)
  )

  private val importedDefs: Seq[CHIR.CustomTypeDef] = Seq(exceptionDef, stringDef, objectDef)
  private val importedFuncs: Seq[CHIR.Func] = Seq(exceptionInitFunc)

  private lazy val generatedFuncs: Seq[CHIR.Func] = Seq(
    CHIRThrowFuncGenerator(this, "throwSymbolResolutionError", "CBC internal error: symbol resolution error").generateFunc(1).get,
    CHIRThrowFuncGenerator(this, "throwAbstractMethodCallError", "CBC internal error: call of an abstract method").generateFunc(2).get,
  )

  private val allFuncs: Seq[CHIR.Func] = importedFuncs ++ generatedFuncs
  private val allDefs: Seq[CHIR.CustomTypeDef] = importedDefs

  def name: String = IntrinsicsPackage.intrinsicsPackageName

  def typeDefs: Iterator[CHIR.CustomTypeDef] = allDefs.iterator

  def values: Iterator[CHIR.Value] = generatedFuncs.iterator

  def function(idx: Int): CHIR.Func = generatedFuncs(idx - 1)

  def getFunc(identifier: String): Option[CHIR.Func] =
    allFuncs.find(_.identifier == "@" + identifier)

  def getDef(identifier: String): Option[CHIR.CustomTypeDef] =
    allDefs.find(_.identifier == "@" + identifier)

  def packageInitFunc: CHIR.Func = IntrinsicsPackage.initFunc

  def packageInitLiteralFunc: CHIR.Func = IntrinsicsPackage.initFunc
}

object IntrinsicsPackage {
  val intrinsicsPackageName = "cbc-intrinsics"

  val intrinsicsSourceName = intrinsicsPackageName + ".chir"

  private val initFunc: CHIR.Func = new FakeFunc(
    identifier = "",
    srcCodeIdentifier = "",
    packageName = intrinsicsPackageName,
    tpe = new CHIR.FuncType {
      def paramTypes: Seq[CHIR.Type] = shouldNotCallThis("no init function")
      def paramTypesWithoutReceiver: Seq[CHIR.Type] = shouldNotCallThis("no init function")
      def receiverType: CHIR.Type = shouldNotCallThis("no init function")
      def returnType: CHIR.Type = shouldNotCallThis("no init function")
      def isC: Boolean = shouldNotCallThis("no init function")
      def hasVarArg: Boolean = shouldNotCallThis("no init function")
    },
    kind = CHIR.Func.Kind.Default,
    attributes = Seq.empty,
    declaringDef = None
  )
}

private final class FakeFunc(
  val identifier: String,
  val srcCodeIdentifier: String,
  val packageName: String,
  val tpe: CHIR.FuncType,
  val kind: CHIR.Func.Kind,
  val attributes: Seq[CHIR.Attribute],
  val declaringDef: Option[CHIR.CustomTypeDef]
) extends CHIR.Func {
  def id: Long = 1L
  def genericTypeParams: Seq[CHIR.GenericType] = Seq.empty
  def body: Option[CHIR.BlockGroup] = None
  def params: Seq[CHIR.Parameter] = Seq.empty
  def retVal: Option[CHIR.LocalVar] = None
  def sourceFile: Option[String] = None
  def annotations: Seq[CHIR.Annotation] = Seq.empty
}

private final class FakeClassDef(
  val packageName: String,
  val identifier: String,
  val srcCodeIdentifier: String,
  val attributes: Seq[CHIR.Attribute],
  val superClass: Option[CHIR.ClassType],
  methods0: => Seq[CHIR.Func]
) extends CHIR.ClassDef {
  lazy val tpe: CHIR.ClassType = new CHIR.ClassType {
    def typeDef: CHIR.ClassDef = FakeClassDef.this
    def genericTypeParams: Seq[CHIR.Type] = Seq.empty
  }
  def isClass: Boolean = true
  def methods: Seq[CHIR.Func] = methods0
  def instanceVars: Seq[CHIR.InstanceVar] = Seq.empty
  def staticVars: Seq[CHIR.GlobalVar] = Seq.empty
  def vtables: Seq[CHIR.VTable] = Seq.empty
  def implementedInterfaces: Seq[CHIR.ClassType] = Seq.empty
  def annotations: Seq[CHIR.Annotation] = Seq.empty
}

private final class FakeStructDef(
  val packageName: String,
  val identifier: String,
  val srcCodeIdentifier: String,
  val attributes: Seq[CHIR.Attribute],
  val instanceVars: Seq[CHIR.InstanceVar]
) extends CHIR.StructDef {
  lazy val tpe: CHIR.StructType = new CHIR.StructType {
    def typeDef: CHIR.StructDef = FakeStructDef.this
    def genericTypeParams: Seq[CHIR.Type] = Seq.empty
  }
  def staticVars: Seq[CHIR.GlobalVar] = Seq.empty
  def methods: Seq[CHIR.Func] = Seq.empty
  def vtables: Seq[CHIR.VTable] = Seq.empty
  def implementedInterfaces: Seq[CHIR.ClassType] = Seq.empty
  def annotations: Seq[CHIR.Annotation] = Seq.empty
}

private final case class FakeRefType(baseType: CHIR.Type) extends CHIR.RefType