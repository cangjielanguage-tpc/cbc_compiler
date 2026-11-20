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
 * The intrinsics throwers delegate to the cbc engine helper lib (see
 * [[EngineHelperPackage]]), so this package refers to nothing from std.core
 * and the generated cbc-intrinsics.cbc works both with std.core compiled to
 * AOT and to cbc.
 */
private final class IntrinsicsPackage extends CHIR.Package {
  private class CHIRThrowFuncGenerator(_pkg: CHIR.Package, _name: String, _delegateTo: CHIR.Func) extends CHIRFuncGenerator {
    implicit val pkg: CHIR.Package = _pkg

    def generateFunc(_id: Int): Option[CHIR.Func] = {
      Option(new CHIR.Func {
        def genericDeclaration: Option[CHIR.Func] = None

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
          gen.startBlock(gen.entryBlock)
          gen.local(CHIR.BuiltinType.Nothing, gen.applyStatic(_delegateTo))
          gen.local(CHIR.BuiltinType.Void, gen.exit())
        })

        def params: Seq[CHIR.Parameter] = Seq.empty

        def retVal: Option[CHIR.LocalVar] = shouldNotCallThis("should not be called")

        def annotations: Seq[CHIR.Annotation] = Seq.empty

        def attributes: Seq[CHIR.Attribute] = Seq.empty

        def declaringDef: Option[CHIR.CustomTypeDef] = Option.empty
      })
    }
  }


  private lazy val generatedFuncs: Seq[CHIR.Func] = Seq(
    CHIRThrowFuncGenerator(this, "throwSymbolResolutionError", EngineHelperPackage.throwSymbolResolutionError).generateFunc(1).get,
    CHIRThrowFuncGenerator(this, "throwAbstractMethodCallError", EngineHelperPackage.throwAbstractMethodCallError).generateFunc(2).get,
  )

  private val allFuncs: Seq[CHIR.Func] = generatedFuncs

  def name: String = IntrinsicsPackage.intrinsicsPackageName

  def typeDefs: Iterator[CHIR.CustomTypeDef] = Iterator.empty

  def values: Iterator[CHIR.Value] = generatedFuncs.iterator

  def function(idx: Int): CHIR.Func = generatedFuncs(idx - 1)

  def getFunc(identifier: String): Option[CHIR.Func] =
    allFuncs.find(_.identifier == "@" + identifier)

  def getDef(identifier: String): Option[CHIR.CustomTypeDef] = Option.empty

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
  val declaringDef: Option[CHIR.CustomTypeDef],
  val id: Long = 1L
) extends CHIR.Func {
  def genericDeclaration: Option[CHIR.Func] = None
  def genericTypeParams: Seq[CHIR.GenericType] = Seq.empty
  def body: Option[CHIR.BlockGroup] = None
  def params: Seq[CHIR.Parameter] = Seq.empty
  def retVal: Option[CHIR.LocalVar] = None
  def sourceFile: Option[String] = None
  def annotations: Seq[CHIR.Annotation] = Seq.empty
}

/** Declaration-only fake CHIR package mirroring the cbc engine helper lib
  * (cbcengine-helper.cj).
  *
  * The intrinsics throwers delegate to the helper lib methods, so the generated
  * cbc-intrinsics.cbc refers to this package instead of std.core and therefore
  * works both with std.core compiled to AOT and to cbc. The delegate methods
  * are declared abstract: the real bodies live in the compilation scope, where
  * the helper chir is always appended (see EngineHelperChir), and the engine
  * resolves the delegation calls against them. Runtime images that embed no
  * real helper bodies (hand-written asm images) must load cbcengine-helper.cbc
  * via the launcher --cbc-path option.
  */
object EngineHelperPackage extends CHIR.Package {
  val packageName: String = "cangjie.runtime.cbcengine.helper"

  val sourceName: String = "cbcengine-helper.chir"

  private val funcs: Seq[CHIR.Func] = Seq(
    throwerFunc("throwSymbolResolutionError", "26throwSymbolResolutionError", 1),
    throwerFunc("throwAbstractMethodCallError", "28throwAbstractMethodCallError", 2),
  )

  private def throwerFunc(name: String, mangledName: String, id: Long): CHIR.Func = new FakeFunc(
    identifier = s"@_CN32cangjie.runtime.cbcengine.helper${mangledName}Hv",
    srcCodeIdentifier = name,
    packageName = packageName,
    tpe = new CHIR.FuncType {
      def paramTypes: Seq[CHIR.Type] = Seq.empty
      def paramTypesWithoutReceiver: Seq[CHIR.Type] = Seq.empty
      def receiverType: CHIR.Type = shouldNotCallThis("receiver type is not expected for the engine helper thrower")
      def returnType: CHIR.Type = CHIR.BuiltinType.Nothing
      def isC: Boolean = false
      def hasVarArg: Boolean = false
    },
    kind = CHIR.Func.Kind.Default,
    attributes = Seq(CHIR.Attribute.Public, CHIR.Attribute.Abstract),
    declaringDef = None,
    id = id
  )

  def throwSymbolResolutionError: CHIR.Func = funcs(0)

  def throwAbstractMethodCallError: CHIR.Func = funcs(1)

  def name: String = packageName

  def typeDefs: Iterator[CHIR.CustomTypeDef] = Iterator.empty

  def values: Iterator[CHIR.Value] = funcs.iterator

  def function(idx: Int): CHIR.Func = funcs(idx - 1)

  def getFunc(identifier: String): Option[CHIR.Func] =
    funcs.find(_.identifier == "@" + identifier)

  def getDef(identifier: String): Option[CHIR.CustomTypeDef] = Option.empty

  def packageInitFunc: CHIR.Func = noInitFunc

  def packageInitLiteralFunc: CHIR.Func = noInitFunc

  private val noInitFunc: CHIR.Func = new FakeFunc(
    identifier = "",
    srcCodeIdentifier = "",
    packageName = packageName,
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
