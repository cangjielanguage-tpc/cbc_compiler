/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.compiler.chir.CHIRPatchGeneratorFactory.intrinsicsPackageName

/** Fake CHIR package with intrinsics.
 *
 * Everything those intrinsics refer to should be referred to as imported AOT CHIR entities
 */
private final class IntrinsicsPackage extends CHIR.Package {

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

  private lazy val patches: Seq[CHIR.Func] = ThrowPatch.throwPatches.zipWithIndex.map { (patch, idx) =>
    CHIRThrowPatch(this, patch).generatePatch(idx + 1).get
  }

  private val allFuncs: Seq[CHIR.Func] = importedFuncs ++ patches
  private val allDefs: Seq[CHIR.CustomTypeDef] = importedDefs

  def name: String = intrinsicsPackageName

  def typeDefs: Iterator[CHIR.CustomTypeDef] = allDefs.iterator

  def values: Iterator[CHIR.Value] = patches.iterator

  def function(idx: Int): CHIR.Func = patches(idx - 1)

  def getFunc(identifier: String): Option[CHIR.Func] =
    allFuncs.find(_.identifier == "@" + identifier)

  def getDef(identifier: String): Option[CHIR.CustomTypeDef] =
    allDefs.find(_.identifier == "@" + identifier)

  def packageInitFunc: CHIR.Func = IntrinsicsPackage.initFunc

  override def packageInitLiteralFunc: CHIR.Func = IntrinsicsPackage.initFunc
}

private object IntrinsicsPackage {
  private val initFunc: CHIR.Func = new FakeFunc(
    identifier = "",
    srcCodeIdentifier = "",
    packageName = intrinsicsPackageName,
    tpe = new CHIR.FuncType {
      def paramTypes: Seq[CHIR.Type] = Seq.empty
      def paramTypesWithoutReceiver: Seq[CHIR.Type] = Seq.empty
      def receiverType: CHIR.Type = CodeHelpers.shouldNotCallThis("no init function")
      def returnType: CHIR.Type = CHIR.BuiltinType.Void
      def isC: Boolean = false
      def hasVarArg: Boolean = false
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
  override def id: Long = 1L
  override def genericTypeParams: Seq[CHIR.GenericType] = Seq.empty
  override def body: Option[CHIR.BlockGroup] = None
  override def params: Seq[CHIR.Parameter] = Seq.empty
  override def retVal: Option[CHIR.LocalVar] = None
  override def sourceFile: Option[String] = None
  override def annotations: Seq[CHIR.Annotation] = Seq.empty
}

private final class FakeClassDef(
  val packageName: String,
  val identifier: String,
  val srcCodeIdentifier: String,
  val attributes: Seq[CHIR.Attribute],
  override val superClass: Option[CHIR.ClassType],
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