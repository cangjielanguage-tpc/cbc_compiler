/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.chir.CHIR.BuiltinType.{Float64, Int64}

object CHIRFloatExpHelperGenerator {
  val float64PowerName = "CJ_CORE_CPow"
  val float64PowerInt64Name = "CJ_CORE_FastPowerDoubleInt64"

  def forPackage(pkg: CHIR.Package): Seq[CHIRFuncGenerator] = {
    Seq(
      new CHIRFloatExpHelperGenerator(pkg, float64PowerName, Float64),
      new CHIRFloatExpHelperGenerator(pkg, float64PowerInt64Name, Int64)
    )
  }
}

private class CHIRFloatExpHelperGenerator(pkg: CHIR.Package, helperName: String, exponentType: CHIR.Type)
  extends CHIRFuncGenerator {
  def generateFunc(funcId: Int): Option[CHIR.Func] = {
    Some(new CHIR.Func {
      val tpe: CHIR.FuncType = new CHIR.FuncType {
        def paramTypes: Seq[CHIR.Type] = Seq(Float64, exponentType)
        def paramTypesWithoutReceiver: Seq[CHIR.Type] = paramTypes
        def receiverType: CHIR.Type = shouldNotCallThis(s"receiver type is not expected for $helperName")
        def returnType: CHIR.Type = Float64
        def isC: Boolean = true
        def hasVarArg: Boolean = false
      }

      def id: Long = funcId.toLong
      def identifier: String = helperName
      def srcCodeIdentifier: String = helperName
      def packageName: String = pkg.name
      def kind: CHIR.Func.Kind = CHIR.Func.Kind.Default
      def attributes: Seq[CHIR.Attribute] = Seq(CHIR.Attribute.Foreign, CHIR.Attribute.Imported)
      def declaringDef: Option[CHIR.CustomTypeDef] = None
      def genericDeclaration: Option[CHIR.Func] = None
      def genericTypeParams: Seq[CHIR.GenericType] = Seq.empty
      def body: Option[CHIR.BlockGroup] = None
      def params: Seq[CHIR.Parameter] = Seq.empty
      def retVal: Option[CHIR.LocalVar] = None
      def sourceFile: Option[String] = None
      def annotations: Seq[CHIR.Annotation] = Seq.empty
    })
  }
}
