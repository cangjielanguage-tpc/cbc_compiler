/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.serialization

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.{ClinitAnalysis, FieldsTypeAnalysis, GlobalInitFieldsAnalysis}
import com.huawei.excelsior.jet.compiler.types.References._
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo._
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo
import com.huawei.excelsior.jet.compiler.symlevel.{Field, Method}


/** Store and load extra info in special files.
  * May cache values for future access.
  */
trait OptExtraInfo extends ExtraInfo
  with FieldsTypeAnalysis with ClinitAnalysis with GlobalInitFieldsAnalysis { self: Universe =>

  override protected def globalInfoUpdated(): Unit = invalidateGlobalDependentNodeTypes()

  /** Returns result of local analysis of given method.
    * Perform on-demand compilation if method is not compiled yet.
    */
  def locallyAnalyzeMethod(method: Method): Option[MethodExtraInfoLocal] = {
    val mustBeAnalyzed = passFront(method)
    val info = loadMethodLocalAnalysisResults(method)
    assert(!mustBeAnalyzed || info.isDefined)
    info
  }

  /** Returns result of global analysis of given method.
    * Perform on-demand compilation if method is not compiled yet and we are in middle or back stage
    * of rootMethod compilation.
    */
  def globallyAnalyzeMethod(method: Method): Option[MethodExtraInfoGlobal] = {
    val mustBeAnalyzed = (currentPhase > CompilerPhase.Serialization) && passFront(method)
    val info = loadMethodGlobalAnalysisResults(method)
    assert(!mustBeAnalyzed || info.isDefined)
    info
  }

  private def notInClinitOfField(field: Field) =
    !(rootMethod.isClinit && ((rootDeclaringClass == field.getDeclaringClass) ensuring (!codeUnit.isVersionedMethod)))

  /** Analyze safe field information initialized during class initialization.
    * Return whether it is obtained using aggressive clinit analysis and some information.
    */
  private def globallyAnalyzeClinitOrGlobalInitFieldsInfo[A](field: Field)(getInfo: FieldExtraInfo => Option[A]): Option[(Boolean, A)] = {
    val host = field.getDeclaringClass
    if (!host.isInstanceOf[RTStruct] && !host.isDeferred && notInClinitOfField(field)) {
      if (currentPhase > CompilerPhase.Serialization) {
        analyzeClassClinitForField(field)
        analyzeGlobalInitForField(field)
      }

      for {
        infos <- classFieldsExtraInfo get host
        info <- infos get field
        v <- getInfo(info)
      } yield (info.isAggressive, v)
    } else {
      None
    }
  }

  /** Analyze array field's length initialized during class initialization.
    * Return whether it is obtained using aggressive clinit analysis and the value.
    */
  def globallyAnalyzeClinitForArrayFieldLength(field: Field): Option[(Boolean, Long)] = {
    assert(!field.getType.isPrimitive)
    globallyAnalyzeClinitOrGlobalInitFieldsInfo(field)(_.arrayLength)
  }

  /** Analyze primitive field's value initialized during class initialization.
    * Return whether it is obtained using aggressive clinit analysis and the value.
    */
  def globallyAnalyzeClinitForPrimitiveFieldValue(field: Field): Option[(Boolean, Number)] = {
    assert(field.getType.isPrimitive)
    globallyAnalyzeClinitOrGlobalInitFieldsInfo(field)(_.constantValue)
  }

  /** Analyze field type approximation (safe or probable).
    * Return whether it is obtained using aggressive clinit analysis and the approximation.
    */
  def globallyAnalyzeFieldType(field: Field): Option[(Boolean, ReferenceApprox)] = {
    assert(!field.getType.isPrimitive)
    val host = field.getDeclaringClass
    if (!host.isInstanceOf[RTStruct] && !host.isDeferred) {
      if (currentPhase > CompilerPhase.Serialization) {
        if (notInClinitOfField(field)) {
          analyzeClassClinitForField(field)
        }
        analyzeGlobalInitForField(field)
        analyzeClassFieldTypesForField(field)
      }
      for {
        infos <- classFieldsExtraInfo get host
        info <- infos get field
        ta <- info.typeApprox
      } yield {
        if (notInClinitOfField(field)) {
          (info.isAggressive, ta)
        } else {
          // we cannot use non-probable type obtained from clinit in clinit, use only probable part:
          (false, formalTypeApproximation(field.getType) withProbableType ta.probableType)
        }
      }

    } else {
      None
    }
  }
}
