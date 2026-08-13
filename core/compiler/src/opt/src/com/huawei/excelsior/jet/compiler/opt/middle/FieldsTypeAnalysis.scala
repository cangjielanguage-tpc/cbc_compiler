/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.options.BoolOption.{FieldsTypeAnalysisForAllFields, NoFieldsTypeAnalysis}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.References.{ReferenceApprox, TypeOnEdge}
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{CangjieArray, JavaArray, TypeVariable}
import com.huawei.excelsior.jet.util.ScalaCollections.*
import com.huawei.excelsior.jet.compiler.symlevel.{Field, MethodAJCallKind, ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.compiler.util.Log

import scala.collection.mutable

/** Fields type analysis scans all stores to private & final fields of each class
  * and saves combined type of values as widened type of these fields.
  *
  * @author ikireev
  */
trait FieldsTypeAnalysis extends FieldsAnalysis { self: Universe =>
  import FieldsTypeAnalysis.*

  private def log = Log(Log.Kind.FieldsTypeAnalysis)

  private def enabled = !env.enabled(NoFieldsTypeAnalysis)
  private def analyzeAllFields = env.enabled(FieldsTypeAnalysisForAllFields)

  private def fieldWithAnalyzableType(field: Field): Boolean =
    (field.isPrivate || field.isFinal || analyzeAllFields) && {
      val tpe = field.getType
      val baseType = tpe match {
        case tpe: JavaArray => tpe.baseType
        case tpe: CangjieArray => tpe.elemType
        case tpe => tpe
      }
      !baseType.isTypeVariable && !baseType.isDeferred && !baseType.isPrimitive && !baseType.symType.isFinal
    }

  private def fieldsToAnalyze(host: SymClassType) =
    host.getDeclaredFields filter fieldWithAnalyzableType

  final def analyzeClassFieldTypesForField(field: Field): Unit = {
    if (enabled && fieldWithAnalyzableType(field)) {
      analyzeClassFieldTypes(field.getDeclaringClass)
    }
  }

  final def resetFieldsTypeAnalysis(): Unit = FieldsTypeAnalysis.clearInfo()

  final def analyzeCurrentClassFieldTypes(): Unit = {
    val host = rootDeclaringClass ensuring (!codeUnit.isVersionedMethod)
    analyzeClassFieldTypes(host)
  }

  private def analyzeClassFieldTypes(host: SymClassType): Unit = {
    if (enabled && host.isInCurrentCompilationSet) {
      getInfo(host) match {
        case None | Some(ClassInView(_)) => analyzeClassMethods(host)
        case Some(ClassVisited) => // everything was already analyzed and serialized or there is nothing to analyze
      }
    }
  }

  /** Provokes compilation of all methods of `host`
    * and stores all calculated types as widened approximations in fields extra info.
    */
  private def analyzeClassMethods(host: SymClassType): Unit = {
    val methods = host.getDeclaredMethods filterNot (_.isAbstract)
    if (methods.isEmpty) {
      putInfo(host, ClassVisited)
      return
    }

    val fields = fieldsToAnalyze(host)
    if (fields.isEmpty) {
      putInfo(host, ClassVisited)
      return
    }

    assert(currentPhase > CompilerPhase.Serialization)
    for (method <- methods) {
      if (method.isNative || method.isNoCodeGen) {
        if (method.getAJCallKind == MethodAJCallKind.NORMAL) {
          putInfo(host, ClassVisited)
          return
        } // else ignore such methods
      } else if (locallyAnalyzeMethod(method).isEmpty) {
        // We cannot compile it right now, but continue fields analysis and try to compile it later.
        // However, some "class in view" may leak if some of their methods will never be compiled successfully.
        return
      }
    }

    // Local part of analysis was done for all methods of the class.

    getInfo(host) match {
      case None => // we have fields, we have methods, but no stores. ¯\_(ツ)_/¯
        putInfo(host, ClassVisited)

      case Some(ClassInView(fieldsTypes)) =>
        saveFieldTypesToExtraInfo(host, fieldsTypes)
        putInfo(host, ClassVisited)

      case Some(ClassVisited) => // already done
    }
  }

  private def saveFieldTypesToExtraInfo(host: SymClassType, fieldsTypes: mutable.HashMap[Field, ReferenceApprox]): Unit = {
    log.inSession("Fields type analysis for: " + host.getName) {
      // previous results from ClinitAnalysis
      val fieldsWithSafeType = classFieldsExtraInfo get host getOrElse Map.empty

      var fieldsWithProbableType = fieldsWithSafeType

      for ((field, calculatedType) <- fieldsTypes.toSeq.filter(_._2 != null).sortBy(_._1.getName)) {
        val formalTypeEdge = TypeOnEdge(formalTypeApproximation(field.getType), isColdEdge = true)
        val calculatedTypeEdge = TypeOnEdge(calculatedType, isColdEdge = false)
        val refinedType = (formalTypeEdge union calculatedTypeEdge).tpe

        fieldsWithSafeType get field match {
          case Some(FieldExtraInfo(_, _, Some(safeType), None)) =>
            // In some rare cases refinedType may be quite conservative.
            verifyTypeOfSafeAnalysisIsBetterThanFieldsTypeAnalysis(safeType, refinedType)

          case Some(x) =>
            shouldNotReachHere(s"unexpected state of field extra info before fields type analysis: $x")

          case None =>
            log(s"Field ${field.getFullName} with formal type ${field.getType.symType.getName} is refined to $refinedType")
            fieldsWithProbableType += (field -> FieldExtraInfo(false, None, Some(refinedType), None))
        }
      }

      if (fieldsWithProbableType != fieldsWithSafeType) {
        classFieldsExtraInfo.put(host, fieldsWithProbableType)
      }
    }
  }

  final def analyzeLocalFieldsTypeStores(): Unit = {
    if (!enabled) {
      return
    }

    val host = rootDeclaringClass ensuring (!codeUnit.isVersionedMethod)
    val state = getInfo(host)
    if (state contains ClassVisited) {
      // some non-compiled method could stop all methods iteration
      return
    }

    val fields = fieldsToAnalyze(host).toSet
    if (fields.isEmpty) {
      // do not waste time for this class anymore
      assert(state.isEmpty)
      putInfo(host, ClassVisited)
      return
    }

    val fieldsAndInTypes = all[PutJavaFieldOperation].
      filter(fields contains _.field).
      map(p => (p.field, nodeTypeAt(p.storedValue(), p)))

    if (fieldsAndInTypes.isEmpty) {
      return
    }

    val fieldsTypes = state match {
      case Some(ClassInView(typesMap)) => typesMap
      case None =>
        val x = new mutable.HashMap[Field, ReferenceApprox]
        putInfo(host, ClassInView(x))
        x
      case _ => shouldNotReachHere(state)
    }

    for ((field, inTypes) <- toMultiMap(fieldsAndInTypes)) {
      updateFieldType(fieldsTypes, field, inTypes reduce ReferenceApprox.union)
    }
  }

  private def updateFieldType(fieldsTypes: mutable.HashMap[Field, ReferenceApprox], field: Field, localType: ReferenceApprox): Unit = {
    // If calculated type is not good enough we store `null` in fieldsTypes as a marker

    val calculatedType = fieldsTypes get field match {
      case None => localType
      case Some(null) => return
      case Some(cached) => ReferenceApprox.union(cached, localType)
    }

    // Note that if calculatedType is equal to formalType.withoutNull, we ignore such "refinement" because it is useless.
    val isRefined = (calculatedType.withNull compareWidened formalTypeApproximation(field.getType)) == CC.Less
    fieldsTypes.put(field, if (isRefined) calculatedType else null)
  }
}

private object FieldsTypeAnalysis {

  sealed abstract class ClassVisitInfo

  object ClassVisited extends ClassVisitInfo

  case class ClassInView(fieldsTypes: mutable.HashMap[Field, ReferenceApprox]) extends ClassVisitInfo

  def getInfo(klass: SymType): Option[ClassVisitInfo] =
    infos.get(klass.getUniqueNumber)

  def putInfo(klass: SymType, info: ClassVisitInfo): Unit =
    infos.put(klass.getUniqueNumber, info)

  def clearInfo(): Unit =
    infos.clear()

  // We do not use Type as key to prevent holding of heavy Type instances.
  private[this] val infos = new mutable.HashMap[Int, ClassVisitInfo]
}
