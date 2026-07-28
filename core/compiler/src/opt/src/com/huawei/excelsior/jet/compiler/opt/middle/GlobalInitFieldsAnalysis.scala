/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo.*
import com.huawei.excelsior.jet.compiler.symlevel.{Field, ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.References.{ReferenceApprox, RefEmpty}
import com.huawei.excelsior.jet.compiler.util.Log
import com.huawei.excelsior.jet.util.ScalaCollections.*

import scala.PartialFunction.condOpt
import scala.collection.mutable

trait GlobalInitFieldsAnalysis extends FieldsAnalysis { self: Universe =>
  import GlobalInitFieldsAnalysis.*

  private def log = Log(Log.Kind.GlobalInitFields)

  private def enabled = env.enabled(BoolOption.GlobalInitFieldsAnalysis)

  private def analyzableClass(host: SymType): Boolean = {
    host.isCangjieType && host.isCangjiePackage && host.isInCurrentCompilationSet
  }

  private def analyzableField(field: Field): Boolean = {
    field.isStatic && field.isFinal
  }

  private def fieldsToAnalyze(host: SymClassType) = host.getDeclaredFields filter analyzableField

  final def analyzeGlobalInitForField(field: Field): Unit = {
    if (enabled && analyzableClass(field.getDeclaringClass) && analyzableField(field)) {
      analyzeGlobalInits(field.getDeclaringClass)
    }
  }

  final def resetGlobalInitFieldsAnalysis(): Unit = GlobalInitFieldsAnalysis.clearInfo()

  final def analyzeCurrentClassGlobalInitFields(): Unit = {
    val host = rootDeclaringClass ensuring (!codeUnit.isVersionedMethod)
    if (enabled && analyzableClass(host)) {
      analyzeGlobalInits(host)
    }
  }

  private def analyzeGlobalInits(host: SymClassType): Unit = {
    getInfo(host) match {
      case None | Some(ClassInView(_)) => analyzeAllGlobalInitMethods(host)
      case Some(ClassVisited) => // everything was already analyzed and serialized or there is nothing to analyze
    }
  }

  /** Provokes compilation of all methods of `host`
    * and stores all calculated types as widened approximations in fields extra info.
    */
  private def analyzeAllGlobalInitMethods(host: SymClassType): Unit = {
    val methods = host.getDeclaredMethods filter (_.isGlobalInit)
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
      assert(!method.isNative)
      if (!passFront(method)) {
        // We cannot compile it right now, but continue fields analysis and try compile it later.
        // However some "class in view" may leak if some of their methods will never be compiled successfully.
        return
      }
    }

    // Local part of analysis was done for all methods of the class.

    getInfo(host) match {
      case None => // we have fields, we have methods, but no stores. ¯\_(ツ)_/¯
        putInfo(host, ClassVisited)

      case Some(ClassInView(fieldInfos)) =>
        saveFieldInfosToExtraInfo(host, fieldInfos)
        putInfo(host, ClassVisited)

      case Some(ClassVisited) => // already done
    }
  }

  private def saveFieldInfosToExtraInfo(host: SymClassType, fieldInfos: mutable.HashMap[Field, FieldInfo]): Unit = {
    // previous results from FieldsTypeAnalysis
    val fieldsWithProbableType = classFieldsExtraInfo get host getOrElse Map.empty

    var fieldsWithSafeInfo = fieldsWithProbableType

    for ((field, info) <- fieldInfos) {
      val (typeAppr, arrayLength, constantValue) = info match {
        case ReferenceFieldInfo(t, l) => (t, l, None)
        case PrimitiveFieldInfo(v) => (None, None, Some(v))
      }
      assert(arrayLength.isDefined || typeAppr.isDefined || constantValue.isDefined)

      // Check that type calculated by this analysis is always better than previously calculated probable type
      fieldsWithProbableType get field match {
        case Some(FieldExtraInfo(false, None, Some(widenedType), None)) =>
          typeAppr match {
            case Some(calculatedType) =>
              verifyTypeOfSafeAnalysisIsBetterThanFieldsTypeAnalysis(calculatedType, widenedType)
            case None =>
              shouldNotReachHere("global_init analysis must also be able to analyze this field")
          }

        case Some(x) =>
          shouldNotReachHere(s"unexpected state of field extra info before global_init analysis: $x")

        case None =>
      }

      fieldsWithSafeInfo += (field ->
        FieldExtraInfo(isAggressive = false, arrayLength, typeAppr, constantValue))
    }

    if (fieldsWithSafeInfo != fieldsWithProbableType) {
      classFieldsExtraInfo.put(host, fieldsWithSafeInfo)
    }
  }

  final def analyzeLocalFieldsStoresInGlobalInit(): Unit = {
    if (!enabled || !analyzableClass(rootDeclaringClass) || !rootMethod.isGlobalInit) {
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

    val fieldPuts = groupBy(all[PutStatic] filter (fields contains _.field))(_.field)

    if (fieldPuts.isEmpty) {
      return
    }

    val fieldInfos = state match {
      case Some(ClassInView(fieldInfos)) => fieldInfos
      case None =>
        val x = new mutable.HashMap[Field, FieldInfo]
        putInfo(host, ClassInView(x))
        x
      case _ => shouldNotReachHere(state)
    }

    log.inSession(s"global_init analysis for $rootMethod") {

      for ((field, puts) <- fieldPuts) {
        assert(puts.size == 1, s"more than one put into immutable field $field")
        val singlePut = puts.head
        val value = singlePut.storedValue()

        val info = if (field.getType.isPrimitive) {
          value match {
            case NumericalConst(c) =>
              log(s"ok: '${field.getName}' value = $c")
              Some(PrimitiveFieldInfo(c))
            case _ =>
              log(s"fail: '${field.getName}' assigned non-constant value")
              None
          }

        } else {
          nodeTypeAt(value, singlePut) match {
            case RefEmpty =>
              // For now we just ignore such fields.
              log(s"fail: unreachable '${field.getName}' initialization")
              None

            case fieldType =>
              val isRefinedType = (fieldType compareWidened formalTypeApproximation(field.getType)) == CC.Less
              val fieldTypeOpt = if (isRefinedType) {
                log(s"ok: '${field.getName}' has type $fieldType")
                // Aggressiveness affects presence of explicit check (see AggressiveClinitAnalysisAssert).
                // Nullable fields are guarded by NullCheck, no need for aggressive check.
                Some(fieldType)
              } else {
                log(s"fail: '${field.getName}' type is not refined")
                None
              }

              def arrayLengthConstStat(lengthOpt: Option[Long]): Option[Long] = {
                lengthOpt match {
                  case Some(length) =>
                    log(s"ok: '${field.getName}' length = $length")
                  case _ =>
                    log(s"fail: '${field.getName}' length is non-constant")
                }
                lengthOpt
              }

              def arrayLengthStat(lengthNode: Node): Option[Long] = arrayLengthConstStat(condOpt(lengthNode) {
                case IntegralConst(len) => len
              })

              val arrayLengthOpt = value match {
                case anyNewArray: AnyNewArray => arrayLengthStat(anyNewArray.lengths.head)
                case newArrayCopy: NewArrayCopy => arrayLengthStat(newArrayCopy.length)
                case newArrayRT: NewArrayRT => arrayLengthStat(newArrayRT.length)
                case arrayCopyOf: NewArrayCopyRT => arrayLengthConstStat(condOpt(arrayCopyOf.from, arrayCopyOf.to) {
                  case (IConst(from), IConst(to)) => to - from
                })
                case _ =>
                  if (field.getType.isArray) {
                    log(s"fail: '${field.getName}' value is not new[]")
                  }
                  None
              }

              if (fieldTypeOpt.isDefined || arrayLengthOpt.isDefined) {
                Some(ReferenceFieldInfo(fieldTypeOpt, arrayLengthOpt))
              } else {
                None
              }
          }
        }

        assert(!(fieldInfos contains field), s"more than one put into immutable field $field")
        for (i <- info) {
          fieldInfos(field) = i
        }
      }
    }
  }

}


private object GlobalInitFieldsAnalysis {

  sealed abstract class FieldInfo
  case class ReferenceFieldInfo(typeInfo: Option[ReferenceApprox], arrayLength: Option[Long]) extends FieldInfo
  case class PrimitiveFieldInfo(value: Number) extends FieldInfo

  sealed abstract class ClassVisitInfo

  object ClassVisited extends ClassVisitInfo

  case class ClassInView(fieldsInfos: mutable.HashMap[Field, FieldInfo]) extends ClassVisitInfo

  def getInfo(klass: SymType): Option[ClassVisitInfo] =
    infos.get(klass.getUniqueNumber)

  def putInfo(klass: SymType, info: ClassVisitInfo): Unit =
    infos.put(klass.getUniqueNumber, info)

  def clearInfo(): Unit =
    infos.clear()

  // We do not use Type as key to prevent holding of heavy Type instances.
  private[this] val infos = new mutable.HashMap[Int, ClassVisitInfo]
}
