/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.StatsKind.ClinitAnalysis
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool.Access
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPoolAccessResult, Bytecode, BytecodeIterator, FieldAccessKind}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.References.{ReferenceApprox, RefEmpty}
import com.huawei.excelsior.jet.compiler.opt.serialization.OptExtraInfo
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo.*
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.symlevel.Field
import com.huawei.excelsior.jet.compiler.util.{Log, Maps}
import com.huawei.excelsior.jet.util.ScalaCollections.groupBy

import scala.PartialFunction.condOpt
import scala.annotation.nowarn

/**
  * Clinit analysis phase tries to calculate some properties about class fields based on initializer method.
  *
  * @author conwor
  */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait ClinitAnalysis extends ClinitAnalysisConfig { self: Universe with OptExtraInfo =>
  private case class TypeInfo(isAggressive: Boolean, typeAppr: ReferenceApprox)

  private sealed abstract class FieldInfo
  private case class ReferenceFieldInfo(typeInfo: Option[TypeInfo], arrayLength: Option[Long]) extends FieldInfo
  private case class PrimitiveFieldInfo(isAggressive: Boolean, value: Number) extends FieldInfo

  private def log = Log(Log.Kind.ClinitAnalysis)

  private def writeStats(statsMsg: String, logMsg: String = null): Unit = {
    stats.count(ClinitAnalysis, statsMsg)
    log(if (logMsg != null) logMsg else statsMsg)
  }

  private sealed abstract class Cleanness
  private case object Clean extends Cleanness
  private abstract class NonClean extends Cleanness {
    def dirtyBorder: ControlNode
  }
  private case class AggressivelyClean(dirtyBorder: ControlNode) extends NonClean
  private case class Dirty(dirtyBorder: ControlNode) extends NonClean

  def analyzeClassClinitForField(field: Field): Unit = {
    if (env.enabled(NoClinitAnalysis) || !isFieldAnalyzableByClinitAnalysis(field)) {
      return
    }

    val clinit = field.getDeclaringClass.getClinit
    if (clinit == null) {
      return
    }

    assert(currentPhase > CompilerPhase.Serialization)
    passFront(clinit)
  }

  /** Perform clinit analysis if method is clinit of some class.
    * Returns whether this clinit method is clean (or aggresively assumed to be clean).
    */
  def analyzeClinit(): Boolean = {
    if (env.enabled(NoClinitAnalysis) || !rootMethod.isClinit || rootDeclaringClass.isCangjieJavaHelper) {
      // This value is ignored for non-clinit methods.
      return false
    }

    // TODO: combine with fields type analysis and get rid of manual bytecode parsing

    assert(!rootDeclaringClass.isCangjieType)
    assert(!codeUnit.isVersionedMethod)
    val host = rootDeclaringClass

    log.inSession(s"Clinit analysis for: ${host.getName}") {

      // Clinit analysis collects types and values of static final fields
      // which are assigned once in clinit (and not reassigned in other methods).
      // Reflection, JNI and Unsafe are ignored because specification allows it.
      //
      // One of the problems with clinit analysis is that in Java
      // it is possible to observe value of static final field before its initialization
      // (i.e. read field during call to method of another class).
      //
      // We call clinit code "clean" if it is guaranteed that no arbitrary code
      // would be executed before execution of this code.
      //
      // Note that throwing exception out of "clean" code does not make it "dirty".
      // In such case all reads of this class fields would be unreachable and may be safely optimized.
      //
      // The simplest example of "dirty" code is Invoke.
      //
      // In case of "dirty" code we do not analyze primitive fields because their value is actually uncertain.
      // However we could analyze types and other properties of reference fields treating them as nullable
      // (because uninitialized reference field is equal to null).
      // Analysis results would influence generated code with nullcheck as a guard
      // which could be treated as "initialized check".
      //
      // Clinit could be totally "clean" or have single "dirty border" which dominates normal exit.
      // In later case all code dominated by "dirty border" (after border) is treated like "dirty"
      // and all other code (before border) is treated like "clean".
      //
      // Sometimes code is "dirty" but we somehow know that all fields are initialized before any read.
      // Such "dirty" parts are "aggressively" analyzed assuming that they are "clean".
      // All uses of such analysis results are guarded by checks in work mode.
      // During such analysis we do not analyze nullable reference fields
      // because it's hard to check that such field was actually initialized
      // (i.e. we assume that non-null means "initialized").

      if (Return.unique.isEmpty) {
        writeStats("fail: clinit has no return")
        return false
      }

      val cleanness = analyzeCleanness(host)
      val result = cleanness == Clean

      val fields = collectFieldsToAnalyse(host)
      if (fields.isEmpty) {
        // There are no interesting fields in this class
        writeStats("fail: host class has no fields to analyze")
        return result
      }

      filterRedefinedFields(host, fields)
      if (fields.isEmpty) {
        writeStats("fail: all fields were redefined")
        return result
      }

      analyzePutFields(fields, cleanness)
      if (fields.isEmpty) {
        writeStats("fail: host class has no fields with determined initialization information")
        return result
      }

      // previous results from FieldsTypeAnalysis
      val fieldsWithProbableType = classFieldsExtraInfo get host getOrElse Map.empty

      var fieldsWithSafeInfo = fieldsWithProbableType

      for ((field, info) <- fields) {
        val (isAggressive, typeAppr, arrayLength, constantValue) = info match {
          case ReferenceFieldInfo(ti, l) => ti match {
            case Some(TypeInfo(a, t)) => (a, Some(t), l, None)
            case None => (false, None, l, None)
          }
          case PrimitiveFieldInfo(a, v) => (a, None, None, Some(v))
        }
        assert(arrayLength.isDefined || typeAppr.isDefined || constantValue.isDefined)

        // Check that type calculated by this analysis is always better than previously calculated probable type
        fieldsWithProbableType get field match {
          case Some(FieldExtraInfo(false, None, Some(widenedType), None)) =>
            typeAppr match {
              case Some(calculatedType) =>
                verifyTypeOfSafeAnalysisIsBetterThanFieldsTypeAnalysis(calculatedType, widenedType)
              case None =>
                shouldNotReachHere("clinit analysis must also be able to analyze this field")
            }

          case Some(x) =>
            shouldNotReachHere(s"unexpected state of field extra info before clinit analysis: $x")

          case None =>
        }

        fieldsWithSafeInfo += (field ->
          FieldExtraInfo(isAggressive, arrayLength, typeAppr, constantValue))
      }

      if (fieldsWithSafeInfo != fieldsWithProbableType) {
        classFieldsExtraInfo.put(host, fieldsWithSafeInfo)
      }

      result
    }
  }

  private def analyzeCleanness(host: symlevel.ClassType): Cleanness = {
    def dirtyOrAggressivelyClean(border: ControlNode, statsReason: String, logReason: String): Cleanness = {
      if (isAggressiveClinitAnalysisAllowedFor(host)) {
        writeStats(
          s"notice: clean clinit, aggressively assumed, ignoring $statsReason",
          s"notice: clean clinit, aggressively assumed, ignoring $logReason")
        AggressivelyClean(border)

      } else {
        writeStats(
          s"notice: dirty clinit, $statsReason",
          s"notice: dirty clinit, $logReason")
        Dirty(border)
      }
    }

    for (superType <- host.getDeclaredSuperTypes) {
      val superTypeClinit = superType.getClinit
      if (superTypeClinit != null) {
        globallyAnalyzeMethod(superTypeClinit) match {
          case Some(info) =>
            if (info.isCleanClinit) {
              // ok
            } else {
              return dirtyOrAggressivelyClean(entryBlock,
                s"supertype has non-clean clinit",
                s"'${superType.getName}' has non-clean clinit")
            }

          case _ =>
            return dirtyOrAggressivelyClean(entryBlock,
              s"supertype clinit was not analysed",
              s"'${superType.getName}' clinit was not analysed")
        }
      }
    }

    // Find such node which is the lowest border between clean part and dirty part:
    //
    //  entryBlock
    //      /\
    //   arbitrary
    //    hammock
    //    (clean)
    //      \/
    //  dirtyBorder
    //      /\
    //   arbitrary
    //    hammock
    //    (dirty)
    //      \/
    //    return
    //
    // This border is quite conservative. Especially when there are big diamonds with dirty nodes at the end of only branch.
    // However it fits well because clinit analysis currently fails to analyze multiple assignments.
    // Moreover on practice clinits are usually linear.
    //
    // Note that only spinal nodes could be dirty. (XPoint is non-spinal and safe.)
    val dirtyNodes = all[SpinalNode] filter (!isClean(_, host))
    if (dirtyNodes.nonEmpty) {
      val topMostDirtyNode = dirtyNodes.reduce[ControlNode](_ nearestDom _)
      val dirtyBorder = topMostDirtyNode nearestDom Return.unique.get
      dirtyOrAggressivelyClean(dirtyBorder,
        "not allowed nodes",
        "not allowed nodes")
    } else {
      writeStats("notice: clean clinit")
      Clean
    }
  }

  private def collectFieldsToAnalyse(host: symlevel.ClassType): Maps[Field]#QMap[FieldInfo] = {
    val result = Maps[Field].newQMap[FieldInfo]
    for (field <- host.getDeclaredFields if isFieldAnalyzableByClinitAnalysis(field)) {
      result(field) = null
    }
    result
  }

  /** Returns true, iff given `node` in given `host` class clinit could not lead to some other code execution. */
  private def isClean(node: SpinalNode, host: symlevel.Type): Boolean = node match {
    case _: Deferred | _: BitcodeDeferred | _: AbstractCall | _: Clinit =>
      false

    case _: Marker | _: PureCheck | _: CheckCastTrustedDelayed | _: StrConcat | _: AssertNode |
         _: Prefetch | _: IDivRemOp | _: PutMemoryOperation | _: ArrayFill | _: AJArrayFill | _: StackZeroing |
         _: LocalReachabilityShield | _: Throw | _: Catch | _: ConvertDomain | _: MemBarrier |
         _: MonitorOperation | _: TrapCheck | _: AJCallerClass |
         _: ThinNew | _: ThinInstanceOf | _: AnyNew | _: NewArrayCopyRT |
         _: NewArrayRT | _: PublishRef | _: ConcealRef | _: PreparationCheck | _: BoxedValue | _: GetClass |
         _: WriteBarrier | _: BeginLocalUnmovable | _: EndLocalUnmovable | _: IncHeldLocks | _: DecHeldLocks |
         _: StackDescriptor | _: MemAtomic | _: ExecEnvInvalidationPoint |
         _: DebugBreakpoint =>
      true

    case _ => shouldNotReachHere(s"unknown node in clinit analysis: $node")
  }

  // TODO: do not scan non-clinit methods in class files of Java >= 9
  /** Filter out fields from given `fields`, which are redefined in non-clinit methods of given `host`. */
  private def filterRedefinedFields(host: symlevel.ClassType, fields: Maps[Field]#QMap[FieldInfo]): Unit = {
    val constantPool = host.getClassConstantPool

    for (method <- host.getDeclaredMethods if method != rootMethod;
         codeAttr = method.codeAttribute if codeAttr != null) {

      val bcIterator = new BytecodeIterator(codeAttr)

      while (bcIterator.hasNext) {
        val bytecode = bcIterator.next()
        bytecode match {
          case Bytecode.PUTSTATIC =>
            val fieldIndex = bcIterator.param
            val fieldAccess = constantPool.getField(fieldIndex, FieldAccessKind.PUTSTATIC)
            fieldAccess.getResult match {
              case ConstantPoolAccessResult.OK =>
                val field = fieldAccess.getObject
                if (fields contains field) {
                  writeStats(
                    s"fail: field redefined in non-clinit method",
                    s"fail: '${field.getName}' redefined in non-clinit method")
                  fields.remove(field)
                }
              case _ =>
            }
          case _ =>
        }
      }
    }
  }

  private def analyzePutFields(fields: Maps[Field]#QMap[FieldInfo], cleanness: Cleanness): Unit = {
    // TODO: fill values from bytecode ConstantValue
    // Motivation for reference fields: non-null Strings
    // Motivation for primitive fields: unknown (javac does constant propagation already)

    val ret = Return.unique.get
    val isAggressiveAnalysis = cleanness.isInstanceOf[AggressivelyClean]

    val fieldPuts = groupBy(all[PutStatic])(_.field)

    for (field <- fields.keys) {
      val info = fieldPuts.get(field) match {
        case None =>
          writeStats(
            s"fail: field not assigned in clinit",
            s"fail: '${field.getName}' not assigned in clinit")
          None

        case Some(xs) if xs.size > 1 =>
          writeStats(
            s"fail: field assigned more than once in clinit",
            s"fail: '${field.getName}' assigned more than once in clinit")
          None

        case Some(Seq(put)) =>
          if (!(put.outCtrl dominates ret)) {
            writeStats(
              s"fail: field assignment does not dominate clinit exit",
              s"fail: '${field.getName}' assignment does not dominate clinit exit")
            None

          } else {
            val value = put.storedValue()
            val isFieldDirty = cleanness match {
              case cleanness: NonClean =>
                (cleanness.dirtyBorder dominates put) ensuring (_ != (put dominates cleanness.dirtyBorder), "border must be strict")

              case Clean =>
                false
            }

            if (field.getType.isPrimitive) {
              if (isFieldDirty && !isAggressiveAnalysis) {
                writeStats(
                  s"fail: primitive field in dirty part",
                  s"fail: '${field.getName}' is primitive and assigned in dirty part")
                None

              } else {
                value match {
                  case NumericalConst(c) =>
                    writeStats(
                      s"ok: primitive field value defined",
                      s"ok: '${field.getName}' value = $c")
                    assert(!isFieldDirty || isAggressiveAnalysis)
                    Some(PrimitiveFieldInfo(isFieldDirty, c))
                  case _ =>
                    writeStats(
                      s"fail: primitive field assigned non-constant value",
                      s"fail: '${field.getName}' assigned non-constant value")
                    None
                }
              }

            } else {
              nodeTypeAt(value, put) match {
                case RefEmpty =>
                  // we have empty approximation =>
                  //   we have unreachable putstatic & it dominates return (checked above) =>
                  //   return is unreachable
                  // If this happens consider to enhance optimizations to eliminate unreachable code.
                  //
                  // We could also enhance algorithm in general to mark such classes as uninitializable
                  // and use this information to further propagate unreachability.
                  // (Or at least mark whole clinit as "clinit has no return".)
                  //
                  // For now we just ignore such fields, it's ok.
                  writeStats(
                    s"fail: unreachable reference field initialization",
                    s"fail: unreachable '${field.getName}' initialization")
                  None

                case initType =>
                  // Make it nullable to make nullcheck work as initialization check.
                  val isCoarsened = isFieldDirty && !isAggressiveAnalysis && !initType.mayBeNull
                  val fieldType = if (isCoarsened) initType.withNull else initType
                  val coarsenedMsg = if (isCoarsened) " (coarsened type in dirty part)" else ""

                  val isRefinedType = (fieldType compareWidened formalTypeApproximation(field.getType)) == CC.Less
                  val fieldTypeOpt = if (isRefinedType) {
                    writeStats(
                      s"ok: field has refined type$coarsenedMsg",
                      s"ok: '${field.getName}' has type $fieldType$coarsenedMsg")
                    // Aggressiveness affects presence of explicit check (see AggressiveClinitAnalysisAssert).
                    // Nullable fields are guarded by NullCheck, no need for aggressive check.
                    Some(TypeInfo(isFieldDirty && isAggressiveAnalysis && !fieldType.mayBeNull, fieldType))
                  } else {
                    writeStats(
                      s"fail: reference field type is not refined$coarsenedMsg",
                      s"fail: '${field.getName}' type is not refined$coarsenedMsg")
                    None
                  }

                  def arrayLengthConstStat(lengthOpt: Option[Long]): Option[Long] = {
                    lengthOpt match {
                      case Some(length) =>
                        writeStats(
                          s"ok: array field length defined",
                          s"ok: '${field.getName}' length = $length")
                      case _ =>
                        writeStats(
                          s"fail: array field length is non-constant",
                          s"fail: '${field.getName}' length is non-constant")
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
                        writeStats(
                          s"fail: array field value is not new[]",
                          s"fail: '${field.getName}' value is not new[]")
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
          }

        case Some(xs) => shouldNotReachHere(xs)
      }

      assert(fields(field) == null)
      info match {
        case Some(i) => fields(field) = i
        case None => fields.remove(field)
      }
    }
  }

}

trait ClinitAnalysisConfig { self: Universe =>

  /** Returns true if we may assume that every field of given `klass` is initialized before any read. */
  def isAggressiveClinitAnalysisAllowedFor(klass: symlevel.Type): Boolean = {
    if (env.enabled(NoClinitAnalysis) || env.enabled(NoAggressiveClinitAnalysis)) {
      return false
    }

    klass.isOptimizedAggressively
  }

  def isFieldAnalyzableByClinitAnalysis(field: Field): Boolean = {
    field.isStatic && field.isFinal &&
      !field.getDeclaringClass.isCangjieType &&
      // exclude "write-protected" fields, see JLS
      !(field.getDeclaringClass.isJavaLangSystem &&
        (field.getName match { case "out" | "err" | "in" => true; case _ => false }))
  }
}
