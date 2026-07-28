/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.serialization

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.ir.{CallEscapeKind, EscapeKind, EscapeKindTuple, NewEscapeKind}
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo.*
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Field, Method, TypeKind, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.References.ReferenceApprox
import com.huawei.excelsior.jet.compiler.util.{Log, Names}
import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider}
import com.huawei.excelsior.jet.compiler.PDB2.EntryKind
import com.huawei.excelsior.jet.util.ScalaCollections
import xscala.io.{DataInput, DataOutput, Path}
import xscala.properties.OS

import scala.collection.mutable
import scala.ref.SoftReference

trait ExtraInfo extends BinaryIO {
  def env: Environment
  private implicit def typeProvider: TypeProvider = env.getTypeProvider
  protected def globalInfoUpdated(): Unit

  /** Version of serialized ExtraInfo format. */
  val ExtraInfoFormatVersion = 22

  private def log = Log(Log.Kind.ExtraInfo)

  protected abstract class ExtraInfoProvider[Object, Info] {
    protected def cache: mutable.HashMap[Long, Info]
    protected def uniqueNumber(obj: Object): Long
    protected def name(obj: Object): String
    protected def updateVersion: Boolean

    def get(obj: Object): Option[Info] = {
      cache.get(uniqueNumber(obj))
    }

    def put(obj: Object, info: Info): Unit = {
      if (log.isEnabled) {
        log.inSession(s"ExtraInfo for ${name(obj)}:") {
          log(info.toString)
        }
      }

      cache.put(uniqueNumber(obj), info)

      if (updateVersion) {
        _globalInfoVersion += 1
        globalInfoUpdated()
      }
    }
  }

  protected abstract class RemovableExtraInfoProvider[Object, Info] extends ExtraInfoProvider[Object, Info] {
    def remove(obj: Object): Unit = {
      cache.remove(uniqueNumber(obj))
    }
  }

  protected abstract class SerializableExtraInfoProvider[Object, Info] extends ExtraInfoProvider[Object, Info] {
    protected def maxReserializationsCount: Int
    protected def makeSerializer(n: Int): Serializer
    private val serializers = Array.tabulate(maxReserializationsCount)(makeSerializer)

    override def get(obj: Object): Option[Info] = {
      super.get(obj) orElse {
        val infoOpt = ScalaCollections.firstElement(serializers.reverseIterator flatMap (_ deserialize obj))
        for (info <- infoOpt) {
          cache.put(uniqueNumber(obj), info)
        }
        infoOpt
      }
    }

    override def put(obj: Object, info: Info): Unit = {
      super.put(obj, info)
      ScalaCollections.firstElement(serializers.iterator filterNot (_ isSerialized obj)) match {
        case Some(x) => x.serialize(obj, info)
        case None => throw SerializationError("extra info rewrites limit exceeded")
      }
    }

    protected trait Serializer {
      protected def locationProvider: ExtraInfoLocationProvider[Object]
      protected def contextClass(obj: Object): ClassType
      protected def deserializeImpl(read: Reader, obj: Object): Info
      protected def serializeImpl(write: Writer, obj: Object, info: Info): Unit

      def isSerialized(obj: Object) = locationProvider.isSerialized(obj, env)

      def deserialize(obj: Object): Option[Info] = {
        if (isSerialized(obj)) {
          locationProvider.read(obj, env) { s =>
            Some(deserialize(newReader(s, obj), obj))
          }
        } else None
      }

      def serialize(obj: Object, info: Info): Unit = {
        assert(!isSerialized(obj))
        locationProvider.write(obj, env) { s =>
          serialize(newWriter(s, obj), obj, info)
        }
      }

      private def deserialize(read: Reader, obj: Object): Info = {
        val version = read.number()
        if (version != ExtraInfoFormatVersion) {
          SerializationError("Invalid serialized extra info format version " + version + ", expected " + ExtraInfoFormatVersion)
        }

        read.readHeader()
        deserializeImpl(read, obj)
      }

      private def serialize(write: Writer, obj: Object, info: Info): Unit = {
        write.number(ExtraInfoFormatVersion)

        val buf = write.withBuffering { serializeImpl(write, obj, info) }
        write.writeHeader()
        write.writeBuffer(buf)
      }

      private def newWriter(out: DataOutput, obj: Object) = new BinaryWriter(out, contextClass(obj), env)
      private def newReader(in: DataInput, obj: Object) = new BinaryReader(in, contextClass(obj), env)
    }
  }


  /** Local methods info is stored in this cache until middle phase when it will be serialized and removed from this cache. */
  private val methodsExtraInfoLocal = new RemovableExtraInfoProvider[Method, MethodExtraInfoLocal] {
    override protected def cache = methodsExtraInfoLocalCache
    override protected def uniqueNumber(obj: Method) = obj.getUniqueNumber
    override protected def name(obj: Method) = obj.getFullName
    override protected def updateVersion = false
  }

  private val methodsExtraInfo = new SerializableExtraInfoProvider[Method, MethodExtraInfo] {
    override protected def cache = methodsExtraInfoCache.getOrCreate()
    override protected def uniqueNumber(obj: Method) = obj.getUniqueNumber
    override protected def name(obj: Method) = obj.getFullName
    override protected def updateVersion = true

    private def hasTraceableReferenceParams(obj: Method) =
      (0 until obj.getParamsCount) exists { i => obj.getParamType(i).isTraceableReference }

    override protected def maxReserializationsCount = 1
    override protected def makeSerializer(n: Int) = new Serializer {
      override protected def locationProvider = methodExtraInfoLocation

      override protected def contextClass(obj: Method): ClassType = obj.getDeclaringClass

      override protected def serializeImpl(write: Writer, obj: Method, info: MethodExtraInfo): Unit = {
        val localInfo = info.local
        write.bool(localInfo.cfi)
        write.bool(localInfo.cfiWithGuard)
        write.doubleNumber(localInfo.bodyWeight)
        write.doubleNumber(localInfo.bodyDuration)
        write.bool(localInfo.leaf)
        write.bool(localInfo.isScalarMethod)
        write.bool(localInfo.isDirtyForClassGC)
        write.bool(localInfo.isUnstructuredLocking)

        write.set(localInfo.syncedParams)(write.number)
        write.doubleNumber(localInfo.bodySyncOperationsWeight)
        write.bool(localInfo.badForCBC)
        write.set(localInfo.alwaysEvacuatedParams)(write.number)
        write.bool(localInfo.isO1Compiled)
        write.bool(localInfo.isNoReturn)

        val globalInfo = info.global
        if (obj.isClinit) {
          write.bool(globalInfo.isCleanClinit)
        } else {
          assert(!globalInfo.isCleanClinit)
        }
        if (obj.getReturnType.isReference) {
          write.option(globalInfo.returnType)(write.typeApproximation)
          if (obj.getReturnType.isTraceableReference) {
            write.seq(globalInfo.generalizedNewTypes)(write.symType)
          }
        } else {
          assert(globalInfo.returnType.isEmpty)
          assert(globalInfo.generalizedNewTypes.isEmpty)
        }
        if (hasTraceableReferenceParams(obj)) {
          write.option(globalInfo.paramsEscape) { escs =>
            escs.zipWithIndex foreach { case (esc, idx) =>
              if (obj.getParamType(idx).isTraceableReference) {
                esc match {
                  case kind: CallEscapeKind =>
                    write.putInt(0)
                    write.enumeration(kind)
                  case kind: NewEscapeKind =>
                    write.putInt(1)
                    write.enumeration(kind)
                  case EscapeKindTuple(callEscapeKind, newEscapeKind) =>
                    write.putInt(2)
                    write.enumeration(callEscapeKind)
                    write.enumeration(newEscapeKind)
                }
              } else {
                assert(esc == null)
              }
            }
          }
        } else {
          assert(globalInfo.paramsEscape.isEmpty)
        }
      }

      override protected def deserializeImpl(read: Reader, obj: Method): MethodExtraInfo = MethodExtraInfo(
        MethodExtraInfoLocal(
          cfi = read.bool(),
          cfiWithGuard = read.bool(),
          bodyWeight = read.doubleNumber(),
          bodyDuration = read.doubleNumber(),
          leaf = read.bool(),
          isScalarMethod = read.bool(),
          isDirtyForClassGC = read.bool(),
          isUnstructuredLocking = read.bool(),
          syncedParams = read.set(read.number),
          bodySyncOperationsWeight = read.doubleNumber(),
          badForCBC = read.bool(),
          alwaysEvacuatedParams = read.set(read.number),
          isO1Compiled = read.bool(),
          isNoReturn = read.bool(),
        ),
        {
          MethodExtraInfoGlobal(
            isCleanClinit =
              if (obj.isClinit) {
                read.bool()
              } else {
                false
              },
            returnType =
              if (obj.getReturnType.isReference) {
                read.option { () => read.typeApproximation() }
              } else {
                None
              },
            generalizedNewTypes =
              if (obj.getReturnType.isTraceableReference) {
                read.seq(read.symType)
              } else {
                Seq.empty
              },
            paramsEscape =
              if (hasTraceableReferenceParams(obj)) {
                read.option { () =>
                  (0 until obj.getParamsCount) map { idx =>
                    if (obj.getParamType(idx).isTraceableReference) {
                      read.nextInt() match {
                        case 0 => read.enumeration(CallEscapeKind.fromOrdinal)
                        case 1 => read.enumeration(NewEscapeKind.fromOrdinal)
                        case 2 => EscapeKindTuple(read.enumeration(CallEscapeKind.fromOrdinal), read.enumeration(NewEscapeKind.fromOrdinal))
                        case x => shouldNotReachHere(s"Unexpected EscapeKind number $x")
                      }
                    } else {
                      null
                    }
                  }
                }
              } else {
                None
              }
          )
        }
      )
    }
  }

  def loadMethodLocalAnalysisResults(method: Method): Option[MethodExtraInfoLocal] =
    (methodsExtraInfoLocal get method) orElse (methodsExtraInfo get method map (_.local))

  def loadMethodGlobalAnalysisResults(method: Method): Option[MethodExtraInfoGlobal] =
    methodsExtraInfo get method map (_.global)

  def saveMethodLocalAnalysisResults(method: Method, info: MethodExtraInfoLocal): Unit = {
    assert((methodsExtraInfo get method).isEmpty)
    methodsExtraInfoLocal.put(method, info)
  }

  def saveMethodGlobalAnalysisResults(method: Method, info: MethodExtraInfoGlobal): Unit = {
    methodsExtraInfoLocal get method match {
      case Some(local) =>
        methodsExtraInfo.put(method, MethodExtraInfo(local = local, global = info))
        methodsExtraInfoLocal.remove(method)

      case x => shouldNotReachHere(s"unexpected state of method extra info: $x")
    }
  }

  val classFieldsExtraInfo = new SerializableExtraInfoProvider[ClassType, ClassFieldsExtraInfo] {
    override protected def cache = classFieldsExtraInfoCache.getOrCreate()
    override protected def uniqueNumber(c: ClassType) = c.getUniqueNumber
    override protected def name(c: ClassType) = c.getName
    override protected def updateVersion = true

    override protected def maxReserializationsCount = 2
    override protected def makeSerializer(n: Int) = new Serializer {
      override protected val locationProvider = new ClassFieldsExtraInfoLocationProvider(n)

      override protected def contextClass(obj: ClassType): ClassType = obj

      override protected def serializeImpl(write: Writer, obj: ClassType, info: ClassFieldsExtraInfo): Unit = {
        write.map(info) { case (f, finfo) =>
          write.bool(finfo.isAggressive)
          write.field(f)
          val tk = typeKind(f)
          if (tk.isReference) {
            assert(finfo.constantValue.isEmpty)
            assert(finfo.arrayLength.isDefined || finfo.typeApprox.isDefined)
            write.option(finfo.arrayLength)(write.longNumber)
            write.option(finfo.typeApprox) { x => write.typeApproximation(x) }
          } else {
            assert(finfo.arrayLength.isEmpty && finfo.typeApprox.isEmpty)
            assert(finfo.constantValue.isDefined)
            write.anyNumber(tk, finfo.constantValue.get)
          }
        }
      }

      override protected def deserializeImpl(read: Reader, obj: ClassType): ClassFieldsExtraInfo = {
        read.map { () =>
          val isAggressive = read.bool()
          val f = read.field()
          val tk = typeKind(f)
          val finfo = if (tk.isReference) {
            FieldExtraInfo(
              isAggressive,
              arrayLength = read.option(read.longNumber),
              typeApprox = read.option { () => read.typeApproximation() },
              constantValue = None
            )
          } else {
            FieldExtraInfo(
              isAggressive,
              arrayLength = None,
              typeApprox = None,
              constantValue = Some(read.anyNumber(tk))
            )
          }
          (f, finfo)
        }
      }

      private def typeKind(f: Field): TypeKind = f.getType.symKindErased.toBytecodeApproximation
    }
  }

  val classReceiverEscapeInfo = new ExtraInfoProvider[ClassType, ClassReceiverEscapeExtraInfo] {
    override protected def cache = classReceiverEscapeInfoCache.getOrCreate()
    override protected def uniqueNumber(obj: ClassType) = obj.getUniqueNumber
    override protected def name(obj: ClassType): String = obj.getName
    override protected def updateVersion = false
  }
}

object ExtraInfo {

  def apply(_env: Environment) = new ExtraInfo {
    override def env = _env
    override protected def globalInfoUpdated(): Unit = {}
  }

  /** Some extra info about method's body calculated locally. */
  case class MethodExtraInfoLocal(
                                   cfi: Boolean,
                                   cfiWithGuard: Boolean,
                                   bodyWeight: Double,
                                   bodyDuration: Double,
                                   leaf: Boolean,
                                   isScalarMethod: Boolean,
                                   isDirtyForClassGC: Boolean,
                                   isUnstructuredLocking: Boolean,
                                   syncedParams: Set[Int],
                                   bodySyncOperationsWeight: Double,
                                   badForCBC: Boolean,
                                   alwaysEvacuatedParams: Set[Int],
                                   isO1Compiled: Boolean,
                                   isNoReturn: Boolean,
                                 )

  object MethodExtraInfoLocal {
    def empty: MethodExtraInfoLocal = MethodExtraInfoLocal(
        cfi = false,
        cfiWithGuard = false,
        bodyWeight = 0.0,
        bodyDuration = 0.0,
        leaf = false,
        isScalarMethod = false,
        isDirtyForClassGC = false,
        isUnstructuredLocking = false,
        syncedParams = Set.empty,
        bodySyncOperationsWeight = 0.0,
        badForCBC = false,
        alwaysEvacuatedParams = Set.empty,
        isO1Compiled = false,
        isNoReturn = false,
      )
  }

  /** Some extra info about method's body calculated using interprocedural analysis. */
  case class MethodExtraInfoGlobal(
                                    // Wheather this method is clinit without any not allowed nodes (see ClinitAnalysis)
                                    // and all it's host superclasses and superinterfaces have clean clinits
                                    isCleanClinit: Boolean,
                                    // Some refined type of reference return value,
                                    // none if calculated type is equal to formal or method has primitive return value (or void)
                                    returnType: Option[ReferenceApprox],
                                    // Set of generalized new types: method allocates and returns them.
                                    generalizedNewTypes: Seq[SymType],
                                    // Sequence of refined escape kind of all reference params, element is `null` for all primitive params,
                                    // empty option if all params are GlobalEscape.
                                    paramsEscape: Option[Seq[EscapeKind]]
                                  )

  case class MethodExtraInfo(local: MethodExtraInfoLocal, global: MethodExtraInfoGlobal)

  case class FieldExtraInfo(
                             // Whether this information was obtained using aggressive clinit analysis.
                             isAggressive: Boolean,
                             // for reference type fields:
                             arrayLength: Option[Long],
                             typeApprox: Option[ReferenceApprox],
                             // for primitive type fields:
                             constantValue: Option[Number]
                           )

  type ClassFieldsExtraInfo = Map[Field, FieldExtraInfo]

  type ClassReceiverEscapeExtraInfo = EscapeKind


  private[ExtraInfo] abstract class ExtraInfoLocationProvider[Object] extends SerializationToolbox {
    protected def contextClass(obj: Object): ClassType
    protected def itemID(obj: Object): String

    private def location(obj: Object) = EntryKind.ExtraInfo.loc(contextClass(obj), itemID(obj))

    def isSerialized(obj: Object, env: Environment): Boolean = env.pdb.exists(location(obj))
    def read[A](obj: Object, env: Environment)(read: DataInput => A): A = readPDBFile(env, location(obj))(read)
    def write(obj: Object, env: Environment)(write: DataOutput => Unit): Unit = writePDBFile(env, location(obj))(write)
  }

  def methodHasGlobalExtraInfo(obj: Method, env: Environment): Boolean =
    methodExtraInfoLocation.isSerialized(obj, env)

  /** Project system has an internal assumption that entry names for .irei files
    * are class names + File.separator + some suffix (to reclaim old entries from PDB).
    * So if it is changed the changes must be reflected in xcMain.ResourceCleanupAdviser.
    */
  private val methodExtraInfoLocation = new ExtraInfoLocationProvider[Method] {
    override protected def contextClass(obj: Method) = obj.getDeclaringClass
    override protected def itemID(obj: Method) = "m" + Names.shortName(obj)
  }

  private final class ClassFieldsExtraInfoLocationProvider(n: Int) extends ExtraInfoLocationProvider[ClassType] {
    override protected def contextClass(c: ClassType) = c
    override protected def itemID(c: ClassType) = "f" + n
  }


  /** The value is stored as a soft reference.
    * If it was collected by GC, the new one will be created by [[getOrCreate()]].
    */
  private class SoftReferencedValue[A <: AnyRef](createValue: () => A) {
    var valueRef = new SoftReference(null.asInstanceOf[A])

    def getOrCreate(): A = {
      valueRef.get.getOrElse {
        val x = createValue()
        valueRef = new SoftReference(x)
        x
      }
    }
  }

  private [ExtraInfo] var _globalInfoVersion = 0
  def globalInfoVersion = _globalInfoVersion

  private val methodsExtraInfoLocalCache = new mutable.HashMap[Long, MethodExtraInfoLocal]
  private val methodsExtraInfoCache = new SoftReferencedValue(() => new mutable.HashMap[Long, MethodExtraInfo])
  private val classFieldsExtraInfoCache = new SoftReferencedValue(() => new mutable.HashMap[Long, ClassFieldsExtraInfo])
  private val classReceiverEscapeInfoCache = new SoftReferencedValue(() => new mutable.HashMap[Long, ClassReceiverEscapeExtraInfo])
}
