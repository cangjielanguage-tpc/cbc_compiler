/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{addressSize, languagePack}
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.debug.info.DebugType
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.PUBLIC
import com.huawei.excelsior.jet.compiler.options.ConstRTFieldsValue
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{Float32, Int16, Int32, Int64, javaLangString, Void as V}
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.*
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodReferenceAccessKind, SignatureType, TypeKind, MethodSignature as MSig}
import com.huawei.excelsior.jet.compiler.*
import xscala.util.StringOps.*

import scala.PartialFunction.condOpt
import scala.annotation.nowarn
import scala.collection.mutable
import scala.runtime.ScalaRunTime

/**
 * Fields from runtime structures. For example, JavaObj.td, JavaArray.length, etc.
 *
 * @author conwor
 */
trait RTStructs { this: CompilerEnvironment =>

  private lazy val sigObjectType = SignatureType.javaLangObject
  private val sigByteType = SignatureType.Primitive(BYTE)
  private val sigShortType = SignatureType.Primitive(SHORT)
  private val sigIntType = SignatureType.Primitive(INT)
  private val sigAddrType = SignatureType.Address
  private val sigAddrIntType = SignatureType.AddrInt

  import ConstRTFieldsValue.*

  private def undefined = shouldNotCallThis("not supported in RT structs")

  abstract class RTStruct extends symlevel.ClassType { host: Product =>

    class Field(sigType: SignatureType, offset: Int, val rtConst: ConstRTFieldsValue) extends symlevel.Field { this: Product =>
      def getDeclaringClass = host
      def getXName = XString.ascii(ScalaRunTime._toString(this))
      def getType = sigType
      def getJavaModifiersValue = Modifiers(PUBLIC).value
      def getCJModifiers = Modifiers(PUBLIC)
      def getOffset = offset
      def isAJFlat = false
      def isRunTimeConstant = rtConst.isEnabled(env)

      def getStaticFieldSymbol = undefined
      def getFieldIndex = undefined
      def getUniqueNumberInClass = undefined
      def hasInitialValue = undefined
      def getInitialValue = undefined
      def size = undefined
      def alignment = undefined
      def getPermanent = undefined

      def shouldBeGenerated = undefined
      def getExportedName = undefined
      def getExternalName = undefined
      def isOverloaded = undefined
      def getCHIRDef = undefined

      def isStringTable: Boolean = undefined
      def getCJAnnotationFactory: Method = undefined
    }

    abstract class PlainEopField(offset: Int, rtConst: ConstRTFieldsValue = UNCLASSIFIED) extends Field(sigObjectType, offset, rtConst) { this: Product => }
    abstract class IntField(offset: Int, rtConst: ConstRTFieldsValue = UNCLASSIFIED) extends Field(sigIntType, offset, rtConst) { this: Product => }
    abstract class ShortField(offset: Int, rtConst: ConstRTFieldsValue = UNCLASSIFIED) extends Field(sigShortType, offset, rtConst) { this: Product => }
    abstract class ByteField(offset: Int, rtConst: ConstRTFieldsValue = UNCLASSIFIED) extends Field(sigByteType, offset, rtConst) { this: Product => }
    abstract class AddrField(offset: Int, rtConst: ConstRTFieldsValue = UNCLASSIFIED) extends Field(sigAddrType, offset, rtConst) { this: Product => }
    abstract class AddrIntField(offset: Int, rtConst: ConstRTFieldsValue = UNCLASSIFIED) extends Field(sigAddrIntType, offset, rtConst) { this: Product => }

    abstract class FlatField(offset: Int, rtConst: ConstRTFieldsValue = UNCLASSIFIED) extends Field(sigAddrType, offset, rtConst) { this: Product =>
      assert(!host.isInstanceOf[ObjectRTStruct]) // TODO: what about managed objects?
      override def isAJFlat = true
    }

    protected implicit def provider: TypeProvider = typeProvider

    def getKind = CLASS
    def getXName = XString.ascii(ScalaRunTime._toString(this))
    def isThinClass: Boolean = false

    def getClassConstantPool = undefined
    def getAccessFlags = undefined
    def getCJModifiers = undefined
    def isAbstractClass = undefined
    def isFinal = undefined
    def isCHIRDef = undefined
    def hasDeferredSuper = undefined
    def isRealAbsentForAOT = undefined
    def isUnavailableForAOT = undefined
    def isUnloadable = undefined
    def isSynthetic = undefined
    def isInCurrentCompilationSet = undefined
    def isBytecodeAvailable = undefined
    def isPreClinited = undefined
    def isTurboClinited = undefined
    def getClinit = undefined
    def finalizable = undefined
    def hasDeclaredSuperInterfaces = undefined
    def getDeclaredSuperInterfaces = undefined
    def getDeclaredMethods = undefined
    def dropDeclaredMethodsCache(): Unit = undefined
    def getGeneratedMethods = undefined
    def getGeneratedMethodIndex(method: symlevel.Method) = undefined
    def getDeclaredFields = undefined
    def getCurrentDeclaredFields = undefined
    def dropDeclaredFieldsCache(): Unit = undefined
    def getVersionedMethods = undefined
    def chooseMethodVersion(method: symlevel.Method) = undefined
    def getArrayDimnum = undefined
    def getArrayBase = undefined
    def getArrayElemType = undefined
    def getVArrayLength = undefined
    def getVArrayElemType = undefined
    def getArraySliceElemType = undefined
    def getCangjieBoxValueType = undefined
    def isSamePackage(that: symlevel.ClassType) = undefined
    def getSuperClass = undefined
    def getCohenSupertype = undefined
    def getCohenLevel = undefined
    def getObjectHeaderSize = undefined
    def getRawObjectSize = undefined

    def classHasRefFields: Boolean = undefined
    def computeTSWord(alignedSizeInBytes: Int, hasFinalize: Boolean, special: Boolean, isArray: Boolean, isStackAlloc: Boolean, noRefFields: Boolean, isGuest: Boolean, hasCHA: Boolean): Int = undefined
    def getObjectAlignment = undefined
    def getThinTypeHandle = undefined
    def getTypeHandle = undefined
    def getInstanceDescriptor = undefined
    def getSingletonObject = undefined
    def isSingletonObject = undefined
    def isPrepared = undefined
    def isVArray = undefined
    def isAJArray = undefined
    def doesImplement(interfType: symlevel.ClassType) = undefined
    def isJavaLangObject = undefined
    def isHierarchyRoot = undefined
    def isJavaLangCloneable = undefined
    def isJavaIoSerializable = undefined
    def isJavaLangSystem = undefined
    def isJavaLangClassLoader = undefined
    def isSunMiscUnsafe = undefined
    def isXScalaAnyRef = undefined
    def isAnonymous = undefined
    def hasSequentialLayout = undefined
    def getKey = undefined
    def getMangledName = undefined
    def getSourceFile = undefined
    def setSourceFile(sourceFile: XString) = undefined
    def setSourceFullName(sourceFullName: XString) = undefined
    def getInputFile = undefined
    def getRefFieldOffsets = undefined
    def getMTLayout = undefined
    def getVerificationInfo = undefined
    def isJetRuntimeClass = undefined
    def isSystemClass = undefined
    def isJDKClass = undefined
    def isOptimizedAggressively = undefined
    def hasRunTimeTypeInfo = undefined
    def preparationRequired = undefined
    def isBootstrapAnnotated = undefined
    def isNonBootstrapAnnotated = undefined
    def isCompilerInterface = undefined
    def getUniqueNumber = undefined
    def isAssignableFrom(symLevelType: symlevel.Type) = undefined
    def getClassLoaderID = undefined
    def getClassLoaderSID = undefined

    def isLambdaClass = undefined
    def isCangjieLambdaClass = undefined
    def isEvacuatedType = undefined
    def getLambdaInfo = undefined
    def isInfectedAJClass: Boolean = undefined
    protected def isAJCompoundBaseType: Boolean = undefined
    def isNamespace: Boolean = undefined
    def isValueClass: Boolean = undefined
    def isStructClass: Boolean = undefined
    def isPolyThinClass: Boolean = undefined
    def isAJManagedType: Boolean = undefined
    def isAJExtendedType: Boolean = undefined
    def getThinInheritanceLevel: Int = undefined
    def getClassBytes = undefined
    def isInterpreterInternals: Boolean = undefined
    def isVerifiable: Boolean = undefined

    def getCangjiePackage = undefined
    def isCangjieType = undefined
    def isJavaAnnotatedCangjieClass = undefined
    def isCangjieJavaHelper = undefined
    def isCangjieArray = undefined

    def isXScalaType = undefined

    def getConstString(value: XString) = undefined
    def getImportTable = undefined
    def getDebugType: DebugType = undefined
    def setDebugType(tpe: DebugType) = undefined
    def getPackageName = undefined
    def getCJAnnotationFactory: Method = undefined
    def isUniversalGeneric = false
    def getGenericInfo = undefined
    def getCHIRVTable = undefined
    def isCangjieEnum = undefined
    def getCangjieEnumInfo = undefined
    def isCangjieExtend = undefined
    def getCangjieExtendInfo = undefined
  }

  /** Runtime structure, reference to which is equal with reference to managed object. */
  abstract class ObjectRTStruct extends RTStruct { this: Product => }


  class SymRTClass(name: String) {
    private val tpe = typeProvider.resolveTypeByName(typeProvider.getObjectType, XString.ascii(name)) ensuring
      {tpe => (tpe.isClassOrInterface || tpe.isRecord) && !tpe.isDeferred}

    def unapply(otherType: symlevel.Type) = tpe == otherType

    def methodRef(name: String, signature: MSig = null, aKind: MethodReferenceAccessKind = MethodReferenceAccessKind.STATIC) =
      Option(tpe).map(_.getMethodRefToLocal(xstr(name), signature, aKind)).orNull

    def method(name: String, signature: MSig = null) =
      Option(tpe).map(_.findDeclaredMethod(xstr(name), signature)).orNull

    def methodOrNull(name: String, signature: MSig = null) =
      Option(tpe).map(_.findDeclaredMethodOrNull(xstr(name), signature)).orNull

    def field(name: String) = Option(tpe).map(_.findDeclaredFieldOrNull(xstr(name))).orNull

    def symType = tpe
  }

  object RT {
    case object ManagedObj extends ObjectRTStruct {
      case object td extends AddrField(RTConst.HeapObj.TYPEDESC_OFFSET.intValue, MANAGED_OBJ_TD)
      case object tsWordConst extends IntField(RTConst.HeapObj.TSWORD_OFFSET.intValue, MANAGED_OBJ_TSWORD_CONST)
    }

    case object JavaArray extends ObjectRTStruct {
      case object length extends IntField(RTConst.JavaArray.length.offset)
    }

    case object ScalaArray extends ObjectRTStruct {
      case object length extends IntField(RTConst.ScalaArray.length.offset)
    }

    case object AJArray extends ObjectRTStruct {
      case object length extends IntField(RTConst.AJArray.LENGTH_OFFS.intValue) {
        override def isRunTimeConstant = true // TODO: consider including it in ConstRTFieldsValue as always enabled
      }

      case object largeLength extends AddrField(RTConst.AJArray.LARGE_LENGTH_OFFS.intValue)
    }

    case object CangjieArray extends ObjectRTStruct {
      case object length extends IntField(RTConst.CangjieArray.LENGTH_OFFS.intValue) {
        override def isRunTimeConstant = true // TODO: consider including it in ConstRTFieldsValue as always enabled
      }

      case object largeLength extends AddrField(RTConst.CangjieArray.LARGE_LENGTH_OFFS.intValue)
    }

    case object RawJavaArray extends RTStruct {
      case object length extends IntField(RTConst.JavaArray.length.offset)
    }

    case object RawScalaArray extends RTStruct {
      case object length extends IntField(RTConst.ScalaArray.length.offset)
    }

    case object RawAJArray extends RTStruct {
      case object length extends IntField(RTConst.AJArray.LENGTH_OFFS.intValue)
    }

    case object RawCangjieArray extends RTStruct {
      case object length extends IntField(RTConst.CangjieArray.LENGTH_OFFS.intValue)
    }

    case object HeapObj extends RTStruct {
      case object td extends AddrField(RTConst.HeapObj.TYPEDESC_OFFSET.intValue)
      case object tsWord extends IntField(RTConst.HeapObj.TSWORD_OFFSET.intValue)
      case object tsWordDebug extends IntField(RTConst.HeapObj.DEBUG_TSWORD_OFFSET.intValue)
    }


    case object TypeHandle extends RTStruct {
      case object classObject extends PlainEopField(RTConst.TypeHandle.rawClassObject.offset)
      case object flags extends ShortField(RTConst.TypeHandle.flags.offset)
      case object td extends AddrField(RTConst.TypeHandle.td.offset)
    }

    case object HostingRunTimeTypeInfo extends RTStruct {
      case object cFuncWrappers extends AddrField(RTConst.HostingRunTimeTypeInfo.cFuncWrappers.offset)
    }

    case object HostingTypeHandle extends RTStruct {
      case object initialized extends AddrField(RTConst.HostingTypeHandle.initialized.offset)
    }

    case object InstanceDescriptor extends RTStruct {
      case object rtti extends AddrField(RTConst.InstanceDescriptor.rtti.offset, JAVA_TD_RTTI)
      case object outlinedCohenDisplay extends AddrField(RTConst.InstanceDescriptor.cohenDisplay.offset + RTConst.CohenDisplay.outlined.offset, JAVA_TD_OUTLINED_COHEN)
      case object cohenLevel extends IntField(RTConst.InstanceDescriptor.cohenDisplay.offset + RTConst.CohenDisplay.level.offset, JAVA_TD_COHEN_LEVEL)

      case class inlinedCohenDesc(inheritanceLevel: Int) extends AddrField(RTConst.InstanceDescriptor.cohenDisplay.offset + RTConst.CohenDisplay.inlined.offset + addressSize * (inheritanceLevel - 1), JAVA_TD_INLINED_COHEN)
      case class outlinedCohenDesc(inheritanceLevel: Int) extends AddrField(addressSize * (inheritanceLevel - RTConst.CohenDisplay.INLINED_SIZE.intValue - 1), JAVA_TD_OUTLINED_COHEN_DESC)
    }

    case object CangjieInstanceDescriptor extends RTStruct {
      case object imtSlots extends AddrField(RTConst.CangjieInstanceDescriptor.IMT_SLOTS_OFFSET.intValue, CANGJIE_TD_IMT_SLOTS)
      case class virtualMethod(vnum: Int) extends AddrField(RTConst.CangjieInstanceDescriptor.VMT_OFFSET.intValue + addressSize * vnum, CANGJIE_TD_VMT)
    }

    case object ManagedInstanceDescriptor extends RTStruct {
      case object imtSlots extends AddrField(RTConst.ManagedInstanceDescriptor.IMT_SLOTS_OFFSET.intValue, MANAGED_TD_IMT_SLOTS)
      case class virtualMethod(vnum: Int) extends AddrField(RTConst.ManagedInstanceDescriptor.VMT_OFFSET.intValue + addressSize * vnum, MANAGED_TD_VMT)
    }

    case object JavaInstanceDescriptor extends RTStruct {
      case object imtSlots extends AddrField(RTConst.JavaInstanceDescriptor.IMT_SLOTS_OFFSET.intValue, JAVA_TD_IMT_SLOTS)
      case class virtualMethod(vnum: Int) extends AddrField(RTConst.JavaInstanceDescriptor.VMT_OFFSET.intValue + addressSize * vnum, JAVA_TD_VMT)

      case object arrayBaseType extends AddrField(RTConst.JavaInstanceDescriptor.arrayBaseType.offset, JAVA_TD_ARRAY_BASE_TYPE)
    }

    case object ScalaInstanceDescriptor extends RTStruct {
      case object imtSlots extends AddrField(RTConst.ScalaInstanceDescriptor.IMT_SLOTS_OFFSET.intValue, SCALA_TD_IMT_SLOTS)
      case class virtualMethod(vnum: Int) extends AddrField(RTConst.ScalaInstanceDescriptor.VMT_OFFSET.intValue + addressSize * vnum, SCALA_TD_VMT)

      case object arrayBaseType extends AddrField(RTConst.ScalaInstanceDescriptor.arrayBaseType.offset, SCALA_TD_ARRAY_BASE_TYPE)
    }

    case object AJArrayInstanceDescriptor extends RTStruct {
      case object elemSize extends IntField(RTConst.AJArrayInstanceDescriptor.elemSize.offset)
    }

    case object ExecEnv extends RTStruct {
      case object memoryManagerData extends FlatField(RTConst.ExecEnv.memoryManagerData.offset)
      case object nativeWrapperFrameAddr extends AddrField(RTConst.ExecEnv.nativeWrapperFrameAddr.offset)
      case object safeSectionEntranceFrameAddr extends AddrField(RTConst.ExecEnv.safeSectionEntranceFrameAddr.offset)
      case object safeRegionEntranceOffset extends IntField(RTConst.ExecEnv.safeRegionEntranceOffset.offset)
      case object stackDescriptor extends AddrField(RTConst.ExecEnv.currentStackDescriptor.offset, EXEC_ENV_STACK_DESCRIPTOR)
      case object threadEnv extends AddrField(RTConst.ExecEnv.Offsets.threadEnv.intValue, EXEC_ENV_THREAD_ENV)
      case object appendixArgumentOfHookInvoker extends AddrField(RTConst.ExecEnv.appendixArgumentOfHookInvoker.offset)
    }

    case object ThreadEnv extends RTStruct {
      case object exceptionContext extends FlatField(RTConst.ThreadEnv.exceptionContext.offset)
    }

    case object ExceptionContext extends RTStruct {
      case object pendingException extends AddrField(RTConst.ExceptionContext.pendingExceptionObj.offset)
    }

    case object InterfCast1LCache extends RTStruct {
      case object ciao extends AddrIntField(RTConst.InterfCast1LCache.ciao.offset)
      case object imt extends AddrField(RTConst.InterfCast1LCache.imt.offset)
    }

    case object MethodTable extends RTStruct {
      case class virtualMethod(vnum: Int) extends AddrField(addressSize * vnum, IMT)
    }

    case object ThreadLocalMMData extends RTStruct {
      case object gcPointsTLD extends FlatField(RTConst.ThreadLocalMMData.gcPointsTLD.offset)
    }

    case object GCPointsThreadLocalData extends RTStruct {
      case object gcSafetyState extends ByteField(RTConst.GCPoints.ThreadLocalData.gcSafetyState.offset)
      case object gcPointTrapAddress extends AddrField(RTConst.GCPoints.ThreadLocalData.gcPointTrapAddressUnion.offset)
    }

    case object ThinObj extends RTStruct {
      case object td extends AddrField(RTConst.ThinObj.td.offset, THIN_OBJ_TD)
      case object fields extends FlatField(RTConst.ThinObj.fields.offset)
    }

    case object ThinTypeHandle extends RTStruct {
      case object cohenLevel extends IntField(RTConst.ThinTypeHandle.level.offset, THIN_TD_COHEN_LEVEL)

      case class virtualMethod(vnum: Int) extends AddrField(RTConst.ThinTypeHandle.vmt.offset + addressSize * vnum, THIN_TD_VMT)

      case class cohenDesc(inheritanceLevel: Int) extends AddrField(- (addressSize * inheritanceLevel), THIN_TD_COHEN) {
        assert(inheritanceLevel > 0)
      }
    }

    case object Allocator extends SymRTClass(RTConst.Allocator.className) {
      def newObject(size: Int)        = method("newObject" + size)
      def newObjectInlined(size: Int) = method("newObjectInlined" + size)
    }

    case object JavaAllocator extends SymRTClass(RTConst.JavaAllocator.className) {
      def newArrayInlined = method("newArrayInlined")
      def newArrayCopyInlined = method("newArrayCopyInlined")

      def newArrayCopyOf(allocType: symlevel.Type, isCopyOfRange: Boolean, inlined: Boolean = false) = {
        val prefix = if (allocType.getArrayElemType.isPrimitive) "Prim" else "Ref"
        val suffix = if (isCopyOfRange) "Range" else ""
        val inlinedSuffix = if (inlined) "Inlined" else ""
        method("new" + prefix + "ArrayCopyOf" + suffix + inlinedSuffix)
      }
    }

    case object WriteBarriers extends SymRTClass(RTConst.WriteBarriers.className) {
      def instance = methodRef("writeBarrier_instance_opt")
      def static   = methodRef("writeBarrier_static_opt")
      def record   = methodRef("writeBarrier_record_opt")
    }

    case object EscapeWriteBarriers extends SymRTClass("com/huawei/excelsior/jet/runtime/memory/gc/top/EscapeWriteBarriers") {
      def instance = method("instanceWriteBarrier_inlined")
      def static   = method("staticWriteBarrier_inlined")
      def alwaysGlobalObject = method("alwaysGlobalObject")
      def alwaysLocalObject  = method("alwaysLocalObject")
    }

    case object DebugPrint extends SymRTClass("com/huawei/excelsior/jet/runtime/os/DebugPrint") {
      lazy val safePrintf = methodRef("__aj__safePrintf__Lcom_huawei_excelsior_aj_util_BString_2_3Ljava_lang_Object_2__V")
    }

    case object FloatArithOp extends SymRTClass("com/huawei/excelsior/jet/runtime/javalib/FloatArithOp") {
      def procFor(target: RTSProc) = method((target.toString ensuring {_.startsWith("JR_")}).substring(3))
    }

    case object Synchronization extends SymRTClass("com/huawei/excelsior/jet/runtime/thread/sync/Synchronization") {
      lazy val monitorEnterInlined   = method("JR_MonitorEnter_inlined")
      lazy val monitorExitInlined    = method("JR_MonitorExit_inlined")
    }

    case object IFaceOps extends SymRTClass("com/huawei/excelsior/jet/runtime/javalib/IFaceOps") {
      def cast(inlined: Boolean)        = method("cast"     + (if (inlined) "Inlined" else ""))
      def instanceOf(inlined: Boolean)  = method("instof"   + (if (inlined) "Inlined" else ""))

      lazy val weakCast        = method("weakCast")
      lazy val weakCastNoCache = method("weakCastNoCache")
      lazy val getEnrichment   = method("getEnrichment")
      lazy val enrich          = method("enrich")
    }

    case object FieldOffsetAccessor extends SymRTClass("com/huawei/excelsior/jet/runtime/javalib/FieldOffsetAccessor") {
      def fieldOffset(primitive: Boolean, interface: Boolean) = {
        val name = "get" +
          (if (primitive) "Primitive" else "Reference" + (if (interface) "Interface" else "NonInterface")) +
          "FieldOffset"
        methodRef(name)
      }
    }

    case object KeyStrings extends SymRTClass("com/huawei/excelsior/jet/runtime/javalib/java/lang/KeyStrings") {
      lazy val newKeyString0 = methodOrNull("newKeyString0")
    }

    case object Arraycopy extends SymRTClass("com/huawei/excelsior/jet/runtime/javalib/java/lang/Arraycopy") {
      lazy val arraycopy = methodOrNull("arraycopy")
    }

    case object AJStandardExceptions extends SymRTClass("com/huawei/excelsior/jet/runtime/excepts/AJStandardExceptions") {
      lazy val throwNPE = methodOrNull("throwNullPointerException", MSig()(V))
    }

    case object JavaStandardExceptions extends SymRTClass("com/huawei/excelsior/jet/runtime/excepts/JavaStandardExceptions") {
      lazy val throwNPE = methodOrNull("throwNullPointerException", MSig()(V))
    }

    case object StarvationPrevention extends SymRTClass("com/huawei/excelsior/jet/runtime/thread/fibers/scheduling/StarvationPrevention") {
      lazy val incHeldLocks = methodOrNull("JR_incHeldLocks_inlined")
      lazy val decHeldLocks = methodOrNull("JR_decHeldLocks_inlined")
    }
  }


  object Com {

    object Huawei {

      object Excelsior {

        object Aj {

          object Internal {

            case object AtomicIntrinsics extends SymRTClass("com/huawei/excelsior/aj/internal/AtomicIntrinsics") {
              lazy val fetchAndByte = methodOrNull("__aj__fetchAnd__Lcom_huawei_excelsior_aj_lang_Address_2B__B")
              lazy val fetchOrByte  = methodOrNull("__aj__fetchOr__Lcom_huawei_excelsior_aj_lang_Address_2B__B")
              lazy val fetchXorByte = methodOrNull("__aj__fetchXor__Lcom_huawei_excelsior_aj_lang_Address_2B__B")
              lazy val fetchAddByte = methodOrNull("__aj__fetchAdd__Lcom_huawei_excelsior_aj_lang_Address_2B__B")
              lazy val fetchMinByte = methodOrNull("__aj__fetchMin__Lcom_huawei_excelsior_aj_lang_Address_2B__B")
              lazy val fetchMaxByte = methodOrNull("__aj__fetchMax__Lcom_huawei_excelsior_aj_lang_Address_2B__B")
              lazy val swapByte      = methodOrNull("__aj__swap__Lcom_huawei_excelsior_aj_lang_Address_2B__B")

              lazy val fetchAndShort = methodOrNull("__aj__fetchAnd__Lcom_huawei_excelsior_aj_lang_Address_2S__S")
              lazy val fetchOrShort  = methodOrNull("__aj__fetchOr__Lcom_huawei_excelsior_aj_lang_Address_2S__S")
              lazy val fetchXorShort = methodOrNull("__aj__fetchXor__Lcom_huawei_excelsior_aj_lang_Address_2S__S")
              lazy val fetchAddShort = methodOrNull("__aj__fetchAdd__Lcom_huawei_excelsior_aj_lang_Address_2S__S")
              lazy val fetchMinShort = methodOrNull("__aj__fetchMin__Lcom_huawei_excelsior_aj_lang_Address_2S__S")
              lazy val fetchMaxShort = methodOrNull("__aj__fetchMax__Lcom_huawei_excelsior_aj_lang_Address_2S__S")
              lazy val swapShort = methodOrNull("__aj__swap__Lcom_huawei_excelsior_aj_lang_Address_2S__S")

              lazy val fetchAndInt = methodOrNull("__aj__fetchAnd__Lcom_huawei_excelsior_aj_lang_Address_2I__I")
              lazy val fetchOrInt  = methodOrNull("__aj__fetchOr__Lcom_huawei_excelsior_aj_lang_Address_2I__I")
              lazy val fetchXorInt = methodOrNull("__aj__fetchXor__Lcom_huawei_excelsior_aj_lang_Address_2I__I")
              lazy val fetchAddInt = methodOrNull("__aj__fetchAdd__Lcom_huawei_excelsior_aj_lang_Address_2I__I")
              lazy val fetchMinInt = methodOrNull("__aj__fetchMin__Lcom_huawei_excelsior_aj_lang_Address_2I__I")
              lazy val fetchMaxInt = methodOrNull("__aj__fetchMax__Lcom_huawei_excelsior_aj_lang_Address_2I__I")
              lazy val swapInt = methodOrNull("__aj__swap__Lcom_huawei_excelsior_aj_lang_Address_2I__I")

              lazy val fetchAndLong = methodOrNull("__aj__fetchAnd__Lcom_huawei_excelsior_aj_lang_Address_2J__J")
              lazy val fetchOrLong  = methodOrNull("__aj__fetchOr__Lcom_huawei_excelsior_aj_lang_Address_2J__J")
              lazy val fetchXorLong = methodOrNull("__aj__fetchXor__Lcom_huawei_excelsior_aj_lang_Address_2J__J")
              lazy val fetchAddLong = methodOrNull("__aj__fetchAdd__Lcom_huawei_excelsior_aj_lang_Address_2J__J")
              lazy val fetchMinLong = methodOrNull("__aj__fetchMin__Lcom_huawei_excelsior_aj_lang_Address_2J__J")
              lazy val fetchMaxLong = methodOrNull("__aj__fetchMax__Lcom_huawei_excelsior_aj_lang_Address_2J__J")
              lazy val swapLong = methodOrNull("__aj__swap__Lcom_huawei_excelsior_aj_lang_Address_2J__J")

              lazy val compareAndSwapByte  = methodOrNull("__aj__compareAndSwap__Lcom_huawei_excelsior_aj_lang_Address_2BB__B")
              lazy val compareAndSwapShort = methodOrNull("__aj__compareAndSwap__Lcom_huawei_excelsior_aj_lang_Address_2SS__S")
              lazy val compareAndSwapInt   = methodOrNull("__aj__compareAndSwap__Lcom_huawei_excelsior_aj_lang_Address_2II__I")
              lazy val compareAndSwapLong  = methodOrNull("__aj__compareAndSwap__Lcom_huawei_excelsior_aj_lang_Address_2JJ__J")
            }

          }

          object Lang {
            case object AJNullPointerException extends SymRTClass("com/huawei/excelsior/aj/lang/AJNullPointerException") {
              lazy val init = method("<init>", MSig()(V))
            }

            case object UnmanagedMath extends SymRTClass("com/huawei/excelsior/aj/lang/UnmanagedMath") {
              lazy val bitCountInt  = methodOrNull("bitCount", MSig(Int32)(Int32))
              lazy val bitCountLong = methodOrNull("bitCount", MSig(Int64)(Int32))
            }

            case object Half extends SymRTClass("com/huawei/excelsior/aj/lang/Half") {
              lazy val h2f = methodOrNull("__aj__softConvert__Lcom_huawei_excelsior_aj_lang_Half_2__F", MSig(Int16)(Float32))
              lazy val f2h = methodOrNull("__aj__softConvert__F__Lcom_huawei_excelsior_aj_lang_Half_2", MSig(Float32)(Int16))
            }

            case object AJString extends SymRTClass("com/huawei/excelsior/aj/lang/AJString") {
              lazy val value = field("value")
            }

          }
        }

        object Util {
          case object MemoryBarrier extends SymRTClass("com/huawei/excelsior/aj/util/MemoryBarrier") {
            lazy val L_L = methodOrNull("L_L", MSig()(V))
            lazy val L_S = methodOrNull("L_S", MSig()(V))
            lazy val S_L = methodOrNull("S_L", MSig()(V))
            lazy val S_S = methodOrNull("S_S", MSig()(V))
          }
        }
      }

    }

  }

  object Cangjie {

    object Support {

      // TODO: JET-15710 Cangjie rtexports
      case object ArraySlice extends SymRTClass(CangjieSymLevelMaker.ARRAY_SLICE_NAME) {
        lazy val base = field("base")
        lazy val start = field("start")
        lazy val size = field("size")
      }

      case object String extends SymRTClass(CangjieSymLevelMaker.STD_CORE_STRING_NAME) {
        lazy val myData = field("myData")
      }
    }

    object Std {

      object Core {

        case object NoneValueException extends SymRTClass(CangjieSymLevelMaker.STD_CORE_NONE_VALUE_EXCEPTION_NAME) {
          lazy val init = methodOrNull("<init>", MSig()(SignatureType.Void))
        }
      }
    }
  }


  sealed abstract class JBCBoxType(val name: String, val kind: BytecodeTypeKind) extends SymRTClass(name) {
    lazy val valueOf = method("valueOf", MSig(SignatureType.Primitive(kind))(SignatureType.JBCReference(name)))
    lazy val value = field("value")
  }

  object Scala {

    object Runtime {

      object Support {

        sealed abstract class LazyType(val name: String, kind: BytecodeTypeKind) extends SymRTClass(name) {
          lazy val _lock = field("_lock")
        }

        object LazyType {
          import Scala.Runtime.Lazy.*
          @nowarn("msg=match may not be exhaustive")
          def apply(kind: BytecodeTypeKind): LazyType = kind match {
            case BytecodeTypeKind.CLASS   => LazyRef
            case BytecodeTypeKind.BOOLEAN => LazyBoolean
            case BytecodeTypeKind.BYTE    => LazyByte
            case BytecodeTypeKind.SHORT   => LazyShort
            case BytecodeTypeKind.CHAR    => LazyChar
            case BytecodeTypeKind.INT     => LazyInt
            case BytecodeTypeKind.LONG    => LazyLong
            case BytecodeTypeKind.FLOAT   => LazyFloat
            case BytecodeTypeKind.DOUBLE  => LazyDouble
            case BytecodeTypeKind.VOID    => LazyUnit
          }

          def unapply(t: symlevel.Type): Option[LazyType] = {
            if (!t.isXScalaType) return None

            condOpt(t.getName) {
              case LazyRef.name     => LazyRef
              case LazyBoolean.name => LazyBoolean
              case LazyByte.name    => LazyByte
              case LazyShort.name   => LazyShort
              case LazyChar.name    => LazyChar
              case LazyInt.name     => LazyInt
              case LazyLong.name    => LazyLong
              case LazyFloat.name   => LazyFloat
              case LazyDouble.name  => LazyDouble
              case LazyUnit.name    => LazyUnit
            }
          }
        }
      }

      object Lazy {

        import Scala.Runtime.Support.LazyType

        case object LazyRef     extends LazyType("scala/runtime/LazyRef",     BytecodeTypeKind.CLASS)
        case object LazyBoolean extends LazyType("scala/runtime/LazyBoolean", BytecodeTypeKind.BOOLEAN)
        case object LazyByte    extends LazyType("scala/runtime/LazyByte",    BytecodeTypeKind.BYTE)
        case object LazyChar    extends LazyType("scala/runtime/LazyChar",    BytecodeTypeKind.CHAR)
        case object LazyShort   extends LazyType("scala/runtime/LazyShort",   BytecodeTypeKind.SHORT)
        case object LazyInt     extends LazyType("scala/runtime/LazyInt",     BytecodeTypeKind.INT)
        case object LazyLong    extends LazyType("scala/runtime/LazyLong",    BytecodeTypeKind.LONG)
        case object LazyFloat   extends LazyType("scala/runtime/LazyFloat",   BytecodeTypeKind.FLOAT)
        case object LazyDouble  extends LazyType("scala/runtime/LazyDouble",  BytecodeTypeKind.DOUBLE)
        case object LazyUnit    extends LazyType("scala/runtime/LazyUnit",    BytecodeTypeKind.VOID)
      }
    }
  }

  object XScala {

    object Support {

      sealed abstract class BoxType(name: String, kind: BytecodeTypeKind) extends JBCBoxType(name, kind)

      object BoxType {
        import XScala.Boxing.*
        @nowarn("msg=match may not be exhaustive")
        def apply(kind: BytecodeTypeKind): BoxType = kind match {
          case BytecodeTypeKind.BOOLEAN => BoxedBoolean
          case BytecodeTypeKind.BYTE    => BoxedByte
          case BytecodeTypeKind.SHORT   => BoxedShort
          case BytecodeTypeKind.CHAR    => BoxedChar
          case BytecodeTypeKind.INT     => BoxedInt
          case BytecodeTypeKind.LONG    => BoxedLong
          case BytecodeTypeKind.FLOAT   => BoxedFloat
          case BytecodeTypeKind.DOUBLE  => BoxedDouble
        }

        def unapply(t: symlevel.Type): Option[BoxType] = {
          if (!t.isXScalaType) return None

          condOpt(t.getName) {
            case BoxedBoolean.name => BoxedBoolean
            case BoxedByte.name => BoxedByte
            case BoxedShort.name => BoxedShort
            case BoxedChar.name => BoxedChar
            case BoxedInt.name => BoxedInt
            case BoxedLong.name => BoxedLong
            case BoxedFloat.name => BoxedFloat
            case BoxedDouble.name => BoxedDouble
          }
        }
      }
    }

    object Boxing {

      import XScala.Support.BoxType

      case object BoxedBoolean extends BoxType("xscala/boxing/BoxedBoolean", BytecodeTypeKind.BOOLEAN)
      case object BoxedByte    extends BoxType("xscala/boxing/BoxedByte",    BytecodeTypeKind.BYTE)
      case object BoxedShort   extends BoxType("xscala/boxing/BoxedShort",   BytecodeTypeKind.SHORT)
      case object BoxedChar    extends BoxType("xscala/boxing/BoxedChar",    BytecodeTypeKind.CHAR)
      case object BoxedInt     extends BoxType("xscala/boxing/BoxedInt",     BytecodeTypeKind.INT)
      case object BoxedLong    extends BoxType("xscala/boxing/BoxedLong",    BytecodeTypeKind.LONG)
      case object BoxedFloat   extends BoxType("xscala/boxing/BoxedFloat",   BytecodeTypeKind.FLOAT)
      case object BoxedDouble  extends BoxType("xscala/boxing/BoxedDouble",  BytecodeTypeKind.DOUBLE)
    }

    case object AnyRef extends SymRTClass("xscala/AnyRef") {
      lazy val init = methodOrNull("<init>", MSig()(V))
    }

    object Sync {
      case object LockJET extends SymRTClass("xscala/sync/LockJET") {
        lazy val lock = field("lock")
      }
    }
  }


  // TODO: rebrand SymRTClass and RTStructs (and someday... kill CacheAPI)
  // TODO: make packages?
  object Java {

    object Support {

      sealed abstract class BoxType(name: String, kind: BytecodeTypeKind) extends JBCBoxType(name, kind)

      object BoxType {
        import Java.Lang.*
        @nowarn("msg=match may not be exhaustive")
        def apply(kind: BytecodeTypeKind): BoxType = kind match {
          case BytecodeTypeKind.BOOLEAN => Boolean
          case BytecodeTypeKind.BYTE    => Byte
          case BytecodeTypeKind.SHORT   => Short
          case BytecodeTypeKind.CHAR    => Character
          case BytecodeTypeKind.INT     => Integer
          case BytecodeTypeKind.LONG    => Long
          case BytecodeTypeKind.FLOAT   => Float
          case BytecodeTypeKind.DOUBLE  => Double
        }

        def unapply(t: symlevel.Type): Option[BoxType] = condOpt(t.getName) {
          case Boolean.name   => Boolean
          case Byte.name      => Byte
          case Short.name     => Short
          case Character.name => Character
          case Integer.name   => Integer
          case Long.name      => Long
          case Float.name     => Float
          case Double.name    => Double
        }
      }
    }

    object Lang {

      case object Object extends SymRTClass("java/lang/Object") {
        lazy val _getClass = methodOrNull("getClass")
      }

      case object Class extends SymRTClass("java/lang/Class") {
        lazy val isAssignableFrom = methodOrNull("isAssignableFrom")
        lazy val isInstance = methodRef("isInstance", aKind = MethodReferenceAccessKind.SPECIAL)
      }

      case object String extends SymRTClass("java/lang/String") {
        lazy val value = field("value")

        lazy val hashCodeMethod = methodOrNull("hashCode")
        lazy val equalsMethod = methodOrNull("equals")
        lazy val endsWith = methodOrNull("endsWith")

        lazy val startsWith = methodOrNull("startsWith", MSig(javaLangString)(SignatureType.Boolean))
        lazy val startsWithFrom = methodOrNull("startsWith", MSig(javaLangString, Int32)(SignatureType.Boolean))

        lazy val indexOfChar = methodOrNull("indexOf", MSig(Int32)(Int32))
        lazy val indexOfStr = methodOrNull("indexOf", MSig(javaLangString)(Int32))
        lazy val indexOfCharFrom = methodOrNull("indexOf", MSig(Int32, Int32)(Int32))
        lazy val indexOfStrFrom = methodOrNull("indexOf", MSig(javaLangString, Int32)(Int32))

        lazy val lastIndexOfChar = methodOrNull("lastIndexOf", MSig(Int32)(Int32))
        lazy val lastIndexOfStr = methodOrNull("lastIndexOf", MSig(javaLangString)(Int32))
        lazy val lastIndexOfCharFrom = methodOrNull("lastIndexOf", MSig(Int32, Int32)(Int32))
        lazy val lastIndexOfStrFrom = methodOrNull("lastIndexOf", MSig(javaLangString, Int32)(Int32))
      }

      object Invoke {

        case object LambdaForm extends SymRTClass("java/lang/invoke/LambdaForm") {
          lazy val vmentry = field("vmentry")
        }

        case object MethodHandle extends SymRTClass("java/lang/invoke/MethodHandle") {
          lazy val form = field("form")
        }

        case object MemberName extends SymRTClass("java/lang/invoke/MemberName") {
          lazy val entryPoint = field("entryPoint")  // JET-specific
        }
      }

      case object NullPointerException extends SymRTClass("java/lang/NullPointerException") {
        lazy val init = methodOrNull("<init>", MSig()(V))
      }

      // TODO: refactor all about intrinsics identification
      case object Math extends SymRTClass(if (languagePack == LanguagePack.SCALA) "xscala/Math" else "java/lang/Math")

      enum MathIntrinsic(val rtProc: RTSProc) {
        case D_SIN extends MathIntrinsic(RTSProc.JR_sin)
        case D_COS extends MathIntrinsic(RTSProc.JR_cos)
        case D_TAN extends MathIntrinsic(RTSProc.JR_tan)
        case D_ASIN extends MathIntrinsic(RTSProc.JR_asin)
        case D_ACOS extends MathIntrinsic(RTSProc.JR_acos)
        case D_ATAN extends MathIntrinsic(RTSProc.JR_atan)
        case D_EXP extends MathIntrinsic(RTSProc.JR_exp)
        case D_LOG extends MathIntrinsic(RTSProc.JR_log)
        case D_SQRT extends MathIntrinsic(RTSProc.JR_sqrt)
        case F_SQRT extends MathIntrinsic(RTSProc.JR_sqrtf)
        case D_CEIL extends MathIntrinsic(RTSProc.JR_ceil)
        case D_FLOOR extends MathIntrinsic(RTSProc.JR_floor)
        case D_RINT extends MathIntrinsic(RTSProc.JR_rint)
        case D_ABS extends MathIntrinsic(RTSProc.JR_dabs)
        case F_ABS extends MathIntrinsic(RTSProc.JR_fabs)
        case D_ATAN2 extends MathIntrinsic(RTSProc.JR_atan2)
        case D_POW extends MathIntrinsic(RTSProc.JR_pow)
        case D_REM1 extends MathIntrinsic(RTSProc.JR_rem1)
        case D_REM extends MathIntrinsic(RTSProc.JR_drem)
        case F_REM extends MathIntrinsic(RTSProc.JR_frem)

        private def hasMethodInJLMath: Boolean = this match {
          case D_REM | F_REM | F_SQRT => false
          case _ => true
        }

        def typeKind: symlevel.TypeKind = this match {
          case F_ABS | F_REM | F_SQRT => FLOAT
          case _ => DOUBLE
        }

        def argsCount: Int = this match {
          case D_ATAN2 | D_POW | D_REM1 | D_REM | F_REM => 2
          case _ => 1
        }

        def isUnary: Boolean = argsCount == 1

        def isBinary: Boolean = argsCount == 2
      }

      object MathIntrinsic {
        private lazy val method2itr = {
          val map = mutable.HashMap.empty[Method, MathIntrinsic]
          for (intrinsic <- values if intrinsic.hasMethodInJLMath) {
            val name = intrinsic match {
              case D_REM1 => "IEEEremainder"
              case _ => intrinsic.toString.substring(2).asciiToLowerCase
            }
            val sigType = SignatureType.Primitive(intrinsic.typeKind)
            val sig = MSig(Seq.fill(intrinsic.argsCount)(sigType) *)(sigType)
            map(Math.method(name, sig)) = intrinsic
          }
          map
        }

        def unapply(method: Method) = method2itr.get(method)
      }

      import Java.Support.BoxType

      case object Boolean   extends BoxType("java/lang/Boolean",   BytecodeTypeKind.BOOLEAN)
      case object Byte      extends BoxType("java/lang/Byte",      BytecodeTypeKind.BYTE)
      case object Short     extends BoxType("java/lang/Short",     BytecodeTypeKind.SHORT)
      case object Character extends BoxType("java/lang/Character", BytecodeTypeKind.CHAR)
      case object Integer   extends BoxType("java/lang/Integer",   BytecodeTypeKind.INT)
      case object Long      extends BoxType("java/lang/Long",      BytecodeTypeKind.LONG)
      case object Float     extends BoxType("java/lang/Float",     BytecodeTypeKind.FLOAT)
      case object Double    extends BoxType("java/lang/Double",    BytecodeTypeKind.DOUBLE)
    }
  }
}
