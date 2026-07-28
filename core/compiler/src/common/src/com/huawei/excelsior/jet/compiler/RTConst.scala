/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.RTConst.{Host, resolver}

abstract class RTConst(val host: RTConst.Host) {

  def fieldName: String = toString

  def boolValue: Boolean = intValue != 0
  def intValue: Int = resolver.intValue(this)
  def longValue: Long = resolver.longValue(this)
  def addrValue: Long = resolver.addrValue(this)

  def offset: Int = resolver.offset(this)
}

object RTConst {
  object CPUFeature extends Host("com/huawei/excelsior/jet/runtime/arch/CPUFeature")
  enum CPUFeature extends RTConst(CPUFeature) {
    case POPCNT
    case F16C
  }

  object XTable extends Host("com/huawei/excelsior/jet/runtime/typedesc/XTable") {

    object State extends Host(XTable, "State") {
      object Initial extends Host(State, "Initial")
      enum Initial extends RTConst(Initial) {
        case XREGION_START
        case HANDLER_OFFSET
        case INLINE_LIST_HEAD
        case GCMAP_LENGTH
        case VNUM
        case RECEIVER_INDEX
        case REF_CLASS_INDEX
        case BYTECODE_POS
        case LINE_NUMBER
        case MARKED_REGION_ID
        case SOFT_EXCEPTION_ID
      }
    }

    object Command extends Host(XTable, "Command")
    enum Command extends RTConst(Command) {
      case BLOCK_END
      case HANDLER_OFFSET_DIFF
      case NO_HANDLER
      case INLINE_LIST_HEAD
      case NO_INLINE
      case RECEIVER_INDEX
      case VNUM
      case UNKNOWN_VNUM
      case VCALL
      case REF_CLASS_INDEX
      case ICALL
      case FIND_BLOCK
      case INLINE_LIST
      case GCMAP
      case GCMAP_LENGTH_DIFF
      case BYTECODE_POS
      case LINE_NUMBER
      case MNCALL
      case MARKED_REGION_ID
      case NO_MARKED_REGION_ID
      case SOFT_EXCEPTION_ID
      case NO_SOFT_EXCEPTION_ID
      case DOMAIN

      case MAX_CODE
      case XREGION_START_DIFF_BASE
      case GCMAP_LENGTH_SMALL_DIFF_BASE
      case GCMAP_LENGTH_SMALL_DIFF_MAX
    }
  }

  enum XTable extends RTConst(XTable) {
    case BLOCK_SIZE
    case BLOCK_ALIGNMENT
    case ALIGNMENT
  }

  object FrameDescriptor extends Host("com/huawei/excelsior/jet/runtime/excepts/FrameDescriptor") {
    object Code extends Host(FrameDescriptor, "Code")
    enum Code extends RTConst(Code) {
      case UNINITIALIZED_FD
      case CTMW_FD
      case STUB_FD
      case MC_FD
      case GC_AWARE_NOT_PREPARED_FD
      case GC_AWARE_PREPARED_FD
      case GC_POINT_TRAP_HANDLER_FD
      case MANUAL_INTERNAL_FD
      case MAX_SPECIAL_GUEST_FD_CODE
      case MIN_SPECIAL_GUEST_FD_CODE
    }

    object AtomicCode extends Host(FrameDescriptor, "AtomicCode")
  }
  enum FrameDescriptor extends RTConst(FrameDescriptor) {
    case code
  }

  object MethodAndTypeInfoFrameDescriptor extends Host("com/huawei/excelsior/jet/runtime/excepts/MethodAndTypeInfoFrameDescriptor")

  object VersionedMethodFrameDescriptor extends Host("com/huawei/excelsior/jet/runtime/excepts/VersionedMethodFrameDescriptor")

  object MethodInfoFrameDescriptor extends Host("com/huawei/excelsior/jet/runtime/excepts/MethodInfoFrameDescriptor")
  enum MethodInfoFrameDescriptor extends RTConst(MethodInfoFrameDescriptor) {
    case CODE_ALIGNMENT
    case NO_EXCEPTION_HANDLER_AS_OFFSET
    case NO_INLINE
    case UNKNOWN_SIBERIA_OFFSET

    case LIGHTWEIGHT_FRAME_BIT
    case FRAME_OF_HOOK_INVOKER_FLAG_BIT
    case IS_VERSIONED_FLAG_BIT
    case IS_INTERPRETER_INTERNALS_FLAG_BIT
    case WITH_SIBERIA_OFFSET_BIT
    case HAS_MARKED_REGIONS_FLAG_BIT
    case IS_DYN_LOADED_FLAG_BIT
    case DIRTY_FOR_CLASS_GC_FRAME_BIT
    case IS_CBC_BIT

    case xTableOffset
    case inlineListOffset
    case gcMapsOffset
    case packedFDInfo
  }

  object MarkableFrameDescriptor extends Host("com/huawei/excelsior/jet/runtime/excepts/MarkableFrameDescriptor")
  enum MarkableFrameDescriptor extends RTConst(MarkableFrameDescriptor) {
    case FOLLOWUP_HIT_MARK_BIT
    case FOLLOWUP_COUNT_MARK_BIT
    case PROFILER_MARK_BITS
    case FOLLOWUP_HIT_MARK_MASK
    case FOLLOWUP_COUNT_MARK_MASK
    case PROFILER_MARKS_MASK
  }

  object HookInvokerFrameDescriptor extends Host("com/huawei/excelsior/jet/runtime/excepts/HookInvokerFrameDescriptor")

  object CallToManagedWrapperFrameDescriptor extends Host("com/huawei/excelsior/jet/runtime/excepts/CallToManagedWrapperFrameDescriptor")
  enum CallToManagedWrapperFrameDescriptor extends RTConst(CallToManagedWrapperFrameDescriptor) {
    case nativeWrapperFrameAddr
    case savedThreadContext
  }

  object InlineList extends Host("com/huawei/excelsior/jet/runtime/excepts/InlineList") {

    object Format extends Host(InlineList, "Format")
    enum Format extends RTConst(Format) {
      case HAS_METHOD_BIT
      case HAS_BCPOS_BIT
      case HAS_LINES_BIT
    }

    object Head extends Host(InlineList, "Head")
    enum Head extends RTConst(Head) {
      case NO_INLINED_METHODS
    }

    object Element extends Host(InlineList, "Element") {
      object Markers extends Host(Element, "Markers")
      enum Markers extends RTConst(Markers) {
        case REFLECT_METHOD_INVOKE
      }
    }

    object Iterator extends Host(InlineList, "Iterator")
    enum Iterator extends RTConst(Iterator) {
      case INLINE_INDEX_ADDEND
      case INLINE_ENTRY_MARKERS
      case INLINE_END
    }

    object Cache extends Host(InlineList, "Cache")
    enum Cache extends RTConst(Cache) {
      case EMPTY
    }
  }

  enum InlineList extends RTConst(InlineList) {
    case data
  }

  object StackOverflowHandling extends Host("com/huawei/excelsior/jet/runtime/excepts/StackOverflowHandling")
  enum StackOverflowHandling extends RTConst(StackOverflowHandling) {
    case STACK_RESERVE_FOR_MANAGED_METHOD
    case STACK_RESERVE_FOR_RT_METHOD
    case STACK_RESERVE_FOR_RT_SUPPORT
    case ADVANCED
  }

  object VirtualMemory extends Host("com/huawei/excelsior/jet/runtime/os/VirtualMemory")
  enum VirtualMemory extends RTConst(VirtualMemory) {
    case SIGNIFICANT_BITS_FOR_POINTER
    case INSIGNIFICANT_BITS_MASK_FOR_POINTER
    case MIN_PAGE_SIZE
  }

  object NewBaselineExceptionTable extends Host("com/huawei/excelsior/jet/runtime/excepts/NewBaselineExceptionTable") {

    object Entry extends Host(NewBaselineExceptionTable, "Entry")
    enum Entry extends RTConst(Entry) {
      case catchType
      case handlerOffset
      case nextEntry
      case handlerOfHandlerEntry
    }
  }

  enum NewBaselineExceptionTable extends RTConst(NewBaselineExceptionTable) {
    case CATCH_TYPE_ANY
    case INVALID_ENTRY_IDX

    case entries
  }

  object DeferredAccessInfo extends Host("com/huawei/excelsior/jet/runtime/classload/resolve/deferred/dai/DeferredAccessInfo") {
    object InvokeVirtualOrInterface extends Host(DeferredAccessInfo, "InvokeVirtualOrInterface")
    enum InvokeVirtualOrInterface extends RTConst(InvokeVirtualOrInterface) {
      case NO_VNUM
    }
  }

  enum DeferredAccessInfo extends RTConst(DeferredAccessInfo) {
    case REF_KIND_MASK
  }

  object JavaDAI extends Host("com/huawei/excelsior/jet/runtime/classload/resolve/deferred/dai/JavaDAI") {
    object JSR292Appendix extends Host(JavaDAI, "JSR292Appendix")
    enum JSR292Appendix extends RTConst(JSR292Appendix) {
      case appendix
    }
  }

  object KindAndFlags extends Host("com/huawei/excelsior/jet/runtime/classload/resolve/deferred/dai/KindAndFlags"){
    object BitNumber extends Host(KindAndFlags, "BitNumber")
    enum BitNumber extends RTConst(BitNumber) {
      case IS_JIT
      case IS_NON_NULL_RECEIVER
      case HAS_APPENDIX
    }
  }

  enum KindAndFlags extends RTConst(KindAndFlags) {
    case REF_KIND_MASK
  }

  object Attribute extends Host("com/huawei/excelsior/jet/runtime/classload/classfile/Attribute"){
    object Code extends Host(Attribute, "Code")
    enum Code extends RTConst(Code) {
      case CODE_ATTR_ID
      case EXT_ATTR_ID
      case SMT_ATTR_ID
      case LVT_ATTR_ID
      case LNT_ATTR_ID
    }
  }

  enum Attribute extends RTConst(Attribute) {
    case REF_KIND_MASK
  }

  object ComponentDescriptor extends Host("com/huawei/excelsior/jet/runtime/bincomps/ComponentDescriptor")
  enum ComponentDescriptor extends RTConst(ComponentDescriptor) {
    case gcPointsTrapAddress
    case cpuFeatures
  }

  object UsageMask extends Host("com/huawei/excelsior/jet/runtime/features/usagelist/UsageMask")
  enum UsageMask extends RTConst(UsageMask) {
    case USG_DEFAULT
  }

  object ExecEnv extends Host("com/huawei/excelsior/jet/runtime/thread/ExecEnv") {
    object Offsets extends Host(ExecEnv, "Offsets") {
      def altLocation(i: Int): Offsets = i match {
        case 0 => altLocation0
        case 1 => altLocation1
      }
    }

    enum Offsets extends RTConst(Offsets) {
      case threadEnv
      case altLocation0
      case altLocation1
    }

    def gcPointTrapAddressUnionOffset =
      memoryManagerData.offset + ThreadLocalMMData.gcPointsTLD.offset + GCPoints.ThreadLocalData.gcPointTrapAddressUnion.offset
  }

  enum ExecEnv extends RTConst(ExecEnv) {
    case customEEHeader
    case safeRegionEntranceOffset
    case memoryManagerData
    case currentStackDescriptor
    case appendixArgumentOfHookInvoker
    case safeSectionEntranceFrameAddr
    case nativeWrapperFrameAddr
  }

  object JavaExecEnv extends Host("com/huawei/excelsior/jet/runtime/thread/JavaExecEnv")
  enum JavaExecEnv extends RTConst(JavaExecEnv) {
    case JNI_ENV_OFFS

    case localRefsPool
    case nativeCallRefArgsStack
  }

  object ThreadEnv extends Host("com/huawei/excelsior/jet/runtime/thread/ThreadEnv")
  enum ThreadEnv extends RTConst(ThreadEnv) {
    case exceptionContext
  }

  object ExceptionContext extends Host("com/huawei/excelsior/jet/runtime/thread/ExceptionContext")
  enum ExceptionContext extends RTConst(ExceptionContext) {
    case pendingExceptionObj
    case pendingHardwareExceptionCode
    case soeInstantiationCheckRequired
  }

  object ThreadLocalMMData extends Host("com/huawei/excelsior/jet/runtime/memory/jalloc/engines/ThreadLocalMMData")
  enum ThreadLocalMMData extends RTConst(ThreadLocalMMData) {
    case gcPointsTLD
  }

  object SavedThreadContext extends Host("com/huawei/excelsior/jet/runtime/memory/gc/sections/SavedThreadContext")
  enum SavedThreadContext extends RTConst(SavedThreadContext) {
    case savedSafeRegionEntranceOffset
  }

  object JNIReference extends Host("com/huawei/excelsior/jet/runtime/jni/JNIReference")
  enum JNIReference extends RTConst(JNIReference) {
    case WRAPPER_FLAG
  }

  object LocalRefsPool extends Host("com/huawei/excelsior/jet/runtime/jni/LocalRefsPool")
  enum LocalRefsPool extends RTConst(LocalRefsPool) {
    case index
  }

  object NativeCallRefArgs extends Host("com/huawei/excelsior/jet/runtime/util/NativeCallRefArgs")
  enum NativeCallRefArgs extends RTConst(NativeCallRefArgs) {
    case outerCallArgs
    case elemNum
    case elems
  }

  object GCPoints extends Host("com/huawei/excelsior/jet/runtime/memory/gc/GCPoints") {
    object ThreadLocalData extends Host(GCPoints, "ThreadLocalData")
    enum ThreadLocalData extends RTConst(ThreadLocalData) {
      case gcPointTrapAddressUnion
      case gcSafetyState
    }
  }

  enum GCPoints extends RTConst(GCPoints) {
    case usualTrapOffset
    case fastNoInspectionTrapOffset
    case fastWithInspectionTrapOffset
    case epilogueNotReturningRefTrapOffset
    case epilogueReturningRefTrapOffset
  }

  object GCSafetyState extends Host("com/huawei/excelsior/jet/runtime/memory/gc/GCSafetyState")
  enum GCSafetyState extends RTConst(GCSafetyState) {
    case SAFE
    case UNSAFE
    case PROHIBITED
  }

  object ThreadLocalGC extends Host("com/huawei/excelsior/jet/runtime/memory/gc/tlgc/ThreadLocalGC")
  enum ThreadLocalGC extends RTConst(ThreadLocalGC) {
    case TLGC_ENABLED
  }

  object WriteBarriers extends Host("com/huawei/excelsior/jet/runtime/memory/gc/WriteBarriers")
  enum WriteBarriers extends RTConst(WriteBarriers) {
    case WRITE_BARRIERS_ENABLED
  }

  object CompactHeader extends Host("com/huawei/excelsior/jet/runtime/jobject/CompactHeader")
  enum CompactHeader extends RTConst(CompactHeader) {
    case COMPACT_HEADER_ENABLED
    case DEBUG_TSWORD_FIELD_PRESENT
  }

  object Allocator extends Host("com/huawei/excelsior/jet/runtime/memory/jalloc/Allocator")
  enum Allocator extends RTConst(Allocator) {
    case MIN_SIZE_OF_SPECIALIZED_OBJECT
    case MAX_SIZE_OF_SPECIALIZED_OBJECT
  }

  object JavaAllocator extends Host("com/huawei/excelsior/jet/runtime/memory/jalloc/JavaAllocator")

  object SmallAJAllocator extends Host("com/huawei/excelsior/jet/runtime/memory/jalloc/SmallAJAllocator")
  enum SmallAJAllocator extends RTConst(SmallAJAllocator) {
    case MAX_LENGTH_OF_SPECIALIZED_REF_ARRAY
    case MAX_LENGTH_OF_SPECIALIZED_PRIM_ARRAY
    case MAX_DISPATCHED_ARRAY_SIZE
    case ARRAY_DISPATCH_TABLE_SIZE
  }

  object SmallCangjieAllocator extends Host("com/huawei/excelsior/jet/runtime/memory/jalloc/SmallCangjieAllocator")
  enum SmallCangjieAllocator extends RTConst(SmallCangjieAllocator) {
    case MAX_LENGTH_OF_SPECIALIZED_REF_ARRAY
    case MAX_LENGTH_OF_SPECIALIZED_PRIM_ARRAY
    case MAX_DISPATCHED_ARRAY_SIZE
    case ARRAY_DISPATCH_TABLE_SIZE
  }

  object KeyObjects extends Host("com/huawei/excelsior/jet/runtime/memory/jalloc/engines/key/KeyObjects")
  enum KeyObjects extends RTConst(KeyObjects) {
    case KEY_OBJECTS_ALLOCATION_ENABLED
  }

  object SmallAllocConfig extends Host("com/huawei/excelsior/jet/runtime/memory/jalloc/engines/small/SmallAllocConfig")
  enum SmallAllocConfig extends RTConst(SmallAllocConfig) {
    case MAX_SMALL_OBJ_SIZE
  }

  object AllocConfig extends Host("com/huawei/excelsior/jet/runtime/memory/jalloc/AllocConfig")
  enum AllocConfig extends RTConst(AllocConfig) {
    case MIN_LARGE_OBJ_SIZE
  }

  object AllocUnit extends Host("com/huawei/excelsior/jet/runtime/memory/AllocUnit")
  enum AllocUnit extends RTConst(AllocUnit) {
    case SIZE
    case LOG2SIZE
    case FAKE_NORMAL
  }

  object Eop extends Host("com/huawei/excelsior/jet/runtime/jobject/Eop")
  enum Eop extends RTConst(Eop) {
    case ENABLED
    case IDX_LIMIT
    case OFFSET_LIMIT
    case ENRICHMENT_SHIFT
    case ENRICHMENT_MASK
    case INCREMENTED_IDX
    case OFFSET_LIMIT_ADDR
  }

  object HeapObj extends Host("com/huawei/excelsior/jet/runtime/jobject/HeapObj") {
    object TSWord extends Host(HeapObj, "TSWord")
    enum TSWord extends RTConst(TSWord) {
      case SIZE
      case SIZE_MASK
    }
  }

  enum HeapObj extends RTConst(HeapObj) {
    case TYPEDESC_OFFSET
    case TSWORD_OFFSET
    case DEBUG_TSWORD_OFFSET
  }

  object LockableObj extends Host("com/huawei/excelsior/jet/runtime/jobject/LockableObj")
  enum LockableObj extends RTConst(LockableObj) {
    case LOCKWORD_OFFSET
  }

  object JavaObj extends Host("com/huawei/excelsior/jet/runtime/jobject/JavaObj")

  object JavaString extends Host("com/huawei/excelsior/jet/runtime/jobject/JavaString")
  enum JavaString extends RTConst(JavaString) {
    case SIZE
  }

  object JavaArray extends Host("com/huawei/excelsior/jet/runtime/jobject/JavaArray")
  enum JavaArray extends RTConst(JavaArray) {
    case MAX_DIMENSION
    case HEADER_SIZE
    case ARRAY_BODY_OFFS

    case length
  }

  object ScalaString extends Host("com/huawei/excelsior/jet/runtime/jobject/ScalaString")
  enum ScalaString extends RTConst(ScalaString) {
    case SIZE
  }

  object ScalaArray extends Host("com/huawei/excelsior/jet/runtime/jobject/ScalaArray")
  enum ScalaArray extends RTConst(ScalaArray) {
    case MAX_DIMENSION
    case HEADER_SIZE
    case ARRAY_BODY_OFFS

    case length
  }

  object AJArray extends Host("com/huawei/excelsior/jet/runtime/jobject/AJArray")
  enum AJArray extends RTConst(AJArray) {
    case HEADER_SIZE
    case BODY_OFFS
    case LARGE_LENGTH_OFFS
    case LENGTH_OFFS

    case length
  }

  object CangjieArray extends Host("com/huawei/excelsior/jet/runtime/jobject/CangjieArray")
  enum CangjieArray extends RTConst(CangjieArray) {
    case HEADER_SIZE
    case BODY_OFFS
    case LARGE_LENGTH_OFFS
    case LENGTH_OFFS

    case length
  }

  object ObjLink extends Host("com/huawei/excelsior/jet/runtime/jobject/ObjLink")

  object ObjTag extends Host("com/huawei/excelsior/jet/runtime/jobject/ObjTag")
  enum ObjTag extends RTConst(ObjTag) {
    case FLC_BIT
    case STAT_BIT
    case GUEST
    case ARRAY
    case SPECIAL_OBJECT
    case NO_TRACEABLE_FIELDS
    case CHA_BIT
  }

  object ObjTags extends Host("com/huawei/excelsior/jet/runtime/jobject/ObjTags")
  enum ObjTags extends RTConst(ObjTags) {
    case LOCATION_BITS

    case LOCATION_TYPE_OF_LARGE_OBJECT
    case LOCATION_TYPE_OF_STACK_ALLOC_OBJECT
    case LOCATION_TYPE_OF_SMALL_OBJECT
    case LOCATION_TYPE_OF_NORMAL_OBJECT
  }

  object LockWord extends Host("com/huawei/excelsior/jet/runtime/thread/sync/LockWord")
  enum LockWord extends RTConst(LockWord) {
    case SIZE
    case LOCK_WORD_FREE_OBJECT
    case STATUS_MASK
    case ALL_EXCEPT_OWNER_CNT_MASK
    case STATUS_BYTE_NUM
  }

  object BiasedLocking extends Host("com/huawei/excelsior/jet/runtime/thread/sync/BiasedLocking") {

    object LockingContext extends Host(BiasedLocking, "LockingContext")
    enum LockingContext extends RTConst(LockingContext) {
      case INVALID_LOCKING_CONTEXT
      case LOCKED_VIA_FAST_PATH
    }
  }

  object ClassLoaderIDProvider extends Host("com/huawei/excelsior/jet/runtime/classload/ClassLoaderIDProvider")
  enum ClassLoaderIDProvider extends RTConst(ClassLoaderIDProvider) {
    case UKNOWN_CLID
    case SYSTEM_CLID
    case EXT_CLID
    case APP_CLID
    case LAST_STD_CLID
  }

  object TDTag extends Host("com/huawei/excelsior/jet/runtime/typedesc/TDTag")
  enum TDTag extends RTConst(TDTag) {
    case PRIMITIVE
    case AJ_ARRAY
    case CLASS
    case INTERF
    case CANGJIE_ARRAY
    case INFECTED
    case RECORD
  }

  object RunTimeTypeInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/RunTimeTypeInfo") {
    object Builder extends Host(RunTimeTypeInfo, "Builder")
    enum Builder extends RTConst(Builder) {
      case HIERARCHY_ROOT_IMPORT_INDEX
    }
  }
  enum RunTimeTypeInfo extends RTConst(RunTimeTypeInfo) {
    case MAGIC
  }


  object TypeHandle extends Host("com/huawei/excelsior/jet/runtime/typedesc/TypeHandle") {

    object Flags extends Host(TypeHandle, "Flags")
    enum Flags extends RTConst(Flags) {
      case PREPARED
      case HAS_RTTI
      case LIGHT
    }
  }

  enum TypeHandle extends RTConst(TypeHandle) {
    case rawClassObject
    case flags
    case td
  }

  object TDInitInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/TDInitInfo")

  object HostingRunTimeTypeInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/HostingRunTimeTypeInfo") {
    object Flags extends Host(HostingRunTimeTypeInfo, "Flags")
    enum Flags extends RTConst(Flags) {
      case PREPARED
      case HAS_RTTI
    }
  }

  enum HostingRunTimeTypeInfo extends RTConst(HostingRunTimeTypeInfo) {
    case cFuncWrappers
    case nativeMethodTable
    case UNINITIALIZED_RT_INUM
  }

  object HostingTypeHandle extends Host("com/huawei/excelsior/jet/runtime/typedesc/HostingTypeHandle") {
    object NativeMethodUnion extends Host(HostingTypeHandle, "NativeMethodUnion")
  }
  enum HostingTypeHandle extends RTConst(HostingTypeHandle) {
    case initialized
    case frameDescriptorsInitInfo
    case importedTypes
    case preparationInfo
    case customTypeInfo
  }


  object InfectedTypeHandle extends Host("com/huawei/excelsior/jet/runtime/typedesc/InfectedTypeHandle")

  enum InfectedTypeHandle extends RTConst(InfectedTypeHandle) {
    case frameDescriptorsInitInfo
    case importedTypes
    case preparationInfo
  }

  object AJArrayTypeHandle extends Host("com/huawei/excelsior/jet/runtime/typedesc/AJArrayTypeHandle")

  object CangjieArrayTypeHandle extends Host("com/huawei/excelsior/jet/runtime/typedesc/CangjieArrayTypeHandle")

  object JavaTypeHandle extends Host("com/huawei/excelsior/jet/runtime/typedesc/JavaTypeHandle") {

    object Flags extends Host(JavaTypeHandle, "Flags")
    enum Flags extends RTConst(Flags) {
      case HAS_CLASS_OBJECT
    }
  }

  object AbsentContainer extends Host("com/huawei/excelsior/jet/runtime/typedesc/AbsentContainer")

  object ScalaTypeHandle extends Host("com/huawei/excelsior/jet/runtime/typedesc/ScalaTypeHandle") {

    object Flags extends Host(ScalaTypeHandle, "Flags")

    enum Flags extends RTConst(Flags) {
      case HAS_CLASS_OBJECT
    }
  }

  object RefFieldOffset extends Host("com/huawei/excelsior/jet/runtime/typedesc/RefFieldOffset")
  enum RefFieldOffset extends RTConst(RefFieldOffset) {
    case END_OF_OFFSETS_MARKER
  }

  object HostedCUDInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/cud/HostedCUDInfo")

  object AnnotationsInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/AnnotationsInfo")

  object InnerClassInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/InnerClassInfo")

  object EnclosingMethodInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/EnclosingMethodInfo")

  object MetaInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/MetaInfo")
  enum MetaInfo extends RTConst(MetaInfo) {
    case UNREFLECTED_FIELD_OFFS

    case aotConstantPoolData
  }

  object JavaCustomTypeInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/JavaCustomTypeInfo") {
    object MetaInfoUnion extends Host(JavaCustomTypeInfo, "MetaInfoUnion")
  }

  object JavaPackageDesc extends Host("com/huawei/excelsior/jet/runtime/typedesc/JavaPackageDesc")
  enum JavaPackageDesc extends RTConst(JavaPackageDesc) {
    case NUM_OF_PKG_CLASSLOADERS
  }

  object Parameters extends Host("com/huawei/excelsior/jet/runtime/typedesc/Parameters")
  enum Parameters extends RTConst(Parameters) {
    case MALFORMED_WRONG_NUM
    case MALFORMED_WRONG_CPI_TYPE
  }

  object Parameter extends Host("com/huawei/excelsior/jet/runtime/typedesc/Parameter")
  enum Parameter extends RTConst(Parameter) {
    case NULL_NAME_BIT_MASK
    case LVT_CONVERTED_BIT_MASK
  }

  object StringRef extends Host("com/huawei/excelsior/jet/runtime/typedesc/StringRef")
  enum StringRef extends RTConst(StringRef) {
    case EMPTY
    case INDEXED_STRING_FLAG
    case LENGTH_SHIFT
    case MAX_LENGTH
    case MAX_OFFSET
  }

  object TypeModifiers extends Host("com/huawei/excelsior/jet/runtime/typedesc/TypeModifiers")
  enum TypeModifiers extends RTConst(TypeModifiers) {
    case HAS_CANGJIE_COLD_STRINGS_AT_STDLIB_CBC
    case DEPRECATED
    case VERIFIED
    case NO_META_INFO
    case LIGHT
    case HIDE_DEPRECATED_IN_CP_MODE
    case HAS_VCF
    case ANONYMOUS
    case FINALIZABLE
    case JIT_COMPILED
    case RUNTIME
    case VERIFY_ERROR
    case CLASS_DEF_ERROR
    case THROWABLE
    case METHOD_ACCESSOR_IMPL
    case CANGJIE
    case CBC
    case HAS_DEFAULTS
    case SUPERINTERFS_HAVE_DEFAULTS
    case PERSISTENT_MODIFIERS
  }

  object FrameDescriptorsArray extends Host("com/huawei/excelsior/jet/runtime/typedesc/FrameDescriptorsArray")
  enum FrameDescriptorsArray extends RTConst(FrameDescriptorsArray) {
    case array
  }

  object FrameDescriptorsInitInfo extends Host("com/huawei/excelsior/jet/runtime/typedesc/FrameDescriptorsInitInfo")
  enum FrameDescriptorsInitInfo extends RTConst(FrameDescriptorsInitInfo) {
    case METHOD_NUM_START
    case EOS
    case DELTA_MASK
    case SIZE_MASK
    case SIZE_SHIFT
    case LARGE_SEGMENT_MARK
    case BIG_DELTA_MARK
    case DELTA_MAX
    case MAX_FRAME_SIZE
    case XTABLE_CHUNK_COUNT_MASK
    case XTABLE_LONG_CHUNK_COUNT_FLAG_SHIFT
    case XTABLE_CHUNK_SIZE
    case WIDE_REGS_BIT_MAPS
    case TRIVIAL_XHANDLER_FLAG_SHIFT
  }

  object InstanceDescriptor extends Host("com/huawei/excelsior/jet/runtime/typedesc/InstanceDescriptor") {
    object Builder extends Host(InstanceDescriptor, "Builder") {
      object Flags extends Host(Builder, "Flags")
      enum Flags extends RTConst(Flags) {
        case LOCKABLE_MASK
        case BACKTRACE_MASK
        case HAS_FINALIZE_MASK
        case GUEST_MASK
        case WEAK_OBJECT_MASK
      }
    }
  }

  enum InstanceDescriptor extends RTConst(InstanceDescriptor) {
    case rtti
    case cohenDisplay
  }

  object CohenDisplay extends Host("com/huawei/excelsior/jet/runtime/typedesc/CohenDisplay")
  enum CohenDisplay extends RTConst(CohenDisplay) {
    case INLINED_SIZE
    case CC_BIT
    case LEVEL_BIT

    case inlined
    case outlined
    case level
  }

  object ManagedInstanceDescriptor extends Host("com/huawei/excelsior/jet/runtime/typedesc/ManagedInstanceDescriptor")
  enum ManagedInstanceDescriptor extends RTConst(ManagedInstanceDescriptor) {
    case IMT_SLOTS_OFFSET
    case VMT_OFFSET
  }

  object CangjieInstanceDescriptor extends Host("com/huawei/excelsior/jet/runtime/typedesc/CangjieInstanceDescriptor")
  enum CangjieInstanceDescriptor extends RTConst(CangjieInstanceDescriptor) {
    case IMT_SLOTS_OFFSET
    case VMT_OFFSET

    case elemSize
  }

  object AJArrayInstanceDescriptor extends Host("com/huawei/excelsior/jet/runtime/typedesc/AJArrayInstanceDescriptor")
  enum AJArrayInstanceDescriptor extends RTConst(AJArrayInstanceDescriptor) {
    case elemSize
  }

  object JavaInstanceDescriptor extends Host("com/huawei/excelsior/jet/runtime/typedesc/JavaInstanceDescriptor") {
    object Builder extends Host(JavaInstanceDescriptor, "Builder") {
      object JavaFlags extends Host(Builder, "JavaFlags")
      enum JavaFlags extends RTConst(JavaFlags) {
        case CLONEABLE_MASK
        case SERIALIZABLE_MASK
      }
    }
  }

  enum JavaInstanceDescriptor extends RTConst(JavaInstanceDescriptor) {
    case IMT_SLOTS_OFFSET
    case VMT_OFFSET

    case arrayBaseType
  }

  object ScalaInstanceDescriptor extends Host("com/huawei/excelsior/jet/runtime/typedesc/ScalaInstanceDescriptor") {

    object ArrayBuilder extends Host(ScalaInstanceDescriptor, "ArrayBuilder")
    enum ArrayBuilder extends RTConst(ArrayBuilder) {
      case VMT_SIZE
      case ALLOC_SIZE
    }
  }

  enum ScalaInstanceDescriptor extends RTConst(ScalaInstanceDescriptor) {
    case IMT_SLOTS_OFFSET
    case VMT_OFFSET

    case arrayBaseType
  }

  object CodeUnitDescriptor extends Host("com/huawei/excelsior/jet/runtime/typedesc/cud/CodeUnitDescriptor") {

    object Ref extends Host(CodeUnitDescriptor, "Ref")
    enum Ref extends RTConst(Ref) {
      case MARK_BIT
      case DECL_CLASS_IDX_SHIFT
      case DECL_CLASS_IDX_MASK
      case METHOD_IDX_SHIFT
      case METHOD_IDX_MASK
    }
  }

  object CodeUnitFlags extends Host("com/huawei/excelsior/jet/runtime/typedesc/cud/CodeUnitFlags")
  enum CodeUnitFlags extends RTConst(CodeUnitFlags) {
    case COMPILED
    case VERSIONED
    case C_ANNOTATED
    case RTS_PROC
  }

  object CangjieSpecialMethodFlags extends Host("com/huawei/excelsior/jet/runtime/jit/cbc/file/CangjieSpecialMethodFlags")
  enum CangjieSpecialMethodFlags extends RTConst(CangjieSpecialMethodFlags) {
    case MUT_PARAM_FLAG
    case UG_DESC_PARAM_FLAG
    case THIS_TYPE_INFO_PARAM_FLAG
    case RET_BY_VAL_PARAM_FLAG
    case C_FUNC_RET_BY_VAL_PARAM_FLAG
  }

  object MethodSearchErrorCUD extends Host("com/huawei/excelsior/jet/runtime/typedesc/cud/MethodSearchErrorCUD") {

    object ErrorCode extends Host(MethodSearchErrorCUD, "ErrorCode")
    enum ErrorCode extends RTConst(ErrorCode) {
      case ABSTRACT_METHOD
      case ILLEGAL_ACCESS
      case INCOMPATIBLE_CLASS_CHANGE

      case COUNT
    }
  }

  object AOTCompiledRTSProcCUD extends Host("com/huawei/excelsior/jet/runtime/typedesc/cud/AOTCompiledRTSProcCUD")

  object TypeKind extends Host("com/huawei/excelsior/jet/runtime/typedesc/TypeKind")
  enum TypeKind extends RTConst(TypeKind) {
    case VOID
    case CLASS
    case INTERFACE
    case ARRAY
    case BOOLEAN
    case CHAR
    case FLOAT
    case DOUBLE
    case BYTE
    case SHORT
    case INT
    case LONG
  }

  object BasicType extends Host("com/huawei/excelsior/jet/runtime/typedesc/BasicType")
  enum BasicType extends RTConst(BasicType) {
    case VOID
    case BYTE
    case BOOLEAN
    case CHAR
    case SHORT
    case INT
    case LONG
    case FLOAT
    case DOUBLE
    case REFERENCE
  }

  object MethodAccessModifier extends Host("com/huawei/excelsior/jet/runtime/typedesc/MethodAccessModifier")
  enum MethodAccessModifier extends RTConst(MethodAccessModifier) {
    case PUBLIC
    case PRIVATE
    case PROTECTED
    case STATIC
    case FINAL
    case SYNCHRONIZED
    case ABSTRACT
    case STRICT
  }

  object VMTEncoding extends Host("com/huawei/excelsior/jet/runtime/typedesc/VMTEncoding") {

    object Instruction extends Host(VMTEncoding, "Instruction") {

      object Kind extends Host(Instruction, "Kind")
      enum Kind extends RTConst(Kind) {
        case OWN_METHOD
        case SUPER_METHOD
        case SKIP
        case ERROR
        case MASK
        case SHIFT
      }
    }
  }

  object CIAO extends Host("com/huawei/excelsior/jet/runtime/typechecks/CIAO")
  enum CIAO extends RTConst(CIAO) {
    case IMT_OFFSET_SHIFT
  }

  object InterfCast1LCache extends Host("com/huawei/excelsior/jet/runtime/typechecks/InterfCast1LCache")
  enum InterfCast1LCache extends RTConst(InterfCast1LCache) {
    case ciao
    case imt
  }

  object InterfInstanceofCacheEx extends Host("com/huawei/excelsior/jet/runtime/typechecks/InterfInstanceofCacheEx")

  object ThinObj extends Host("com/huawei/excelsior/jet/runtime/thin/ThinObj")
  enum ThinObj extends RTConst(ThinObj) {
    case td
    case fields
  }

  object ThinTypeHandle extends Host("com/huawei/excelsior/jet/runtime/thin/ThinTypeHandle")
  enum ThinTypeHandle extends RTConst(ThinTypeHandle) {
    case MAGIC

    case level
    case vmt
  }

  object SoftExceptions extends Host("com/huawei/excelsior/jet/runtime/excepts/SoftExceptions") {

    object Kind extends Host(SoftExceptions, "Kind")
    enum Kind extends RTConst(Kind) {
      case TD_BARRIER
    }
  }

  object PackedFDInfoImpl extends Host(s"com/huawei/excelsior/jet/runtime/excepts/$targetArch/PackedFDInfoImpl")
  enum PackedFDInfoImpl extends RTConst(PackedFDInfoImpl) {
    case TRIV_XHANDLER_FLAG_BIT
    case SAVED_REGS_BM_MASK
    case XMMS_BM_MASK
    case GPRS_BM_MASK
    case GPRS_SHIFT
    case CORE_REGS_SHIFT

    case savedRegsBitMap
  }

  object CodeRegion extends Host(s"com/huawei/excelsior/jet/runtime/excepts/CodeRegion") {

    object Kind extends Host(CodeRegion, "Kind")
    enum Kind extends RTConst(Kind) {
      case GC_AWARE
      case MANUAL
    }
  }

  object GCMapDecoder extends Host("com/huawei/excelsior/jet/runtime/memory/gc/stack/maps/decoders/GCMapDecoder") {
    object Code extends Host(GCMapDecoder, "Code")
    enum Code extends RTConst(Code) {
      case ONE_SLOT_BASED_OPCODE
      case LIST_SLOTS_BASED_OPCODE
      case LIST_SLOTS_OPCODE
      case LIST_NEGATIVE_SLOTS_OPCODE
      case LIST_STACK_ALLOC_BASED_OPCODE
      case LIST_UNMOVABLE_SLOTS_OPCODE
      case MASK_SLOTS_BASED_OPCODE
      case MASK_SLOTS_OPCODE
    }
  }
  enum GCMapDecoder extends RTConst(GCMapDecoder) {
    case MAX_MASK_WIDTH
    case MAX_STACK_SLOTS_NUMBER
    case I_REGS_COUNT
    case F_REGS_COUNT
  }

  object AJStrConcatGeneric extends Host("com/huawei/excelsior/jet/runtime/managedlib/support/AJStrConcatGeneric") {
    object Args extends Host(AJStrConcatGeneric, "Args")
    enum Args extends RTConst(Args) {
      case SLOT_SIZE
    }
  }

  object RVA extends Host("com/huawei/excelsior/jet/runtime/bincomps/RVA") {
    object Ref extends Host(RVA, "Ref")
    enum Ref extends RTConst(Ref) {
      case RVA_COMPLEMENT
    }
  }

  object TypeTag extends Host("com/huawei/excelsior/jet/runtime/jit/cbc/file/data/TypeTag")
  enum TypeTag extends RTConst(TypeTag) {
    case NOTHING
    case INTERFACES
    case IS_WEAK_REF
    case ANNOTATION_FACTORY_INDEX
    case IS_SINGLETON_OBJECT
    case GENERIC_PARAMETERS
    case GENERIC_CONSTRAINTS
    case FTC_POOL
    case MINI_IMT
    case MANGLE_KIND
    case PREBUILT_DATA
    case FINALIZATION_INDEX
    case PACKAGE_INIT_INDEX
    case IS_RUNTIME_LIB
  }

  object MethodTag extends Host("com/huawei/excelsior/jet/runtime/jit/cbc/file/data/MethodTag")
  enum MethodTag extends RTConst(MethodTag) {
    case NOTHING
    case CODE
    case DEBUG_INFO
    case SOURCE_FULL_NAME
    case SOURCE_FILE
    case GENERIC_PARAMETERS
    case GENERIC_CONSTRAINTS
    case FTC_STRING
    case FTC_STRING_IN_POOL
    case ANNOTATION_FACTORY_INDEX
    case ANNOTATION_FACTORY_INDEXES_FOR_PARAMETERS
    case COVERAGE_START_ID
    case MANGLE_KIND
  }

  object FieldTag extends Host("com/huawei/excelsior/jet/runtime/jit/cbc/file/data/FieldTag")
  enum FieldTag extends RTConst(FieldTag) {
    case NOTHING
    case SLEB_CONST
    case U32_CONST
    case U64_CONST
    case ANNOTATION_FACTORY_INDEX
    case MANGLE_KIND
    case PREBUILT_OFFSET
  }

  object LinkageAccessKind extends Host("com/huawei/excelsior/jet/runtime/jit/cbc/file/LinkageAccessKind")
  enum LinkageAccessKind extends RTConst(LinkageAccessKind) {
    case INVOKE_STATIC
    case INVOKE_VIRTUAL
    case INVOKE_INTERFACE
    case INVOKE_SPECIAL
    case INVOKE_MUT
    case INVOKE_STATIC_VIRTUAL

    case GETFIELD
    case PUTFIELD
    case GETSTATIC
    case PUTSTATIC
  }

  object CangjieFusion extends Host("com/huawei/excelsior/jet/runtime/CangjieFusion")
  enum CangjieFusion extends RTConst(CangjieFusion) {
    case CANGJIE_FUSION_ENABLED
  }

  // -----------------------------------------

  abstract class Host(val className: String) {
    def this(outer: Host, name: String) = this(s"${outer.className}$$$name")

    def alignment: Int = resolver.alignment(this)
    def size: Int = resolver.size(this)
  }

  // -----------------------------------------

  trait Resolver {
    def alignment(host: Host): Int
    def size(host: Host): Int

    def intValue(const: RTConst): Int
    def longValue(const: RTConst): Long
    def addrValue(const: RTConst): Long

    def offset(const: RTConst): Int
  }

  private var _resolver: Resolver = _
  private[RTConst] def resolver: Resolver = _resolver
  def init(resolver: Resolver): Unit = {
    assert(resolver != null)
    _resolver = resolver
  }

  // -----------------------------------------
  // Workaround for jit-bridge unit-tests

  def java_MethodInfoFrameDescriptor_NO_EXCEPTION_HANDLER_AS_OFFSET = MethodInfoFrameDescriptor.NO_EXCEPTION_HANDLER_AS_OFFSET
  def java_InlineList_Head_NO_INLINED_METHODS = InlineList.Head.NO_INLINED_METHODS
  def java_InlineList_Iterator_INLINE_INDEX_ADDEND = InlineList.Iterator.INLINE_INDEX_ADDEND
  def java_InlineList_Iterator_INLINE_ENTRY_MARKERS = InlineList.Iterator.INLINE_ENTRY_MARKERS
  def java_InlineList_Iterator_INLINE_END = InlineList.Iterator.INLINE_END
  def java_InlineList_data = InlineList.data
  def java_InlineList_Format_HAS_METHOD_BIT = InlineList.Format.HAS_METHOD_BIT
  def java_InlineList_Format_HAS_BCPOS_BIT = InlineList.Format.HAS_BCPOS_BIT
  def java_InlineList_Format_HAS_LINES_BIT = InlineList.Format.HAS_LINES_BIT
  def java_InlineList_Element_Markers_REFLECT_METHOD_INVOKE = InlineList.Element.Markers.REFLECT_METHOD_INVOKE
  def java_XTable_ALIGNMENT = XTable.ALIGNMENT
  def java_XTable_BLOCK_ALIGNMENT = XTable.BLOCK_ALIGNMENT
  def java_XTable_BLOCK_SIZE = XTable.BLOCK_SIZE
  def java_XTable_Command_BLOCK_END = XTable.Command.BLOCK_END
  def java_XTable_Command_HANDLER_OFFSET_DIFF = XTable.Command.HANDLER_OFFSET_DIFF
  def java_XTable_Command_NO_HANDLER = XTable.Command.NO_HANDLER
  def java_XTable_Command_INLINE_LIST_HEAD = XTable.Command.INLINE_LIST_HEAD
  def java_XTable_Command_NO_INLINE = XTable.Command.NO_INLINE
  def java_XTable_Command_RECEIVER_INDEX = XTable.Command.RECEIVER_INDEX
  def java_XTable_Command_VNUM = XTable.Command.VNUM
  def java_XTable_Command_UNKNOWN_VNUM = XTable.Command.UNKNOWN_VNUM
  def java_XTable_Command_VCALL = XTable.Command.VCALL
  def java_XTable_Command_REF_CLASS_INDEX = XTable.Command.REF_CLASS_INDEX
  def java_XTable_Command_ICALL = XTable.Command.ICALL
  def java_XTable_Command_FIND_BLOCK = XTable.Command.FIND_BLOCK
  def java_XTable_Command_INLINE_LIST = XTable.Command.INLINE_LIST
  def java_XTable_Command_GCMAP = XTable.Command.GCMAP
  def java_XTable_Command_GCMAP_LENGTH_DIFF = XTable.Command.GCMAP_LENGTH_DIFF
  def java_XTable_Command_BYTECODE_POS = XTable.Command.BYTECODE_POS
  def java_XTable_Command_MNCALL = XTable.Command.MNCALL
  def java_XTable_Command_MARKED_REGION_ID = XTable.Command.MARKED_REGION_ID
  def java_XTable_Command_NO_MARKED_REGION_ID = XTable.Command.NO_MARKED_REGION_ID
  def java_XTable_Command_LINE_NUMBER = XTable.Command.LINE_NUMBER
  def java_XTable_Command_DOMAIN = XTable.Command.DOMAIN
  def java_XTable_Command_MAX_CODE = XTable.Command.MAX_CODE
  def java_XTable_Command_XREGION_START_DIFF_BASE = XTable.Command.XREGION_START_DIFF_BASE
  def java_XTable_Command_GCMAP_LENGTH_SMALL_DIFF_BASE = XTable.Command.GCMAP_LENGTH_SMALL_DIFF_BASE
  def java_XTable_Command_GCMAP_LENGTH_SMALL_DIFF_MAX = XTable.Command.GCMAP_LENGTH_SMALL_DIFF_MAX
  def java_XTable_Command_NO_SOFT_EXCEPTION_ID = XTable.Command.NO_SOFT_EXCEPTION_ID
  def java_XTable_State_Initial_XREGION_START = XTable.State.Initial.XREGION_START
  def java_XTable_State_Initial_HANDLER_OFFSET = XTable.State.Initial.HANDLER_OFFSET
  def java_XTable_State_Initial_INLINE_LIST_HEAD = XTable.State.Initial.INLINE_LIST_HEAD
  def java_XTable_State_Initial_GCMAP_LENGTH = XTable.State.Initial.GCMAP_LENGTH
  def java_XTable_State_Initial_VNUM = XTable.State.Initial.VNUM
  def java_XTable_State_Initial_RECEIVER_INDEX = XTable.State.Initial.RECEIVER_INDEX
  def java_XTable_State_Initial_REF_CLASS_INDEX = XTable.State.Initial.REF_CLASS_INDEX
  def java_XTable_State_Initial_LINE_NUMBER = XTable.State.Initial.LINE_NUMBER
  def java_XTable_State_Initial_BYTECODE_POS = XTable.State.Initial.BYTECODE_POS
  def java_XTable_State_Initial_MARKED_REGION_ID = XTable.State.Initial.MARKED_REGION_ID
  def java_XTable_State_Initial_SOFT_EXCEPTION_ID = XTable.State.Initial.SOFT_EXCEPTION_ID
  def java_GCMapDecoder_MAX_MASK_WIDTH = GCMapDecoder.MAX_MASK_WIDTH
  def java_GCMapDecoder_Code_MASK_SLOTS_BASED_OPCODE = GCMapDecoder.Code.MASK_SLOTS_BASED_OPCODE
  def java_GCMapDecoder_Code_MASK_SLOTS_OPCODE = GCMapDecoder.Code.MASK_SLOTS_OPCODE
  def java_GCMapDecoder_Code_LIST_NEGATIVE_SLOTS_OPCODE = GCMapDecoder.Code.LIST_NEGATIVE_SLOTS_OPCODE
  def java_GCMapDecoder_Code_ONE_SLOT_BASED_OPCODE = GCMapDecoder.Code.ONE_SLOT_BASED_OPCODE
  def java_GCMapDecoder_Code_LIST_SLOTS_BASED_OPCODE = GCMapDecoder.Code.LIST_SLOTS_BASED_OPCODE
  def java_GCMapDecoder_Code_LIST_SLOTS_OPCODE = GCMapDecoder.Code.LIST_SLOTS_OPCODE
  def java_GCMapDecoder_F_REGS_COUNT = GCMapDecoder.F_REGS_COUNT
  def java_GCMapDecoder_I_REGS_COUNT = GCMapDecoder.I_REGS_COUNT
  def java_GCMapDecoder_MAX_STACK_SLOTS_NUMBER = GCMapDecoder.MAX_STACK_SLOTS_NUMBER
  def java_GCMapDecoder_Code_LIST_STACK_ALLOC_BASED_OPCODE = GCMapDecoder.Code.LIST_STACK_ALLOC_BASED_OPCODE

  def java_VMTEncoding_Instruction_Kind_OWN_METHOD = VMTEncoding.Instruction.Kind.OWN_METHOD
  def java_VMTEncoding_Instruction_Kind_SUPER_METHOD = VMTEncoding.Instruction.Kind.SUPER_METHOD
  def java_VMTEncoding_Instruction_Kind_SKIP = VMTEncoding.Instruction.Kind.SKIP
  def java_VMTEncoding_Instruction_Kind_ERROR = VMTEncoding.Instruction.Kind.ERROR
  def java_VMTEncoding_Instruction_Kind_SHIFT = VMTEncoding.Instruction.Kind.SHIFT
}
