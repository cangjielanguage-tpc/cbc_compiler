/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.fixups.{Relocation, RelocationKind}
import com.huawei.excelsior.jet.assembler.{Label, Segment, Symbol}
import com.huawei.excelsior.jet.codeemitter.SymbolInfo
import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind
import com.huawei.excelsior.jet.common.{BuiltInField, XString}
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.Env.{addressSize, isStandalone, languagePack, targetArch}
import com.huawei.excelsior.jet.compiler.PDB2.Location
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodeTypeKind, Tag}
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.debug.dwarf.{Dwarf, DwarfLinker}
import com.huawei.excelsior.jet.compiler.debug.java.JavaDebugToolbox
import com.huawei.excelsior.jet.compiler.intrinsics.{Intrinsic, IntrinsicWithBody, IntrinsicWithoutBody}
import com.huawei.excelsior.jet.compiler.ir.LineNumber.UNKNOWN
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.o2lib.opt.OptEnvModule
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule, formOMFModule, opAttrsModule}
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule.ClassloaderIDGetter
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcJCAModule, pcNamesModule, pcOModule}
import com.huawei.excelsior.jet.compiler.o2lib.tools.ExportNames
import com.huawei.excelsior.jet.compiler.o2lib.u.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.StrOption.{IrLogsDir, PDBName}
import com.huawei.excelsior.jet.compiler.options.{BoolOption, Option}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.{ConstValue, IntValue, LongValue}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.TypeImpl.{cleanTypeCacheDroppableData, fromO2Type}
import com.huawei.excelsior.jet.util.Worklist
import xscala.collection.IdentityHashMap
import xscala.io.*
import xscala.time.LocalDateTime
import xscala.util.MathUtils

import scala.collection.mutable

trait SymLevelObject {
  def o2object: pc.SymLevelObject
}

/** Java sym-level <-> o2 sym-level bridge.
  *
  * @author paul
  */
object LightweightEnvironment {
  private[light] val env = new LightweightEnvironment
  def getInstance = env

  private[light] val o2env = new O2Env

  private[light] def getO2PrimType(typeKind: TypeKind) = pc.SymType.JBC.Primitive(typeKind)

  def newTypeImpl(o2type: pc.SymType) = new TypeImpl(o2type)

  def typeToO2Type(`type`: Type) = `type`.asInstanceOf[TypeImpl].o2object

  def typeToO2Class(`type`: Type) = `type`.asInstanceOf[TypeImpl].asClass

  def foreachO2TypeInSignature(sig: Signature)(action: pc.SymType => Unit): Unit = {
    import SignatureType.*
    def process(sig: Signature): Unit = sig match {
      case _: Primitive | BString | _: TypeVariable => // ignored
      case CPointer(pointee) => process(pointee)
      case t: Wrapper => process(t.baseType)
      case ArraySlice(elemType) => process(elemType)
      case JavaArray(baseType, _) => process(baseType)
      case t @ CangjieArray(elemType) =>
        process(elemType)
        if (!isStandalone) {
          action(sigTypeToO2Type(t))
        }
      case t @ (_: Reference | _: Record) => action(sigTypeToO2Type(t))
      case MethodSignature(returnType, paramTypes) =>
        process(returnType)
        paramTypes foreach process
      case VArray(elemType, _) => process(elemType)
      case t: InstantiatedReference =>
        t.instantiatedTypeParameters foreach process
        action(sigTypeToO2Type(t))
      case t: InstantiatedRecord =>
        t.instantiatedTypeParameters foreach process
        action(sigTypeToO2Type(t))
      case t: Tuple =>
        t.params foreach process
      case t: Box =>
        process(t.base)
      case t: OptionLikeEnum =>
        t.params foreach process
        process(t.someType)
        action(sigTypeToO2Type(t))
      case t: CangjieEnum =>
        t.params foreach process
        action(sigTypeToO2Type(t))
    }
    process(sig)
  }

  def sigTypeToO2Type(`type`: SignatureType) = `type`.symType(getInstance).asInstanceOf[TypeImpl].o2object

  def methodToO2Method(m: Method) = getO2Method(m)

  def fieldToO2Field(f: Field) = f.asInstanceOf[FieldImpl].o2object

  private[light] def memberEquals(m1: pcOModule.Member, m2: pcOModule.Member) = m1 == m2

  private[light] def memberHashCode(m: pcOModule.Member) = m.hashCode

  private[light] def o2name(obj: pc.SymLevelObject) = obj match {
    case obj: pc.Symbol => obj.name
    case obj: pc.SymType => obj.toJString
  }

  def getO2Method(method: Method) = if method == null then null else method.asInstanceOf[MethodImpl].o2m

  private var symCache: IdentityHashMap[AnyRef, AnyRef] = _

  private def getFromSymCache[T <: AnyRef](o2obj: AnyRef) = {
    if (symCache == null) symCache = IdentityHashMap.empty[AnyRef, AnyRef]
    symCache.getOrElse(o2obj, null).asInstanceOf[T]
  }

  private def putToSymCache[T <: AnyRef](o2obj: AnyRef, jobj: T) = {
    symCache(o2obj) = jobj
    jobj
  }

  private[light] class O2Env extends OptEnvModule.Env {
    override def enterClass(c: pcOModule.Class) = {
      super.enterClass(c)
      symCache = IdentityHashMap.empty[AnyRef, AnyRef]
      typesForPreparation.clear()
      typesForBootstrapPreparation.clear()
    }

    override def exitClass() = {
      cleanTypeCacheDroppableData()
      symCache = null
      super.exitClass()
    }
  }

  def typeByO2Object(o2type: pc.SymType): Type =
    fromO2Type(o2type)

  def classByO2Object(o2type: pc.SymType): ClassType =
    fromO2Type(o2type)

  def fieldByO2Object(o2f: pcOModule.Field): Field = {
    val f = getFromSymCache[Field](o2f)
    if (f != null) f else putToSymCache(o2f, new FieldImpl(o2f))
  }

  def methodByO2Object(o2m: pcOModule.Method): Method = {
    val m = getFromSymCache[Method](o2m)
    if (m != null) m else putToSymCache(o2m, new MethodImpl(o2m))
  }

  private[light] def symbolByO2Object(o2obj: pc.Symbol, description: String = null): Symbol = {
    val s = getFromSymCache[Symbol](o2obj)
    if (s != null) s else putToSymCache(o2obj, new SymbolImpl(o2obj, description))
  }

  private[light] def stringTableSymbolByO2Object(o2obj: pc.Symbol, description: String): StringTableSymbol = {
    val s = getFromSymCache[StringTableSymbol](o2obj)
    if (s != null) s else putToSymCache(o2obj, new StringTableSymbolImpl(o2obj, description))
  }

  private def symbolToO2Object(sym: Symbol) =
    sym.asInstanceOf[SymLevelObject].o2object.asInstanceOf[pc.Symbol]

  private[light] def constStringByHostAndStringNumber(hostClass: pcOModule.Class, strnum: Int) = {
    // No caching because key object for this string will be pair (hostClass, strnum),
    // which we need to create every time.
    // But this hash key is equal to CPConstString object.
    CPConstString(hostClass, strnum)
  }

  private[light] def getSymbolAccessKind(sym: Symbol) = o2env.getAccessKind()

  def getMethodFrameDescriptor(m: Method) = getO2Method(m).getFrameDescriptor

  private[light] val typesForPreparation = mutable.LinkedHashSet.empty[Type]
  val typesForBootstrapPreparation = Worklist.empty[pc.SymType]

  private[light] val typeKinds = TypeKind.values
  private[light] val tags = Tag.values

  private def check(enumElem: scala.reflect.Enum, o2Value: Int): Unit = assert(enumElem.ordinal == o2Value,
    "inconsistency, " + enumElem.getClass + "." + enumElem + ".ordinal() should be equal to " + o2Value)

  private def checkBCTypeKind(value: TypeKind, rtValue: Int): Unit = assert(value.getBCTypeKind == rtValue,
    "inconsistency, TypeKind." + value + ".getBCTypeKind() should be equal to " + rtValue)

  private def checkBasicType(value: TypeKind, rtValue: Int): Unit = assert(value.getBasicType == rtValue,
    "inconsistency, TypeKind." + value + ".getBasicType() should be equal to " + rtValue)

  private def checkTypeKindConsistency(): Unit = {
    checkBCTypeKind(TypeKind.VOID,       RTConst.TypeKind.VOID.intValue)
    checkBCTypeKind(TypeKind.BOOLEAN,    RTConst.TypeKind.BOOLEAN.intValue)
    checkBCTypeKind(TypeKind.BYTE,       RTConst.TypeKind.BYTE.intValue)
    checkBCTypeKind(TypeKind.SHORT,      RTConst.TypeKind.SHORT.intValue)
    checkBCTypeKind(TypeKind.CHAR,       RTConst.TypeKind.CHAR.intValue)
    checkBCTypeKind(TypeKind.INT,        RTConst.TypeKind.INT.intValue)
    checkBCTypeKind(TypeKind.LONG,       RTConst.TypeKind.LONG.intValue)
    checkBCTypeKind(TypeKind.FLOAT,      RTConst.TypeKind.FLOAT.intValue)
    checkBCTypeKind(TypeKind.DOUBLE,     RTConst.TypeKind.DOUBLE.intValue)
    checkBCTypeKind(TypeKind.CLASS,      RTConst.TypeKind.CLASS.intValue)
    checkBCTypeKind(TypeKind.INTERFACE,  RTConst.TypeKind.INTERFACE.intValue)
    checkBCTypeKind(TypeKind.ARRAY,      RTConst.TypeKind.ARRAY.intValue)

    checkBasicType(TypeKind.VOID,      RTConst.BasicType.VOID.intValue)
    checkBasicType(TypeKind.BOOLEAN,   RTConst.BasicType.BOOLEAN.intValue)
    checkBasicType(TypeKind.BYTE,      RTConst.BasicType.BYTE.intValue)
    checkBasicType(TypeKind.SHORT,     RTConst.BasicType.SHORT.intValue)
    checkBasicType(TypeKind.CHAR,      RTConst.BasicType.CHAR.intValue)
    checkBasicType(TypeKind.INT,       RTConst.BasicType.INT.intValue)
    checkBasicType(TypeKind.LONG,      RTConst.BasicType.LONG.intValue)
    checkBasicType(TypeKind.FLOAT,     RTConst.BasicType.FLOAT.intValue)
    checkBasicType(TypeKind.DOUBLE,    RTConst.BasicType.DOUBLE.intValue)
    checkBasicType(TypeKind.CLASS,     RTConst.BasicType.REFERENCE.intValue)
    checkBasicType(TypeKind.INTERFACE, RTConst.BasicType.REFERENCE.intValue)
    checkBasicType(TypeKind.ARRAY,     RTConst.BasicType.REFERENCE.intValue)
  }

  private def checkMethodSearchErrorConsistency(): Unit = {
    check(MethodSearchError.ABSTRACT_METHOD,           RTConst.MethodSearchErrorCUD.ErrorCode.ABSTRACT_METHOD.intValue)
    check(MethodSearchError.ILLEGAL_ACCESS,            RTConst.MethodSearchErrorCUD.ErrorCode.ILLEGAL_ACCESS.intValue)
    check(MethodSearchError.INCOMPATIBLE_CLASS_CHANGE, RTConst.MethodSearchErrorCUD.ErrorCode.INCOMPATIBLE_CLASS_CHANGE.intValue)
    assert(MethodSearchError.COUNT == RTConst.MethodSearchErrorCUD.ErrorCode.COUNT.intValue,
      "inconsistency, MethodSearchError.COUNT should be equal to " + RTConst.MethodSearchErrorCUD.ErrorCode.COUNT.intValue)
  }

  private def checkAbsentMemberTypeConsistency(): Unit = {} // TODO: rewise

  private def checkRTMirrorConsistency(): Unit = {
    def checkInstanceSize(tpe: ClassID, expected: Int) = {
      val unalignedSize = CacheAPIModule.getClass(tpe).size
      val actual = MathUtils.alignUp(unalignedSize, RTConst.HeapObj.alignment)
      assert(actual == expected,
        s"inconsistency, expected instance size of ${tpe.name} to be $expected, but was $actual")
    }

    import com.huawei.excelsior.common.LanguagePack.*
    languagePack match {
      case SCALA =>
        checkInstanceSize(ClassID.XScalaString, RTConst.ScalaString.SIZE.intValue)
      case JAVA =>
        checkInstanceSize(ClassID.String, RTConst.JavaString.SIZE.intValue)
      case NONE | CANGJIE =>
        // no checks yet
    }
  }

  def checkRTConstConsistency(): Unit = {
    checkTypeKindConsistency()
    checkMethodSearchErrorConsistency()
    checkAbsentMemberTypeConsistency()
    checkRTMirrorConsistency()
    ClassloaderIDGetter.verify()
  }

  object RTConstResolver extends RTConst.Resolver {
    import RTConst.*

    private val symTypeCache = mutable.HashMap.empty[RTConst.Host, ClassType]
    private def symType(host: RTConst.Host) = symTypeCache.getOrElseUpdate(host, {
      assert(!isStandalone)
      val name = XString(host.className)
      // TODO: remove copy-paste with other find and resolve methods from Environment
      val pdbName = pcNamesModule.newClassName(name).getMangledName
      val pdbPlace = xPDBModule.findPlaceToReadFrom(pdbName, xPDBModule.ContentType.SYM)
      xiEnvModule.loadType(name)

      val clazz = pcOModule.findClass(name, tryAbsent = false)
      assert(clazz != null, s"Could not find RTConst for '$name', possibly incompatible language pack")
      classByO2Object(clazz)
    })

    def alignment(host: RTConst.Host): Int = if (isStandalone) standaloneAlignment(host) else symType(host).getObjectAlignment
    def size(host: RTConst.Host): Int = if (isStandalone) standaloneSize(host) else symType(host).getRawObjectSize

    private val fieldCache = mutable.HashMap.empty[RTConst, ConstValue]
    private def value(const: RTConst) = fieldCache.getOrElseUpdate(const, {
      if (isStandalone) {
        standaloneValue(const)
      } else {
        symType(const.host)
          .findDeclaredFieldOrNull(XString(const.fieldName))
          .getInitialValue
      }
    })

    def intValue(const: RTConst): Int = value(const).asInstanceOf[IntValue].value
    def longValue(const: RTConst): Long = value(const).asInstanceOf[LongValue].value
    def addrValue(const: RTConst): Long = (value(const): @unchecked) match {
      case IntValue(v) => v.toLong
      case LongValue(v) => v
    }

    def offset(const: RTConst): Int = fieldCache.getOrElseUpdate(const, {
      if (isStandalone) {
        IntValue(standaloneOffset(const))
      } else {
        IntValue(symType(const.host)
          .findDeclaredFieldOrNull(XString(const.fieldName))
          .getInstanceFieldOffset)
      }
    }).asInstanceOf[IntValue].value

    private def standaloneAlignment(host: RTConst.Host): Int = host match {
      case HeapObj => addressSize
    }

    private def standaloneSize(host: RTConst.Host): Int = shouldNotReachHere(host)

    private def standaloneOffset(const: RTConst): Int = shouldNotReachHere(const)

    private def standaloneValue(const: RTConst): ConstValue = const match {
      case Eop.ENABLED => IntValue(0)
      case CangjieFusion.CANGJIE_FUSION_ENABLED => IntValue(0)
      case WriteBarriers.WRITE_BARRIERS_ENABLED => IntValue(0)

      case MethodInfoFrameDescriptor.CODE_ALIGNMENT => IntValue(addressSize)
      case MethodInfoFrameDescriptor.NO_EXCEPTION_HANDLER_AS_OFFSET => IntValue(0)
      case MethodInfoFrameDescriptor.NO_INLINE => IntValue(-1)
      case MethodInfoFrameDescriptor.UNKNOWN_SIBERIA_OFFSET => IntValue(-1)

      case MethodInfoFrameDescriptor.LIGHTWEIGHT_FRAME_BIT => IntValue(0)
      case MethodInfoFrameDescriptor.FRAME_OF_HOOK_INVOKER_FLAG_BIT => IntValue(1)
      case MethodInfoFrameDescriptor.IS_VERSIONED_FLAG_BIT => IntValue(3)
      case MethodInfoFrameDescriptor.IS_INTERPRETER_INTERNALS_FLAG_BIT => IntValue(4)
      case MethodInfoFrameDescriptor.WITH_SIBERIA_OFFSET_BIT => IntValue(5)
      case MethodInfoFrameDescriptor.HAS_MARKED_REGIONS_FLAG_BIT => IntValue(6)
      case MethodInfoFrameDescriptor.IS_DYN_LOADED_FLAG_BIT => IntValue(7)
      case MethodInfoFrameDescriptor.DIRTY_FOR_CLASS_GC_FRAME_BIT => IntValue(8)
      case MethodInfoFrameDescriptor.IS_CBC_BIT => IntValue(9)

      case InlineList.Format.HAS_METHOD_BIT => IntValue(0)
      case InlineList.Format.HAS_BCPOS_BIT => IntValue(1)
      case InlineList.Format.HAS_LINES_BIT => IntValue(2)

      case InlineList.Head.NO_INLINED_METHODS => IntValue(-1)

      case InlineList.Element.Markers.REFLECT_METHOD_INVOKE => IntValue(1)

      case InlineList.Iterator.INLINE_INDEX_ADDEND => IntValue(2)
      case InlineList.Iterator.INLINE_ENTRY_MARKERS => IntValue(1)
      case InlineList.Iterator.INLINE_END => IntValue(0)

      case InlineList.Cache.EMPTY => IntValue(0)

      case XTable.BLOCK_SIZE => IntValue(32)
      case XTable.BLOCK_ALIGNMENT => IntValue(4)
      case XTable.ALIGNMENT => IntValue(4)

      case XTable.State.Initial.XREGION_START => IntValue(-1)
      case XTable.State.Initial.HANDLER_OFFSET => IntValue(MethodInfoFrameDescriptor.NO_EXCEPTION_HANDLER_AS_OFFSET.intValue)
      case XTable.State.Initial.INLINE_LIST_HEAD => IntValue(InlineList.Head.NO_INLINED_METHODS.intValue)
      case XTable.State.Initial.GCMAP_LENGTH => IntValue(0)
      case XTable.State.Initial.VNUM => IntValue(-1)
      case XTable.State.Initial.RECEIVER_INDEX => IntValue(0)
      case XTable.State.Initial.REF_CLASS_INDEX => IntValue(0)
      case XTable.State.Initial.BYTECODE_POS => IntValue(-1)
      case XTable.State.Initial.LINE_NUMBER => IntValue(-1)
      case XTable.State.Initial.MARKED_REGION_ID => IntValue(-1)
      case XTable.State.Initial.SOFT_EXCEPTION_ID => IntValue(-1)

      case XTable.Command.BLOCK_END => IntValue(0)
      case XTable.Command.HANDLER_OFFSET_DIFF => IntValue(1)
      case XTable.Command.NO_HANDLER => IntValue(2)
      case XTable.Command.INLINE_LIST_HEAD => IntValue(3)
      case XTable.Command.NO_INLINE => IntValue(4)
      case XTable.Command.RECEIVER_INDEX => IntValue(5)
      case XTable.Command.VNUM => IntValue(6)
      case XTable.Command.UNKNOWN_VNUM => IntValue(7)
      case XTable.Command.VCALL => IntValue(8)
      case XTable.Command.REF_CLASS_INDEX => IntValue(9)
      case XTable.Command.ICALL => IntValue(10)
      case XTable.Command.FIND_BLOCK => IntValue(11)
      case XTable.Command.INLINE_LIST => IntValue(12)
      case XTable.Command.GCMAP => IntValue(13)
      case XTable.Command.GCMAP_LENGTH_DIFF => IntValue(14)
      case XTable.Command.BYTECODE_POS => IntValue(15)
      case XTable.Command.LINE_NUMBER => IntValue(16)
      case XTable.Command.MNCALL => IntValue(17)
      case XTable.Command.MARKED_REGION_ID => IntValue(18)
      case XTable.Command.NO_MARKED_REGION_ID => IntValue(19)
      case XTable.Command.SOFT_EXCEPTION_ID => IntValue(20)
      case XTable.Command.NO_SOFT_EXCEPTION_ID => IntValue(21)
      case XTable.Command.DOMAIN => IntValue(22)

      case XTable.Command.MAX_CODE => IntValue(31)
      case XTable.Command.XREGION_START_DIFF_BASE => IntValue(63)
      case XTable.Command.GCMAP_LENGTH_SMALL_DIFF_BASE => IntValue(31)
      case XTable.Command.GCMAP_LENGTH_SMALL_DIFF_MAX => IntValue(XTable.Command.XREGION_START_DIFF_BASE.intValue - XTable.Command.GCMAP_LENGTH_SMALL_DIFF_BASE.intValue)

      case GCMapDecoder.MAX_MASK_WIDTH => IntValue(32)
      case GCMapDecoder.MAX_STACK_SLOTS_NUMBER => IntValue(0xFFFE)
      case GCMapDecoder.I_REGS_COUNT => IntValue(15) // amd64: 15; arm64: 31
      case GCMapDecoder.F_REGS_COUNT => IntValue(16) // amd64: 16; arm64: 32

      case GCMapDecoder.Code.ONE_SLOT_BASED_OPCODE => IntValue(15)
      case GCMapDecoder.Code.LIST_SLOTS_BASED_OPCODE => IntValue(14)
      case GCMapDecoder.Code.LIST_SLOTS_OPCODE => IntValue(13)
      case GCMapDecoder.Code.LIST_NEGATIVE_SLOTS_OPCODE => IntValue(12)
      case GCMapDecoder.Code.LIST_STACK_ALLOC_BASED_OPCODE => IntValue(11)
      case GCMapDecoder.Code.LIST_UNMOVABLE_SLOTS_OPCODE => IntValue(10)
      case GCMapDecoder.Code.MASK_SLOTS_BASED_OPCODE => IntValue(9)
      case GCMapDecoder.Code.MASK_SLOTS_OPCODE => IntValue(8)

      case TypeTag.NOTHING => IntValue(0)
      case TypeTag.INTERFACES => IntValue(1)
      case TypeTag.IS_WEAK_REF => IntValue(2)
      case TypeTag.ANNOTATION_FACTORY_INDEX => IntValue(3)
      case TypeTag.IS_SINGLETON_OBJECT => IntValue(4)
      case TypeTag.GENERIC_PARAMETERS => IntValue(5)
      case TypeTag.GENERIC_CONSTRAINTS => IntValue(6)
      case TypeTag.FTC_POOL => IntValue(7)
      case TypeTag.MINI_IMT => IntValue(8)
      case TypeTag.MANGLE_KIND => IntValue(9)
      case TypeTag.PREBUILT_DATA => IntValue(10)
      case TypeTag.FINALIZATION_INDEX => IntValue(11)
      case TypeTag.PACKAGE_INIT_INDEX => IntValue(12)
      case TypeTag.IS_RUNTIME_LIB => IntValue(13)

      case MethodTag.NOTHING => IntValue(0)
      case MethodTag.CODE => IntValue(1)
      case MethodTag.DEBUG_INFO => IntValue(2)
      case MethodTag.SOURCE_FULL_NAME => IntValue(3)
      case MethodTag.SOURCE_FILE => IntValue(4)
      case MethodTag.GENERIC_PARAMETERS => IntValue(5)
      case MethodTag.GENERIC_CONSTRAINTS => IntValue(6)
      case MethodTag.FTC_STRING => IntValue(7)
      case MethodTag.FTC_STRING_IN_POOL => IntValue(8)
      case MethodTag.ANNOTATION_FACTORY_INDEX => IntValue(9)
      case MethodTag.ANNOTATION_FACTORY_INDEXES_FOR_PARAMETERS => IntValue(10)
      case MethodTag.COVERAGE_START_ID => IntValue(11)
      case MethodTag.MANGLE_KIND => IntValue(12)

      case FieldTag.NOTHING => IntValue(0)
      case FieldTag.SLEB_CONST => IntValue(1)
      case FieldTag.U32_CONST => IntValue(2)
      case FieldTag.U64_CONST => IntValue(3)
      case FieldTag.ANNOTATION_FACTORY_INDEX => IntValue(4)
      case FieldTag.MANGLE_KIND => IntValue(5)
      case FieldTag.PREBUILT_OFFSET => IntValue(6)

      case LinkageAccessKind.INVOKE_STATIC => IntValue(0)
      case LinkageAccessKind.INVOKE_VIRTUAL => IntValue(1)
      case LinkageAccessKind.INVOKE_INTERFACE => IntValue(2)
      case LinkageAccessKind.INVOKE_SPECIAL => IntValue(3)
      case LinkageAccessKind.INVOKE_MUT => IntValue(4)
      case LinkageAccessKind.INVOKE_STATIC_VIRTUAL => IntValue(5)

      case LinkageAccessKind.GETFIELD => IntValue(6)
      case LinkageAccessKind.PUTFIELD => IntValue(7)
      case LinkageAccessKind.GETSTATIC => IntValue(8)
      case LinkageAccessKind.PUTSTATIC => IntValue(9)
    }
  }
}

final class LightweightEnvironment extends Environment with TypeProvider with SymbolLinker with DwarfLinker {

  override protected def rtConstResolver = LightweightEnvironment.RTConstResolver

  private var stage = Pass.Middle

  override def isCHAEnabledForTraceableReferences = {
    // Disabled in XKRN and CJ StdLib
    !enabled(BuildXKRN) && !enabled(GenLibrary) && !enabled(GenCbcStdLib)
  }

  override def stage[A](stage: Stage)(action: => A): A = TimeRecModule.stage(stage)(action)

  override def getPass = stage

  def setPass(stage: Pass): Unit = this.stage = stage

  override def getBuiltInFieldOffset(f: BuiltInField) =
    o2env.getBuiltInFieldOffset(f) ensuring (_ >= 0)

  def getO2Env = o2env

  def fromO2(o2method: pcOModule.Method) = methodByO2Object(o2method)

  override def getRTSGlobalSymbol(global: RTSGlobal) = symbolByO2Object(o2env.getObjForStdSym(global))

  override def getRTSProc(proc: RTSProc) = {
    fromO2(o2env.getObjForRTSProc(proc))
  }

  override def getSpecStrConcatMethod(format: String) = try { // TODO: optimize method search
    fromO2(CacheAPIModule.getMethod(MethodID.valueOf(format)))
  } catch {
    case _: IllegalArgumentException => null
  }

  override def getStaticFieldSymbol(host: Type, fieldOffset: Int) = shouldNotCallThis()

  override def getStringPoolEntry(host: Type, index: Int) = shouldNotCallThis()

  override def makeStringRef(str: XString) = shouldNotCallThis() // unused, but o2 implementation exists o2env.makeStringRef(str)

  override def accessKind(symbol: Symbol): SymbolInfo.AccessKind = {
    if (symbol.isInstanceOf[Label]) return AccessKind.DIRECT
    if (symbol.isInstanceOf[MethodImpl]) return Method.accessKind
    getSymbolAccessKind(symbol)
  }

  override def makeConstStringData(str: XString, bstr: Boolean) = symbolByO2Object(o2env.makeConstStringData(str, bstr))

  override def makeConstData(value: Array[Byte], align: Int) = symbolByO2Object(o2env.makeConstData(value, value.length, align))

  override def makeUninitializedData(size: Int) = symbolByO2Object(opAttrsModule.newUninitializedData(size))

  override def makeDataSymbol() = symbolByO2Object(opAttrsModule.newRawData(JStringsModule.newJString("$$data")))

  override implicit def getTypeProvider: TypeProvider = this

  override def getSymbolLinker(rootClass: ClassType) = this

  private[light] def convertByteBufferToCodeSegm(buf: ByteBuffer): CodeDefModule.Segment = {
    if (buf == null) return null
    CodeDefModule.makeSeg(buf.getBytesPointer, buf.length)
  }

  private def convertSegmentToCodeSegm(seg: Segment): CodeDefModule.Segment = convertSegmentToCodeSegm(seg, fixupsConverter)

  private def convertSegmentToCodeSegm(seg: Segment, converter: Relocation.Converter): CodeDefModule.Segment = CodeDefModule.makeSeg(seg.getAlignment) {
    seg.finish(converter)
    CodeDefModule.getSeg.setCode(seg.getBytesPointer, seg.length)
  }

  private def sendRawData(seg: Segment) = {
    val symbol = seg.getSymbol
    val o2seg = convertSegmentToCodeSegm(seg)
    val alignment = seg.getAlignment
    assert(alignment != Segment.UNSPECIFIED_ALIGNMENT)
    assert(MathUtils.isPowerOf2(alignment))
    o2seg.requiredSectionAlignmentLg = MathUtils.log2(alignment)

    val o2obj = symbolToO2Object(symbol)
    opAttrsModule.setSegment(o2obj, o2seg)
    o2obj.asInstanceOf[pc.DataSymbol.Sized]
  }

  override def sendData(seg: Segment, method: Method): Unit =
    pcOModule.setPlainArrayLength(sendRawData(seg), seg.length)

  override def sendBytecode(seg: Segment): Unit = sendRawData(seg)

  override def getAllClasses: Iterator[ClassType] = pcOModule.allClasses.map(classByO2Object)

  override def getPrimitiveType(typeKind: TypeKind) = typeByO2Object(getO2PrimType(typeKind))

  override def getArrayType(baseType: Type, dimNum: Int) = classByO2Object(typeToO2Type(baseType).array(dimNum))

  private def getTypeByClassID(classID: ClassID) = classByO2Object(CacheAPIModule.getClass(classID))

  override def getObjectType                    = getTypeByClassID(ClassID.Object)
  override def getAJObjectType                  = getTypeByClassID(ClassID.AJObject)
  override def getFinalizableType               = getTypeByClassID(ClassID.Finalizable)
  override def getLockableAJObjectType          = getTypeByClassID(ClassID.LockableAJObject)
  override def getAJStringType                  = getTypeByClassID(ClassID.AJString)
  override def getAJThrowableType               = getTypeByClassID(ClassID.AJThrowable)
  override def getAJIteratorType                = getTypeByClassID(ClassID.AJIterator)
  override def getCloneableType                 = getTypeByClassID(ClassID.Cloneable)
  override def getSerializableType              = getTypeByClassID(ClassID.Serializable)
  override def getStringType                    = getTypeByClassID(ClassID.String)
  override def getClassType                     = getTypeByClassID(ClassID.Class)
  override def getThrowableType                 = getTypeByClassID(ClassID.JavaThrowable)
  override def getReferenceType                 = getTypeByClassID(ClassID.JavaReference)
  override def getIteratorType                  = getTypeByClassID(ClassID.Iterator)
  override def getThinTypeType                  = getTypeByClassID(ClassID.ThinType)
  override def getPolyThinTypeType              = getTypeByClassID(ClassID.PolyThinType)
  override def getParameterPassingLocationsType = getTypeByClassID(ClassID.ParameterPassingLocations)
  override def getCVarArgListDescType           = getTypeByClassID(ClassID.CVarArgListDesc)
  override def getBacktraceType                 = getTypeByClassID(ClassID.Backtrace)
  override def getManagedEopType                = getTypeByClassID(ClassID.ManagedEopType)
  override def getAJWeakRefType                 = getTypeByClassID(ClassID.AJWeakRef)
  override def getCompilerInterfaceType         = getTypeByClassID(ClassID.CompilerInterface)

  override def getJavaRefType                   = getTypeByClassID(ClassID.JavaRefType)
  override def getScalaRefType                  = getTypeByClassID(ClassID.ScalaRefType)
  override def getCangjieRefType                = getTypeByClassID(ClassID.CangjieRefType)

  override def isCangjieIterator(`type`: ClassType): Boolean =
    if (isCangjieIteratorBase(`type`)) true
    else `type`.getDeclaredSuperTypes.exists(isCangjieIterator)

  private def isCangjieIteratorBase(`type`: Type): Boolean =
    `type`.isCangjieType && `type`.isInterface && `type`.getName.contains(CangjieSymLevelMaker.STD_CORE_ITERATOR_PART)

  def isCangjieWeakRef(`type`: Type): Boolean = {
    if (!`type`.isClass) false
    else if (isCangjieWeakRefBase(`type`)) true
    else `type`.getSuperClasses.exists(isCangjieWeakRefBase)
  }

  private def isCangjieWeakRefBase(`type`: Type): Boolean =
    `type`.isCangjieType && `type`.getName.contains(CangjieSymLevelMaker.STD_REF_WEAK_REF_BASE_PART)

  private var noScalaIteratorInCompilationSet = false
  private var scalaIteratorType: ClassType = _

  override def getScalaIteratorType: ClassType = {
    if (noScalaIteratorInCompilationSet) return null
    if (scalaIteratorType == null) {
      scalaIteratorType = getClassTypeByNameAndClassLoaderSID("scala/collection/Iterator", null)
      noScalaIteratorInCompilationSet = scalaIteratorType == null || scalaIteratorType.isDeferred
    }
    scalaIteratorType
  }

  private var noScalaBoxesRunTimeInCompilationSet = false
  private var scalaBoxesRunTimeType: ClassType = _

  override def getScalaBoxesRunTimeType: ClassType = {
    if (noScalaBoxesRunTimeInCompilationSet) return null
    if (scalaBoxesRunTimeType == null) {
      scalaBoxesRunTimeType = getClassTypeByNameAndClassLoaderSID("scala/runtime/BoxesRunTime", null)
      noScalaBoxesRunTimeInCompilationSet = scalaBoxesRunTimeType == null || scalaBoxesRunTimeType.isDeferred
    }
    scalaBoxesRunTimeType
  }

  override def getXScalaAnyRef = getTypeByClassID(ClassID.XScalaAnyRef)
  override def getXScalaString = getTypeByClassID(ClassID.XScalaString)
  override def getXScalaClass = getTypeByClassID(ClassID.XScalaClass)
  override def getXScalaSerializable = getTypeByClassID(ClassID.XScalaSerializable)

  override def getAJArrayType(kind: BytecodeTypeKind): ClassType = {
    import BytecodeTypeKind.*
    val id = kind match {
      case ARRAY |
           CLASS    => ClassID.AJRefArray
      case BYTE     => ClassID.AJByteArray
      case BOOLEAN  => ClassID.AJBooleanArray
      case CHAR     => ClassID.AJCharArray
      case SHORT    => ClassID.AJShortArray
      case INT      => ClassID.AJIntArray
      case LONG     => ClassID.AJLongArray
      case FLOAT    => ClassID.AJFloatArray
      case DOUBLE   => ClassID.AJDoubleArray
      case _ => shouldNotReachHere("unexpected AJ array kind: " + kind)
    }
    getTypeByClassID(id)
  }

  private[light] def getAJArrayTypeKind(`type`: TypeImpl): TypeKind = {
    val t = `type`.o2object
    if (CacheAPIModule.isThisClass(t, ClassID.AJRefArray))      return TypeKind.CLASS
    if (CacheAPIModule.isThisClass(t, ClassID.AJByteArray))     return TypeKind.BYTE
    if (CacheAPIModule.isThisClass(t, ClassID.AJBooleanArray))  return TypeKind.BOOLEAN
    if (CacheAPIModule.isThisClass(t, ClassID.AJCharArray))     return TypeKind.CHAR
    if (CacheAPIModule.isThisClass(t, ClassID.AJShortArray))    return TypeKind.SHORT
    if (CacheAPIModule.isThisClass(t, ClassID.AJIntArray))      return TypeKind.INT
    if (CacheAPIModule.isThisClass(t, ClassID.AJLongArray))     return TypeKind.LONG
    if (CacheAPIModule.isThisClass(t, ClassID.AJFloatArray))    return TypeKind.FLOAT
    if (CacheAPIModule.isThisClass(t, ClassID.AJDoubleArray))   return TypeKind.DOUBLE
    shouldNotReachHere("unexpected AJ array: " + `type`)
  }

  override def resolveTypeByName(refType: ClassType, name: XString) = {
    val refClass = refType.asInstanceOf[TypeImpl].asClass
    val clazz = refClass.resolveClass(name, addImport = false) ensuring (_ != null)
    classByO2Object(clazz)
  }

  def findO2Class(name: XString, loadPDB: Boolean): pcOModule.Class = {
    if (loadPDB) {
      val pdbName = pcNamesModule.newClassName(name).getMangledName
      val pdbPlace = xPDBModule.findPlaceToReadFrom(pdbName, xPDBModule.ContentType.SYM)
      if (pdbPlace != null) {
        xiEnvModule.loadType(name)
      }
    }
    pcOModule.findClass(name, tryAbsent = false)
  }

  override def findClass(name: XString, loadPDB: Boolean) = {
    val clazz = findO2Class(name, loadPDB)
    if (clazz == null) null else classByO2Object(clazz)
  }

  override val pdb: PDB2 = new PDB2 {
    override def getFile(name: String) = Path(valueOf(PDBName)) / name

    override def getDataInputOrNull(loc: Location): DataInput = {
      val place = findPlace(loc, true)
      if (place == null || !place.exists) {
        return null
      }

      new DataInput {
        val symFile = place.openAsSymForRead()
        var consumed = 0
        val length = symFile.lengthAsInt

        override def getByte(): Int = {
          val b = symFile.tryRead()
          if (b >= 0) {
            consumed += 1
          }
          b
        }

        override def available: Int = length - consumed

        override def close(): Unit = symFile.close()
      }
    }

    override def getDataOutput(loc: Location): DataOutput = new DataOutput {
      val place = findPlace(loc, false)

      val symFile = try {
        place.openAsSymForWrite()
      } catch {
        case _: AssertionError =>
          throw new UnsupportedOperationException("cannot open output stream") // See JET-10335.
      }

      override def putByte(b: Int): Unit = symFile.write(b)
      override def close(): Unit = symFile.closeNew()
    }

    override def exists(loc: Location) = {
      val place = findPlace(loc, true)
      (place != null) && place.exists
    }

    private def findPlace(loc: Location, read: Boolean): xPDBModule.Placeholder = {
      val ctype = xPDBModule.getLocationType(loc)
      val locName = XString(loc.name)
      val lookInMainPDBOnly = ctype == xPDBModule.ContentType.MOD
      if (read && lookInMainPDBOnly) {
        xPDBModule.manager.mainPDB.findPlaceToReadFrom(locName, ctype)
      } else if (read) {
        xPDBModule.findPlaceToReadFrom(locName, ctype)
      } else {
        xPDBModule.findPlaceToWriteTo(locName, ctype)
      }
    }
  }

  override def getSymlevelWriter(writer: SymlevelWriter.StreamWriter, contextClass: Type) = new SymlevelWriterImpl(writer, contextClass)

  override def getSymlevelReader(reader: SymlevelReader.StreamReader, contextClass: Type) = new SymlevelReaderImpl(reader, contextClass)

  private def fixupsConverter: Relocation.Converter = (position: Int, kind: RelocationKind, target: Symbol) => {
    assert(kind supportedOn targetArch)
    val obj = symbolToO2Object(target)
    CodeDefModule.addFixupAt(position)(kind, symbolToO2Object(target), 0)
  }

  override protected def sendMethodCode0(codeUnit: CodeUnit, seg: Segment, xinfo: XInfo, xTable: ByteBuffer, trivialXHandler: Boolean,
                                         hasMarkedRegions: Boolean, siberiaOffset: Int, frame: Frame[_, _, _]): Unit = {
    assert(targetArch ne CBC) // BackEndCBC should have sent segment to CBCFileGenerator

    val method = codeUnit.method
    assert(opAttrsModule.currProc == method.asInstanceOf[MethodImpl].o2m, "must be called only for root methods of compilation")

    val o2seg = convertSegmentToCodeSegm(seg)
    CodeDefModule.setSeg(o2seg)

    val codeAlignment = seg.getAlignment
    assert(codeAlignment != Segment.UNSPECIFIED_ALIGNMENT)
    assert(codeAlignment >= RTConst.MethodInfoFrameDescriptor.CODE_ALIGNMENT.intValue)
    assert(MathUtils.isPowerOf2(codeAlignment))
    o2seg.requiredSectionAlignmentLg = MathUtils.log2(codeAlignment)

    // set metadata
    val dirtyForClassGCFrame = (xinfo != null) && xinfo.isDirtyForClassGC
    val metadata = new CodeMetadata(xTable, trivialXHandler, dirtyForClassGCFrame, hasMarkedRegions, siberiaOffset, frame)

    val `object` = codeUnit.getSymbol.asInstanceOf[SymLevelObject].o2object.asInstanceOf[pc.Symbol]
    opAttrsModule.setSegment(`object`, o2seg, metadata)

    if (enabled(GenDebug) && frame != null) {
      assert((targetArch eq ARM64) || (targetArch eq AMD64)) // TODO-DWARF: support debug info for other platforms

      // TODO-DWARF: refactor dependency injection
      Dwarf.typeProvider = this
      Dwarf.dwarfLinker = this
      Dwarf.linkageName = (s: Symbol) => ExportNames.symbolLinkageName(s)
      Dwarf.typeHandleToType = (t: Any) => typeByO2Object(t.asInstanceOf[pc.SymType])

      // TODO-DWARF find proper place for this
      if (!method.getDeclaringClass.isCangjieType && method.getSourceLine == UNKNOWN) {
        // bytecode can be absent for runtime classes when compiling CangJieStdLib, see JET-13691
        val ca = if (method.getDeclaringClass.isBytecodeAvailable) method.codeAttribute else null
        if (ca != null) {
          method.setSourceFile(method.getDeclaringClass.getSourceFilePath)
          method.setSourceLine(JavaDebugToolbox.methodSourceLine(ca))
        }
      }

      Dwarf.append(method, seg)(this)
    }
  }

  override def start(headerInfo: DwarfLinker.HeaderInfo): Unit = formOMFModule.startDWARFObj(headerInfo)

  override def finishSection(idx: Int, section: XString, bytes: Segment) = {
    val fixups = new ByteBuffer
    val o2seg = convertSegmentToCodeSegm(bytes, (position: Int, kind: RelocationKind, target: Symbol) => {
      fixups.putW8(Dwarf.fixupCode(kind, target))
      fixups.putW32(position)
    })
    formOMFModule.outDWARFSection(idx, section, o2seg, fixups.getBytesPointer, fixups.length)
  }

  override def finish(): Unit = formOMFModule.finishDWARFObj()

  override def getMarkedForPreparationTypes = typesForPreparation.iterator

  override def markForPreparation(`type`: Type): Unit = typesForPreparation.add(`type`)

  override def markForBootstrapPreparation(`type`: Type): Unit = {
    assert(!`type`.isNonBootstrapAnnotated)

    typesForBootstrapPreparation.addOne(typeToO2Type(`type`))
    if (enabled(LogBootstrapPromotionDetailed)) stdout.printStackTrace(new RuntimeException(s"[BOOTSTRAP] ${`type`}"))
  }

  private[light] val intrinsicsCache = mutable.Map[IntrinsicWithBody, Method]()
  private[light] val intrinsicsWithoutBodyCache = mutable.Map[IntrinsicWithoutBody, Method]()

  private[light] def findIntrinsicType(m: Method): Intrinsic = {
    assert(m != null)
    for (intr <- IntrinsicWithoutBody.values) {
      val cachedMethod = intrinsicsWithoutBodyCache.get(intr).orNull
      if (m == cachedMethod) return intr
      if (cachedMethod == null) {
        if (intr.isThisMethod(m)) {
          intrinsicsWithoutBodyCache.put(intr, m)
          return intr
        }
      }
    }
    for (intr <- IntrinsicWithBody.values) {
      val cachedMethod = intrinsicsCache.get(intr).orNull
      if (m == cachedMethod) return intr
      if (cachedMethod == null) {
        if (intr.isThisMethod(m)) {
          intrinsicsCache.put(intr, m)
          return intr
        }
      }
    }
    null
  }

  override def dropSymCache(): Unit = {
    cleanTypeCacheDroppableData()
    intrinsicsCache.clear()
    intrinsicsWithoutBodyCache.clear()
    symCache = null
  }

  override def getClassTypeByNameAndClassLoaderSID(name: String, clsid: String) = {
    val o2type = o2env.getTypeByNameAndClassLoaderSID(XString(name), XString(clsid))
    if (o2type == null) {
      null
    } else {
      val `type` = classByO2Object(o2type)
      assert(`type`.getClassLoaderSID == clsid)
      `type`
    }
  }

  override def forcePrint(s: String): Unit = xiEnvModule.info.forcePrint("%s", s)

  override def print(s: String): Unit = xiEnvModule.info.print(s)

  override def reportStatus(stage: String, methodName: String): Unit =
    OptEnvModule.reportStatus(XString.ascii(stage), XString(methodName))

  override def reportPGOFailure(methodName: String, fatal: Boolean): Unit =
    OptEnvModule.reportPGOFailure(XString(methodName), fatal)

  override def reportWarning(s: String): Unit =
    OptEnvModule.reportWarning(XString(s))

  override def getImportedClassIdx(importedClass: Type, _host: Object) = {
    val currClass = opAttrsModule.currClass
    assert(_host.isInstanceOf[Type] ||
      (currClass != null && (_host == null ||
        (_host.isInstanceOf[Method] && opAttrsModule.currProc == getO2Method(_host.asInstanceOf[Method])))),
      "must be called only for root methods of compilation or with known type")

    val host = _host match {
      case host: Type => host
      case _ =>
        // Note that we can't use ((Method) host).getDeclaringClass if host is a method because of exterior versioning
        fromO2Type(currClass)
    }
    assert(currClass == null || (fromO2Type(currClass) == host), "currClass is known but differs from host")

    if (targetArch == CBC) {
      host.asInstanceOf[TypeImpl].getImportedClassIdx(asClassType(importedClass))
    } else {
      TypeMetaInfoGenerator.Imports.getImportedClassIdx(typeToO2Type(importedClass))
    }
  }

  def addImport(importer: Type, importee: Type): Unit = {
    typeToO2Class(importer).addImport(typeToO2Class(importee))
  }

  override protected def optionValueOrNullFromConfig(option: Option[_]): Any = {
    if (option.isInstanceOf[BoolOption]) {
      val o2res = xiEnvModule.optionSpecified(option.name).toInt
      o2res match {
        case 0 => false
        case 1 => true
        case 2 => null
        case _ => shouldNotReachHere()
      }
    } else {
      val o2res = xiEnvModule.config.equation(option.name)
      if (o2res == null) null else option.parse(o2res.toString)
    }
  }

  override def getHotSwitchCases(method: Method, cases: Int) = new Iterator[Int]() {
    val hotCases = pcJCAModule.getJCAHotSwitchCases(method.asInstanceOf[MethodImpl].o2m, cases)
    var idx = 0

    override def hasNext: Boolean = idx < hotCases.size

    override def next() = {
      assert(hasNext)
      idx += 1
      hotCases(idx - 1)
    }
  }

  private var debugIrLogsDir: Path = _

  override def getDebugIrLogsDir: Path = {
    if (debugIrLogsDir != null) return debugIrLogsDir
    if (env.defined(IrLogsDir)) {
      debugIrLogsDir = Path(env.valueOf(IrLogsDir))
    } else {
      debugIrLogsDir = Path("irlogs") / LocalDateTime.now.toString("yyyyMMdd_HHmmss")
    }
    debugIrLogsDir
  }

  override def asVerifiableMethod(method: Method) = method match {
    case method: MethodImpl => new VerifiableMethodImpl(method)
  }
}
