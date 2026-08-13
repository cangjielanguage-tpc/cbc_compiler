/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.common.{JetDirs, ProcessUtils, XProcess}
import com.huawei.excelsior.jet.assembler.Fixup.seq
import com.huawei.excelsior.jet.assembler.cbc.Fixups.BTTBySymbol
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector.LiveState
import com.huawei.excelsior.jet.assembler.cbc.{ExceptionTable, FieldReference, RawData, StackSlot, isa12}
import com.huawei.excelsior.jet.assembler.fixups.{CoverageLocs, Relocation, RelocationKind}
import com.huawei.excelsior.jet.assembler.{Fixup, Segment}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.NotImplementedFeature.CBC_FILE_ONE_REGION_LIMIT_EXCEEDED
import com.huawei.excelsior.jet.compiler.RTConst.{LinkageAccessKind, TypeTag}
import com.huawei.excelsior.jet.compiler.abi.XTableGenerator
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.cbc.LegacyCBCFileGenerator.*
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator.GenerationTarget
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator.GenerationTarget.{CBC, EXE, STDLIB}
import com.huawei.excelsior.jet.compiler.coverage.JcnoFileGenerator
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox.Types.sigType
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.CommonToolbox.*
import com.huawei.excelsior.jet.compiler.debug.info.*
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels.*
import com.huawei.excelsior.jet.compiler.driver.CompilationMode.ONoCode
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{CJ_MUT, CJ_OVERRIDE, CJ_REDEF, CJ_SEALED}
import com.huawei.excelsior.jet.compiler.layout.MethodTables
import com.huawei.excelsior.jet.compiler.options.BoolOption.{GenCoverageInCBC, GenDebug, ZipCbcChunks}
import com.huawei.excelsior.jet.compiler.options.StrOption.{ForeignLibs, OutputName}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.{Environment, RTConst, TypeProvider}
import com.huawei.excelsior.jet.util.ScalaCollections
import xscala.io.*
import xscala.util.MathUtils.isNBits

import java.io.IOException
import scala.annotation.nowarn
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Using

object LegacyCBCFileGenerator extends CBCFileGenerator {

  // !!!!!----------------------------------------------------------------------!!!!!
  // !!!!!  MUST BE INCREMENTED EACH TIME FILE OR BYTECODE FORMATS ARE CHANGED  !!!!!
  // !!!!!----------------------------------------------------------------------!!!!!
  //
  // Also update versions in runtime and CBC file format specification:
  // - com.huawei.excelsior.jet.runtime.jit.cbc.file.VersionMetadata

  private val FILE_VERSION: Byte     = 28
  private val BYTECODE_VERSION: Byte = 11

  private case class MethodCode(seg: Segment,
                                literalsOffset: Int,
                                xinfo: XTableGenerator.PackedXInfo,
                                untypedStackSlotsCount: Int,
                                usedNonVolIRegsMask: Int,
                                usedNonVolFRegsMask: Int,
                                maxCalleeStackArgsCount: Int,
                                mayHaveNativeCalls: Boolean,
                                stackAllocatedTypeSigs: Seq[SignatureType],
                                variableSizeTypes: Seq[SignatureType])

  private val methodsCode = mutable.LinkedHashMap.empty[Method, MethodCode]

  def sendCode(m: Method, seg: Segment, literalsOffset: Int,
               xinfo: XTableGenerator.PackedXInfo, exTable: ExceptionTable, liveness: LivenessInfoCollector.AllStates,
               tailParamCount: Int, untypedStackSlotsCount: Int,
               usedNonVolIRegsMask: Int, usedNonVolFRegsMask: Int, maxCalleeStackArgsCount: Int,
               mayHaveNativeCalls: Boolean,
               stackAllocatedTypeSigs: Seq[SignatureType], variableSizeTypes: Seq[SignatureType]): Unit = {
    val was = methodsCode.put(m, MethodCode(seg, literalsOffset, xinfo,
      untypedStackSlotsCount,
      usedNonVolIRegsMask, usedNonVolFRegsMask, maxCalleeStackArgsCount,
      mayHaveNativeCalls, stackAllocatedTypeSigs, variableSizeTypes))

    assert(was.isEmpty)
  }

  def env = CBCFileGenerator.env

  private lazy val genCov = env.enabled(GenCoverageInCBC)

  private def writeColdStringsForWorkers(cbc: LegacyCBCFileGenerator): Unit = {
    val strings = cbc.coldStrings
    val output = env.pdb.getFile(CBCFileGenerator.coldStringsForWorkersOut)
    Using.resource(DataOutput.from(output)) { out =>
      out.putW32(strings.size)
      for ((s, i) <- strings.iterator) {
        out.putUTF(s)
        out.putW32(i)
      }
    }
  }

  def generate(output: Path, generationTarget: GenerationTarget = CBC): Unit = {
    assert(env != null)
    val segment = new Segment()
    val cbc = new LegacyCBCFileGenerator(env, output.toString, segment, generationTarget)
    cbc.genCBCFile()
    val f = DataOutput.from(output)
    try {
      f.putBytes(segment.getBytesPointer, 0, segment.length)
    } finally {
      f.close()
    }
    if (cbc.shouldGenJcno && !cbc.metaOnly) {
      assert(genCov)
      JcnoFileGenerator.generate(env.valueOf(OutputName))
    }
    if (generationTarget == STDLIB) {
      writeColdStringsForWorkers(cbc)
    }
  }

  // TODO workaround for JET-15271, preparations for JET-15337
  private enum MangleKind {
    case No, Type, Package
  }

  private object MangleKind {
    def apply(tpe: ClassType): MangleKind = {
      if (tpe.isCangjieType) {
        if (tpe.getCangjiePackage == tpe) {
          Package
        } else {
          Type
        }
      } else {
        No
      }
    }

    def apply(method: Method): MangleKind = {
      if (method.getDeclaringClass.isCangjieType) {
        Package
      } else {
        No
      }
    }
  }

  // TODO-MODIFIERS: refactor this
  private val FLAG_PUBLIC      = 0x0001
  private val FLAG_JAVAREF     = 0x0080
  private val FLAG_VARARG      = 0x0080
  private val FLAG_FOREIGN     = 0x0080
  private val FLAG_NATIVE      = 0x0100
  private val FLAG_RTS_PROC    = 0x0800
  private val FLAG_ARRAY       = 0x1000
  private val FLAG_RECORD      = 0x2000
  private val FLAG_C_ANNOTATED = 0x2000
  private val FLAG_SEALED      = 0x0020
  private val FLAG_MUT         = 0x4000
  private val FLAG_REDEF       = 0x0200
  private val FLAG_OVERRIDE    = 0x1000
  private val FLAG_OPEN        = 0x0040
  private val FLAG_CONDESC     = 0x0100
}

// TODO-CBC: is writeInt ok for writing uint32 offsets? Introduce writeOffset instead
class LegacyCBCFileGenerator(env: Environment, outputName: String, segment: Segment, generationTarget: GenerationTarget = CBC) {

  implicit val typeProvider: TypeProvider = env.getTypeProvider

  private val USHORT_MAX_VALUE: Int = 0xFFFF

  private val zipCbcChunks = env.enabled(ZipCbcChunks) || generationTarget == STDLIB

  private val metaOnly = generationTarget match {
    case STDLIB | EXE => true
    case CBC => ProjectLogic.compilationMode == ONoCode
  }

  private val aotData = generationTarget match {
    case STDLIB | EXE => true
    case CBC => false
  }

  private val shouldGenerateRuntimeData = generationTarget == STDLIB

  private[cbc] type Offset = Int

  private[cbc] final def collectFreeVariables(typeParameters: Seq[SignatureType]): collection.Seq[LocalTypeVariable] = {
    val freeVariables = ArrayBuffer.empty[LocalTypeVariable]

    def rec(types: Seq[SignatureType]): Unit = types foreach {
      case sig: LocalTypeVariable => freeVariables += sig
      case sig: InstantiatedType => rec(sig.instantiatedTypeParameters)
      case _ => // do nothing
    }

    rec(typeParameters)
    freeVariables
  }

  // strings that are expected to be rarely accessed
  private[cbc] val coldStrings = mutable.LinkedHashMap.empty[String, Offset]
  // name of the data chunk -> its content
  private[cbc] val chunksForZipping = mutable.LinkedHashMap.empty[FileTag, Array[Byte]]
  private[cbc] val strings = mutable.LinkedHashMap.empty[String, Offset]
  private val rawData = mutable.LinkedHashMap.empty[RawData, Offset]
  private[cbc] val bytecode: mutable.Map[Method, Offset] = mutable.HashMap.empty[Method, Offset]
  private val debugInfo = mutable.HashMap.empty[Method, Offset]

  private var covCurId = 0
  private val covStartIds = mutable.HashMap.empty[Method, Int]

  private[cbc] type Index = Int

  private def shouldGenJcno = covCurId != 0

  private[cbc] type CBCType = Type | sigIndex.ConstraintType
  private val typeDefs = mutable.LinkedHashMap.empty[String, Offset]
  private[cbc] val typeIndex = mutable.LinkedHashMap.empty[CBCType, Index]

  private val fieldRefIndex = mutable.LinkedHashMap.empty[FieldReference, Index]
  private val fieldRefOffsets = mutable.HashMap.empty[FieldReference, Offset]

  private[cbc] val methodRefIndex = mutable.LinkedHashMap.empty[MethodReference, Index]
  private val methodRefOffsets = mutable.HashMap.empty[MethodReference, Index]

  private var mainType: Type = _
  private var foreignLibs: String = _

  private val genDebug = env.enabled(GenDebug)

  // Tags
  private val NOTHING = 0
  assert(NOTHING == RTConst.TypeTag.NOTHING.intValue)
  assert(NOTHING == RTConst.MethodTag.NOTHING.intValue)
  assert(NOTHING == RTConst.FieldTag.NOTHING.intValue)

  private val GENERIC_PARAMETERS = 5
  assert(GENERIC_PARAMETERS == RTConst.TypeTag.GENERIC_PARAMETERS.intValue)
  assert(GENERIC_PARAMETERS == RTConst.MethodTag.GENERIC_PARAMETERS.intValue)

  private val GENERIC_CONSTRAINTS = 6
  assert(GENERIC_CONSTRAINTS == RTConst.TypeTag.GENERIC_CONSTRAINTS.intValue)
  assert(GENERIC_CONSTRAINTS == RTConst.MethodTag.GENERIC_CONSTRAINTS.intValue)

  private def checkInUShort(id: Int): Int = {
    if (id >= USHORT_MAX_VALUE) notImplemented(CBC_FILE_ONE_REGION_LIMIT_EXCEEDED)
    id
  }

  private def typeName(tpe: Type) = if (tpe.isJavaArray) SignatureType.toJBCSignature(tpe) else tpe.getName

  private def removeNullableWrappers(sig: SignatureType): SignatureType = {
    sig match {
      case ref: (NullableWrapper | NonNullableWrapper) => ref.baseType
      case _ => sig
    }
  }

  private def indexType(tpe: CBCType): Unit = {
    tpe match {
      case tpe: sigIndex.ConstraintType => typeIndex.getOrElseUpdate(tpe, checkInUShort(typeIndex.size))
      case tpe: Type =>
        if (tpe.isJavaReference) {
          strings(typeName(tpe)) = 0
        } else if (tpe.isCangjieType && (tpe.isReference || tpe.isRecord)) {
          typeIndex.getOrElseUpdate(tpe, {
            strings(typeName(tpe)) = 0
            checkInUShort(typeIndex.size)
          })
        } else if (shouldGenerateRuntimeData && tpe.isCompilerInterface) {
          strings(RTMethods.typeDef) = 0
        }
    }
  }

  private def indexFieldRef(fr: FieldReference): Int = {
    fieldRefIndex.getOrElseUpdate(fr, {
      val (name, refType, fieldType, _) = unpackFieldRef(fr)

      strings(name) = 0
      sigIndex.indexFromMetadata(fieldType)
      sigIndex.indexFromMetadata(refType)

      checkInUShort(fieldRefIndex.size)
    })
  }

  private def unpackFieldRef(fr: FieldReference): (String, SignatureType, SignatureType, Int) = {
    val (name, fieldType, accessKind) = fr.field match {
      case PermanentMember(f: Field) => (f.getName, f.getType, linkageAccessKind(f))
      case f: BitcodeFieldReference => (f.fieldName.toString, f.fieldType, linkageAccessKind(f))
      case _ => shouldNotReachHere(fr)
    }

    val refType = fr.refType match {
      case CodeSigSymbol(refType: SignatureType) => refType
      case _ => shouldNotReachHere(fr)
    }

    (name, refType, fieldType, accessKind)
  }

  private def indexMethodRef(mr: MethodReference): Int = {

    def indexRefTypeSig(mr: MethodReference): Unit = {
      sigIndex.indexFromMetadata(mr.refType.sigType)
    }

    methodRefIndex.getOrElseUpdate(mr, {
      mr match {
        case mr: ConstraintCallMethodReference =>
          notImplemented("constraint calls are not supported yet") // FIXME-UG

        case mr: BitcodeMethodReference =>
          strings(mr.methodName.toString) = 0
          sigIndex.indexFromMetadata(mr.sourceMethodType)
          sigIndex.indexFromMetadata(SignatureType.fromSymType(mr.refClass))

        case mr =>
          strings(mr.method.getName) = 0
          sigIndex.indexFromMetadata(SignatureType.fromSymType(mr.method.getDeclaringClass))
          sigIndex.indexFromMetadata(mr.method.getSignature)
          indexRefTypeSig(mr)
      }

      checkInUShort(methodRefIndex.size)
    })
  }

  private def align(v: Int): Unit = {
    var curPos = getCurPos
    while ((curPos % v) != 0) {
      writeByte(0)
      curPos += 1
    }
  }

  private val allClasses = typeProvider.getAllClasses.toArray

  private val compiledTypes = allClasses.filterNot(x => x.isDeferred || x.isJavaAnnotatedCangjieClass || x.isJavaReference || x.isPrimitive || !x.isInCurrentCompilationSet)

  private val constraintTypes = mutable.LinkedHashMap.empty[GenericInfo.Constraint, sigIndex.ConstraintType]

  private def getSuperClass(c: ClassType): Option[SignatureType] = {
    Option(c.getSuperClassSig).map(removeNullableWrappers)
  }

  private[cbc] def collectData(): Unit = {

    def indexGenericInfo(gi: GenericInfo): Unit = {
      for (constraint <- gi.constraints) {
        if (constraint.upperBounds.nonEmpty) {
          constraintTypes.getOrElseUpdate(constraint, {
            val ct = sigIndex.ConstraintType(constraint)
            sigIndex.indexFromMetadata(ct)
            ct
          })
        }
      }
    }

    for (c <- compiledTypes) {
      c.getImportTable.keys foreach { x =>
        indexType(x)
        sigIndex.indexFromMetadata(SignatureType.fromSymType(x))
      }

      if (c.hasMain) {
        assert(mainType == null)
        mainType = c
        strings(mainType.getName) = 0
      }

      indexType(c)
      sigIndex.indexFromMetadata(SignatureType.fromSymType(c))

      val pkg = c.getCangjiePackage
      if (pkg != null) {
        indexType(pkg)
        sigIndex.indexFromMetadata(SignatureType.fromSymType(pkg))
      }

      getSuperClass(c) foreach sigIndex.indexFromMetadata

      if (c.isCangjieArray) {
        val elemType = c.getArrayElemType
        if (elemType.isRecord) {
          assert(elemType eq removeNullableWrappers(elemType))
          sigIndex.indexFromMetadata(elemType)
        }
      }

      for (iface <- c.getDeclaredSuperInterfacesSig) {
          sigIndex.indexFromMetadata(removeNullableWrappers(iface))
      }

      if (c.isUniversalGeneric) {
        indexGenericInfo(c.getGenericInfo)
      }

      for (f <- c.getDeclaredFields) {
        strings(f.getName) = 0
        sigIndex.indexFromMetadata(f.getType)
      }

      for (m <- c.getDeclaredMethods) {
        strings(m.getName) = 0
        sigIndex.indexFromMetadata(m.getSignature)

        if (m.isUniversalGeneric) {
          indexGenericInfo(m.getGenericInfo)
        }

        for (mc <- methodsCode.get(m); sig <- mc.stackAllocatedTypeSigs) {
          sigIndex.indexFromMetadata(sig)
        }

        for (mc <- methodsCode.get(m); sig <- mc.variableSizeTypes) {
          sigIndex.indexFromCode(m, sig)
        }
      }
    }

    for (c <- if (generationTarget == STDLIB) allClasses else compiledTypes;
         m <- c.getDeclaredMethods) {
      if (m.hasSourceFullName) {
        coldStrings(m.getSourceFullName.toString) = 0
      }
      if (m.hasSourceFile) {
        coldStrings(m.getSourceFile.toString) = 0
      }
      if (genDebug && m.hasSourceName) {
        coldStrings(m.getSourceName.toString) = 0
      }
    }

    if (shouldGenerateRuntimeData) {
      val compilerInterface = typeProvider.getCompilerInterfaceType
      indexType(compilerInterface)
      for (m <- RTMethods.methods(env)) {
        indexMethodRef(compilerInterface.getMethodRefToLocal(m.getXName, m.getSignature, STATIC))
      }
    }

    if (genDebug) {
      for (code <- methodsCode.values) {
        for (case LocalVarLabel(info, _, formalType) <- code.seg.filterLabels(_.isInstanceOf[LocalVarLabel])) {
          strings(info.name.toString) = 0
          val sigType = CangjieDebugToolbox.Types.sigType(info.varType, env) getOrElse formalType
          sigIndex.indexFromMetadata(sigType)
        }
      }
    }
  }

  private[cbc] def writeStringsSection(stringsSection: mutable.LinkedHashMap[String, Offset]): Unit = {
    stringsSection.mapValuesInPlace((s, _) => {
      assert(getCurPos < 0xffffffffL)
      val res = getCurPos
      writeString(s)
      res
    })
  }

  private def writeRawData(): Unit = {
    rawData.mapValuesInPlace((a, _) => {
      assert(getCurPos < 0xffffffffL)
      val res = getCurPos
      writeULEB128(a.data.length ensuring (_ < Integer.MAX_VALUE))
      writeByte(a.alignment)
      writeBytes(a.data)
      res
    })
  }

  private def getCurPos: Int = segment.length

  private def writeByte(b: Int): Unit = segment.putByte(b)

  private def writeBytes(bytes: Array[Byte]): Unit = segment.putBytes(bytes)

  private def writeInt(v: Int): Unit = {
    segment.putW32(v)
  }

  private def writeUShort(v: Int): Unit = {
    assert(v <= USHORT_MAX_VALUE)
    segment.putW16(v)
  }

  private def writeLong(v: Long): Unit = {
    segment.putW64(v)
  }

  private def writeULEB128(v: Int): Unit = {
    segment.putULEB(v)
  }

  private def writeLongULEB128(v: Long): Unit = {
    segment.putULEB(v)
  }

  private def writeSLEB128(v: Int): Unit = {
    segment.putSLEB(v)
  }

  private def writeString(s: String): Unit = {
    val bytes = s.getBytes("UTF-8")
    writeULEB128(bytes.length)
    writeBytes(bytes)
  }

  private def getStringSize(s: String): Int = {
    val bytes = s.getBytes("UTF-8")
    LEB128Encoder.calcSizeULEB128(bytes.length) + bytes.length
  }

  private def writeDebugInfo(m: Method, seg: Segment): Unit = {
    debugInfo(m) = getCurPos

    writeULEB128(if (m.getSourceLine != -1) m.getSourceLine else 0)
    if (m.getSourceFile != null) {
      writeColdStringOffset(m.getSourceFile)
    } else {
      writeInt(0)
    }
    if (m.getSourceName != null) {
      writeColdStringOffset(m.getSourceName)
    } else {
      writeInt(0)
    }

    var lnTable = new ArrayBuffer[(Int, Int)]
    val lvTable = new ArrayBuffer[LocalVarLabel]

    /*
     * For compatibility between the CJDB/AOT and CJDB/CJVM debuggers, we repeat here the same filtering on code labels
     * that is done for dwarf (see the [[com.huawei.excelsior.jet.compiler.debug.CodeRecord]] constructor).
     */
    var previousLabel: CodeOriginLabel = null
    seg.filterLabels(_.isInstanceOf[DebugLabel]).sortBy(_.position) foreach {
      case label: CodeOriginLabel if previousLabel == null || previousLabel != label =>
        label match {
          case l: SourceCodeLabel => lnTable += ((l.line, l.position))
          case l: SyntheticCodeLabel => lnTable += ((0, l.position))
          case _: PrologueEndLabel | _: EpilogueBeginLabel => // intentionally ignored
          case _ => shouldNotReachHere("Unsupported CodeOriginLabel")
        }
        previousLabel = label

      case lv: LocalVarLabel =>
        lvTable += lv

      case _ => // ignore other labels
    }

    lnTable = lnTable.distinct

    writeULEB128(lnTable.length)
    for ((line, pc) <- lnTable.sortBy(_._2)) {
      writeULEB128(pc)
      writeULEB128(line)
    }

    def sigTypeForLocalVar(localVarLabel: LocalVarLabel): SignatureType = {
      CangjieDebugToolbox.Types.sigType(localVarLabel.info.varType, env) match {
        case Some(sigType) => removeNullableWrappers(sigType)
        case None => unwrapDebugType(localVarLabel.info.varType) match {
          case rec: DTRecord if rec.identifier.startsWith(XString(CangjieSymLevelMaker.STD_CORE_OPTION_PREFIX)) =>
            // A local variable of the type `Option<T>`, where `T` is a reference type, is represented as a synthetic
            // structure with a single field which is the reference itself (nullable).
            CangjieDebugToolbox.Types.sigType(rec.elements(0).baseType, env) match {
              case Some(sigType) => sigType
              case None => localVarLabel.allocType
            }
          case _ =>
            removeNullableWrappers(localVarLabel.allocType)
        }
      }
    }

    writeULEB128(lvTable.length)
    for (lv <- lvTable.sortWith(LocalVarLabel.lessThan)) {
      writeInt(strings(lv.info.name.toString) ensuring (_ > 0))
      writeULEB128(sigIndex(sigTypeForLocalVar(lv)))
      writeULEB128(lv.location.asInstanceOf[Int])
    }
  }

  private[cbc] def writeCode(): Unit = {
    val ohmmBuilder = OffHeapMemoryMultisetBuilder(this)

    for {
      c <- compiledTypes
      m <- c.getDeclaredMethods
      if !m.isAbstract && !m.isNative
    } {
      bytecode(m) = getCurPos

      val MethodCode(
        seg, literalsOffset, xinfo,
        untypedStackSlotsCount,
        usedNonVolIRegsMask, usedNonVolFRegsMask, maxCalleeStackArgsCount,
        mayHaveNativeCalls, stackAllocatedTypeSigs, variableSizeTypes
      ) = methodsCode(m)

      val instructions: Array[Byte] = seg.toByteArray
      val xtable = Option(xinfo.xTable).map(_.toByteArray).getOrElse(Array.empty[Byte])

      writeULEB128(untypedStackSlotsCount)           // untyped_stack_slots_count
      writeULEB128(stackAllocatedTypeSigs.size)      // typed_stack_slots_count
      for (sig <- stackAllocatedTypeSigs) {
        writeULEB128(sigIndex(sig))
      }

      // OHM multiset
      if (m.hasUniversalGenericContext && variableSizeTypes.nonEmpty) {
        val ohmMultiset = ohmmBuilder.build(m, variableSizeTypes)
        writeULEB128(ohmMultiset.size)
        ohmMultiset foreach writeULEB128
      } else {
        writeULEB128(0)
      }

      writeByte(usedNonVolIRegsMask ensuring (x => isNBits(x, 8))) // used_nonvol_iregs_mask
      writeByte(usedNonVolFRegsMask ensuring (x => isNBits(x, 8))) // used_nonvol_fregs_mask
      writeULEB128(maxCalleeStackArgsCount)          // max_callee_stack_args_count
      writeByte(if (mayHaveNativeCalls) 1 else 0)    // native_calls
      writeULEB128(instructions.length)              // code_size
      writeULEB128(literalsOffset)                   // literals_offset
      writeULEB128(xtable.length)                    // xtable_size
      writeBytes(instructions)                       // instructions
      writeByte(if (xinfo.trivialXHandler) 1 else 0) // triv_xhandle
      writeBytes(xtable)                             // xtable

      if (genDebug && !CangjieDebugToolbox.Names.isArtificial(m)) {
        writeDebugInfo(m, seg)
      }
    }
  }

  private def collectMetadataFromFixups(): Unit = {
    for ((m, code) <- methodsCode) {
      covStartIds(m) = covCurId
      for (f <- code.seg.fixupsIterator) f match {
        case r: Relocation =>
          r.target match {
            case s: ConstraintCallMethodReference => notImplemented("constraint calls are not supported yet") // FIXME-UG
            case s: MethodReference => indexMethodRef(s)
            case s: MethodType => sigIndex.indexFromCode(s)
            case s: FieldReference => indexFieldRef(s)
            case s: ConstStringSymbol => strings(s.value.toString) = 0
            case s: RawData => rawData(s) = 0

            case s: InstantiatedGenericMethod => sigIndex.indexFromCode(m, s)
            case s: CodeSigSymbol => sigIndex.indexFromCode(m, s.sig)

            case _ => //ignore
          }

        case l: CoverageLocs =>
          JcnoFileGenerator.send(covCurId, l.locs)
          covCurId += 1

        case b: BTTBySymbol =>
          sigIndex.indexFromCode(m, b.symbol.asInstanceOf[CodeSigSymbol].sig)

        case b: isa12.Fixups.BTTBySymbol =>
          sigIndex.indexFromCode(m, b.symbol.asInstanceOf[CodeSigSymbol].sig)

        case _ => // Code fixups are not resolved yet so we should skip them. TODO: consider collecting strings during resolving
      }
    }

    JcnoFileGenerator.completeValidLines()
  }

  private def resolveCodeFixups(): Unit = {
    for ((m, code) <- methodsCode) {
      code.seg.finish((pos, kind, target) => {
        kind match {
          case RelocationKind.CBC_ID16 =>
            val id = target match {
              case s: MethodReference => indexMethodRef(s)
              case s: FieldReference => indexFieldRef(s)
              case s: MethodType => checkInUShort(sigIndex(s))
              case s: InstantiatedGenericMethod => sigIndex(m, s)
              case s: CodeSigSymbol => sigIndex(m, s.sig)
              case s: StackSlot.OffHeapMemory => s.idx
              case _ => shouldNotReachHere(target)
            }
            code.seg.setW16(pos, id)

          case RelocationKind.CBC_ID32 =>
            val id = target match {
              case s: ConstStringSymbol => strings(s.value.toString) ensuring (_ > 0)
              case s: RawData => rawData(s) ensuring (_ > 0)
              case _ => shouldNotReachHere(target)
            }
            code.seg.setW32(pos, id)

          case _ => shouldNotReachHere(kind)
        }
      })
    }
  }

  private def fieldOffset(f: Field): Int = {
    if (f.isStatic) {
      f.getStaticFieldOffset
    } else {
      f.getInstanceFieldOffset
    }
  }

  class ColdString(string: XString) extends Fixup(false, 4) {
    override def expectedSize = 4

    override def resolve(converter: Relocation.Converter): Unit = {
      this.segment.setW32(position, coldStrings(string.toString) ensuring (_ > 0))
    }

    override protected def guts = seq(string)
  }

  private def writeTypeDefs(): Unit = {
    for (t <- compiledTypes) {
      writeTypeDef(t)
    }

    if (shouldGenerateRuntimeData) {
      writeCompilerInterface()
    }

    for (constraintType <- constraintTypes.values.toList.distinct.sortBy(_.name)) {
      writeConstraint(constraintType)
    }
  }

  private def writeGenericInfo(gi: GenericInfo): Unit = {
    val constraints = gi.constraints
    if (constraints exists (_.upperBounds.nonEmpty)) {
      writeByte(GENERIC_CONSTRAINTS)
      writeByte(constraints.length)
      for (constraint <- constraints) {
        if (constraint.upperBounds.nonEmpty) {
          writeULEB128(sigIndex(constraintTypes(constraint)))
        } else {
          writeULEB128(0) // unconstrained
        }
      }
    } else {
      writeByte(GENERIC_PARAMETERS)
      writeByte(constraints.size)
    }
  }

  private def writeTypeDef(t: ClassType): Unit = {

    def writeImportTable(): Int = {
      val importTable = t.getImportTable
      if (importTable.isEmpty) {
        0
      } else {
        val curPos = getCurPos
        writeULEB128(importTable.size)
        for (((importedType, i), idx) <- importTable.zipWithIndex) {
          assert(i == idx)
          writeULEB128(sigIndex(SignatureType.fromSymType(importedType)))
        }
        curPos
      }
    }

    typeIndex.getOrElseUpdate(t, checkInUShort(typeIndex.size))

    val importTablePos = writeImportTable()
    val (dynamicMethodMapPos, stationaryMethodMapPos) = writeMethods(t)
    val (instanceFieldSeqPos, staticFieldMapPos) = writeFields(t)

    typeDefs(t.getName) = getCurPos

    val name = t.getName
    writeInt(strings(name) ensuring(_ > 0, s"$name -> ${strings(name)}"))   // name_idx
    writeULEB128(typeFlags(t))                                              // access_flags
    writeInt(importTablePos)                                                // import_table_offs

    val cjPackage = t.getCangjiePackage
    writeUShort(if cjPackage != null then typeIndex(cjPackage) else 0)      // package_idx

    val superClass = getSuperClass(t).filter(_.isCangjieType)
    writeULEB128(superClass map sigIndex.apply getOrElse 0)                 // super_type_idx

    writeInt(dynamicMethodMapPos)                                           // dynamic_method_table_offs
    writeInt(stationaryMethodMapPos)                                        // stationary_method_table_offs
    writeInt(instanceFieldSeqPos)                                           // instance_field_seq_offs
    writeInt(staticFieldMapPos)                                             // static_field_table_offs

    {                                                                       // type_data
      import RTConst.TypeTag
      if (t.hasDeclaredSuperInterfaces) {
        val interfs = t.getDeclaredSuperInterfacesSig.toArray

        // write super interfaces
        writeByte(TypeTag.INTERFACES.intValue)
        writeULEB128(interfs.length)

        for (iface <- interfs) {
          writeULEB128(sigIndex(removeNullableWrappers(iface)))
        }
      }

      writeMangleKind(TypeTag.MANGLE_KIND.intValue, MangleKind(t))
      writeAnnotationFactory(TypeTag.ANNOTATION_FACTORY_INDEX.intValue, t.getCJAnnotationFactory, t)

      if (typeProvider.isCangjieWeakRef(t)) {
        writeByte(TypeTag.IS_WEAK_REF.intValue)
      }

      if (t.isSingletonObject) {
        writeByte(TypeTag.IS_SINGLETON_OBJECT.intValue)
      }

      if (t.isUniversalGeneric) {
        writeGenericInfo(t.getGenericInfo)
        sigIndex.ftc.ftcClassPool(t) foreach { s =>
          writeByte(TypeTag.FTC_POOL.intValue)
          writeInt(s.offset)
          writeULEB128(s.length)
        }
      }

      if (aotData) {
        writePrebuiltData(t)
      }

      if (t.isClass && t.finalizable) {
        writeByte(TypeTag.FINALIZATION_INDEX.intValue)
        writeULEB128(ScalaCollections.singleElement(t.getDeclaredMethods.filter(_.isFinalize)).getHostedIndex);
      }

      if (t.isCangjiePackage) {
        ScalaCollections.singleton(t.getDeclaredMethods.filter(_.isPackageInit)) match {
          case Some(method) =>
            writeByte(TypeTag.PACKAGE_INIT_INDEX.intValue)
            writeULEB128(method.getHostedIndex)
          case _ =>
        }
      }

      writeByte(NOTHING)
    }
  }

  private def writeConstraint(constraintType: sigIndex.ConstraintType): Unit = {
    val supers = constraintType.supers
    assert(supers.nonEmpty)

    val curPos = getCurPos

    typeDefs(constraintType.name) = curPos
    val name = constraintType.name
    writeInt(strings(name) ensuring(_ > 0, s"$name -> ${strings(name)}"))
    writeULEB128(FLAG_CONDESC) // type flags
    writeInt(0) // importTableOffset
    writeUShort(0) // packageId

    val superInterfaces = supers filter (_.isInterface)
    val superClasses = supers filter (_.isClass)
    assert(superInterfaces.size + superClasses.size == supers.size)

    // superClassSigIdx
    if (superClasses.nonEmpty) {
      assert(superClasses.size == 1) // FIXME-UG JET-16959
      writeULEB128(sigIndex(superClasses.head))
    } else {
      writeULEB128(0)
    }

    writeSLEB128(0) // raw instance size

    // start of type_data
    // superInterfaces
    if (superInterfaces.nonEmpty) {
      writeByte(TypeTag.INTERFACES.intValue)
      writeULEB128(superInterfaces.size)
      for (superInterface <- superInterfaces) {
        writeULEB128(sigIndex(superInterface))
      }
    }

    // generic parameters
    constraintType match {
      case sig: sigIndex.ParameterizedConstraint =>
        writeByte(GENERIC_PARAMETERS)
        writeByte(sig.freeVariables.size)
      case _ => // do nothing
    }

    writeByte(NOTHING)
    // end of type_data

    writeULEB128(0) // number of fieldDefsAndStableFieldRefs
    writeULEB128(0) // number of methods
  }

  private def writeCompilerInterface(): Unit = {
    val t = typeProvider.getCompilerInterfaceType

    val (dynamicMethodMapPos, stationaryMethodMapPos) = writeRTMethods(t)
    val (instanceFieldSeqPos, staticFieldMapPos) = writeFields(t)

    val name = RTMethods.typeDef
    typeDefs(name) = getCurPos

    writeInt(strings(name) ensuring(_ > 0, s"$name -> ${strings(name)}"))
    writeULEB128(typeFlags(t))
    writeInt(0) // importTableOffset
    writeUShort(0) // packageId
    writeULEB128(0) // super_type_idx

    writeInt(dynamicMethodMapPos) // dynamic_method_table_offs
    writeInt(stationaryMethodMapPos) // stationary_method_table_offs
    writeInt(instanceFieldSeqPos) // instance_field_seq_offs
    writeInt(staticFieldMapPos) // static_field_table_offs

    writeByte(TypeTag.IS_RUNTIME_LIB.intValue)
    writePrebuiltData(t)
    writeByte(NOTHING)
  }

  private def writePrebuiltData(t: ClassType): Unit = {
    assert(aotData)

    writeByte(TypeTag.PREBUILT_DATA.intValue)

    writeULEB128(t.getRawObjectSize)
    if (t.isRecord) writeULEB128(t.getObjectAlignment)

    val sfbSize = t.getDeclaredFields.filter(_.isStatic).foldLeft(0) {
      case (size, field) =>
        Math.max(size, field.getStaticFieldOffset + field.size)
    }
    writeULEB128(sfbSize)
  }

  private def writeMethods(t: ClassType): (Int, Int) = {
    assert(!t.isCompilerInterface)
    writeMethodsImpl(t, t.getDeclaredMethods)
  }

  private def writeRTMethods(t: ClassType): (Int, Int) = {
    assert(t.isCompilerInterface)
    writeMethodsImpl(t, RTMethods.methods(env).iterator)
  }

  private def writeMethodsImpl(t: ClassType, methods: Iterator[Method]): (Int, Int) = {
    val dynamicMethods = mutable.LinkedHashMap.empty[Method, Offset]
    val stationaryMethods = mutable.LinkedHashMap.empty[Method, Offset]

    for (m <- methods) {
      if (MethodTables.canBeInMethodTable(m) && !t.isRecord) {
        assert(!dynamicMethods.contains(m))
        dynamicMethods(m) = getCurPos
      } else {
        assert(!stationaryMethods.contains(m))
        stationaryMethods(m) = getCurPos
      }
      writeMethod(m, t)
    }

    val dynamicMethodMapPos = getCurPos
    BucketBasedHashTableEncoding.write(dynamicMethods.keys, _.getName, dynamicMethods(_), writeInt)

    val stationaryMethodMapPos = getCurPos
    BucketBasedHashTableEncoding.write(stationaryMethods.keys, _.getName, stationaryMethods(_), writeInt)

    (dynamicMethodMapPos, stationaryMethodMapPos)
  }

  private def writeMethod(method: Method, declaringType: ClassType): Unit = {
    writeInt(strings(method.getName) ensuring (_ > 0))         // name_off
    writeULEB128(sigIndex(method.getSignature))                // src_sig_idx
    writeByte(cangjieSpecialMethodFlags(method.getMethodType)) // special_flags
    writeULEB128(methodFlags(method))                          // access_flags
    writeSLEB128(method.getHostedIndex)                        // method_idx

    {                                                          // method_data
      import RTConst.MethodTag

      if (!metaOnly && !method.isAbstract && !method.isNative) {
        writeByte(MethodTag.CODE.intValue)
        writeInt(bytecode(method))
        if (genDebug && debugInfo.contains(method)) {
          writeByte(MethodTag.DEBUG_INFO.intValue)
          writeInt(debugInfo(method))
        }
        if (genCov) {
          writeByte(MethodTag.COVERAGE_START_ID.intValue)
          writeULEB128(covStartIds(method))
        }
      }

      if (method.isUniversalGeneric) {
        writeGenericInfo(method.getGenericInfo)
        sigIndex.ftc.ftcString(method) foreach { s =>
          assert(sigIndex.ftc.ftcStringRef(method).isEmpty)
          assert(s.length <= sigIndex.MAX_SIG_INDEX_FROM_CODE)
          writeByte(MethodTag.FTC_STRING.intValue)
          writeInt(s.offset)
          writeULEB128(s.length)
        }
      } else if (method.hasUniversalGenericContext) {
        sigIndex.ftc.ftcStringRef(method) foreach { sr =>
          assert(sigIndex.ftc.ftcString(method).isEmpty)
          writeByte(MethodTag.FTC_STRING_IN_POOL.intValue)
          writeULEB128(sr.startIdx)
          writeULEB128(checkInUShort(sr.length))
        }
      }

      if (method.hasSourceFullName) {
        writeByte(MethodTag.SOURCE_FULL_NAME.intValue)
        writeColdStringOffset(method.getSourceFullName)
      }
      if (method.hasSourceFile) {
        writeByte(MethodTag.SOURCE_FILE.intValue)
        writeColdStringOffset(method.getSourceFile)
      }

      writeMangleKind(MethodTag.MANGLE_KIND.intValue, MangleKind(method))

      writeAnnotationFactory(MethodTag.ANNOTATION_FACTORY_INDEX.intValue, method.getCJAnnotationFactory, declaringType)

      val annotationFactoriesForParameters = method.getCJAnnotationFactoriesForParameters
      if (annotationFactoriesForParameters != null) {
        writeByte(MethodTag.ANNOTATION_FACTORY_INDEXES_FOR_PARAMETERS.intValue)
        // Note: annotation factories do not include receiver parameter for instance methods.
        writeByte(annotationFactoriesForParameters.length)
        for (factory <- annotationFactoriesForParameters) {
          val factoryIdx = if (factory == null) {
            -1
          } else {
            assert(factory.getDeclaringClass == method.getDeclaringClass)
            factory.getHostedIndex
          }
          writeSLEB128(factoryIdx)
        }
      }

      writeByte(NOTHING)
    }
  }

  private def writeFields(t: ClassType): (Int, Int) = {

    val instanceFields = mutable.LinkedHashMap.empty[Field, Offset]
    val staticFields = mutable.LinkedHashMap.empty[Field, Offset]

    for (f <- t.getDeclaredFields) {
      if (f.isStatic) {
        assert(!staticFields.contains(f))
        staticFields(f) = getCurPos
      } else {
        assert(!instanceFields.contains(f))
        instanceFields(f) = getCurPos
      }
      writeField(f, SignatureType.fromSymType(t))
    }

    val staticFieldMapPos = getCurPos
    BucketBasedHashTableEncoding.write(staticFields.keys, _.getName, staticFields(_), writeInt)

    val instanceFieldSeqPos = getCurPos
    writeULEB128(instanceFields.size)
    instanceFields.valuesIterator.foreach(writeInt)

    (instanceFieldSeqPos, staticFieldMapPos)
  }

  private def writeField(f: Field, declType: SignatureType): Unit = {
    import RTConst.FieldTag

    def writeInitialValue(): Unit = {
      (f.getInitialValue match {
        case ConstValues.IntValue(value)    => writeByte(FieldTag.SLEB_CONST.intValue); writeSLEB128(value) // TODO: check what is better: sleb or U32
        case ConstValues.LongValue(value)   => writeByte(FieldTag.U64_CONST.intValue); writeLong(value) // TODO: check what is better: sleb, U32 or U64
        case ConstValues.FloatValue(value)  => writeByte(FieldTag.U32_CONST.intValue); writeInt(java.lang.Float.floatToRawIntBits(value))
        case ConstValues.DoubleValue(value) => writeByte(FieldTag.U64_CONST.intValue); writeLong(java.lang.Double.doubleToRawLongBits(value))
      }): @nowarn("msg=match may not be exhaustive")
    }

    writeInt(strings(f.getName) ensuring (_ > 0)) // name_off
    writeULEB128(sigIndex(declType))              // decl_type_sig_idx
    writeULEB128(sigIndex(f.getType))             // sig_idx
    writeULEB128(fieldFlags(f))                   // access_flags

    {                                             // field_data
      if (f.hasInitialValue) writeInitialValue()
      writeAnnotationFactory(FieldTag.ANNOTATION_FACTORY_INDEX.intValue, f.getCJAnnotationFactory, asClassType(declType.symType))
      writeMangleKind(FieldTag.MANGLE_KIND.intValue, MangleKind(f.getDeclaringClass))

      if (aotData) {
        writeByte(FieldTag.PREBUILT_OFFSET.intValue)
        writeULEB128(fieldOffset(f))
      }

      writeByte(FieldTag.NOTHING.intValue)
    }
  }

  private def writeMangleKind(tag: Int, mangleKind: MangleKind): Unit = {
    if (mangleKind != MangleKind.No) {
      writeByte(tag)
      writeByte(mangleKind.ordinal)
    }
  }

  private def writeAnnotationFactory(tag: Int, factory: Method, expectedDeclaringType: ClassType): Unit = {
    if (factory != null) {
      assert(factory.getDeclaringClass == expectedDeclaringType)
      writeByte(tag)
      writeULEB128(factory.getHostedIndex)
    }
  }

  private def typeFlags(tpe: CBCType) = {
    tpe match {
      case tpe: sigIndex.ConstraintType => FLAG_CONDESC
      case tpe: Type =>
        var flags = 0
        if (tpe.isJavaArray) {
          flags = FLAG_ARRAY | FLAG_JAVAREF
        } else if (tpe.isJavaReference) {
          flags = tpe.getAccessFlags | FLAG_JAVAREF
        } else {
          flags |= tpe.getAccessFlags
          if (tpe.isArray) {
            assert((flags & FLAG_ARRAY) == 0)
            flags |= FLAG_ARRAY
          }
          if (tpe.isRecord) {
            assert((flags & FLAG_RECORD) == 0)
            flags |= FLAG_RECORD
          }
          if (tpe.isCangjieType) {
            val modifiers = tpe.getCJModifiers
            if (modifiers contains CJ_SEALED) {
              flags |= FLAG_SEALED
            }
            if (!tpe.isFinal && !tpe.isAbstractClass) {
              flags |= FLAG_OPEN
            }
          }
        }
        flags
    }
  }

  private def methodFlags(m: Method) = { // TODO-MODIFIERS: refactor this
    var flags = m.getJavaModifiersValue

    def clearNativeFlag(flags: Int): Int = {
      assert((flags & FLAG_NATIVE) != 0) // FIXME: HLIR should distinguish foreign, native and rts procs
      flags & ~FLAG_NATIVE
    }

    if (m.isJavaVarArgs) {
      // clear useless `FLAG_VARARG` to avoid clash with [[Modifier.FOREIGN]]
      flags &= ~FLAG_VARARG
    }

    if (m.getDeclaringClass.isCompilerInterface) {
      flags = clearNativeFlag(flags)
      flags |= FLAG_RTS_PROC
    } else if (m.isCangjieForeign) {
      flags = clearNativeFlag(flags)
      flags |= FLAG_FOREIGN
    } else if (m.isCAnnotated) {
      assert((flags & FLAG_C_ANNOTATED) == 0)
      flags |= FLAG_C_ANNOTATED
    }

    if (m.getDeclaringClass.isCangjieType) {
      val modifiers = m.getCJModifiers
      if (modifiers contains CJ_MUT) {
        flags |= FLAG_MUT
      }
      if (modifiers contains CJ_REDEF) {
        flags |= FLAG_REDEF
      }
      if (modifiers contains CJ_OVERRIDE) {
        flags |= FLAG_OVERRIDE
      }
      if (!m.isStatic && !m.isFinal && (flags & FLAG_OVERRIDE) == 0) {
        flags |= FLAG_OPEN
      }
    }

    flags
  }

  /** Bit mask of Cangjie-specific flags for method.
    * Should correspond to [[com.huawei.excelsior.jet.runtime.jit.cbc.file.CangjieSpecialMethodFlags]]
    */
  private final def cangjieSpecialMethodFlags(mt: MethodType): Int = {
    import RTConst.CangjieSpecialMethodFlags.*
    var mask = 0
    if (mt.hasMutObjectParameter) mask |= MUT_PARAM_FLAG.intValue
    if (shouldNotReachHere("ugdesc")) mask |= UG_DESC_PARAM_FLAG.intValue
    if (mt.hasThisTypeInfoParameter) mask |= THIS_TYPE_INFO_PARAM_FLAG.intValue
    if (mt.hasRetByValParameter) mask |= RET_BY_VAL_PARAM_FLAG.intValue
    if (mt.hasCFuncRetByValParameter) mask |= C_FUNC_RET_BY_VAL_PARAM_FLAG.intValue
    mask
  }

  private def fieldFlags(f: Field) = {
    var flags = f.getJavaModifiersValue
    if (f.getType.isRecord) {
      assert((flags & FLAG_RECORD) == 0)
      flags |= FLAG_RECORD
    }
    flags
  }

  private def checkIndexMap[T](indexMap: mutable.LinkedHashMap[T, Int]): Iterable[T] = {
    for (((_, i), idx) <- indexMap.zipWithIndex) assert(i == idx)
    indexMap.keys
  }

  private def writeTypeRegionIndex(): Unit = {
    for (tpe <- checkIndexMap(typeIndex)) {
      tpe match {
        case tpe: sigIndex.ConstraintType =>
          writeInt(strings(tpe.name))

        case tpe: Type =>
          val name = typeName(tpe)
          writeInt(strings(name) ensuring(_ > 0, s"$name -> ${strings(name)}"))
      }
    }
  }

  private def writeMethodIndex(): Unit = {
    for (mr <- checkIndexMap(methodRefIndex)) {
      writeInt(methodRefOffsets(mr))
    }
  }

  private def writeFieldIndex(): Unit = {
    for (f <- checkIndexMap(fieldRefIndex)) {
      writeInt(fieldRefOffsets(f))
    }
  }

  private def writeMethodRefs(): Unit = {
    for (mr <- methodRefIndex.keys) {
      methodRefOffsets(mr) = getCurPos

      val (name, sourceSignatureIdx, refType, specialFlags, accessKind) = mr match {
        case bmr: BitcodeMethodReference =>
          val name = bmr.methodName.toString
          val sourceSignatureIdx = sigIndex(bmr.sourceMethodType)
          val refType = bmr.refType
          val specialFlags = cangjieSpecialMethodFlags(bmr.methodType)
          val accessKind = linkageAccessKind(bmr)
          (name, sourceSignatureIdx, refType, specialFlags, accessKind)

        case mr =>
          assert(mr.hasMethod)
          val method = mr.method

          val name = method.getName
          val sourceSignatureIdx = sigIndex(method.getSignature)
          val refType = (mr, mr.isDirectCall) match {
            case (mr: InstantiatedMethodReference, _) => mr.refType
            case (_, true) => CompiledType(mr.method.getDeclaringClass)
            case _ => mr.refType
          }
          val specialFlags = cangjieSpecialMethodFlags(method.getMethodType)
          val accessKind = linkageAccessKind(mr)
          (name, sourceSignatureIdx, refType, specialFlags, accessKind)
      }

      writeInt(strings(name) ensuring (_ > 0)) // name_off
      writeULEB128(sigIndex(refType.sigType))  // ref_type_sig_idx
      writeULEB128(sourceSignatureIdx)         // src_sig_idx
      writeByte(specialFlags)                  // special_flags
      writeUShort(accessKind)                  // access_kind
    }
  }

  /** See LinkageAccessKind.ajl */
  private def linkageAccessKind(mr: MethodReference): Int = mr.accessKind match {
    case STATIC         => LinkageAccessKind.INVOKE_STATIC.intValue
    case VIRTUAL        => LinkageAccessKind.INVOKE_VIRTUAL.intValue
    case INTERFACE      => LinkageAccessKind.INVOKE_INTERFACE.intValue
    case SPECIAL        => LinkageAccessKind.INVOKE_SPECIAL.intValue
    case MUT            => LinkageAccessKind.INVOKE_MUT.intValue
    case STATIC_VIRTUAL => LinkageAccessKind.INVOKE_STATIC_VIRTUAL.intValue
  }

  /** See LinkageAccessKind.ajl */
  private def linkageAccessKind(f: BitcodeFieldReference): Int = (f.isWrite, f.isStatic) match {
    case (false, false) => LinkageAccessKind.GETFIELD.intValue
    case (true,  false) => LinkageAccessKind.PUTFIELD.intValue
    case (false, true)  => LinkageAccessKind.GETSTATIC.intValue
    case (true,  true)  => LinkageAccessKind.PUTSTATIC.intValue
  }

  /** See LinkageAccessKind.ajl */
  private def linkageAccessKind(f: Field): Int = if (f.isStatic) {
    LinkageAccessKind.GETSTATIC.intValue
  } else {
    LinkageAccessKind.GETFIELD.intValue
  }

  private def writeFieldRefs(): Unit = {
    for (fr <- fieldRefIndex.keys) {
      fieldRefOffsets(fr) = getCurPos

      val (name, refType, fieldType, accessKind) = unpackFieldRef(fr)

      writeInt(strings(name) ensuring (_ > 0)) // name_off
      writeULEB128(sigIndex(refType))          // ref_type_sig_idx
      writeULEB128(sigIndex(fieldType))        // sig_idx
      writeUShort(accessKind)                  // access_kind
    }
  }

  private def writeRegionsData(): Unit = {
    align(4)

    regionIndexOff = getCurPos

    // Currently we have the only region. TODO-CBC: support more regions
    // region header
    assert(getCurPos % 4 == 0)

    writeUShort(typeIndex.size)         // type_idx_size
    writeInt(typeRegionIndexOff)        // type_idx_off

    writeUShort(methodRefIndex.size)    // method_idx_size
    writeInt(methodRefIdxOff)           // method_idx_off

    writeUShort(fieldRefIndex.size)     // field_idx_size
    writeInt(fieldIdxOff)               // field_idx_off

    writeULEB128(sigIndex.size())       // sig_idx_size
    writeInt(sigIndexOff)               // sig_idx_off
  }

  /** After we have combined all cold strings into byte array, it is time to:
    *  1.  Save it as a file
    *  1.  Compress the file (LZMA is used in the current implementation)
    *  1.  Load compressed file as a byte array
    *  1.  Add the compressed chunk to the current *.cbc file
    */
  private[cbc] def zipColdStrings(cbcOutputName: String): (Int, ByteBuffer) = {
    def dropColdStringsOnDisk(cbcOutputName: String): (Int, Path) = {
      val coldStringsOutput = env.pdb.getFile(Path(cbcOutputName).name + "_cold_strings.cbc_help")
      var sectionSize = 0
      Using.resource(DataOutput.from(coldStringsOutput)) { out =>
        for (s <- coldStrings.keysIterator) {
          val bytes = s.getBytes("UTF-8")
          out.putULEB(bytes.length)
          out.putBytes(bytes)
          sectionSize += getStringSize(s)
        }
      }
      (sectionSize, coldStringsOutput)
    }

    def processFileZipping(path: Path): Unit = {
      try {
        val p = XProcess.start(ProcessUtils.sanitizeCommand(Seq(
          (JetDirs.jetHome / "bin" / "xlink").toString,
          "-lzmazip",
          path.toString
        )))
        p.stdin.close() // started process doesn't expect an input
        val res = p.waitFor()
        assert(res == 0)
      } catch {
        case e@(_: IOException | _: InterruptedException) =>
          throw new Error(e)
      }
    }

    val (unzippedColdStringsSize, coldStringsPath) = dropColdStringsOnDisk(cbcOutputName)
    processFileZipping(coldStringsPath)
    val zippedColdStringsPathName = env.pdb.getFile(coldStringsPath.name + ".lzma")
    val zippedColdStrings = Files.readAllBytes(zippedColdStringsPathName)

    Files.delete(coldStringsPath)
    Files.delete(zippedColdStringsPathName)

    (unzippedColdStringsSize, zippedColdStrings)
  }

  private def genVirtualSections(startVirtualOffset: Int): Unit = {
    // TODO: split on chunks
    var virtualOffset = startVirtualOffset
    coldStrings.mapValuesInPlace((s, _) => {
      val stringLocation = virtualOffset
      virtualOffset += getStringSize(s)
      stringLocation
    })
  }

  private def writeColdStringOffset(s: XString): Unit = {
    if (zipCbcChunks) {
      segment.addFixup(ColdString(s))
    } else {
      writeInt(coldStrings(s.toString) ensuring (_ > 0))
    }
  }

  private final def constructFileProperties(): Int = {
    var mask = 0
    if (metaOnly) mask |= 1
    if (aotData) mask |= 2
    mask
  }

  private var fileHeaderSize = 24

  private var typeIndexOff: Int = _
  private var typeRegionIndexOff: Int = _
  private var sigIndexOff: Int = _
  private var regionIndexOff: Int = _
  private var methodRefIdxOff: Int = _
  private var fieldIdxOff: Int = _

  private var compressedChunksPos: Option[Int] = None

  private def genHeader(): Unit = {
    var curPos = 0

    def writeByte(v: Int): Unit = {
      segment.setW8(curPos, v)
      curPos += 1
    }

    def writeShort(v: Int): Unit = {
      segment.setW16(curPos, v)
      curPos += 2
    }

    def writeInt(v: Int): Unit = {
      segment.setW32(curPos, v)
      curPos += 4
    }

    def writeBytes(bytes: Int *): Unit= {
      segment.setBytes(curPos, bytes*)
      curPos += bytes.length
    }

    def writeULEB128(v: Int): Unit = {
      LEB128Encoder.encodeULEB128(v, (b: Int) => {
          segment.setByte(curPos, b)
          curPos += 1
        })
    }

    writeBytes('C', 'B', 'C', FILE_VERSION)       // magic & file_version
    writeByte(BYTECODE_VERSION)                   // bytecode_version
    writeByte(constructFileProperties())          // file_properties
    writeInt(typeIndexOff)                        // type_idx_offs
    writeShort(1)                                 // num_index_regions
    writeInt(regionIndexOff)                      // index_section_off
    if (mainType != null) {
      writeInt(strings(mainType.getName) ensuring (_ > 0))
    } else {
      writeInt(0)
    }

    if (foreignLibs != null) {
      writeInt(strings(foreignLibs))
    } else {
      writeInt(0)
    }

    writeULEB128(covCurId)
    if (covCurId != 0) {
      assert(genCov)
      writeInt(JcnoFileGenerator.checksum.getValue)
    }

    assert(curPos == fileHeaderSize)
  }

  private[cbc] enum FileTag(val tag: Byte) {
    case Nil extends FileTag(0x0)
    case Chunks extends FileTag(0x1)
  }

  private def writeFileInfo(): Unit = {
    def writeFileTag(ht: FileTag): Unit = {
      writeByte(ht.tag)
    }

    if (zipCbcChunks) {
      writeFileTag(FileTag.Chunks)
      compressedChunksPos = Some(getCurPos)
      writeInt(0) // chunks offset placeholder

      writeByte(1) // chunk num
      val (unzippedSize, zipped) = zipColdStrings(outputName)
      writeInt(unzippedSize)
      writeInt(zipped.length)
      chunksForZipping(FileTag.Chunks) = zipped.toByteArray
    }

    writeFileTag(FileTag.Nil)
  }

  def writeZippedSections(): Unit = {
    for (chunk <- chunksForZipping.valuesIterator) {
      writeBytes(chunk)
    }
  }

  private def initForeignLibs(): Unit = {
    foreignLibs = env.valueOfOrNull(ForeignLibs)

    if (foreignLibs != null) {
      strings(foreignLibs) = 0
    }
  }

  def genCBCFile(): Unit = {
    collectData()

    collectMetadataFromFixups()

    sigIndex.freeze()

    fileHeaderSize += LEB128Encoder.calcSizeULEB128(covCurId) + (if covCurId != 0 then 4 else 0)
    segment.putZeroes(fileHeaderSize)

    writeFileInfo()

    initForeignLibs()

    if (!zipCbcChunks) {
      writeStringsSection(coldStrings)
    }

    writeStringsSection(strings)
    writeRawData()

    sigIndex.ftc.writeSymbols()

    if (!metaOnly) {
      resolveCodeFixups()
      writeCode()
    }

    writeTypeDefs()

    writeFieldRefs()
    writeMethodRefs()

    typeIndexOff = getCurPos
    BucketBasedHashTableEncoding.write(typeDefs.keys, identity, typeDefs(_), writeInt)

    typeRegionIndexOff = getCurPos
    writeTypeRegionIndex()

    methodRefIdxOff = getCurPos
    writeMethodIndex()

    fieldIdxOff = getCurPos
    writeFieldIndex()

    sigIndex.writeSignatures()

    sigIndexOff = getCurPos
    sigIndex.writeSigIndex()

    writeRegionsData()

    if (zipCbcChunks) {
      val compressedChunksStart = getCurPos
      segment.setW32(compressedChunksPos.get, compressedChunksStart)

      writeZippedSections()
      genVirtualSections(compressedChunksStart)
    }

    genHeader()

    segment.finish((_, _, _) => shouldNotCallThis("all fixups should be resolved"))
  }

  // TODO consider moving the object to another file
  private[cbc] object sigIndex {

    private[cbc] val MAX_BUILTIN_SIG_INDEX = BuiltinSignature.values.length
    private[cbc] val MAX_SIG_INDEX_FROM_CODE = USHORT_MAX_VALUE >>> 1 // one bit to encode whether sig idx corresponds to ftc symbol idx, or plain sig idx
    private val FTC_BIT_MASK = 0x8000
    assert(MAX_BUILTIN_SIG_INDEX <= MAX_SIG_INDEX_FROM_CODE) // invariant for SignatureIndex.freeze

    private[cbc] type Sig = Signature | BuiltinSignature | InstantiatedGenericMethod | ConstraintType | GenericEntity | GenericTypeTerm

    private[cbc] var _sigIndex = mutable.LinkedHashMap.empty[Sig, Index]
    private val allSignatures = mutable.LinkedHashSet.empty[Sig]
    private val codeSignatures = mutable.LinkedHashSet.empty[Sig]

    private type InstantiatedEntity = InstantiatedType | InstantiatedGenericMethod
    private val instantiatedEntities = mutable.HashMap.empty[InstantiatedEntity, GenericEntity]

    private val typeTerms = mutable.HashMap.empty[InstantiatedEntity, GenericTypeTerm]
    private def createGtt(generic: GenericEntity, instantiated: InstantiatedEntity, typeParameters: Seq[SignatureType]) = {
      val freeVariables = collectFreeVariables(typeParameters)
      Option.when(freeVariables.nonEmpty) {
        typeTerms.getOrElseUpdate(instantiated, {
          GenericTypeTerm(generic, freeVariables.map(_.idx))
        })
      }
    }

    private val signatures = mutable.HashMap.empty[Sig, Offset]
    private var frozen = false

    def apply(mt: MethodType): Index = apply(getMethodTypeSig(mt))

    def apply(m: Method, sig: SignatureType | InstantiatedGenericMethod): Index = {
      assert(frozen)

      val containsTypeVariables = sig match {
        case s: SignatureType => s.containsTypeVariables
        case s: InstantiatedGenericMethod => s.containsTypeVariables
      }

      if (containsTypeVariables) {
        val ftcSymbol = sig match {
          case s: (InstantiatedType | InstantiatedGenericMethod) => getGenericTypeSig(s)
          case s: LocalTypeVariable => s
          case _ => shouldNotReachHere(sig)
        }
        val idx = ftc.ftcString(m)
          .orElse(ftc.ftcStringRef(m).map(_.ftcString))
          .map(_.getSymbolIdx(ftcSymbol))
          .get
        FTC_BIT_MASK | (idx ensuring (_ <= MAX_SIG_INDEX_FROM_CODE))
      } else {
        apply(sig) ensuring (_ <= MAX_SIG_INDEX_FROM_CODE)
      }
    }

    def apply(sig: Sig): Index = {
      assert(frozen)
      sig match {
        case sig: InstantiatedEntity => apply(getGenericTypeSig(sig))
        case _ => _sigIndex(eraseRuntimeSig(sig))
      }
    }

    def indexFromMetadata(mt: MethodType): Unit = indexFromMetadata(getMethodTypeSig(mt))
    def indexFromMetadata(sig: Sig): Unit = indexSig(sig)

    def indexFromCode(mt: MethodType): Unit = indexFromCode(getMethodTypeSig(mt))
    def indexFromCode(m: Method, sig: SignatureType | InstantiatedGenericMethod): Unit = {
      assert(!frozen)
      val containsTypeVariables = sig match {
        case s: SignatureType => s.containsTypeVariables
        case s: InstantiatedGenericMethod => s.containsTypeVariables
      }
      if (containsTypeVariables) {
        indexFromMetadata(sig)
        val ftcSymbol = sig match {
          case s: (InstantiatedType | InstantiatedGenericMethod) => getGenericTypeSig(s)
          case s: LocalTypeVariable => s
          case _ => shouldNotReachHere(sig)
        }
        ftc.addSymbol(m, ftcSymbol)
      } else {
        indexFromCode(sig)
      }
    }

    private[cbc] def indexFromCode(sig: Sig): Unit = {
      codeSignatures += sig
      indexSig(sig)
    }

    private[cbc] def getMethodTypeSig(mt: MethodType) = mt.dropReceiverParameter.signature

    private def getGenericTypeSig(sig: InstantiatedEntity) = typeTerms.getOrElse(sig, instantiatedEntities(sig))

    private def indexSig(sig: Sig): Unit = {
      assert(!frozen)
      if (allSignatures.add(eraseRuntimeSig(sig))) {
        sig match {
          case sig: NullableWrapper =>
            indexSig(sig.baseType)
          case sig: NonNullableWrapper =>
            indexSig(sig.baseType)
          case sig: CangjieArray =>
            indexSig(sig.elemType)
          case sig: ArraySlice =>
            indexSig(sig.elemType)
            indexType(sig.symType)
          case sig: JavaArray =>
            indexSig(sig.baseType)
          case sig: CangjieEnumWrapper =>
            strings(sig.name) = 0
            indexSig(sig.baseType)
          case sig: CPointer =>
            indexSig(sig.pointee)
          case sig: MethodSignature =>
            indexSig(sig.returnType)
            sig.parameterTypes foreach indexSig
          case sig: VArray =>
            indexSig(sig.elemType)
          case sig: ConstraintType =>
            indexType(sig)
            sig.supers foreach indexSig
          case _: (LocalTypeVariable | GenericEntity | GenericTypeTerm) =>
            // nothing to index here
          case sig: InstantiatedType =>
            instantiatedEntities.getOrElseUpdate(sig, {
              val symType = sig.symType
              indexType(symType)

              val genericType = sig match {
                case _: InstantiatedReference => GenericReference(symType, sig.instantiatedTypeParameters map eraseTypeVariables)
                case _: InstantiatedRecord => GenericRecord(symType, sig.instantiatedTypeParameters map eraseTypeVariables)
              }
              indexSig(genericType)

              createGtt(genericType, sig, sig.instantiatedTypeParameters) foreach indexSig

              genericType
            })
          case sig: InstantiatedGenericMethod =>
            instantiatedEntities.getOrElseUpdate(sig, {
              val methodRef = sig.mr
              indexMethodRef(methodRef)

              val genericMethod = GenericMethod(methodRef, sig.instantiatedTypeParameters map eraseTypeVariables)
              indexSig(genericMethod)

              createGtt(genericMethod, sig, sig.instantiatedTypeParameters) foreach indexSig

              genericMethod
            })
          case _: BuiltinSignature =>
            // already indexed, do nothing
          case sig: Reference =>
            indexType(sig.symType)
          case sig: Record =>
            indexType(sig.symType)
          case _: Primitive | BString =>
            // TODO refactor providing clearer dependency between BuiltinSignature and Primitive/BString.
            //      As BuiltinSignature already involves both Primitive/BString this case should be removed.
            // do nothing
          case _: Signature =>
            shouldNotReachHere(sig)
        }
      }
    }

    private def eraseRuntimeSig(sig: Sig): Sig = sig match {
      case sig: JBCReference if !sig.isJavaReference =>
        if sig.symType.isCompilerInterface then BuiltinSignature.RuntimeLib.sig else BuiltinSignature.AnyReference.sig
      case sig => sig
    }

    private def eraseTypeVariables(signatureType: SignatureType) = signatureType match {
      case _: LocalTypeVariable =>
        // no need to index builtin signatures
        BuiltinSignature.Nil
      case sig: InstantiatedType =>
        indexSig(sig)
        instantiatedEntities(sig).asInstanceOf[GenericType]
      case sig =>
        indexSig(sig)
        sig
    }

    // TODO: support uleb128 fixups from instructions, so no restrictions on signatures from code are left
    /** Freeze signature indices from metadata and code, so final index will contain:
      *  1. [[MAX_BUILTIN_SIG_INDEX]] built-in signatures
      *  1. less or equal to [[MAX_SIG_INDEX_FROM_CODE]] signatures referenced from code
      *  1. signatures that are referenced from metadata
      */
    def freeze(): Unit = {
      assert(!frozen)
      val newSigIndex = initSigIdxWithBuiltinSignatures()
      assert(newSigIndex.size == MAX_BUILTIN_SIG_INDEX)
      assert(newSigIndex.size + codeSignatures.size <= MAX_SIG_INDEX_FROM_CODE)

      codeSignatures.iterator ++ allSignatures map {
        case sig: InstantiatedEntity => getGenericTypeSig(sig)
        case sig => sig
      } foreach { newSigIndex.getOrElseUpdate(_, newSigIndex.size) }

      _sigIndex = newSigIndex
      frozen = true
    }

    private def initSigIdxWithBuiltinSignatures(): mutable.LinkedHashMap[Sig, Index] = {
      val res = mutable.LinkedHashMap.empty[Sig, Index]
      for (builtin <- BuiltinSignature.values) {
        val builtinIdx = builtin.idx
        assert(builtinIdx < MAX_BUILTIN_SIG_INDEX)
        res(builtin.sig) = builtinIdx
      }
      res
    }

    def size(): Int = {
      assert(frozen)
      _sigIndex.size
    }

    def writeSignatures(): Unit = {
      for (sig <- sigIndexKeysWithoutBuiltin()) {
        signatures(sig) = getCurPos
        writeSig(sig)
      }
    }

    private def writeSig(sig: Sig): Unit = {
      assert(frozen)

      def writeTag(st: SignatureTag): Unit = {
        writeByte(st.tag)
      }

      sig match {
        case sig: NullableWrapper =>
          writeTag(SignatureTag.Nullable)
          writeULEB128(sigIndex(sig.baseType))
        case sig: NonNullableWrapper =>
          writeTag(SignatureTag.NonNullable)
          writeULEB128(sigIndex(sig.baseType))
        case sig: Record =>
          writeTag(SignatureTag.Record)
          writeULEB128(typeIndex(sig.symType))
        case sig: CangjieReference =>
          writeTag(SignatureTag.Reference)
          writeULEB128(typeIndex(sig.symType))
        case sig: JBCReference if sig.isJavaReference =>
          writeTag(SignatureTag.JavaReference)
          writeInt(strings(sig.symType.getName))
        case sig: CangjieArray =>
          writeTag(SignatureTag.CangjieArray)
          writeULEB128(sigIndex(sig.elemType))
        case sig: ArraySlice =>
          writeTag(SignatureTag.Record)
          writeULEB128(typeIndex(sig.symType))
        case sig: JavaArray =>
          writeTag(SignatureTag.JavaArray)
          writeByte(sig.dimNum)
          writeULEB128(sigIndex(sig.baseType))
        case sig: CangjieEnumWrapper =>
          writeTag(SignatureTag.EnumWrapper)
          writeULEB128(sigIndex(sig.baseType))
          writeInt(strings(sig.name))
        case sig: CPointer =>
          writeTag(SignatureTag.CPointer)
          writeULEB128(sigIndex(sig.pointee))
        case sig: MethodSignature =>
          writeTag(SignatureTag.MethodSignature)
          assert(sig.parameterTypes.size <= 255)
          writeByte(sig.parameterTypes.size)
          for (paramType <- sig.parameterTypes) {
            writeULEB128(sigIndex(paramType))
          }
          writeULEB128(sigIndex(sig.returnType))
        case sig: VArray =>
          writeTag(SignatureTag.VArray)
          writeULEB128(sigIndex(sig.elemType))
          writeLongULEB128(sig.length)
        case sig: Constraint =>
          writeTag(SignatureTag.Constraint)
          writeULEB128(typeIndex(sig))
        case sig: ParameterizedConstraint =>
          writeTag(SignatureTag.ParameterizedConstraint)
          writeULEB128(typeIndex(sig))
          writeByte(sig.freeVariables.size)
          sig.freeVariables map(_.idx) foreach writeByte
        case sig: LocalTypeVariable =>
          writeTag(SignatureTag.GenericTypeVar)
          writeByte(sig.idx)
        case sig: GenericReference =>
          writeTag(SignatureTag.GenericReference)
          writeULEB128(typeIndex(sig.tpe))
          writeByte(sig.typeParameters.size)
          sig.typeParametersSigIndices foreach writeULEB128
        case sig: GenericRecord =>
          writeTag(SignatureTag.GenericRecord)
          writeULEB128(typeIndex(sig.tpe))
          writeByte(sig.typeParameters.size)
          sig.typeParametersSigIndices foreach writeULEB128
        case sig: GenericMethod =>
          writeTag(SignatureTag.GenericMethod)
          writeULEB128(methodRefIndex(sig.methodRef))
          writeByte(sig.typeParameters.size)
          sig.typeParametersSigIndices foreach writeULEB128
        case sig: GenericTypeTerm =>
          writeTag(SignatureTag.GenericTypeTerm)
          writeULEB128(sigIndex(sig.generic))
          writeByte(sig.freeVariablesIndices.size)
          sig.freeVariablesIndices foreach writeByte
        case x => shouldNotReachHere(x)
      }
    }

    def writeSigIndex(): Unit = {
      assert(frozen)
      for (sig <- sigIndexKeysWithoutBuiltin()) {
        writeInt(signatures(sig))
      }
    }

    private def sigIndexKeysWithoutBuiltin(): Iterator[Sig] = {
      assert(frozen)
      _sigIndex.iterator collect { case (k, v) if v >= MAX_BUILTIN_SIG_INDEX => k }
    }

    private[cbc] enum SignatureTag(val tag: Byte) {
      case Nil extends SignatureTag(0x00)
      case Record extends SignatureTag(0x01)
      case Reference extends SignatureTag(0x02)
      case CangjieArray extends SignatureTag(0x03)
      case VArray extends SignatureTag(0x04)
      case EnumWrapper extends SignatureTag(0x05)
      case CPointer extends SignatureTag(0x06)
      case GenericTypeTerm extends SignatureTag(0x07)
      case GenericTypeVar extends SignatureTag(0x08)
      case GenericRecord extends SignatureTag(0x09)
      case GenericReference extends SignatureTag(0x0a)
      case Nullable extends SignatureTag(0x0b)
      case MethodSignature extends SignatureTag(0x0c)
      case GenericMethod extends SignatureTag(0x0d)
      case Constraint extends SignatureTag(0x0e)
      case ParameterizedConstraint extends SignatureTag(0x0f)
      case JavaReference extends SignatureTag(0x10)
      case JavaArray extends SignatureTag(0x11)
      case NonNullable extends SignatureTag(0x12)
    }

    private[cbc] enum BuiltinSignature(val sig: Signature, val idx: Byte) {
      case Nil extends BuiltinSignature(null, 0x00)
      case Void extends BuiltinSignature(SignatureType.Void, 0x01)
      case Unit extends BuiltinSignature(SignatureType.Unit, 0x02)
      case Nothing extends BuiltinSignature(SignatureType.Nothing, 0x03)
      case Boolean extends BuiltinSignature(SignatureType.Boolean, 0x04)
      case I8 extends BuiltinSignature(SignatureType.Int8, 0x05)
      case U8 extends BuiltinSignature(SignatureType.UInt8, 0x06)
      case I16 extends BuiltinSignature(SignatureType.Int16, 0x07)
      case U16 extends BuiltinSignature(SignatureType.UInt16, 0x08)
      case I32 extends BuiltinSignature(SignatureType.Int32, 0x09)
      case U32 extends BuiltinSignature(SignatureType.UInt32, 0x0a)
      case UChar32 extends BuiltinSignature(SignatureType.UnicodeChar32, 0x0b)
      case I64 extends BuiltinSignature(SignatureType.Int64, 0x0c)
      case U64 extends BuiltinSignature(SignatureType.UInt64, 0x0d)
      case IAddr extends BuiltinSignature(SignatureType.AddrInt, 0x0e)
      case UAddr extends BuiltinSignature(SignatureType.AddrUInt, 0x0f)
      case BString extends BuiltinSignature(SignatureType.BString, 0x10)
      case F16 extends BuiltinSignature(SignatureType.Float16, 0x11)
      case F32 extends BuiltinSignature(SignatureType.Float32, 0x12)
      case F64 extends BuiltinSignature(SignatureType.Float64, 0x13)

      case AnyReference extends BuiltinSignature(SignatureType.fromSymType(typeProvider.getAJObjectType), 0x14)
      case RuntimeLib extends BuiltinSignature(SignatureType.fromSymType(typeProvider.getCompilerInterfaceType), 0x15)
    }

    private[cbc] object ConstraintType {
      private[cbc] val typeNamesCounter = mutable.HashMap.empty[String, Int]
      private val pool = mutable.HashMap.empty[ConstraintType, ConstraintType]

      private[cbc] def apply(constraint: GenericInfo.Constraint) = {
        val freeVariables = collectFreeVariables(constraint.upperBounds)
        val constraintType = if (freeVariables.isEmpty) {
          Constraint(mutable.LinkedHashSet.from(constraint.upperBounds))

        } else {
          val freeVariablesSet = mutable.LinkedHashSet.from(freeVariables)
          val constraintTypeParameters = freeVariablesSet.zipWithIndex.toMap
          def replaceTypeVariables(types: Seq[SignatureType]): Seq[SignatureType] = types map {
            case sig: InstantiatedReference =>
              sig.copy(instantiatedTypeParameters = replaceTypeVariables(sig.instantiatedTypeParameters))
            case sig: InstantiatedRecord =>
              sig.copy(instantiatedTypeParameters = replaceTypeVariables(sig.instantiatedTypeParameters))
            case sig: LocalTypeVariable =>
              sig.copy(idx = constraintTypeParameters(sig))
            case sig =>
              sig
          }
          val supers = replaceTypeVariables(constraint.upperBounds)
          ParameterizedConstraint(mutable.LinkedHashSet.from(supers), freeVariablesSet)
        }

        pool.getOrElseUpdate(constraintType, constraintType)
      }

      case class ConstraintTypeMethod(name: String, sourceSig: MethodSignature)
    }

    private[cbc] sealed abstract class ConstraintType {
      assert(supers.nonEmpty)

      lazy val name = {
        val typeNames = supers.toSeq map {
          case upperBound: Reference => upperBound.symType.getName
          case upperBound: InstantiatedReference => upperBound.name
          case upperBound => shouldNotReachHere(upperBound)
        }
        val typeName = s"$$${typeNames.sorted.mkString}"
        val typeNameIdx = ConstraintType.typeNamesCounter.updateWith(typeName) {
          case Some(counter) => Some(counter + 1)
          case None => Some(0)
        }.get
        s"${typeName}_${typeNameIdx}_CONDESC"
      }

      def supers: collection.Set[SignatureType]
    }
    private[cbc] case class Constraint(supers: collection.Set[SignatureType]) extends ConstraintType
    private[cbc] case class ParameterizedConstraint(supers: collection.Set[SignatureType], freeVariables: collection.Set[LocalTypeVariable]) extends ConstraintType {
      assert(freeVariables.nonEmpty)
    }

    private[cbc] type GenericTypeParameter = SignatureType | GenericType | BuiltinSignature

    private[cbc] sealed trait GenericEntity {
      assert(typeParameters.nonEmpty)
      typeParameters foreach {
        case tp: BuiltinSignature => assert(tp == BuiltinSignature.Nil, s"$tp != ${BuiltinSignature.Nil}")
        case tp: InstantiatedEntity => shouldNotReachHere(tp)
        case _ => // do nothing
      }

      def typeParameters: Seq[GenericTypeParameter]
      def typeParametersSigIndices = {
        assert(frozen)
        typeParameters map {
          case sig: BuiltinSignature => sig.idx.toInt
          case sig => sigIndex(sig)
        }
      }
    }

    private[cbc] sealed trait GenericType extends GenericEntity {
      def tpe: Type
    }
    private[cbc] case class GenericReference(tpe: Type, typeParameters: Seq[GenericTypeParameter]) extends GenericType
    private[cbc] case class GenericRecord(tpe: Type, typeParameters: Seq[GenericTypeParameter]) extends GenericType

    private[cbc] case class GenericMethod(methodRef: MethodReference, typeParameters: Seq[GenericTypeParameter]) extends GenericEntity

    private[cbc] case class GenericTypeTerm(generic: GenericEntity, freeVariablesIndices: collection.Seq[Int])

    // FIXME-UG add tests
    private[cbc] object ftc {

      private[cbc] type FTCSymbol = GenericEntity | LocalTypeVariable | GenericTypeTerm

      private val classesPools = mutable.LinkedHashMap.empty[ClassType, FTCPool]

      private val rawFTCStrings = mutable.LinkedHashMap.empty[Method, RawFTCString]
      private val ftcStrings = mutable.HashMap.empty[Method, FTCString]

      private var written = false

      def addSymbol(m: Method, ftcSymbol: FTCSymbol): Unit = {
        assert(!frozen)
        if (m.hasUniversalGenericContext) {
          // FIXME-UG: also generic super-interfaces with non-overridden default methods must be prohibited/supported
          rawFTCStrings.getOrElseUpdate(m, RawFTCString(m)).addSymbol(ftcSymbol)
        } else {
          shouldNotReachHere(s"Method $m is not in universal generic context")
        }
      }

      def writeSymbols(): Unit = {
        assert(frozen)
        assert(!ftc.written)

        def isGenericMethod(m: Method) = {
          assert(m.hasUniversalGenericContext)
          m.isUniversalGeneric
        }

        def generateClassesPools = {
          // FIXME-UG support FTC-pool for generic classes with generic super classes
          val rawStrings = mutable.LinkedHashMap.empty[ClassType, ArrayBuffer[(Method, RawFTCString)]]
          for ((m, rawString) <- rawFTCStrings if !isGenericMethod(m)) {
            val classRawStrings = rawStrings.getOrElseUpdate(m.getDeclaringClass, ArrayBuffer.empty[(Method, RawFTCString)])
            classRawStrings += m -> rawString
          }
          rawStrings
        }

        def writeFTCString(rs: RawFTCString): FTCString = {
          val ftcStringOffset = getCurPos
          val segmented = rs.buildSegmentedString
          segmented.indices.keys map sigIndex.apply foreach writeULEB128
          FTCString(ftcStringOffset, segmented)
        }

        for ((t, methodRawStrings) <- generateClassesPools) {
          val classPoolOffset = getCurPos
          val ftcStringRefs = mutable.LinkedHashMap.empty[Method, FTCStringRef]

          var ftcStringPos = 0
          for ((m, rawString) <- methodRawStrings) {
            val ftcString = writeFTCString(rawString)

            ftcStringRefs.put(m, FTCStringRef(ftcStringPos, ftcString))
            ftcStringPos += ftcString.length
          }

          classesPools.put(t, FTCPool(classPoolOffset, ftcStringRefs))
        }

        for ((m, rawFTCString) <- rawFTCStrings if isGenericMethod(m)) {
          ftcStrings.put(m, writeFTCString(rawFTCString))
        }

        ftc.written = true
      }

      def ftcClassPool(t: ClassType): Option[FTCPool] = {
        assert(ftc.written)
        classesPools.get(t)
      }

      def ftcString(m: Method): Option[FTCString] = {
        assert(ftc.written)
        ftcStrings.get(m)
      }

      def ftcStringRef(m: Method): Option[FTCStringRef] = {
        ftcClassPool(m.getDeclaringClass) flatMap (_.ftcStringRef(m))
      }

      case class FTCPool(offset: Offset, private val ftcStrings: collection.Map[Method, FTCStringRef]) {
        def length = ftcStrings.valuesIterator.map(_.length).sum

        def ftcStringRef(m: Method) = {
          ftcStrings.get(m)
        }
      }

      case class FTCStringRef(startIdx: Int, ftcString: FTCString) {
        def length = ftcString.length
      }

      case class FTCString(offset: Offset, private val str: SegmentedFTCString) {
        def length = str.indices.size

        def getSymbolIdx(s: FTCSymbol): Index = {
          assert(ftc.written)
          str.indices(s)
        }

        def getSymbolsInSegment(sk: SegmentKind): Iterable[FTCSymbol] = {
          assert(ftc.written)
          str(sk).keys
        }
      }

      private class RawFTCString(m: Method) {
        private val symbols = mutable.LinkedHashSet.empty[FTCSymbol]

        def addSymbol(symbol: FTCSymbol): Unit = {
          assert(!frozen)
          symbols.add(symbol)
          assert(symbols.size <= MAX_SIG_INDEX_FROM_CODE)
        }

        /** Sorts out symbols in FTC-string by proper segments. */
        def buildSegmentedString: SegmentedFTCString = {
          assert(frozen)

          def collectOHMTypes: mutable.LinkedHashSet[FTCSymbol] = {
            val res = mutable.LinkedHashSet.empty[FTCSymbol]
            // TODO check if all methods exist
            for (ohm <- methodsCode(m).variableSizeTypes) {
              val s = ohm.asInstanceOf[FTCSymbol]
              assert(symbols.contains(s))
              res.add(s)
            }
            res
          }

          // TODO scalify the code
          var newSymbols = collectOHMTypes
          val segmentStartIndices = Array.fill(SegmentKind.values.length)(-1)

          if (newSymbols.isEmpty) {
            segmentStartIndices(SegmentKind.OTHER.idx) = 0
            newSymbols = symbols
          } else {
            segmentStartIndices(SegmentKind.VST.idx) = 0
            SegmentKind.VST.next foreach (next => segmentStartIndices(next.idx) = newSymbols.size)
            newSymbols ++= symbols
          }

          SegmentedFTCString(mutable.LinkedHashMap.from(newSymbols.zipWithIndex), segmentStartIndices)
        }
      }

      private[cbc] class SegmentedFTCString(private[cbc] val indices: collection.Map[FTCSymbol, Index],
                                            private val segmentStartIndices: Array[Int]) {
        assert(segmentStartIndices.length == SegmentKind.values.length)

        def apply(segment: SegmentKind = SegmentKind.OTHER): collection.Map[FTCSymbol, Index] = {
          val startIdx = segmentStartIndices(segment.idx)
          val endIdx = segment.next map(_.idx) getOrElse indices.size
          if (startIdx == -1 || endIdx == -1) {
            Map.empty
          } else {
            indices.slice(startIdx, endIdx)
          }
        }
      }

      private[cbc] enum SegmentKind {
        case VST, OTHER // FIXME-UG add other segments, if necessary.

        def idx = ordinal

        def next: Option[SegmentKind] = this match {
          case VST   => Some(OTHER)
          case OTHER => None
        }
      }
    }
  }
}
