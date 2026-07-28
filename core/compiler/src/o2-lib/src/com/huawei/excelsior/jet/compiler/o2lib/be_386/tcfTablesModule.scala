/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386

import com.huawei.excelsior.jet.assembler.fixups.RelocationKind
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.o2lib.be_386.CodeDefModule.Segment
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule as cd, opAttrsModule as at}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, NumerateModule as Numerate, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule as js
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.{getMethodFrameDescriptor, getO2Method, methodByO2Object, typeToO2Class}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{DataGen, ExteriorMethodsVersioning, VersionedMethod}
import com.huawei.excelsior.jet.compiler.{CodeMetadata, RTConst}
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.*
import xscala.util.UShort

import scala.collection.mutable.ArrayBuffer

object tcfTablesModule {

  private case class Table(method: Method, bodyLength: Int, methodNum: Int, metadata: CodeMetadata, versioned: VersionedMethod) {
    def xTableLen: Int = if (metadata.xTable != null) metadata.xTable.length else 0
    def isVersioned = versioned != null
    def frameDescriptor: pc.Symbol = if (isVersioned) versioned.getFrameDescriptor else getMethodFrameDescriptor(method)
  }

  class tcf_tables_info {
    var fdmap: pc.Symbol = _
    var frameDescriptors: pc.Symbol = _
    var importList: pc.Symbol = _
    var aotCPData: pc.Symbol = _
  }

  private var tables: ArrayBuffer[Table] = _
  private var versionsCount: Int = _

  private def addTable(method: Method, methodNum: Int, versioned: VersionedMethod = null): Unit = {
    val isVersioned = versioned != null
    val (bodyObj, bodySymbol) = if (isVersioned) {
      assert(versioned.method == method)
      (versioned.bodyObj, versioned.getSymbol)
    } else {
      (getO2Method(method), method)
    }
    if (bodySymbol.ownsSegment && method.hasManagedExecEnv && method.hasFrameDescriptor) {
      tables += Table(method, at.getSegment(bodyObj).length, methodNum, at.getMetadata(bodyObj), versioned)
      if (versioned != null) {
        versionsCount += 1
      }
    }
  }

  private def outXTable(): pc.Symbol = {
    val seg = cd.newSeg()
    val (gcAwareOrManual, notGCAware) = tables.filter(_.metadata.xTable != null).partition(t => t.method.isGCAware || t.method.isManual)
    for (table <- notGCAware ++ gcAwareOrManual) seg.append(table.metadata.xTable)

    if (seg.length == 0) {
      null
    } else {
      seg.alignData(4)
      at.setSegment(at.newUnsizedConst(js.format("$$tcfTable")), seg)
    }
  }

  private def fixupRVA(obj: pc.Symbol): Unit = {
    if (obj == null) {
      cd.genLWord(0)
    } else {
      cd.addFixup(RVA_32, obj, 0)
    }
  }

  private def genWord(x: Int): Unit = {
    assert(x == (x & 0xFFFF) || (x & -0x8000) == -0x8000) // two bytes, possibly with sign extension
    cd.genWord(x.toShort)
  }

  private def genDword(x: Int): Unit = {
    // little-endian
    genWord(x & 0xFFFF)
    genWord(O2JSupport.logicLeftShift(x, 32, -16))
  }

  private def getFlags(table: Table): Short = {
    var x = 0
    def set(bit: Int, cond: Boolean): Unit = if (cond) x |= (1 << bit)

    set(RTConst.MethodInfoFrameDescriptor.LIGHTWEIGHT_FRAME_BIT.intValue,              table.metadata.frameIsLightweight && table.method.hasManagedExecEnv)
    set(RTConst.MethodInfoFrameDescriptor.DIRTY_FOR_CLASS_GC_FRAME_BIT.intValue,       table.metadata.dirtyForClassGCFrame)
    set(RTConst.MethodInfoFrameDescriptor.IS_VERSIONED_FLAG_BIT.intValue,              table.isVersioned)
    set(RTConst.MethodInfoFrameDescriptor.IS_INTERPRETER_INTERNALS_FLAG_BIT.intValue,  table.method.getDeclaringClass.isInterpreterInternals)
    set(RTConst.MethodInfoFrameDescriptor.HAS_MARKED_REGIONS_FLAG_BIT.intValue,        table.metadata.hasMarkedRegions)
    set(RTConst.MethodInfoFrameDescriptor.WITH_SIBERIA_OFFSET_BIT.intValue,            table.metadata.siberiaOffset != RTConst.MethodInfoFrameDescriptor.UNKNOWN_SIBERIA_OFFSET.intValue)

    x.toShort
  }

  private def outFdmap(frameDescriptors: pc.Symbol, xtable: pc.Symbol): Segment = {
    val sizeMask: Int = RTConst.FrameDescriptorsInitInfo.SIZE_MASK.intValue
    val sizeShift: Int = RTConst.FrameDescriptorsInitInfo.SIZE_SHIFT.intValue
    val deltaLimit = RTConst.FrameDescriptorsInitInfo.DELTA_MAX.intValue
    val sizeLimit = UShort.MaxValue - 1
    val bigDeltaLimit: Int = O2JSupport.logicLeftShift(sizeMask, 32, -sizeShift)
    val xTableChunkCountMask: Int = RTConst.FrameDescriptorsInitInfo.XTABLE_CHUNK_COUNT_MASK.intValue
    val xTableChunkCountWideMask = xTableChunkCountMask | O2JSupport.logicLeftShift(0xFFFF, 32, 16)
    val xTableLongChunkCountFlagShift: Int = RTConst.FrameDescriptorsInitInfo.XTABLE_LONG_CHUNK_COUNT_FLAG_SHIFT.intValue

    assert(tables.nonEmpty)

    cd.makeSeg {
      fixupRVA(xtable)
      fixupRVA(frameDescriptors)

      var lastNum = RTConst.FrameDescriptorsInitInfo.METHOD_NUM_START.intValue

      var xTableOffs = 0
      for (table @ Table(method, bodyLength, methodNum, metadata, _) <- tables if !method.isGCAware && !method.isManual) { // GCAware and Manual methods have pre-initialized frame descriptors
        assert(methodNum > lastNum)
        var delta = methodNum - lastNum

        // If you really want managed frames with more than 2^16 frame slots,
        // you would need to thoroughly refactor FrameDescriptors and GCMapsDecoder, see JET-11548.
        val frameSize = metadata.frameSize / addressSize
        assert(0 <= frameSize && frameSize <= RTConst.FrameDescriptorsInitInfo.MAX_FRAME_SIZE.intValue)

        val siberiaOffset = metadata.siberiaOffset

        val size = Numerate.mkAlign(bodyLength, RTConst.MethodInfoFrameDescriptor.CODE_ALIGNMENT.intValue)
        val large = size > sizeLimit

        while (large && delta > 1 || !large && delta > deltaLimit) {
          // emit bigDelta entries
          var rem = 1
          if (delta > bigDeltaLimit) {
            rem += delta - bigDeltaLimit
          }
          val x = O2JSupport.logicLeftShift(delta - rem, 32, sizeShift)
          genWord(x | RTConst.FrameDescriptorsInitInfo.BIG_DELTA_MARK.intValue) // UInt16 sizeAndDelta;
          delta = rem
        }

        if (large) {
          val x = size & sizeMask
          genWord(x | RTConst.FrameDescriptorsInitInfo.LARGE_SEGMENT_MARK.intValue) // UInt16 sizeAndDelta;
          genWord(O2JSupport.logicLeftShift(size, 32, -16)) // UInt16 sizeHi;
        } else {
          genWord(size | delta) // UInt16 sizeAndDelta;
        }

        genWord(frameSize) // UInt16 frameSize;
        genWord(getFlags(table))

        if (table.isVersioned) {
          genWord(method.getHostedIndex)
          genWord(TypeMetaInfoGenerator.Imports.getImportedClassIdx(typeToO2Class(method.getDeclaringClass)))
        } else {
          assert(method.getHostedIndex == methodNum)
          assert(typeToO2Class(method.getDeclaringClass) eq at.currClass)
        }

        if (siberiaOffset != RTConst.MethodInfoFrameDescriptor.UNKNOWN_SIBERIA_OFFSET.intValue) {
          genDword(siberiaOffset) // gen siberiaOffset
        }

        // gen xtable info
        val xTableLen = table.xTableLen
        xTableOffs += xTableLen

        assert((xTableLen & 1) == 0)
        var xTableChunksCount = O2JSupport.div(xTableLen, RTConst.FrameDescriptorsInitInfo.XTABLE_CHUNK_SIZE.intValue)

        if (xTableChunksCount <= xTableChunkCountMask) {
          assert((xTableChunksCount & O2JSupport.logicLeftShift(1, 32, xTableLongChunkCountFlagShift)) == 0)
          genWord(xTableChunksCount)
        } else {
          assert(xTableChunksCount.toLong == (xTableChunksCount.toLong & xTableChunkCountWideMask))
          xTableChunksCount = xTableChunksCount | O2JSupport.logicLeftShift(1, 32, xTableLongChunkCountFlagShift)
          genDword(xTableChunksCount)
        }

        val regsBM = {
          val trivXHflag = (if (metadata.trivXHandler) 1 else 0).toShort.toInt
          assert(trivXHflag == 0 || trivXHflag == 1)
          val trivXHflagMask = O2JSupport.logicLeftShift(trivXHflag, 32, RTConst.FrameDescriptorsInitInfo.TRIVIAL_XHANDLER_FLAG_SHIFT.intValue)

          val regsBM = metadata.savedIRegsBitMap
          assert(regsBM == (regsBM & trivXHflagMask - 1))
          regsBM | trivXHflagMask
        }

        if (!RTConst.FrameDescriptorsInitInfo.WIDE_REGS_BIT_MAPS.boolValue) {
          genWord(regsBM)
          genWord(metadata.savedFRegsBitMap)
        } else {
          genDword(regsBM)
          genDword(metadata.savedFRegsBitMap)
        }

        lastNum = methodNum
      }

      cd.genWord(RTConst.FrameDescriptorsInitInfo.EOS.intValue.toShort) // UInt16 endOfStream;
    }
  }

  private def tieObject(seg: Segment, position: Int)(kind: RelocationKind, target: pc.Symbol, addend: Int): Unit = {
    cd.withSeg(seg) {
      cd.addFixupAt(position)(kind, target, if (kind == OFFS32_LOCAL) addend + position else addend)
    }
  }

  private def tieSegments(seg: Segment, position: Int)(kind: RelocationKind, target: Segment, name: String, addend: Int): pc.Symbol = {
    if (target == null) {
      return null
    }

    val obj = at.newUnsizedConst(js.format(name))
    at.setSegment(obj, target)
    tieObject(seg, position)(kind, obj, addend)
    obj
  }

  private def genGCAwareOrManualFrameDescriptor(table: Table, xTable: pc.Symbol, xTableOffset: Int): Segment = {
    val Table(method, bodyLength, _, metadata, _) = table
    val seg = cd.newSeg()

    val manual = table.method.isManual

    if (manual) {
      seg.putW64(RTConst.FrameDescriptor.Code.MANUAL_INTERNAL_FD.longValue) // code
    } else {
      seg.putW64(RTConst.FrameDescriptor.Code.GC_AWARE_NOT_PREPARED_FD.longValue) // code
    }

    seg.putW16(metadata.frameSize / addressSize)                                      // frameSize
    seg.alignData(addressSize)                                                        // -- alignment
    if (xTable != null) seg.addFixup(RVA_32, xTable, xTableOffset) else seg.putW32(0) // xTableOffset
    seg.putW32(RTConst.MethodInfoFrameDescriptor.NO_INLINE.intValue)                  // inlineListOffset (do not generate this info for gc-aware for now)
    seg.putW32(RTConst.InlineList.Cache.EMPTY.intValue)                               // inlineListCache
    seg.putW32(0)                                                                     // gcMapsOffset (to be updated in RT during preparation)
    seg.putW32(RTConst.MethodInfoFrameDescriptor.UNKNOWN_SIBERIA_OFFSET.intValue)     // siberiaOffset (do not generate this info for gc-aware for now)
    seg.putW16(0)                                                                     // methodIndex (do not generate this info for gc-aware for now)
    seg.putW16(getFlags(table))                                                       // flags
    var savedRegsBitMap = (metadata.savedIRegsBitMap.toLong << RTConst.PackedFDInfoImpl.CORE_REGS_SHIFT.intValue) | metadata.savedFRegsBitMap.toLong
    
    // GC aware and manual methods cannot have non-trivial exception handlers 
    assert(metadata.trivXHandler)
    savedRegsBitMap |= (1L << RTConst.PackedFDInfoImpl.TRIV_XHANDLER_FLAG_BIT.intValue)
    
    if (RTConst.FrameDescriptorsInitInfo.WIDE_REGS_BIT_MAPS.boolValue) {
      seg.putW64(savedRegsBitMap)                                                     // packedFDInfo.savedRegsBitMap
    } else {
      seg.putW32(savedRegsBitMap.toInt)                                               // packedFDInfo.savedRegsBitMap
      seg.putW32(0)                                                                   // packedFDInfo.padding (to properly align range)
    
    }
    seg.addFixup(if (targetArch.is64Bit) ADDR64 else ADDR32, method, 0)                         // range.start
    seg.putW32(Numerate.mkAlign(bodyLength, RTConst.MethodInfoFrameDescriptor.CODE_ALIGNMENT.intValue)) // range.length
    if (manual) {
      seg.putW16(RTConst.CodeRegion.Kind.MANUAL.intValue.toShort)                                       // range.kind
    } else {
      seg.putW16(RTConst.CodeRegion.Kind.GC_AWARE.intValue.toShort)                                     // range.kind
    }
    seg.putW16(0)                                                                                       // range.skipListInfo

    seg
  }

  private def buildTables(tcf_info: tcf_tables_info): Unit = {
    if (tables.isEmpty) {
      tcf_info.fdmap = null
      tcf_info.frameDescriptors = null

    } else {
      val gcAwareCount = tables count (_.method.isGCAware)
      val manualCount = tables count (_.method.isManual)
      val methodsCount = tables.length - versionsCount
      val arraySize = methodsCount - gcAwareCount - manualCount

      //--------- write xtable --------------
      val xTable = outXTable()

      //--------- create frame descriptors ------------
      if (arraySize + versionsCount > 0) {
        val arrayOffset = RTConst.FrameDescriptorsArray.array.offset
        tcf_info.frameDescriptors = at.newSizedData(js.format("$$frameDescriptorsArray"), arrayOffset + arraySize * RTConst.MethodAndTypeInfoFrameDescriptor.size + versionsCount * RTConst.VersionedMethodFrameDescriptor.size)

        var arrayIndex = 0
        for (i <- 0 until methodsCount) {
          assert(!tables(i).isVersioned)
          if (!tables(i).method.isGCAware && !tables(i).method.isManual) {
            assert(arrayIndex < arraySize)
            at.setBaseOffsAttr(tables(i).frameDescriptor, tcf_info.frameDescriptors, arrayOffset + arrayIndex * RTConst.MethodAndTypeInfoFrameDescriptor.size)
            arrayIndex += 1
          }
        }
        assert(arrayIndex == arraySize)

        for (i <- 0 until versionsCount) {
          assert(tables(methodsCount + i).isVersioned)
          at.setBaseOffsAttr(tables(methodsCount + i).frameDescriptor, tcf_info.frameDescriptors, arrayOffset + arraySize * RTConst.MethodAndTypeInfoFrameDescriptor.size + i * RTConst.VersionedMethodFrameDescriptor.size)
        }
        //--------- write frame descriptors map --------------
        val fds_sg = outFdmap(tcf_info.frameDescriptors, xTable)

        //-------- patch td (frame descriptors map) ----------
        if (at.currClass.hasManagedMetaInformation) {
          val offs = if (at.currClass.isInfectedAJClass) {
            RTConst.InfectedTypeHandle.frameDescriptorsInitInfo.offset
          } else {
            RTConst.HostingTypeHandle.frameDescriptorsInitInfo.offset
          }
          val td_sg = at.getSegment(at.currClass.typeHandle)
          tcf_info.fdmap = tieSegments(td_sg, offs)(RVA_32, fds_sg, "$$fdMap", 0)
        }
      } else {
        tcf_info.fdmap = null
        tcf_info.frameDescriptors = null
      }

      //--------- write frame descriptors for GCAware and Manual methods --------------
      var xTableOffset = tables.iterator.collect {case t if !t.method.isGCAware && !t.method.isManual => t.xTableLen} .sum

      for (table <- tables if table.method.isGCAware || table.method.isManual) {
        val seg = genGCAwareOrManualFrameDescriptor(table, xTable, xTableOffset)
        val frameDesc = getMethodFrameDescriptor(table.method)
        pcO.setPlainArrayLength(frameDesc, seg.length)
        at.setSegment(frameDesc, seg)
        xTableOffset += table.xTableLen
      }
    }

    if (at.currClass.hasManagedMetaInformation && !at.currClass.isAJArray && !at.currClass.isCangjieArray) {
      assert(TypeMetaInfoGenerator.Imports.importTableSeg != null)
      TypeMetaInfoGenerator.Imports.writeImport(null) // ensure that there is terminating 0

      //-------- patch td (import table) ----------
      val offs = if (at.currClass.isInfectedAJClass) {
        RTConst.InfectedTypeHandle.importedTypes.offset
      } else {
        RTConst.HostingTypeHandle.importedTypes.offset
      }
      var td_sg = at.getSegment(at.currClass.typeHandle)
      tcf_info.importList = tieSegments(td_sg, offs)(RVA_32, TypeMetaInfoGenerator.Imports.importTableSeg, "$$importTable", 0)

      //-------- gen preparation info and patch td ----------
      td_sg = at.getSegment(at.currClass.typeHandle)
      val prepInfo = DataGen.genPreparationInfo(at.currClass)
      if (prepInfo != null) {
        val offs = if (at.currClass.isInfectedAJClass) {
          RTConst.InfectedTypeHandle.preparationInfo.offset
        } else {
          RTConst.HostingTypeHandle.preparationInfo.offset
        }
        tieObject(td_sg, offs)(RVA_32, prepInfo, 0)
      }


      if (!at.currClass.isInfectedAJClass && !at.currClass.isAJManagedType && !at.currClass.isCangjieType && !at.currClass.isXScalaType) {
        //-------- gen AOT CP and patch MetaInfo ----------
        td_sg = at.getSegment(at.findSpecialObject(at.TDReflection))
        tcf_info.aotCPData = tieSegments(td_sg, RTConst.MetaInfo.aotConstantPoolData.offset)(OFFS32_LOCAL, TypeMetaInfoGenerator.AOTConstantPool.genAOTConstantPool(), "$$aotCPData", 0)
      } else {
        TypeMetaInfoGenerator.AOTConstantPool.ensureAOTConstantPoolIsEmpty()
      }
    } else {
      tcf_info.importList = null
      tcf_info.aotCPData = null
      TypeMetaInfoGenerator.AOTConstantPool.ensureAOTConstantPoolIsEmpty()
    }
  }

  def buildTcfTables(tcf_info: tcf_tables_info): Unit = {
    versionsCount = 0
    tables = new ArrayBuffer[Table]

    val c = at.currClass

    for ((method, index) <- c.symType.getGeneratedMethods.zipWithIndex) {
      addTable(method, index)
    }

    for (versioned <- ExteriorMethodsVersioning.getIteratorOverVersionedMethods(c)) {
      addTable(methodByO2Object(versioned.original), versioned.getHostedIndex, versioned)
    }

    buildTables(tcf_info)
    tables = null
  }
}
