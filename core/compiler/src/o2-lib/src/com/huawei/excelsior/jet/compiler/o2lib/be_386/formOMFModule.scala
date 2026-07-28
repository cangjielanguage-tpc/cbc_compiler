/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386

import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.*
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.DwarfLinker.HeaderInfo
import com.huawei.excelsior.jet.compiler.o2lib.be_386.CodeDefModule.Segment
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator
import com.huawei.excelsior.jet.compiler.o2lib.be_386.formOMFModule.XOMF
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule as cd, opAttrsModule as at, opDefModule as def0, tcfTablesModule as tcfTables}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{NumerateModule, pc, ObjNamesModule as nms, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.tools.ExportIds.{getExportID, getExportIDForMember}
import com.huawei.excelsior.jet.compiler.o2lib.tools.ExportNames
import com.huawei.excelsior.jet.compiler.o2lib.tools.NamesCommon.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule as xPDB
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xcVersionModule as xcVersion, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xjRTSModule as xjRTS
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenDebug
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.classByO2Object
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.VersionedMethod
import com.huawei.excelsior.o2j.runtime.*
import xscala.collection.IdentityHashMap
import xscala.util.MathUtils.isNBits

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object formOMFModule {

  /////////////////////////////////////////////////////////////////////////////
  // Objects addresses inside some segments

  private case class Address(seg: Int, offs: Int, extNum: Int)
  private var addresses: IdentityHashMap[pc.Symbol, Address] = _

  def setAddress(obj: pc.Symbol, seg: Int, offs: Int, extNum: Int = EXTDEF_Invalid): Unit = {
    addresses(obj) = Address(seg, offs, extNum)
  }

  private def hasAddress(obj: pc.Symbol): Boolean = addresses contains obj

  private def getAddress(obj: pc.Symbol): Address = addresses(obj)


  // Type INT = LONGINT
  // Type INT8 = SHORTINT
  // Type INT32 = LONGINT
  // Type REC_TYPE = byteArray

  private class ProcSegmentGenerator extends at.ModuleVisitor {

    private def genProcObject(o: pc.Symbol, nameForSeg: () => XString): Unit = {
      if (o.mno == at.currClass.mno && hasAddress(o) && getAddress(o).seg > XOMF.Segment.lastStdSeg) {
        val rsegm = at.getSegment(o)
        val name = nameForSeg()

        val align = rsegm.requiredSectionAlignmentLg
        assert(align >= XOMF.Segment.CODE.align.ordinal)
        val segClass = XOMF.SegmentClass.CODE
        val grpInx = XOMF.Segment.CODE.ordinal
        outXsegdef(name, grpInx, segClass, rsegm.length, XOMF.SegmentAlign.fromOrdinal(align))
      }
    }

    override def versionedMethod(m: VersionedMethod): Unit = {
      genProcObject(m.bodyObj, () => ExportNames.versionedMethodLinkageName(m, needClassName = false)) // name unique in .obj (w/o class)
    }

    override def otherObject(o: pc.Symbol): Unit = {
      genProcObject(o, () => ExportNames.linkageName(o, needClassName = false)) // name unique in .obj (w/o class)
    }
  }


  private class ObjectAllocator extends at.ModuleVisitor {

    override def otherObject(o: pc.Symbol): Unit = {
      if (o.mno != at.currClass.mno) {
        return
      }

      if (at.hasBaseOffsAttr(o)) {
        val baseOffs = at.getBaseOffsAttr(o)
        val baseAddress = getAddress(baseOffs.base)
        setAddress(o, baseAddress.seg, baseAddress.offs + baseOffs.offs)
        return
      }

      o match {
        case _: pc.DataSymbol.RW =>
          if (hasAddress(o)) {
            return
          }
          assert(!o.isInstanceOf[pcO.Member])
          assert(!at.hasBaseOffsAttr(o))
          if (o.ownsSegment) {
            allocateObjAt(o, XOMF.Segment.DATA)
          } else {
            allocateObjAt(o, XOMF.Segment.BSS)
          }
        case _: pcO.VersionedMethodBody =>
          allocateObjAt(o, XOMF.Segment.CODE)
        case _: pc.DataSymbol.Const =>
          allocateObjAt(o, if (hasRealFxups(o)) XOMF.Segment.DATA else XOMF.Segment.CONST)
      }
    }

    override def staticField(sf: pcO.StaticField): Unit = {
      if (hasAddress(sf) || sf.isExternal) {
        return
      }

      if (at.hasBaseOffsAttr(sf)) {
        val baseOffs = at.getBaseOffsAttr(sf)
        val base = baseOffs.base ensuring (_ eq at.findSpecialObject(at.StaticBundle))
        val baseAddress = getAddress(base)
        setAddress(sf, baseAddress.seg, baseAddress.offs + baseOffs.offs)
      } else {
        if (sf.ownsSegment) {
          allocateObjAt(sf, if (sf.hasDataAnnot) XOMF.Segment.CONST else XOMF.Segment.DATA)
        } else {
          allocateObjAt(sf, XOMF.Segment.BSS)
        }
      }
    }

    override def method(m: pcO.Method): Unit = {
      allocateObjAt(m, XOMF.Segment.CODE)
    }

    override def stringTable(strTable: pcO.StringTable): Unit = {
      if (strTable.ownsSegment) {
        assert(at.currClass.hasManagedMetaInformation)
        allocateObjAt(strTable, XOMF.Segment.JSTR)
      } else {
        assert(!at.currClass.hasManagedMetaInformation || strTable.getLength == 0)
        setAddress(strTable, XOMF.Segment.BSS.segNum, XOMF.Segment.BSS.size) // generate empty segment in BSS
      }
    }

    override def singletonObject(x: pc.DataSymbol.SingletonObject): Unit = {
      allocateObjAt(x, XOMF.Segment.BSS)
    }
  }


  private class fixup {
    private[formOMFModule] var type0: Int = _
    private[formOMFModule] var offs: Int = _
    private[formOMFModule] var tkind: Int = _
    private[formOMFModule] var trg: Int = _

    private[formOMFModule] def setup(fxtype: Int, offset: Int, targetKind: Int, target: Int): Unit = {
      type0 = fxtype
      offs = offset
      tkind = targetKind
      trg = target
    }
  }

  private class FX_PAIR extends fixup {
    private[formOMFModule] var srcNo: Int = _
  }

  private class ObjPlacer extends at.ModuleVisitor {
    override def otherObject(o: pc.Symbol): Unit = {
      if (o.mno == at.currClass.mno && o.ownsSegment && !at.hasBaseOffsAttr(o)) {
        val address = getAddress(o)
        val seg = at.getSegment(o)
        genReadySegm(address.seg, address.offs, seg)
      }
    }
  }

  private class ExternalsCollector extends at.ModuleVisitor {
    var extdefRecord: XOMFRecord = _

    override def absentContainer(type0: pc.DataSymbol.TypeHandle): Unit = {
      assert(at.currClass.hasManagedMetaInformation)
      outExtern(extdefRecord, type0, alien = false)
      this.otherObject(type0)
    }

    override def otherObject(o: pc.Symbol): Unit = {
      if (o.mno == at.currClass.mno) {
        if (externObject(o) && !hasAddress(o)) {
          outExtern(extdefRecord, o)
        } else if (o.ownsSegment) {
          outFixupTargets(extdefRecord, at.getSegment(o))
        }
      }
    }
  }

  /* ------------------------ Export ------------------------ */
  private class ExportWriter extends at.ModuleVisitor {
    private class ExportedObject(val exportId: Int, val obj: pc.Symbol)

    private val expobjs: ArrayBuffer[ExportedObject] = new ArrayBuffer[ExportedObject]

    def reset(): Unit = expobjs.clear()
    def getExportedObjects: ArrayBuffer[pc.Symbol] = expobjs.sortBy(_.exportId).map(_.obj)
    private def addAsExported(o: pc.Symbol, id: Int): Unit = {
      if (!optNoexport && id >= 0) {
        assert(!expobjs.exists(_.exportId == id))
        expobjs += new ExportedObject(id, o)
      }
    }

    override def otherObject(o: pc.Symbol): Unit = {
      // not exported
    }

    override def stringTable(strTable: pcO.StringTable): Unit = {
      doExport(strTable, XOMF_TYPE_DATA)
      addAsExported(strTable, getExportIDForMember(strTable))
    }

    override def versionedMethod(versioned: VersionedMethod): Unit = {
      val name = ExportNames.versionedMethodLinkageName(versioned)
      outPUBDEF(versioned.bodyObj, name, XOMF_TYPE_CODE)
      // do not add versioned to exported
    }

    override def method(m: pcO.Method): Unit = {
      if (m.isExternal) {
        assert(!m.ownsSegment)
      }

      if (m.isExternal || m.ownsSegment) {
        doExport(m, XOMF_TYPE_CODE, m.isExternal)
        addAsExported(m, getExportIDForMember(m))
      }
    }

    override def staticField(sf: pcO.StaticField): Unit = {
      if (sf.isExternal) {
        assert(!sf.ownsSegment)
      }
      doExport(sf, XOMF_TYPE_DATA, sf.isExternal)
      addAsExported(sf, getExportIDForMember(sf))
    }

    override def instanceDescriptor(desc: pc.DataSymbol.InstanceDescriptor): Unit = {
      doExport(desc, XOMF_TYPE_DATA)
      addAsExported(desc, getExportID(desc))
    }

    override def singletonObject(singleton: pc.DataSymbol.SingletonObject): Unit = {
      doExport(singleton, XOMF_TYPE_DATA)
      addAsExported(singleton, getExportID(singleton))
    }

    override def typeHandleBase(type0: pc.DataSymbol.TypeHandle): Unit = {
      doExport(type0, XOMF_TYPE_TD)
      addAsExported(type0, getExportID(type0))
    }

    override def runTimeTypeInfo(x: pc.DataSymbol.RunTimeTypeInfo): Unit = {
      doExport(x, XOMF_TYPE_DATA)
      addAsExported(x, getExportID(x))
    }

    override def thinTypeHandle(x: pc.DataSymbol.ThinTypeInfo, isInfected: Boolean): Unit = {
      // HeaderThinTypeHandle should not be used in code, so it doesn't need export name
      // If class is not infected, it only has ThinTD, so we need to export ThinTD with `XOMF_TYPE_TD` kind.
      // Otherwise, InfectedTD will be exported with that kind and ThinTD will be exported with `XOMF_TYPE_DATA`.
      doExport(x.thinTypeHandle, if (!isInfected) XOMF_TYPE_TD else XOMF_TYPE_DATA)
      addAsExported(x.thinTypeHandle, getExportID(x.thinTypeHandle))
    }

    override def absentContainer(type0: pc.DataSymbol.TypeHandle): Unit = {
      doExport(type0, XOMF_TYPE_ATD)
      // do not add absent to exported
    }
  }

  //------------------------------

  def generateFormOMF(): Unit = {
    val tcf_info: tcfTables.tcf_tables_info = new tcfTables.tcf_tables_info()

    if (env.errors.errDetected) {
      return
    }

    val name = mkOutName()

    XOMF.Segment.init()
    beginOutput(name)

    addresses = IdentityHashMap.empty[pc.Symbol, Address]

    writeHeader(at.currClass)
    writeNames()

    tcfTables.buildTcfTables(tcf_info)

    allocateObjs(tcf_info)

    writeExternals()

    writeSegments()

    at.iterateModule(objPlacer)

    writeExport()

    writeEnd()

    if (at.currClass.hasMetaInformation) {
      writePackage(at.currClass)
    }

    endOutput()

    addresses = null
  }

  def startDWARFObj(headerInfo: HeaderInfo): Unit = {
    if (!O2Env.env.enabled(GenDebug)) {
      return
    }
    val worker = env.config.equationOrDefault("worker", "")
    val name = XString.ascii("DWARF" + worker)

    XOMF.Segment.init()
    beginOutput(name)

    // HEADER
    val debugFmt: Byte = XOMF_DEBUG_FORMAT_BY_COMPILER
    writeXOMFHeader(headerInfo.source, headerInfo.name, headerInfo.uid, common = false, debugFmt)

    newCommentRecord().outNameJstr(jetVersionString()).outb(0).emit()

    new XOMFRecord(LNAMES).outNameJstr(XString("DWARF")).emit()
  }

  def outDWARFSection(SegInx: Int, section: XString, Seg: Segment, fxData: Array[Byte], fxLen: Int): Int = {
    outXsegdef(section, 1, XOMF.SegmentClass.DEBUG, Seg.length, XOMF.SegmentAlign._4)
    // segment data
    assert(Seg.fixups.isEmpty)
    genReadySegm(SegInx, 0, Seg)


    // until dwarf fixups are not converted into standard ones they goes in the next properly named segment
    val generatedSegments =
      if (fxLen <= 0) {
        1
      } else {
        val fxName = section.concat(XString("_FX"))
        outXsegdef(fxName, 1, XOMF.SegmentClass.DEBUG, fxLen, XOMF.SegmentAlign._1)
        outLedata(SegInx + 1, 0, fxData, fxLen)
        2
      }
    generatedSegments
  }

  def finishDWARFObj(): Unit = {
    if (!O2Env.env.enabled(GenDebug)) {
      return
    }

    writeEnd()

    endOutput()
  }
  // disable all export from this component
  private val optNoexport = env.config.option("noexport")

  // prefix for all names or NIL
  private val globalNamePrefix: XString = env.config.equation("globalNamePrefix")
  /* -------------------- OMF Record tags ---------------------- */
  private val COMENT: Int = 0x88
  private val EXTDEF: Int = 0x8C   /* External Names Definition Record */
  private val PUBDEF: Int = 0x91   /* Public Names Definition Record */
  private val LNAMES: Int = 0x96   /* List of Name Records */
  private val LEDATA: Int = 0xA1   /* Logical Enumerated Data Record */
  private val XOMF_HEADER: Int = 0xD0 /* XOMF Header Record */
  private val XOMF_SEGDEF: Int = 0xD1 /* Segment Definition Record */
  private val XOMF_OBJEND: Int = 0xD2 /* Object File End Record */
  private val XOMF_EXPDEF: Int = 0xD9 /* XOMF Class' Export Definition Record */
  private val XOMF_RAWDATA: Int = 0xDE /* raw data for link-time optimizations (strings) */
  private val XOMF_FIXUP: Int = 0xDF /* XOMF fixup record */
  /*----------------------  Output file  ---------------------------*/
  /* ----------- */
  private var objFile: xfs.RawFile = _
  /* -------------------- Low-Level Output -------------------- */
  /* ---------------- */
  private val outbuf: Array[Byte] = new Array[Byte](4096)
  private var outcnt: Int = _
  private val XOMF_SIGNATURE: Int = 0x464D4F58 // "XOMF";
  private val XOMF_VERSION: Int = targetArch match {
    case AMD64 => 14
    case ARM64 => 16
    case CBC => -1
  }
  private val XOMF_MODULE_NORMAL: Int = 0x1
  private val XOMF_ARCH_AMD64: Int = 0x2
  private val XOMF_ARCH_ARM64: Int = 0x4
  private val JETEditionFlag: Int = xjRTS.EDITION_ENTERPRISE
  private val XOMF_DEBUG_FORMAT_IS_EMPTY: Byte = 0x00;    // debug information is absent
  private val XOMF_DEBUG_FORMAT_BY_COMPILER: Byte = 0x05; // DWARF parts are provided by compiler
  //------------  N a m e  L i s t  -------------------------------
  //------------------

  private object XOMF {
    /**
      * The segment class is part of the segment (XOMF_SEGDEF = 0xD1) definition format in XOMF and
      * defines the "type" of its content.
      *
      * For more information see XOMF documentation, section 7.
      */
    enum SegmentClass {
      // According to the documentation, segment numbering starts with one.
      // So this value is created for the `ordinal` method to work correctly and should not be used.
      private case INVALID

      case CODE
      case DATA
      case BSS
      case RODATA
      case DEBUG
      case STACKTRACE
    }

    enum SegmentAlign {
      case _1
      case _2
      case _4
      case _8
      case _16
      case _32
      case _64
    }

    enum Segment(_segClass: SegmentClass, str: String, val align: SegmentAlign) {
      var size = 0 // TODO: use new segments in XOMF.Semgent and remove this

      case INVALID      extends Segment(null,                null,           null            )
      case EMPTY        extends Segment(null,                "",             null            )

      case CODE         extends Segment(SegmentClass.CODE,   null,           SegmentAlign._16)
      case DATA         extends Segment(SegmentClass.DATA,   "_DATA",        SegmentAlign._8 )
      case CONST        extends Segment(SegmentClass.RODATA, "CONST",        SegmentAlign._8 )
      case BSS          extends Segment(SegmentClass.BSS,    "_BSS",         SegmentAlign._8 )

      case THANDLE      extends Segment(SegmentClass.DATA,   "_THANDLE",     SegmentAlign._8 )
      case THIN_TDESC   extends Segment(SegmentClass.DATA,   "THIN_DESC",    SegmentAlign._8 )
      case REFL         extends Segment(SegmentClass.RODATA, "_REFL",        SegmentAlign._4 )
      case SFIELD       extends Segment(null,                "_SFIELD",      SegmentAlign._8 )
      case JSTR         extends Segment(SegmentClass.DATA,   "_JSTR",        SegmentAlign._8 )
      case TDINIT       extends Segment(SegmentClass.RODATA, "_TDINIT",      SegmentAlign._4 )
      case STI          extends Segment(SegmentClass.RODATA, "_STI",         SegmentAlign._4 )
      case VMTE         extends Segment(SegmentClass.RODATA, "_VMTE",        SegmentAlign._4 )

      case REORDER_TEXT extends Segment(SegmentClass.CODE,   "REORDER_TEXT", SegmentAlign._16)
      case REORDER_DATA extends Segment(SegmentClass.DATA,   "REORDER_DATA", SegmentAlign._8 )

      case BOOTSTRAP    extends Segment(SegmentClass.RODATA, null,           SegmentAlign._2 )

      def jstr: XString = this match {
        case CODE =>
          val s = env.config.equation("CODENAME")
          if (s != null && s.nonEmpty) s.toUpperCase else XString("_TEXT")
        case BOOTSTRAP =>
          if (at.currClass.isBootstrap && !at.currClass.isNonBootstrap)
            XString("BOOTSTRAP_ON")
          else
            XString("BOOTSTRAP_OFF")
        case _ => XString(str)
      }

      def printableName: Boolean = this.jstr != null

      def shouldBeWritten: Boolean = this.segClass != null

      def segClass = this match {
        case SFIELD => Segment.SFieldSegClass
        case _ => this._segClass
      }

      def segNum: Int = this.ordinal - 1
    }

    object Segment {
      val lastStdSeg = Segment.values.last.segNum

      def init(): Unit = {
        Segment.values.foreach(_.size = 0)

        segmCount = lastStdSeg + 1
      }

      var segmCount = 0
      var SFieldSegClass: SegmentClass = _
    }
  }


  //----------- Segment definitions --------------------------------
  private val SegRawData: Int = -1
  private val SegExtern: Int = 0

  /*-------------------- Object addresses --------------------------*/
  val EXTDEF_Invalid: Int = -1
  private val procSegmentGenerator: ProcSegmentGenerator = new ProcSegmentGenerator()
  private val objectAllocator: ObjectAllocator = new ObjectAllocator()
  /* -- Start new record -- */
  /* -------------- FIXUPs ------------------- */
  // target kinds
  private val TK_SEG: Int = 1
  private val TK_ID: Int = 2
  private val TK_RAWDATA: Int = 3
  // fixup kinds
  private val FX_ADDR32: Int = 1
  private val FX_OFFS32: Int = 2
  private val FX_TDINDEX16: Int = 4
  private val FX_BYTESTR32: Int = 6
  private val FX_ADDR64: Int = 10
  private val FX_RVA32: Int = 13
  private val FX_ARM64_B_BL_IMM26: Int = 18
  private val FX_ARM64_ADRP_IMM21: Int = 21
  private val FX_ARM64_ADRP_IMM27: Int = 25
  private val FX_ARM64_ADD_LO12: Int = 22
  // cnt of RawData bytestr records. # of 1st jstr RawData == RawDataCnt+1
  private var rawDataCnt: Int = _
  private var lastFxNo: Int = _ // last # of fixup in the fixups table
  // data kinds for XOMF_RAWDATA record
  private val DK_BYTESTR: Int = 1
  private val objPlacer: ObjPlacer = new ObjPlacer()
  /* ---------------- XOMF Import/Export Generation ---------------- */
  /* ----------------------------- */
  private val XOMF_TYPE_DATA: Int = 1 // this symbol belongs to DATA
  private val XOMF_TYPE_CODE: Int = 2 // this symbol belongs to CODE
  private val XOMF_TYPE_TD: Int = 5 // this symbol is a type descriptor
  private val XOMF_TYPE_ATD: Int = 9 // this symbol is an absent type descriptor
  private var extCnt: Int = _
  private val externalsCollector: ExternalsCollector = new ExternalsCollector()
  private val exportWriter: ExportWriter = new ExportWriter()
  //------------------------------
  private val jstrObjext: XString = js.newJString(".obj")

  private def createObjFile(name: XString): Unit = {
    val place = xPDB.findPlaceToWriteTo(name, xPDB.ContentType.OBJ)
    objFile = place.openAsRawForWrite()
  }

  private def closeObjFile(): Unit = {
    env.info.lines = objFile.lengthAsInt
    objFile.closeNew()
    objFile = null
  }

  private def flush(): Unit = {
    if (outcnt > 0) {
      objFile.writeBlock(outbuf, 0, outcnt)
    }
    outcnt = 0
  }

  def out(data: Array[Byte]): Unit = {
    val size = data.length

    assert(0 <= size && size <= data.length)
    if (size == 0) {
      return
    }
    if (size > outbuf.length) {
      flush()
      objFile.writeBlock(data, 0, size)
      return
    }
    val req = math.min(outbuf.length - outcnt, size)

    def copyToOutbuf(from: Int, size: Int): Unit = {
      Array.copy(data, from, outbuf, outcnt, size)
      outcnt += size
    }

    copyToOutbuf(0, req)
    val rSize = size - req
    if (rSize > 0) {
      flush()
      copyToOutbuf(req, rSize)
    }
  }

  private def dw2bytes(value: Int): (Byte, Byte, Byte, Byte) = {
    val b0 = (value & 0xFF).toByte
    val b1 = (O2JSupport.div(value, 256) & 0xFF).toByte
    val b2 = (O2JSupport.div(value, 256 * 256) & 0xFF).toByte
    val b3 = (O2JSupport.div(value, 256 * 256 * 256) & 0xFF).toByte

    (b0, b1, b2, b3)
  }

  private def write4b(data: Int): Unit = {
    val bytes = dw2bytes(data)
    out(Array[Byte](bytes._1, bytes._2, bytes._3, bytes._4))
  }

  private def beginOutput(name: XString): Unit = {
    createObjFile(name)
    outcnt = 0
  }

  private def endOutput(): Unit = {
    flush()
    closeObjFile()
  }

  private def newCommentRecord(class0: Int = 0, subtype: Int = 0): XOMFRecord = {
    new XOMFRecord(COMENT)
      .outb(0)
      .outb(class0)
      .outb(subtype)
  }

  private val REC_SZOFFS: Int = 1
  private val REC_SZLEN: Int = 4
  private val REC_MAX: Int = 50 * 1024

  private class XOMFRecord(private val tag: Int, private val emitWhenEmpty: Boolean = false) {
    private val arr = mutable.ArrayBuilder.make[Byte]
    initRecord()

    private def initRecord(): Unit = {
      assert(arr.length == 0)
      outb(tag) // record tag
      out4b(0, 0, 0, 0) // placehoder for record length
    }

    private def out4b(bytes: (Byte, Byte, Byte, Byte)): XOMFRecord = {
      arr += bytes._1
      arr += bytes._2
      arr += bytes._3
      arr += bytes._4
      this
    }

    private def isEmpty: Boolean = arr.length <= 1 + REC_SZLEN

    private def recRem: Int = (REC_MAX - arr.length) + 1 + REC_SZLEN

    def outb(b: Int): XOMFRecord = {
      assert(b >= 0 && b <= 255)
      arr += b.toByte
      this
    }

    def outdw(value: Int): XOMFRecord = out4b(dw2bytes(value))

    def outIndex(inx: Int): XOMFRecord = {
      assert(0 <= inx && inx <= 0x7FFF)
      if (inx <= 0x7F) {
        outb(inx)
      } else {
        outb(O2JSupport.div(inx, 0x100) + 0x80)
        outb(inx & 255)
      }
      this
    }

    def outByteArr(src: Array[Byte], len: Int): XOMFRecord = {
      if (len > 0) {
        arr.addAll(src, 0, len)
      }
      this
    }

    def outJstr(s: XString): XOMFRecord = {
      assert(s != null)
      for (i <- 0 until s.length) {
        outb(O2JSupport.convCharToInt(s.charAtAsChar(i)).toShort.toInt)
      }
      this
    }

    def outZstr(s: XString): XOMFRecord = {
      if (s != null) {
        outJstr(s)
      }
      outb(0)
      this
    }

    def outNameJstr(name: XString): XOMFRecord = {
      val len = name.length
      assert(len <= 255)
      outb(len)
      outJstr(name)
      this
    }

    private def outint(w: Short): XOMFRecord = {
      val b0 = (w & 0xFF.toShort).toShort.toByte
      val b1 = O2JSupport.div(w, 256.toShort).toShort.toByte
      arr += b0
      arr += b1
      this
    }

    // we can process longer names of course (up to 2*MAX_LONGNAME)
    // but just do not want to have deal with such meaningless hair
    def outLongname(name: XString): XOMFRecord = {
      val len = name.length
      assert(len <= 2 * ExportNames.MAX_LONGNAME)
      outint(len.toShort)
      outJstr(name)
      this
    }

    def ensure(len: Int): Unit = {
      if (recRem < len) {
        emitAndReinit()
      }
    }

    private def result(): Array[Byte] = {
      outb(0) // placeholder for crc

      assert(1 + 4 + 1 <= arr.length) // range check: arr contains at least tag + length + crc

      val res = arr.result()

      // put record length into placeholder
      val len = res.length - 1 - REC_SZLEN // len accounts all after length placeholder including crc
      val lenBytes = dw2bytes(len)

      res(REC_SZOFFS + 0) = lenBytes._1
      res(REC_SZOFFS + 1) = lenBytes._2
      res(REC_SZOFFS + 2) = lenBytes._3
      res(REC_SZOFFS + 3) = lenBytes._4

      val crcIndex = res.length - 1
      res(crcIndex) = 0
      res(crcIndex) = (-res.sum).toByte // put crc into placeholder

      res
    }

    def emit(): Unit = {
      if (!isEmpty || emitWhenEmpty) {
        out(result())
      }
    }

    def emitAndReinit(): Unit = {
      emit()
      arr.clear() // reset buffer
      initRecord() // make it ready to be appended
    }
  }

  //-----------  H e a d e r  -------------------------------------
  //-------------
  private def writeEnd(): Unit = {
    new XOMFRecord(XOMF_OBJEND, emitWhenEmpty = true).emit()
  }

  private def writeXOMFHeader(srcname: XString, modname: XString, uid: XString, common: Boolean, dbgcode: Byte): Unit = {
    // XOMF Magic
    write4b(XOMF_SIGNATURE)

    // XOMF Format Version
    write4b(XOMF_VERSION)

    val headerRec = new XOMFRecord(XOMF_HEADER)

    // Common Flag
    headerRec.outb(if (common) 1 else 0)

    // Module Kind
    headerRec.outb(XOMF_MODULE_NORMAL)

    // Source File Name
    headerRec.outZstr(srcname)

    // Module Name
    headerRec.outZstr(modname)

    // UID Name
    headerRec.outZstr(if (globalNamePrefix == null) uid else js.format("%S%S", globalNamePrefix, uid))

    // Version Stamp
    headerRec.outb(xcVersion.MajorJETVersion)
    headerRec.outb(xcVersion.MinorJETVersion)
    headerRec.outb(xcVersion.InternalJETVersion)
    headerRec.outb(JETEditionFlag)

    // Debug Info Format
    headerRec.outb(dbgcode.toInt)

    // Target Architecture
    val arch = targetArch match {
      case AMD64  => XOMF_ARCH_AMD64
      case ARM64  => XOMF_ARCH_ARM64
      case CBC => shouldNotReachHere("no XOMF for CBC arch")
    }
    headerRec.outb(arch)

    headerRec.emit()
  }

  private def jetVersionString(): XString = js.format("Excelsior JET %s%s", xcVersion.JETVersionString, xcVersion.Edition)
  private def debugFormat: Byte = if (O2Env.env.enabled(GenDebug)) XOMF_DEBUG_FORMAT_BY_COMPILER else XOMF_DEBUG_FORMAT_IS_EMPTY
  private def writeHeader(cls: pcO.Class): Unit = {
    writeXOMFHeader(cls.getBCSourceName, cls.name, nms.getClassName(cls, CL_uid), common = false, debugFormat)
    newCommentRecord().outNameJstr(jetVersionString()).outb(0).emit()
  }

  private def writeNames(): Unit = {
    val rec = XOMFRecord(LNAMES)
    for (s <- XOMF.Segment.values if s.printableName) {
      rec.outNameJstr(s.jstr)
    }
    rec.emit()
  }

  private def outXsegdef(name: XString, GrpInx: Int, cls: XOMF.SegmentClass, Size: Int, align: XOMF.SegmentAlign): Unit = {
    new XOMFRecord(XOMF_SEGDEF)
      .outZstr(name)               // seg name (zero-terminated)
      .outIndex(GrpInx)            // group name (index)
      .outb(cls.ordinal)
      .outdw(Size)
      .outb(align.ordinal)
    .emit()
  }

  private def writeSegments(): Unit = {
    for (s <- XOMF.Segment.values if s.shouldBeWritten) {
      outXsegdef(null, s.ordinal, s.segClass, s.size, s.align)
    }
    at.iterateModule(procSegmentGenerator)
  }

  //-------------------- Objects Allocation ----------------------------
  //----------------
  private def hasRealFxups(o: pc.Symbol): Boolean = {
    if (!o.ownsSegment) {
      return false
    }
    val sg = at.getSegment(o)
    sg.length > 0 && (sg.fixups exists cd.isRTFixup)
  }

  private def allocateObjAt(o: pc.Symbol, seg: XOMF.Segment): Unit = {
    if (hasAddress(o)) {
      return
    }
    val bss = seg.segClass == XOMF.SegmentClass.BSS
    if (o.ownsSegment || bss) {
      seg match {
        case XOMF.Segment.CODE =>
          assert(o.ownsSegment)
          setAddress(o, XOMF.Segment.segmCount, 0)
          XOMF.Segment.segmCount += 1
        case _ =>
          val size = def0.objectSize(o, !bss)
          val align = def0.objectAlign(o, 1 << seg.align.ordinal)
          seg.size = NumerateModule.mkAlign(seg.size, align)
          setAddress(o, seg.segNum, seg.size)
          seg.size += size
      }
    }
  }

  // there are 4 special objects to be allocated in their own segments:
  // 1. Type Descriptor (1st layer): positive & negative parts
  // 2. Type Descriptor (2nd layer - Reflection info): positive & negative parts
  // 3. Static Fields Bundle
  // 4. Local String Pool (per-class)
  private def allocateObjs(/*VAR*/ tcf_info: tcfTables.tcf_tables_info): Unit = {
    XOMF.Segment.SFieldSegClass = XOMF.SegmentClass.BSS
    if (at.currClass.hasMetaInformation) {
      // allocate static fields
      val staticBundle = at.findSpecialObject(at.StaticBundle)
      if (staticBundle != null) {
        allocateObjAt(staticBundle, XOMF.Segment.SFIELD)
        if (staticBundle.ownsSegment) {
          XOMF.Segment.SFieldSegClass = XOMF.SegmentClass.DATA
        }
      }

      // allocate type handle
      allocateObjAt(at.currClass.typeHandle, XOMF.Segment.THANDLE)

      if (at.currClass.hasThinTD) {
        // allocate thin type handle
        allocateObjAt(at.currClass.thinTypeInfo.headerTypeHandle, XOMF.Segment.THIN_TDESC)
        allocateObjAt(at.currClass.thinTypeInfo.thinTypeHandle, XOMF.Segment.THIN_TDESC)
      }

      // allocate serial type info
      val serialTypeInfo = at.findSpecialObject(at.SerialTypeInfo)
      if (serialTypeInfo != null) {
        assert(!hasRealFxups(serialTypeInfo))
        allocateObjAt(serialTypeInfo, XOMF.Segment.STI)
      }

      // allocate serial type info
      val vmtEncoding = at.findSpecialObject(at.VMTEncoding)
      if (vmtEncoding != null) {
        assert(!hasRealFxups(vmtEncoding))
        allocateObjAt(vmtEncoding, XOMF.Segment.VMTE)
      }

      // allocate perparation info
      val preparationInfo = at.findSpecialObject(at.PreparationInfo)
      if (preparationInfo != null) {
        assert(!hasRealFxups(preparationInfo))
        allocateObjAt(preparationInfo, XOMF.Segment.TDINIT)
      }

      // allocate reflection
      val reflectionNegative = at.findSpecialObject(at.TDReflectionNegative)
      if (reflectionNegative != null) {
        assert(!hasRealFxups(reflectionNegative))
        allocateObjAt(reflectionNegative, XOMF.Segment.REFL)
        assert((XOMF.Segment.REFL.size & 3) == 0)
      }

      val reflection = at.findSpecialObject(at.TDReflection)
      assert(!hasRealFxups(reflection))
      allocateObjAt(reflection, XOMF.Segment.REFL)
    }

    // allocate bootstrap
    val o = at.findSpecialObject(at.BootstrapRequirements)
    if (o != null) {
      assert(!hasRealFxups(o))
      allocateObjAt(o, XOMF.Segment.BOOTSTRAP)
    }

    // allocate exception tables
    if (tcf_info.aotCPData != null) {
      allocateObjAt(tcf_info.aotCPData, XOMF.Segment.REFL)
    }
    if (tcf_info.fdmap != null) {
      allocateObjAt(tcf_info.fdmap, XOMF.Segment.TDINIT)
    }
    if (tcf_info.frameDescriptors != null) {
      allocateObjAt(tcf_info.frameDescriptors, XOMF.Segment.BSS)
    }
    if (tcf_info.importList != null) {
      allocateObjAt(tcf_info.importList, XOMF.Segment.THANDLE)
    }

    // allocate all other objects
    at.iterateModule(objectAllocator)
  }

  //-------------- Call Context Table Generation -----------------------
  //---------------------------
  /*
  PROCEDURE writeseg(nm-: ARRAY OF CHAR; a: cd.CODE_SEGM);

    PROCEDURE hexdig(b: INT): CHAR;
    BEGIN
      IF b < 10 THEN RETURN CHR(ORD('0')+b); ELSE RETURN CHR(ORD('a')+b-10); END;
    END hexdig;

  VAR i, b: INT;
      digit: ARRAY 3 OF CHAR;
  BEGIN
    digit[2] := 0X;
    env.info.print("\n-- segment %s: codesz=%d {", nm, a.code_len);
    i := 0;
    WHILE i < a.code_len DO
      IF (i MOD 32) = 0 THEN
        env.info.print("\n--    ");
      ELSE
        env.info.print(" ");
      END;
      b := VAL(INT, SYSTEM.VAL(SYSTEM.CARD8, a.bcode[i]));
      digit[0] := hexdig(b DIV 16);
      digit[1] := hexdig(b MOD 16);
      env.info.print(digit);
      INC(i);
    END;
    env.info.print("\n-- } fixupsz=%d\n", a.fxup_len);
    FOR i:= 0 TO a.fxup_len-1 DO
      env.info.print("--    k%d at %d: %d+ %S\n",
        a.fxup[i].kind, a.fxup[i].offs, a.fxup[i].fx_offs, a.fxup[i].obj.GetName());
    END;
    env.info.print("-- } segm. %s\n", nm);
  END writeseg;
  */
  //----------------- Ready Segment Generation -------------------------
  //----------------------
  private def outLedata(SegInx: Int, SegOffs: Int, Data: Array[Byte], DataLen: Int): Unit = {
    new XOMFRecord(LEDATA)
      .outIndex(SegInx)
      .outdw(SegOffs)
      .outByteArr(Data, DataLen)
    .emit()
  }

  private def outFixup(rec: XOMFRecord, fx: fixup): Unit = {
    val FIXUP_MAX: Int = 16 + 3

    rec.ensure(FIXUP_MAX)
    rec.outb(fx.type0)
    rec.outdw(fx.offs)
    rec.outb(fx.tkind)
    rec.outdw(fx.trg)
  }

  private def convertFixup(kind: RelocationKind): Int = {
    // convert fixup kind to XOMF format
    assert(kind supportedOn targetArch)
    (kind: @unchecked) match {
      case ADDR32               => FX_ADDR32
      case ADDR64               => FX_ADDR64
      case OFFS32 | CODE_OFFS32 => FX_OFFS32
      case BYTE_STR_32          => FX_BYTESTR32
      case TD_INDEX_16          => FX_TDINDEX16
      case RVA_32               => FX_RVA32

      case ARM64_B_BL_IMM     => FX_ARM64_B_BL_IMM26
      case ARM64_ADRP_IMM     => FX_ARM64_ADRP_IMM21
      case ARM64_ADD_IMM_LO12 => FX_ARM64_ADD_LO12
    }
  }

  private def qsortFixups(list: ArrayBuffer[cd.Fixup], fxTab: Array[FX_PAIR], l: Int, r: Int): Unit = {
    var i = l
    var j = r
    var ix = O2JSupport.div(l + r, 2)
    while (i <= j) {
      while (list(fxTab(i).srcNo).position < list(fxTab(ix).srcNo).position) {
        i += 1
      }
      while (list(fxTab(ix).srcNo).position < list(fxTab(j).srcNo).position) {
        j -= 1
      }
      if (i <= j) {
        if (i != j) {
          if (ix == i) {
            ix = j
          } else if (ix == j) {
            ix = i
          }
          val tmp = fxTab(i).srcNo
          fxTab(i).srcNo = fxTab(j).srcNo
          fxTab(j).srcNo = tmp
        }
        i += 1
        j -= 1
      }
    }
    if (l < j) {
      qsortFixups(list, fxTab, l, j)
    }
    if (i < r) {
      qsortFixups(list, fxTab, i, r)
    }
  }

  private def translateFixup(Adr: Int, seg: Segment, /*VAR*/ pair: FX_PAIR): Unit = {

    /* --- t r a n s l a t e --- */
    val N = pair.srcNo

    val fixup = seg.fixups(N)
    val target = fixup.getTargetAsOBJECT
    var kind = convertFixup(fixup.kind)
    val position = fixup.position
    val address = getAddress(target)
    val (omfKind, omfTarget, offs) = kind match {
      case FX_BYTESTR32 =>
        assert(address.seg == SegRawData)
        assert(address.extNum != EXTDEF_Invalid)
        (TK_RAWDATA, address.extNum, 0)
      case FX_TDINDEX16 =>
        val num = address.extNum
        assert(num != EXTDEF_Invalid)
        (TK_ID, num, 0)
      case _ =>
        assert(address.seg != SegRawData)
        address.seg match {
          case SegExtern =>
            (TK_ID, address.extNum, 0) // # of EXTDEF
          case _ =>
            (TK_SEG, address.seg, address.offs) // # of seg
        }
    }
    val addend = fixup.addend + offs + (if (kind == FX_OFFS32) 4 else 0)
    kind match {
      case FX_ADDR32 |
           FX_OFFS32 |
           FX_RVA32 =>
        seg.patchW32(position, addend)
      case FX_ADDR64 =>
        seg.patchW64(position, addend.toLong)
      case FX_ARM64_ADD_LO12 =>
        seg.patchW32(position, (addend & 4095) << 10)
      case FX_ARM64_ADRP_IMM21 =>
        if (isNBits(addend, 20)) {
          seg.patchW32(position, (addend & 3) << 29)
          seg.patchW32(position, (addend >>> 2) << 5)
        } else {
          assert(isNBits(addend, 27))
          kind = FX_ARM64_ADRP_IMM27
          // extract destination register from instruction encoding
          // and rewrite instruction format to fit more bits of addend
          val reg = seg.getW32(position) & 0x1F
          seg.setW32(position, (addend << 5) | reg)
        }
      case FX_TDINDEX16 |
           FX_BYTESTR32 |
           FX_ARM64_B_BL_IMM26 =>
        assert(addend == 0)
    }
    pair.setup(kind, Adr + position, omfKind, omfTarget)
  }

  private def mkFxtab(SegInx: Int, Adr: Int, seg: Segment): Array[FX_PAIR] = {
    /* --- m k _ f x t a b --- */
    lastFxNo = -1
    if (seg.fixups.isEmpty) {
      return null
    }
    val cnt = seg.fixups.length
    val fxTab = Array.fill[FX_PAIR](cnt)(new FX_PAIR())
    for (n <- 0 until cnt) {
      val f = seg.fixups(n)
      f.kind match {
        case OFFS32 | CODE_OFFS32 | TD_REL_32_DEL | OFFS32_LOCAL =>
          val address = getAddress(f.getTargetAsOBJECT)
          if (address.seg == SegInx) { /* target object is from the same segment */
            val toffs = ((address.offs + f.addend) - Adr) - f.position
            seg.patchW32(f.position, toffs)
          } else {
            assert(f.kind != TD_REL_32_DEL && f.kind != OFFS32_LOCAL)
            lastFxNo += 1
            fxTab(lastFxNo).srcNo = n
          }
        case TD_REL_32 | TD_REL_16 =>
          shouldNotReachHere(f.kind)
        case _ =>
          lastFxNo += 1
          fxTab(lastFxNo).srcNo = n
      }
    }
    if (lastFxNo > 0) {
      qsortFixups(seg.fixups, fxTab, 0, lastFxNo)
    }
    for (cnt <- 0 to lastFxNo) {
      translateFixup(Adr, seg, fxTab(cnt))
    }
    fxTab
  }

  private def genReadySegm(SegInx: Int, Adr: Int, Seg: Segment): Unit = {
    if (Seg.length == 0) {
      return
    }
    val fxTab = mkFxtab(SegInx, Adr, Seg)
    val (code, len) = Seg.getCode
    outLedata(SegInx, Adr, code, len)
    if (lastFxNo >= 0) {
      val rec = new XOMFRecord(XOMF_FIXUP)
      for (i <- 0 to lastFxNo) {
        outFixup(rec, fxTab(i))
      }
      rec.emit()
    }
  }

  private def outEXTDEF(rec: XOMFRecord, o: pc.Symbol, name: XString, alien: Boolean = true): Unit = {
    extCnt += 1
    val (seg, offs) = if (alien) {
      (SegExtern, 0)
    } else {
      val address = getAddress(o)
      (address.seg, address.offs)
    }
    setAddress(o, seg, offs, extCnt)

    rec.ensure(name.length + 4)
    rec.outLongname(name)
    rec.outIndex(0)
  }

  // write XOMF_IMPDEF record (previous EXTDEF/IMPDEF)
  //   dllName (ASCIIZ)
  //   hash
  //   className (UTF8Z)
  //   entries = 1
  //     index
  //     type
  //     internalName (UTF8Z)
  private def outExtern(rec: XOMFRecord, o: pc.Symbol, alien: Boolean = true): Unit = {
    o match {
      case o: pc.DataSymbol.TypeHandle =>
        o.tpe match {
          case t: pc.SymType.Array =>
            val b = t.arrayBaseType
            val typeHandle = b.typeHandle
            // we need to generate EXTDEF & IMPDEF for array base type handle
            if ((b ne at.currClass) && !hasAddress(typeHandle)) {
              outExtern(rec, typeHandle)
            }
          case _ =>
        }
      case _ =>
    }
    val extname = ExportNames.linkageName(o)
    outEXTDEF(rec, o, extname, alien)
  }

  private def outBytestr(o: at.StringHolder): Unit = {
    new XOMFRecord(XOMF_RAWDATA)
      .outb(DK_BYTESTR)
      .outdw(o.str.length)
      .outJstr(o.str)
    .emit()

    rawDataCnt += 1
    setAddress(o, SegRawData, 0, rawDataCnt)
  }

  private def externObject(o: pc.Symbol): Boolean = o.mno != at.currClass.mno || o.isInstanceOf[pcO.Member] && o.asInstanceOf[pcO.Member].isExternal

  private def outFixupTargets(rec: XOMFRecord, sg: Segment): Unit = {
    for (f <- sg.fixups) {
      val trg = f.getTargetAsOBJECT
      if (trg != null && !hasAddress(trg)) {
        if (f.kind == BYTE_STR_32) {
          rec.emitAndReinit()                           // emit collected extdefs, reinitialize rec to reuse it later
          outBytestr(trg.asInstanceOf[at.StringHolder]) // emit rawdata record with bytestr
        } else if (externObject(trg)) {
          outExtern(rec, trg)
        }
      }
    }
  }

  //-----------------------------------------------------------------------------
  private def writeExternals(): Unit = {
    rawDataCnt = 0
    extCnt = 0

    val rec = new XOMFRecord(EXTDEF)
    if (at.currClass.hasMetaInformation) {
      // EXTDEF #1: TypeHandle object
      if (!at.currClass.hasThinTD || at.currClass.isInfectedAJClass) {
        // If class doesn't have TD in TDesc segment (Managed/Infected/AJArray)
        outExtern(rec, at.currClass.typeHandle, alien = false)
      } else {
        // If class has only ThinTD
        outExtern(rec, at.currClass.thinTypeInfo.thinTypeHandle, alien = false)
      }
    }
    externalsCollector.extdefRecord = rec
    // write EXTDEFs for absent type handles & all extern objects
    at.iterateModule(externalsCollector)
    externalsCollector.extdefRecord.emit()
  }

  private def writeExportedObjects(expobjs: ArrayBuffer[pc.Symbol]): Unit = {
    assert(expobjs.nonEmpty)

    val className = nms.getClassName(at.currClass, CL_uid)

    // write XOMF_EXPDEF record
    val rec = new XOMFRecord(XOMF_EXPDEF)
    rec.outdw(className.getJavaHashCode)        // hash
    rec.outZstr(className)                      // className (UTF8Z)
    rec.outdw(expobjs.length)                   // entries

    for (obj <- expobjs) {
      rec.outZstr(ExportNames.linkageName(obj)) // elems[i] (UTF8Z)
    }

    rec.emit()
  }

  private def outPUBDEF(o: pc.Symbol, name: XString, kind: Int): Unit = {
    val address = getAddress(o)
    val rec = new XOMFRecord(PUBDEF)
      .outIndex(address.seg ensuring (_ > 0))
      .outLongname(name)
      .outdw(address.offs)
      .outb(kind)
    if (kind == XOMF_TYPE_TD) {
      val className = nms.getClassName(o.asInstanceOf[pc.DataSymbol.TypeInfo].tpe.asInstanceOf[pcO.Class], CL_slash)
      rec.outdw(className.hashCode)
    }
    rec.emit()
  }

  private def doExport(o: pc.Symbol, pubdefKind: Int, external: Boolean = false): Unit = {
    if (!external) {
      outPUBDEF(o, ExportNames.linkageName(o), pubdefKind)
    }
  }

  //-----------------------------------------------------------------------------
  private def writeExport(): Unit = {
    val exportWriter: ExportWriter = new ExportWriter()
    exportWriter.reset()
    at.iterateModule(exportWriter)
    val expobjs = exportWriter.getExportedObjects
    if (expobjs.nonEmpty) {
      writeExportedObjects(expobjs)
    }
  }

  /* ----------------------- Package ------------------------ */
  private def writePackage(cls: pcO.Class): Unit = {
    if (!classByO2Object(cls).isJavaReference) {
      return
    }

    val uid = TypeMetaInfoGenerator.Utils.makePackageName(cls, symbolName = false)
    if (uid == null) {
      return
    }

    writeXOMFHeader(js.newJString("PACKAGE DESCRIPTOR"), null, uid, common = true, XOMF_DEBUG_FORMAT_IS_EMPTY)

    // name #1
    XOMFRecord(LNAMES).outNameJstr(XOMF.Segment.THANDLE.jstr).emit()

    // segdef #1
    val sg = TypeMetaInfoGenerator.genPackageDescr(cls)
    outXsegdef(null, 1, XOMF.SegmentClass.DATA, sg.length, XOMF.SegmentAlign._4) /*name#*/
    // segment data
    assert(sg.fixups.isEmpty)
    genReadySegm(1, 0, sg) /*seg#*/

    // PUBDEF
    XOMFRecord(PUBDEF)
      .outIndex(1) // seg #
      .outLongname(TypeMetaInfoGenerator.Utils.makePackageName(cls, symbolName = true))
      .outdw(0) // offset in seg
      .outb(XOMF_TYPE_DATA)
    .emit()

    writeEnd()
  }

  private def mkOutName(): XString = at.currClass.getMangledName
}
