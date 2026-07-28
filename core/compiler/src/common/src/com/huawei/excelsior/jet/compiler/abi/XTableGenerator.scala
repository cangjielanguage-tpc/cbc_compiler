/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.compiler.Env.{targetArch, targetPlatform}
import com.huawei.excelsior.jet.compiler.{Domain, Environment, RTConst, TypeProvider}
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.XTableGenerator.PackedXInfo
import com.huawei.excelsior.jet.compiler.ir.*
import com.huawei.excelsior.jet.compiler.ir.XSite.GCDeltaMap
import com.huawei.excelsior.jet.compiler.options.BoolOption.{GenStackTrace, GenerateMarkedRegions}
import com.huawei.excelsior.jet.compiler.symlevel.{BytecodeMethodReference, Method, MethodReferenceAccessKind as MAK}
import xscala.io.ByteBuffer
import xscala.io.LEB128Encoder.*

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * @author ijorch
  */
class XTableGenerator(val rootMethod: Method, val slotOffset: Slot => Int)(implicit val env: Environment)
  extends InlineListGenerator with GCMapsGenerator {
  implicit def typeProvider: TypeProvider = env.getTypeProvider

  def writeMethods = env.enabled(GenStackTrace) || env.enabled(GenerateMarkedRegions)
  def writeBCPos = env.enabled(GenerateMarkedRegions)
  def writeLines = env.enabled(GenStackTrace)

  val noInlinedMethods = RTConst.InlineList.Head.NO_INLINED_METHODS.intValue
  val noExceptionHandler = RTConst.MethodInfoFrameDescriptor.NO_EXCEPTION_HANDLER_AS_OFFSET.intValue
  val noMarkedRegionID = RTConst.XTable.State.Initial.MARKED_REGION_ID.intValue
  val noSoftExceptionID = RTConst.XTable.State.Initial.SOFT_EXCEPTION_ID.intValue

  final class XSiteInfo(val xSite: XSite) extends XRegion {

    def xRegionStart = xSite.siteOffset

    /** Offset to exception handler for this xSite (it may be two special values). */
    def handlerOffset = xSite.handlerOffset

    /** Inline list of this `xSite`, with markers, from deepest to root. */
    val inlineList = new InlineList(xSite.inlineContext, xSite.kind)

    /** Offset from start of inline buffer to current inline sequence. */
    var inlineListHead = noInlinedMethods

    /** Length of GCMap sequence that's needed to be interpreted to construct valid GCMap for current xSite. */
    var gcMapLength = InitState.gcMapLength

    var deltaMap = xSite.gcDeltaMap

    var markedRegionID = noMarkedRegionID

    // TODO: investigate missing MethodReferences in o2-comp and remove check of calledMethodRef
    val xRegionKind = if (xSite.kind.isCall && xSite.calledMethodRef != null) {
      xSite.calledMethodRef match {
        case ref: BytecodeMethodReference if ref.isMemberNameInvoke =>
          MNCall
        case ref => ref.accessKind match {
          case MAK.VIRTUAL => VCall
          case MAK.INTERFACE => ICall
          case MAK.STATIC | MAK.SPECIAL | MAK.MUT => DCall
          case MAK.STATIC_VIRTUAL => VSCall
        }
      }
    } else InitState.xRegionKind

    val refClassIndex =
      if (xRegionKind == ICall) {
        val refClass = xSite.calledMethodRef.refClass
        if (rootMethod.getDeclaringClass.isCangjieType && refClass.isDeferred) {
          // We don't add deferred interfaces to import tables of cangjie methods
          // to avoid versioning of CbcClasses in runtime.
          InitState.refClassIndex
        } else {
          // TODO: ensure that refClass is never array for interface call and replace with assert
          val adjustedRefClass = if (refClass.isJavaArray) env.getTypeProvider.getObjectType else refClass
          env.getImportedClassIdx(adjustedRefClass, rootMethod)
        }
      } else InitState.refClassIndex

    val vnum = if (xRegionKind == VCall || xRegionKind == ICall) {
      val ref = xSite.calledMethodRef
      if (ref.hasVirtualMethodSlot) {
        ref.virtualMethodSlot
      } else InitState.vnum
    } else InitState.vnum

    val receiverIndex = if (xRegionKind == VCall || xRegionKind == ICall) {
      xSite.calledMethodRef.methodType.getReceiverArgIdx
    } else InitState.receiverIndex

    def bytecodePos = if (writeBCPos) xSite.bytecodePos else BytecodeOffset.INVALID

    def lineNumber = if (writeLines) xSite.lineNumber else LineNumber.UNKNOWN

    def softExceptionID = xSite.softExceptionID

    def domain = xSite.domain

    override def toString =
      s""" siteOffset: ${xSite.siteOffset};
         | calledMethodRef: ${xSite.calledMethodRef}
         | kind: ${xSite.kind}
         | handlerOffset: $handlerOffset;
         | inlineList: $inlineList;
         | deltaMap: $deltaMap
         | lineNumber: $lineNumber
       """.stripMargin
  }

  /** Generates exception table and inline list for method with given `xinfo`. */
  def packXInfo(xinfo: XInfo, markedRegions: collection.Seq[MarkedRegion]): PackedXInfo = {

    // 1. Make xSiteInfo by xSite and filter such, that are equal to previous one.
    val xSiteInfos = ArrayBuffer.empty[XSiteInfo]

    def skip(next: XSiteInfo): Boolean = {
      if (xSiteInfos.isEmpty) return false

      val last = xSiteInfos.last
      assert(last.xRegionStart <= next.xRegionStart, "Unsorted xSites!")

      !next.xSite.kind.needSeparateRegion && // separate trap-checks into different XRegions to distinguish them from unexpected hardware exceptions
        last.handlerOffset == next.handlerOffset &&
        last.xRegionKind == next.xRegionKind &&
        last.vnum == next.vnum &&
        last.receiverIndex == next.receiverIndex &&
        last.refClassIndex == next.refClassIndex &&
        last.bytecodePos == next.bytecodePos &&
        last.lineNumber == next.lineNumber &&
        last.softExceptionID == next.softExceptionID &&
        last.domain == next.domain &&
        last.inlineList == next.inlineList && (
          if (last.deltaMap == null) {
            last.deltaMap = next.deltaMap
            next.deltaMap = GCDeltaMap.emptyDelta
            true
          } else {
            next.deltaMap == null || next.deltaMap.isEmpty
          }
        )
    }

    for (xsite <- xinfo.getCollectedXSites) {
      val info = new XSiteInfo(xsite)
      if (!skip(info)) xSiteInfos += info
    }

    checkXSiteSequence(xSiteInfos, "xSites weren't merged but have the same offset") { (f, s) => f.xSite.siteOffset < s.xSite.siteOffset }
    checkXSiteSequence(xSiteInfos, "PreCall xSite is followed by a non-call xSite") { (f, s) => !f.xSite.kind.isPreCall || s.xSite.kind.isCall }
    assert(xSiteInfos.isEmpty || !xSiteInfos.last.xSite.kind.isPreCall)

    // 2. Write exception sites and inline sequence to `xTable`.
    val xTable = genXTable(xSiteInfos, markedRegions)

    // 3. Every compiler needs to know whether there are any exception handlers, so calc it here.
    val trivialXHandler = xSiteInfos forall (_.handlerOffset == XSite.NO_EXCEPTION_HANDLER)

    new PackedXInfo(xTable, trivialXHandler)
  }

  private def checkXSiteSequence(xSiteInfos: ArrayBuffer[XSiteInfo], assertMessage: String)
                                (check: (XSiteInfo, XSiteInfo) => Boolean) : Unit = {
    if (xSiteInfos.length < 2) {
      return
    }

    val found = xSiteInfos.sliding(2).find { case ArrayBuffer(f, s) => !check(f, s) }
    assert(found.isEmpty, {
      val Some(ArrayBuffer(first, second)) = found
      s"""$assertMessage in $rootMethod:
         |First:
         |$first
         |Second:
         |$second
      """.stripMargin
    })
  }

  /** @return buffer with generated exception table of new format. */
  private def genXTable(xSitesInfo: ArrayBuffer[XSiteInfo], markedRegions: collection.Seq[MarkedRegion]): ByteBuffer = {
    def xTableAlignment = RTConst.XTable.ALIGNMENT.intValue
    def blockEnd = RTConst.XTable.Command.BLOCK_END.intValue

    def align(buf: ByteBuffer, align: Int): Unit = buf.align(align, blockEnd)

    def genBlocks(xRegions: List[XRegion]) = {
      val blockSize = RTConst.XTable.BLOCK_SIZE.intValue
      val buf = new ByteBuffer()

      @tailrec
      def genBlocksRec(prevState: State, xRegions: List[XRegion], nBlocks: Int, blockStart: Int): Int = {
        if (xRegions.nonEmpty) {
          val newState = xRegions.head
          val chunkStart = buf.length
          val curState = newState.encodeDiff(prevState, buf)
          val chunkEnd = buf.length

          if (chunkEnd - blockStart > blockSize - 1) {
            // New chunk doesn't fit into current block.
            buf.cropAt(chunkStart)
            buf.putByte(blockEnd)
            align(buf, blockSize)
            genBlocksRec(InitState, xRegions, nBlocks + 1, buf.length)
          } else {
            genBlocksRec(curState, xRegions.tail, nBlocks, blockStart)
          }
        } else {
          buf.putByte(blockEnd)
          nBlocks
        }
      }

      (buf, genBlocksRec(InitState, xRegions, nBlocks = 1, blockStart = 0))
    }

    if (xSitesInfo.isEmpty) null else {
      // 1. Write inline sequences to `inlineList`.
      // NB: It calculates all `xSiteInfo.inlineStart`, so must be executed before writing xsites.
      val inlineList = genInlineList(xSitesInfo)

      // 2. Write GCMaps.
      // NB: It calculates all `xSiteInfo.gcMapLength`, so must be executed before writing xsites.
      val gcmap = genGCMap(xSitesInfo)
      GCMapStatisticCollector.collect(gcmap)

      // 3. Split xSitesInfo at marked regions boundaries and assign proper `xSiteInfo.markedRegionID`.
      val xRegions = injectMarkedRegions(xSitesInfo, markedRegions)

      // 4. Generate blocks of XTable
      val (blocks, nBlocks) = genBlocks(xRegions)

      // 5. Generate init & global commands
      val xtable = new ByteBuffer()

      if (gcmap != null) Encode(xtable).Command.gcmap(gcmap)
      if (inlineList != null) Encode(xtable).Command.inlineList(inlineList)
      if (nBlocks == 1) xtable.append(blocks) else Encode(xtable).Command.findBlock(blocks, nBlocks)

      align(xtable, xTableAlignment)
      xtable
    }
  }

  private def writeStackMap(buf: ByteBuffer, bitset: collection.BitSet): Unit = {
    val words = bitset.toBitMask
    val nwords = words.length
    var len = 8 * (nwords - 1)
    var x = words.last
    while (x != 0) {
      len += 1
      x >>>= 8
    }
    assert(len > 0 && len <= Byte.MaxValue)

    buf.putW8(len)

    var rem = 0L
    for (i <- 0 until len) {
      if (i % 8 == 0) {
        rem = words(i / 8)
      }
      buf.putW8((rem & 0xFF).toInt)
      rem >>>= 8
    }
  }

  private case class Boundary(offset: Int, regionID: Int)

  private def injectMarkedRegions(xSitesInfo: ArrayBuffer[XSiteInfo], markedRegions: collection.Seq[MarkedRegion]): List[XRegion] = {
    if (markedRegions == null || markedRegions.isEmpty) return xSitesInfo.toList

    val existingBoundaries = xSitesInfo.iterator.map(_.xRegionStart).toSet
    assert(existingBoundaries.size == xSitesInfo.size, "duplicate xRegionStarts are not allowed")

    val starts = markedRegions.iterator.map(m => (m.startOffset, m.id)).toMap
    val hangingEnds = markedRegions.iterator.map(_.endOffset).filterNot(starts.keySet).toSet

    val allBoundaries = ArrayBuffer.empty[Either[Boundary, XSiteInfo]]
    allBoundaries ++= xSitesInfo.iterator map (Right(_))
    allBoundaries ++= markedRegions.iterator collect { case m if !existingBoundaries(m.startOffset) => Left(Boundary(m.startOffset, m.id)) }
    allBoundaries ++= hangingEnds.iterator collect { case e if !existingBoundaries(e) => Left(Boundary(e, noMarkedRegionID)) }
    allBoundaries.sortInPlaceBy {
      case Right(xsi) => xsi.xRegionStart
      case Left(Boundary(offset, _)) => offset
    }

    val xRegions = ListBuffer.empty[XRegion]
    var lastFullState: State = InitState
    var lastRegionID = noMarkedRegionID
    for (e <- allBoundaries) e match {
      case Right(xsi) =>
        if (hangingEnds(xsi.xRegionStart)) {
          lastRegionID = noMarkedRegionID
        } else {
          lastRegionID = starts.getOrElse(xsi.xRegionStart, lastRegionID)
        }
        xsi.markedRegionID = lastRegionID
        lastFullState = xsi
        xRegions += xsi

      case Left(Boundary(offset, regionID)) =>
        lastRegionID = regionID
        xRegions += new XSiteSplit(offset, regionID, lastFullState)
    }

    xRegions.toList
  }

  // splitting XRegion because of change of markedRegionID (and only because of it)
  final class XSiteSplit(val xRegionStart: Int, val markedRegionID: Int, lastFullState: State) extends XRegion {
    require(xRegionStart > lastFullState.xRegionStart)

    def xRegionKind               = InitState.xRegionKind
    def bytecodePos               = InitState.bytecodePos

    // TODO: consider making transient to avoid propagating across block boundaries
    def refClassIndex             = lastFullState.refClassIndex
    def vnum                      = lastFullState.vnum
    def receiverIndex             = lastFullState.receiverIndex

    def handlerOffset             = lastFullState.handlerOffset
    def inlineListHead            = lastFullState.inlineListHead
    def gcMapLength               = lastFullState.gcMapLength
    def lineNumber                = lastFullState.lineNumber

    def softExceptionID           = lastFullState.softExceptionID
    def domain                    = lastFullState.domain
  }

  sealed trait XRegionKind
  object VCall extends XRegionKind
  object ICall extends XRegionKind
  object DCall extends XRegionKind
  object MNCall extends XRegionKind
  object VSCall extends XRegionKind
  object Other extends XRegionKind

  sealed trait State {
    def xRegionKind: XRegionKind
    def xRegionStart: Int
    def handlerOffset: Int
    def inlineListHead: Int
    def gcMapLength: Int
    def refClassIndex: Int
    def vnum: Int
    def receiverIndex: Int
    def bytecodePos: Int
    def lineNumber: Int
    def markedRegionID: Int
    def softExceptionID: Int
    def domain: Domain
  }

  /** Represents initial state of each block in new xTable. */
  object InitState extends State {
    import RTConst.XTable.State.Initial.*
    def xRegionKind               = Other
    def xRegionStart              = XREGION_START.intValue
    def handlerOffset             = HANDLER_OFFSET.intValue ensuring (_ == noExceptionHandler)
    def inlineListHead            = INLINE_LIST_HEAD.intValue ensuring (_ == noInlinedMethods)
    def gcMapLength               = GCMAP_LENGTH.intValue
    def refClassIndex             = REF_CLASS_INDEX.intValue
    def vnum                      = VNUM.intValue
    def receiverIndex             = RECEIVER_INDEX.intValue
    def bytecodePos               = BYTECODE_POS.intValue ensuring (_ == BytecodeOffset.INVALID)
    def lineNumber                = LINE_NUMBER.intValue ensuring (_ == LineNumber.UNKNOWN)
    def markedRegionID            = MARKED_REGION_ID.intValue ensuring (_ == noMarkedRegionID)
    def softExceptionID           = SOFT_EXCEPTION_ID.intValue ensuring (_ == noSoftExceptionID)
    def domain                    = Domain.AJ
  }

  /** Represents state, which will be obtained after decoding new chunk of instructions. */
  class CurrentState(state: State) extends State {
    var xRegionKind               = state.xRegionKind
    var xRegionStart              = state.xRegionStart
    var handlerOffset             = state.handlerOffset
    var inlineListHead            = state.inlineListHead
    var gcMapLength               = state.gcMapLength
    var refClassIndex             = state.refClassIndex
    var vnum                      = state.vnum
    var receiverIndex             = state.receiverIndex
    var bytecodePos               = state.bytecodePos
    var lineNumber                = state.lineNumber
    var markedRegionID            = state.markedRegionID
    var softExceptionID           = state.softExceptionID
    var domain                    = state.domain
  }


  abstract class XRegion extends State {

    /** Encode difference between `this` state describing real xRegion and `that` state currently accumulated in XTable.
      * @return updated state. */
    def encodeDiff(that: State, buf: ByteBuffer) = {
      val updatedState = new CurrentState(that)
      val enc = Encode(buf)
      import enc.Command

      updatedState.xRegionStart = this.xRegionStart
      Command.xRegionStartDiff(this.xRegionStart - that.xRegionStart)

      if (this.handlerOffset == noExceptionHandler && that.handlerOffset != noExceptionHandler) {
        updatedState.handlerOffset = noExceptionHandler
        Command.noHandler()
      } else {
        updatedState.handlerOffset = this.handlerOffset
        val diff = this.handlerOffset - that.handlerOffset
        if (diff != 0) Command.handlerOffsetDiff(diff)
      }

      if (this.inlineListHead != that.inlineListHead) {
        updatedState.inlineListHead = this.inlineListHead
        if (inlineListHead == noInlinedMethods) Command.noInline()
        else Command.inlineListHead(inlineListHead)
      }

      if (this.markedRegionID != that.markedRegionID) {
        updatedState.markedRegionID = this.markedRegionID
        if (markedRegionID == noMarkedRegionID) Command.noMarkedRegionID()
        else Command.markedRegionID(markedRegionID)
      }

      if (this.gcMapLength != 0) {
        updatedState.gcMapLength = this.gcMapLength
        val diff = this.gcMapLength - that.gcMapLength
        val smallDiffMax = RTConst.XTable.Command.GCMAP_LENGTH_SMALL_DIFF_MAX.intValue

        if (diff != 0) {
          if (diff <= smallDiffMax) Command.gcMapLengthSmallDiff(diff)
          else Command.gcMapLengthDiff(diff - smallDiffMax)
        }
      }

      if (xRegionKind == VCall || xRegionKind == ICall || xRegionKind == DCall || xRegionKind == VSCall) {
        if (xRegionKind == VCall || xRegionKind == ICall) {
          updatedState.receiverIndex = this.receiverIndex
          if (this.receiverIndex != that.receiverIndex) {
            Command.receiverIndex(receiverIndex)
          }
          
          updatedState.vnum = this.vnum
          if (this.vnum == that.vnum) Command.VCall()
          else if (vnum == InitState.vnum) Command.unknownVNum()
          else Command.vnum(vnum)

          if (xRegionKind == ICall) {
            updatedState.refClassIndex = this.refClassIndex
            if (this.refClassIndex == that.refClassIndex) Command.ICall()
            else Command.refClassIndex(refClassIndex)
          }
        }

        if (xRegionKind == VSCall) {
          Command.VCall()
        }

        if (this.bytecodePos != that.bytecodePos) {
          Command.bytecodePos(this.bytecodePos)
        }
      }
      // drop bcPos in each state to avoid its erroneous propagation onto next states
      updatedState.bytecodePos = InitState.bytecodePos // TODO: refactor everything to get rid of packXInfo::skip

      if (this.softExceptionID != that.softExceptionID) {
        updatedState.softExceptionID = this.softExceptionID
        if (softExceptionID == noSoftExceptionID) Command.noSoftExceptionID()
        else Command.softExceptionID(softExceptionID)
      }

      if (this.domain != that.domain) {
        updatedState.domain = this.domain
        Command.domain(domain.ordinal)
      }

      // TODO: consider writing line numbers only for calls & null-checks
      updatedState.lineNumber = this.lineNumber
      if (this.lineNumber != that.lineNumber) {
        Command.lineNumber(this.lineNumber)
      }

      if (xRegionKind == MNCall) {
        Command.MNCall()
      }

      updatedState
    }
  }

  /** Low-level encoder. */
  private case class Encode(buffer: ByteBuffer) {
    import RTConst.XTable.Command.*
    object Command {
      def xRegionStartDiff(xrs: Int): Unit = {
        assert(xrs > 0)
        buffer.putULEB(xrs + XREGION_START_DIFF_BASE.intValue)
      }

      def handlerOffsetDiff(ho: Int): Unit = {
        code(HANDLER_OFFSET_DIFF)
        buffer.putSLEB(ho)
      }
      def noHandler(): Unit = code(NO_HANDLER)

      def inlineListHead(ilh: Int): Unit = {
        code(INLINE_LIST_HEAD)
        buffer.putULEB(ilh)
      }
      def noInline(): Unit = code(NO_INLINE)

      def markedRegionID(idx: Int): Unit = {
        code(MARKED_REGION_ID)
        buffer.putULEB(idx)
      }
      def noMarkedRegionID(): Unit = code(NO_MARKED_REGION_ID)

      def softExceptionID(idx: Int): Unit = {
        code(SOFT_EXCEPTION_ID)
        buffer.putULEB(idx)
      }
      def noSoftExceptionID(): Unit = code(NO_SOFT_EXCEPTION_ID)

      def domain(idx: Int): Unit = {
        code(DOMAIN)
        buffer.putULEB(idx)
      }

      def receiverIndex(receiverIndex: Int): Unit = {
        code(RECEIVER_INDEX)
        buffer.putULEB(receiverIndex)
      }

      def vnum(vnum: Int): Unit = {
        code(VNUM)
        buffer.putULEB(vnum)
      }
      def unknownVNum(): Unit = code(UNKNOWN_VNUM)
      def VCall(): Unit = code(VCALL)

      def refClassIndex(rci: Int): Unit = {
        code(REF_CLASS_INDEX)
        buffer.putULEB(rci)
      }
      def ICall(): Unit = code(ICALL)

      def MNCall(): Unit = code(MNCALL)

      def findBlock(b: ByteBuffer, nb: Int): Unit = {
        code(FIND_BLOCK)
        buffer.putULEB(nb)
        buffer.align(RTConst.XTable.BLOCK_ALIGNMENT.intValue)
        buffer.append(b)
      }

      def inlineList(inl: ByteBuffer): Unit = {
        globalCommandWithData(inl, INLINE_LIST)
      }

      def gcmap(gcmap: ByteBuffer): Unit = {
        globalCommandWithData(gcmap, GCMAP)
      }

      private def globalCommandWithData(data: ByteBuffer, c: RTConst): Unit = {
        code(c)
        buffer.putULEB(data.length)
        buffer.append(data)
      }

      def gcMapLengthDiff(d: Int): Unit = {
        assert(d > 0)
        code(GCMAP_LENGTH_DIFF)
        buffer.putULEB(d)
      }

      def gcMapLengthSmallDiff(d: Int): Unit = {
        assert(0 < d && d <= GCMAP_LENGTH_SMALL_DIFF_MAX.intValue)
        buffer.putByte(d + GCMAP_LENGTH_SMALL_DIFF_BASE.intValue)
      }

      def bytecodePos(p: Int): Unit = {
        code(BYTECODE_POS)
        buffer.putSLEB(p)
      }

      def lineNumber(p: Int): Unit = {
        code(LINE_NUMBER)
        buffer.putSLEB(p)
      }

      private def code(codeConst: RTConst): Unit = {
        val code = codeConst.intValue
        assert(0 < code && code <= MAX_CODE.intValue)
        buffer.putByte(code)
      }
    }
  }
}

object XTableGenerator {
  class PackedXInfo(val xTable: ByteBuffer, val trivialXHandler: Boolean)
}
