/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc.codegen

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.isStandalone
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.abi.cbc.FrameCBC
import com.huawei.excelsior.jet.compiler.ir.{XInfo, XSiteKind}
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.{BackEndCBC, FrameComponentCBC}
import com.huawei.excelsior.jet.compiler.opt.backend.codegen.{CodeGenerator, XSitesToolbox}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.{Resources, Universe}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{DebugHintsGeneration, LivenessHintsAtBlockStart, LivenessHintsGeneration}
import com.huawei.excelsior.jet.compiler.symlevel.MethodReference
import xscala.util.MathUtils.{isBitSubset, isNBits}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait XSitesToolboxCBC extends XSitesToolbox with LocalLivenessAnalyzerCBC { self: Universe with BackEndCBC with CodeGeneratorCBC =>

  final def needXSiteImpl(node: Node): Boolean = node match {
    case _: New | _: NewArray | _: NewArrayFill | _: BitcodeDeferred.New | _: BitcodeDeferred.NewArray | _: Clinit => true // can throw and not lowered to calls
    case _: InterfaceCastCBC => true
    case _: Box | _: SpawnFuture | _: SpawnClosure | _: LoadFieldSeqGeneric |
         _: OptionPayloadGeneric | _: NewNoneOptionGeneric | _: NewSomeOptionGeneric |
         _: AssignGeneric => true
    case _ => super.needXSite(node)
  }

  /** Returns true iff `node` should be generated with XSite. */
  override def needXSite(node: Node): Boolean = if !isStandalone then needXSiteImpl(node) else false

  override def xSiteKind(node: Node): XSiteKind = node match {
    case _: New | _: NewArray | _: NewArrayFill | _: BitcodeDeferred.New | _: BitcodeDeferred.NewArray | _: Clinit => XSiteKind.CALL
    case _: InterfaceCastCBC => XSiteKind.CALL
    case _: Box | _: SpawnFuture | _: SpawnClosure | _: LoadFieldSeqGeneric |
         _: OptionPayloadGeneric | _: NewNoneOptionGeneric | _: NewSomeOptionGeneric |
         _: AssignGeneric => XSiteKind.CALL
    case WithImplicitCheck(_: DivisorCheck) => XSiteKind.DIV_WITH_CHECK
    case _ => super.xSiteKind(node)
  }

  override def xSiteTargetRef(node: Node): MethodReference = node match {
    case _: New | _: NewArray | _: NewArrayFill | _: BitcodeDeferred.New | _: BitcodeDeferred.NewArray | _: Clinit => null // Note that direct non-deferred calls don't use `methodRef` from `xSite`
    case _: InterfaceCastCBC => null
    case WithImplicitCheck(_: DivisorCheck) => null
    case _ => super.xSiteTargetRef(node)
  }

  override def needGCMap(node: Node): Boolean = node match {
    case block: Block => block.reachable // Blocks are required to generate liveness analyzer hints for CBC
    case _: Evacuate | _: CatchCBC => true
    case _ => needXSiteImpl(node) && xSiteKind(node).needGCMap
  }

  override def couldGatherLocalUnmovableAt(node: ControlNode): Boolean =
    node.isInstanceOf[Block] || super.needGCMap(node) // Blocks are required to generate liveness analyzer hints for CBC

  /** Returns true iff `node` resource will be collected in GC map at XSites which it live through. */
  override def willBeCollectedInGCMap(n: Node): Boolean = valueOf(n).producer match {
    // Local liveness analysis of CodeGeneratorCBC requires reference constants on registers/locals
    // to be in precise GC maps along with other references
    case _: AJString | _: ConstString | _: AnyNull if n.resource != Resources.Immediate && env.enabled(LivenessHintsGeneration) => true
    case StackAlloc.Local(t) => t.isTraceableReference
    case st: StackAlloc if st.tpe.isTraceableRefType && n.resource != Resources.Immediate => shouldNotReachHere("StackAlloc with non-immediate resource is unsupported")

    case _ => super.willBeCollectedInGCMap(n)
  }

  trait XSitesGeneratorCBC extends XSitesGenerator { self: CodeGeneratorImplCBC =>

    /** Flush collected state from code generator to XInfo */
    def fillWithComputedState(): Unit = {
      xInfo.startGCMap()
      xInfo.setRegistersMask(currentAliveRegsMask)
      xInfo.setUnmovableRegisters(currentUnmovableRegsMask)
      currentAliveSlots foreach xInfo.addTracedSlot
    }

    override protected def afterGatherGCMap(node: Node, slots: Iterable[FrameSlot], mask: Int, unmovableMask: Int): Unit = {

      def transformSlot(fs: FrameSlot): XInfo.Slot = fs match {
        case slot: FrameComponentCBC.FrameSlotCBC => frame.newSlot(slot.local)
        case _ => shouldNotReachHere("Unexpected CBC slot")
      }

      val aliveSlots = slots.map(transformSlot)
      if (shouldAddLivenessHints(node) || node == entryBlock) {
        resetLiveness(mask, unmovableMask, aliveSlots) // setup codegenerator`s state
      }

      if (node.isInstanceOf[Block]) {
        if (shouldAddLivenessHints(node)) {
          genLivenessHintsAtBlockStart(mask, unmovableMask, aliveSlots)
        }
        xInfo.getDeltaMap // reset gathered info
      }
    }

    protected def gatherGCMapCBC(node: Node): Unit = {
      if (!env.enabled(LivenessHintsGeneration)) {
        return
      }

      def genLivenessHintCheck(): Unit = {
        if (env.enabled(DebugHintsGeneration)) {
          val numeratedSlots = numerateSlots(currentAliveRegsMask, currentAliveSlots)
          val bytes = encodeIndices(numeratedSlots)
          cbc.aliveRefCheck(bytes)
        }
      }

      if (node.isInstanceOf[Block]) {
        gatherGCMap(node)
        if (node == entryBlock) {
          markParams(rootMethodParams, rootABI.paramLocations)
        }
        if (node != entryBlock || !genDebug) { // JIT does not know about DebugVar at entry, can`t check
          genLivenessHintCheck()
        }
        return
      }

      fillWithComputedState() // fill calculated state

      genLivenessHintCheck()
      val prev = xInfo.getDeltaMap // discard calculated state, JIT already knows it

      val maskFromLocalAnalysis = currentAliveRegsMask
      val unmovableMaskFromLocalAnalysis = currentUnmovableRegsMask
      val aliveSlotsFromLocalAnalysis = currentAliveSlots

      gatherGCMap(node) // local analyzer reset with precise liveness
      val diff = xInfo.getDeltaMap // usefull map for JIT
      assert(diff != null, s"Diff is null: node: $node, method: ${rootMethod.getFullName}")

      if (env.enabled(LivenessHintsGeneration) && env.enabled(LivenessHintsAtBlockStart)) {
        // check that precise liveness is subset of local analysis liveness
        // that means liveness diff can only kill locals
        assert(isBitSubset(maskFromLocalAnalysis, currentAliveRegsMask))
        assert(isBitSubset(unmovableMaskFromLocalAnalysis, currentUnmovableRegsMask))
        // but debug vars always alive in precise analysis
        assert(currentAliveSlots.subsetOf(aliveSlotsFromLocalAnalysis) || genDebug)
      }
      val prevUnmovableRegisters = diff.unmovableRegistersMask

      if (!diff.isEmpty) {
        diff.prepareUnmovable(null, prevUnmovableRegisters)
        val numeratedSlots = numerateSlots(diff.registersMask, diff.deltaSlots.filter(_.isInstanceOf[FrameCBC.Slot]))
        val numeratedUnmovableSlots = numerateSlots(diff.unmovableRegistersMask, Iterable.empty)
        // alive.ref.diff and alive.unmovable.diff have no side effects other then marking provided locals, no need to generate empty
        if (numeratedSlots.nonEmpty) {
          cbc.aliveRefDifference(encodeIndices(numeratedSlots))
        }
        if (numeratedUnmovableSlots.nonEmpty) {
          cbc.aliveUnmovableDifference(encodeIndices(numeratedUnmovableSlots))
        }
      }
    }

    def numerateSlots(mask: Int, slots: Iterable[XInfo.Slot]): Iterable[Int] = {
      val iRegNum = FrameComponentCBC.MAX_CBC_IREG_COUNT

      def iRegMask(i: Int) = (mask & (1 << i)) != 0

      def slotIdx(slot: XInfo.Slot): Int = slot match {
        case slot: FrameCBC.Slot => slot.local.encoding
        case s => shouldNotReachHere(s"unsupported cbc slot $s")
      }

      val numeratedSlots = ArrayBuffer.empty[Int]
      numeratedSlots ++= (0 until iRegNum) filter iRegMask
      numeratedSlots ++= (slots map slotIdx)
      numeratedSlots.sorted // TODO improve stability and remove sorting: JET-17378
    }

    def encodeIndices(indices: Iterable[Int]): Array[Byte] = {
      val byteBuf = ArrayBuffer.empty[Byte]
      for (idx <- indices) {
        assert(isNBits(idx, 16))
        byteBuf.addOne(idx.toByte)
        byteBuf.addOne((idx >> 8).toByte)
      }
      byteBuf.toArray
    }

    protected def shouldAddLivenessHints(node: Node): Boolean = env.enabled(LivenessHintsGeneration) && (node match {
      case block: Block => ((block != entryBlock) || genDebug) && env.enabled(LivenessHintsAtBlockStart)
      case n => needGCMap(n)
    })

    protected def genLivenessHintsAtBlockStart(referenceMask: Int, unmovableReferenceMask: Int, referenceSlots: Iterable[XInfo.Slot]): Unit = {
      val referenceNumeratedSlots = numerateSlots(referenceMask, referenceSlots)
      val referenceEncoded = encodeIndices(referenceNumeratedSlots)
      val unmovableReferenceNumeratedSlots = numerateSlots(unmovableReferenceMask, Iterable.empty)
      val unmovableReferenceEncoded = encodeIndices(unmovableReferenceNumeratedSlots)
      cbc.aliveReference(referenceEncoded) // alive.ref also clears the liveness state, so we generate it even if it is empty
      if (unmovableReferenceEncoded.nonEmpty) {
        cbc.unmovableReference(unmovableReferenceEncoded) // unmovable.ref have no side effects other then marking provided locals, no need to generate empty
      }
    }
  }

}
