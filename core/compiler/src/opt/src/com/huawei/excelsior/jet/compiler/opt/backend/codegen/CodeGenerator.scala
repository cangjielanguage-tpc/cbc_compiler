/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler
import com.huawei.excelsior.jet.assembler.*
import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.assembler.Width.WPTR
import com.huawei.excelsior.jet.codeemitter.{BranchOp, CodeEmitter}
import com.huawei.excelsior.jet.compiler.Env.stackPointer
import com.huawei.excelsior.jet.compiler.RTSProc.ExceptionHandling_trivialHandler
import com.huawei.excelsior.jet.compiler.abi.ABI.AltLocation
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels.SyntheticCodeLabel
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.opt.backend.{BackEnd, MachineDescription}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Universe}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.IdescHigh16BitsCleaning
import com.huawei.excelsior.jet.util.ScalaCollections.uniqueValue
import com.huawei.excelsior.jet.compiler.{RTConst, RTSProc, StatsKind}
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.Loop

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.PartialFunction.condOpt
import scala.annotation.nowarn

/** Assembler code generation - last phase of backend.
  *
  * @author conwor
  */
@nowarn("msg=match may not be exhaustive")
trait CodeGenerator extends DataGenerator
  with LayoutComponent
  with GCMapsToolbox
  with MutPairsDataGenerator
  with StackPtrsDataGenerator
  with XSitesToolbox
  with ProfilingRegions
  with DebugGenerator
  with CallGenerator { self: Universe with BackEnd with MachineDescription =>

  protected def asmTypeForReadWrite(opType: AsmType, forceZX: Boolean = false): AsmType = {
    if (!forceZX) {
      opType
    } else {
      require(1 <= opType.sizeInBytes && opType.sizeInBytes <= 4)
      AsmType.integral(opType.width, signed = false)
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Code definition & generation tools.

  protected def asm: AsmEmitter
  protected def emit: CodeEmitter

  trait CodeGeneratorImpl extends XSitesGenerator with ProfilingRegionsGenerator with DebugGeneratorImpl with CallGeneratorImpl {

    ///////////////////////////////////////////////////////////////////////////
    // Code segments and labels

    protected val segment = new Segment(codeUnit.getSymbol)
    private val xInfo: XInfo = new XInfo
    private val methodStart = segment.newLabel


    /** Map from every point in IR which [[needGCMap]] to set of nodes which [[willBeCollectedInGCMap]] and live at this point. */
    lazy val gcMaps: collection.Map[Node, Set[Node]] = calcGCMaps()

    /** Map from every point in IR which [[needGCMap]] to set of nodes which [[willBeCollectedInGCMap]] and live at this point. */
    lazy val stackPtrsData: collection.Map[Node, Set[Node]] = stackPointers()

    /** Map from every point in IR which [[needMutInfo]] to set of mut pairs at this point. */
    lazy val mutPairsData: collection.Map[Node, Set[(Node, Node)]] = {
      val data = calcMutPairs()
      // Every base pointer resource must also be present in GC maps since it's a traceable reference
      val missing = data flatMap { (node, pairs) =>
        val maps = gcMaps(node)
        pairs flatMap { (base, ptr) =>
          Option.unless(maps.exists(_.resource == base.resource))((base, ptr, node, maps))
        }
      }
      assert(missing.isEmpty, missing)
      data
    }

    private val startLabels = all[Block].map(b => (b, segment.newLabel)).toMap
    private val endLabels = all[Block].map(b => (b, segment.newLabel)).toMap

    /** Returns label of `b` code start. */
    protected final def startOf(b: Block): Label = startLabels(b)

    /** Returns label of `b` code end. */
    protected final def endOf(b: Block): Label = endLabels(b)

    /** Sets up current [[Segment]] and [[XInfo]] and executes `action`. */
    private def withSegmentAndXInfo[T](segment: Segment, xInfo: XInfo)(action: => T): T = withXInfo(xInfo) {
      var result: T = 0.asInstanceOf[T]
      asm.withSegment(segment) { result = action }
      result
    }

    private val slowPathStubsSegment = new Segment
    private val slowPathStubsXInfo = new XInfo

    /** Runs `action` in [[slowPathStubsSegment]] and returns label to a start of generated code. */
    protected final def slowPathStub(action: => Unit): Label = withSegmentAndXInfo(slowPathStubsSegment, slowPathStubsXInfo) {
      val label = asm.newBoundLabel
      action
      label
    }

    ///////////////////////////////////////////////////////////////////////////
    // Node matchers - extract concrete resources and types from node.

    protected object NodeWithResource {
      def unapply(n: Node): Option[Resource] = if (n.mayHaveResource) Some(n.resource) else None
    }

    protected object RegNode {
      def unapply(n: Node): Option[AnyReg] = condOpt(n) { case NodeWithResource(r: AnyReg) => r }
    }

    protected object IRegNode {
      def unapply(n: Node): Option[IREG] = condOpt(n) { case NodeWithResource(r: IReg) => r.asInstanceOf[IREG] }
    }

    protected object FRegNode {
      def unapply(n: Node): Option[FREG] = condOpt(n) { case NodeWithResource(r: FReg) => r.asInstanceOf[FREG] }
    }

    protected object MemNode {
      def unapply(n: Node): Option[Mem] = condOpt(n) { case NodeWithResource(slot: FrameSlot) => slot.mem as asmType(n) }
    }

    protected object AltLocationNode {
      def unapply(n: Node): Option[Mem] = condOpt(n) { case NodeWithResource(AltLocation(slot)) => mem(PTR, frame.EER, eeOffsetFor(slot)) }

      private def eeOffsetFor(slot: Int) = RTConst.ExecEnv.Offsets.altLocation(slot).intValue
    }

    protected object Immediate32 {
      def unapply(n: Node): Option[Int] = condOpt(n) {
        case DWordConst(c) => c
        case FConst(f) => floatToRawIntBits(f)
        case ZeroValueNode() => 0
      }
    }

    protected def asmType(n: Node) = ValueType.toAsm(n.tpe)

    protected def iReg(node: Node) = node.resource.asIReg.asInstanceOf[IREG]

    protected def fReg(node: Node) = node.resource.asFReg.asInstanceOf[FREG]

    protected def reg(node: Node) = node.resource.asReg

    protected def memLoc(`type`: AsmType, n: Node): Mem = n match {
      case lea @ Lea.Base(base, disp) if lea.attachedAsArg =>
        mem(`type`, iReg(base), disp)
      case lea @ Lea.Scaled(base, index, scale, 0) if lea.attachedAsArg =>
        val indexReg = iReg(index) // index is zero-extended
        if (scale == 1) {
          mem(`type`, iReg(base), indexReg)
        } else {
          mem(`type`, iReg(base), scaled(indexReg, `type`.width))
        }
      case _ =>
        mem(`type`, iReg(n))
    }

    protected def widthOf(tpe: Type): Width = tpe match {
      case _: StructureType => Width.WPTR
      case TypeWithSize(w) => Width(w)
    }

    protected def widthOf(n: Node): Width = widthOf(n.tpe)

    protected def getAttachedCondVal(fp: FlagProducer): CondVal =
      fp.singleAttachedByReason(Group.AttachReason.COND_VAL_RESULT).get.asInstanceOf[CondVal]


    ///////////////////////////////////////////////////////////////////////////
    // Node generators - gen code for one node.

    /** Generates code for `node`. */
    private def genNode(node: Node): Unit = {
      bindDebugLabels(node)
      genNodeImpl(node)
    }

    /** Generates machine-dependent code for `node`. */
    protected def genNodeImpl(node: Node): Unit = node match {
      case x: Transfer          => genTransfer(x)
      case x: Call              => genCall(x)
      case x: PreCall           => genPreCall(x)
      case x: AbstractNullCheck => genNullCheck(x)
      case x: FrameHeader       => genFrameHeader(x)
      case x: Throw             => genThrow(x)
      case x: TDBarrier         => genTDBarrier(x)
      case x: LoadTailParam     => genLoadTailParam(x)

      case phi: Phi =>
        assert(phi.resource == uniqueValue(phi.args map {_.resource}).get)

      case execEnv: ExecEnv =>
        assert(rootMethod.hasManagedExecEnv && iReg(execEnv) == frame.EER)

      case memBarrier: MemBarrier =>
        emit.memBarrier(memBarrier.kinds.toSeq*)

      case va: VarArguments =>
        frame.loadCVarArgsAddrTo(iReg(va), null)

      case deprive: Deprive =>
        genDeprive(iReg(deprive), iReg(deprive.obj))

      case covCounter: CoverageCounter =>
        genCoverageCounter(covCounter.locs)

      case x: MutFunc.Offset =>
        emit.sub(iReg(x), iReg(x.record), iReg(x.host), WPTR)

      case x: MutFunc.Combine =>
        emit.add(iReg(x), iReg(x.host), iReg(x.offset), WPTR)

      case _: DebugBreakpoint =>
        // CJDB will ignore source region, if it will not contain any code. To simplify code generation we just
        // generate NOP at each breakpoint.
        genNop()

      case _ if noCodeShouldBeGenerated(node) =>

      case _: BlockEnd => // genBlockEnd should work with these nodes

      case _ => shouldNotReachHere("gen code for: " + node)
    }

    protected def genBranchIfCmp(op: BranchOp, l: Node, r: Node, width: Width, target: Label): Unit = r match {
      case _: AnyNull         => emit.branchIf(op, iReg(l), 0,   width, target)
      case IntegralConst(imm) => emit.branchIf(op, iReg(l), imm, width, target)
      case IRegNode(ir)       => emit.branchIf(op, iReg(l), ir,  width, target)
      case FRegNode(fr)       => emit.branchIf(op, fReg(l), fr,  width, target)
    }

    private def genBranchIfTest(op: BranchOp, l: Node, r: Node, width: Width, target: Label): Unit = r match {
      case IntegralConst(imm) => emit.branchIfTest(op, iReg(l), imm, width, target)
      case IRegNode(r)        => emit.branchIfTest(op, iReg(l), r,   width, target)
    }

    /** Generates code for end of `block`. */
    protected def genBlockEnd(block: Block, isNext: Block => Boolean): Unit = block.blockEnd match {
      case Goto(_, target) =>
        genJump(target, isNext)

      case _: Return =>
        genReturn()

      case _: Halt =>
        genHalt()

      case branch: If =>
        assert(branch.hasAttachedByReason(Group.AttachReason.COND_BRANCH_ARG))
        val (condition, isFP, directJmpBlock, condJmpBlock) = prepareGenBranch(branch, isNext)
        val target = startOf(condJmpBlock)
        branch.selector match {
          case x: Cmp  => genBranchIfCmp  (branchOp(condition, x.keyType), x.l, x.r, widthOf(x.keyType), target)
          case x: Test => genBranchIfTest (branchOp(condition, x.keyType), x.l, x.r, widthOf(x.keyType), target)
        }
        genJump(directJmpBlock, isNext)
    }

    protected final def trapPageAddress =
      RTConst.ExecEnv.memoryManagerData.offset + RTConst.ThreadLocalMMData.gcPointsTLD.offset + RTConst.GCPoints.ThreadLocalData.gcPointTrapAddressUnion.offset

    private def genFrameHeader(frameHeader: FrameHeader): Unit = {
      assert(rootMethod.hasFrameDescriptor)
      ensureFullFrame()
      emit.mov(iReg(frameHeader), stackPointer)
    }

    protected def genNullCheckImpl(nullCheck: AbstractNullCheck): Unit

    private def genNullCheck(nullCheck: AbstractNullCheck): Unit = {
      assert(!nullCheck.isImplicit)
      assert(!nullCheck.trusted)
      addXSite(nullCheck)
      genNullCheckImpl(nullCheck)
    }

    protected def genJump(target: Label): Unit = emit.jump(target)

    protected def genJump(target: Block, isNext: Block => Boolean): Unit =
      if (!isNext(target)) genJump(startOf(target))

    protected def genReturn(): Unit = frame.genDestroy(true)

    private def genHalt(): Unit = {} // NOP (for more details look at insertFatalErrorBeforeHalt).

    protected def genTransferImpl(transfer: Transfer): Unit = (transfer, transfer.transferArg) match {
      case (RegNode(dst), RegNode(src)) =>
        emit.copyAny(dst, src, asmType(transfer))

      case (IRegNode(dst), sa: HasFrameSlot) =>
        emit.lea(dst, sa.slot.mem)

      case (IRegNode(dst), ac: AddrConst) =>
        emit.lea(dst, mem(PTR, ac.symbol, ac.offset))

      case (RegNode(dst), src: Constant) =>
        val imm = src match {
          case IntegralConst(ic) => ic
          case FConst(fc) => floatToRawIntBits(fc).toLong
          case DConst(dc) => doubleToRawLongBits(dc)
          case _: AnyNull => 0L
          case _ => shouldNotReachHere(s"unexpected constant $src")
        }
        emit.mov(dst, imm, asmType(transfer).width)

      case (RegNode(dst), MemNode(src)) =>
        // [[NoAvailableScratchError]] exception may be thrown out of here because if spill slot have huge offset
        // from SP, CodeEmitter could require scratch register for load. As we do not know spill size and slots order
        // in LocalGenerator, we have not allocate one for Transfer node. This problem should not be actual for O2
        // compilation mode because of spill slots sorting in [[FrameComponent]] but for O1 it is still actual (look
        // at JET-15980). So for O1 compilation mode for platforms with problems with huge offsets (ARM64) we exclude
        // one register from allocator and give it as scratch register for CodeEmitter.
        //
        // If one day this problem will return for O2 we should check `maxSpillPressure` in [[BGCMHints]] and make the
        // same workaround or re-enter in back-end with O1 compilation mode. TODO: make back-end re-enterable.
        emit.load(dst, src)

      case (MemNode(dst), RegNode(src)) =>
        // See comment in (RegNode(dst), MemNode(src)) case.
        emit.store(dst, src)

      case (MemNode(dst), Immediate32(imm)) =>
        emit.store(dst, imm)

      case (RegNode(dst), AltLocationNode(src)) =>
        emit.load(dst, src)

      case (AltLocationNode(dst), RegNode(src)) =>
        emit.store(dst, src)

      case (AltLocationNode(dst), Immediate32(imm)) =>
        emit.store(dst, imm)

      case (dst, src) =>
        shouldNotReachHere(s"unexpected transfer from ${src.resource} to ${dst.resource}")
    }

    private def genTransfer(transfer: Transfer): Unit = {
      val arg = transfer.transferArg
      assert(applicableResourcesForTransfer(transfer.resource, arg.resource, arg),
        s"not applicable resources for transfer: from = ${arg.resource}, to = ${transfer.resource}, arg = $arg")

      SpillStats.collect(transfer, arg)
      if (transfer.resource != arg.resource) genTransferImpl(transfer)
    }

    protected def slowPathThrowingStub(node: CanThrow, throwProc: RTSProc): Label = slowPathStub {
      val target = env.getRTSProc(throwProc)
      assert((rootMethod.isManaged == target.isManaged) && target.isAjNoReturn)

      ensureFullFrame()
      emit.bind(SyntheticCodeLabel())
      emit.call(target)
      addXSite(node)
    }

    protected def genNop(): Unit

    protected def genThrow(throwNode: Throw): Unit = {
      // Throw nodes with known handler should be replaced by Goto in SimplifyComponent.replaceThrowByGoto
      assert(!throwNode.hasXHandler)

      ensureFullFrame()
      val reg = ScalaCollections.singleElement(throwNode.spoiled).asIReg

      emit.load(reg, mem(PTR, frame.EER, RTConst.ExecEnv.Offsets.threadEnv.intValue))
      emit.store(mem(PTR, reg, RTConst.ThreadEnv.exceptionContext.offset + RTConst.ExceptionContext.pendingExceptionObj.offset), iReg(throwNode.inValue))
      if (throwNode.shouldPreventBareSOEInstantiation) {
        val soeInstantiationCheckRequiredOffset = RTConst.ThreadEnv.exceptionContext.offset + RTConst.ExceptionContext.soeInstantiationCheckRequired.offset
        emit.store(mem(PTR, reg, soeInstantiationCheckRequiredOffset), 0)
      }
      emit.jump(env.getRTSProc(ExceptionHandling_trivialHandler))
    }

    protected def flagProducerProperties(flagProducer: Node, negated: Boolean): (Condition, Boolean) = {
      val (condition, isFP) = flagProducer match {
        case cmp: Cmp    => (cmp.op, cmp.keyType.isFloatingPointType)
        case cmp: CmpCAS => (cmp.op, cmp.keyType.isFloatingPointType)
        case test: Test  => (test.op, false)
        case _: TauTest  => (Condition.EQ, false)
      }
      (if (negated) condition.negate(isFP) else condition, isFP)
    }

    protected def prepareGenBranch(branch: If, isNext: Block => Boolean): (Condition, Boolean, Block, Block) = {
      val negated = isNext(branch.trueBlock)
      val (condition, isFP) = flagProducerProperties(valueOf(branch.selector).producer, negated)
      val (directJmpBlock, condJmpBlock) = if (negated) {
        (branch.trueBlock, branch.falseBlock)
      } else {
        (branch.falseBlock, branch.trueBlock)
      }
      (condition, isFP, directJmpBlock, condJmpBlock)
    }

    protected def genDeprive(dst: IREG, src: IREG): Unit

    /** Merge IMT field from `imt` register and pointer from `ptr` register and put result into `dst` register. */
    protected def mergeRichPointer(dst: IREG, imt: IREG, ptr: IREG): Unit

    protected def genLoadTailParam(ltp: LoadTailParam): Unit = ltp match {
      case LoadTailParam(tail, offset) =>
        emit.load(reg(ltp), mem(asmType(ltp), iReg(tail), offset))
    }

    private def genTDBarrier(barrier: TDBarrier): Unit = {
      val obj = iReg(barrier.obj)
      val res = iReg(barrier)
      val tmp = barrier.spoiled.head.asIReg
      assert(obj == res)

      val exit = asm.newLabel
      if (barrier.argMayBeNull) {
        emit.branchIfNull(obj, exit)
      }

      def makeBarrier(deprived: IREG): Unit = {
        addXSite(barrier)
        val start = asm.newBoundLabel
        emit.load(tmp, mem(PTR, deprived, RTConst.HeapObj.TYPEDESC_OFFSET.intValue))
        if (env.enabled(IdescHigh16BitsCleaning)) {
          emit.lsli(tmp, tmp, 16, WPTR)
          emit.lsri(tmp, tmp, 16, WPTR)
        }
        val end = asm.newBoundLabel
        emit.load(tmp, mem(PTR, tmp, end.position - start.position))
      }

      if (!barrier.argMayBeRich) {
        makeBarrier(obj)
      } else {
        // TODO: in 99% number of cases, td barrier was not triggered and this "copy enrichment" does not change anything.
        //  (JET-14122 task)
        val deprived = barrier.spoiled(1).asInstanceOf[IREG]
        genDeprive(deprived, obj)
        makeBarrier(deprived)
        mergeRichPointer(obj, obj, deprived)
      }

      emit.bind(exit)
    }


    ///////////////////////////////////////////////////////////////////////////
    // Spill statistics

    private object SpillStats {
      private lazy val loops = cfg.loops

      private def loopWithoutCalls(loop: Loop[Block]) = loop != null &&
        (loop.body filterNot cold flatMap (_.spine) forall (!_.isInstanceOf[AbstractCall]))

      private var enabled: Boolean = false
      def init(b: Block): Unit = {
        enabled = stats.isEnabled(StatsKind.SpillInPGOLoopsWithoutCalls) && profile.isPGOHost && !cold(b) && loopWithoutCalls(loops loopOf b)
        if (enabled) {
          statsGlobal.count(StatsKind.SpillInPGOLoopsWithoutCalls, s"counted in ${rootMethod.getFullName} block")
        }
      }

      def collect(node: Node, arg: Node): Unit = {
        if (enabled) {
          if (node.allocatedToFrameSlot) {
            statsGlobal.count(StatsKind.SpillInPGOLoopsWithoutCalls, s"store at ${rootMethod.getFullName}")
          }
          if (arg.allocatedToFrameSlot) {
            statsGlobal.count(StatsKind.SpillInPGOLoopsWithoutCalls, s"load at ${rootMethod.getFullName}")
          }
        }
      }
    }


    ///////////////////////////////////////////////////////////////////////////
    // Main generation script.

    private[codegen] lazy val cold = findColdBlocks()

    /** Generates assembler code for rootMethod and returns final code segment with some attributes. */
    final def genCode(): Code = {
      calculateUsedResourcesAndMakeFrame()
      checkDebugInfoConsistency()

      val layout = makeLayout()

      var expectedXSitesCount = 0
      var slowPathStubStart: Label = null

      // Generate resulting code segment for the method
      withSegmentAndXInfo(segment, xInfo) {
        asm.bind(methodStart)
        collectLocalVarsDebugInfo(stackPointer)

        // Emit prologue code
        frame.genBuildAndAdjustParams(needFrameDescriptor)

        for (case Seq(block: Block, nextB) <- (layout.order :+ null).sliding(2)) {
          layout.alignment.get(block) foreach { asm.alignCode }
          for (x <- layout.withAliases(block)) asm.bind(startOf(x))

          appendBlockDebugInfo(block)
          val positionAfterHints = asm.currentPosition
          SpillStats.init(block)
          initDebugLabelsForBlock(block)
          genXHandlerInfo(block)

          CodeOrder in block foreach { node =>
            assert(node.isGroupRoot)
            if (needXSite(node)) expectedXSitesCount += 1
            genNode(node)
          }
          genBlockEnd(block, succ => layout.isAliasOf(succ, nextB))

          for (x <- layout.withAliases(block)) asm.bind(endOf(x))

          checkConsistency(CheckLevels.Desirable) {
            val acceptableEmptyBlock = block == entryBlock || CodeOrder.in(block).forall {
              case _: Halt => true
              case n => noCodeShouldBeGenerated(n)
            }
            assert(isO1Compiled || genDebug || (positionAfterHints != asm.currentPosition) || acceptableEmptyBlock,
              s"Empty block in layout: ${CodeOrder.in(block).mkString("[", ", ", "]")}")
          }
        }

        // Append code of generated slow path stubs
        if (slowPathStubsSegment.nonEmpty) {
          slowPathStubStart = asm.newBoundLabel
          asm appendCode slowPathStubsSegment
          xInfo.addXInfo(slowPathStubsXInfo)
        }

        asm.alignStart(requiredMethodAlignment)
        doFreeze()
      }

      assert(expectedXSitesCount == xInfo.getCollectedXSites.size)
      xInfo.prepare(segment)

      genCode0(segment, layout, xInfo, methodStart, slowPathStubStart)
    }

    protected def genCode0(segment: Segment, layout: Layout, xInfo: XInfo, methodStart: Label, slowPathStubStart: Label): Code
    
    protected def doFreeze(): Unit

    protected def genXHandlerInfo(b: Block): Unit = { /* do nothing */ }
  }

  trait CodeGeneratorImplMach { self: CodeGeneratorImpl =>
    override def genCode0(segment: Segment, layout: Layout, xInfo: XInfo, methodStart: Label, slowPathStubStart: Label): Code = {
      val siberiaStart = layout.coldStart.map(startOf).getOrElse(slowPathStubStart)

      val siberiaOffset = if (siberiaStart != null && profile.isPGOHost) {
        siberiaStart.position
      } else {
        RTConst.MethodInfoFrameDescriptor.UNKNOWN_SIBERIA_OFFSET.intValue
      }

      val markedRegions = calculateMarkedRegions(methodStart, siberiaStart, layout.order)
      CodeMach(segment, xInfo, markedRegions, siberiaOffset)
    }

    override def doFreeze(): Unit = {
      asm.freeze()
    }
  }
}
