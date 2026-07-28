/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.backend

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.common.LanguagePack.SCALA
import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.Location.{IReg, mem}
import com.huawei.excelsior.jet.assembler.Width.W32
import com.huawei.excelsior.jet.assembler.{AsmEmitter, AsmType, Label, Location, Segment, Symbol}
import com.huawei.excelsior.jet.codeemitter.BarrierKind.{LOAD_LOAD, LOAD_STORE, STORE_LOAD, STORE_STORE}
import com.huawei.excelsior.jet.codeemitter.{BranchOp, CodeEmitter}
import com.huawei.excelsior.jet.common.{BuiltInField, DAIRefKind, XString}
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.Env.{languagePack, tailRegister}
import com.huawei.excelsior.jet.compiler.NotImplementedFeature.ERROR_CATCH_TYPE
import com.huawei.excelsior.jet.compiler.abi.{DAIGenerator, Frame}
import com.huawei.excelsior.jet.compiler.bytecode.*
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeIterator.INVALID_IDX
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool.{Access, ErrorAccessInfo}
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPoolAccessResult.*
import com.huawei.excelsior.jet.compiler.bytecode.parsing.BlockDataFlowParser.RET_ADDR_KIND
import com.huawei.excelsior.jet.compiler.bytecode.parsing.XHInfo
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels
import com.huawei.excelsior.jet.compiler.ir.{InlineContext, XInfo, XSiteKind}
import com.huawei.excelsior.jet.compiler.lambda.LambdaTypeGenerator
import com.huawei.excelsior.jet.compiler.newbaseline.DEBUG_PRINT
import com.huawei.excelsior.jet.compiler.newbaseline.backend.MethodBytecodeGenerator.*
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.Generator
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.*
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.NodeType.TREF
import com.huawei.excelsior.jet.compiler.newbaseline.frontend.Block.End
import com.huawei.excelsior.jet.compiler.newbaseline.frontend.{BaseParser, Block, BlockLivenessAnalyzer}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{GenTDBarriers, GenerateWriteBarriers, PreparationAsserts}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodAJCallKind
import com.huawei.excelsior.jet.compiler.symlevel.SigPolyMethodID.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{JavaArray, Primitive}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.fromBytecode
import com.huawei.excelsior.jet.util.SuffixTree
import xscala.util.MathUtils

import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object MethodBytecodeGenerator {
  case class GenerationResult (body: Segment, frame: Frame[?, ?, ?], xinfo: XInfo)
}

abstract class MethodBytecodeGenerator[ASM <: AsmEmitter, CE <: CodeEmitter] (
        protected val env: Environment,
        protected val rootInlineContext: InlineContext,
        private val slots: Slots,
        private val globalInfo: GlobalInfo) {

  protected def rootMethod = rootInlineContext.method
  protected def rootDeclaringClass = rootMethod.getDeclaringClass

  private def isInJavaClass = rootDeclaringClass.isJavaReference
  private implicit def typeProvider: TypeProvider = env.getTypeProvider

  protected val symbolLinker = env.getSymbolLinker(rootMethod)

  protected val asm = createAssembler()
  protected val globalLocations = createGlobalLocations()
  protected val frame = globalLocations.frame
  protected val emit = createCodeEmitter()

  private val blockLabels = new Array[Label](globalInfo.blocksCount)
  private lazy val labelForEpilogue = emit.newLabel

  private val previousBlocks = mutable.HashSet[Block]()

  private var globalReturnValueLoc: Location = null
  private var monitorLocForSynchronized: Location.Mem = null

  private var exceptionLocForTransitionToHandler: Location.Mem = null
  private var transitionToHandlerLabels: Array[Label] = null

  private var xinfo: XInfo = new XInfo
  private var exceptions: ExceptionHandlersGenerator = null

  private var needMemBarBeforeRootReturn = false

  private def labelForBlock(block: Block) = {
    val alreadyCreated = blockLabels(block.id)
    if (alreadyCreated != null) {
      alreadyCreated
    } else {
      val label = emit.newLabel
      blockLabels(block.id) = label
      label
    }
  }

  protected def createAssembler(): ASM
  protected def createGlobalLocations(): GlobalLocations
  protected def createCodeEmitter(): CE

  private def createGeneratorWithoutHandlerForRootMethod() = createGenerator(new Generator.XSitesWithoutHandler(xinfo))

  private def createGenerator(xSiteCreator: Generator.XSiteCreator): Generator = {
    val locations = new Locations(globalLocations, emit)
    val nodes = new Nodes(locations, emit, frame)
    locations.nodes = nodes
    createGenerator(locations, nodes, xSiteCreator)
  }

  protected def createGenerator(locations: Locations, nodes: Nodes, xSiteCreator: Generator.XSiteCreator): Generator

  /** @param block     this is the block which will be generated now
    * @param nextBlock this is the block which will be generated next, may be used to fallthrough
    */
  def genBlock(block: Block, nextBlock: Block, blockLiveness: BlockLivenessAnalyzer): Unit = {
    val bcGen = new BlockGenerator(block, nextBlock, blockLiveness)

    if (block.isHandler) {
      val transitionToHandler = emit.newBoundLabel
      transitionToHandlerLabels(block.id) = transitionToHandler
      genTransitionToHandler(block)
    }

    emit.bind(labelForBlock(block))
    previousBlocks += block

    bcGen.iterateBytecode()
    bcGen.genBlockEnd()
  }

  /** Initialize params location, generate prolog, etc.
    * @param entryBlock                this is the entry block of this method which should be executed first
    * @param nextBlock                 this is the block which will be generated next, may be used to fallthrough
    * @param blocks                    all method blocks
    * @param hasExceptionHandlers      `true` if there are exception handlers
    * @param exceptionHandlersTreeRoot root of exception handlers tree
    */
  def startMethod(entryBlock: Block, nextBlock: Block, blocks: collection.Seq[Block],
                  hasExceptionHandlers: Boolean, exceptionHandlersTreeRoot: SuffixTree[XHInfo[Block]]): Unit = {

    exceptions = new ExceptionHandlersGenerator(exceptionHandlersTreeRoot)

    emit.setUp()

    val gen = createGeneratorWithoutHandlerForRootMethod()
    val nodes = gen.nodes

    val params = new ArrayBuffer[Node]
    var thisParam: Node = null
    var localIdx = 0
    for (p <- 0 until rootMethod.getParamsCount) {
      val slotIdx = slots.localIdx(localIdx)
      localIdx += 1
      val paramTypeKind = rootMethod.getParamType(p).jbcKind

      if (globalInfo.isSlotAliveAtBlockStart(slotIdx, entryBlock)) {
        val param = Node.forInputSlot(NodeType.by(paramTypeKind), slotIdx)
        params += param
        nodes.bind(param, gen.receiveParameter(p))
        if (rootMethod.hasReceiverParameter && p == rootMethod.getReceiverArgIdx) thisParam = param
      }

      if (paramTypeKind.is2Slots) {
        val longHalfSlotIdx = slots.localIdx(localIdx)
        localIdx += 1
        assert(!globalInfo.isSlotAliveAtBlockStart(longHalfSlotIdx, entryBlock))
      }
    }

    if (needsThisParamForPrologue && (thisParam == null)) {
      val receiverIdx = rootMethod.getReceiverArgIdx
      // this may be not alive in this method, so we create temporary node
      thisParam = Node.newTemporary(NodeType.by(rootMethod.getParamType(receiverIdx).jbcKind))
      nodes.bind(thisParam, gen.receiveParameter(receiverIdx))
    }

    genPrologue(gen, entryBlock, nextBlock, thisParam)
    // thisParam is not usable any more because it may be released in genProlog

    for (param <- params) {
      val slotIdx = param.definition.asInstanceOf[Node.InputSlotPosition].slotIdx
      globalInfo.setLocationAtBlockStart(entryBlock, slotIdx, nodes.getLoc(param))
      globalInfo.setTypeAtBlockStart(entryBlock, slotIdx, param.`type`)
    }

    if (hasExceptionHandlers) {
      initializeGlobalLocationsForHandlers(gen.locations, blocks)
    }

    needMemBarBeforeRootReturn = false

    emit.bind(new DebugLabels.PrologueEndLabel)
  }

  private def needsThisParamForPrologue = !rootMethod.isStatic && rootMethod.isSynchronized

  private def genPrologue(gen: Generator, entryBlock: Block, nextBlock: Block, thisParam: Node): Unit = {
    if (!globalInfo.structuredLocking) {
      gen.rtsCall(RTSProc.JR_FillStructuredCheckingTable)()
    }

    val method = rootMethod
    val methodHostClass = method.getDeclaringClass

    if (env.enabled(PreparationAsserts) && method.isManaged && methodHostClass.preparationRequired) {
      gen.ensurePrepared(methodHostClass, method, PreparationKind.PROLOGUE_ASSERTION)
    }

    if (method.isStatic) {
      gen.genClinit(methodHostClass)
    }

    if (method.isSynchronized) {
      val monitor = if (method.isStatic) {
        val n = Node.newTemporary(TREF)
        gen.loadCurrentClassObject(n)
        n
      } else {
        assert(thisParam != null)
        thisParam
      }

      assert(monitorLocForSynchronized == null)
      monitorLocForSynchronized = gen.copyRefValueToNewTracedFrameSlot(monitor)

      val enterProc = if (!globalInfo.structuredLocking) RTSProc.JR_CheckedMonitorEnter else RTSProc.JR_MonitorEnter

      gen.rtsCall(enterProc)(monitor)
    }
    // thisParam is not usable any more because it may be released in call to JR_MonitorEnter

    if (env.isTurboClinitHost(rootMethod)) {
      for (c <- typeProvider.getTurboClinitedClasses) {
        assert(!c.isPreClinited && !c.isTurboClinitedIn(method))
        gen.genClinit(c)
      }
    }

    if (entryBlock != nextBlock) {
      emit.jump(labelForBlock(entryBlock))
    } // else fallthrough
  }

  private def initializeGlobalLocationsForHandlers(locations: Locations, blocks: Iterable[Block]): Unit = {
    // All handlers have input nodes on the same locations.

    for (s <- 0 until slots.totalCount) {
      var loc: Location.Mem = null
      for (handler <- blocks if handler.isHandler && globalInfo.isSlotAliveAtBlockStart(s, handler)) {
        if (loc == null) {
          // We have to create frame slots which are large enough to store any scalar value.
          loc = globalLocations.allocateOnStackUntraced(locations.maxNodeAsmType)
        }
        globalInfo.setLocationAtBlockStart(handler, s, loc)
      }
      if (loc != null) {
        globalInfo.setLocationAtHandlersStart(s, loc)
      }
    }

    // Set type for stack slot containing exception object.
    // Exception object is placed there while exception catching.
    for (handler <- blocks if handler.isHandler) {
      val exceptionSlot = slots.stackIdx(handler.exceptionObjStackIdx)
      if (globalInfo.isSlotAliveAtBlockStart(exceptionSlot, handler)) {
        globalInfo.setTypeAtBlockStart(handler, exceptionSlot, TREF)
      }
    }

    exceptionLocForTransitionToHandler = globalLocations.allocateOnStackUntraced(TREF.toAsm)

    transitionToHandlerLabels = new Array[Label](globalInfo.blocksCount)
  }

  /** This generated code is executed between universal handler and real block-handler.
    * The only thing to do is to place exception object to proper slot of block-handler.
    */
  private def genTransitionToHandler(block: Block): Unit = {
    val exceptionSlotIdx = slots.stackIdx(block.exceptionObjStackIdx)
    if (globalInfo.isSlotAliveAtBlockStart(exceptionSlotIdx, block)) {
      emit.copyAny(globalInfo.locationAtBlockStart(block, exceptionSlotIdx), exceptionLocForTransitionToHandler, PTR)
    }
  }

  private def getMethodMonitor(gen: Generator) = {
    assert(monitorLocForSynchronized != null)
    val monitor = Node.newTemporary(TREF)
    val reg = gen.nodes.bindToAnyFreeIReg(monitor)
    emit.load(reg, monitorLocForSynchronized)
    monitor
  }

  /** Generate frame, epilogue, etc. and return code segment. */
  def finishMethod = {
    genEpilogue()
    val body = emit.tearDown()

    // we generate code for this handlers, but they are appended at the end of method's code
    exceptions.genSegmentWithHandlers()

    exceptions.defaultHandlerForSynchronized.genSegment()
    exceptions.defaultHandlerForUnstructuredLocking.genSegment()

    // we cannot use extra registers after generation of frame build/drop,
    // so we cannot use generators, nodes, locations from this moment

    emit.setUp(rootMethod)
    frame.makeLayout(Frame.Mode.FULL)
    frame.genBuildAndAdjustParams(rootMethod.hasFrameDescriptor)
    emit.appendCode(body)
    frame.genDestroy(true)

    exceptions.appendSegmentsWithHandlers()
    exceptions.defaultHandlerForSynchronized.appendSegment()
    exceptions.defaultHandlerForUnstructuredLocking.appendSegment()

    emit.alignStart(RTConst.MethodInfoFrameDescriptor.CODE_ALIGNMENT.intValue)
    val wholeMethod = emit.freeze().tearDown()
    exceptions.genExceptionTable(wholeMethod)

    GenerationResult(wholeMethod, frame, xinfo)
  }

  /** Generate some epilogue code (do synchronization, add gc-point, etc.). */
  private def genEpilogue(): Unit = {
    emit.bind(labelForEpilogue)

    val gen = createGeneratorWithoutHandlerForRootMethod()

    // return value should not be spoiled before this line,
    // bind some node to return value location to prevent spoiling by another actions
    val returnValue = if (globalReturnValueLoc != null) {
      Node.newTemporary(NodeType.by(rootMethod.getReturnType.jbcKind))
    } else {
      // note that method may have non void return value,
      // but no return value (forever loop or throw).
      null
    }
    if (returnValue != null) {
      val loc = gen.nodes.bindToAnyFreeLoc(returnValue)
      emit.copyAny(loc, globalReturnValueLoc, returnValue.asmType)
    }

    if (rootMethod.isSynchronized) {
      val monitor = getMethodMonitor(gen)
      val exitProc = if (!globalInfo.structuredLocking) RTSProc.JR_CheckedMonitorExit else RTSProc.JR_MonitorExit
      gen.rtsCall(exitProc, releaseBCParams = true)(monitor)
    }

    if (!globalInfo.structuredLocking) {
      gen.rtsCall(RTSProc.JR_CheckStructuredLockingOnNormalExit, releaseBCParams = true)()
    }

    if (rootMethod.shouldContainGCPointInEpilogueBeforeFrameDrop) {
      gen.genGCPoint()
    }

    if (returnValue != null) {
      gen.genReturnValue(returnValue)
    }
    // return value should not be spoiled after this line

    if (needMemBarBeforeRootReturn) {
      emit.memBarrier(STORE_STORE)
    }
  }

  private def moveNodesToLocations(nodes: Nodes, locations: Locations, srcNodes: collection.IndexedSeq[Node], dstLocs: collection.IndexedSeq[Location]): Unit = {
    // Set of already used output locations, it is used to prevent spoiling previous location
    val occupiedOutputLocs = new mutable.HashSet[Location]
    assert(srcNodes.length == dstLocs.length)
    for ((node, loc) <- srcNodes zip dstLocs) {
      if (loc != nodes.getLoc(node)) {
        nodes.rescueAndSpoilLoc(loc)
        val oldLoc = nodes.transfer(node, loc)
        if (occupiedOutputLocs contains oldLoc) {
          locations.acquire(oldLoc)
        }
      }
      occupiedOutputLocs += loc
    }
  }

  private final class ExceptionHandlersGenerator(xhInfoTreeRoot: SuffixTree[XHInfo[Block]]) {

    private val (handlersSegment, handlersLabels) = {
      if (rootMethod.hasFrameDescriptor && (xhInfoTreeRoot != null)) {
        (new Segment, mutable.LinkedHashMap.empty[HandlerWithTransfers, Label])
      } else {
        (null, null)
      }
    }

    abstract sealed class DefaultHandler {
      var label: Label = null
      var segment: Segment = null

      protected def shouldBeDefined: Boolean
      protected def genSegmentImpl(gen: Generator): Unit

      final def genSegment(): Unit = {
        if (!shouldBeDefined || label == null) {
          // there were no xsites with this default handler
          return
        }
        emit.setUp()
        val gen = createGeneratorWithoutHandlerForRootMethod()
        genSegmentImpl(gen)
        segment = emit.tearDown()
      }

      final def getLabel = {
        assert(shouldBeDefined)
        if (label == null) {
          label = emit.newLabel
        }
        label
      }

      final def appendSegment(): Unit = {
        if (segment == null) {
          assert(label == null)
        } else {
          emit.bind(label)
          emit.appendCode(segment)
        }
      }
    }

    final class SynchronizedDefaultHandler extends DefaultHandler {
      protected def shouldBeDefined = rootMethod.isSynchronized

      protected def genSegmentImpl(gen: Generator): Unit = {
        assert(isInJavaClass)
        gen.rtsCall(RTSProc.NewBaselineJavaExceptionsHandling_unlockAndRethrowWithSLCheck)(getMethodMonitor(gen))
      }
    }

    final class UnstructuredLockingDefaultHandler extends DefaultHandler {
      protected def shouldBeDefined = !globalInfo.structuredLocking

      protected def genSegmentImpl(gen: Generator): Unit = {
        val rtsProc = if (isInJavaClass) RTSProc.NewBaselineJavaExceptionsHandling_rethrowWithSLCheck
                      else RTSProc.NewBaselineExceptionTable_rethrow
        gen.rtsCall(rtsProc)()
      }
    }

    private var xhInfoTreeTableIndices: mutable.Map[SuffixTree[XHInfo[Block]], Int] = null
    private var exceptionTableSymbol: Symbol = null

    val defaultHandlerForSynchronized = new SynchronizedDefaultHandler
    val defaultHandlerForUnstructuredLocking = new UnstructuredLockingDefaultHandler


    private case class Transfer(src: Location, dst: Location, `type`: NodeType)
    private case class HandlerWithTransfers(xhInfoSeq: SuffixTree[XHInfo[Block]], transfers: ArrayBuffer[Transfer])

    def findHandler(block: Block, slotNodes: Array[Node], getLocation: Node => Location): Label = {
      assert(block.hasHandler)

      val transfers = ArrayBuffer.empty[Transfer]
      for (s <- 0 until slots.totalCount) {
        if (globalInfo.isSlotAliveAtHandler(s, block)) {
          val n = slotNodes(s)
          if (!n.isLongHalf) {
            transfers += Transfer(getLocation(n), globalInfo.locationAtHandlersStart(s), n.`type`)
          }
        }
      }
      val handler = HandlerWithTransfers(block.handlerInfoSequence, transfers)

      // Check if similar handler was already created and has label
      handlersLabels.getOrElseUpdate(handler, emit.newLabel)
    }

    def genSegmentWithHandlers(): Unit = {
      if (handlersLabels == null || handlersLabels.isEmpty) {
        return
      }

      frame.registerStackCheckForExceptionHandling()

      enumerateExceptionTable()

      assert(exceptionTableSymbol == null)
      exceptionTableSymbol = symbolLinker.makeDataSymbol()

      assert(handlersSegment != null)
      emit.setUp(handlersSegment)

      // The following generated code have some common parts but its deduplication
      // is a little bit tricky (note that all alive references should be in findHandler* call's GC map).
      // Size benefit is estimated to be less than 1% so currently it's better to have simple compiler's code.
      for ((handler, label) <- handlersLabels) {
        emit.bind(label)

        val gen = createGeneratorWithoutHandlerForRootMethod()
        val nodes = gen.nodes
        val locations = gen.locations

        // Move all alive nodes to proper locations and make them visible to GC maps builder.
        val nodeByLoc = mutable.HashMap.empty[Location, Node]
        val srcNodes = handler.transfers map { tr =>
          nodeByLoc.get(tr.src) match {
            case None =>
              val node = Node.newTemporary(tr.`type`)
              nodes.bind(node, tr.src)
              nodeByLoc(tr.src) = node
              node
            case Some(node) =>
              // there could not be two nodes on the same location, it is the same node
              assert(node.`type` == tr.`type`)
              node
          }
        }

        moveNodesToLocations(nodes, locations, srcNodes, handler.transfers.map(_.dst))

        // Find handler address using exception tables.
        val transitionToHandlerAddr = Node.newTemporary(NodeType.ADDR)
        locally {
          val domain = handler.xhInfoSeq.elem.domain.ordinal
          val table = exceptionTableSymbol
          val startIdx = xhInfoTreeTableIndices(handler.xhInfoSeq)
          if (rootMethod.isSynchronized) {
            assert(isInJavaClass)
            val monitor = getMethodMonitor(gen)
            val proc = RTSProc.NewBaselineJavaExceptionsHandling_findHandlerOrUnlockAndRethrowWithSLCheck
            gen.rtsCall(proc, transitionToHandlerAddr)(table, startIdx, domain, monitor)
          } else {
            val proc = if (isInJavaClass) RTSProc.NewBaselineJavaExceptionsHandling_findHandlerOrRethrowWithSLCheck
            else RTSProc.NewBaselineExceptionTable_findHandlerOrRethrow
            gen.rtsCall(proc, transitionToHandlerAddr)(table, startIdx, domain)
          }
        }

        locally {
          val xobj = Node.newTemporary(TREF)
          gen.rtsCall(RTSProc.JR_ObtainPendingException, xobj)()
          emit.store(exceptionLocForTransitionToHandler, nodes.loadToIReg(xobj))
          nodes.releaseLoc(xobj)
        }

        emit.jump(nodes.loadToIReg(transitionToHandlerAddr))
      }

      emit.tearDown()
    }

    def appendSegmentsWithHandlers(): Unit = {
      if (handlersSegment == null) {
        assert(handlersLabels == null)
      } else if (handlersSegment.isEmpty) {
        assert(handlersLabels.isEmpty)
      } else {
        emit.appendCode(handlersSegment)
      }
    }

    private def enumerateExceptionTable(): Unit = {
      assert(xhInfoTreeTableIndices == null)
      xhInfoTreeTableIndices = new mutable.HashMap[SuffixTree[XHInfo[Block]], Int]

      def process(treeElems: Iterable[SuffixTree[XHInfo[Block]]]): Unit = {
        for (treeElem <- treeElems) {
          xhInfoTreeTableIndices(treeElem) = xhInfoTreeTableIndices.size
          process(treeElem.getChildren)
        }
      }

      process(xhInfoTreeRoot.getChildren)
    }

    private def genExceptionTableEntry(seg: Segment, methodBody: Segment, xhInfo: XHInfo[Block], nextIdx: Int): Unit = {
      val handler = xhInfo.handler
      assert(handler != null)

      assert(MathUtils.isAligned(seg.length, RTConst.NewBaselineExceptionTable.Entry.alignment))
      var size = 0 // current generated size of entry

      assert(size == RTConst.NewBaselineExceptionTable.Entry.catchType.offset)
      val catchTypeIdx = if (xhInfo.isCatchAll) {
        RTConst.NewBaselineExceptionTable.CATCH_TYPE_ANY.intValue
      } else {
        val catchType = xhInfo.getCatchType(rootDeclaringClass.getClassConstantPool)
        if (catchType.isError) { // see JET-7092
          notImplemented(ERROR_CATCH_TYPE)
        }
        env.getImportedClassIdx(catchType.getObject, rootMethod)
      }
      seg.putW32(catchTypeIdx)
      size += 4

      assert(size == RTConst.NewBaselineExceptionTable.Entry.handlerOffset.offset)
      val transitionToHandlerOffset = methodBody.getLabelPosition(transitionToHandlerLabels(handler.id))
      assert(transitionToHandlerOffset >= 0)
      seg.putW32(transitionToHandlerOffset)
      size += 4

      assert(size == RTConst.NewBaselineExceptionTable.Entry.nextEntry.offset)
      seg.putW32(nextIdx)
      size += 4

      assert(size == RTConst.NewBaselineExceptionTable.Entry.handlerOfHandlerEntry.offset)
      val handlerOfHandlerIdx = if (handler.handlerInfoSequence != null) {
        xhInfoTreeTableIndices(handler.handlerInfoSequence)
      } else {
        RTConst.NewBaselineExceptionTable.INVALID_ENTRY_IDX.intValue
      }
      seg.putW32(handlerOfHandlerIdx)
      size += 4

      assert(size == RTConst.NewBaselineExceptionTable.Entry.size)
    }

    def genExceptionTable(methodBody: Segment): Unit = {
      if (exceptionTableSymbol == null) {
        return
      }

      val seg = new Segment(exceptionTableSymbol)
      seg.alignStart(RTConst.NewBaselineExceptionTable.alignment)
      assert(0 == RTConst.NewBaselineExceptionTable.entries.offset)
      var curIdx = 0

      def process(treeElems: Iterable[SuffixTree[XHInfo[Block]]], nextIdx: Int): Unit = {
        for (treeElem <- treeElems) {
          val thisElemIdx = curIdx
          assert(thisElemIdx == xhInfoTreeTableIndices(treeElem))
          genExceptionTableEntry(seg, methodBody, treeElem.elem, nextIdx)
          curIdx += 1
          process(treeElem.getChildren, thisElemIdx)
        }
      }

      process(xhInfoTreeRoot.getChildren, RTConst.NewBaselineExceptionTable.INVALID_ENTRY_IDX.intValue)
      symbolLinker.sendData(seg, rootMethod)
    }
  }

  private final class DeferredsGenerator (gen: Generator, cp: ConstantPool) {
    private val daiGenerator = DAIGenerator(env, symbolLinker, rootMethod)

    def genResolvableConstant(cpIndex: Int, tag: Tag, result: Node): Unit = {
      val dai = daiGenerator.forDirectCPEntryOperation(getRuntimeCPIndex(cpIndex))
      val loadProc = tag match {
        case Tag.METHOD_HANDLE => RTSProc.JR_LoadConstantMethodHandleThroughDAI
        case Tag.METHOD_TYPE   => RTSProc.JR_LoadConstantMethodTypeThroughDAI
        case Tag.CLASS         => RTSProc.JR_DeferredGetClassObject
        case _ => shouldNotReachHere(tag)
      }
      gen.rtsCall(loadProc, result)(dai.symbol)
    }

    def genNew(cpIndex: Int, result: Node): Unit = {
      val dai = daiGenerator.forDirectCPEntryOperation(getRuntimeCPIndex(cpIndex))
      gen.rtsCall(RTSProc.JR_DeferredNew, result)(dai.symbol)
    }

    def genNewArray(cpIndex: Int, allNum: Int, dimArray: Array[Node], result: Node): Unit = {
      val dai = daiGenerator.forDirectCPEntryOperation(getRuntimeCPIndex(cpIndex))
      val dims = gen.storeIntValuesToStackAllocatedArray(dimArray)
      gen.rtsCall(RTSProc.JR_DeferredNewArray, result)(dai.symbol, allNum, dimArray.length, dims)
    }

    def genInstanceOf(cpIndex: Int, `object`: Node, result: Node): Unit = {
      val dai = daiGenerator.forDirectCPEntryOperation(getRuntimeCPIndex(cpIndex))
      gen.rtsCall(RTSProc.JR_DeferredInstanceof, result, releaseBCParams = true)(dai.symbol, `object`)
    }

    def genCheckCast(cpIndex: Int, `object`: Node): Unit = {
      val dai = daiGenerator.forDirectCPEntryOperation(getRuntimeCPIndex(cpIndex))
      gen.rtsCall(RTSProc.JR_DeferredCast, releaseBCParams = true)(dai.symbol, `object`)
    }

    def genInstanceFieldRead(cpIndex: Int, fieldType: SignatureType, `object`: Node, result: Node): Unit = {
      genFieldOperation(cpIndex, fieldType, `object`, null, result)
    }

    def genInstanceFieldWrite(cpIndex: Int, fieldType: SignatureType, `object`: Node, value: Node): Unit = {
      genFieldOperation(cpIndex, fieldType, `object`, value, null)
    }

    def genStaticFieldRead(cpIndex: Int, fieldType: SignatureType, result: Node): Unit = {
      genFieldOperation(cpIndex, fieldType, null, null, result)
    }

    def genStaticFieldWrite(cpIndex: Int, fieldType: SignatureType, value: Node): Unit = {
      genFieldOperation(cpIndex, fieldType, null, value, null)
    }

    private def genFieldOperation(cpIndex: Int, fieldType: SignatureType, `object`: Node, value: Node, result: Node): Unit = {
      assert((value == null) == (result != null))
      val isWrite = value != null
      val isStatic = `object` == null
      val receiverNonNull = isStatic

      val thunkMT = DAIGenerator.methodTypeForDeferredFieldAccess(typeProvider, fieldType, isStatic, isWrite)
      val dai = daiGenerator.forFieldOperation(getRuntimeCPIndex(cpIndex), isWrite, isStatic, receiverNonNull)
      val thunkRef = new MethodReference(thunkMT, MethodReferenceAccessKind.STATIC, null, null, null)
      val params = DAIGenerator.FieldAccessParametersOrdering.forMethodInvocation(`object`, value, isWrite, isStatic)
      gen.genInvokeViaDAI(thunkRef, dai, params, result)
    }

    def genInvoke(cpIndex: Int, akind: MethodReferenceAccessKind, targetRef: MethodReference, params: collection.Seq[Node], result: Node, receiverNonNull: Boolean): Unit = {
      genInvokeWithRuntimeCPIndex(getRuntimeCPIndex(cpIndex), akind, targetRef, params, result, receiverNonNull)
    }

    private def genInvokeWithRuntimeCPIndex(rtCPIndex: Int, akind: MethodReferenceAccessKind, targetRef: MethodReference, params: collection.Seq[Node], result: Node, receiverNonNull: Boolean): Unit = {
      val dai = daiGenerator.forUnresolvedInvoke(akind, rtCPIndex, receiverNonNull)
      gen.genInvokeViaDAI(targetRef, dai, params, result)
    }

    private val MAX_JVM_ARITY = 255 // this is mandated by the JVM spec.

    def genInvokeDynamicOrSigPoly(cpIndex: Int, refKind: DAIRefKind, methodTypePar: MethodType, args: ArrayBuffer[Node], result: Node): Unit = {
      // Runtime code (j.l.i.MethodHandleNatives) defines when appendix argument is null or not null.
      // To simplify this logic we statically assume that all methods have appendix argument
      // except methods with MAX_JVM_ARITY arguments (because they have no place for extra argument).
      // This assumption is verified after resolve of the invoke via assert in RT code.
      var methodType = methodTypePar
      val hasAppendix = (methodType.parameterSlotCount + 1) /*appendix*/ <= MAX_JVM_ARITY
      if (hasAppendix) {
        methodType = methodType.appendParameterType(SignatureType.javaLangObject)
        val appendix = Node.newTemporary(TREF)
        val loc = gen.nodes.bindToAnyFreeIReg(appendix) // argument node must always be bound to some loc
        emit.movNull(loc) // prevent GC from seeing uninitialized location
        args += appendix
      }
      val dai = daiGenerator.forIndyOrSigpoly(refKind, getRuntimeCPIndex(cpIndex), hasAppendix)
      val methodRef = new MethodReference(methodType, MethodReferenceAccessKind.STATIC, null, null, null)
      gen.genInvokeViaDAI(methodRef, dai, args, result)
    }

    /** Returns index used by runtime to access data located at `originalCPIndex`. */
    private def getRuntimeCPIndex(originalCPIndex: Int) = {
      assert(originalCPIndex != DAIGenerator.NO_CP_INDEX)
      assert(Env.isJIT)
      // in JIT classfile is preserved, so originalCPIndex can be safely used since all data is saved as is
      originalCPIndex
    }
  }

  /** Class for interpretation of bytecode instruction and their generation using [[Generator]]. */
  private final class BlockGenerator(block: Block, _nextBlock: Block, blockLiveness: BlockLivenessAnalyzer)
    extends BaseParser[Node](rootMethod, block, slots) with Generator.XSiteCreator { blockGen =>

    /** Block that would be generated next. May be `null` if current `block` is the last block. */
    private val nextBlock = _nextBlock

    private val gen = createGenerator(this)
    private val deferred = new DeferredsGenerator(gen, cp)

    private val slotNodes = new Array[Node](blockGen.slots.totalCount)
    private val instrResultNodes = mutable.HashMap.empty[Int, Node]

    private var curBC = -1

    private var generatedBlockEnd = false

    override def xinfo = MethodBytecodeGenerator.this.xinfo

    private def locations = gen.locations
    private def nodes = gen.nodes

    locally {
      initLiveNodes()
      initLocationsAtBlockStart()
      setTypeAtHandlersAtBlockStart()
    }

    private def initLiveNodes(): Unit = {
      for (n <- blockLiveness.nodes) n.definition match {
        case d: Node.InputSlotPosition =>
          slotNodes(d.slotIdx) = n
        case d: Node.BCPosition =>
          instrResultNodes(d.bcOffset) = n
      }
    }

    private def initLocationsAtBlockStart(): Unit = {
      for (s <- slotNodes.indices if globalInfo.isSlotAliveAtBlockStart(s, block)) {
        val n = slotNodes(s)
        if (!n.isDead && !n.isLongHalf) {
          nodes.bind(n, globalInfo.locationAtBlockStart(block, s))
        }
      }
    }

    override def addXSite(gen: Generator, kind: XSiteKind, site: Label, bytecodeOffset: Int, lineNumber: Int,
                          inlineContext: InlineContext, calledMethodRef: MethodReference, softExceptionID: Int,
                          domain: Domain, additionalAliveLocations: Seq[(Node, Location)]): Unit = {
      val label = if (block.hasHandler) {
        exceptions.findHandler(block, slotNodes, nodes.locationsMapping)
      } else if (rootMethod.isSynchronized) {
        exceptions.defaultHandlerForSynchronized.getLabel
      } else if (!globalInfo.structuredLocking) {
        exceptions.defaultHandlerForUnstructuredLocking.getLabel
      } else {
        null
      }
      addXSiteImpl(gen, kind, site, label, bytecodeOffset, lineNumber, inlineContext, calledMethodRef, softExceptionID, domain, additionalAliveLocations)
    }

    /** Destroys SSA-form at block end.
      *
      * Either move all nodes from all live slots to some already fixed global locations or select global
      * locations for all live slots and copy nodes to this locations.
      */
    private def destroySSAFormAtBlockEndWithFixed(): Unit = {
      destroySSAFormAtBlockEnd(targetLocsMayBeFixed = true, null)
    }

    /** Destroys SSA-form at block end.
      *
      * Select global locations for all live slots and copy nodes to this locations.
      *
      * As we can select global locations, we can also save some registers (`untouchables`). We are
      * using this technique to generate branches/switches at block end. This approach based on condition
      * that blocks with branch/switch at their ends cannot have fixed locations, as we have no critical
      * edges and process blocks in top sort order.
      */
    private def destroySSAFormAtBlockEndWithoutFixed(untouchables: Location*): Unit = {
      destroySSAFormAtBlockEnd(targetLocsMayBeFixed = false, null, untouchables*)
    }

    /** @param returnValue may be `null`
      * @see [[destroySSAFormAtBlockEndWithoutFixed]]
      */
    private def destroySSAFormAtReturn(returnValue: Node): Unit = {
      destroySSAFormAtBlockEnd(targetLocsMayBeFixed = false, returnValue)
    }

    /** Destroys SSA-form at block end.
      *
      * Either move all nodes from all live slots to some already fixed global locations
      * (`moveLiveNodesToFixedLocations`) or select global locations for all live slots and copy
      * nodes to these locations (`selectGlobalLocationsForLiveNodes`).
      *
      * Parameter `targetLocsMayBeFixed` is `true` iff first way (fixed global locations) is allowed.
      *
      * If we can select global locations (`targetLocsMayBeFixed` is false), we can also save some
      * registers (`untouchables`). We are using this technique to generate branches/switches at
      * block end. This approach based on condition that blocks with branch/switch at their ends cannot
      * have fixed locations, as we have no critical edges and process blocks in top sort order.
      */
    private def destroySSAFormAtBlockEnd(targetLocsMayBeFixed: Boolean, returnValue: Node, untouchables: Location*): Unit = {
      if (targetLocsMayBeFixed) {
        assert(untouchables.isEmpty, "we can not guarantee some locations be untouchable if target locations may be fixed")
      }

      setTypesAtSuccessorsAtBlockEnd()
      prepareNodesForSSAFormDestroy(returnValue)

      def isAlive(s: Int) = globalInfo.isSlotAliveAtBlockEnd(s, block) && !slotNodes(s).isLongHalf // long halves actually do not have any location

      // Array of indices of slots alive at block end (not including long halves).
      val liveSlotIndices = (slotNodes.indices filter isAlive).toArray
      assert(liveSlotIndices.iterator.map(s => nodes.getLoc(slotNodes(s))).filterNot(Locations.isInvalid).toSet == locations.getAllBusy,
        "all used slots should be occupied by some alive node")

      if (liveSlotIndices.nonEmpty) {
        assert(returnValue == null, "there should be no transfers because returnValue was already placed on the right location and should not be overwritten")

        def targetLoc(s: Int) = globalInfo.locationAtBlockEnd(block, s)
        val fixedCount = liveSlotIndices count (targetLoc(_) != null)
        if (fixedCount == liveSlotIndices.length) {
          assert(targetLocsMayBeFixed, "all slots have fixed locations at block end but we do not know about it")
          moveNodesToLocations(nodes, locations, liveSlotIndices map slotNodes, liveSlotIndices map targetLoc)
        } else {
          assert(fixedCount == 0, "either all slots should have fixed locations at block end or none of them may have")
          selectGlobalLocationsForLiveNodes(liveSlotIndices, untouchables)
        }
      }
    }

    private def setTypesAtSuccessorsAtBlockEnd(): Unit = {
      // Note that we should set type for all live slots including long halves.
      for (s <- 0 until blockGen.slots.totalCount) {
        if (globalInfo.isSlotAliveAtBlockEnd(s, block)) {
          globalInfo.setTypeAtBlockEnd(block, s, slotNodes(s).`type`)
        }
      }
    }

    private def setTypeAtHandlersAtBlockStart(): Unit = {
      // Type of locals in handlers is equal to types of locals at the start of block.
      // It may not equal to type at the end of block if last instruction is store.
      // Note that we should set type for all live slots including long halves.
      if (block.hasHandler) {
        for (s <- 0 until blockGen.slots.totalCount) {
          if (globalInfo.isSlotAliveAtHandler(s, block)) {
            globalInfo.setTypeAtHandler(block, s, slotNodes(s).`type`)
          }
        }
      }
    }

    private def prepareNodesForSSAFormDestroy(returnValue: Node): Unit = {
      val aliveOnlyInHandlers = mutable.LinkedHashSet.empty[Node]
      if (block.hasHandler) {
        // Release nodes which are used in exception handlers...
        for (s <- 0 until blockGen.slots.totalCount) {
          if (globalInfo.isSlotAliveAtHandler(s, block)) {
            val n = slotNodes(s)
            if (!n.isLongHalf) aliveOnlyInHandlers += n
          }
        }
        // ... but not used in successors.
        for (s <- 0 until blockGen.slots.totalCount) {
          if (globalInfo.isSlotAliveAtBlockEnd(s, block)) {
            val n = slotNodes(s)
            aliveOnlyInHandlers -= n
          }
        }
      }

      // Note that we should first release nodes which are alive only in handlers
      // because they may be located on `globalReturnValueLoc`.

      if (returnValue != null) {
        // It will be placed on the right location and released a little bit later.
        aliveOnlyInHandlers -= returnValue
      }

      nodes.releaseLoc(aliveOnlyInHandlers)

      if (returnValue != null) {
        assert(nodes.hasLoc(returnValue))
        assert(!rootMethod.isConstructor, "constructors do not have explicit return value")

        if (globalReturnValueLoc == null) {
          globalReturnValueLoc = nodes.getLoc(returnValue)
        } else if (globalReturnValueLoc != nodes.getLoc(returnValue)) {
          assert(locations.isFree(globalReturnValueLoc), "there should be no alive node on globalReturnValueLoc")
          nodes.transfer(returnValue, globalReturnValueLoc)
        }
        nodes.releaseLoc(returnValue)
      }
    }

    private def selectGlobalLocationsForLiveNodes(liveSlotIndices: Array[Int], untouchables: Seq[Location]): Unit = {
      // Set of already used output locations, it is used to guarantee uniqueness of output locations.
      val occupiedOutputLocs = mutable.HashSet.empty[Location]

      // Prevent spoiling and using untouchables locations
      for (untouchable <- untouchables) {
        occupiedOutputLocs += untouchable
        if (locations.isFree(untouchable)) {
          locations.acquire(untouchable)
        }
      }

      for (s <- liveSlotIndices) {
        val n = slotNodes(s)
        val loc = nodes.getLoc(n)
        val outLoc = if (occupiedOutputLocs(loc)) {
          val outLoc = locations.getAnyFreeLocUnsafe(n.`type`)
          nodes.transfer(n, outLoc)
          locations.acquire(loc) // prevent spoiling previous location
          outLoc
        } else loc

        globalInfo.setLocationAtBlockEnd(block, s, outLoc)
        if (!Locations.isInvalid(outLoc)) {
          occupiedOutputLocs += outLoc
        }
      }
    }

    private def markGeneratedBlockEnd(): Unit = {
      assert(!generatedBlockEnd)
      generatedBlockEnd = true
    }

    def genBlockEnd(): Unit = {
      if (!generatedBlockEnd) {
        block.end.kind match {
          case End.Kind.GOTO =>  // Block without any explicit end in bytecode,
            jump()               // i.e. it was just splitted during control flow parsing.
          case End.Kind.HALT =>
            shouldNotReachHere("block with halt should be unreachable and should not be generated at all")
          case kind =>
            shouldNotReachHere(s"unexpected end kind: $kind")
        }
      }
      assert(generatedBlockEnd)
    }

    override def longHalfOf(n: Node) = n.`type` match {
      case NodeType.LONG | NodeType.DOUBLE => Node.LONG_HALF
      case _ => shouldNotReachHere()
    }

    override def writeSlot(slotIdx: Int, value: Node): Unit = {
      if (globalInfo.isSlotAliveAtHandler(slotIdx, block)) {
        // Every bytecode instruction may throw exception.
        // So every node has to be alive all the time
        // while it is stored in the slot which is alive at handler.
        // Last "use" of such node is the moment when new node is written in this slot.
        // @see BlockLivenessAnalyzer.ValueLivenessProcessor#writeSlot
        val oldValue = slotNodes(slotIdx)
        assert(oldValue != null)

        if (nodes.hasLoc(oldValue)) {
          nodes.releaseLocIfNotUsedLater(oldValue)
        } else {
          // The only case when such node's location is already released
          // is a generation of instruction which reads and writes the same location (e.g. iinc)
          // (it may release read node before writing it).
          // The good news is that current implementation of such instructions do not throw exceptions.

          // Check that oldValue is really last used in current instruction.
          assert(oldValue.lastUse.asInstanceOf[Node.BCPosition].bcOffset == curBC)
        }
      }
      slotNodes(slotIdx) = value
    }

    override def readSlot(slotIdx: Int) = {
      val value = slotNodes(slotIdx)
      assert(value != null, s"trying to access an uninitialized slot ${blockGen.slots.slotToString(slotIdx)}")
      value
    }

    private def getInstructionResult = instrResultNodes(curBC)

    override def startInstruction(offset: Int, nextOffset: Int): Unit = {
      curBC = offset
      nodes.setInstructionBC(offset)
      gen.setCurrentLineNumber(rootMethod.codeAttribute.findLineNumber(offset))
      gen.setCurrentBytecodeOffset(offset)
    }

    override def pushCPEntry(index: Int): Unit = {
      val result = getInstructionResult
      val tag = cp.getTag(index)
      val typeKind = tag match {
        case Tag.INTEGER =>
          gen.genIntConst(result, cp.getInt(index))
          BytecodeTypeKind.INT
        case Tag.LONG =>
          gen.genLongConst(result, cp.getLong(index))
          BytecodeTypeKind.LONG
        case Tag.CLASS =>
          val typeAccess = cp.getClassType(index)
          typeAccess.getResult match {
            case OK =>
              gen.genClassObject(result, typeAccess.getObject)
            case DEFERRED =>
              deferred.genResolvableConstant(index, tag, result)
            case ERROR =>
              genThrowAtErrorAccess(typeAccess.getError)
              nodes.bindToAnyFreeLoc(result)
          }
          BytecodeTypeKind.CLASS
        case Tag.STRING =>
          gen.genConstString(result, cp.getConstString(index))
          BytecodeTypeKind.CLASS
        case Tag.FLOAT =>
          gen.genFloatConst(result, cp.getFloat(index))
          BytecodeTypeKind.FLOAT
        case Tag.DOUBLE =>
          gen.genDoubleConst(result, cp.getDouble(index))
          BytecodeTypeKind.DOUBLE
        case Tag.METHOD_TYPE | Tag.METHOD_HANDLE =>
          deferred.genResolvableConstant(index, tag, result)
          BytecodeTypeKind.CLASS
        case _ =>
          shouldNotReachHere()
      }

      push(typeKind, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    override def pushConst(tk: BytecodeTypeKind, value: Int): Unit = {
      import BytecodeTypeKind.*
      val n = getInstructionResult
      (tk: @unchecked) match {
        case FLOAT =>
          gen.genFloatConst(n, value.toFloat)
        case DOUBLE =>
          gen.genDoubleConst(n, value.toDouble)
        case LONG =>
          gen.genLongConst(n, value)
        case _ if tk.isIntegral =>
          gen.genIntConst(n, value)
        case _ if tk.isReference =>
          assert(value == 0)
          emit.movNull(nodes.bindToAnyFreeIReg(n))
      }

      push(tk, n)
      nodes.releaseLocIfNotUsedLater(n)
    }

    override def arithOp(tk: BytecodeTypeKind, op: ArithOp): Unit = {
      import BytecodeTypeKind.*
      val result = getInstructionResult

      if (op == ArithOp.NEG) {
        val arg = pop(tk)
        gen.genNeg(fromBytecode(tk), arg, result)
      } else {
        val arg2 = pop(if (op.isShift) INT else tk)
        val arg1 = pop(tk)
        gen.genBinaryArithOp(op, fromBytecode(tk), arg1, arg2, result)
      }

      push(if (op.isCmp) INT else tk, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    override def convert(op: ConvertOp): Unit = {
      val arg = pop(op.srcKind)
      val result = getInstructionResult

      gen.genConvert(op, arg, result)

      push(op.dstKind, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    override def increment(local: Int, delta: Int): Unit = {
      val tk = BytecodeTypeKind.INT
      val value = read(tk, local)
      val result = getInstructionResult

      val argLoc = nodes.loadToIRegAndReleaseIfNotUsedLater(value)
      val resultLoc = nodes.bindToAnyFreeIRegWithPreferred(result, argLoc)
      emit.add32(resultLoc, argLoc, delta)

      write(tk, local, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    override def arrayGet(tk: BytecodeTypeKind): Unit = {
      val index = pop(BytecodeTypeKind.INT)
      val array = pop(BytecodeTypeKind.ARRAY)

      prepareBeforeObjectDereference(array)
      gen.genCheckIndex(array, index)

      val result = getInstructionResult
      gen.readArrayElem(tk, array, index, result)

      push(tk, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    override def arrayPut(tk: BytecodeTypeKind): Unit = {
      val value = pop(tk)
      val index = pop(BytecodeTypeKind.INT)
      val array = pop(BytecodeTypeKind.ARRAY)

      if (env.enabled(GenerateWriteBarriers) && tk.isTraceableReference) {
        generateInstanceWriteBarrier(array, value)
      }

      prepareBeforeObjectDereference(array)
      gen.genCheckIndex(array, index)

      if (tk.isReference) {
        gen.rtsCall(RTSProc.JR_CheckArrayStore)(array, value)
      }

      gen.writeArrayElem(tk, array, index, value)
    }

    private def generateInstanceWriteBarrier(receiver: Node, value: Node): Unit = {
      gen.rtsCall(RTSProc.WriteBarriers_writeBarrier_instance_baseline)(receiver, value)
    }

    private def generateStaticWriteBarrier(value: Node): Unit = {
      gen.rtsCall(RTSProc.WriteBarriers_writeBarrier_static_baseline)(value)
    }

    override def fieldOp(index: Int, akind: FieldAccessKind): Unit = {
      val fieldAccess = cp.getField(index, akind)
      if (fieldAccess.isError) {
        // cp.getFieldTypeKind would erase THIN to CLASS
        // but this is not an issue here because Thin field access may never be erroneous
        fieldOpWithErrorAccess(fieldAccess.getError, akind, cp.getFieldTypeKind(index))
      } else {
        val refClass = cp.getFieldRefClass(index)
        val field = fieldAccess.getObject
        val fieldKind = field.getType.jbcKind

        val value = if (akind.isWrite) pop(fieldKind) else null
        val obj = if (akind.isInstance) pop(BytecodeTypeKind.CLASS) else null
        val result = if (akind.isRead) getInstructionResult else null

        fieldOpImpl(index, field, refClass, obj, value, result)

        if (akind.isRead) {
          push(fieldKind, result)
          nodes.releaseLocIfNotUsedLater(result)
        }
      }
    }

    private def fieldOpImpl(index: Int, field: Field, refClass: Type, obj: Node, value: Node, result: Node): Unit = {
      assert((value == null) == (result != null))
      assert(field.isStatic == (obj == null))
      val isRead = value == null
      val `type` = field.getType
      val isInstance = !field.isStatic
      val isAbsent = field.getDeclaringClass.isDeferred

      val isVolatile = if (isAbsent) {
        assert(index != DAIGenerator.NO_CP_INDEX)
        false // deferred thunks contain necessary barriers
      } else field.isVolatile

      if (isRead) {
        if (isInstance) {
          assert(obj != null) // helps static analyzers
          if (isAbsent) {
            deferred.genInstanceFieldRead(index, `type`, obj, result)
          } else {
            instanceFieldRead(field, refClass, obj, result)
          }
        } else {
          if (isAbsent) {
            deferred.genStaticFieldRead(index, `type`, result)
          } else {
            staticFieldRead(field, refClass, result)
          }
        }

        if (isVolatile) emit.memBarrier(LOAD_LOAD, LOAD_STORE)

      } else {
        val needWriteBarrier = env.enabled(GenerateWriteBarriers) && `type`.isTraceableReference

        if (isVolatile) emit.memBarrier(LOAD_STORE, STORE_STORE)

        if (isInstance) {
          if (needWriteBarrier) generateInstanceWriteBarrier(obj, value)
          if (isAbsent) {
            deferred.genInstanceFieldWrite(index, `type`, obj, value)
          } else {
            instanceFieldWrite(field, refClass, obj, value)
          }
        } else {
          if (needWriteBarrier) generateStaticWriteBarrier(value)
          if (isAbsent) {
            deferred.genStaticFieldWrite(index, `type`, value)
          } else {
            staticFieldWrite(field, refClass, value)
          }
        }

        if (isVolatile) emit.memBarrier(STORE_LOAD, STORE_STORE)
      }
    }

    /** TOP GC algorithm uses TDBarriers (unconditional dereferences of obj.TD field) to implement read barriers
      * and intercept all field accesses of displaced (a.k.a. concurrently evacuated, moved, proxy) objects.
      *
      * It is important to insert TD barriers before any field access to ensure mutator will not read outdated
      * information. Objects tend to "disappear" at random places (any write barrier, any safe-point) so code
      * generator need to ensure that TDBarrier and actual dereference happen "atomically" from the GC standpoint
      * (no intermittent safe-point/method invocation allowed).
      *
      * TODO: implement JET-14166 and use TDBarrier instructions for implicit null check.
      */
    private def ensureNotEvacuatedObjectOnReg(value: Node, mayBeNull: Boolean): Unit = {
      if (env.enabled(GenTDBarriers)) {
        gen.genTDBarrier(value, mayBeNull)
      }
    }

    private def prepareBeforeObjectDereference(obj: Node): Unit = {
      gen.genCheckNull(obj)
      ensureNotEvacuatedObjectOnReg(obj, mayBeNull = false)
    }

    /** Generate throw at error access and simulate field operations for ir correctness. */
    private def fieldOpWithErrorAccess(error: ErrorAccessInfo, akind: FieldAccessKind, fieldKind: BytecodeTypeKind): Unit = {
      var unusedNodes = ArrayBuffer.empty[Node]
      if (akind.isWrite) unusedNodes += pop(fieldKind)
      if (akind.isInstance) unusedNodes += pop(BytecodeTypeKind.CLASS)
      nodes.releaseLocIfNotUsedLater(unusedNodes)

      genThrowAtErrorAccess(error)

      if (akind.isRead) {
        val result = getInstructionResult
        nodes.bindToAnyFreeLoc(result)
        push(fieldKind, result)
        nodes.releaseLocIfNotUsedLater(result)
      }
    }

    /** Generate throw of exception. */
    private def genThrowAtErrorAccess(error: ErrorAccessInfo): Unit = {
      val msgSymbol = symbolLinker.makeConstStringData(error.getErrorMessage, bstr = true)
      gen.rtsCall(error.getThrowProc)(msgSymbol)
    }

    private def instanceFieldRead(field: Field, refClass: Type, obj: Node, result: Node): Unit = {
      assert(!refClass.isThinClass)
      assert(!field.isStatic)
      val `type` = field.getType
      val fieldOffset = field.getInstanceFieldOffset
      val isVolatile = field.isVolatile
      assert(!field.getDeclaringClass.isDeferred)

      prepareBeforeObjectDereference(obj)
      gen.readInstanceField(`type`, isVolatile, obj, fieldOffset, result)
    }

    private def instanceFieldWrite(field: Field, refClass: Type, obj: Node, value: Node): Unit = {
      assert(!refClass.isThinClass)
      assert(!field.isStatic)
      assert(!field.getDeclaringClass.isDeferred)
      assert(!field.isAJFlat)

      val `type` = field.getType
      val fieldOffset = field.getInstanceFieldOffset
      val isVolatile = field.isVolatile

      prepareBeforeObjectDereference(obj)
      if (`type`.isTraceableReference) {
        ensureNotEvacuatedObjectOnReg(value, mayBeNull = true)
      }
      gen.writeInstanceField(`type`, isVolatile, obj, fieldOffset, value)

      if (!needMemBarBeforeRootReturn &&
          field.isFinal && rootMethod.isConstructor &&
          field.getDeclaringClass == rootDeclaringClass) {
        needMemBarBeforeRootReturn = true
      }
    }

    private def staticFieldRead(field: Field, refClass: Type, result: Node): Unit = {
      assert(field.isStatic)
      assert(!field.isAJFlat)

      val host = field.getDeclaringClass
      assert(!host.isDeferred)
      gen.genClinit(host)

      val fieldSym = field.getStaticFieldSymbol
      gen.ensurePrepared(PreparationRequired.forGetStatic(field))
      gen.readStaticField(field.getType, field.isVolatile, fieldSym, result)
    }

    private def staticFieldWrite(field: Field, refClass: Type, value: Node): Unit = {
      assert(field.isStatic)
      assert(!field.isAJFlat)

      val host = field.getDeclaringClass
      assert(!host.isDeferred)
      gen.genClinit(host)

      gen.ensurePrepared(PreparationRequired.forPutStatic(field), rootMethod)
      if (field.getType.isTraceableReference) {
        ensureNotEvacuatedObjectOnReg(value, mayBeNull = true)
      }
      gen.writeStaticField(field.getType, field.isVolatile, field.getStaticFieldSymbol, value)
    }

    override def invoke(index: Int, akind: MethodAccessKind): Unit = {
      val methodTypeErased = MethodType.jbcErased(cp.getRefSignature(index), this.typeProvider, akind.hasObjectArg)

      val args = popMethodArgs(methodTypeErased)
      val returnKind = methodTypeErased.returnType.jbcKindErased

      val result = if (!returnKind.isVoid) getInstructionResult else null

      if (akind == MethodAccessKind.DYNAMIC) {
        val lambdaConstructor = getLambdaConstructor(index)
        assert(lambdaConstructor == null) // no LambdaTypeGenerator in JIT
        deferred.genInvokeDynamicOrSigPoly(index, DAIRefKind.INVOKE_DYNAMIC, methodTypeErased, args, result)

      } else {
        val sigPolyMethodID = cp.getSignaturePolymorphicMethodID(index)
        if (sigPolyMethodID != SigPolyMethodID.NONE) {
          if (sigPolyMethodID.isStatic) {
            assert(akind == MethodAccessKind.STATIC)
          } else {
            assert(akind == MethodAccessKind.VIRTUAL)
          }

          if (sigPolyMethodID.isMethodHandleInvoker) {
            assert(!sigPolyMethodID.isStatic)
            deferred.genInvokeDynamicOrSigPoly(index, DAIRefKind.INVOKE_SIGPOLY, methodTypeErased, args, result)
          } else {
            invokeSigPolyIntrinsic(sigPolyMethodID, methodTypeErased, args, result)
          }

        } else {
          val methodAccess = cp.getMethodReference(index, akind)
          methodAccess.getResult match {
            case ERROR =>
              invokeWithErrorAccess(methodAccess.getError, args, result)

            case DEFERRED =>
              // cannot perform receiver null-check before target resolve
              deferred.genInvoke(index, akind.asMethodRefAccessKind, methodAccess.getObject, args, result, !akind.hasObjectArg)

            case OK =>
              genInvokeMethod(methodAccess.getObject, args, result)
          }
        }
      }

      if (!returnKind.isVoid) {
        if (nodes.hasLoc(result)) {
          nodes.releaseLocIfNotUsedLater(result)
        }
        push(returnKind, result)
      }
    }

    private def popMethodArgs(methodType: MethodType): ArrayBuffer[Node] = {
      val args = ArrayBuffer.fill[Node](methodType.parameterCount)(null)
      for (i <- methodType.parameterCount - 1 to 0 by -1) {
        args(i) = pop(methodType.parameterType(i).jbcKindErased)
      }
      args
    }

    /** Try to generate lambda class for this invokedynamic constant pool entry.
      *
      * @return the constructor of the generated lambda class or `null`
      *         if the class cannot be generated (not lambda or JIT compilation).
      */
    private def getLambdaConstructor(index: Int) = {
      assert(cp.getTag(index) == Tag.INVOKE_DYNAMIC)
      LambdaTypeGenerator(_.getLambdaConstructor(rootDeclaringClass, index)).orNull
    }

    private def genInvokeMethod(methodRef: MethodReference, args: ArrayBuffer[Node], result: Node): Unit = {
      val target = methodRef.method
      assert(!target.isVarArgs)
      assert(target.getAJCallKind == MethodAJCallKind.NORMAL, "baseline cannot compile AJ")
      gen.genInvokeNormal(methodRef, args, releaseBCParams = true, result)
    }

    /** Generate throw at error access and simulate invoke for ir correctness. */
    private def invokeWithErrorAccess(error: ErrorAccessInfo, args: collection.Seq[Node], result: Node): Unit = {
      nodes.releaseLocIfNotUsedLater(args)
      genThrowAtErrorAccess(error)
      if (result != null) {
        nodes.bindToAnyFreeLoc(result)
      }
    }

    private def invokeSigPolyIntrinsic(id: SigPolyMethodID, methodType: MethodType, args: ArrayBuffer[Node], result: Node): Unit = {
      import BuiltInField.*
      import env.getBuiltInFieldOffset as btfOffset
      id match {
        case INVOKE_BASIC =>
          // calculate entry (methodHandle.form.vmentry.entryPoint) and call it
          val methodHandle = args(0)
          // We cannot easily access real classes of form & vmentry (especially in JIT).
          // So we use some fake class (java.lang.Object) for correct absense of deprive.
          val someClassType = SignatureType.javaLangObject

          val form = Node.newTemporary(TREF)
          val formType = someClassType // java.lang.invoke.LambdaForm

          // methodHandle should not be released, it still is the first argument of call
          gen.readInstanceField(formType, false, methodHandle,
            btfOffset(METHOD_HANDLE_FORM), form, releaseObject = false)

          val vmentry = Node.newTemporary(TREF)
          val vmentryType = someClassType // java.lang.invoke.MemberName
          gen.readInstanceField(vmentryType, false, form,
            btfOffset(LAMBDA_FORM_VMENTRY), vmentry)

          // Current implementation of genRtsCall releases all temporary nodes.
          // But we want `vmentry` to live longer, so we need to create a copy here.
          val vmentryCopy = Node.newTemporary(vmentry.`type`)
          emit.copyAny(nodes.bindToAnyFreeIReg(vmentryCopy), nodes.getLoc(vmentry), vmentry.asmType)
          gen.rtsCall(RTSProc.JR_MemberNamePreparationCheck)(vmentryCopy) // must prepare target host, see JET-11809

          val entry = Node.newTemporary(NodeType.ADDR)
          val entryType = SignatureType.Address // com.huawei.excelsior.aj.lang.CodeAddr
          gen.readInstanceField(entryType, false, vmentry,
            btfOffset(MEMBER_NAME_ENTRY_POINT), entry, releaseObject = false)

          gen.genInvokeSigPolyIntrinsic(methodType, entry, vmentry, args, result)

        case LINK_TO_VIRTUAL |
             LINK_TO_STATIC |
             LINK_TO_SPECIAL |
             LINK_TO_INTERFACE =>
          assert(Env.isJIT)

          // MemberName is the last parameter
          assert(methodType.parameterType(methodType.parameterCount - 1).jbcKind == BytecodeTypeKind.CLASS)
          val methodTypeWithoutMemberName = methodType.dropLastParameter
          val memberName = args.remove(args.length - 1)

          if (id == SigPolyMethodID.LINK_TO_STATIC) {
            assert(!memberName.isTemporary)
            gen.rtsCall(RTSProc.JR_MemberNamePreparationCheck)(memberName) // must prepare target host, see JET-11809
          }

          val entry = Node.newTemporary(NodeType.ADDR)
          val entryType = SignatureType.Address

          if (id == SigPolyMethodID.LINK_TO_SPECIAL) {
            // INVOKE_SPECIAL could have null-check in generated call thunk,
            // but we don't want to waste memory generating that thunk, so we need to perform NC here
            gen.genCheckNull(args.head)

            assert(!memberName.isTemporary)
            // can't use address from mn.entryPoint, see JET-10820
            gen.rtsCall(RTSProc.JR_getTargetForLinkToSpecial, entry)(memberName)

          } else {
            // INVOKE_VIRTUAL & INVOKE_INTERFACE have implicit null-check in thunk
            gen.readInstanceField(entryType, false, memberName,
              btfOffset(MEMBER_NAME_ENTRY_POINT), entry, releaseObject = false)
          }

          gen.genInvokeSigPolyIntrinsic(methodTypeWithoutMemberName, entry, memberName, args, result)

        case _ =>
          shouldNotReachHere(id)
      }
    }

    private def genMonitorAction(structuredLockingAction: RTSProc, potentiallyUnstructuredLockingAction: RTSProc, monitor: Node): Unit = {
      gen.genCheckNull(monitor)
      if (globalInfo.structuredLocking) {
        gen.rtsCall(structuredLockingAction, releaseBCParams = true)(monitor)
      } else {
        gen.rtsCall(potentiallyUnstructuredLockingAction, releaseBCParams = true)(monitor)
      }
    }

    override def monitorEnter(): Unit = {
      genMonitorAction(RTSProc.JR_MonitorEnter, RTSProc.JR_CheckedMonitorEnter, pop(BytecodeTypeKind.CLASS))
    }

    override def monitorExit(): Unit = {
      genMonitorAction(RTSProc.JR_MonitorExit, RTSProc.JR_CheckedMonitorExit, pop(BytecodeTypeKind.CLASS))
    }

    override def doNew(index: Int): Unit = {
      val result = getInstructionResult

      val typeAccess = cp.getClassType(index)
      typeAccess.getResult match {
        case OK =>
          gen.genNew(typeAccess.getObject, result)
        case DEFERRED =>
          deferred.genNew(index, result)
        case ERROR =>
          genThrowAtErrorAccess(typeAccess.getError)
          nodes.bindToAnyFreeLoc(result)
      }

      push(BytecodeTypeKind.CLASS, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    override def instanceOf(index: Int): Unit = {
      // Determine if object is of given type and push result (1 or 0)
      val obj = pop(BytecodeTypeKind.CLASS)
      val result = getInstructionResult

      val typeAccess = cp.getType(index)
      typeAccess.getResult match {
        case OK =>
          gen.genInstanceOf(typeAccess.getObject, obj, result)
        case DEFERRED =>
          deferred.genInstanceOf(index, obj, result)
        case ERROR =>
          nodes.releaseLocIfNotUsedLater(obj)
          genThrowAtErrorAccess(typeAccess.getError)
          nodes.bindToAnyFreeLoc(result)
      }

      push(BytecodeTypeKind.INT, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    override def checkCast(index: Int): Unit = {
      // Check if object is of given type and push it back
      val obj = pop(BytecodeTypeKind.CLASS)

      val typeAccess = cp.getType(index)
      typeAccess.getResult match {
        case OK =>
          gen.genCheckCast(typeAccess.getObject, obj)
        case DEFERRED =>
          deferred.genCheckCast(index, obj)
        case ERROR =>
          nodes.releaseLocIfNotUsedLater(obj)
          genThrowAtErrorAccess(typeAccess.getError)
      }

      push(BytecodeTypeKind.CLASS, obj)
      // object is already released above if needed
    }

    override def doThrow(): Unit = {
      markGeneratedBlockEnd()
      assert(block.end.kind == Block.End.Kind.THROW)

      val obj = pop(BytecodeTypeKind.CLASS)
      clearStack()
      gen.genCheckNull(obj, rootDeclaringClass)
      gen.rtsCall(RTSProc.JR_Throw, releaseBCParams = true)(obj)

      destroySSAFormAtBlockEndWithoutFixed()
    }

    override def newPrimitiveArray(tk: BytecodeTypeKind): Unit = {
      val length = pop(BytecodeTypeKind.INT)
      val result = getInstructionResult

      val baseType = this.typeProvider.getPrimitiveType(fromBytecode(tk))
      val arrayType = this.typeProvider.getArrayType(baseType, 1)
      gen.genNewArray(arrayType, length, result)

      push(BytecodeTypeKind.CLASS, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    override def newObjectArray(index: Int): Unit = {
      val result = getInstructionResult

      val typeAccess = cp.getType(index)
      typeAccess.getResult match {
        case OK | DEFERRED =>
          val elemType = typeAccess.getObject
          val baseType = if (elemType.isJBCArray) elemType.getArrayBase else elemType
          val dimNum = if (elemType.isJBCArray) elemType.getArrayDimnum + 1 else 1
          val arrayType = this.typeProvider.getArrayType(baseType, dimNum)
          if (baseType.isDeferred) {
            deferred.genNewArray(index, arrayType.getArrayDimnum, popArrayDimensions(1), result)
          } else {
            genNewArray(arrayType, 1, result)
          }

        case ERROR =>
          nodes.releaseLocIfNotUsedLater(pop(BytecodeTypeKind.INT)) // for IR correctness
          genThrowAtErrorAccess(typeAccess.getError)
          nodes.bindToAnyFreeLoc(result)
      }

      push(BytecodeTypeKind.CLASS, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    override def newMultiObjectArray(index: Int, dimNum: Int): Unit = {
      val result = getInstructionResult

      val typeAccess = cp.getType(index)
      typeAccess.getResult match {
        case OK | DEFERRED =>
          val `type` = typeAccess.getObject
          if (`type`.isDeferred) {
            deferred.genNewArray(index, `type`.getArrayDimnum, popArrayDimensions(dimNum), result)
          } else {
            genNewArray(`type`, dimNum, result)
          }

        case ERROR =>
          for (i <- 0 until dimNum) {
            nodes.releaseLocIfNotUsedLater(pop(BytecodeTypeKind.INT)) // for IR correctness
          }
          genThrowAtErrorAccess(typeAccess.getError)
          nodes.bindToAnyFreeLoc(result)
      }

      push(BytecodeTypeKind.CLASS, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    private def genNewArray(arrayType: Type, dimNum: Int, result: Node): Unit = {
      val baseType = arrayType.getArrayBase
      assert(!baseType.isDeferred)
      if (dimNum == 1) {
        gen.genNewArray(arrayType, pop(BytecodeTypeKind.INT), result)
      } else {
        assert(arrayType.getArrayDimnum >= dimNum)
        gen.genNewMultidimArray(arrayType, popArrayDimensions(dimNum), result)
      }
    }

    /** Pops sequence of array dimensions from stack. */
    private def popArrayDimensions(count: Int) = {
      Array.tabulate[Node](count) { _ => pop(BytecodeTypeKind.INT) }
    }

    override def arrayLength(): Unit = {
      val array = pop(BytecodeTypeKind.ARRAY)
      val result = getInstructionResult

      prepareBeforeObjectDereference(array)
      gen.readArrayLength(array, result)

      push(BytecodeTypeKind.INT, result)
      nodes.releaseLocIfNotUsedLater(result)
    }

    private def genGCPointAtBackwardJump(): Unit = {
      if (rootMethod.shouldContainGCPoints) {
        val hasBackwardJump = block.end.outputs exists previousBlocks
        if (hasBackwardJump) {
          gen.genGCPoint()
        }
      }
    }

    private def commonIf(op: CompareOp, tkind: BytecodeTypeKind, xLoc: IReg, yLoc: IReg): Unit = {
      markGeneratedBlockEnd()
      assert(block.end.kind == Block.End.Kind.IF)
      assert(block.end.outputs.size == 2)

      val falseTarget = block.end.outputs(0)
      val trueTarget = block.end.outputs(1)

      val trueTargetLabel = labelForBlock(trueTarget)
      if (yLoc == null) {
        emit.branchIf(op.toBranchOp, xLoc, 0, tkind.width, trueTargetLabel)
      } else {
        emit.branchIf(op.toBranchOp, xLoc, yLoc, tkind.width, trueTargetLabel)
      }

      if (falseTarget != nextBlock) {
        emit.jump(labelForBlock(falseTarget))
      } // else fallthrough
    }

    override def unaryIf(tkind: BytecodeTypeKind, op: CompareOp, bc: Int): Unit = {
      genGCPointAtBackwardJump()

      val x = pop(tkind)
      val xLoc = nodes.loadToIRegAndReleaseIfNotUsedLater(x)

      destroySSAFormAtBlockEndWithoutFixed(xLoc)

      commonIf(op, tkind, xLoc, null)
    }

    override def binaryIf(tkind: BytecodeTypeKind, op: CompareOp, bc: Int): Unit = {
      genGCPointAtBackwardJump()

      val y = pop(tkind)
      val x = pop(tkind)

      if (y.`type` == TREF) {
        assert(x.`type` == TREF)
        ensureNotEvacuatedObjectOnReg(y, mayBeNull = true)
        ensureNotEvacuatedObjectOnReg(x, mayBeNull = true)
      }

      val yLoc = nodes.loadToIReg(y)
      val xLoc = nodes.loadToIReg(x)
      nodes.releaseLocIfNotUsedLater(x, y)

      destroySSAFormAtBlockEndWithoutFixed(xLoc, yLoc)

      commonIf(op, tkind, xLoc, yLoc)
    }

    override def jump(bc: Int): Unit = jump()

    private def jumpCommon(): Unit = {
      genGCPointAtBackwardJump()
      markGeneratedBlockEnd()

      assert(block.end.outputs.size == 1)
      val target = block.end.outputs.head

      destroySSAFormAtBlockEndWithFixed()

      if (target != nextBlock) {
        emit.jump(labelForBlock(target))
      } // else fallthrough
    }

    private def jump(): Unit = {
      assert(block.end.kind == Block.End.Kind.GOTO)
      jumpCommon()
    }

    override def jsr(bc: Int): Unit = {
      val retAddr = getInstructionResult
      nodes.bindToInvalidLoc(retAddr)
      nodes.releaseLocIfNotUsedLater(retAddr)
      push(RET_ADDR_KIND, retAddr)
      jump()
    }

    override def ret(`var`: Int): Unit = {
      // we ignore saved ret addr, because subroutines are inlined
      assert(block.end.kind == Block.End.Kind.GOTO, "non-inlined RET from subroutine")
      jump()
    }

    override def doReturn(tkind: BytecodeTypeKind, isLastBytecode: Boolean): Unit = {
      markGeneratedBlockEnd()
      assert(block.end.kind == Block.End.Kind.RETURN)

      val returnValue = if (!tkind.isVoid) pop(tkind) else null
      clearStack()
      destroySSAFormAtReturn(returnValue)

      if (nextBlock != null) {
        emit.jump(labelForEpilogue)
      } // else fallthrough to epilogue
    }

    private def genSwitch(matches: Array[Int]): Unit = {
      genGCPointAtBackwardJump()
      markGeneratedBlockEnd()

      assert(block.end.kind == Block.End.Kind.SWITCH)
      assert(block.end.outputs.size == 1 + matches.length)

      val key = pop(BytecodeTypeKind.INT)
      val keyReg = nodes.loadToIRegAndReleaseIfNotUsedLater(key)

      assert(block.end.outputs.nonEmpty)
      if (block.end.outputs.size == 1) {
        // Rare case - switch with one default branch. We can not destroySSAForm normal way,
        // because if this single branch is backward branch it can have fixed output locations.
        destroySSAFormAtBlockEndWithFixed()

      } else {
        destroySSAFormAtBlockEndWithoutFixed(keyReg)

        // block.end.outputs = [default, case_0, case_1, ..., case_{matches.length-1}]
        for (i <- matches.indices) {
          val caseValue = matches(i)
          val caseBlock = block.end.outputs(1 + i)
          emit.branchIf(BranchOp.EQ, keyReg, caseValue, W32, labelForBlock(caseBlock))
        }
      }

      val defaultBlock = block.end.outputs(0)
      if (defaultBlock != nextBlock) {
        emit.jump(labelForBlock(defaultBlock))
      } // else fallthrough
    }

    override def tableSwitch(bcDefault: Int, lowMatch: Int, highMatch: Int, bcTargets: Array[Int]): Unit = {
      val matches = Array.tabulate[Int](bcTargets.length)(i => lowMatch + i)
      genSwitch(matches)
    }

    override def lookupSwitch(bcDefault: Int, matches: Array[Int], bcTargets: Array[Int]): Unit = {
      genSwitch(matches)
    }
  }
}
