/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Tag, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.inline.InlineEngine
import com.huawei.excelsior.jet.compiler.opt.middle.{Optimize, UCEComponent}
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.util.WhileChanged.whileChanged
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.compiler.{RTSProc, Stage}

import scala.collection.mutable

/**
 * Lowering of IR operations. All high-level and complex operations are replaced by
 * low-level simple operations.
 *
 * @author alexm
 */
trait Lowering extends PreLowering with TypeChecks with Allocators with Invokes with Switches with DeferredOps with MiscOps with Toolbox
                  with UCEComponent with InlineEngine with Optimize { self: Universe =>

  /** Max number of elements in ArrayFill for splitting it into a series of ArrayPut operations. */
  protected def MaxArrayFillSizeForSplitting: Int

  private val alwaysLowerDeprive = env.enabled(AlwaysLowerDeprive)

  enum LoweringKind {
    case NONE     // node should not be lowered
    case FLOATING // lowering of the node without changing control and memory states
    case SPINAL   // lowering of the node with changing of full state, but in the same block
    case COMPLEX  // lowering with block splitting
  }

  import LoweringKind.*

  /////////////////////////////////////////////////////////////////////////////////////

  val nodesToLower = Sets[Node].newMSet

  val nodesCloned = Sets[Node].newMSet

  private[lowering] def safeLoweredNodeClone(node: Node): Node = {
    assert(shouldBeLowered(node) != NONE, s"$node ${shouldBeLowered(node)}")
    assert(!nodesCloned.contains(node), s"nodesCloned contains $node")
    assert(!nodesToLower.contains(node), s"nodesToLower contains $node")

    val clone = if (node.isInstanceOf[FloatingNode]) Node.cloneExact(node) else Node.clone(node)
    assert(clone != node, s"$node is same as it's clone") // probably you don't want to clone and get itself, since this will break lowering
    assert(nodesToLower.contains(clone)) // new node should be dispatched for lowering as the original one
    nodesToLower.remove(clone)

    assert(!nodesCloned.contains(clone), s"$clone is already in nodesCloned")
    nodesCloned.add(clone)

    clone
  }

  /**
   * Lowering entry point.
   * Decomposes all complex nodes into low-level operations.
   */
  def doLowering(): Unit = stage(Stage.Lowering) { inlineLogSession("lowering") {

    def lowerAll(): Unit = {
      ContextTypesMap.dropCache()

      dbgPrinter.debugNodes("All graph before lowering")
      dbgPrinter.debugGraphs("All graph before lowering")

      def dispatchForLowering(n: Node): Unit =
        if (shouldBeLowered(n) != NONE) nodesToLower.add(n)

      def decommitCallback(n: Node): Unit =
        nodesToLower.remove(n)

      allNodes foreach dispatchForLowering

      assert(toBeInlinedCalls.isEmpty)

      var iterCount = 1
      whileChanged { changed =>
        // Before each lowering iteration we clean up cache of ErrorRTSCall blocks, to prevent bugs of CFG sharing
        // mixed with optimizations. For more details look at JET-12398.
        errorRTSCallBlocks.clear()

        val allCompositeLowered = nodesToLower.isEmpty
        nodesToLower.foreach(_.markExact())
        nodesToLower.clear()

        onCommit.withCallback(dispatchForLowering) {
          onDecommit.withCallback(decommitCallback) {

            if (!allCompositeLowered) {
              // prevent moving heavy ops (e.g. weakcasts) too high in CFG by splitting critical edges before GCM
              splitCriticalEdges()
              dbgPrinter.debugNodes("All graph after critical edges split before lowering")

              val ts = cfg.topSort

              def makeNodeOrders(): Maps[Block]#QMap[collection.Seq[Node]] = {
                val engine = new GCMEngine(optimizeMemoryAntiDependency = true)
                val nodeOrders = withGCM(engine) {
                  eliminateCrossBlockMemoryEdges(ts)
                  dbgPrinter.debugNodes("All graph after memory adjusted in lowering")

                  val nodeOrders = Maps[Block].newQMap[collection.Seq[Node]]
                  for (block <- ts.order) {
                    nodeOrders(block) = LinearNodeOrder.strictBlockOrder(block, strictlyByPoints = true)
                  }
                  nodeOrders
                }

                if (engine.irWasChanged && simplifyIR()) {
                  dbgPrinter.debugNodes("All graph after simplifyIR in lowering")
                  makeNodeOrders()
                } else {
                  nodeOrders
                }
              }

              // Pre-calculate node orderings as blocks are split during lowering.
              val nodeOrders = makeNodeOrders()

              for (block <- ts.order if block.isCommitted) {
                lowerBlockNodes(nodeOrders(block))
              }
            }

            // If we inline from bytecode, then we need to ensure that all new calls are inlined
            def collectInlineEverywhereCalls(n: Node) = n match {
              case call @ DirectCall(method) if method.isInlineAllAndRemove => toBeInlinedCalls += call
              case _ =>
            }
            onCommit.withCallback(collectInlineEverywhereCalls) {
              // It's time to process DirectCall inline queue.
              // N.B.: the call could have become unreachable after we have added it to the queue, skip those.
              for (call <- toBeInlinedCalls.drain if call.isCommitted) {
                inlineCall(call)
              }
            }

            linkStructuredSynchronization()

            dbgPrinter.debugNodes(s"All graph after lowering ($iterCount)")
            checkDAGsConsistency(CheckLevels.Desirable)
            checkIRConsistency(CheckLevels.Important)

            iterCount += 1

            optimize()
          }
        }

        if (nodesToLower.nonEmpty) changed()
      }
    }

    def inlineCall(call: Call): Unit = stage(Stage.LoweringInline) {
      val DirectCall(method) = call
      val success = doInline(new CallSite(method, direct = true, call), allowFromBytecode = false)
      assert(success, s"$call must be inlined")
      inlineLog("inline", method.getFullName)
    }

    /** Lowers all nodes in the given block. */
    def lowerBlockNodes(nodes: collection.Seq[Node]): Unit = {
      var ctrl: ControlNode = null
      var memory: MemoryNode = null
      var memoryUsesBelow: mutable.Set[Node] = null

      for (node <- nodes if node.isCommitted) {
        node match {
          case node: MemoryNode =>
            memoryUsesBelow = Sets[Node].newMSet(node.memoryUses)
          case _ if memoryUsesBelow != null =>
            memoryUsesBelow -= node
          case _ =>
        }

        shouldBeLowered(node) match {
          case NONE =>
            node match {
              case x: ControlNode => ctrl = x
              case _ =>
            }
            node match {
              case x: MemoryNode => memory = x
              case _ =>
            }

          case kind =>
            assert (ctrl != null)

            val nodeName = node.name // it will be non-valid after decommit
            val TaggedState(outCtrl, outMemory, _) = lowerNode(kind, node, TaggedState(ctrl, memory), memoryUsesBelow)
            if (env.enabled(DetailedLoweringLogs)) {
              dbgPrinter.debugNodes("All graph after full lowering of " + nodeName)
            }

            if (outCtrl == null) {
              return
            }

            ctrl = outCtrl
            memory = outMemory
        }
      }
    }

    // MonitorExit is non-throwing node however it is lowered to throwing calls.
    // Exceptions from these calls (i.e. StackOverflowError or some other InternalError)
    // lead to non-released monitor and it's not correct to continue execution.
    // We assume that consecutive calls won't succeed either.
    // So the only option to preserve structured locking is to spin forever (or crash). :/
    lazy val handlerForMonitorExits = withPos(rootMethodPos) {
      val handler = XBlock()
      val xobj = Catch(handler)

      if (env.enabled(CrashOnMonitorExitException)) {
        val fatal = ErrorRTSCall(RTSProc.JR_FatalError_Exception)(handler, handler,
          AJString.bstr("monitorexit instruction thrown an exception"), xobj)
        Halt.afterRTSCall(RTSProc.JR_FatalError_Exception, "monitorexit instruction thrown an exception")(fatal, fatal)

      } else {
        val goToSpinner = Goto(handler, handler)

        val spinner = BBlock(goToSpinner)
        val continueSpinning = Goto(spinner, spinner)
        spinner.addArg(continueSpinning)
      }

      handler
    }

    /** Lowers one node. Returns resulting control and memory. */
    def lowerNode(loweringKind: LoweringKind, node: Node, state: TaggedState, memoryUsesBelow: mutable.Set[Node]): TaggedState = {
      val newState = loweringKind match {
        case FLOATING =>
          val st @ TaggedState(ctrl, memory, _) = lowerSimpleNode(node, state, null)
          assert(node.tagsMask == Tag.VALUE.asMask)
          assert(ctrl == state.ctrl)
          assert(memory == state.memory)
          st

        case SPINAL if !(node.isInstanceOf[SpinalNode] && node.asInstanceOf[SpinalNode].hasXHandler) =>
          lowerSimpleNode(node, state, memoryUsesBelow)

        case NONE =>
          shouldNotReachHere()

        case _ =>
          lowerComplexNode(node, state, memoryUsesBelow)
      }

      assert(!node.isCommitted)

      newState
    }

    def lowerSimpleNode(node: Node, state: TaggedState, memoryUsesBelow: mutable.Set[Node]): TaggedState = withPos(node) {
      val TaggedState(prevCtrl: UpperPoint, inMemory, _) = state
      val (outValue, outCtrl, outMemory) = currentScope.inState(prevCtrl, inMemory) {
        val value: Node = decomposeNode(node)
        (value, currentCtrl, currentMemory)
      }

      assert(outCtrl.block == prevCtrl.block)
      assert(!outCtrl.isInstanceOf[Halt])

      if (memoryUsesBelow != null && outMemory != inMemory) {
        inMemory.replaceUses { case e if e.isMemory && memoryUsesBelow(e.target) => outMemory }
      }

      replaceCompletelyInPartsAndRemoveXPoint(node) {
        case Tag.CONTROL => outCtrl
        case Tag.MEMORY => outMemory
        case Tag.VALUE => outValue
      }

      TaggedState(outCtrl, outMemory)
    }

    def lowerComplexNode(node: Node, state: TaggedState, memoryUsesBelow: mutable.Set[Node]): TaggedState = withPos(node) {
      val TaggedState(prevCtrl: UpperPoint, inMemory, _) = state

      // Special handling of BlockEnd nodes.
      node match {
        case switch: Switch =>
          currentScope.inState(prevCtrl, inMemory) { lowerSwitch(switch) }
          return TaggedState.Unreachable

        case gt: TauSwitch =>
          currentScope.inState(prevCtrl, inMemory) { lowerTauSwitch(gt) }
          return TaggedState.Unreachable

        case _ =>
      }

      // See handlerForMonitorExits.
      val monitorExitIsLowered = node.isInstanceOf[MonitorExit]

      val inCtrl =
        if (monitorExitIsLowered) {
          // Extra split is required because we don't support multiple handlers inside of a single block.
          Block.splitAfter(prevCtrl, keepControlled = true).target
        } else {
          prevCtrl
        }
      val splitBlockEnd = Block.splitAfter(inCtrl, keepControlled = true)
      val nextBlock = splitBlockEnd.target
      inCtrl.block.blockEnd = null

      // TODO: unify with Toolbox.insertCode

      val (allowXCtrls, attachToXHandler, origXCtrl) = node match {
        case xctrl: SpinalNode if xctrl.canThrow =>
          (true, xctrl.hasXHandler, xctrl)
        case _ if monitorExitIsLowered =>
          (true, true, null)
        case _ =>
          (false, false, null)
      }

      // Note that we do not need to collect new xctrls if there is no xhandler.
      val newXPoints = if (attachToXHandler) new mutable.ArrayBuffer[XPoint] else null

      def collectXCtrl(n: Node): Unit = { n match {
        case xs: XPoint =>
          assert(allowXCtrls, s"non-throwing node $node must not be lowered to throwing node ${xs.owner}")
          if (attachToXHandler) {
            newXPoints += xs
          }
        case _ =>
      }}

      val (outValue, outCtrl, outMemory) = currentScope.inState(inCtrl, inMemory) {
        val value: Node = onCommit.withCallback(collectXCtrl) { decomposeNode(node) }
        (value, currentCtrl, currentMemory)
      }

      val unreachable = outCtrl.isInstanceOf[Halt]

      if (attachToXHandler) {
        if (monitorExitIsLowered) {
          assert(origXCtrl == null)
          handlerForMonitorExits.addArgs(newXPoints.toSeq)
        } else {
          assert(origXCtrl != null)
          linkNewXPoints(origXCtrl, newXPoints)
        }
      }

      if (unreachable) {
        // tail of split block becomes unreachable and should be eliminated (including node)
        splitBlockEnd.makeUsesUnreachable()
        decommit(splitBlockEnd)

        eliminateUnreachableCode()

        TaggedState.Unreachable

      } else {
        assert (outCtrl.block.blockEnd == null)

        splitBlockEnd.replaceArgs(outCtrl, outMemory)
        outCtrl.block.blockEnd = splitBlockEnd

        node match {
          case controlNode: SpinalNode => assert (controlNode.inCtrl == nextBlock)
          case _: ControlNode => shouldNotReachHere()
          case _ =>
        }

        inMemory.replaceUses { case e if e.isMemory && memoryUsesBelow(e.target) => nextBlock }

        replaceCompletelyInPartsAndRemoveXPoint(node) {
          case Tag.CONTROL => nextBlock
          case Tag.MEMORY => nextBlock
          case Tag.VALUE => outValue
        }

        // TODO: consider merging blocks, but it invalidates nextBlock

        TaggedState(nextBlock, nextBlock)
      }
    }

    lowerAll()
  }}

  /** Should be lowered if returned value is not `NONE`. */
  private def shouldBeLowered(node: Node): LoweringKind = {
    if (nodesCloned.contains(node)) return NONE
    shouldBeLoweredCases(node)
  }

  protected def shouldBeLoweredCases(node: Node): LoweringKind = node match {
    case _: GetStatic | _: GetField | _: GetConstField | _: ArrayGetOperation => FLOATING
    case _: ConstString | _: AJString | _: SymbolAddress | _: RunTimeTypeInfo | _: InstanceDescriptor | _: InstanceDescriptorBy | _: FieldAddr | _: GetElementPtr | _: VirtualMethodAddr => FLOATING
    case _: ThisTypeInfo | _: ThisTypeInfoBy => FLOATING
    case _: GetFlatThin => FLOATING
    case _: CompileTimeOp => FLOATING
    case _: InitializedTest => FLOATING
    case ValueConvert(F16, I32 | I64, _) | ValueConvert(I32 | I64, F16, _) => FLOATING
    case _: AnyInvokeTarget => FLOATING
    case _: StackDescriptor => FLOATING

    case newOp: InlineableAllocator if !newOp.shouldBeInlined => SPINAL
    case _: NewStackAllocated | _: NewArrayStackAllocated | _: NewString | _: ThinNew => SPINAL
    case _: StrConcat => SPINAL
    case _: GetClass => SPINAL
    case aic: ArrayIndexCheck if aic.trusted => SPINAL
    case asc: ArrayStoreCheck if asc.trusted => SPINAL
    case arrayFill: ArrayFill if arrayFill.size <= MaxArrayFillSizeForSplitting => SPINAL
    case _: AJCallerClass => SPINAL
    case c: CheckCast if c.trusted => SPINAL
    case c: ThinCheckCast if c.trusted => SPINAL
    case _: CheckCastTrustedDelayed => SPINAL
    case _: PutStatic | _: PutField | _: ArrayPutOperation => SPINAL
    case _: Deferred => SPINAL
    case _: ZeroRefs => SPINAL
    case b: BoxedValue if !b.isHot => SPINAL
    case x: MemAtomic if procForMemAtomic(x).isDefined => SPINAL
    case _: VerificationWriteBarrier => SPINAL
    case _: WriteBarrier => SPINAL
    case _: AcquireRawData | _: ReleaseRawData => SPINAL
    case _: MutFunc.HostLocal | _: MutFunc.HostGlobal => SPINAL

    case _: CompositeNode => COMPLEX
    case c: Call if callFromManagedToForeign(c) => COMPLEX
    case ValueConvert(F16, F32, _) | ValueConvert(F32, F16, _) if env.enabled(SoftFP16) => COMPLEX
    case d: Deprive if alwaysLowerDeprive || d.isLoweredWithWeakCast => COMPLEX
    case _: Enrich if !useEnrichedPointers => COMPLEX
    case nc: AbstractNullCheck if nc.trusted => COMPLEX
    case CheckedOp(CheckedOp.Kind.DIV, _, _) => COMPLEX
    case dc: DivisorCheck if dc.trusted => COMPLEX
    case x: MathIntrinsic if procForMathIntrinsic(x).isDefined => COMPLEX

    case _ => NONE
  }

  /** Decomposes one node. */
  protected def decomposeNode(node: Node): Node = {
    val value = node match {
      case x: CheckCast                       => lowerCheckCast(x); null
      case x: InstanceOf                      => lowerInstanceOf(x)
      case x: InitializedTest                 => lowerInitializedTest(x)
      case x: AbstractInitializationCheck     => lowerInitializationCheck(x); null
      case x: PreparationCheck                => lowerPreparationCheck(x); null
      case x: New                             => lowerNew(x)
      case x: NewStackAllocated               => lowerNewStackAllocated(x)
      case x: NewArray                        => lowerNewArray(x)
      case x: NewArrayStackAllocated          => lowerNewArrayStackAllocated(x)
      case x: NewArrayMimic                   => lowerNewArrayMimic(x)
      case x: NewArrayCopy                    => lowerNewArrayCopy(x)
      case x: NewArrayCopyRT                  => lowerNewArrayCopyRT(x)
      case x: NewString                       => lowerNewString(x)
      case x: StrConcat                       => lowerStrConcat(x)
      case x: XClassObject                    => lowerClassObject(x)
      case x: GetClass                        => lowerGetClass(x)
      case x: NullCheck                       => lowerNullCheck(x); null
      case x: ThinNullCheck                   => lowerThinNullCheck(x); null
      case x: DivisorCheck                    => lowerTrustedDivisorCheck(x); null
      case x: CheckedOp                       => lowerCheckedOp(x)
      case x: ArrayIndexCheck                 => lowerArrayIndexCheck(x); null
      case x: ArrayStoreCheck                 => lowerArrayStoreCheck(x); null
      case x: ArrayFill                       => lowerArrayFill(x); null
      case x: AJArrayFill                     => lowerAJArrayFill(x); null
      case x: AnyInvokeTarget                 => lowerAnyInvokeTarget(x)
      case x: Call                            => lowerCall(x)
      case x: WeakCast                        => lowerWeakCast(x)
      case x: Deprive                         => lowerDeprive(x)
      case x: Enrich                          => lowerEnrich(x)
      case x: AJCallerClass                   => lowerAJCallerClass(x)
      case x: ThreeCmp                        => lowerThreeCmp(x)
      case x: MonitorEnter                    => lowerMonitorEnter(x)
      case x: MonitorExit                     => lowerMonitorExit(x); null
      case x: TauTest                         => lowerTauTest(x)
      case x: ErrorRTSCall                    => lowerErrorRTSCall(x); null
      case _: CheckCastTrustedDelayed         => null
      case x: GetStatic                       => lowerGetStatic(x)
      case x: PutStatic                       => lowerPutStatic(x); null
      case x: InstanceFieldOperation          => lowerInstanceFieldOperation(x)
      case x: ArrayElementOperation           => lowerArrayElementOperation(x)
      case x: AggressiveClinitAnalysisAssert  => lowerAggressiveClinitAnalysisCheck(x); null
      case x: ArrayLength                     => lowerArrayLength(x)
      case x: ConstString                     => lowerConstString(x)
      case x: AJString                        => lowerAJString(x)
      case x: SymbolAddress                   => lowerSymbolAddress(x)
      case x: Deferred                        => lowerDeferredOp(x)
      case x: ThinCheckCast                   => lowerThinCheckCast(x); null
      case x: ThinInstanceOf                  => lowerThinInstanceOf(x)
      case x: ThinNew                         => lowerThinNew(x); null
      case x: RunTimeTypeInfo                 => lowerRunTimeTypeInfo(x)
      case x: ThisTypeInfo                    => lowerThisTypeInfo(x)
      case x: InstanceDescriptor              => lowerInstanceDescriptor(x)
      case x: InstanceDescriptorBy            => lowerInstanceDescriptorBy(x)
      case x: ThisTypeInfoBy                  => lowerThisTypeInfoBy(x)
      case x: FieldAddr                       => lowerFieldAddr(x)
      case x: GetElementPtr                   => lowerGetElementPtr(x)
      case x: CFuncWrapperAddr                => lowerExportedToCWrapperAddr(x)
      case x: VirtualMethodAddr               => lowerVirtualMethodAddr(x)
      case x: GetFlatThinCheck                => lowerGetFlatThinCheck(x); null
      case x: GetFlatThin                     => lowerGetFlatThin(x)
      case x: BoxedValue                      => lowerBoxing(x)
      case x: MathIntrinsic                   => lowerMathIntrinsic(x)
      case x: MemAtomic                       => lowerMemAtomic(x)
      case x: ValueConvert                    => lowerValueConvert(x)
      case x: WriteBarrier                    => lowerWriteBarrier(x)
      case x: VerificationWriteBarrier        => lowerVerificationWriteBarrier(x); null
      case x: AcquireRawData                  => lowerAcquireRawData(x)
      case x: ReleaseRawData                  => lowerReleaseRawData(x)
      case x: CompileTimeOp                   => lowerCompileTimeOp(x)
      case x: CopyStructure                   => lowerCopyStructure(x); null
      case x: LockWrapper                     => lowerLockWrapper(x)
      case _: StackDescriptor                 => lowerGetStackDescriptor();
      case x: ExtractEnrichment               => lowerExtractEnrichment(x)
      case x: RawEnrich                       => lowerRawEnrich(x)
      case x: RawDeprive                      => lowerRawDeprive(x)
      case x: Evacuate                        => lowerEvacuate(x)
      case x: SingletonObject                 => lowerSingletonObject(x)
      case x: ZeroRefs                        => lowerZeroRefs(x); null
      case x: MutFunc.HostLocal               => lowerMutFuncHost(x, false)
      case x: MutFunc.HostGlobal              => lowerMutFuncHost(x, true)

      case x: DelayedOp                       => lowerDelayedOp(x)

      case _ => shouldNotReachHere(node)
    }

    if (value != null && node.tpe.isTraceableRefType) {
      ContextTypesMap.lowerNode(value, node)
    }
    value
  }

}
