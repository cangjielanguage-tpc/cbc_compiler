/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.serialization

import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, BytecodeTypeKind, NoPosition, Position}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.{Domain, RTSProc, symlevel}
import com.huawei.excelsior.jet.compiler.ir.*
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.common.DAIRefKind
import com.huawei.excelsior.jet.compiler.debug.info.{DebugDeclaration, DebugLocalVar, DebugType}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.*

import collection.mutable
import collection.immutable
import com.huawei.excelsior.jet.compiler.opt.middle.UCEComponent
import com.huawei.excelsior.jet.compiler.opt.serialization.RTSProcValues.rtsProcs
import com.huawei.excelsior.jet.compiler.options.BoolOption.DetailedInlineLogs
import com.huawei.excelsior.jet.compiler.serialization.SerializationError
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{InstantiatedType, TypeVariable}
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeMethodReference, ConstraintCallMethodReference, InstantiatedMethodReference, SignatureType}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.condOpt
import scala.annotation.nowarn

/**
 * Deserialization component.
 *
 * Deserialization of method IR can be called in two modes: with optimizations enabled (for inline),
 * and with optimizations disabled (for code generation).
 * The following optimizations are performed during deserialization: node identity and value numbering,
 * const branch elimination and unreachable code elimination.
 *
 * @author cypok
 * @author conwor
 * @author alexm
 */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait Deserialization extends IOComponent with UCEComponent { self: Universe =>

  private val TraceDeserialization = false

  private[serialization] def deserializeWithReader(read: OptReader, method: symlevel.Method, args: Seq[Node]): Unit =
    new Deserializer(read, method, args).perform()

  private def getXPointOrNull(node: Node) = node match {
    case sn: SpinalNode => sn.xpoint
    case null => null
  }

  private class Deserializer(read: OptReader, method: symlevel.Method, args: Seq[Node]) {
    private val reservedIdNum = ReservedNodeIds.getNumber(method)

    private var inlineContexts: mutable.IndexedSeq[InlineContext] = _
    private var lexBlocks: mutable.IndexedSeq[LexicalBlock] = _

    private val nodes = new mutable.LinkedHashMap[NodeId, Node]
    private val replacedNodeTags = new mutable.LinkedHashMap[NodeId, (Tag => Node)]

    private def nodeExists(id: NodeId) = (0 <= id && id < reservedIdNum) || nodes.contains(id)

    private def id2node(id: NodeId, tag: Tag): Node = {
      assert(nodeExists(id))
      if (tag == Tag.MEMORY && id == ReservedNodeIds.EntryBlock) {
        entryMemory
      } else {
        nodes.getOrElseUpdate(id, { id match {
          case ReservedNodeIds.EntryBlock => entryBlock
          case x if x < reservedIdNum => args(ReservedNodeIds.getParamNum(x))
        }})
      }
    }

    private def nodeResult(id: NodeId, tag: Tag): Node = {
      replacedNodeTags.get(id) match {
        case Some(tag2node) => tag2node(tag)
        case None => id2node(id, tag)
      }
    }

    private var currBlock: Block = _
    private var lastCtrl: Node = _
    private var returnNode: Return = _

    private case class UnresolvedNode(id: NodeId, args: Seq[NodeId])
    private val nodesToResolve = Maps[Node].newQMap[UnresolvedNode]
    private def toResolve(n: Node, id: NodeId, args: Seq[NodeId]): Unit = { nodesToResolve(n) = UnresolvedNode(id, args) }
    private def isUnresolved(n: Node): Boolean = nodesToResolve.contains(n)

    /** Removed edge nodes */
    private val removedEdges = mutable.Set[NodeId]()
    private def isRemovedEdge(nodeId: NodeId) = removedEdges contains nodeId

    /** Filters out arguments corresponding to removed (unreachable) edges. */
    private def filterArgs[T](argsToFilter: Seq[T], blockArgs: Seq[NodeId]): Seq[T] = {
      assert (argsToFilter.size == blockArgs.size)
      (argsToFilter zip blockArgs) collect { case (arg, edge) if !isRemovedEdge(edge) => arg }
    }

    /** Corrects given node after edge is removed. */
    private def correctNode(n: Node, blockArgs: Seq[NodeId]): Unit = {
      for (UnresolvedNode(id, args) <- nodesToResolve.get(n)) {
        val correctedArgs = filterArgs(args, blockArgs)
        toResolve(n, id, correctedArgs)
        trace("corrected args of unresolved " + n + " from " + args + " to " + correctedArgs)
      }
      n.replaceArgsBySeq(n match {
        case phi: Phi       => filterArgs(phi.argsSeq, blockArgs)
        case block: BBlock  => filterArgs(block.inputs, blockArgs)
        case xblock: XBlock => filterArgs(xblock.inputs, blockArgs) map getXPointOrNull
      })
      trace("corrected args of " + n)
    }

    /** Corrects given already deserialized block after edge is removed. */
    def correctBlock(block: Block): Unit = {
      trace("correcting deserialized block " + block)

      assert (isUnresolved(block))

      val blockArgs = nodesToResolve(block).args
      assert (blockArgs exists isRemovedEdge)

      for (phi <- block.phies.toList) {
        correctNode(phi, blockArgs)
      }
      correctNode(block, blockArgs)
    }

    private var blocks: Seq[BlockSketch] = _
    private val blockById = new mutable.HashMap[NodeId, BlockSketch]

    private def trace(msg: => String): Unit = { if (TraceDeserialization) println("[Deser] " + msg) }

    private implicit object BlockSketchSetsAndMaps extends Sets.Default[BlockSketch] with Maps.Default[BlockSketch]

    /**
     * Basic block representation used to skip reading of unreachable blocks.
     */
    private class BlockSketch(val blockId: NodeId,
                              val xHandlers: Seq[NodeId],
                              val xHandledNodes: Seq[NodeId],
                              val inEdges: Seq[NodeId],
                              val outEdges: Seq[NodeId],
                              val idomId: NodeId,
                              val size: Int) {

      val succs = new Array[BlockSketch](outEdges.size)
      private var removed = false

      blockById(this.blockId) = this
      outEdges foreach { blockById(_) = this }

      def correctSuccessors(): Unit = {
        if (!isEntryBlock) {
          for (id <- inEdges) {
            assert (id >= reservedIdNum)
            // Optional `get` used, because for XBlock inEdges are ids of throwable nodes, not predecessor blocks.
            // TODO: kill BlockSketch with fire, please
            for (predBlock <- blockById.get(id)) {
              val outEdgeIndex = predBlock.outEdges.indexOf(id)
              assert (outEdgeIndex >= 0)
              predBlock.succs(outEdgeIndex) = this
            }
          }
        }
      }

      def isEntryBlock: Boolean = (this == blocks(0))
      def isRemoved: Boolean = removed

      var reachable = true

      def deserialized: Boolean = nodeExists(blockId)

      def node: Block = {
        if (isEntryBlock) {
          entryBlock
        } else {
          id2node(blockId, Tag.CONTROL).asInstanceOf[Block]
        }
      }

      def removeOutEdge(outEdgeId: NodeId): Unit = {
        trace("Removing unreachable edge " + outEdgeId)
        assert (outEdges contains outEdgeId)
        removedEdges += outEdgeId

        val succBlock = succs(outEdges.indexOf(outEdgeId))
        if (succBlock.deserialized) {
          correctBlock(succBlock.node)
        }
      }

      def removeXControlEdges(xControlNodes: Seq[NodeId]): Unit = {
        removedEdges ++= xControlNodes
        for (xHandler <- xHandlers; xBlock = blockById(xHandler)) {
          if (xBlock.deserialized) {
            correctBlock(xBlock.node)
          }
        }
        updateReachability()
      }

      def removeXControlEdge(xControlNode: NodeId): Unit = {
        if (xHandledNodes contains xControlNode) {
          removeXControlEdges(Seq(xControlNode))
        } // Otherwise it is not handled node, and there is no reason to correct xHandlers and update reachability
      }

      def remove(): Unit = {
        removed = true
        outEdges foreach removeOutEdge
        removeXControlEdges(xHandledNodes)
      }
    }

    /** Reads method IR from serialized representation. */
    def perform(): Unit = withFreeUnreachableBlocks {
      currentScope.makeDeserialization {
        currBlock = entryBlock
        lastCtrl = entryBlock

        read.readHeader()
        readBlocksCatalog()
        readInlineContexts()
        readLexicalBlocks()
        for (block <- blocks) {
          if (block.reachable) {
            readBlock(block)
          } else {
            trace("Skipping unreachable block " + block.blockId)
            block.remove()
            read.skip(block.size)
          }
        }

        if (!read.isEOF) {
          SerializationError("Extra data at end of file")
        }

        currentScope.invalidateGraphCaches()
        resolveNodes()

        // Following code support phi functions with one argument.
        // Such phi functions may be produced by deserialization with specialization.
        for (phi <- all[Phi]; value <- ScalaCollections.singleton(phi.args)) {
          phi replaceBy value
        }
      }

      if (env.enabled(DetailedInlineLogs)) {
        dbgPrinter.debugNodes("Local nodes after deserialization")
      }

      // TODO: track these nodes more carefully (they can be decommited while UCE)
      if (returnNode != null && !returnNode.isCommitted) returnNode = null
      currentScope.setResult(returnNode)
    }

    private def updateReachability(): Unit = {
      blocks foreach { _.reachable = false }

      def visit(b: BlockSketch): Unit = if (!b.reachable) {
        b.reachable = true
        for ((succ, idx) <- b.succs.zipWithIndex) {
          if (!removedEdges(b.outEdges(idx))) {
            visit(succ)
          }
        }
        for (xHandler <- b.xHandlers) {
          visit(blockById(xHandler))
        }
      }

      visit(blocks.head)
    }

    private def readBlocksCatalog(): Unit = {
      read.delimiter()
      blocks = read.seq { () =>
        val blockId = read.id()
        val xHandlers = readIds()
        val xHandledNodes = readIds()
        val args = readIds()
        val outEdges = readIds()
        val idomId = read.id()
        val size = read.number()

        new BlockSketch(blockId, xHandlers, xHandledNodes, args, outEdges, idomId, size)
      }
      read.delimiter()

      blocks foreach (_.correctSuccessors())
      updateReachability()
    }

    private def readIds(): Seq[NodeId] = {
      // write.iterable & read.seq are compatible
      read.seq(read.id)
    }

    private def readInlineContexts(): Unit = {
      val count = read.unsignedNumber()
      read.delimiter()

      inlineContexts = new Array[InlineContext](count)
      for (index <- 0 until count) {
        val method = read.method()
        val lineNumber = read.number()
        val bytecodePos = read.number()
        val callerIndex = read.number()
        read.delimiter()

        assert (callerIndex < index)

        val caller = if (callerIndex == NullInlineContextIndex) {
          if (currentInlineContext != null) {
            assert(currentInlineContext.isTopLevel)
            assert(method == currentInlineContext.method)
            currentInlineContext.caller
          } else {
            null
          }
        } else {
          inlineContexts(callerIndex)
        }

        inlineContexts(index) = InlineContext(method, lineNumber, bytecodePos, caller)
      }
    }

    private def readLexicalBlocks(): Unit = {
      val count = read.unsignedNumber()
      read.delimiter()

      lexBlocks = new Array[LexicalBlock](count)
      for (index <- 0 until count) {
        val line = read.number()
        val column = read.number()
        val outerIndex = read.number()
        read.delimiter()

        assert (outerIndex < index)
        val outer = if (outerIndex != NullLexicalBlockIndex) lexBlocks(outerIndex) else null
        lexBlocks(index) = new LexicalBlock(null, line, column, outer)
      }
    }

    private def resolveNodes(): Unit = {
      for ((node, UnresolvedNode(id, args)) <- nodesToResolve) {
        assert(node.arity == args.size)
        val nodeArgs = Seq.from(
          node.inEdges.zip(args.iterator) map { case (e, arg) => nodeResult(arg, e.sourceLabel) }
        )

        node match {
          case _: XBlock => node.replaceArgsBySeq(nodeArgs map getXPointOrNull)
          case _         => node.replaceArgsBySeq(nodeArgs)
        }

        trace("resolved node " + node)

        // Node is already appended, but append is called here to reapply identity and value numbering
        // after correcting arguments.
        val appended = commit(node)
        if (appended != node) {
          node replaceBy appended
          nodes(id) = appended
          trace("just resolved node replaced with " + appended)
        }
      }
    }

    private def readProto(): Prototype[_ <: Node] = {
      val proto = read.proto()
      proto match {
        case x: Prototype[_] => x.asInstanceOf[Prototype[_ <: Node]]

        case Void => Void()
        case True => True()
        case False => False()
        case VarArguments => VarArguments()
        case ExecEnv => ExecEnv()
        case StackDescriptor => StackDescriptor()
        case FrameHeader => FrameHeader()
        case MutFunc.Host => MutFunc.Host()
        case DerivedPtr.Local => DerivedPtr.Local()
        case DerivedPtr.Global => DerivedPtr.Global()
        case Catch => Catch(currBlock.asInstanceOf[XBlock])

        case IConst => IConst(read.number())
        case LConst => LConst(read.longNumber())
        case FConst => FConst(read.floatNumber())
        case DConst => DConst(read.doubleNumber())

        case AnyNull =>
          AnyNull(read.tpe())

        case AJString =>
          AJString.newProto(read.xstring(), read.bool())

        case StackAlloc =>
          StackAlloc(readFrameSlotKind())

        case SymbolAddress =>
          val symbol = read.number() match {
            case 0 => read.method()
            case 1 => asClassType(read.symType()).getThinTypeHandle
            case 2 => read.symType().getTypeHandle
            case 3 => read.symType().getInstanceDescriptor
            case 4 => read.frameDesc()
          }
          SymbolAddress.newProto(symbol)

        case ImportedIndex =>
          ImportedIndex(asClassType(read.symType()))

        case LightInterfCastCBC =>
          LightInterfCastCBC(asClassType(read.symType()))


        case RunTimeTypeInfo.Proto =>
          RunTimeTypeInfo.proto(asClassType(read.symType()))

        case ThisTypeInfo.Proto =>
          ThisTypeInfo.proto(read.sigType())

        case InstanceDescriptor.Proto =>
          InstanceDescriptor(asClassType(read.symType()))

        case FieldAddr.Proto =>
          FieldAddr(read.field())

        case CFuncWrapperAddr.Proto =>
          CFuncWrapperAddr(read.method())

        case VirtualMethodAddr.Proto =>
          VirtualMethodAddr(read.methodRef())

        case GetElementPtr.Proto =>
          GetElementPtr.proto(read.field())

        case NullCheck.Proto =>
          NullCheck(read.bool(), read.domain())

        case Clinit.Proto =>
          Clinit(asClassType(read.symType()))

        case PackageInit.Proto =>
          PackageInit(asClassType(read.symType()))

        case PackageInitCheck.Proto =>
          PackageInitCheck(asClassType(read.symType()))

        case PreparationCheck.Proto =>
          PreparationCheck(read.symType(), read.preparationKind())

        case ConstString.Proto =>
          ConstString(read.constString(), read.symType())

        case New.Proto =>
          New(read.sigType())

        case ArrayIndexCheck.Proto =>
          ArrayIndexCheck(read.sigType(), read.bool())

        case ArrayStoreCheck.Proto =>
          ArrayStoreCheck(read.sigType(), read.bool())

        case ClassObject.Proto =>
          ClassObject(read.symType())

        case InstanceOf.Proto =>
          InstanceOf(read.sigType())

        case MemBarrier.Proto =>
          MemBarrier(BarrierKind.toSet(read.number()))

        case CheckCast.Proto =>
          CheckCast(read.sigType(), read.bool())

        case NewArray.Proto =>
          NewArray(read.sigType())

        case NewArrayMimic.Proto =>
          NewArrayMimic(read.sigType(), read.bool())

        case Switch.Proto =>
          Switch(read.seq(read.number))

        case CondVal.Proto =>
          CondVal(read.bool())

        case Enrich.Proto =>
          Enrich(read.symType())

        case Deprive.Proto =>
          Deprive(read.symType())

        case WeakCast.Proto =>
          WeakCast(read.symType())

        case ICRegionEnter.Proto =>
          ICRegionEnter(inlineContexts(read.number()))

        case ICRegionExit.Proto =>
          ICRegionExit(inlineContexts(read.number()))

        case AJCallerClass.Proto =>
          AJCallerClass(inlineContexts(read.number()))

        case ClinitedAssert.Proto =>
          ClinitedAssert(asClassType(read.symType()))

        case InitializedAssert.Proto =>
          InitializedAssert(asClassType(read.symType()))

        case Prefetch.Proto =>
          Prefetch(read.bool())

        case InitStringRecord.Proto =>
          InitStringRecord.proto(read.sigType(), read.bool(), read.constString())

        case CheckCastTrustedDelayed.Proto =>
          CheckCastTrustedDelayed.proto(read.tpe())

        case AggressiveClinitAnalysisAssert.Proto =>
          AggressiveClinitAnalysisAssert(read.field())

        case ThinCheckCast.Proto =>
          ThinCheckCast(read.symType(), read.bool())

        case ThinInstanceOf.Proto =>
          ThinInstanceOf(read.symType())

        case ThinNullCheck.Proto =>
          ThinNullCheck(read.bool())

        case ThinNew.Proto =>
          ThinNew(read.symType())

        case NewArrayCopy.Proto =>
          NewArrayCopy(read.sigType())

        case NewArrayCopyRT.Proto =>
          NewArrayCopyRT(read.symType(), read.bool())

        case Add.Proto =>
          Add.proto(read.tpe())

        case CheckedOp.Proto =>
          CheckedOp(read.tpe(), read.enumeration(CheckedOp.Kind.fromOrdinal), read.asmType(), read.bool())

        case Mul.Proto =>
          Mul.proto(read.tpe())

        case Sub.Proto =>
          Sub.proto(read.tpe())

        case IDivRemOp.Proto =>
          IDivRemOp(read.tpe(), read.bool(), read.bool())

        case FDiv.Proto =>
          FDiv(read.tpe())

        case And.Proto =>
          And.proto(read.tpe())

        case Or.Proto =>
          Or.proto(read.tpe())

        case Xor.Proto =>
          Xor.proto(read.tpe())

        case Neg.Proto =>
          Neg(read.tpe())

        case Shift.Proto =>
          Shift.proto(read.tpe(), read.arithOp())

        case Phi.Proto =>
          Phi.proto(read.tpe())

        case Cmp.Proto =>
          Cmp(read.tpe(), read.enumeration(Condition.fromOrdinal))

        case ErrorRTSCall.Proto =>
          ErrorRTSCall.proto(rtsProcs(read.number()), read.methodRef())

        case Call.Proto =>
          Call.proto(read.methodRef())

        case InvokeTarget.Proto =>
          InvokeTarget.proto(read.methodRef())

        case InvokeInterfaceTarget.Proto =>
          InvokeInterfaceTarget.proto(read.methodRef())

        case InvokeVirtualStaticTarget.Proto =>
          InvokeVirtualStaticTarget.proto(read.methodRef())

        case PutField.Proto =>
          PutField.proto(read.field())

        case GetField.Proto =>
          GetField.proto(read.field())

        case PutStatic.Proto =>
          PutStatic.proto(read.field())

        case GetStatic.Proto =>
          GetStatic.proto(read.field())

        case ArrayGet.Proto =>
          ArrayGet(read.sigType(), read.sigType())

        case ArrayPut.Proto =>
          ArrayPut(read.sigType(), read.sigType())

        case ThreeCmp.Proto =>
          ThreeCmp(read.tpe(), read.arithOp())

        case LoadMemory.Normal.Proto =>
          LoadMemory.Normal.proto(read.tpe(), read.asmType(), read.sigType(), read.bool())

        case LoadMemory.Soft.Proto =>
          LoadMemory.Soft.proto(read.tpe(), read.asmType(), read.sigType(), read.number())

        case StoreMemory.Proto =>
          StoreMemory.proto(read.tpe(), read.asmType(), read.sigType(), read.bool())

        case UArrayGet.Proto =>
          UArrayGet(read.asmType())

        case UArrayPut.Proto =>
          UArrayPut(read.asmType())

        case ArrayFill.Proto =>
          ArrayFill(read.sigType(), read.seq(read.longNumber))

        case AJArrayFill.Proto =>
          AJArrayFill(read.sigType(), read.sigType())

        case ReinterpretCast.Proto =>
          ReinterpretCast(read.tpe(), read.tpe())

        case ValueConvert.Proto =>
          ValueConvert(read.asmType(), read.asmType())

        case BitFieldExtract.Proto =>
          BitFieldExtract.raw(read.tpe(), read.tpe(), read.number(), read.number(), read.bool())

        case StrConcat.Proto =>
          StrConcat(read.seq(read.symType), read.bool())

        case MulH.Proto =>
          MulH.proto(read.tpe())

        case UMulH.Proto =>
          UMulH.proto(read.tpe())

        case CAS.Proto =>
          CAS(read.asmType())

        case BitCount.Proto =>
          BitCount(read.tpe(), read.enumeration(BitCount.Kind.fromOrdinal))

        case BitSwap.Proto =>
          BitSwap.proto(read.tpe())

        case Deferred.New.Proto =>
          Deferred.New(read.number())

        case Deferred.NewArray.Proto =>
          Deferred.NewArray(read.number(), read.number(), read.number())

        case Deferred.InstanceOf.Proto =>
          Deferred.InstanceOf(read.number())

        case Deferred.CheckCast.Proto =>
          Deferred.CheckCast(read.number())

        case Deferred.ClassObject.Proto =>
          Deferred.ClassObject(read.number())

        case Deferred.FieldOp.Proto =>
          Deferred.FieldOp(read.number(), read.sigType(), read.bool(), read.bool())

        case Deferred.UnresolvedInvoke.Proto =>
          Deferred.UnresolvedInvoke(read.number(), read.methodRef())

        case BitcodeDeferred.InvokeTarget.Proto =>
          BitcodeDeferred.InvokeTarget(read.methodRef().asInstanceOf[BitcodeMethodReference])

        case BitcodeDeferred.New.Proto =>
          BitcodeDeferred.New(read.sigType())

        case BitcodeDeferred.NewArray.Proto =>
          BitcodeDeferred.NewArray(read.sigType())

        case BitcodeDeferred.InstanceOf.Proto =>
          BitcodeDeferred.InstanceOf(read.sigType())

        case BitcodeDeferred.CheckCast.Proto =>
          BitcodeDeferred.CheckCast(read.sigType())

        case BitcodeDeferred.GetField =>
          BitcodeDeferred.GetField.proto(read.fieldRef())
          
        case BitcodeDeferred.PutField =>
          BitcodeDeferred.PutField.proto(read.fieldRef())

        case GetFlatThin.Proto =>
          GetFlatThin(asClassType(read.symType()))

        case BoxedValue.Proto =>
          BoxedValue(read.enumeration(BytecodeTypeKind.fromOrdinal), read.domain())

        case MathIntrinsic.Proto =>
          MathIntrinsic(read.enumeration(Java.Lang.MathIntrinsic.fromOrdinal))

        case DivisorCheck.Proto =>
          DivisorCheck.proto(read.bool(), read.tpe())

        case MemAtomic.Proto =>
          MemAtomic(read.enumeration(MemAtomic.Kind.fromOrdinal), read.asmType())

        case DelayedGet.Proto =>
          DelayedGet(read.xstring(), read.xstring(), read.tpe())

        case DelayedPut.Proto =>
          DelayedPut(read.xstring(), read.xstring(), read.tpe())

        case DelayedInstanceMethodVNum.Proto =>
          DelayedInstanceMethodVNum(read.xstring(), read.xstring(), read.xstring())

        case DelayedInstanceFieldAddress.Proto =>
          DelayedInstanceFieldAddress(read.xstring(), read.xstring(), read.xstring())

        case DelayedMethodAddr.Proto =>
          DelayedMethodAddr(read.xstring(), read.xstring())

        case IsComputableAtCompileTime.Proto =>
          IsComputableAtCompileTime(read.enumeration(CompileTimeOp.Kind.fromOrdinal))

        case ComputeAtCompileTime.Proto =>
          ComputeAtCompileTime(read.enumeration(CompileTimeOp.Kind.fromOrdinal))

        case CopyStructure.Proto =>
          CopyStructure.proto(read.sigType())

        case ConvertDomain.Proto =>
          ConvertDomain(read.domain())

        case Return.Proto =>
          Return.proto(read.tpe())

        case Halt.Proto =>
          Halt.proto(read.reason())

        case FieldReferenceNode.Proto =>
          FieldReferenceNode.proto(read.cangjieFieldReference())

        case ConstIndex.Proto =>
          ConstIndex.proto(read.number(), read.sigType(), read.sigType())
          
        case IndexFieldReference.Proto =>
          IndexFieldReference.proto(read.sigType(), read.sigType())

        case FieldReferenceNodeGeneric.Proto =>
          FieldReferenceNodeGeneric.proto(read.cangjieFieldReference())

        case ConstIndexGeneric.Proto =>
          ConstIndexGeneric.proto(read.number(), read.sigType(), read.sigType())

        case IndexFieldReferenceGeneric.Proto =>
          IndexFieldReferenceGeneric.proto(read.sigType(), read.sigType())

        case TDBarrier.Proto =>
          TDBarrier(read.bool(), read.bool())

        case EscapeWriteBarrier.Instance.Proto =>
          EscapeWriteBarrier.Instance.proto(read.tpe())

        case EscapeWriteBarrier.Static.Proto =>
          EscapeWriteBarrier.Static.proto(read.tpe())

        case ZeroRefs.Proto =>
          ZeroRefs.proto(read.sigType())

        case UniversalGeneric.ToHolder.Proto =>
          UniversalGeneric.ToHolder.proto(read.sigType(), read.sigType())

        case UniversalGeneric.FromHolder.Proto =>
          UniversalGeneric.FromHolder.proto(read.sigType(), read.sigType())

        case UniversalGeneric.GetElementPtr.Proto =>
          UniversalGeneric.GetElementPtr.proto(read.field(), read.sigType().asInstanceOf[InstantiatedType], read.sigType())

        case UniversalGeneric.GetField.Proto =>
          UniversalGeneric.GetField.proto(read.field(), read.sigType().asInstanceOf[InstantiatedType], read.sigType())

        case UniversalGeneric.GetFieldOHM.Proto =>
          UniversalGeneric.GetFieldOHM.proto(read.field(), read.sigType().asInstanceOf[InstantiatedType], read.sigType())

        case UniversalGeneric.PutField.Proto =>
          UniversalGeneric.PutField.proto(read.field(), read.sigType().asInstanceOf[InstantiatedType], read.sigType())

        case UniversalGeneric.InvokeConstraintMethod.Target.Proto =>
          UniversalGeneric.InvokeConstraintMethod.Target.proto(read.methodRef().asInstanceOf[ConstraintCallMethodReference])

        case UniversalGeneric.InvokeMethodWithGenericContext.Target.Proto =>
          UniversalGeneric.InvokeMethodWithGenericContext.Target.proto(read.methodRef().asInstanceOf[InstantiatedMethodReference])

        case UniversalGeneric.CopyResultVST.Proto =>
          UniversalGeneric.CopyResultVST.proto(read.sigType())

        case UniversalGeneric.OffHeapMemorySlotPointer.Proto =>
          UniversalGeneric.OffHeapMemorySlotPointer.proto(read.sigType())

        case UniversalGeneric.TypeVarIsRef.Proto =>
          UniversalGeneric.TypeVarIsRef.proto(read.sigType().asInstanceOf[TypeVariable])
  
        case UniversalGeneric.CopyUniversalVariable.Proto =>
          UniversalGeneric.CopyUniversalVariable.proto(read.sigType())

        case UniversalGeneric.HolderConst =>
          UniversalGeneric.HolderConst()

        case Deferred.DynamicOrSigPolyInvoke.Proto =>
          Deferred.DynamicOrSigPolyInvoke(read.number(), read.enumeration(DAIRefKind.fromOrdinal), read.methodType(), read.bool())

        case Deferred.SigPolyInvokeBasic.Proto =>
          Deferred.SigPolyInvokeBasic(read.number(), read.methodType())

        case Deferred.MethodHandle.Proto =>
          Deferred.MethodHandle(read.number())

        case Deferred.MethodType.Proto =>
          Deferred.MethodType(read.number())

        case MutFunc.Offset.Proto =>
          MutFunc.Offset.proto(read.sigType())

        case MutFunc.Combine.Proto =>
          MutFunc.Combine.proto(read.tpe())

        case GetFieldSeqRef.Proto =>
          GetFieldSeqRef.proto(read.tpe(), read.tpe())

        case GetStaticFieldSeqRef.Proto =>
          GetStaticFieldSeqRef.proto(read.tpe())

        case LoadFieldSeq.Proto =>
          LoadFieldSeq.proto(read.tpe(), read.tpe())

        case LoadStaticFieldSeq.Proto =>
          LoadStaticFieldSeq.proto(read.tpe())

        case StoreFieldSeq.Proto =>
          StoreFieldSeq.proto(read.tpe(), read.tpe())

        case StoreStaticFieldSeq.Proto =>
          StoreStaticFieldSeq.proto(read.tpe())

        case LoadTypeInfo.Proto =>
          LoadTypeInfo.proto(read.sigType())

        case LoadTypeInfoGeneric.Proto =>
          LoadTypeInfoGeneric.proto(read.sigType())

        case GenericTypeArg.Proto =>
          GenericTypeArg.proto(read.number())

        case Box.Proto =>
          Box.proto(read.sigType())

        case Unbox.Proto =>
          Unbox.proto(read.sigType())

        case UnboxRec.Proto =>
          UnboxRec.proto(read.sigType())

        case UnboxLea.Proto =>
          UnboxLea.proto(read.sigType())

        case SpawnFuture.Proto =>
          SpawnFuture.proto(read.sigType())

        case SpawnClosure.Proto =>
          SpawnClosure.proto(read.sigType())

        case OptionTagGeneric.Proto =>
          OptionTagGeneric.proto(read.sigType().asInstanceOf[SignatureType.OptionLikeEnum])

        case OptionPayloadGeneric.Proto =>
          OptionPayloadGeneric.proto(read.sigType().asInstanceOf[SignatureType.OptionLikeEnum])

        case NewNoneOptionGeneric.Proto =>
          NewNoneOptionGeneric.proto(read.sigType().asInstanceOf[SignatureType.OptionLikeEnum])

        case NewSomeOptionGeneric.Proto =>
          NewSomeOptionGeneric.proto(read.sigType().asInstanceOf[SignatureType.OptionLikeEnum])

        case AssignGeneric.Proto =>
          AssignGeneric.proto(read.sigType())

        case InstanceOfGeneric.Proto =>
          InstanceOfGeneric.proto(read.sigType())

        case AtomicOps.Load.Proto =>
          AtomicOps.Load.proto(read.tpe(), read.cangjieFieldReference())

        case AtomicOps.Store.Proto =>
          AtomicOps.Store.proto(read.tpe(), read.cangjieFieldReference())

        case AtomicOps.CAS.Proto =>
          AtomicOps.CAS.proto(read.tpe(), read.cangjieFieldReference())

        case AtomicOps.Simple.Proto =>
          AtomicOps.Simple.proto(read.enumeration(AtomicOps.Simple.Kind.fromOrdinal), read.tpe(), read.cangjieFieldReference())

        case NewGeneric.Proto =>
          NewGeneric.proto(read.sigType())
      }
    }

    /**
     * Makes phi-function or block arguments as a sequence of [[com.huawei.excelsior.jet.compiler.opt.ir.Nodes.Node Node]].
     * `null` is used in place of arguments referencing not yet deserialized nodes.
     */
    private def makePartiallyDefinedArgs(args: Seq[NodeId], tag: Tag): Seq[Node] = {
      args.map { arg => if (nodeExists(arg)) nodeResult(arg, tag) else null }
    }

    private def makeBlock[N <: Node](nodeId: NodeId, proto: Prototype[N], blockSketch: BlockSketch): Block = {
      assert (nodeId == blockSketch.blockId)

      val origArgIds = blockSketch.inEdges
      val argIds = filterArgs(origArgIds, origArgIds)
      val args = makePartiallyDefinedArgs(argIds, Tag.CONTROL)

      val block = (proto match {
        case XBlock => proto(args map getXPointOrNull: _*)
        case BBlock => proto(args: _*)
      }).asInstanceOf[Block]

      val undefArgs = block.hasUndefinedArgs
      if (undefArgs) {
        toResolve(block, nodeId, argIds)
      }

      // immediate dominator from serialized information
      val sIdom @ (_sIdom: Block) = id2node(blockSketch.idomId, Tag.CONTROL)

      if (undefArgs || (proto == XBlock)) {
        toResolve(block, nodeId, argIds)

        // because some of block preds are not deserialized yet
        // we should conservatively use serialized dominator information
        currentScope.refreshDominatorsForced(block, sIdom)
      } else {
        assert(sIdom dominates block.idomBlock)
      }

      block
    }

    private def makePhi(proto: Phi.Proto, nodeId: NodeId, blockSketch: BlockSketch): Node = {
      assert(!blockSketch.isEntryBlock) // No phies in entry block, please

      val argIds = filterArgs(readIds(), blockSketch.inEdges)
      val args = currBlock +: makePartiallyDefinedArgs(argIds, Tag.VALUE)
      val phi = Phi(proto.keyType)(args: _*)

      if (phi.hasUndefinedArgs && !isUnresolved(phi)) {
        toResolve(phi, nodeId, argIds)
      }

      phi
    }

    private def makeNormalNode(proto: Prototype[_ <: Node]): Node = {
      val controlArg = condOpt(proto) {
        case _: SpinalNodePrototype[_] | _: BlockEndProto[_] => lastCtrl
      }

      val args = readIds().zipWithIndex

      val otherArgs = args map { x => nodeResult(x._1, proto.argTag(x._2 + controlArg.size)) }
      val allArgs = Seq.empty ++ controlArg ++ otherArgs

      val node = proto.withExplicitArgs(allArgs: _*)

      node match {
        case _: ArrayStoreCheck =>
          val asc = node.asInstanceOf[ArrayStoreCheck]
          val hasArrayTypeForFastPath = read.bool()
          if (hasArrayTypeForFastPath) {
            asc.arrayTypeForFastPath = read.typeApproximation()
            val valueHasRelaxedType = read.bool()
            if (valueHasRelaxedType) {
              asc.valueRelaxedType = read.referenceType()
            }
          }

        case _ =>
      }

      node
    }

    /** Returns `true` if this control node was created and is not yet appended to control skeleton.
     *  Returns `false` if it has been already created and appended to control skeleton before.
     *
     *  Used to avoid optimization of control nodes returned by identity.
     *  For example, memory phi-function `phi(x,x)` can be replaced with `x`, where `x` is control node.
     */
    private def isNewCtrl(ctrl: SpinalNode) = (ctrl.inCtrl == lastCtrl)

    private def adjustControl(node: Node): Unit = {
      node match {
        case end: BlockEnd =>
          assert (currBlock.blockEnd == end)
          lastCtrl = null
          currBlock = null

        case block: Block =>
          currBlock = block
          lastCtrl = block

        case ctrl: SpinalNode if isNewCtrl(ctrl) =>
          lastCtrl = ctrl

        case _ =>
      }
    }

    private def processBranch(branch: Branch, block: BlockSketch): BlockEnd = {
      val exitsWithIds = branch.exits zip block.outEdges
      for ((exit, id) <- exitsWithIds) {
        nodes(id) = exit
      }

      for (constExit <- branch.constExit) {
        val goto = Goto(branch.inCtrl, branch.inMemory) //TODO: replace by tryEliminateConstBranch(branch)

        val takenEdgeId = exitsWithIds.collectFirst { case (`constExit`, id) => id }.get
        nodes(takenEdgeId) = goto

        for (notTakenEdgeId <- exitsWithIds.collect { case (exit, id) if (exit != constExit) => id }) {
          nodes.remove(notTakenEdgeId)
          block.removeOutEdge(notTakenEdgeId)
        }

        decommit(branch)
        updateReachability()

        return goto
      }

      branch
    }

    private def readPosition(): Position = {
      val lineNumber = read.number()
      if (lineNumber != LineNumber.INVALID) {
        val offset = read.number()
        val icIdx = read.number()
        val lbIdx = read.number()
        val columnNumber = if (LineNumber.isKnown(lineNumber)) read.number() else ColumnNumber.UNKNOWN
        val lexBlock = if (lbIdx != NullLexicalBlockIndex) lexBlocks(lbIdx) else null
        BytecodePosition(offset, lineNumber, columnNumber, inlineContexts(icIdx), lexBlock)
      } else {
        NoPosition
      }
    }

    private def readFrameSlotKind(): FrameSlot.Kind = {
      read.number() match {
        case 0 => FrameSlot.Raw(read.number(), read.number())
        case 1 => FrameSlot.NewOnStack(read.sigType())
        case 2 => FrameSlot.NewArrayOnStack(read.sigType(), read.number())
        case 3 => FrameSlot.Local(read.sigType(), read.bool())
        case 4 => FrameSlot.DebugVar(read.sigType(), readDebugLocalVar())
        case 5 => FrameSlot.OffHeapMemory(read.sigType())
        case x => shouldNotReachHere(s"unsupported FrameSlot.Kind: $x")
      }
    }

    private def readDebugLocalVar(): DebugLocalVar = {
      val name = read.xstring()
      val varType = DebugType.deserialize(read.number, read.xstring)
      val argIndex = read.number()
      val isPointer = read.bool()
      val declaration = read.option { () => DebugDeclaration(read.xstring(), read.number(), read.number(), read.number()) }
      DebugLocalVar(name, varType, argIndex, isPointer, declaration)
    }

    private def readNode(blockSketch: BlockSketch) = {
      val nodeId = read.id()
      val pos = readPosition()

      val node = withPos(pos) {
        val proto = readProto()
        val n = proto match {
          case BBlock | XBlock     => makeBlock(nodeId, proto, blockSketch)
          case phiProto: Phi.Proto => makePhi(phiProto, nodeId, blockSketch)
          case _ => makeNormalNode(proto)
        }

        n match {
          case ret: Return =>
            assert(returnNode == null, "more than one return node in serialized IR")
            returnNode = ret
            n

          case branch: Branch =>
            processBranch(branch, blockSketch)

          case _ => n
        }
      }

      read.delimiter()

      if (!optimizeNode(blockSketch, node, nodeId)) {
        nodes(nodeId) = node
        adjustControl(node)
      }

      trace("read node " + node)

      node match {
        case _: BlockEnd => false
        case _ => true
      }
    }

    /** Returns whether node was replaced by some other node. */
    private def optimizeNode(blockSketch: BlockSketch, node: Node, nodeId: NodeId): Boolean = {
      node match {
        case node: Idempotent if isNewCtrl(node) && IdempotentOperationsOptimizer.shouldOptimize(node) =>
          node match {
            case sn: SpinalNode if sn.canThrow =>
              assert(sn.xpoint.uses.isEmpty)
              assert(sn.uses.size == 1)
            case _ =>
              assert(node.uses.isEmpty)
          }

          IdempotentOperationsOptimizer.findIdempotentDominator(node) match {
            case Some(idemDom) =>
              trace("idempotent dominator is found: node " + node + " replaced by " + idemDom)
              var replacement = immutable.Map(Tag.CONTROL -> node.inCtrl)
              node match {
                case node: SpinalMemoryNode =>
                  replacement += (Tag.MEMORY -> node.inMemory)
                case _ =>
              }
              if (node.producesValue) {
                replacement += (Tag.VALUE -> idemDom)
              }
              replacedNodeTags(nodeId) = replacement
              nodes(nodeId) = Void()
              node match {
                case x: SpinalNode if x.canThrow => blockSketch.removeXControlEdge(nodeId)
                case _ =>
              }
              decommit(node)
              true

            case None =>
              false
          }

        case _ =>
          false
      }
    }

    private def readBlock(blockSketch: BlockSketch): Unit = {
      while (readNode(blockSketch)) {
      }
    }
  }
}

private object RTSProcValues {
  lazy val rtsProcs = RTSProc.values
}
