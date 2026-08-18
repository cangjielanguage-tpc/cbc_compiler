/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.serialization

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.{ArithOp, BytecodePosition, NoPosition}
import com.huawei.excelsior.jet.compiler.debug.info.DebugLocalVar
import com.huawei.excelsior.jet.compiler.ir.*
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.*
import com.huawei.excelsior.jet.compiler.symlevel.{InstanceDescriptorSymbol, MethodReference, MethodReferenceAccessKind, MethodType, SignatureType}
import com.huawei.excelsior.jet.compiler.types.References.ReferenceApprox
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.compiler.{Domain, PreparationKind, RTSProc, symlevel}

import scala.collection.mutable

/**
 * Serialization component.
 *
 * @author cypok
 * @author conwor
 * @author alexm
 */
trait Serialization extends IOComponent { self: Universe =>

  private[serialization] def serializeWithWriter(write: OptWriter, method: symlevel.Method): Unit = {
    new Serializer(write, method).perform()
  }

  private class Serializer(write: OptWriter, method: symlevel.Method) {

    val entryBlock = self.entryBlock
    val entryMemory = self.entryMemory

    private val reservedIdNum = ReservedNodeIds.getNumber(method)

    private var inlineContextsCount: Int = 0
    private val inlineContexts = mutable.LinkedHashMap[InlineContext, Int]()

    private var lexBlocksCount: Int = 0
    private val lexBlocks = mutable.LinkedHashMap[LexicalBlock, Int]()

    def perform(): Unit = {
      withGCM(new GCMEngine(onlyEarly = true)) {
        dbgPrinter.debugNodes("All graph before serialization")
        dbgPrinter.debugNodes("All graph before serialization with positions", { "(" + _.pos.toString + ")" })
        dbgPrinter.debugGraphs("before serialization")
        checkDAGsConsistency(CheckLevels.Desirable)
        checkIRConsistency(CheckLevels.Important)

        val blocks = Maps[Block].newQMap[Int]

        val nodesBuffer = write.withBuffering {
          cfg.topSort.order.foreach { block =>
            val startPos = write.bufferPosition
            LinearNodeOrder.strictBlockOrder(block) foreach writeNode
            val size = (write.bufferPosition - startPos)
            blocks(block) = size
          }
        }

        val inlineContextsBuffer = write.withBuffering {
          writeInlineContexts()
        }
        val lbContextsBuffer = write.withBuffering {
          writeLexBlocks()
        }

        write.writeHeader()
        writeBlocksCatalog(blocks)
        write.writeBuffer(inlineContextsBuffer)
        write.writeBuffer(lbContextsBuffer)
        write.writeBuffer(nodesBuffer)
      }
    }

    /**
     * Writes brief information about CFG.
     *
     * @param blocks Ordered map from blocks to their serialized size.
     *               Order should be the same as the order of blocks in the serialized representation (topsort).
     */
    private def writeBlocksCatalog(blocks: Maps[Block]#QMap[Int]): Unit = {
      write.delimiter()
      write.seq(blocks.toSeq) { case (block, size) =>
        writeNodeId(block)
        writeNodeIds(block.xHandlers.toList)
        writeNodeIds(block.handledXPoints.map(_.owner).toList)
        writeNodeArgs(block)
        writeNodeIds(block.blockEnd.exits)

        val idom = block.idomBlock
        assert ((idom == null) == (block == entryBlock))
        if (idom == null) {
          write.id(ReservedNodeIds.Invalid)
        } else {
          writeNodeId(idom)
        }

        write.number(size)
      }
      write.delimiter()
    }

    private def writeAny(x: Any): Unit = x match {
      case x: Boolean => write.bool(x)
      case x: Int => write.number(x)
      case x: Long => write.longNumber(x)
      case x: Float => write.floatNumber(x)
      case x: Double => write.doubleNumber(x)
      case x: Type => write.tpe(x)
      case x: VMStateApprox => write.vmstate(x)
      case x: XString => write.xstring(x)
      case x: ArithOp => write.arithOp(x)
      case x: Width => write.width(x)
      case x: AsmType => write.asmType(x)
      case x: SignatureType => write.sigType(x)
      case x: symlevel.Type => write.symType(x)
      case x: symlevel.Field => write.field(x)
      case x: symlevel.BitcodeFieldReference => write.fieldRef(x)
      case x: symlevel.Method => write.method(x)
      case x: symlevel.ConstString => write.constString(x)
      case x: RTSProc => write.number(x.ordinal)
      case x: Seq[_] => write.seq(x)(writeAny)
      case x: InlineContext => write.number(getInlineContextRef(x))
      case x: LexicalBlock => write.number(getLexBlockRef(x))
      case x: ReferenceApprox => write.typeApproximation(x)
      case x: MethodType => write.methodType(x)
      case x: MethodReference => write.methodRef(x)
      case x: MethodReferenceAccessKind => write.methodRefAccessKind(x)
      case x: PreparationKind => write.preparationKind(x)
      case x: Domain => write.domain(x)
      case x: JBCBoxType => write.enumeration(x.kind)
      case x: scala.reflect.Enum => write.enumeration(x)
    }

    private def writeProto(n: Node): Unit = {
      val proto: AnyRef = n match {
        case _: Void => Void
        case _: True => True
        case _: False => False
        case _: VarArguments => VarArguments
        case _: ExecEnv => ExecEnv
        case _: StackDescriptor => StackDescriptor
        case _: FrameHeader => FrameHeader
        case _: IConst => IConst
        case _: LConst => LConst
        case _: FConst => FConst
        case _: DConst => DConst
        case _: AnyNull => AnyNull
        case _: AJString => AJString
        case _: StackAlloc => StackAlloc
        case _: SymbolAddress => SymbolAddress
        case _: ImportedIndex => ImportedIndex
        case _: LightInterfCastCBC => LightInterfCastCBC
        case _: MutFunc.Host => MutFunc.Host
        case _: DerivedPtr.Local => DerivedPtr.Local
        case _: DerivedPtr.Global => DerivedPtr.Global
        case _: Catch => Catch
        case _: UniversalGeneric.HolderConst => UniversalGeneric.HolderConst
        case _: Proxy => shouldNotReachHere("Proxy serialization is not expected")
        case _: NoValue => shouldNotReachHere("NoValue serialization is not expected")
        case _: LeafNode[_] => shouldNotReachHere(s"all leaf nodes should be listed here, missing ${n.simpleName}")

        case _: NullCheck => NullCheck.Proto
        case _: Clinit => Clinit.Proto
        case _: PackageInit => PackageInit.Proto
        case _: PackageInitCheck => PackageInitCheck.Proto
        case _: PreparationCheck => PreparationCheck.Proto
        case _: ConstString => ConstString.Proto
        case _: New => New.Proto
        case _: ArrayIndexCheck => ArrayIndexCheck.Proto
        case _: ArrayStoreCheck => ArrayStoreCheck.Proto
        case _: ClassObject => ClassObject.Proto
        case _: RunTimeTypeInfo => RunTimeTypeInfo.Proto
        case _: ThisTypeInfo => ThisTypeInfo.Proto
        case _: InstanceDescriptor => InstanceDescriptor.Proto
        case _: FieldAddr => FieldAddr.Proto
        case _: CFuncWrapperAddr => CFuncWrapperAddr.Proto
        case _: VirtualMethodAddr => VirtualMethodAddr.Proto
        case _: GetElementPtr => GetElementPtr.Proto
        case _: DelayedInstanceMethodVNum => DelayedInstanceMethodVNum.Proto
        case _: DelayedInstanceFieldAddress => DelayedInstanceFieldAddress.Proto
        case _: DelayedMethodAddr => DelayedMethodAddr.Proto
        case _: InstanceOf => InstanceOf.Proto
        case _: MemBarrier => MemBarrier.Proto
        case _: CheckCast => CheckCast.Proto
        case _: NewArray => NewArray.Proto
        case _: NewArrayMimic => NewArrayMimic.Proto
        case _: Switch => Switch.Proto
        case _: CondVal => CondVal.Proto
        case _: Enrich => Enrich.Proto
        case _: Deprive => Deprive.Proto
        case _: WeakCast => WeakCast.Proto
        case _: ICRegionEnter => ICRegionEnter.Proto
        case _: ICRegionExit => ICRegionExit.Proto
        case _: AJCallerClass => AJCallerClass.Proto
        case _: ClinitedAssert => ClinitedAssert.Proto
        case _: InitializedAssert => InitializedAssert.Proto
        case _: Prefetch => Prefetch.Proto
        case _: CheckCastTrustedDelayed => CheckCastTrustedDelayed.Proto
        case _: AggressiveClinitAnalysisAssert => AggressiveClinitAnalysisAssert.Proto
        case _: ThinCheckCast => ThinCheckCast.Proto
        case _: ThinInstanceOf => ThinInstanceOf.Proto
        case _: ThinNullCheck => ThinNullCheck.Proto
        case _: ThinNew => ThinNew.Proto
        case _: NewArrayCopy => NewArrayCopy.Proto
        case _: NewArrayCopyRT => NewArrayCopyRT.Proto
        case _: Add => Add.Proto
        case _: CheckedOp => CheckedOp.Proto
        case _: Mul => Mul.Proto
        case _: Sub => Sub.Proto
        case _: IDivRemOp => IDivRemOp.Proto
        case _: FDiv => FDiv.Proto
        case _: And => And.Proto
        case _: Or => Or.Proto
        case _: Xor => Xor.Proto
        case _: Neg => Neg.Proto
        case _: Shift => Shift.Proto
        case _: Phi => Phi.Proto
        case _: Cmp => Cmp.Proto
        case _: ErrorRTSCall => ErrorRTSCall.Proto
        case _: Call => Call.Proto
        case _: InvokeTarget => InvokeTarget.Proto
        case _: InvokeInterfaceTarget => InvokeInterfaceTarget.Proto
        case _: InvokeVirtualStaticTarget => InvokeVirtualStaticTarget.Proto
        case _: PutField => PutField.Proto
        case _: GetField => GetField.Proto
        case _: PutStatic => PutStatic.Proto
        case _: GetStatic => GetStatic.Proto
        case _: ArrayGet => ArrayGet.Proto
        case _: ArrayPut => ArrayPut.Proto
        case _: ThreeCmp => ThreeCmp.Proto
        case n: LoadMemory => n.proto match {
          case _: LoadMemory.Independent.Proto => shouldNotReachHere("independent nodes are not serialized")
          case _: LoadMemory.Normal.Proto => LoadMemory.Normal.Proto
          case _: LoadMemory.Soft.Proto => LoadMemory.Soft.Proto
        }
        case _: InitStringRecord => InitStringRecord.Proto
        case _: StoreMemory => StoreMemory.Proto
        case _: CopyStructure => CopyStructure.Proto
        case _: UArrayGet => UArrayGet.Proto
        case _: UArrayPut => UArrayPut.Proto
        case _: ArrayFill => ArrayFill.Proto
        case _: AJArrayFill => AJArrayFill.Proto
        case _: ReinterpretCast => ReinterpretCast.Proto
        case _: ValueConvert => ValueConvert.Proto
        case _: BitFieldExtract => BitFieldExtract.Proto
        case _: StrConcat => StrConcat.Proto
        case _: MulH => MulH.Proto
        case _: UMulH => UMulH.Proto
        case _: CAS => CAS.Proto
        case _: BitCount => BitCount.Proto
        case _: BitSwap => BitSwap.Proto
        case _: GetFlatThin => GetFlatThin.Proto
        case _: BoxedValue => BoxedValue.Proto
        case _: MathIntrinsic => MathIntrinsic.Proto
        case _: DivisorCheck => DivisorCheck.Proto
        case _: MemAtomic => MemAtomic.Proto
        case _: IsComputableAtCompileTime => IsComputableAtCompileTime.Proto
        case _: ComputeAtCompileTime => ComputeAtCompileTime.Proto
        case _: TrapCheck => TrapCheck
        case _: ExecEnvInvalidationPoint => ExecEnvInvalidationPoint
        case _: ConvertDomain => ConvertDomain.Proto
        case _: Return => Return.Proto
        case _: TDBarrier => TDBarrier.Proto
        case _: EscapeWriteBarrier.Instance => EscapeWriteBarrier.Instance.Proto
        case _: EscapeWriteBarrier.Static => EscapeWriteBarrier.Static.Proto
        case _: ZeroRefs => ZeroRefs.Proto
        case _: Halt => Halt.Proto
        case _: FieldReferenceNode => FieldReferenceNode.Proto
        case _: ConstIndex => ConstIndex.Proto
        case _: Index => Index.Proto
        case _: FieldReferenceNodeGeneric => FieldReferenceNodeGeneric.Proto
        case _: ConstIndexGeneric => ConstIndexGeneric.Proto
        case _: IndexGeneric => IndexGeneric.Proto

        case n: Deferred => n.proto match {
          case _: Deferred.New.Proto => Deferred.New.Proto
          case _: Deferred.NewArray.Proto => Deferred.NewArray.Proto
          case _: Deferred.InstanceOf.Proto => Deferred.InstanceOf.Proto
          case _: Deferred.CheckCast.Proto => Deferred.CheckCast.Proto
          case _: Deferred.ClassObject.Proto => Deferred.ClassObject.Proto
          case _: Deferred.FieldOp.Proto => Deferred.FieldOp.Proto
          case _: Deferred.UnresolvedInvoke.Proto => Deferred.UnresolvedInvoke.Proto
          case _: Deferred.DynamicOrSigPolyInvoke.Proto => Deferred.DynamicOrSigPolyInvoke.Proto
          case _: Deferred.SigPolyInvokeBasic.Proto => Deferred.SigPolyInvokeBasic.Proto
          case _: Deferred.MethodHandle.Proto => Deferred.MethodHandle.Proto
          case _: Deferred.MethodType.Proto => Deferred.MethodType.Proto
        }

        case _: BitcodeDeferred.InvokeTarget => BitcodeDeferred.InvokeTarget.Proto
        case _: BitcodeDeferred.New => BitcodeDeferred.New.Proto
        case _: BitcodeDeferred.NewArray => BitcodeDeferred.NewArray.Proto
        case _: BitcodeDeferred.InstanceOf => BitcodeDeferred.InstanceOf.Proto
        case _: BitcodeDeferred.CheckCast => BitcodeDeferred.CheckCast.Proto
        case _: BitcodeDeferred.GetField => BitcodeDeferred.GetField
        case _: BitcodeDeferred.PutField => BitcodeDeferred.PutField

        case _: DelayedGet => DelayedGet.Proto
        case _: DelayedPut => DelayedPut.Proto

        case _: UniversalGeneric.ToHolder => UniversalGeneric.ToHolder.Proto
        case _: UniversalGeneric.FromHolder => UniversalGeneric.FromHolder.Proto
        case _: UniversalGeneric.GetElementPtr => UniversalGeneric.GetElementPtr.Proto
        case _: UniversalGeneric.GetField => UniversalGeneric.GetField.Proto
        case _: UniversalGeneric.GetFieldOHM => UniversalGeneric.GetFieldOHM.Proto
        case _: UniversalGeneric.PutField => UniversalGeneric.PutField.Proto
        case _: UniversalGeneric.InvokeConstraintMethod.Target => UniversalGeneric.InvokeConstraintMethod.Target.Proto
        case _: UniversalGeneric.InvokeMethodWithGenericContext.Target => UniversalGeneric.InvokeMethodWithGenericContext.Target.Proto
        case _: UniversalGeneric.CopyResultVST => UniversalGeneric.CopyResultVST.Proto
        case _: UniversalGeneric.OffHeapMemorySlotPointer => UniversalGeneric.OffHeapMemorySlotPointer.Proto
        case _: UniversalGeneric.TypeVarIsRef => UniversalGeneric.TypeVarIsRef.Proto
        case _: UniversalGeneric.CopyUniversalVariable => UniversalGeneric.CopyUniversalVariable.Proto

        case _: MutFunc.Offset => MutFunc.Offset.Proto
        case _: MutFunc.Combine => MutFunc.Combine.Proto

        case _: GetFieldSeqRef => GetFieldSeqRef.Proto
        case _: GetStaticFieldSeqRef => GetStaticFieldSeqRef.Proto
        case _: LoadFieldSeq => LoadFieldSeq.Proto
        case _: LoadStaticFieldSeq => LoadStaticFieldSeq.Proto
        case _: StoreFieldSeq => StoreFieldSeq.Proto
        case _: StoreStaticFieldSeq => StoreStaticFieldSeq.Proto
        case _: DerivedPtr => DerivedPtr.Proto
        case _: LoadTypeInfo => LoadTypeInfo.Proto
        case _: LoadTypeInfoGeneric => LoadTypeInfoGeneric.Proto
        case _: GenericTypeArg => GenericTypeArg.Proto
        case _: Box => Box.Proto
        case _: Unbox => Unbox.Proto
        case _: UnboxRec => UnboxRec.Proto
        case _: SpawnFuture => SpawnFuture.Proto
        case _: SpawnClosure => SpawnClosure.Proto
        case _: OptionTagGeneric => OptionTagGeneric.Proto
        case _: OptionPayloadGeneric => OptionPayloadGeneric.Proto
        case _: NewNoneOptionGeneric => NewNoneOptionGeneric.Proto
        case _: NewSomeOptionGeneric => NewSomeOptionGeneric.Proto
        case _: AssignGeneric => AssignGeneric.Proto
        case _: InstanceOfGeneric => InstanceOfGeneric.Proto
        case _: AtomicOps.Load => AtomicOps.Load.Proto
        case _: AtomicOps.Store => AtomicOps.Store.Proto
        case _: AtomicOps.CAS => AtomicOps.CAS.Proto
        case _: AtomicOps.Simple => AtomicOps.Simple.Proto

        case _ => n.proto
      }
      write.proto(proto)
    }

    private def writeProtoArgs(n: Node): Unit = {
      n match {
        case n: IConst => write.number(n.value)
        case n: LConst => write.longNumber(n.value)
        case n: FConst => write.floatNumber(n.value)
        case n: DConst => write.doubleNumber(n.value)

        case n: AnyNull =>
          write.tpe(n.tpe)

        case n: AJString =>
          write.xstring(n.str)
          write.bool(n.bstr)

        case n: StackAlloc =>
          writeFrameSlotKind(n.kind)

        case n: SymbolAddress =>
          n.symbol match {
            case method: symlevel.Method =>
              write.number(0)
              write.method(method)
            case td: symlevel.ThinTypeHandleSymbol =>
              write.number(1)
              write.symType(td.tpe)
            case th: symlevel.TypeHandleSymbol =>
              write.number(2)
              write.symType(th.tpe)
            case id: InstanceDescriptorSymbol =>
              write.number(3)
              write.symType(id.tpe)
            case fd: symlevel.FrameDescSymbol =>
              write.number(4)
              write.frameDesc(fd)
            case _ =>
              shouldNotReachHere()
          }

        case n: ImportedIndex =>
          write.symType(n.targetType)

        case n: LightInterfCastCBC =>
          write.symType(n.rcvType)

        case n: RunTimeTypeInfo =>
          write.symType(n.targetType)

        case n: ThisTypeInfo =>
          write.sigType(n.target)

        case n: InstanceDescriptor =>
          write.symType(n.targetType)

        case n: FieldAddr =>
          write.field(n.field)

        case n: VirtualMethodAddr =>
          write.methodRef(n.originalRef)

        case n: GetElementPtr =>
          write.field(n.field)

        case n: MemBarrier =>
          write.number(BarrierKind.toMask(n.kinds.toSeq: _*))

        case n: UniversalGeneric.InvokeMethodWithGenericContext.Target =>
          write.methodRef(n.targetRef)

        case n: UniversalGeneric.CopyResultVST =>
          write.sigType(n.sig)

        case n: UniversalGeneric.CopyUniversalVariable =>
          write.sigType(n.variableType)

        case n: InitStringRecord =>
          write.sigType(n.allocType)
          write.bool(n.isStatic)
          write.constString(n.str)

        case n: MutFunc.Offset =>
          write.sigType(n.recordType)

        case n: MutFunc.Combine =>
          write.tpe(n.tpe)

        case n: Halt =>
          write.reason(n.reason)

        case _ => n.proto match {
          case proto: Product =>
            proto.productIterator foreach writeAny

          case _ =>
        }
      }
    }

    private def writeNodeFields(node: Node): Unit = {
      node match {
        case asc: ArrayStoreCheck =>
          write.bool(asc.hasFastPathInfo)
          if (asc.hasFastPathInfo) {
            write.typeApproximation(asc.arrayTypeForFastPath)
            write.bool(asc.valueHasRelaxedType)
            if (asc.valueHasRelaxedType) {
              write.referenceType(asc.valueRelaxedType)
            }
          }

        case _ =>
      }
    }

    private def nodeId(n: Node): NodeId = n match {
      case `entryBlock`    => ReservedNodeIds.EntryBlock
      case Param(num)      => ReservedNodeIds.getParamId(num)
      case _               => n.id + reservedIdNum
    }

    private def writeNodeArgs(node: Node): Unit = {
      val argsToSerialize = node match {
        case p: LowerPoint  => p.notControlArgs
        case xb: XBlock     => xb.inputs map { _.owner }
        case _              => node.argsSeq
      }
      writeNodeIds(argsToSerialize)
    }

    private def writeNodeId(node: Node): Unit = {
      write.id(nodeId(node))
    }

    private def writeNodeIds(nodes: Iterable[Node]): Unit = {
      // write.iterable & read.seq are compatible
      write.iterable(nodes, writeNodeId)
    }

    private def writeNodePosition(node: Node): Unit = {
      node.pos match {
        case BytecodePosition(bcPos, lineNum, colNum, ic, scope) =>
          assert(lineNum != LineNumber.INVALID)
          write.number(lineNum)
          write.number(bcPos)
          write.number(getInlineContextRef(ic))
          write.number(getLexBlockRef(scope))
          if (LineNumber.isKnown(lineNum)) {
            write.number(colNum)
          }

        case NoPosition =>
          write.number(LineNumber.INVALID)
      }
    }

    private def writeFrameSlotKind(kind: FrameSlot.Kind): Unit = {
      kind match {
        case FrameSlot.Raw(size, alignment) =>
          write.number(0)
          write.number(size)
          write.number(alignment)
        case FrameSlot.NewOnStack(allocType) =>
          write.number(1)
          write.sigType(allocType)
        case FrameSlot.NewArrayOnStack(allocType, length) =>
          write.number(2)
          write.sigType(allocType)
          write.number(length)
        case FrameSlot.Local(allocType, workaround) =>
          write.number(3)
          write.sigType(allocType)
          write.bool(workaround)
        case FrameSlot.DebugVar(allocType, info) =>
          write.number(4)
          write.sigType(allocType)
          writeDebugLocalVar(info)
        case FrameSlot.OffHeapMemory(allocType) =>
          write.number(5)
          write.sigType(allocType)
        case _: FrameSlot.Param => shouldNotReachHere("FrameSlot.Param is not serialized")
      }
    }

    private def writeDebugLocalVar(info: DebugLocalVar): Unit = {
      write.xstring(info.name)
      info.varType.serialize(write.number, write.xstring)
      write.number(info.argIndex)
      write.bool(info.isPointer)
      write.option(info.declaration) { decl =>
        write.xstring(decl.file)
        write.number(decl.line)
        write.number(decl.lbLine)
        write.number(decl.lbCol)
      }
    }

    private def getInlineContextRef(ic: InlineContext): Int = {
      if (ic != null) {
        inlineContexts.getOrElseUpdate(ic, {
          getInlineContextRef(ic.caller)  // Ensure the caller is registered before.
          val index = inlineContextsCount
          inlineContextsCount += 1
          index
        })
      } else {
        NullInlineContextIndex
      }
    }

    private def writeInlineContexts(): Unit = {
      assert (inlineContexts.size == inlineContextsCount)
      write.unsignedNumber(inlineContextsCount)
      write.delimiter()
      for (((ic, ref), index) <- inlineContexts.zipWithIndex) {
        assert (ref == index)

        val caller = ic.caller
        val callerIndex = if (caller != null) inlineContexts(caller) else NullInlineContextIndex
        assert (callerIndex < ref)

        write.method(ic.method)
        write.number(ic.lineNumber)
        write.number(ic.bytecodePos)
        write.number(callerIndex)
        write.delimiter()
      }
    }

    private def getLexBlockRef(lb: LexicalBlock): Int = {
      if (lb != null) {
        lexBlocks.getOrElseUpdate(lb, {
          getLexBlockRef(lb.outer)  // Ensure the outer is registered before.
          val index = lexBlocksCount
          lexBlocksCount += 1
          index
        })
      } else {
        NullLexicalBlockIndex
      }
    }

    private def writeLexBlocks(): Unit = {
      assert (lexBlocks.size == lexBlocksCount)
      write.unsignedNumber(lexBlocksCount)
      write.delimiter()
      for (((lb, ref), index) <- lexBlocks.zipWithIndex) {
        assert (ref == index)

        val outer = lb.outer
        val outerIndex = if (outer != null) lexBlocks(outer) else NullLexicalBlockIndex
        assert (outerIndex < ref)

        write.number(lb.line)
        write.number(lb.column)
        write.number(outerIndex)
        write.delimiter()
      }
    }

    private def writeNode(node: Node): Unit = {
      node match {
        case _: Param                 => return // TODO: should we serialize Convert(Param, IntType)?
        case `entryBlock`             => return
        case phi: Phi                 => assert (phi.block != entryBlock) // No phies in entry block, please
        case If(_: ConstCondition)    => shouldNotReachHere(node) // No const branches, please
        case Switch(_: IConst)        => shouldNotReachHere(node) // No const switches, please
        case _: Branch.Exit           => shouldNotReachHere(node) // no blockEnd results, please
        case _ =>
      }

      writeNodeId(node)
      writeNodePosition(node)
      writeProto(node)
      writeProtoArgs(node)

      node match {
        case _: BBlock => // Arguments are already written in the blocks catalog.
        case x: XBlock => // Arguments are already written in the blocks catalog as control nodes of the corresponding try blocks.

        case _ => writeNodeArgs(node)
      }

      writeNodeFields(node)

      write.delimiter()
    }
  }
}
