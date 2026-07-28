/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Width.W32
import com.huawei.excelsior.jet.assembler.cbc.Assembler.normalizeImm
import com.huawei.excelsior.jet.assembler.cbc.{Assembler, FieldReference}
import com.huawei.excelsior.jet.assembler.{AsmType, Symbol, Width}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import com.huawei.excelsior.jet.compiler.Env.{addressSize, isStandalone}
import com.huawei.excelsior.jet.compiler.NotImplementedFeature.CBC
import com.huawei.excelsior.jet.compiler.opt.backend.MachineDescription
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.{FrameSlot, Resource, ResourceSet, emptySet}
import com.huawei.excelsior.jet.compiler.opt.ir.Tag.VALUE
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.ir.nodes.ObjectOperationNodes
import com.huawei.excelsior.jet.compiler.options.BoolOption.UseIsa12
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{ArraySlice, Record, VArray}
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeFieldReference, Field, PermanentMember, SignatureType}
import com.huawei.excelsior.jet.util.ScalaCollections
import xscala.util.MathUtils.*

import scala.PartialFunction.cond
import scala.annotation.{nowarn, tailrec}

@nowarn("msg=match may not be exhaustive")
trait MachineDescriptionCBC extends MachineDescription { self: Universe with BackEndCBC =>

  import ExitKind.*
  import RegFile.*


  /////////////////////////////////////////////////////////////////////////////

  override def mayImmediateBeMovedToMemoryDirectly(source: Node, isStack: Boolean): Boolean = source match {
    case _: StackAlloc | _: ConstString | _: AJString | _: AnyNull => false
    case DWordConst(_) => isStack // Only W32-fitted values could be stored directly, due to Intel limitations
    case _ => false
  }

  override def temporaryResourcesForTransfer(to: ResourceKind, from: ResourceKind, source: Node): Option[ResourceSet] = (to, from, source) match {
    case (_: RegResourceKind, ImmResourceKind, _: ConstString | _: AJString) =>
      None

    case _ => super.temporaryResourcesForTransfer(to, from, source)
  }

  override def temporaryResourcesForIntermediateCopy(source: Node): Option[ResourceSet] = source match {
    case _: StackAlloc | _: ConstString | _: AJString => Some(allIRegsSet)

    case _ => super.temporaryResourcesForIntermediateCopy(source)
  }

  /////////////////////////////////////////////////////////////////////////////

  private object NumericalConstRawW64 {
    def unapply(n: Node): Option[Long] = n match {
      case ULConst(c) => Some(c)
      case FConst(c) => Some(zeroExtend(java.lang.Float.floatToRawIntBits(c)))
      case DConst(c) => Some(java.lang.Double.doubleToRawLongBits(c))
      case _ => None
    }
  }

  /** Checks if Lowering JIT code emitter can store immediate value without additional scratches (on Amd64). */
  private def canStoreImm(memType: AsmType, imm: Long) = {
    val bitSize = if (memType == AsmType.PTR) addressSize * 8 else memType.width.nbits
    val unsignedImm = bits(imm, 0, bitSize - 1)
    val adjustedImm = if (memType.signed) signExtend(unsignedImm, bitSize) else unsignedImm
    // TODO: enable storing immediates in standalone mode
    !isStandalone && (bitSize <= 32 || isNBitsSigned(adjustedImm, 32))
  }

  private def fieldType(f: FieldReference): SignatureType = f.field match {
    case PermanentMember(f: Field) => f.getType
    case b: BitcodeFieldReference => b.fieldType
  }

  private def isRecordConstructor(n: Node) = cond(n) {
    case call: Call => call.targetRef.hasMethod && call.targetRef.method.isRecordConstructor
  }

  override protected def shouldBeUsedAsImmediate(use: Edge): Boolean = use match {
      case IDivRemOp.DivisorEdge(op) =>
        cond(use.source) {
          case IntegralConst(c) =>
            assert(c != 1 && c != -1 && c != 0) // Must be handled before.
            op.isDiv && Isa12Mode
        }

      case Edge(_: StackAlloc, offs: MutFunc.Offset) if isRecordConstructor(offs.groupRoot) => true

      case Edge(_: StackAlloc, _: (Box | Unbox)) => true

      case _ => super.shouldBeUsedAsImmediate(use)
    }

  override def mayBeUsedAsImmediate(use: Edge): Boolean = {
    use.source.isInstanceOf[Constant] && ((use, use.source) match {
      case (Cmp.SecondArg(cmp), _: AnyNull | IntegralConst(_)) => true

      case (CheckedOp.SecondtValueArg(cop), IntegralConst(_)) => cop.kind != CheckedOp.Kind.DIV

      case (Add.SecondArg(_),  FloatingPointConst(_)) => Isa12Mode && !isStandalone
      case (Sub.SecondArg(_),  FloatingPointConst(_)) => Isa12Mode && !isStandalone
      case (Mul.SecondArg(_),  FloatingPointConst(_)) => Isa12Mode && !isStandalone
      case (FDiv.SecondArg(_), FloatingPointConst(_)) => Isa12Mode && !isStandalone

      case (Add.SecondArg(_), IntegralConst(_)) => true
      case (Sub.SecondArg(_), IntegralConst(_)) => true
      case (Mul.SecondArg(_), IntegralConst(_)) => true
      case (MulH.SecondArg(_), IntegralConst(_)) => true
      case (UMulH.SecondArg(_), IntegralConst(_)) => true
      case (LogicalBinaryOp.SecondArg(_), IntegralConst(_)) => true
      case (Shift.NumEdge(_), IConst(_)) => true

      case (Enrich.EnrichmentEdge(_), IntegralConst(_)) => true

      case (InvokeInterfaceTarget.CIAOEdge(_), IntegralConst(c)) if isNBits(c, 16) => true

      // TODO-ISA12: enable back when `newarfill.const` is supported
      //case (NewArrayFill.ValueEdge(_), NumericalConst(_)) => true

      case (Edge(sa: StackAlloc, access: LoadStoreMemoryAccess), _) if use == access.addrEdge =>
        sa.kind match {
          case FrameSlot.Typed(_: VArray) =>
            // TODO: JET-16640: leave only second `case` of this `match` when `VArray{Get,Set}` nodes are introduced,
            //                  as then we no longer would use raw memory access with typed cbc slots.
            false
          case FrameSlot.Typed(allocType) =>
            assert(!allocType.isRecord)
            true
        }

      case (Edge(_, FieldChainWrite(_: RecordArrayGet, _, _)), _) => false // TODO: support movi for ISA12

      case (Edge(sa: StackAlloc, _: InitStringRecord), _) => typedFrameSlot(sa.kind).nonEmpty
      case (Edge(sa: StackAlloc, gf: (GetField | FieldChainRead)), _) if gf.obj == sa => typedFrameSlot(sa.kind).nonEmpty
      case (Edge(sa: StackAlloc, pf: (PutField | FieldChainWrite)), _) if pf.obj == sa => typedFrameSlot(sa.kind).nonEmpty
      case (Edge(NumericalConstRawW64(c), pf: PutField), _) => canStoreImm(pf.field.getType.toAsm, c)
      case (Edge(NumericalConstRawW64(c), fw: FieldChainWrite), _) => canStoreImm(fieldType(fw.fields.last).toAsm, c)
      case (Edge(_: AnyNull, _: PutField | _: FieldChainWrite), _) => true
      case (Edge(sa: StackAlloc, cs: CopyStructureCBC), _) if cs.src == sa || cs.dst == sa => typedFrameSlot(sa.kind).nonEmpty

      case (Edge(sa: StackAlloc, ohm: UniversalGeneric.OffHeapMemorySlotPointer), _) if ohm.ohms == sa => true
      case (Edge(sa: StackAlloc, n: UniversalGeneric.GetFieldOHM), _) if n.ohms == sa => true
      case (Edge(sa: StackAlloc, n: UniversalGeneric.CopyUniversalVariable), _) if n.dst == sa => !isStandalone

      case _ => super.mayBeUsedAsImmediate(use)
    })
  }

  /////////////////////////////////////////////////////////////////////////////

  override def boundEdges(node: Node) = node match {
    //TODO-CBC:
    //  Non-commutative operations' generation if right arg == dst cannot be done effectively on AMD64,
    //  so we need to prohibit this configurations. Current code is workaround and
    //  will be removed in favor of mov+op combining.
    case _: FDiv | _: Sub if node.tpe.isFloatingPointType => Seq(node.inEdge(0))
    case _: IDivRemOp => Seq(node.inEdge(IDivRemOp.DividendEdge.index))
    case CheckedOp(CheckedOp.Kind.DIV | CheckedOp.Kind.SUB, _, _) => Seq(node.inEdge(CheckedOp.FirstValueArg.index))

    case _ => super.boundEdges(node)
  }

  def memoryAccessCanBeGroupedWithLea(rma: RawMemoryAccess, lea: Lea): Boolean = false

  def isAccessTypeConformsLea(tpe: AsmType, lea: Lea): Boolean = notImplemented(CBC, "MachineDescriptionCBC.isAccessTypeConformsLea")


  /////////////////////////////////////////////////////////////////////////////

  override protected def volatileRegistersOnAnyExit(node: Node, file: RegFile): ResourceSet = (node match {
    case cp: CopyStructureCBC if file == IREG => {
      if      (cp.hasComplexDst && cp.hasComplexSrc) stdTmp1StdTmp2Set
      else if (cp.hasComplexDst || cp.hasComplexSrc) ir1Set else emptySet
    }

    case isr: InitStringRecord if !Isa12Mode && isr.isStatic && file == IREG => ir1Set

    case If(_: TypeTest) =>
      file match { // TypeTest and If are grouped
        case IREG => stdTmp1StdTmp2Set // We need 2 temporal registers in worst case scenario to perform type check
        case FREG => emptySet
      }

    case _: StackZeroing =>
      file match {
        case IREG => ir7Set
        case FREG => emptySet
      }

    case _: Clinit | _: PackageInit | _: PackageInitCheck =>
      file match {
        case IREG => ir1Set
        case FREG => emptySet
      }

    case _: (New | BitcodeDeferred.New | NewArray | BitcodeDeferred.NewArray) =>
      volatileSet(file)

    // Non-orthogonal ISA, spoils RAX (IR7) on AMD64, while result can be on any register
    case _: MulH | _: UMulH if node.tpe == LongType =>
      file match {
        case IREG => ir7Set
        case FREG => emptySet
      }

    case _ => super.volatileRegistersOnAnyExit(node, file)

  }) | extraVolatileRegistersOnAnyExit(node, file)

  /** Spoil volatile registers on all nodes except trusted ones,
    * that are free from use of temporals in Lowering JIT
    */
  private def extraVolatileRegistersOnAnyExit(node: Node, file: RegFile): ResourceSet = {
    val blockEndSpoiledSet = node match {
      case _: BlockEnd if file == IREG && node.hasAttachedByReason(Group.AttachReason.INSTANCE_OF_BRANCH) => ir7Set
      case _ => emptySet
    }
    val volatileRegs = if (freeOfTemporals(node)) emptySet else volatileSet(file)
    volatileRegs | blockEndSpoiledSet
  }

  override def noCodeShouldBeGenerated(node: Node): Boolean = node match {
    case _: MemBarrier => true // FIXME JET-14017: decide how to generate mem-barriers
    case _: EndLocalUnmovable => false
    case _ => super.noCodeShouldBeGenerated(node)
  }

  protected def freeOfTemporals(node: Node) = node match {
    case _ if noCodeShouldBeGenerated(node) => true

    case _: (BlockEnd | CheckedOp | ArrayGet | ArrayPut | ArrayIndexCheck | ArrayLength | Transfer | FieldChainRead
      | Add | Sub | IDivRemOp | Mul | Cmp | CondVal | FDiv | MathIntrinsic | LogicalBinaryOp | GetField | PutField
      | BitcodeDeferred.FieldOp | Shift | GetStatic | PutStatic | ValueConvert | ReinterpretCast | LoadTailParam
      | New | BitcodeDeferred.New | NewArray | BitcodeDeferred.NewArray | DivisorCheck | Evacuate | BitFieldExtract
      | Clinit | PackageInit | PackageInitCheck | GCPoint | AbstractNullCheck | SingletonObject
      | DepriveOperation | EnrichOperation | EnrichCBC | ExtractEnrichment | FieldChainWrite | Neg | MutFunc.Combine
      | CopyStructure | CopyStructureCBC | Throw | InterfaceCastCBC | CatchCBC | EndLocalUnmovable | DebugBreakpoint
      | LoadMemory | StoreMemory | InitStringRecord | ThisTypeInfoCBC | ThisTypeInfoByCBC
      | LoadFieldSeq | LoadStaticFieldSeq | StoreFieldSeq | StoreStaticFieldSeq | GetFieldSeqRef | GetStaticFieldSeqRef
      | DerivedPtr | LoadTypeInfo | Box | Unbox | SpawnFuture) => true

    case _: (TypeTest | CallTarget | MutFuncArgNode | RecordArrayGet) => true // always grouped with another node

    case AnyInstanceOf(tpe, _) => true

    case _: MulH | _: UMulH => node.tpe == IntType

    case _: (NewArrayFill | ArrayStoreCheck | ArrayFill
      | CheckCast | BitcodeDeferred.CheckCast
      | CoverageCounter | Call | ZeroRefs
      | UniversalGeneric
      | CFuncWrapperAddr | FieldAddr | InitObj | StackZeroing) => false
  }


  /////////////////////////////////////////////////////////////////////////////

  override def implicitCheckVolatileResources(check: PureCheck, exit: ExitKind): ResourceSet = (check, exit) match {
    case _ => emptySet  // TODO-CBC
  }


  /////////////////////////////////////////////////////////////////////////////

  override protected def resultResourcesSetImpl(node: Node): ResourceSet = node match {
    case x: Cmp => shouldNotReachHere(s"All Cmp are either part of branchIf or setIf: $x ${x.singleValueUse} ${x.groupRoot} ${if (x.hasGroup) x.group.attached.toSeq else Seq.empty}")
    case x: TypeTest => shouldNotReachHere(s"All type tests must be part of BTT: $x ${x.singleValueUse} ${x.groupRoot}}")

    case _: MulH | _: UMulH =>
      // AMD64 W32 & ARM64 are very flexible. AMD64 W64 has result on RDX (IR2), so we will generate mov
      resRegs(node)

    case _: InterfaceCastCBC | AnyInstanceOf(_, _) | _: MathIntrinsic | _: ArithCommutativeOp | _: Sub | _: LogicalBinaryOp |
         _: FDiv | _: Shift | _: CondVal | _: SingletonObject | _: ReinterpretCast | _: ValueConvert |
         _: BitFieldExtract | _: ArrayLength | _: GetField | _: FieldChainRead | _: GetStatic | _: CatchCBC |
         _: DepriveOperation | _: EnrichOperation | _: ExtractEnrichment | _: Neg | _: CheckedOp |
         _: ArrayGet | _: FieldAddr | _: CheckCast | _: BitcodeDeferred.CheckCast |
         _: CFuncWrapperAddr | _: LoadMemory | _: MutFunc.Combine | _: RecordArrayGet =>
      resRegs(node)

    case node: BitcodeDeferred.FieldOp if !node.hasInValue =>
      resRegs(node)

    case _: (New | BitcodeDeferred.New | NewArray | BitcodeDeferred.NewArray | Evacuate | UniversalGeneric.CopyResultVST | SpawnFuture) => ir1Set

    case _ => super.resultResourcesSetImpl(node)
  }


  /////////////////////////////////////////////////////////////////////////////

  override def implicitDivisorCheckAllowed: Boolean = true

  override def lowLevelMemoryOperationsAddressesCombiningInLeaHasImpact: Boolean = false

  override def arithOperationsCombiningInLeaHasImpact: Boolean = false
}
