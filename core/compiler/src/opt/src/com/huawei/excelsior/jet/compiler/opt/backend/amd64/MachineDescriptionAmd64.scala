/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.amd64

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.codeemitter.BarrierKind.STORE_LOAD
import com.huawei.excelsior.jet.codeemitter.amd64.CodeEmitterAmd64
import com.huawei.excelsior.jet.compiler.Env.addressSize
import com.huawei.excelsior.jet.compiler.opt.backend.MachineDescription
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.{Resource, ResourceSet, emptySet, invalidSet, setOf}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

import scala.PartialFunction.cond
import scala.annotation.nowarn

@nowarn("msg=match may not be exhaustive")
trait MachineDescriptionAmd64 extends MachineDescription { self: Universe with BackEndAmd64 =>

  import ExitKind.*
  import RegFile.*


  /////////////////////////////////////////////////////////////////////////////

  override def mayImmediateBeMovedToMemoryDirectly(source: Node, isStack: Boolean): Boolean = source match {
    case DWordConst(_) | _: FConst | _: AnyNull => true
    case _ => false
  }


  /////////////////////////////////////////////////////////////////////////////

  override def mayBeUsedAsImmediate(use: Edge): Boolean = {
    use.source.isInstanceOf[Constant] && cond((use.target, indexInValueArgs(use))) {
      case (_: Lea, 0) => cond(use.source) {
        case _: StackAlloc => true
      }

      case (_: RawMemoryAccess, 0) => cond(use.source) {
        case _: AddrConst | DWordConst(_) | _: StackAlloc => true
      }

      case (_: Cmp, 1) => cond(use.source) {
        case DWordConst(_) | _: FConst | _: DConst | _: AnyNull => true
      }

      case (_: Test, 1) => cond(use.source) {
        case DWordConst(_) => true
      }

      case (MathIntrinsic(kind), _) => true ensuring { kind.isBinary }

      case (_: Shift, 1) => cond(use.source) {
        case _: IConst => true
      }

      case (_: Add | _: Sub | _: And | _: Or | _: Xor | _: FDiv | _: Mul, 1) => cond(use.source) {
        case DWordConst(_) | _: FConst | _: DConst => true
      }

      case (_: Enrich, 1) => true

      case (ai: MemAtomic, 1) =>
        import MemAtomic.Kind.*
        cond((ai.kind, use.source)) {
          case (AND | OR | XOR, DWordConst(_)) => true
          case (ADD, DWordConst(_)) if !ai.hasValueUses => true
        }

      case _ => super.mayBeUsedAsImmediate(use)
    }
  }


  /////////////////////////////////////////////////////////////////////////////

  override def boundEdges(node: Node): Seq[Edge] = {
    import Java.Lang.MathIntrinsic.*
    import MemAtomic.Kind.*

    node match {
      case Mul(_, DWordConst(_)) =>
        Seq.empty

      case _: Mul | _: And | _: Or | _: Xor =>
        Seq(node.inEdge(0), node.inEdge(1))

      case _: Add if node.isFP =>
        Seq(node.inEdge(0), node.inEdge(1))

      case _: Neg | _: Sub | _: FDiv | _: Shift =>
        Seq(node.inEdge(0))

      case MathIntrinsic(D_ABS | F_ABS) =>
        Seq(node.inEdge(0))

      case cas: CAS =>
        Seq(node.inEdge(CAS.ExpectedValueEdge.index))

      case ma @ MemAtomic(ADD | SWAP, _) if node.hasValueUses =>
        Seq(node.inEdge(MemAtomic.ValueEdge.index))

      case _: CheckedOp =>
        Seq(node.inEdge(CheckedOp.FirstValueArg.index))

      case _ => super.boundEdges(node)
    }
  }


  /////////////////////////////////////////////////////////////////////////////

  override def argumentShouldBeSaved(edge: Edge): Boolean = (edge.target, indexInValueArgs(edge)) match {
    case (IDiv() | UDiv(), 0) => true

    case (_: MulH | _: UMulH | IRem() | URem(), 0) => true

    case (cas: CmpCAS, 1) => true

    case _ => super.argumentShouldBeSaved(edge)
  }


  /////////////////////////////////////////////////////////////////////////////

  def memoryAccessCanBeGroupedWithLea(rma: RawMemoryAccess, lea: Lea): Boolean = rma match {
    case _: LoadMemory | _: StoreMemory | _: Prefetch | _: CAS | _: MemAtomic => true
  }

  def isAccessTypeConformsLea(tpe: AsmType, lea: Lea): Boolean = true


  /////////////////////////////////////////////////////////////////////////////

  object RepeatedMoveAmd64 {
    val dstTmp = RDI
    val sizeTmp = RCX

    /** ArrayFill, MemZeroing & etc. are generated with repeated move if they move more than this number of bytes.
     *
     * Unrolled move takes: 2 code bytes per 8 bytes of data.
     * Repeated move takes: 8 code bytes and 1 extra register for any data size.
     *
     * So equal amount of code is generated for 32 bytes of data, but unrolled version requires no extra register.
     *
     * @see CodeGeneratorImplAmd64#genRepeatedStringMovs
     */
    private val maxSizeInBytesForUnroll: Int = 32

    def generateUnrolled(size: Int): Boolean = size <= maxSizeInBytesForUnroll
  }

  object ArrayFillAmd64 {
    val srcTmp = RSI

    def generateUnrolled(af: ArrayFill): Boolean =
      RepeatedMoveAmd64.generateUnrolled(af.totalBytes)
  }

  object StackZeroingAmd64 {
    val srcTmp = RAX

    def generateUnrolled(sz: StackZeroing): Boolean =
      sz.isSizeAndSlotDefined && RepeatedMoveAmd64.generateUnrolled(sz.size)
  }

  override protected def volatileRegistersOnAnyExit(node: Node, file: RegFile): ResourceSet = {
    import Group.AttachReason.*
    import Java.Lang.MathIntrinsic.*

    (node, file) match {
      case (IDiv() | UDiv(), IREG) =>
        rdxSet

      case (CheckedOp(CheckedOp.Kind.MUL, _, _), IREG) =>
        // TODO: consider to exclude RDX spoil for W8 mul (Intel spec, volume 2, IMUL-Signed Multiply).
        rdxSet

      case (_: MulH | _: UMulH | IRem() | URem(), IREG) =>
        raxSet

      case (MathIntrinsic(D_SIN | D_COS | D_TAN | D_REM1 | D_REM | F_REM), IREG) =>
        raxSet

      case (af: ArrayFill, IREG) =>
        import ArrayFillAmd64.*
        import RepeatedMoveAmd64.*
        if (ArrayFillAmd64.generateUnrolled(af)) setOf(dstTmp, srcTmp) else setOf(dstTmp, srcTmp, sizeTmp)

      case (sz: StackZeroing, IREG) =>
        import RepeatedMoveAmd64.*
        import StackZeroingAmd64.*
        if (StackZeroingAmd64.generateUnrolled(sz)) setOf(dstTmp, srcTmp) else setOf(dstTmp, srcTmp, sizeTmp)

      case (iff: If, IREG) if iff.hasAttachedByReason(COND_BRANCH_ARG_CAS) =>
        raxSet

      case (_: Throw, IREG) =>
        raxSet

      case _ => super.volatileRegistersOnAnyExit(node, file)
    }
  }

  override protected def volatileSlotsOnAnyExit(node: Node): ResourceSet = node match {
    case call: Call if call.abi.shadowSpaceSize > 0 =>
      val shadowSlots = (0 until (call.abi.shadowSpaceSize / addressSize)).map(i => slotForArg(AddrType, i * addressSize))
      super.volatileSlotsOnAnyExit(node) ++ shadowSlots

    case _ => super.volatileSlotsOnAnyExit(node)
  }


  /////////////////////////////////////////////////////////////////////////////

  override protected def temporalRegistersAmount(node: Node, file: RegFile): Int = (node, file) match {
    case (_: GCPoint | _: TrapCheck, IREG) => 1

    case (ValueConvert(AsmType.F32, AsmType.F16, _), FREG) => 1

    case _ => super.temporalRegistersAmount(node, file)
  }


  /////////////////////////////////////////////////////////////////////////////

  override def implicitCheckVolatileResources(check: PureCheck, exit: ExitKind): ResourceSet = check match {
    case _: DivisorCheck =>
      // `genIntegralDivRem` spoils RDX register before `div` instruction
      rdxSet

    case _ => emptySet
  }


  /////////////////////////////////////////////////////////////////////////////

  override protected def resultResourcesSetImpl(node: Node): ResourceSet = {
    import MemAtomic.Kind.*

    node match {
      case IDiv() | UDiv() =>
        raxSet

      case _: MulH | _: UMulH | IRem() | URem() =>
        rdxSet

      case MemAtomic(AND | OR | XOR | ADD | SWAP, _) =>
        // This is a complicated case. [[MemAtomic]] AND|OR|XOR nodes with value uses are lowered on amd64, and
        // ADD|SWAP with value uses are bound. For bound nodes this function should not be called, BUT after
        // refactoring of results/volatiles we will call this function to exclude singleton results from volatiles.
        // Also it will be called for unbound nodes, but as they do not have value uses, we should not allocate
        // them. In both cases it is simplest way to implement it here with invalidSet.
        // TODO: review all bound support in backend
        invalidSet

      case _ => super.resultResourcesSetImpl(node)
    }
  }

  override def noCodeShouldBeGenerated(node: Node): Boolean = node match {
    case memBarrier: MemBarrier =>
      !memBarrier.kinds.contains(STORE_LOAD)

    case BitFieldExtract.ZeroExtend.From32To64(arg) =>
      arg.resource == node.resource

    case _ =>
      super.noCodeShouldBeGenerated(node)
  }


  /////////////////////////////////////////////////////////////////////////////

  // TODO: actually, FConst|DConst rematerialization requires movss|movsd instructions
  override def zeroCostRematerialization(node: Node): Boolean = super.zeroCostRematerialization(node)

  override def implicitDivisorCheckAllowed: Boolean = true
}
