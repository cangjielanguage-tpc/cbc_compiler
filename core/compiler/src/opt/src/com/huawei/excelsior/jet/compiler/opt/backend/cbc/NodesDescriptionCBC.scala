/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.{IR1, IR2}
import com.huawei.excelsior.jet.compiler.opt.backend.NodesDescription
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.BackEndCBC
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.{Resource, ResourceSet, emptySet, immSet, invalidSet, setOf}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.VOID
import com.huawei.excelsior.jet.compiler.types.Guards.{CHABitGuard, ConeGuard, LevelGuard, PointGuard}

import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer

@nowarn("msg=match may not be exhaustive")
trait NodesDescriptionCBC extends NodesDescription { self: Universe with BackEndCBC =>

  override protected def nodeClassFormImpl(node: Node): NodeForm = node match {
    case _: MulH | _: UMulH            => nodeClassFormMulH(node)
    case _: TypeTest                   => new CustomForm(Seq(allParamIRegsExceptStdTmp1AndStdTmp2))

    case _: Evacuate                              => new CustomForm(Seq(ir1Set))
    case _: (NewArray | BitcodeDeferred.NewArray) => new CustomForm(Seq(ir2Set))
    case _: CopyStructureCBC                      => new CustomForm(Seq(copyStructureCbcSet, copyStructureCbcSet))
    case _: RecordArrayGet                        => new CustomForm(Seq(copyStructureDependentSet, copyStructureDependentSet))
    case _: MutFunc.Combine                       => new CustomForm(Seq(copyStructureDependentSet, copyStructureDependentSet))

    case _: MutFunc.Offset    => mutFuncArgForm
    case _: MutFunc.OffsetCBC => mutFuncArgForm

    case _: EndLocalUnmovable     => simpleForm // overrides base form

    case _: LoadTypeInfoGeneric => loadTypeInfoGenericForm

    case _: Return if Isa12Mode => simpleForm

    case _ => super.nodeClassFormImpl(node)
  }

  private def nodeClassFormMulH(node: Node): NodeForm = node.tpe match {
    case LongType                      => new CustomForm(Seq(ir7Set, allParamIRegsExceptIR7)) // restricted for the sake of AMD64 implementation simplicity
    case IntType                       => super.nodeClassFormImpl(node)                       // simpleForm
  }

  /** List of nodes and conditions for which compiler can generate bytecodes
    * that allow passing volatile registers for arguments/results.
    * This should be a subset of nodes that are not spoiling volatile registers.
    * See [[MachineDescriptionCBC.extraVolatileRegistersOnAnyExit]].
    */
  protected def canGenerateWithVolatiles(n: Node) = n match {
    case _: (Transfer | ArrayGet | ArrayPut | ArrayIndexCheck | ArrayLength | Add | Sub | Mul | Pow | Neg | LogicalBinaryOp | IDivRemOp
      | DivisorCheck | FDiv | MathIntrinsic | Cmp | TypeTest | Shift | ReinterpretCast | ValueConvert | BitFieldExtract | New
      | BitcodeDeferred.New | NewArray | BitcodeDeferred.NewArray | Evacuate | AbstractNullCheck | SingletonObject | LoadTailParam
      | GetField | FieldChainRead | PutField | FieldChainWrite | ExtractEnrichment | DepriveOperation | EnrichOperation
      | CopyStructure | CopyStructureCBC | Throw | CheckedOp | EndLocalUnmovable
      | MutFuncArgNode | MutFunc.Combine | RecordArrayGet | Return | UniversalGeneric.ConvertHolder | BulldozerHint 
      | LoadTypeInfoGeneric | GenericTypeArg
      | LoadFieldSeq | StoreFieldSeq | GetFieldSeqRef | LoadStaticFieldSeq | StoreStaticFieldSeq | GetStaticFieldSeqRef
      | LoadFieldSeqGeneric | StoreFieldSeqGeneric | GetFieldSeqRefGeneric) => true

    case x: BitcodeDeferred.FieldOp => x.hasObj

    case _: (MulH | UMulH) =>
      // W32 will use CodeEmitter scratch (R10) on AMD64 (to hold the immediate value),
      //     but it's not in the CBC register file at the moment (will be IR7).
      // W64 will use RAX (IR7) & RDX (IR2) on AMD64. While we could be more precise, returning false is simpler.
      n.tpe == IntType

    case _ => false
  }

  override protected val resRegs: Node => ResourceSet = {
    case n if canGenerateWithVolatiles(n) =>
      if (n.isFP) allFRegsSet else allIRegsSet
    case n =>
      if (n.isFP) allFRegsSet &~ volatileFRegsSet else allIRegsSet &~ volatileIRegsSet
  }

  override protected val argRegs: Edge => ResourceSet = {
    case e @ Edge(_, n) if canGenerateWithVolatiles(n) =>
      if (e.source.isFP) allFRegsSet else allParamIRegsSet
    case e =>
      if (e.source.isFP) allFRegsSet &~ volatileFRegsSet else allParamIRegsSet &~ volatileIRegsSet
  }

  //// Call / Return

  protected override def indirectCallTargetSet(call: Call): ResourceSet = {
    // Consider foreign call with record parameter passed by value. We limit head registers to the first ones before
    // this record parameter (see ABICBC.initLocations), and the following head registers are not used to pass params
    // (in CBC ABI). But we could not use them for indirect call target either, because when ABI CBC will be converted
    // to native ABI (in Lowering JIT), these head parameters will be used.
    val excludedIRegs = if (call.abi.hasTail) call.abi.allArgumentIRegs.toSet else call.abi.usedArgumentIRegs

    allParamIRegsSet -- excludedIRegs
  }

  private def copyStructureCbcArg(x: CopyStructureCBC) =
    if (x.hasComplexDst && x.hasComplexSrc) { // if both dst and src are complex we use IR1 and IR2 as temporals
      allParamIRegsSet &~ stdTmp1StdTmp2Set
    } else if (x.hasComplexDst || x.hasComplexSrc) { // if any of dst and src are complex we use IR1 as temporal
      allParamIRegsSet &~ ir1Set
    } else {
      allParamIRegsSet
    }

  private lazy val copyStructureCbcSet: Edge => ResourceSet = { e =>
    copyStructureCbcArg(e.target.asInstanceOf[CopyStructureCBC])
  }

  private lazy val copyStructureDependentSet: Edge => ResourceSet = { e => e.target.groupRoot match {
    case x: CopyStructureCBC => copyStructureCbcArg(x)
    case _ => allParamIRegsSet
  }}

  private lazy val mutFuncArgForm: NodeForm = new NodeForm {
    override protected def argumentRegisters(e: Edge): ResourceSet = {
      val call = e.target.groupRoot.asInstanceOf[Call]
      e match {
        case Edge(_: MutFunc.Host, _: MutFunc.Offset) =>
          immSet
        case Edge(_, _: MutFunc.Offset) =>
          callParamSet(call, call.methodType.getMutRecordArgIdx, e) // offset (= record address) arg
        case Edge(_, _: MutFunc.OffsetCBC) =>
          callParamSet(call, call.methodType.getMutObjectArgIdx, e) // object arg
      }
    }
  }

  private lazy val loadTypeInfoGenericForm: NodeForm = new NodeForm {
    override protected def argumentRegisters(e: Edge): ResourceSet = {
      // 0th argument is inCtrl, so the rest of arguments are mapped to IR one-to-one
      setOf(IR.fromOrdinal(e.targetArgIndex))
    }
  }
}
