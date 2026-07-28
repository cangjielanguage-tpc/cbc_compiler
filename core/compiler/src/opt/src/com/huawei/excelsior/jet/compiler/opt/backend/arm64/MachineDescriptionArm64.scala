/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.arm64

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.arm64.Assembler
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.*
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.opt.backend.MachineDescription
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.{Resource, ResourceSet, emptySet, setOf}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.Arm64CASBackupPath

import scala.PartialFunction.cond
import scala.annotation.nowarn

@nowarn("msg=match may not be exhaustive")
trait MachineDescriptionArm64 extends MachineDescription { self: Universe with BackEndArm64 =>

  import Condition.*
  import ExitKind.*
  import RegFile.*


  /////////////////////////////////////////////////////////////////////////////

  override def mayImmediateBeMovedToMemoryDirectly(source: Node, isStack: Boolean): Boolean = source match {
    case ZeroValueNode() => true
    case _ => false
  }


  /////////////////////////////////////////////////////////////////////////////

  override def mayBeUsedAsImmediate(use: Edge): Boolean = {
    use.source.isInstanceOf[Constant] && cond((use.target, indexInValueArgs(use))) {
      // TODO: arguments of grouped arguments

      case (_: Add | _: Sub | _: Cmp, 1) => cond(use.source) {
        case ShiftedImm12Node(_) => true
      }

      case (_: And | _: Or | _: Xor | _: Test, 1) => cond(use.source) {
        case BitMaskNode(_) => true
      }

      case (_: Shift, 1) => true

      case _ => super.mayBeUsedAsImmediate(use)
    }
  }


  /////////////////////////////////////////////////////////////////////////////

  override def boundEdges(node: Node): Seq[Edge] = node match {
    case _: CAS => Seq(node.inEdge(CAS.ExpectedValueEdge.index))

    case _ => super.boundEdges(node)
  }


  /////////////////////////////////////////////////////////////////////////////

  protected def isDisplacementAllowed(tpe: AsmType, disp: Int): Boolean =
    Assembler.isValidImmForLdrOrStr(disp, tpe.width)

  protected def isScaleAllowed(tpe: AsmType, scale: Int): Boolean = {
    (scale == 1) || (scale == tpe.sizeInBytes)
  }

  def memoryAccessCanBeGroupedWithLea(rma: RawMemoryAccess, lea: Lea): Boolean = rma match {
    case _: Prefetch | _: LoadMemory | _: StoreMemory => isAccessTypeConformsLea(rma.accessType, lea)

    case _: CAS | _: MemAtomic => false
  }

  def isAccessTypeConformsLea(tpe: AsmType, lea: Lea): Boolean = lea match {
    case Lea.Base(_, disp) => isDisplacementAllowed(tpe, disp)
    case Lea.Baseless(_, _, _) => shouldNotReachHere("in current implementation, baseless Lea can not be used in RMA directly")
    case Lea.Scaled(_, _, scale, 0) => isScaleAllowed(tpe, scale)
    case _ => false
  }


  /////////////////////////////////////////////////////////////////////////////

  override protected def volatileRegistersOnAnyExit(node: Node, file: RegFile): ResourceSet = (node, file) match {
    case (_: GCPoint | _: TrapCheck, IREG) =>
      // GCPoint requires register to load trap page address. We use fixed IP0 for this,
      // because runtime will use it anyway on trap exit.
      //
      // We also spoil IP1 and LR, because GCPoint handler can be a RTCall and therefore there should be no references
      // on IP0, IP1 and LR during GCPoint as it should look completely like a call.
      setOf(IP0, IP1, LR)

    case (_: CAS, IREG) if env.enabled(Arm64CASBackupPath) =>
      stdTmpSet

    case (_: Throw, IREG) =>
      stdTmpSet

    case _ => super.volatileRegistersOnAnyExit(node, file)
  }

  override protected def extraVolatileRegistersOnTrapExit(node: Node, file: RegFile): ResourceSet = (node, file) match {
    case (_: DivisorCheck, IREG) =>
      setOf(IP0, IP1, LR)

    case _ => super.extraVolatileRegistersOnTrapExit(node, file)
  }


  /////////////////////////////////////////////////////////////////////////////

  override protected def temporalRegistersAmount(node: Node, file: RegFile): Int = (node, file) match {
    case (_: StackZeroing, IREG) => 2

    case (_: ArrayFill, IREG) => 4

    case (_: BitCount, FREG) => 1

    case (ValueConvert(AsmType.F32 | AsmType.F64, AsmType.F16, _), FREG) => 1

    case _ => super.temporalRegistersAmount(node, file)
  }


  /////////////////////////////////////////////////////////////////////////////

  override def implicitCheckVolatileResources(check: PureCheck, exit: ExitKind): ResourceSet = (check, exit) match {
    case (_: AbstractNullCheck, NORMAL) =>
      emptySet

    case (_: AbstractNullCheck, _) =>
      // Runtime uses IP0 to jump on handler
      // TODO: remove spoiled register when exception handling will not use IP0
      ip0Set

    case _ => emptySet
  }


  /////////////////////////////////////////////////////////////////////////////

  override protected def resultResourcesSetImpl(node: Node): ResourceSet = node match {
    case _: MemAtomic if env.enabled(Arm64CASBackupPath) =>
      stdTmpSet

    case _ => super.resultResourcesSetImpl(node)
  }

  override def noCodeShouldBeGenerated(node: Node): Boolean = node match {
    case BitFieldExtract.ZeroExtend.From32To64(arg) =>
      arg.resource == node.resource

    case _ =>
      super.noCodeShouldBeGenerated(node)
  }


  /////////////////////////////////////////////////////////////////////////////

  // TODO-ARM64: reconsider this predicate using benches
  override def zeroCostRematerialization(node: Node): Boolean = node match {
    case _: AddrConst => false // ldr literal
    case _ => super.zeroCostRematerialization(node) // TODO: actually, FConst|DConst rematerialization often requires literal
  }

  override def implicitDivisorCheckAllowed: Boolean = false
}
