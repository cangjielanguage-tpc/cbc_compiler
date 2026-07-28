/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering.arm64

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.codeemitter.BarrierKind.STORE_LOAD
import com.huawei.excelsior.jet.compiler.StatsKind.MSubPattern
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.lowering.Toolbox
import com.huawei.excelsior.jet.compiler.opt.lowering.arch64.LoweringArch64
import com.huawei.excelsior.jet.compiler.opt.middle.Optimize
import com.huawei.excelsior.jet.compiler.{RTConst, symlevel}
import xscala.util.MathUtils.rightNBits64

trait LoweringArm64 extends LoweringArch64 with Toolbox { self: Universe with Optimize =>

  import LoweringKind._

  override def MaxArrayFillSizeForSplitting: Int = fixMeArm64(12) // FIXME-ARM64: reimplement ArrayFill and tune this value

  private[lowering] def storeLoadForCellBarrier(obj: Node): Unit =
    fixMeArm64(MemBarrier(Set(STORE_LOAD))())

  override def shouldBeLoweredCases(node: Node): LoweringKind = node match {
    case op: IDivRemOp if !op.isDiv => FLOATING
    case op @ CheckedOp(CheckedOp.Kind.SUB | CheckedOp.Kind.ADD, _, _) if op.width < Width.W32 => COMPLEX
    case CheckedOp(CheckedOp.Kind.MUL, _, _) => COMPLEX
    case _ => super.shouldBeLoweredCases(node)
  }

  override def decomposeNode(node: Node): Node = node match {
    case x: IDivRemOp => lowerIntegralRem(x)
    case op @ CheckedOp(CheckedOp.Kind.MUL, _, _) if op.width == Width.W8 || op.width == Width.W16 => lowerShortCheckedMul(op)
    case _ => super.decomposeNode(node)
  }

  override def genExtractObject(rich: Node, bits: Node, enrichment: Node): Node = {
    PublishRef(And(bits, addrConst(~enrichmentMask)))
  }

  override def genMakeCIAO(itype: symlevel.Type, plain: Node, enrichment: Node): Node = {
    enrichment
  }

  private def lowerShortCheckedMul(n: CheckedOp): Node = {
    assert(n.width == Width.W16 || n.width == Width.W8)

    val tpe = n.tpe
    val signed = n.signed
    val width = n.width.nbits

    def normalize(x: Node): Node =
      BitFieldExtract.BFX(tpe, 0, n.width.nbits, signExtension = signed, x)

    val (l, r) = (normalize(n.l), normalize(n.r))
    val res = Mul(l, r)

    val check = if (n.signed) {
      If(Cmp(tpe, Condition.NE)(ShiftByConst(tpe, ArithOp.ASR, width, res), ShiftByConst(tpe, ArithOp.ASR, width - 1, res)))
    } else {
      If(Cmp(tpe, Condition.NE)(ShiftByConst(tpe, ArithOp.LSR, width, res), IntegralConst(tpe)(0)))
    }
    coldBlockWithErrorRTSCallAndHalt(check.trueExit)(n, n.throwProc)
    continue(check.falseExit)
    res
  }

  private def lowerIntegralRem(op: IDivRemOp): Node = {
    assert(!op.isDiv)
    val quotient = if (op.isUnsigned) {
      UDiv(op.tpe)(op.l, op.r)
    } else {
      IDiv(op.tpe)(op.l, op.r)
    }
    stats.count(MSubPattern, "MSub pattern in rem")
    MSub(op.tpe)(op.r, quotient, op.l)
  }

  override private [lowering] def procForMathIntrinsic(node: MathIntrinsic): Option[symlevel.Method] = {
    import Java.Lang.MathIntrinsic._
    node.kind match {
      case D_ABS | F_ABS | D_SQRT => None
      case _ => super.procForMathIntrinsic(node)
    }
  }

  override private [lowering] def procForMemAtomic(ai: MemAtomic): Option[symlevel.Method] = {
    import MemAtomic.Kind._
    ai.kind match {
      case ADD | AND | OR | XOR | MIN | UMIN | MAX | UMAX | SWAP if (ai.tpe == IntType) || (ai.tpe == LongType) => None
      case _ => shouldNotReachHere("unexpected MemAtomic: " + ai.kind + " " + ai.tpe)
    }
  }

  protected def clearHigh16Bits(node: Node): Node = {
    val mask = ~RTConst.VirtualMemory.INSIGNIFICANT_BITS_MASK_FOR_POINTER.addrValue

    And(node, IntegralConst(AddrIntType)(mask))
  }
}
