/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering.amd64

import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.{LSL, ASR, LSR}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.lowering.arch64.LoweringArch64
import com.huawei.excelsior.jet.compiler.opt.middle.Optimize
import com.huawei.excelsior.jet.compiler.options.BoolOption.IdescHigh16BitsCleaning
import com.huawei.excelsior.jet.compiler.{RTConst, symlevel}

/**
  * Amd64-specific lowering.
  *
  * @author liontiger
  */
trait LoweringAmd64 extends LoweringArch64 with PreLoweringAmd64 { self: Universe with Optimize =>

  import BitFieldExtract._
  import LoweringKind._

  override def MaxArrayFillSizeForSplitting: Int = 5

  override def shouldBeLoweredCases(node: Node): LoweringKind = node match {
    case mulh: MulH if mulh.tpe == IntType => FLOATING
    case umulh: UMulH if umulh.tpe == IntType => FLOATING
    case lzcnt: BitCount if lzcnt.kind == BitCount.Kind.LEADING_ZEROS => FLOATING
    case ValueConvert(F16, F64, _) | ValueConvert(F64, F16, _) => FLOATING
    case _ => super.shouldBeLoweredCases(node)
  }

  override def decomposeNode(node: Node): Node = node match {
    case x: MulH         => lowerMulH(x, signExtension = true)
    case x: UMulH        => lowerMulH(x, signExtension = false)
    case x: BitCount     => lowerLzcnt(x)
    case x: ValueConvert => lowerValueConvert(x)
    case _ => super.decomposeNode(node)
  }

  override def genExtractObject(rich: Node, bits: Node, enrichment: Node): Node = {
    val shiftedEnrichment = Shift(ArithOp.LSL, enrichment, IConst(enrichmentIMTOffsetShift))
    PublishRef(Xor(bits, shiftedEnrichment))
  }

  override def genMakeCIAO(itype: symlevel.Type, plain: Node, enrichment: Node): Node = {
    enrichment
  }

  private def lowerMulH(mulh: ArithCommutativeOp, signExtension: Boolean): Node = {
    assert(mulh.tpe == IntType)
    val l = Extend(signExtension, mulh.l)
    val r = Extend(signExtension, mulh.r)
    val fullMul = Mul(l, r)

    val intSize = typeSizeInBits(IntType)
    assert(intSize + intSize == typeSizeInBits(LongType))

    // Extract upper half of the multiplication result
    BFX(IntType, offset = intSize, size = intSize, signExtension = false, fullMul)
  }

  private def lowerLzcnt(lzcnt: BitCount): Node = {
    assert(lzcnt.kind == BitCount.Kind.LEADING_ZEROS)

    // `lzcnt` instruction is not available, we use `bsr`.
    // `lzcnt` + `bsr` + 1 = maxCount
    val tpe = lzcnt.argTpe
    val maxCount = typeSizeInBits(tpe)

    val bsr = BitCount.highestBit(tpe, lzcnt.arg)
    Sub(IConst(maxCount - 1), bsr)
  }

  private[lowering] def storeLoadForCellBarrier(obj: Node): Unit = {
    // no-op on intel
    // See JET-9664.
  }

  override private [lowering] def procForMathIntrinsic(node: MathIntrinsic): Option[symlevel.Method] = {
    import Java.Lang.MathIntrinsic._
    node.kind match {
      case D_ASIN | D_ACOS | D_EXP | D_POW | D_CEIL | D_FLOOR
           | D_SIN | D_COS | D_TAN | D_ATAN | D_RINT => super.procForMathIntrinsic(node)
      case _ => None
    }
  }

  override private [lowering] def procForMemAtomic(ai: MemAtomic): Option[symlevel.Method] = {
    import MemAtomic.Kind._
    ai.kind match {
      case AND | OR | XOR if ai.hasValueUses => super.procForMemAtomic(ai)
      case ADD | AND | OR | XOR | SWAP => None
      case _ => shouldNotReachHere("unexpected MemAtomic: " + ai.kind + " " + ai.tpe)
    }
  }

  override private [lowering] def lowerValueConvert(cast: ValueConvert): Node = (cast.fromAsm, cast.toAsm) match {
    case (F16, F64) =>
      ValueConvert(F32, F64)(ValueConvert(F16, F32)(cast.arg))
    case (F64, F16) =>
      ValueConvert(F32, F16)(ValueConvert(F64, F32)(cast.arg))

    case _ => super.lowerValueConvert(cast)
  }

  protected def clearHigh16Bits(node: Node): Node = {
    val offset = 64 - RTConst.VirtualMemory.SIGNIFICANT_BITS_FOR_POINTER.intValue
    
    ShiftByConst(AddrType, LSR, offset, ShiftByConst(AddrType, LSL, offset, node))
  }
}
