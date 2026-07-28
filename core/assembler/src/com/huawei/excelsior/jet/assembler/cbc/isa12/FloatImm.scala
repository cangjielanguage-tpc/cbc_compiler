/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12

import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.B3xrrt4iK.K
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.B3xrrt4iK.K.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Width
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Width.{W32, W64}
import xscala.util.MathUtils.{alignDown, isNBitsSigned, zeroExtend, isNBits as isNBitsUnsigned}

import scala.PartialFunction.condOpt


object FloatImm {
  private object IntegralConstFromFloatingPoint {
    def unapply(fimm: Double): Option[Long] = condOpt(scala.math.floor(fimm)) {
      case x if x == fimm && fimm != -.0f && !fimm.isInfinite && !fimm.isNaN => x.toLong
    }
  }

  case class EncodeData(t4: Int, K: K, iK: Int, immext: Long = 0)

  def encode(fimm: Double, width: Width): EncodeData = {
    def prepareT4AndIK(width: Width, bits: Long): (Int, Long) = {
      val shiftStep = width.nbits / 16

      val shift = alignDown(java.lang.Long.numberOfTrailingZeros(bits), shiftStep)
      (16 - shift / shiftStep, bits >>> shift)
    }

    val bits = (width: @unchecked) match {
      case W32 => zeroExtend(java.lang.Float.floatToRawIntBits(fimm.toFloat))
      case W64 => java.lang.Double.doubleToRawLongBits(fimm)
    }
    val (shiftPartT4, otherPart) = prepareT4AndIK(width, bits)

    fimm match {
      // If float imm actually an integer number (and != -0.0), and ranges in [-8; 8), it might be encoded as t4
      case IntegralConstFromFloatingPoint(floorValue) if isNBitsSigned(floorValue, 4) =>
        EncodeData(t4 = (floorValue & 0xf).toInt, K = K0, iK = 0)

      // If float imm actually an integer number (and != -0.0), and ranges in [-2048; 2048), it might be encoded as i8:t4
      case IntegralConstFromFloatingPoint(floorValue) if isNBitsSigned(floorValue, 12) =>
        EncodeData(t4 = (floorValue & 0xf).toInt, K = K8, iK = ((floorValue >> 4) & 0xff).toInt)

      // If shifted float imm can be written in 16 bits, it might be encoded as i16 and t4 will contain rotation count
      case _ if isNBitsUnsigned(otherPart, 16) =>
        EncodeData(t4 = shiftPartT4, K = K16, iK = otherPart.toInt)

      // Write high part of number in immextN and other part in i16
      case _ =>
        val immEXT = bits >>> 16
        EncodeData(t4 = 0, K = K16, iK = (bits & 0xFFFF).toInt, immEXT)
    }
  }
}
