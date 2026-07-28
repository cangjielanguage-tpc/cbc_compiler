/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.B3xrrt4iK.K.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.ImmEXT.N.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Sign.*
import com.huawei.excelsior.jet.assembler.cbc.SignedImmCompactEncoding.EncodedImmParts
import org.scalatest.funsuite.AnyFunSuite
import xscala.util.MathUtils.*

class SignedImmCompactEncodingSuite extends AnyFunSuite {

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Common tests

  locally {
    import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Common.*

    for ((imm, width, expectedT4, iK, k, immext) <- Seq(
      // consists of (originalImm, targetWidth, t4, iK, K, Option[immext])
      (0x12345678_90AB_CDE0L, Width.W64, 0x0, 0xCDE0, K16, Some(ImmEXT(N48,  Signed,   0x12345678_90ACL))),
      (0xFFFFFFFF_FFAB_CDE0L, Width.W64, 0x0, 0xCDE0, K16, Some(ImmEXT(N8,   Signed,   0xACL))),
      (0x00000000_00AB_CDF0L, Width.W64, 0x0, 0xCDF0, K16, Some(ImmEXT(N8,   Unsigned, 0xAC))),
      (0xFFFFFFFF_FF00_0000L, Width.W64, 0x0, 0x0000, K0,  Some(ImmEXT(N16,  Signed,   0xFF00L))),
      (0x00000000_00F0_F0F0L, Width.W64, 0x0, 0xF0F0, K16, Some(ImmEXT(N8,   Unsigned, 0xF1))),
      (0x00000000_00AB_CDE0L, Width.W64, 0x0, 0xCDE0, K16, Some(ImmEXT(N8,   Unsigned, 0xAC))),
      (0x80000000_0000_0000L, Width.W64, 0x1, 0x0008, K16, None),
      (0x80000000_0000_0111L, Width.W64, 0x1, 0x1118, K16, None),
      (0x80000000_0000_1110L, Width.W64, 0x0, 0x1110, K16, Some(ImmEXT(N48,  Signed,   0x8000_0000_0000L))),
      (0x80000000_1111_1110L, Width.W64, 0x0, 0x1110, K16, Some(ImmEXT(N48,  Signed,   0x8000_0000_1111L))),
      (0x81111111_0000_0001L, Width.W64, 0x1, 0x0000, K0,  Some(ImmEXT(N48,  Signed,   0x8111_1111_0000L))),
      (0x81111111_0000_0010L, Width.W64, 0x0, 0x0001, K8,  Some(ImmEXT(N48,  Signed,   0x8111_1111_0000L))),
      (0x55555555_5555_5555L, Width.W64, 0x0, 0x5555, K16, Some(ImmEXT(N48,  Signed,   0x5555_5555_5555L))),
      (0x01010101_FFFF_FFFEL, Width.W64, 0x0, 0xFFFE, K16, Some(ImmEXT(N48,  Signed,   0x0101_0102_0000L))),
      (0x01010000_0000_7FFEL, Width.W64, 0x0, 0x7FFE, K16, Some(ImmEXT(N48,  Signed,   0x0101_0000_0000L))),
      (0x00000000_FFFF_0000L, Width.W64, 0x0, 0x0000, K0,  Some(ImmEXT(N16,  Unsigned, 0xFFFF))),
      (0xFFFFFFFF_FFFF_0000L, Width.W64, 0x0, 0x0000, K0,  Some(ImmEXT(N8,   Signed,   0xFF))),
      (0x00000000_FFFF_0002L, Width.W64, 0x2, 0x0000, K0,  Some(ImmEXT(N16,  Unsigned, 0xFFFF))),
      (0x00000000_7FFF_FFFFL, Width.W64, 0x0, 0xFFFF, K16, Some(ImmEXT(N16,  Unsigned, 0x8000))),
      (0xFFFFFFFF_8000_0000L, Width.W64, 0x0, 0x0000, K0,  Some(ImmEXT(N16,  Signed,   0x8000))),

      (0x00000000_0000_0101L, Width.W32, 0x1, 0x0010, K8,  None),
      (0x00000000_1010_0000L, Width.W32, 0x6, 0x0101, K16, None),
      (0x00000000_8000_0101L, Width.W32, 0x1, 0x0406, K16, None),
      (0x00000000_8111_0101L, Width.W32, 0x1, 0x0010, K8,  Some(ImmEXT(N16,  Unsigned, 0x8111))),
      (0x00000000_0000_0001L, Width.W32, 0x1, 0x0000, K0,  None), // not valid for compact encoding (as it fits into 4 or 12 bits and therefore is handled by other means), test it just in case
      (0x00000000_0000_0010L, Width.W32, 0xE, 0x0001, K16, None), // not valid for compact encoding (as it fits into 4 or 12 bits and therefore is handled by other means), test it just in case
      (0x00000000_5555_5555L, Width.W32, 0x0, 0x5555, K16, Some(ImmEXT(N16,  Signed,  0x5555))),
      (0x00000000_0101_FFFEL, Width.W32, 0x0, 0xFFFE, K16, Some(ImmEXT(N16,  Signed,  0x0102))),
      (0x00000000_0000_F000L, Width.W32, 0xA, 0x000F, K16, None),
      (0x00000000_FFFF_0FFFL, Width.W32, 0xA, 0xFFF0, K16, None)

    )) {
      test(f"common $width 0x$imm%016X $expectedT4 0x$iK%016X $k $immext") {
        val encImm @ EncodedImmParts(t4, shortImm, calculatedK, encodedImmEXT) = SignedImmCompactEncoding.calculateMemoryCompactImm(imm, width)
        assert(t4 == expectedT4)
        assert(shortImm == iK, f"0x$shortImm%016X")
        assert(calculatedK == k)
        assert(immext == encodedImmEXT)
        assert(imm == encImm.decodeImm(width))
      }
    }

    for ((imm, signed, i16, immext) <- Seq(
      // consists of (originalImm, signed, i16, Option[immext])
      (0x00000000_0000_FFFFL, true,  0xFFFFL, Some(ImmEXT(N8,  Signed,   0x1))),
      (0xFFFFFFFF_FFFF_FFFEL, true,  0xFFFEL, None),
      (0xFFFFFFFF_FFFF_8000L, true,  0x8000L, None),
      (0x00000000_0000_8000L, true,  0x8000L, Some(ImmEXT(N8,  Signed,   0x1))),
      (0x00000000_FFFF_8000L, true,  0x8000L, Some(ImmEXT(N32, Signed,   0x10000))),
      (0x00000000_0000_FFFFL, false, 0xFFFFL, None),
      (0xFFFFFFFF_FFFF_FFFEL, false, 0xFFFEL, Some(ImmEXT(N8,  Signed,   0xFF))),
      (0xFFFFFFFF_FFFF_8000L, false, 0x8000L, Some(ImmEXT(N8,  Signed,   0xFF))),
      (0x00000000_0000_8000L, false, 0x8000L, None),
      (0x00000000_FFFF_8000L, false, 0x8000L, Some(ImmEXT(N16, Unsigned, 0xFFFF)))

    )) {
      test(f"bcc+i16 0x$imm%016X $signed $immext") {
        val low16signBit = if signed then bit(imm, 15) else 0
        val encodedImmext = getImmext((imm >> 16) + low16signBit)
        assert(encodedImmext == immext)
        val decodedImmext = immext.map(_.decodeImmEXT(Width.W64)).getOrElse(0L)
        if (signed) {
          assert(signExtend(i16, 16) + decodedImmext == imm)
        } else {
          assert((i16 & rightNBits64(16)) + decodedImmext == imm)
        }
      }
    }
  }
}
