/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

import com.huawei.excelsior.jet.assembler.fixups.Relocation
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.{ADDR32, OFFS32}
import org.scalatest.funsuite.AnyFunSuite

/** Tests for [[Segment]] class.
  *
  * @author conwor
  * @author cypok
  */
class SegmentSuite extends AnyFunSuite with AssemblerToolbox[Null] {

  test("append bytes") {
    val seg1 = new Segment
    seg1.putW32(0x78563412)

    val seg2 = new Segment
    seg2.putW32(0x99EFCDAB)

    seg1.append(seg2)

    checkFinal(seg1,
      0x12, 0x34, 0x56, 0x78,
      0xAB, 0xCD, 0xEF, 0x99)
  }

  test("append fixups") {
    val seg1 = new Segment
    val seg2 = new Segment

    val target1 = newSymbol
    val target2 = newSymbol

    seg1.putByte(0x01)
    seg1.addFixup(new Relocation(ADDR32, target1, 2))

    seg2.putByte(0x03)
    seg2.addFixup(new Relocation(ADDR32, target2, 4))

    seg1.append(seg2)

    checkFinal(seg1,
      0x01,
      relocation(ADDR32, target1), 0x02, 0x00, 0x00, 0x00,
      0x03,
      relocation(ADDR32, target2), 0x04, 0x00, 0x00, 0x00)
  }

  test("append code label") {
    val seg1 = new Segment
    val seg2 = new Segment

    val seg1Symbol = newSymbol
    seg1.bind(seg1Symbol)

    val label1 = seg1.newLabel
    val label2 = seg1.newLabel

    seg1.putByte(0x01)
    seg1.bindLabel(label1)
    seg1.putByte(0x02)
    seg1.addFixup(new Relocation(OFFS32, label2, 0x33330000))

    seg2.putByte(0x04)
    seg2.bindLabel(label2)
    seg2.putByte(0x05)
    seg2.addFixup(new Relocation(OFFS32, label1, 0x66660000))

    seg1.append(seg2)

    assertResult(label1.segment)(seg1)
    assertResult(label1.position)(1)
    assertResult(label2.segment)(seg1)
    assertResult(label2.position)(7)

    checkIntermediate(seg1,
      0x01, /*label1*/ 0x02,
      new Relocation(OFFS32, label2, 0x33330000), 0x00, 0x00, 0x00, 0x00,
      0x04, /*label2*/ 0x05,
      new Relocation(OFFS32, label1, 0x66660000), 0x00, 0x00, 0x00, 0x00)

    checkFinal(seg1,
      0x01, 0x02,
      0x05, 0x00, 0x33, 0x33, // relative offset = 5
      0x04, 0x05,
      0xf9, 0xff, 0x65, 0x66) // relative offset = -7
  }

  test("bind in another segment") {
    val seg1 = new Segment
    val l = seg1.newLabel

    val seg2 = new Segment
    seg2.append(seg1)

    seg2.bindLabel(l)
  }
}
