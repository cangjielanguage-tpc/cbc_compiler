/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import xscala.util.MathUtils
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{ArithNodesDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.*
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.InterfaceType
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeField, FakeMethodReference, FakeSymbol, FakeType}
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, TypeKind as TKind}
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement

import scala.util.Random

/**
 * @author kit
 */
class IdentitiesSuite extends CompilerSuite with Identities with GlobalNodesBuilder with ArithNodesDSL with Types {

  import BitFieldExtract._

  startPhase(CompilerPhase.PreLowering)

  test("int and long common consts matching") {
    0 should be (0L)
    1 should be (1L)
    -1 should be (-1L)
  }

  test("phi identity") {
    makeCFG(0 -> 1 -> 1)
    Phi(IntType)(b(0), ic(0)) should be (ic(0))
    Phi(IntType)(b(0), ic(42), ic(42)) should be (ic(42))
    Phi.cyclic(IntType)(b(1), phi => Seq(iarg, phi)) should be (iarg)
  }

  test("bfx identity") {
    BFX(IntType, 0, 32, signExtension = true, iarg) should be (iarg)

    val ibfx = BFX(IntType, 0, 5, signExtension = true, iarg)
    BFX(IntType, 0, 5, signExtension = true, ibfx) should be (ibfx)

    val lbfx = BFX(LongType, 0, 33, signExtension = false, larg)
    BFX(LongType, 0, 33, signExtension = false, lbfx) should be (lbfx)

    BFX(IntType, 5, 5, signExtension = true, BFX(IntType, 10, 20, signExtension = false, iarg)) should be (BFX(IntType, 15, 5, signExtension = true, iarg))

    BFX(LongType, 10, 25, signExtension = false, BFX(LongType, 5, 55, signExtension = true, larg)) should be (BFX(LongType, 15, 25, signExtension = false, larg))

    BFX(IntType, 10, 15, signExtension = true, BFX(IntType, 25, 20, signExtension = true, larg)) should be (BFX(IntType, 35, 10, signExtension = true, larg))

    Truncate(BFX(LongType, 22, 31, signExtension = true, larg)) should be (BFX(IntType, 22, 31, signExtension = true, larg))
    Truncate(BFX(LongType, 8, 23, signExtension = false, iarg)) should be (BFX(IntType, 8, 23, signExtension = false, iarg))
    Truncate(Shift(LSL, larg, ic(32))) should be (IConst(0))
    Shift(LSL, Truncate(Shift(LSL, larg, ic(20))), ic(10)) should be (Truncate(Shift(LSL, larg, ic(30))))

    Shift(LSR, Truncate(Shift(ASR, larg, ic(16))), ic(16))  should be (BFX(IntType, 32, 16, signExtension = false, larg))
    Shift(ASR, Truncate(Shift(LSR, larg, ic(31))), ic(-15)) should be (BFX(IntType, 48, 15, signExtension = true,  larg))
    Shift(LSR, Truncate(Shift(LSR, larg, ic(31))), ic(17))  should be (BFX(IntType, 48, 15, signExtension = false, larg))
    Shift(ASR, Truncate(Shift(ASR, larg, ic(3))),  ic(2))   should be (BFX(IntType, 5,  30, signExtension = true,  larg))
    Shift(LSR, Truncate(larg), ic(5)) should be (BFX(IntType, 5, 27, signExtension = false, larg))
    Shift(ASR, Truncate(larg), ic(6)) should be (BFX(IntType, 6, 26, signExtension = true,  larg))

    SignExtend(BFX(IntType, 5, 7, signExtension = true, iarg)) should be (BFX(LongType, 5, 7, signExtension = true, iarg))
    ZeroExtend(Truncate(larg)) should be (BFX(LongType, 0, 32, signExtension = false, larg))
    SignExtend(BFX(IntType, 22, 31, signExtension = true, larg)) should be (BFX(LongType, 22, 31, signExtension = true, larg))

    BFX(IntType, 8, 16, signExtension = false, ic(0xFFABCDFF)) should be (ic(0xABCD))
    BFX(IntType, 4, 12, signExtension = true, ic(0x00ABCDEF)) should be (ic(0xFFFFFCDE))

    for (_ <- 1 to 10) {
      for ((from, to) <- Seq((IntType, IntType), (IntType, LongType), (LongType, IntType), (LongType, LongType))) {
        val offset = Random.nextInt(typeSizeInBits(from))
        val sizeRandom = 1 + Random.nextInt(typeSizeInBits(to) - 1) // [1,typeSize)
        val size = Math.min(sizeRandom, typeSizeInBits(from) - offset)
        val value = Random.nextLong()
        val valueNode = IntegralConst(from)(value)
        BFX(to, offset, size, signExtension = false, valueNode) should be (IntegralConst(to)(MathUtils.bits(value, offset, offset + size - 1)))
        BFX(to, offset, size, signExtension = true, valueNode) should be (IntegralConst(to)(MathUtils.bitsSigned(value, offset, offset + size - 1)))
      }
    }

    Extend(IntType, I8, signExtension = true, ic(0xff)) should be (ic(-1))
    Extend(IntType, I16, signExtension = true, ic(0xffff)) should be (ic(-1))
    SignExtend(ic(42)) should be (lc(42))
    SignExtend(ic(-1)) should be (lc(-1))

    Extend(IntType, I8,  signExtension = false, ic(-1)) should be (ic(0xff))
    Extend(IntType, I8,  signExtension = false, ic(0xabcdef)) should be (ic(0xef))
    Extend(IntType, U16,  signExtension = false, ic(1)) should be (ic(1))
    Extend(IntType, U16,  signExtension = false, ic(-1)) should be (ic(0xffff))
    Extend(IntType, U16,  signExtension = false, ic(0xffff)) should be (ic(0xffff))
    Extend(IntType, I16, signExtension = false, ic(-1)) should be (ic(0xffff))
    Extend(IntType, I16, signExtension = false, ic(0xabcdef)) should be (ic(0xcdef))

    Truncate(lc(0x1122334455667788L)) should be (ic(0x55667788))

    for (shortType <- Seq(I8, U8, I16, U16)) {
      val casted = JavaShortIntegralExtend(shortType, iarg)
      JavaShortIntegralExtend(shortType, casted) should be (casted)
    }
  }

  test("cast identity") {
    ValueConvert(I32, F32)(ic(-16)) should be (fc(-16.0f))
    ValueConvert(F64, I32)(dc(Double.PositiveInfinity)) should be (ic(Int.MaxValue))
    ValueConvert(F64, I64)(dc(Double.PositiveInfinity)) should be (lc(Long.MaxValue))
    ValueConvert(F64, F32)(dc(92.64d)) should be (fc(92.64f))

    ReinterpretCast(FloatType,  IntType   )(fc(37.0f))               should be (ic(0x42140000))
    ReinterpretCast(DoubleType, LongType  )(dc(37.0d))               should be (lc(0x4042800000000000L))
    ReinterpretCast(IntType,    FloatType )(ic(0x42140000))          should be (fc(37.0f))
    ReinterpretCast(LongType,   DoubleType)(lc(0x4042800000000000L)) should be (dc(37.0d))
  }

  test("short integral extend getMemOp identity") {
    makeCFG(0)

    def f(kind: TKind) = new FakeField(`type` = SignatureType.Primitive(kind))
    def gf(kind: TKind) = GetField(f(kind))(addObjNode())
    def gs(kind: TKind) = GetStatic(f(kind))
    def ag(kind: TKind) = ArrayGet(sig(env.get1DimArrayType(kind)))(addObjNode(), ic(0))
    def lm(kind: TKind) = LoadMemory(SignatureType.Primitive(kind).toAsm, SignatureType.Primitive(kind), false)(IntegralConst(AddrType)(0))

    for {
      nodeConstr <- Seq(gf(_), gs(_), ag(_), lm(_))
      kind <- Seq(TKind.BYTE, TKind.SHORT, TKind.CHAR)
    } {
      makeNodes { at =>
        at(0)
        val n = nodeConstr(kind)
        withClue(s"$n") {
          JavaShortIntegralExtend(SignatureType.Primitive(kind).toAsm, n) should be (n)
        }
      }
    }
  }

  test("add identity") {
    val lv = Long.MaxValue
    for ((v1, v2, res) <- Seq(
      (lc(lv), lc(0), lc(lv)),
      (ic(0), ic(5), ic(5)),
      (ic(Int.MaxValue - 10), ic(Int.MaxValue - 20), ic(-32)),
      (larg, lc(0), larg),
      (iarg, ic(0), iarg),
      (ic(2), ic(2), ic(4)),
      (lc(-2), lc(-2), lc(-4)),
      (neg(iarg), iarg, ic(0)),
      (larg, neg(larg), lc(0)),
    ))
    {
      add(v1, v2) should be (res)
    }
  }

  test("associative operations identity") {
    // (1 + (2 + x)) -> (3 + x)
    add(ic(1), add(ic(2), iarg)) should be (add(ic(3), iarg))
    // (x + (3 + x)) -> (3 + (x + x))
    add(iarg, add(ic(3), iarg)) should be (add(ic(3), add(iarg, iarg)))
    // ((-2) + ((x + 2) + x)) -> (x + x)
    add(lc(-2), add(add(larg, lc(2)), larg)) should be (add(larg, larg))
    // ((x + 1) + (2 + x)) -> (3 + (x + x))
    add(add(iarg, ic(1)), add(ic(2), iarg)) should be (add(ic(3), add(iarg, iarg)))

    for (op <- Seq[(Node, Node) => Node](add, mul, and, or, xor)) {
      val const = op(ic(3), op(ic(1), ic(5)))
      const should matchPattern {
        case IConst(_) =>
      }
      op(ic(3), op(op(iarg, ic(1)), ic(5))) should be (op(const, iarg))
    }

    // ID dependence checks
    val x = iarg
    val c = ic(111)
    val y = Param(IntType, 111)
    val sum = add(x, c)
    val z = Param(IntType, 333)

    // ((x + c) + y) -> (c + (x + y))
    assert(y.id < sum.id)
    add(sum, y) should be (add(c, add(x, y)))

    // ((x + c) + z) -> (c + (x + z))
    assert(sum.id < z.id)
    add(sum, z) should be (add(c, add(x, z)))
  }

  test("add and and identity") {
    // ((x & 1) + 2) -> ((x & 1) + 2)
    add(and(iarg, ic(1)), ic(2)) should matchPattern {
      case Add(And(Param(_), IConst(1)), IConst(2)) =>
    }
  }

  test("sub & neg non-const folding"){
    val ix = iarg
    val iy = Param(IntType, 111)
    for ((v1, v2, res) <- Seq(
      // (x + y) - y -> x
      (add(ix, iy), iy, ix),
      // (y + x) - y -> x
      (add(iy, ix), iy, ix),
      // -y - -(x + y) -> x
      (neg(iy), neg(add(iy, ix)), ix),
      // -y - -(y + x) -> x
      (neg(iy), neg(add(ix, iy)), ix)
    )) {
      sub(v1, v2) should be(res)
    }
  }

  test("sub identity") {
    val lv = Long.MaxValue
    for ((v1, v2, res) <- Seq(
      (lc(lv),  lc(5),  lc(lv - 5)),
      (ic(10),  ic(7),  ic(3)),
      (lc(0),   larg,   neg(larg)),
      (iarg,    iarg,   ic(0)),
      (iarg,    ic(0),  iarg),

      (ic(10),              sub(ic(7), iarg),   add(ic(3), iarg)),
      (ic(10),              sub(iarg, ic(7)),   sub(ic(17), iarg)),
      (sub(ic(10), iarg),   ic(7),              sub(ic(3), iarg)),
      (sub(iarg, ic(10)),   ic(7),              sub(iarg, ic(17))),

      (add(iarg, ic(1)),    ic(1),              iarg)
    )) {
      sub(v1, v2) should be (res)
    }
  }

  test("neg identitity") {
    for ((v, res) <- Seq(
       (ic(-42), ic(42)),
       (ic(Int.MinValue), ic(Int.MinValue)),
       (lc(-37), lc(37)),
       (lc(Long.MinValue), lc(Long.MinValue)),
       (neg(larg), larg),
       (sub(iarg, ic(37)), sub(ic(37), iarg))))
    {
      neg(v) should be (res)
    }
  }

  test("sub & neg folding") {
    // (-(x - 1) - 1) -> (-x)
    sub(neg(sub(iarg, ic(1))), ic(1)) should be (neg(iarg))
  }

  test("mul identitity") {
    for ((v1, v2, res) <- Seq(
      (lc(5),  lc(5), lc(25)),
      (ic(6),  ic(6), ic(36)),
      (lc(0), larg, lc(0)),
      (iarg,ic(0), ic(0)),
      (lc(1),  larg,larg),
      (iarg, ic(1), iarg),
      (iarg, ic(-1), neg(iarg)),
      (iarg, ic(8),  lsl(iarg, ic(3))),
      (larg, lc(64), lsl(larg, ic(6))),
      (ic(-10), ic(8), ic((-10)<<3))
    ))
    {
      mul(v1, v2) should be (res)
    }
  }

  test("mulh identitity") {
    for ((v1, v2, res) <- Seq(
      (ic(5), ic(5), ic(0)),
      (lc(6), lc(6), lc(0)),
      (ic(0x12345678), ic(0x12345678), ic(0x014B66DC)),
      (ic(0x87654321), ic(0x12345678), ic(0xF76C768D)),
      (ic(0x8FFFFFFF), ic(0x8FFFFFFF), ic(0x31000000)),
      (lc(0x1234567887654321L), lc(0x1234567887654321L), lc(0x014B66DC3136724BL)),
      (lc(0x8765432112345678L), lc(0x1234567887654321L), lc(0xF76C768D323AA60DL)),
      (lc(0x8FFFFFFFFFFFFFFFL), lc(0x8FFFFFFFFFFFFFFFL), lc(0x3100000000000000L))
    ))
    {
      mulh(v1, v2) should be (res)
    }
  }

  test("umulh identitity") {
    for ((v1, v2, res) <- Seq(
      (ic(5), ic(5), ic(0)),
      (lc(6), lc(6), lc(0)),
      (ic(0x12345678), ic(0x12345678), ic(0x014B66DC)),
      (ic(0x87654321), ic(0x12345678), ic(0x09A0CD05)),
      (ic(0x8FFFFFFF), ic(0x8FFFFFFF), ic(0x50FFFFFE)),
      (lc(0x1234567887654321L), lc(0x1234567887654321L), lc(0x014B66DC3136724BL)),
      (lc(0x8765432112345678L), lc(0x1234567887654321L), lc(0x09A0CD05B99FE92EL)),
      (lc(0x8FFFFFFFFFFFFFFFL), lc(0x8FFFFFFFFFFFFFFFL), lc(0x50FFFFFFFFFFFFFEL))
    ))
    {
      umulh(v1, v2) should be (res)
    }
  }

  test("idiv identity") {
    for ((v1, v2, res) <- Seq(
      (lc(25),  lc(5), lc(5)),
      (ic(36),  ic(6), ic(6)),
      (lc(0),  larg, lc(0)),
      (iarg, ic(1), iarg),
      (larg, lc(-1), neg(larg)),
      (iarg, iarg, ic(1)),
      (larg, larg, lc(1)),
      (ic(-10), ic(8), ic(-1)),
      (ic(10), ic(8), ic((10 + ((10>>31)&7))>>3)),
      (ic(0x0A3), ic(2), ic((0x0A3 + ((0x0A3>>31) & 1))>>1)),
      (ic(-10), ic(8), ic((-10 + ((-10>>31) & 7))>>3))
    ))
    {
      div(v1, v2) should be (res)
    }
  }

  test("idiv with convert identity") {
    for ((valid, d) <- Seq(
      (true,  123L),
      (true,  Int.MaxValue.toLong),
      (true,  Int.MinValue.toLong),
      (false, Int.MinValue.toLong - 30),
      (false, Long.MaxValue),
      (false, Long.MinValue),
      (false, -1L)
    ))
    {
      val actual = div(SignExtend(iarg), lc(d))
      def expected = SignExtend(div(iarg, ic(d.toInt)))
      if (valid) actual should be (expected)
      else actual should not be a [Cast]
    }
  }

  test("irem identity") {
    def crem(v1: Int, v2: Int): Int = {
      val signext = v1 >> 31
      val xor = v1 ^ signext
      val diff = xor - signext
      val and = diff & (v2-1)
      val xor2 = and ^ signext
      xor2 - signext
    }

    for ((v1, v2, res) <- Seq(
      (lc(7),  lc(5), lc(2)),
      (ic(-6), ic(4), ic(-2)),
      (lc(0), larg, lc(0)),
      (iarg, ic(1), ic(0)),
      (larg, lc(-1), lc(0)),
      (iarg, iarg, ic(0)),
      (larg, larg, lc(0)),
      (ic(-34567), ic(16), ic(crem(-34567, 16)))
    ))
    {
      rem(v1, v2) should be (res)
    }
  }

  test("irem with convert identity") {
    for ((valid, d) <- Seq(
      (true,  123L),
      (true,  Int.MaxValue.toLong),
      (true,  Int.MinValue.toLong),
      (false, Int.MinValue.toLong - 30),
      (false, Long.MaxValue),
      (false, Long.MinValue),
      (false, -1L)
    ))
    {
      val actual = rem(SignExtend(iarg), lc(d))
      def expected = SignExtend(rem(iarg, ic(d.toInt)))
      if (valid) actual should be (expected)
      else actual should not be a [Cast]
    }
  }

  test("udiv identitity") {
    val lv: Long = Long.MaxValue - 42
    val iv: Int = Int.MaxValue - 123
    for ((v1, v2, res) <- Seq(
      (lc(25),  lc(5), lc(5)),
      (ic(36),  ic(6), ic(6)),
      (ic(iv*2 + 43), ic(iv), ic(2)),
      (lc(lv*2 + 42), lc(lv), lc(2)),
      (lc(0),  larg, lc(0)),
      (iarg, ic(1), iarg),
      (iarg, iarg, ic(1)),
      (larg, larg, lc(1))
    ))
    {
      udiv(v1, v2) should be (res)
    }
  }

  test("urem identity") {
    val lv: Long = Long.MaxValue - 42
    val iv: Int = Int.MaxValue - 123
    for ((v1, v2, res) <- Seq(
      (lc(7),  lc(5), lc(2)),
      (ic(6),  ic(4), ic(2)),
      (ic(iv*2 + 43), ic(iv), ic(43)),
      (lc(lv*2 + 42), lc(lv), lc(42)),
      (lc(0), larg, lc(0)),
      (iarg, ic(1), ic(0)),
      (iarg, iarg, ic(0)),
      (larg, larg, lc(0)),
      (ic(834), ic(32), ic(834&31))
    ))
    {
      urem(v1, v2) should be (res)
    }
  }

  test("and identity") {
    for ((v1, v2, res) <- Seq(
      (lc(0xCD), lc(0xAB00),     lc(0)),
      (ic(0xFF), ic(0xFF0),      ic(0xF0)),
      (lc(0),    larg,         lc(0)),
      (iarg,   ic(0xffffffff), iarg)))
    {
      and(v1, v2) should be (res)
    }
  }

  test("or identity") {
    for ((v1, v2, res) <- Seq(
      (lc(0xCD), lc(0xAB00),    lc(0xABCD)),
      (ic(0xFF), ic(0xFF0),     ic(0xFFF)),
      (iarg,  ic(0),          iarg),
      (larg,  lc(-1.toLong),  lc(-1.toLong))))
    {
      or(v1, v2) should be (res)
    }
  }

  test("xor identity") {
    for ((v1, v2, res) <- Seq(
      (lc(0xCD), lc(0xAB00),    lc(0xABCD)),
      (ic(0xFF), ic(0xFF0),     ic(0xF0F)),
      (iarg,   ic(0),         iarg)))
    {
      xor(v1, v2) should be (res)
    }
  }

  test("shl identity") {
    for ((op, v1, v2, res) <- Seq(
      (LSL, lc(0xCD), ic(4),       lc(0xCD0)),
      (ASR, ic(0xFF), ic(4),       ic(0xF)),
      (ASR, ic(0xFFFFFFF0), ic(4), ic(-1)),
      (LSR, ic(-1),   ic(4),       ic(0xFFFFFFF)),
      (LSR, lc(0xFF), ic(4),       lc(0xF)),
      (LSL, ic(0),    iarg,        ic(0)),
      (ASR, lc(0),    iarg,        lc(0)),
      (LSR, ic(0),    iarg,        ic(0)),
      (ASR, lc(-1),   iarg,        lc(-1)),
      (LSL, iarg,     ic(0),       iarg),
      (ASR, iarg,     ic(-10),     Shift(ASR,  iarg, ic(22))),
      (LSR, iarg,     ic(40),      Shift(LSR, iarg, ic(8))),
      (LSL, larg,     ic(-40),     Shift(LSL,  larg, ic(24))),
      (ASR, larg,     ic(128),     larg)))
    {
      Shift(op, v1, v2) should be (res)
    }
  }

  test("shl merge identity") {
    for ((op, v1, v2, res) <- Seq(
      (ASR, ic(32),   ic(2),   ic(34)),
      (LSL, ic(32),   ic(6),   ic(38)),
      (LSR, ic(32),   ic(1),   ic(33)),
      (ASR, ic(33),   ic(2),   ic(35)),
      (LSL, ic(34),   ic(6),   ic(40)),
      (LSR, ic(35),   ic(1),   ic(36))
    ))
    {
      Shift(op, Truncate(Shift(op, larg, v1)), v2) should be (
        Truncate(Shift(op, larg, res))
      )
    }
  }

  test("shl merge false identity") {
    for ((op1, op2, v1, v2) <- Seq(
      (ASR, LSR, ic(33),  ic(16)),
      (LSR, ASR, ic(33),  ic(-15)),
      (ASR, ASR, ic(-15), ic(23)),
      (LSL, LSR, ic(33),  ic(5)),
      (ASR, LSL, ic(20),  ic(1))
    ))
    {
      Shift(op2, Truncate(Shift(op1, larg, v1)), v2) should not matchPattern {
        case bfx: BitFieldExtract if bfx.argType == LongType =>
      }
    }
  }

  test("cmpIdentity math") {
    import Condition._
    def cmp(op: Condition, x: Node, y: Node) = Cmp(x.tpe | y.tpe, op)(x, y)

    def checkCmp(op: Condition, x: Node, y: Node, res: Boolean) = withClue(s"$x $op $y expecting $res: ") {
      val isFP = x.tpe.isFloatingPointType
      cmp(op, x, y) shouldBe ConstCondition(res)
      cmp(op.negate(isFP), x, y) shouldBe ConstCondition(!res)
      cmp(op.swap, y, x) shouldBe ConstCondition(res)
      cmp(op.swap.negate(isFP), y, x) shouldBe ConstCondition(!res)
      cmp(op.negate(isFP).swap, y, x) shouldBe ConstCondition(!res)
    }

  }

  test("cmp identity") {
    import Condition._
    import Double.NaN

    def cmp(op: Condition, x: Node, y: Node) = Cmp(x.tpe | y.tpe, op)(x, y)
    def lcmp(x: Node, y: Node) = ThreeCmp(LongType, CMP)(x, y)
    def fcmpl(x: Node, y: Node) = ThreeCmp(FloatType, CMPL)(x, y)
    def dcmpg(x: Node, y: Node) = ThreeCmp(DoubleType, CMPG)(x, y)

    makeCFG(0)
    val classA = FakeType("A")
    val classB = FakeType("B")

    def refConst(x: AnyRef) = x match {
      case null => Null()

      case s: String => ConstString(new symlevel.ConstString {
        override def value = XString.ascii(s)

        override def getStringTable = shouldNotCallThis("not implemented")
        override def getStringNumber = shouldNotCallThis("not implemented")
        override def getHost = shouldNotCallThis("not implemented")
      }, typeProvider.getStringType)(entryBlock)

      case t: FakeType => ClassObject(t)(entryBlock)

      case _ => shouldNotReachHere(x)
    }

    def checkCmp(op: Condition, x: Node, y: Node, res: Boolean) = withClue(s"$x $op $y expecting $res: "){
      val isFP = x.tpe.isFloatingPointType
      cmp(op, x, y) shouldBe ConstCondition(res)
      cmp(op.negate(isFP), x, y) shouldBe ConstCondition(!res)
      cmp(op.swap, y, x) shouldBe ConstCondition(res)
      cmp(op.swap.negate(isFP), y, x) shouldBe ConstCondition(!res)
      cmp(op.negate(isFP).swap, y, x) shouldBe ConstCondition(!res)
    }

    for ((op, v1, v2, res) <- Seq(
      (EQ,   42, 42, true),
      (EQ,   42, 43, false),
      (NE,   37, 37, false),
      (NE,   37, 38, true),
      (GE,   42, 43, false),
      (GE,   42, 42, true),
      (GT,   43, 42, true),
      (GT,   42, 42, false),
      (LE,   42, 43, true),
      (LE,   42, 42, true),
      (LT,   43, 42, false),
      (LT,   42, 42, false)))
    {
      checkCmp(op, ic(v1), ic(v2), res)
      checkCmp(op, lc(v1), lc(v2), res)
      checkCmp(op, lcmp(lc(v1), lc(v2)), ic(0), res)
    }

    for ((op, v1, v2, res) <- Seq(
      (UGE,  42, 43, false),
      (UGE,  42, 42, true),
      (UGT,  43, 42, true),
      (UGT,  42, 42, false),
      (ULE,  42, 43, true),
      (ULE,  42, 42, true),
      (ULT,  43, 42, false),
      (ULT,  42, 42, false),
      (ULT,  -1,  0, false),
      (UGE,  -42, 0, true)))
    {
      checkCmp(op, ic(v1), ic(v2), res)
      checkCmp(op, lc(v1), lc(v2), res)
    }

    for ((op, v1, v2, res) <- Seq(
      (EQ,   0.0,  -0.0, true),
      (EQ,   NaN,   1.0, false),
      (EQ,   NaN,   NaN, false),
      (NE,   NaN,   NaN, true),
      (NE,   1.0,  -1.0, true),
      (GE,  1e10, 1e-10, true),
      (GE,   0.0, NaN,   false),
      (GE_OR_UNORDERED, 0.0, NaN, true)))
    {
      checkCmp(op, dc(v1), dc(v2), res)
      checkCmp(op, fc(v1.toFloat), fc(v2.toFloat), res)
    }

    for (v <- Seq(null, classA)) {
      refConst(v) should be (refConst(v))
      checkCmp(EQ, refConst(v), refConst(v), res = true)
    }
    for ((v1, v2, res) <- Seq(
      (null,   "abc",  false),
      (null,   classA, false),
      ("abc",  "abc",  true ),
      ("abc",  "def",  false),
      ("abc",  classA, false),
      (classA, classB, false)))
    {
      val n1 = refConst(v1)
      val n2 = refConst(v2)
      n1 shouldNot be (n2) // equal nodes are not interesting here, see above
      checkCmp(EQ, n1, n2, res)
    }

    val sym = new FakeSymbol
    cmp(EQ, SymbolAddress(sym), nullAddr) should be (False())
    cmp(NE, nullAddr, SymbolAddress(sym)) should be (True())

    val interfType = FakeType(TKind.INTERFACE)
    val richVal = addRichObjNode(InterfaceType(interfType))
    cmp(EQ, Deprive(interfType)(richVal), AnyNull(richVal.tpe)) should be (cmp(EQ, richVal, AnyNull(richVal.tpe)))
    val deprVal = addObjNode()
    cmp(EQ, Enrich(interfType)(deprVal, IntegralConst(AddrType)(37)), AnyNull(TRefType)) should be (cmp(EQ, deprVal, AnyNull(TRefType)))

    cmp(EQ, iarg, iarg) should be (True())
    cmp(LT, lcmp(larg, larg), ic(0)) should be (False())

    val cmp1 = cmp(GE, lcmp(larg, lc(42)), ic(0)).asInstanceOf[Cmp]
    (cmp1.op, cmp1.l, cmp1.r) should (be ((GE, larg, lc(42)))
                                   or be ((LE, lc(42), larg)))

    val cmp2 = cmp(LT, fcmpl(fc(42.5f), farg), ic(0)).asInstanceOf[Cmp]
    (cmp2.op, cmp2.l, cmp2.r) should (be ((GT_OR_UNORDERED, farg, fc(42.5f)))
                                   or be ((LT_OR_UNORDERED, fc(42.5f), farg)))

    val cmp3 = cmp(LE, dcmpg(darg, dc(33.8)), ic(0)).asInstanceOf[Cmp]
    (cmp3.op, cmp3.l, cmp3.r) should (be ((LE, darg, dc(33.8)))
                                   or be ((GE, dc(33.8), darg)))

    val cmp4 = cmp(EQ, farg, farg).asInstanceOf[Cmp]
    (cmp4.op, cmp4.l, cmp4.r) should be ((EQ, farg, farg))

    val cond = addSomeConditionNode()
    cmp(NE, CondVal(cond), ic(0)) should be (cond)
    cmp(EQ, CondVal(cond), ic(0)) should be (Not(cond))
    cmp(NE, CondVal(true)(cond), ic(0)) should be (Not(cond))
    cmp(EQ, CondVal(true)(cond), ic(0)) should be (cond)

    val someNode = addNode()
    cmp(EQ, someNode, someNode) should be (True()) // regression test for no SOE during identity

    cmp(NE, Neg(IntType)(CondVal(cond)), IConst(0)) shouldBe cond
    cmp(EQ, Neg(IntType)(CondVal(cond)), IConst(0)) shouldBe Not(cond)
    cmp(NE, Neg(IntType)(CondVal(negated = true)(cond)), IConst(0)) shouldBe Not(cond)

    // Cangjie pattern of i1 negation:
    cmp(NE, xor(CondVal(cond), IConst(1)), IConst(0)) shouldBe Not(cond)
    cmp(EQ, xor(CondVal(cond), IConst(1)), IConst(0)) shouldBe cond
    cmp(NE, xor(CondVal(negated = true)(cond), IConst(1)), IConst(0)) shouldBe cond

    // With Max/Min values
    val intNode: Node = Param(IntType, 0)
    val longNode: Node = Param(LongType, 0)
    for ((op, v1, v2, res) <- Seq(
      (LE, intNode,  ic(Int.MaxValue),  true),
      (GE, intNode,  ic(Int.MinValue),  true),
      (LE, longNode, lc(Long.MaxValue), true),
      (GE, longNode, lc(Long.MinValue), true),
      (GT, intNode,  ic(Int.MaxValue),  false),
      (LT, intNode,  ic(Int.MinValue),  false),
      (GT, longNode, lc(Long.MaxValue), false),
      (LT, longNode, lc(Long.MinValue), false),
    )) {
      checkCmp(op, v1, v2, res)
    }
  }

  test("condval identity") {
    CondVal(True()) should be (ic(1))
    CondVal(False()) should be (ic(0))
    val x = Fake(ConditionType)
    CondVal(Not(x)) should be (CondVal(true)(x))
  }

  test("invalid float/double identities") {
    add(farg, neg(farg)) should not be (fc(0))
    sub(darg, darg) should not be (dc(0))

    // (1 + (2 + x)) -> (3 + x)
    add(fc(1), add(fc(2), farg)) should not be (add(fc(3), farg))
  }

  test("not") {
    import Condition._

    Not(True()) should be (False())
    Not(False()) should be (True())

    val cond = addSomeConditionNode()
    Not(Not(cond)) should be (cond)

    Not(Cmp(LongType, UGT)(larg, lc(42))) should be (Cmp(LongType, ULE)(larg, lc(42)))
    Not(Cmp(FloatType, LE)(farg, fc(3.14f))) should be (Cmp(FloatType, GT_OR_UNORDERED)(farg, fc(3.14f)))
  }

  test("WeakCast") {
    val t = FakeType(symlevel.TypeKind.INTERFACE)
    val obj = addObjNode()

    {
      val wc = WeakCast(t)(obj, InstanceOf(sig(t))(obj)).asInstanceOf[WeakCast]
      wc.hasDominatingCheck shouldBe true
      wc.dominatingCheck shouldBe an[InstanceOf]
    }

    { // Regression test for JET-11600.
      val wc = WeakCast(t)(obj, CondVal(Cmp(TRefType, Condition.NE)(obj, Null()))).asInstanceOf[WeakCast]
      wc.hasDominatingCheck shouldBe false
      wc.dominatingCheck shouldBe WeakCast.NoCheck()
    }
  }

  test("BoxedValue identity") {

    def findField(tpe: FakeType, name: String) = tpe.findDeclaredFieldOrNull(XString.ascii(name))

    val integerSymType = FakeType.create(classOf[java.lang.Integer])

    // Pre-init type resolution cache for RTStructs
    env.typesResolution.put(integerSymType.getXName, integerSymType)

    val integerType = Java.Lang.Integer
    integerType.symType shouldBe integerSymType

    val wrongField = findField(integerSymType, "MAX_VALUE")

    val stringValueField = findField(FakeType.create(classOf[java.lang.String]), "value")

    makeCFG(0)
    makeNodes { at =>
      at(0)
      val primitiveInt = Fake(ValueType(integerType.kind))
      val boxedInt = BoxedValue(integerType)(primitiveInt)

      GetField(integerType.value)(boxedInt) should be (primitiveInt)

      // Regression test for JET-15309.
      GetField(wrongField)(boxedInt) should not be primitiveInt
      GetField(stringValueField)(boxedInt) should not be primitiveInt
    }
  }

}
