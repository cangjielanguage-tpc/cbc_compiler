/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.cbc.FieldReference
import com.huawei.excelsior.jet.assembler.{AsmType, Symbol, Width}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{bitsInByte, isStandalone}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.*
import com.huawei.excelsior.jet.compiler.bytecode.{ArithOp, BytecodeTypeKind, CompareOp}
import com.huawei.excelsior.jet.compiler.debug.info.DebugLocalVar
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Universe}
import com.huawei.excelsior.jet.compiler.options.BoolOption.UseIsa12
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement
import com.huawei.excelsior.jet.compiler.{Domain, PreparationRequired, RTSProc, symlevel}
import xscala.util.MathUtils.*

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.PartialFunction.{cond, condOpt}
import scala.annotation.{nowarn, tailrec}

/**
 * Simple nodes are nodes, that corresponds to simple operations.
 *
 * 1) Arithmetic
 * 2) Comparison
 * 3) Numeric conversions
 * 4) Conditional move
 * 5) Class object
 * 6) Constants
 * 7) Method args
 *
 * @author paul
 * @author cypok
 * @author conwor
 */

trait SimpleNodes { self: Universe with Nodes =>

  /////////////////////////////////////////
  // Arithmetic

  abstract class BinaryOp protected(proto: BinaryOp.Proto[_ <: BinaryOp]) extends NodeWithFixedArgs(proto) {
    def l: Node = arg(proto.lIdx)
    def r: Node = arg(proto.rIdx)

    def lEdge: Edge = inEdge(proto.lIdx)
    def rEdge: Edge = inEdge(proto.rIdx)

    def swapArgs(): Unit = {
      val tmp = l
      updateArg(proto.lIdx, r)
      updateArg(proto.rIdx, tmp)
    }
  }

  object BinaryOp {
    abstract class Proto[N <: BinaryOp](auxArgTypes: Type*)(argType: Type)(resType: Type)
      extends FixedArgs[N](auxArgTypes ++ Seq(argType, argType): _*)(resType) {
      def lIdx = auxArgTypes.size
      def rIdx = lIdx + 1
    }

    abstract class Floating[N <: BinaryOp](argType: Type)(resType: Type)
      extends Proto[N]()(argType)(resType)

    abstract class Controlled[N <: BinaryOp with ControlledNode](argType: Type)(resType: Type)
      extends Proto[N](ControlType)(argType)(resType)

    abstract class SpinalWithMemory[N <: BinaryOp with SpinalNode](argType: Type)(resType: Type)
      extends Proto[N](ControlType, MemoryType)(argType)(resType)

    def unapply(op: BinaryOp): Option[(Node, Node)] = Some((op.l, op.r))
  }

  abstract class CommutativeBinaryOp protected (proto: BinaryOp.Proto[_ <: BinaryOp]) extends BinaryOp(proto)

  abstract class ArithCommutativeOp protected(proto: BinaryOp.Proto[_ <: BinaryOp]) extends CommutativeBinaryOp(proto)

  object ArithCommutativeOp {
    object SecondArg extends EdgeMatcher[ArithCommutativeOp](1)
  }

  class Add private (proto: Add.Proto) extends ArithCommutativeOp(proto) with FloatingNode

  object Add {
    case class Proto private[Add] (keyType: Type) extends BinaryOp.Floating[Add](keyType)(keyType) {
      def newInstance() = new Add(this)
    }

    def proto(tpe: Type) = Prototype.intern(Proto(tpe))

    def apply(l: Node, r: Node) = proto((l.tpe | r.tpe) ensuring (_.isNumericType))(l, r)
    def unapply(add: Add): Option[(Node, Node)] = Some((add.l, add.r))
    object SecondArg extends EdgeMatcher[Add](1)
  }

  object AddWithIntegralConst {
    /** Matches add of anything with const. */
    def unapply(op: BinaryOp): Option[(Node, Long)] = condOpt(op) {
      case Add(IntegralConst(c), x) => (x, c)
      case Add(x, IntegralConst(c)) => (x, c)
      case Sub(x, IntegralConst(c)) => (x, -c) // it's correct even if c == Int.MinValue or Long.MinValue
      // CheckedOp arguments aren't sorted (can be only in case of Add and Mul)
      case CheckedOp(CheckedOp.Kind.ADD, x, IntegralConst(c)) => (x, c)
      case CheckedOp(CheckedOp.Kind.ADD, IntegralConst(c), x) => (x, c)
      case CheckedOp(CheckedOp.Kind.SUB, x, IntegralConst(c)) => (x, -c)
      case op @ CheckedOp(CheckedOp.Kind.SUB, IntegralConst(c), x) =>
        assert(c != minExtended(op.width.nbits)) // if this explodes, then we didn't optimize this in simplify
        (x, -c)
    }
  }

  class Sub private (proto: Sub.Proto) extends BinaryOp(proto) with FloatingNode

  object Sub {
    case class Proto private[Sub] (keyType: Type) extends BinaryOp.Floating[Sub](keyType)(keyType) {
      def newInstance() = new Sub(this)
    }

    def proto(tpe: Type) = Prototype.intern(Proto(tpe))

    def apply(l: Node, r: Node) = proto((l.tpe | r.tpe) ensuring (_.isNumericType))(l, r)
    def unapply(sub: Sub): Option[(Node, Node)] = Some((sub.l, sub.r))
    object SecondArg extends EdgeMatcher[Sub](1)
  }

  class Mul private (proto: Mul.Proto) extends ArithCommutativeOp(proto) with FloatingNode

  object Mul {
    case class Proto private[Mul] (keyType: Type) extends BinaryOp.Floating[Mul](keyType)(keyType) {
      def newInstance() = new Mul(this)
    }

    def proto(tpe: Type) = Prototype.intern(Proto(tpe))

    def apply(l: Node, r: Node) = proto((l.tpe | r.tpe) ensuring (_.isNumericType))(l, r)
    def unapply(mul: Mul): Option[(Node, Node)] = Some((mul.l, mul.r))
    object SecondArg extends EdgeMatcher[Mul](1)
  }
  
  class MSub private (proto: MSub.Proto) extends FloatingNodeWithFixedArgs(proto) {
    /** op3 - op1 * op2 */
    def op1 = arg(0)
    def op2 = arg(1)
    def op3 = arg(2)
  }
  
  object MSub {
    case class Proto private[MSub] (keyType: Type) extends FixedArgs[MSub](keyType, keyType, keyType)(keyType) {
      assert(keyType.isIntegralType)
      def newInstance() = new MSub(this)
    }
    
    def apply(keyType: Type) = Prototype.intern(Proto(keyType))
    def unapply(msub: MSub) = Some((msub.op1, msub.op2, msub.op3))
  }


  /** Gets the high-order bits of product */
  class MulH private (proto: MulH.Proto) extends ArithCommutativeOp(proto) with FloatingNode

  object MulH {
    case class Proto private[MulH] (keyType: Type) extends BinaryOp.Floating[MulH](keyType)(keyType) {
      assert(keyType.isIntegralType)
      def newInstance() = new MulH(this)
    }

    def proto(tpe: Type) = Prototype.intern(Proto(tpe))
    def apply(l: Node, r: Node) = proto((l.tpe | r.tpe) ensuring (_.isIntegralType))(l, r)

    object SecondArg extends EdgeMatcher[MulH](1)
  }


  /** Gets the high-order bits of unsigned product */
  class UMulH private (proto: UMulH.Proto) extends ArithCommutativeOp(proto) with FloatingNode

  object UMulH {
    case class Proto private[UMulH] (keyType: Type) extends BinaryOp.Floating[UMulH](keyType)(keyType) {
      assert(keyType.isIntegralType)
      def newInstance() = new UMulH(this)
    }

    def proto(tpe: Type) = Prototype.intern(Proto(tpe))
    def apply(l: Node, r: Node) = proto((l.tpe | r.tpe) ensuring (_.isIntegralType))(l, r)

    object SecondArg extends EdgeMatcher[UMulH](1)
  }

  class Pow private(proto: Pow.Proto) extends ArithCommutativeOp(proto) with FloatingNode

  object Pow {
    case class Proto private[Pow](keyType: Type) extends BinaryOp.Floating[Pow](keyType)(keyType) {
      def newInstance() = new Pow(this)
    }

    def proto(tpe: Type) = Prototype.intern(Proto(tpe))

    def apply(l: Node, r: Node) = proto((l.tpe | r.tpe) ensuring (_.isNumericType))(l, r)

    def unapply(pow: Pow): Option[(Node, Node)] = Some((pow.l, pow.r))

    object SecondArg extends EdgeMatcher[Pow](1)
  }

  class CheckedOp(proto: CheckedOp.Proto) extends BinaryOp(proto) with SpinalNode with CanThrow with ProducesValue {
    def throwProc: RTSProc = (kind, managed) match {
      case (CheckedOp.Kind.ADD, true) => RTSProc.JR_ThrowAJAddOverflowException
      case (CheckedOp.Kind.SUB, true) => RTSProc.JR_ThrowAJSubOverflowException
      case (CheckedOp.Kind.MUL, true) => RTSProc.JR_ThrowAJMulOverflowException
      case (CheckedOp.Kind.DIV, true) => RTSProc.JR_ThrowAJDivOverflowException
      case (CheckedOp.Kind.POW, true) => shouldNotReachHere("not implemented")
      case (CheckedOp.Kind.ADD, false) => RTSProc.JR_ThrowManualAJAddOverflowException
      case (CheckedOp.Kind.SUB, false) => RTSProc.JR_ThrowManualAJSubOverflowException
      case (CheckedOp.Kind.MUL, false) => RTSProc.JR_ThrowManualAJMulOverflowException
      case (CheckedOp.Kind.DIV, false) => RTSProc.JR_ThrowManualAJDivOverflowException
      case (CheckedOp.Kind.POW, false) => RTSProc.JR_ThrowManualAJMulOverflowException
    }

    def width = proto.width
    def signed = proto.signed
    def asmType = proto.asmType
    def kind = proto.kind
    def managed = proto.managed
  }

  object CheckedOp {
    enum Kind:
      case ADD, SUB, MUL, DIV, POW
    import Kind._

    def normalizeArg(tpe: Type, from: Width, signed: Boolean, x: Node): Node = BitFieldExtract.BFX(tpe, 0, from.nbits, signed, x)

    def replaceWithUncheckedCopy(n: CheckedOp): Unit = {
      val tpe = n.tpe
      val (l, r) = (n.l, n.r)

      val replacement = (n.kind: @unchecked) match {
        case ADD => Add(l, r)
        case SUB => Sub(l, r)
        case MUL => Mul(l, r)
        case DIV =>
          insertCodeBefore(n) {
            DivisorCheck()(r)
            IDivRemOp(tpe, !n.signed, n.kind == DIV)(l, r)
          }
      }

      strikeOutWithValueUses(n, replacement)
    }

    case class Proto(keyType: Type, kind: Kind, asmType: AsmType, managed: Boolean)
      extends BinaryOp.SpinalWithMemory[CheckedOp](keyType)(keyType) with ControlValueTagged[CheckedOp] {
      def width = asmType.width
      def signed = asmType.signed
      override protected def newInstance() = new CheckedOp(this)

      override def apply(args: Node*) = {
        val res = super.apply(args: _*)
        res match {
          case n @ CheckedOp(CheckedOp.Kind.ADD | CheckedOp.Kind.MUL, _, _) if !areArgsSorted(n.l, n.r) => n.swapArgs()
          case _ =>
        }
        res
      }
    }

    def apply(tpe: Type, width: Width, kind: Kind, signed: Boolean, managed: Boolean): Proto = apply(tpe, kind, AsmType.integral(width, signed), managed)
    def apply(tpe: Type, kind: Kind, asmType: AsmType, managed: Boolean): Proto = Prototype.intern(Proto(tpe, kind, asmType, managed))
    def unapply(op: CheckedOp): Option[(Kind, Node, Node)] = Some((op.kind, op.l, op.r))

    object FirstValueArg extends EdgeMatcher[CheckedOp](2)
    object SecondtValueArg extends EdgeMatcher[CheckedOp](3)
  }


  trait MayHaveImplicitCheck extends Node {
    import Group.AttachReason.IMPLICIT_CHECK_ARG
    def hasImplicitCheck = hasAttachedByReason(IMPLICIT_CHECK_ARG)
    def implicitCheck: PureCheck = singleElement(attachedByReason(IMPLICIT_CHECK_ARG)).asInstanceOf[PureCheck]
  }

  object WithImplicitCheck {
    def unapply(x: MayHaveImplicitCheck): Option[PureCheck] =
      if (x.hasImplicitCheck) Some(x.implicitCheck) else None
  }

  class DivisorCheck private (proto: DivisorCheck.Proto) extends PureCheck(proto) with ThrowingPureCheck with NotProducesValue {
    def divisor = arg(2)
    def divisor_=(n: Node): Unit = updateArg(2, n)

    override def throwInfo =
      inlineContext.method.getDomain match {
        case Domain.AJ => (RTSProc.JR_ThrowAJArithmeticException, Seq())
        case Domain.JAVA => (RTSProc.JR_ThrowArithmeticException, Seq())
        case Domain.CANGJIE => (RTSProc.JR_ThrowCJArithmeticException, Seq())
        case Domain.SCALA => (RTSProc.JR_ThrowScalaArithmeticException, Seq())
      }
  }

  object DivisorCheck {
    case class Proto private[DivisorCheck] (trusted: Boolean, argType: Type)
      extends PureCheckPrototype[DivisorCheck](ControlType, MemoryType, argType)(ControlType)() with ControlTagged[DivisorCheck] {
      assert(argType.isIntegralType)

      def newInstance() = new DivisorCheck(this)
    }

    def proto(trusted: Boolean, argType: Type): Proto = Prototype.intern(Proto(trusted, argType))

    def apply(trusted: Boolean = false)(divisor: Node) = proto(trusted, divisor.tpe)(divisor)
    def unapply(n: DivisorCheck): Option[Node] = Some(n.divisor)
  }

  class IDivRemOp private (proto: IDivRemOp.Proto)
    extends BinaryOp(proto) with ControlledNode with FloatingNode with MayHaveImplicitCheck {

    def isUnsigned = proto.isUnsigned
    def isDiv = proto.isDiv

    override def name: String = simpleName + "[" +
      (if (isUnsigned) "unsigned" else "signed") + " " +
      (if (isDiv) "div" else "rem") + "]"
  }

  object IDivRemOp {
    case class Proto private[IDivRemOp](keyType: Type, isUnsigned: Boolean, isDiv: Boolean)
      extends BinaryOp.Controlled[IDivRemOp](keyType)(keyType) {

      assert(keyType.isIntegralType)
      def newInstance() = new IDivRemOp(this)
    }

    object DividendEdge extends EdgeMatcher[IDivRemOp](1)
    object DivisorEdge extends EdgeMatcher[IDivRemOp](2)

    def apply(tpe: Type, isUnsigned: Boolean, isDiv: Boolean) = Prototype.intern(IDivRemOp.Proto(tpe, isUnsigned, isDiv))
  }

  object IDivRemByConstOp {
    def unapply(n: IDivRemOp) = condOpt(n.r) {
      case IntegralConst(c) => c
    }
  }

  object IDiv {
    def apply(tpe: Type) = IDivRemOp(tpe, isUnsigned = false, isDiv = true)
    def unapply(n: IDivRemOp) = !n.isUnsigned && n.isDiv
  }

  object IRem {
    def apply(tpe: Type) = IDivRemOp(tpe, isUnsigned = false, isDiv = false)
    def unapply(n: IDivRemOp) = !n.isUnsigned && !n.isDiv
  }

  object UDiv {
    def apply(tpe: Type) = IDivRemOp(tpe, isUnsigned = true, isDiv = true)
    def unapply(n: IDivRemOp) = n.isUnsigned && n.isDiv
  }

  object URem {
    def apply(tpe: Type) = IDivRemOp(tpe, isUnsigned = true, isDiv = false)
    def unapply(n: IDivRemOp) = n.isUnsigned && !n.isDiv
  }


  class FDiv private (proto: FDiv.Proto) extends BinaryOp(proto) with FloatingNode

  object FDiv {
    case class Proto private[FDiv] (keyType: Type) extends BinaryOp.Floating[FDiv](keyType)(keyType) {
      assert(keyType.isFloatingPointType)
      def newInstance() = new FDiv(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
    object SecondArg extends EdgeMatcher[Mul](1)
  }


  object FRem {
    def apply(tpe: Type) = {
      val intrinsic = tpe match {
        case DoubleType => Java.Lang.MathIntrinsic.D_REM
        case FloatType => Java.Lang.MathIntrinsic.F_REM
        case _ => shouldNotReachHere(tpe)
      }
      MathIntrinsic(intrinsic)
    }
  }


  abstract class LogicalBinaryOp protected (proto: BinaryOp.Proto[_ <: BinaryOp]) extends CommutativeBinaryOp(proto) {
    require(tpe.isIntegralType)
  }

  object LogicalBinaryOp {
    object SecondArg extends EdgeMatcher[LogicalBinaryOp](1)
    def unapply(op: LogicalBinaryOp): Option[(Node, Node)] = Some((op.l, op.r))
  }


  class And private (proto: And.Proto) extends LogicalBinaryOp(proto) with FloatingNode

  object And {
    case class Proto private[And] (keyType: Type) extends BinaryOp.Floating[And](keyType)(keyType) {
      def newInstance() = new And(this)
    }

    def proto(tpe: Type) = Prototype.intern(Proto(tpe))
    def apply(l: Node, r: Node) = proto((l.tpe | r.tpe) ensuring (_.isIntegralType))(l, r)
    def unapply(and: And): Option[(Node, Node)] = Some((and.l, and.r))
  }

  object AndWithIConst {
    def unapply(node: Node): Option[(Node, Int)] = condOpt(node) {
      case And(l, IConst(c)) => (l, c)
      case And(IConst(c), l) => (l, c)
      case bfx @ BitFieldExtract(0, size, false, arg) if bfx.tpe == IntType && bfx.argType == IntType =>
        (arg, rightNBits32(size))
    }
  }


  class Or private (proto: Or.Proto) extends LogicalBinaryOp(proto) with FloatingNode

  object Or {
    case class Proto private[Or] (keyType: Type) extends BinaryOp.Floating[Or](keyType)(keyType) {
      def newInstance() = new Or(this)
    }

    def proto(tpe: Type) = Prototype.intern(Proto(tpe))
    def apply(l: Node, r: Node) = proto((l.tpe | r.tpe) ensuring (_.isIntegralType))(l, r)
  }


  class Xor private (proto: Xor.Proto) extends LogicalBinaryOp(proto) with FloatingNode

  object Xor {
    case class Proto private[Xor] (keyType: Type) extends BinaryOp.Floating[Xor](keyType)(keyType) {
      def newInstance() = new Xor(this)
    }

    def proto(tpe: Type) = Prototype.intern(Proto(tpe))
    def apply(l: Node, r: Node) = proto((l.tpe | r.tpe) ensuring (_.isIntegralType))(l, r)
    def unapply(n: Xor): Option[(Node, Node)] = Some((n.l, n.r))
  }


  class Neg private (proto: Neg.Proto) extends FloatingNodeWithFixedArgs(proto)

  object Neg {
    case class Proto private[Neg] (keyType: Type) extends FixedArgs[Neg](keyType)(keyType) {
      def newInstance() = new Neg(this)
    }

    def apply(tpe: Type) = Prototype.intern(Proto(tpe))
    def unapply(n: Neg): Option[Node] = Some(n.arg)
  }


  class Shift private (proto: Shift.Proto) extends FloatingNodeWithFixedArgs(proto) {
    assert(op == LSL || op == ASR || op == LSR)
    def op = proto.op

    /** Value to be shifted. */
    def value = arg(0)

    /** Number of places to shift. */
    private def numArg = Shift.NumEdge.index
    def num = arg(numArg)
    def num_=(n: Node): Unit = updateArg(numArg, n)
  }

  object Shift {
    case class Proto private[Shift] (keyType: Type, op: ArithOp) extends FixedArgs[Shift](keyType, IntType)(keyType) {
      assert(keyType.isIntegralType)
      def newInstance() = new Shift(this)
    }

    def proto(keyType: Type, op: ArithOp) = Prototype.intern(Proto(keyType, op))

    def apply(op: ArithOp, l: Node, r: Node) = {
      assert(op.isShift)
      proto(l.tpe, op)(l, r)
    }

    def unapply(shift: Shift) = Some((shift.op, shift.value, shift.num))

    object NumEdge extends EdgeMatcher[Shift](1)
  }

  object ShiftByConst {
    def masked(num: Int, tpe: Type) = tpe match {
      case IntType => num & 0x1F
      case LongType => num & 0x3F
    }

    def unapply(node: Node) = condOpt(node) {
      case bfx @ BitFieldExtract(offset, size, sx, arg) if bfx.tpe == arg.tpe && offset + size == typeSizeInBits(bfx.tpe) =>
        (if (sx) ASR else LSR, arg, offset)
      case Shift(LSL, arg, IConst(offset)) =>
        (LSL, arg, masked(offset, node.tpe))
    }

    def apply(tpe: Type, op: ArithOp, offset: Int, arg: Node) = {
      val maskedOffset = masked(offset, tpe)
      op match {
        case ASR | LSR => BitFieldExtract(tpe, maskedOffset, typeSizeInBits(tpe) - maskedOffset, op == ASR, arg)
        case LSL => Shift(LSL, arg, IConst(maskedOffset))
        case _ => shouldNotReachHere()
      }
    }
  }


  class BitCount private (proto: BitCount.Proto) extends FloatingNodeWithFixedArgs(proto) {
    assert(kind != BitCount.Kind.HIGHEST_BIT || (currentPhase >= CompilerPhase.Lowering))

    def kind = proto.kind
    def argTpe = proto.argType(0)

    def rtMethod(): symlevel.Method = {
      import BitCount.Kind._
      import Com.Huawei.Excelsior.Aj.Lang.UnmanagedMath._
      (kind, argTpe) match {
        case (BIT_COUNT, IntType) => bitCountInt
        case (BIT_COUNT, LongType) => bitCountLong

        case (x, y) =>
          shouldNotReachHere("feel free to extend Com.Huawei.Excelsior.Aj.Lang.UnmanagedMath description for: " + x + " / " + y)
      }
    }
  }

  object BitCount {
    case class Proto private[BitCount] (keyType: Type, kind: Kind) extends FixedArgs[BitCount](keyType)(IntType) {
      assert(keyType.isIntegralType)
      def newInstance() = new BitCount(this)
    }

    def apply(keyType: Type, kind: Kind) = Prototype.intern(Proto(keyType, kind))
    def unapply(node: BitCount) = Some(node.kind)

    enum Kind {
      case LEADING_ZEROS
      case TRAILING_ZEROS
      /** Index of most significant bit or -1. */
      case HIGHEST_BIT
      case BIT_COUNT
    }

    def leadingZeros (tpe: Type, arg: Node) = apply(tpe, Kind.LEADING_ZEROS)(arg)
    def trailingZeros(tpe: Type, arg: Node) = apply(tpe, Kind.TRAILING_ZEROS)(arg)
    def highestBit   (tpe: Type, arg: Node) = apply(tpe, Kind.HIGHEST_BIT)(arg)
    def bitCount     (tpe: Type, arg: Node) = apply(tpe, Kind.BIT_COUNT)(arg)
  }

  class BitSwap private (proto: BitSwap.Proto) extends FloatingNodeWithFixedArgs(proto)

  object BitSwap {
    case class Proto private[BitSwap] (keyType: Type) extends FixedArgs[BitSwap](keyType)(keyType) {
      assert(keyType.isIntegralType)
      def newInstance() = new BitSwap(this)
    }

    def proto(keyType: Type) = Prototype.intern(Proto(keyType))
    def apply(tpe: Type)(arg: Node) = proto(tpe)(arg)
  }

  /** Node for bytecode *cmpl, *cmpg, lcmp instructions that produce -1, 0, 1 results */
  class ThreeCmp private (proto: ThreeCmp.Proto) extends FloatingNodeWithFixedArgs(proto) with CompositeNode {
    assert( (proto.keyType.isFloatingPointType && (op == CMPL || op == CMPG)) ||
            (proto.keyType == LongType && op == CMP))
    def op = proto.op

    def l = arg(0)
    def r = arg(1)
  }

  object ThreeCmp {
    case class Proto private[ThreeCmp] (keyType: Type, op: ArithOp) extends FixedArgs[ThreeCmp](keyType, keyType)(IntType) {
      def newInstance() = new ThreeCmp(this)
    }

    def apply(keyType: Type, op: ArithOp) = Prototype.intern(Proto(keyType, op))
    def unapply(cmp: ThreeCmp): Option[(ArithOp, Node, Node)] = Some((cmp.op, cmp.l, cmp.r))
  }


  /////////////////////////////////////////
  // Comparison

  /**
   * Condition codes for comparison, test operations.
   */
  enum Condition {
    case EQ, NE, GE, GT, LT, LE

    // Unsigned comparisons.
    case UGE, UGT, ULT, ULE

    // Floating-point comparisons with `true` result for unordered arguments (NaN).
    // Note that `NE` also yields `true` for NaN (but it is a default behaviour).
    case GE_OR_UNORDERED, GT_OR_UNORDERED, LT_OR_UNORDERED, LE_OR_UNORDERED

    /** Returns operation of `Cmp` which is equal to `Cmp` with given `op` and swapped arguments. . */
    def swap: Condition = this match {
      case EQ  => EQ
      case NE  => NE
      case GE  => LE
      case GT  => LT
      case LT  => GT
      case LE  => GE
      case UGE => ULE
      case UGT => ULT
      case ULT => UGT
      case ULE => UGE
      case GE_OR_UNORDERED => LE_OR_UNORDERED
      case GT_OR_UNORDERED => LT_OR_UNORDERED
      case LT_OR_UNORDERED => GT_OR_UNORDERED
      case LE_OR_UNORDERED => GE_OR_UNORDERED
    }

    /** Returns operation of `Cmp` which is equal to `Cmp` with `this` condition and negated result. . */
    def negate(isFP: Boolean): Condition = {
      if (isFP) {
        this match {
          case EQ  => NE
          case NE  => EQ
          case GE  => LT_OR_UNORDERED
          case GT  => LE_OR_UNORDERED
          case LT  => GE_OR_UNORDERED
          case LE  => GT_OR_UNORDERED
          case GE_OR_UNORDERED => LT
          case GT_OR_UNORDERED => LE
          case LT_OR_UNORDERED => GE
          case LE_OR_UNORDERED => GT
          case _   => shouldNotReachHere(s"Unexpected FP cmp $this")
        }
      } else {
        this match {
          case EQ  => NE
          case NE  => EQ
          case GE  => LT
          case GT  => LE
          case LT  => GE
          case LE  => GT
          case UGE => ULT
          case UGT => ULE
          case ULT => UGE
          case ULE => UGT
          case _   => shouldNotReachHere(s"Unexpected non-FP cmp $this")
        }
      }
    }

    def isUnsigned: Boolean =
      this == UGE || this == ULT || this == UGT || this == ULE
  }

  object Condition {
    /** Conversion from bytecode `CompareOp`. */
    def apply(compareOp: CompareOp): Condition = {
      compareOp match {
        case CompareOp.EQ => EQ
        case CompareOp.NE => NE
        case CompareOp.GE => GE
        case CompareOp.GT => GT
        case CompareOp.LT => LT
        case CompareOp.LE => LE
      }
    }
  }

  /** Combination of [[Cmp]] and [[CAS]] nodes attached to [[If]]. Amd64 specific.
    * Because this node is always attached to [[If]], it is floating node.
    * In group with [[If]] they are acting like another [[BlockEnd]] implementation.
    * */
  class CmpCAS private (proto: CmpCAS.Proto) extends NodeWithFixedArgs(proto) with FloatingNode with FlagProducer {
    def op = proto.op
    def keyType = proto.keyType
    def accessType = proto.accessType

    def addr = arg(CmpCAS.AddrEdge.index)
    def expectedValue = arg(CmpCAS.ExpectedValueEdge.index)
    def newValue = arg(CmpCAS.NewValueEdge.index)
  }

  object CmpCAS {
    case class Proto private[CmpCAS] (keyType: Type, op: Condition, accessType: AsmType) extends FixedArgs(AddrType, keyType, keyType)(ConditionType) {
      assert(keyType.isIntegralType || keyType.isFloatingPointType || keyType.isTraceableRefType || keyType == ThinType, s"$keyType")

      def newInstance() = new CmpCAS(this)
    }

    def proto(tpe: Type, op: Condition, accessType: AsmType) = Prototype.intern(Proto(tpe, op, accessType))
    def apply(tpe: Type, op: Condition, accessType: AsmType)(addr: Node, expectedValue: Node, newValue: Node) =
      proto(tpe, op, accessType)(addr, expectedValue, newValue)

    object AddrEdge extends EdgeMatcher[CmpCAS](0)
    object ExpectedValueEdge extends EdgeMatcher[CmpCAS](1)
    object NewValueEdge extends EdgeMatcher[CmpCAS](2)
  }

  class Cmp private (proto: Cmp.Proto) extends BinaryOp(proto) with FloatingNode with FlagProducer {
    def op = proto.op
    def keyType = proto.keyType
  }

  object Cmp {
    case class Proto private[Cmp] (keyType: Type, op: Condition) extends BinaryOp.Floating[Cmp](keyType)(ConditionType) {
      assert(keyType.isIntegralType || keyType.isFloatingPointType ||
        keyType.isTraceableRefType || keyType == ThinType)

      def newInstance() = new Cmp(this)
    }

    def apply(tpe: Type, op: Condition) = Prototype.intern(Proto(tpe, op))
    def unapply(cmp: Cmp) = Some((cmp.op, cmp.l, cmp.r))

    /** Creates new Cmp with swapped args and negated `cmp.op`. */
    def withSwappedArgs(cmp: Cmp) = Cmp(cmp.keyType, cmp.op.swap)(cmp.r, cmp.l)

    object SecondArg extends EdgeMatcher[Cmp](1)
  }

  object CmpOrTest {
    def unapply(n: Node): Option[(Condition, Node, Node)] = n match {
      case c: Cmp => Cmp.unapply(c)
      case t: Test => Test.unapply(t)
      case _ => None
    }
  }

  /** Important property of such comparison is that it could be generated both for rich and for plain argument.
    */
  object CmpWithNull {
    def unapply(cmp: Cmp): Boolean = cmp match {
      case Cmp(_, _, Null()) => true
      case Cmp(_, Null(), _) => true // this object is used in consistency checks, where sorted args invariant may not hold
      case _ => false
    }
  }

  object IsZero {
    def apply(n: Node) = Cmp(n.tpe, Condition.EQ)(n, IntegralConst(n.tpe)(0))
    def unapply(n: Node) = condOpt(n) {
      case Cmp(Condition.EQ, x, IntegralConst(0)) => x
    }
  }

  object NonZero {
    def apply(n: Node) = Cmp(n.tpe, Condition.NE)(n, IntegralConst(n.tpe)(0))
    def unapply(n: Node) = condOpt(n) {
      case Cmp(Condition.NE, x, IntegralConst(0)) => x
    }
  }

  object ZeroComparison {
    def unapply(n: Node) = condOpt(n) {
      case NonZero(x) => x
      case IsZero(x) => x
    }
  }

  class Test private (proto: Test.Proto) extends CommutativeBinaryOp(proto) with FloatingNode with FlagProducer {
    def op = proto.op
    def keyType = proto.keyType
  }

  object Test {
    case class Proto private[Test] (keyType: Type, op: Condition) extends BinaryOp.Floating[Test](keyType)(ConditionType) {
      assert(keyType.isIntegralType)
      def newInstance() = new Test(this)
    }

    def apply(tpe: Type, op: Condition) = Prototype.intern(Proto(tpe, op))
    def unapply(test: Test) = Some(test.op, test.l, test.r)
    object SecondArg extends EdgeMatcher[Test](1)
  }


  /////////////////////////////////////////
  // Bit Field Extract

  /** Extracts any number of bits at any position from integral value.
    * This node encapsulates such operations as:
    *   - integral type casts
    *   - arithmetic/logical shift right
    *   - logical conjunction with continuous bit pattern
    * and their combinations.
    *
    * Could be grouped with memory read for using access type extension of load instructions.
    *
    * Examples:
    *   1. `signExtend(BYTE, INT) ->  BFX(INT,  offset = 0,  size = 8 , signExtension = true)`
    *   1. `truncate(INT, CHAR) ->    BFX(INT,  offset = 0,  size = 16, signExtension = false)`
    *   1. `zeroExtend(INT, LONG) ->  BFX(LONG, offset = 0,  size = 32, signExtension = false)`
    *   1. `ASR(LONG, 10) ->          BFX(LONG, offset = 10, size = 54, signExtension = true)`
    *   1. `AND(LONG, 0xFFFFFFFFL) -> BFX(LONG, offset = 0,  size = 32, signExtension = false)`
    */
  class BitFieldExtract(proto: BitFieldExtract.Proto) extends FloatingNodeWithFixedArgs(proto) {
    def argType = arg(0).tpe

    def offset = proto.offset
    def size = proto.size
    def signExtension = proto.signExtension

    def dataAligned = BitFieldExtract.dataAligned(offset, size)

    def offsetInBytes = {
      assert(dataAligned)
      offset / bitsInByte
    }

    def sizeInBytes = {
      assert(dataAligned)
      size / bitsInByte
    }

    def extensionIrrelevant = size == typeSizeInBits(tpe)
  }

  object BitFieldExtract {
    case class Proto private[BitFieldExtract](argType: Type, retType: Type, offset: Int, size: Int, signExtension: Boolean)
      extends FixedArgs[BitFieldExtract](argType)(retType) {
      assert(argType.isIntegralType)
      assert(retType.isIntegralType)

      def newInstance() = new BitFieldExtract(this)
    }

    private def dataAligned(offset: Int, size: Int) =
      isAligned(size, bitsInByte) && isAligned(offset, bitsInByte) &&
        isPowerOf2(size) && (offset == 0 || isAligned(offset, size))

    /** Create proto without bounds checks for deserialization. */
    def raw(argTpe: Type, tpe: Type, offset: Int, size: Int, signExtension: Boolean) =
      Prototype.intern(Proto(argTpe, tpe, offset, size, signExtension))

    def apply(tpe: Type, offset: Int, size: Int, signExtension: Boolean, arg: Node) =
      BFX(tpe, offset, size, signExtension, arg)

    def unapply(bfx: BitFieldExtract) = BFX.unapply(bfx)

    object BFX {
      def unapply(bfx: BitFieldExtract) = Some((bfx.offset, bfx.size, bfx.signExtension, bfx.arg))

      def apply(tpe: Type, offset: Int, size: Int, signExtension: Boolean, arg: Node) = arg match {
        case _: NoValue => arg
        case _ =>
          assert(offset >= 0 && size >= 0)
          assert((size <= typeSizeInBits(tpe)) && (size + offset <= typeSizeInBits(arg.tpe)))
          raw(arg.tpe, tpe, offset, size, signExtension)(arg)
      }
    }

    object Extend {
      def apply(tpe: Type, from: AsmType, signExtension: Boolean, arg: Node) = {
        BFX(tpe, 0, from.sizeInBits, signExtension, arg)
      }
      def apply(signExtension: Boolean, arg: Node) = {
        BFX(LongType, 0, typeSizeInBits(IntType), signExtension, arg)
      }
    }

    object ZeroExtend {
      def unapply(bfx: BitFieldExtract) = condOpt(bfx) {
        case BFX(0, 32, false, arg) if arg.tpe == IntType => arg
      }
      def apply(arg: Node) = Extend(signExtension = false, arg)

      object From32To64 {
        def unapply(bfx: BitFieldExtract) = condOpt(bfx) {
          case ZeroExtend(arg) if bfx.tpe == LongType => arg
        }
      }
    }

    object SignExtend {
      def unapply(bfx: BitFieldExtract) = condOpt(bfx) {
        case BFX(0, 32, true, arg) if arg.tpe == IntType => arg
      }
      def apply(arg: Node) = Extend(signExtension = true, arg)
    }

    object JavaShortIntegralExtend {
      def unapply(bfx: BitFieldExtract) = condOpt(bfx) {
        case BFX(0, 8,  sx, arg) if arg.tpe == IntType && bfx.tpe == IntType => (if (sx) AsmType.I8  else AsmType.U8,  arg)
        case BFX(0, 16, sx, arg) if arg.tpe == IntType && bfx.tpe == IntType => (if (sx) AsmType.I16 else AsmType.U16, arg)
      }
      def apply(from: AsmType, arg: Node) = {
        assert(from.isShortIntegral)
        BFX(IntType, 0, from.width.nbits, from.isSigned, arg)
      }
    }

    object Truncate {
      def unapply(bfx: BitFieldExtract) = condOpt(bfx) {
        case BFX(0, 32, _, arg) if bfx.tpe == IntType => arg
      }
      def apply(arg: Node) = {
        BFX(IntType, 0, typeSizeInBits(IntType), signExtension = false, arg)
      }
    }
  }


  /////////////////////////////////////////
  // Cast

  sealed abstract class Cast(proto: Cast.Proto[_ <: Cast]) extends FloatingNodeWithFixedArgs(proto) {
    def from = proto.from
    def to = proto.to
  }

  object Cast {
    sealed abstract class Proto[N <: Cast] protected (_from: Type, _to: Type) extends FixedArgs[N](_from)(_to) {
      def from: Type = _from
      def to: Type = _to
    }
  }

  class ReinterpretCast(proto: ReinterpretCast.Proto) extends Cast(proto)

  object ReinterpretCast {
    case class Proto private[ReinterpretCast](override val from: Type, override val to: Type)
      extends Cast.Proto[ReinterpretCast](from, to) {

      def newInstance() = new ReinterpretCast(this)
    }

    def apply(from: Type, to: Type) = Prototype.intern(Proto(from, to))
    def unapply(cast: ReinterpretCast) = Some(cast.from, cast.to, cast.arg)

    def skip(x: Node): Node = x match {
      case x: ReinterpretCast => x.arg
      case x => x
    }
  }

  class ValueConvert(proto: ValueConvert.Proto) extends Cast(proto) {
    def fromAsm = proto.fromType
    def toAsm = proto.toType
  }

  object ValueConvert {
    case class Proto private[ValueConvert](fromType: AsmType, toType: AsmType)
      extends Cast.Proto[ValueConvert](ValueType(fromType), ValueType(toType)) {

      def newInstance() = new ValueConvert(this)
    }

    def apply(from: AsmType, to: AsmType) = Prototype.intern(Proto(from, to))
    def unapply(cast: ValueConvert): Option[(AsmType, AsmType, Node)] = Some(cast.fromAsm, cast.toAsm, cast.arg)
  }

  object JavaConvert {
    def apply(from: BytecodeTypeKind, to: BytecodeTypeKind)(arg: Node): Node = {
      require(from.isIntegral || from.isFloatingPoint)
      require(to.isIntegral || to.isFloatingPoint)

      import BytecodeTypeKind.*
      import BitFieldExtract.*

      (from, to) match {
        case _ if from == to => arg

        case (DOUBLE | FLOAT, LONG | INT) |
             (LONG | INT, DOUBLE | FLOAT) |
             (DOUBLE, FLOAT) |
             (FLOAT, DOUBLE) =>
          ValueConvert(from.toAsm, to.toAsm)(arg)

        case (LONG, INT) => Truncate(arg)
        case (INT, LONG) => SignExtend(arg)

        case (INT, BOOLEAN | BYTE | SHORT | CHAR) =>
          assert(SignatureType.Primitive(to).toAsm == to.toAsm, s"${SignatureType.Primitive(to).toAsm} ${to.toAsm}")
          JavaShortIntegralExtend(to.toAsm, arg)

        case (BOOLEAN | BYTE | SHORT | CHAR, INT) =>
          assert(SignatureType.Primitive(from).toAsm == from.toAsm, s"${SignatureType.Primitive(from).toAsm} ${from.toAsm}")
          JavaShortIntegralExtend(from.toAsm, arg)

        case (_, _) =>
          val intArg = JavaConvert(from, INT)(arg)
          JavaConvert(INT, to)(intArg)
      }
    }
  }


  /////////////////////////////////////////
  // Conditional stuff

  /**
   * Condition value is the conversion of condition to 1 / 0 integer.
   */
  class CondVal private (proto: CondVal.Proto) extends FloatingNodeWithFixedArgs(proto) {
    def negated = proto.negated
    def condition = arg(0)

    override def name: String = simpleName + (if (negated) "[negated]" else "")
  }

  object CondVal {
    case class Proto private[CondVal] (negated: Boolean) extends FixedArgs[CondVal](ConditionType)(IntType) {
      def newInstance() = new CondVal(this)
    }

    def apply(args: Node*): Node = apply(false)(args: _*)
    def apply(negated: Boolean): CondVal.Proto = Prototype.intern(Proto(negated))
    def unapply(n: CondVal) = Some(n.negated, n.condition)
  }

  object CondValNE {
    def unapply(n: CondVal) = condOpt(n) {
      case CondVal(false, Cmp(Condition.NE, x, y)) => (x, y)
      case CondVal(true, Cmp(Condition.EQ, x, y)) => (x, y)
    }
  }


  class Not private extends FloatingNodeWithFixedArgs(Not)

  object Not extends FixedArgs[Not](ConditionType)(ConditionType) {
    def newInstance() = new Not()
    def unapply(n: Not) = Some(n.arg)
  }


  /////////////////////////////////////////
  // Miscellaneous nodes

  trait AnyClassObject extends Node {
    def symType: symlevel.Type
    require(!symType.isDeferred)
  }

  /** ClassObject for given class, array or primitive type. */
  class ClassObject private (proto: ClassObject.Proto) extends FloatingNodeWithFixedArgs(proto) with ContextDependentNode with Constant with AnyClassObject {
    assert(symType.isJavaReference || symType.isPrimitive || symType.isXScalaType)

    def symType = proto.symType

    // TODO: make data-flow node
    override def contextKey = if (!ContextTypesMap.loweredTypes) JVMState() else null
    override def requiredKeyType = new VMStateApprox()
  }

  object ClassObject {
    case class Proto private[ClassObject] (symType: symlevel.Type) extends FixedArgs[ClassObject](ControlType)(TRefType) {
      def newInstance() = new ClassObject(this)
    }

    def apply(symType: symlevel.Type) = Prototype.intern(Proto(symType))
    def unapply(n: ClassObject) = Some(n.symType)
  }

  /** ClassObject for given class, array or primitive type.
    * Throwing version for lowering.
    */
  class XClassObject private (proto: XClassObject.Proto)
    extends NodeWithFixedArgs(proto) with SpinalNode with CompositeNode with CanThrow with AnyClassObject with ProducesValue {

    def symType = proto.symType
  }

  object XClassObject {
    case class Proto private[XClassObject] (symType: symlevel.Type)
      extends FixedArgs[XClassObject](ControlType, MemoryType)(TRefType) with ControlValueTagged[XClassObject] {

      def newInstance() = new XClassObject(this)
    }

    def apply(symType: symlevel.Type) = Prototype.intern(Proto(symType))
    def unapply(n: XClassObject) = Some(n.symType)
  }


  /////////////////////////////////////////
  // Constant nodes

  /** Run-time constants. May be compile-time value (e.g. IConst) or some immutable object/symbol/address.
    *
    * Have the following properties:
    *   - considered invariant for all loops
    *   - do not interfere with rootMethod inline (considered as "free" nodes)
    *   - being call arguments, increase the chance of it's inline by CDI heuristic
    *   - excluded from GC-maps
    *   - allocated to Immediate in backend (or lowered like ConstString & ClassObject)
    *
    * Even if your node is "constant" by nature, make sure that these properties are suitable for it.
    */
  trait Constant extends FloatingNode

  /** Compile-time constants. Subset of run-time constants, used in Identities by rule:
    *   - data-flow node from compile-time value arguments should be always optimized into compile-time value
    *
    * The only one exception from this rule is reinterpret cast from AddrType constant to RefType. We cannot support
    * this case, because do not have node like RefConst with integral constant inside.
    */
  trait CompileTimeValue extends Constant

  object NumericalConst {
    def apply(v: Number): Node = v match {
      case v: java.lang.Integer => IConst(v.intValue)
      case v: java.lang.Long    => LConst(v.longValue)
      case v: java.lang.Float   => FConst(v.floatValue)
      case v: java.lang.Double  => DConst(v.doubleValue)
      case _ => shouldNotReachHere(v)
    }

    def unapply(n: Node): Option[Number] = n match {
      case IConst(v) => Some(v)
      case LConst(v) => Some(v)
      case FConst(v) => Some(v)
      case DConst(v) => Some(v)
      case _ => None
    }
  }

  object IntegralConst {
    def apply(tpe: Type)(v: Long): Node = tpe match {
      case LongType => LConst(v)
      // TODO: check overflow
      // Note that some usages rely on overflow (i.e. Identities). Introduce two versions? Introduce (Type, Int) version?
      case IntType  => IConst(v.toInt)
    }

    def unapply(c: Node): Option[Long] = condOpt(c) {
      case IConst(v) => v.toLong
      case LConst(v) => v
    }
  }

  object UnsignedIntegralConstMaxValue {
    def unapply(n: Node): Boolean = cond(n) {
      case IntegralConst(-1) => true
    }
  }

  object UnsignedIntegralConstMinValue {
    def unapply(n: Node): Boolean = cond(n) {
      case IntegralConst(0) => true
    }
  }

  object IntegralConstMaxValue {
    /** Returns value and isLong */
    def unapply(n: Node): Boolean = cond(n) {
      case IConst(v @ Int.MaxValue)  => true
      case LConst(v @ Long.MaxValue) => true
    }
  }

  object IntegralConstMinValue {
    /** Returns value and isLong */
    def unapply(n: Node): Boolean = cond(n) {
      case IConst(v @ Int.MinValue)  => true
      case LConst(v @ Long.MinValue) => true
    }
  }

  object DWordConst {
    def unapply(c: Node): Option[Int] = condOpt(c) {
      case IConst(v) => v
      case LConst(v) if isNBitsSigned(v, 32) => v.toInt
    }
  }

  object ULConst {
    def unapply(c: Node): Option[Long] = condOpt(c) {
      case IConst(v) => zeroExtend(v)
      case LConst(v) => v
    }
  }

  /** Integer constant. */
  class IConst private (val value: Int) extends CachedLeafNode[IConst](IntType) with CompileTimeValue {
    def cacheKey = value
  }

  object IConst {
    def apply(x: Int) = Prototype.intern(new IConst(x))()
    def unapply(x: IConst) = Some(x.value)
  }


  /** Long constant. */
  class LConst private (val value: Long) extends CachedLeafNode[LConst](LongType) with CompileTimeValue {
    def cacheKey = value
  }

  object LConst {
    def apply(x: Long) = Prototype.intern(new LConst(x))()
    def unapply(x: LConst) = Some(x.value)
  }


  /** Float constant. */
  class FConst private (val value: Float) extends CachedLeafNode[FConst](FloatType) with CompileTimeValue {
    private def bits = floatToRawIntBits(value)
    def cacheKey = bits
    override def name = s"$simpleName[$value # ${Integer.toHexString(bits)}]"
  }

  object FConst {
    def apply(x: Float) = Prototype.intern(new FConst(x))()
    def unapply(x: FConst) = Some(x.value)
  }


  /** Double constant. */
  class DConst private (val value: Double) extends CachedLeafNode[DConst](DoubleType) with CompileTimeValue {
    private def bits = doubleToRawLongBits(value)
    def cacheKey = bits
    override def name = s"$simpleName[$value # ${java.lang.Long.toHexString(bits)}]"
  }

  object DConst {
    def apply(x: Double) = Prototype.intern(new DConst(x))()
    def unapply(x: DConst) = Some(x.value)
  }

  object FloatingPointConst {
    def unapply(x: FConst | DConst): Option[Double] = x match {
      case FConst(x) => Some(x)
      case DConst(x) => Some(x)
    }
  }


  /** Node that represents a Java constant string. */
  class ConstString private (proto: ConstString.Proto) extends FloatingNodeWithFixedArgs(proto) with ContextDependentNode with Constant with CompositeNode {
    def str = proto.str
    def stringValue = str.value
    def strType = proto.strType

    override def contextKey = if (!ContextTypesMap.loweredTypes) JVMState() else null

    override def requiredKeyType = {
      val requiredType = PreparationRequired.forConstString(str)
      if (requiredType != null) {
        new VMStateApprox() withPreparation requiredType
      } else {
        new VMStateApprox()
      }
    }
  }

  object ConstString {
    case class Proto private[ConstString] (str: symlevel.ConstString, strType: symlevel.Type)
      extends FixedArgs[ConstString](ControlType)(TRefType) with PrototypeStrictNodeClass[ConstString, ConstString] {

      def newInstance() = new ConstString(this)
    }

    def apply(str: symlevel.ConstString, strType: symlevel.Type) = Prototype.intern(Proto(str, strType))
    def unapply(n: ConstString) = Some(n.stringValue)
  }

  object XStr {
    def unapply(xstr: XString): Option[String] = Some(xstr.toString)
  }


  class Void private extends LeafNode[Void](VoidType) with Constant

  object Void {
    private lazy val instance = new Void
    def apply() = instance()
  }


  class AnyNull private (keyType: Type) extends CachedLeafNode[AnyNull](keyType) with CompileTimeValue {
    assert(keyType.isTraceableRefType || keyType == ThinType)
    def cacheKey = keyType
  }

  object AnyNull {
    def apply(tpe: Type): AnyNull = Prototype.intern(new AnyNull(tpe))()
  }

  object Null {
    def apply(): Node = AnyNull(EopType.Null)
    def unapply(n: AnyNull): Boolean = n.tpe.isTraceableRefType
  }


  class ReturnAddress private extends LeafNode[ReturnAddress](ReturnAddressType) with FloatingNode

  object ReturnAddress {
    private lazy val instance = new ReturnAddress
    def apply() = instance()
  }

  // TODO: simplify hierarchy
  abstract class ConstCondition(val value: Boolean) extends LeafNode[ConstCondition](ConditionType) with CompileTimeValue

  class True private extends ConstCondition(true)

  object True {
    private lazy val instance = new True
    def apply() = instance()
    def unapply(n: True): Boolean = true
  }

  class False private extends ConstCondition(false)

  object False {
    private lazy val instance = new False
    def apply() = instance()
    def unapply(n: False): Boolean = true
  }

  object ConstCondition {
    def apply(x: Boolean) = if (x) True() else False()
    def unapply(x: ConstCondition) = Some(x.value)
  }


  /////////////////////////////////////////
  // Method input data: params and memory

  /** Node that represents a parameter of generated method */
  class Param private (keyType: Type, val num: Int) extends CachedLeafNode[Param](keyType) with BlockParamNode {
    assert(keyType.isValueType)

    def cacheKey = (keyType, num)

    override def block = this.scope.entryBlock

    def formalType = rootMethod.getParamType(num)
    def isReceiver = rootMethod.getMethodType.isReceiverParameter(num)
  }

  object Param {
    def apply(tpe: Type, num: Int) = Prototype.intern(new Param(tpe, num))()
    def unapply(p: Param): Option[Int] = Some(p.num)
  }

  object ReceiverParam {
    def apply() = {
      assert(rootMethod.hasReceiverParameter)
      rootMethodParam(rootMethod.getReceiverArgIdx)
    }

    def unapply(p: Param): Boolean = p.isReceiver
  }

  object MutParam {
    def apply(method: symlevel.Method, param: Int => Node) = {
      assert(method.hasMutObjectParameter && method.hasMutRecordParameter)
      MutFunc.Combine(param(method.getMutObjectArgIdx), param(method.getMutRecordArgIdx), ValueType(method.getMutRecordType))
    }
  }


  /** Node that represents var arguments of generated method */
  class VarArguments private extends LeafNode[VarArguments](AddrType) with FloatingNode

  object VarArguments {
    private lazy val instance = new VarArguments
    def apply() = instance()
  }

  object EntryMemory {
    def apply(): MemoryNode = entryBlock
  }


  /////////////////////////////////////////
  // Any access to memory
  trait AnyMemoryAccess { self: Node =>
    def accessType: AsmType
  }


  /////////////////////////////////////////
  // Raw access to memory

  abstract class RawMemoryAccess protected (proto: RawMemoryAccess.Proto[_ <: RawMemoryAccess]) extends NodeWithFixedArgs(proto) with AnyMemoryAccess {
    def addrIdx = proto.addrIdx
    def addr: Node = arg(addrIdx)
    def addrEdge: Edge = inEdge(addrIdx)
    def addr_=(a: Node): Unit = { updateArg(addrIdx, a) }
    override final def accessType: AsmType = proto.accessType
  }

  object RawMemoryAccess {
    abstract class Proto[N <: RawMemoryAccess](argTypes: Type*)(resType: Type)
      extends FixedArgs[N](argTypes: _*)(resType) {

      def addrIdx: Int
      def accessType: AsmType
    }
  }

  abstract class LoadStoreMemoryAccess protected (proto: LoadStoreMemoryAccess.Proto[_ <: LoadStoreMemoryAccess]) extends RawMemoryAccess(proto)
      with MayHaveImplicitCheck {
    def atomic: Boolean = proto.atomic
    def signature: SignatureType = proto.signature

    override def name: String = simpleName + "[" + signature + (if (atomic) ", atomic" else "") + "]"
  }

  object LoadStoreMemoryAccess {
    abstract class Proto[N <: LoadStoreMemoryAccess](argTypes: Type*)(resType: Type)
      extends RawMemoryAccess.Proto[N](argTypes: _*)(resType) {

      def atomic: Boolean
      def signature: SignatureType
    }

    object Disposed {
      def unapply(rma: LoadStoreMemoryAccess): Option[(Node, Int)] = rma.addr match {
        case lea @ Lea.Base(base, offset) if lea.attachedTo(rma) => Some((base, offset))
        case x => Some((x, 0))
      }
    }
  }

  abstract class LoadMemory(proto: LoadMemory.Proto) extends LoadStoreMemoryAccess(proto) with ControlledNode

  object LoadMemory {
    sealed abstract class Proto private[LoadMemory](argTypes: Type*)(retType: ValueType)
      extends LoadStoreMemoryAccess.Proto[LoadMemory](argTypes: _*)(retType)

    class Normal private[Normal](proto: LoadMemory.Normal.Proto) extends LoadMemory(proto) with GetMemoryOperation

    object Normal {
      case class Proto private[LoadMemory](addrTpe: Type, accessType: AsmType, signature: SignatureType, atomic: Boolean)
        extends LoadMemory.Proto(ControlType, MemoryType, addrTpe)(retType(accessType, signature)) {

        def addrIdx: Int = 2
        def newInstance() = new Normal(this)
      }

      def proto(addrTpe: Type, accessType: AsmType, sig: SignatureType, atomic: Boolean): Proto =
        Prototype.intern(Proto(addrTpe, accessType, sig, atomic))
    }

    class Soft private[Soft](proto: LoadMemory.Soft.Proto) extends LoadMemory(proto) with GetMemoryOperation {
      def kind: Integer = proto.kind
    }

    object Soft {
      case class Proto private[LoadMemory](addrTpe: Type, accessType: AsmType, signature: SignatureType, kind: Int)
        extends LoadMemory.Proto(ControlType, MemoryType, addrTpe)(retType(accessType, signature)) {

        def addrIdx: Int = 2
        def newInstance() = new Soft(this)

        override def atomic = false
      }

      def proto(addrTpe: Type, accessType: AsmType, sig: SignatureType, kind: Int): Proto =
        Prototype.intern(Proto(addrTpe, accessType, sig, kind))
    }

    class Independent(proto: LoadMemory.Independent.Proto) extends LoadMemory(proto) with FloatingNode with AnyMemoryAccess

    object Independent {
      case class Proto private[LoadMemory](addrTpe: Type, accessType: AsmType, signature: SignatureType, atomic: Boolean)
        extends LoadMemory.Proto(ControlType, addrTpe)(retType(accessType, signature)) {

        def addrIdx: Int = 1
        def newInstance() = new Independent(this)
      }

      def proto(addrTpe: Type, accessType: AsmType, sig: SignatureType, atomic: Boolean): Proto =
        Prototype.intern(Proto(addrTpe, accessType, sig, atomic))
    }

    /** Returns LoadMemory node independent from current control (with entryBlock input control) and memory. */
    def independent(accessType: AsmType, sig: SignatureType, atomic: Boolean)(addr: Node): Node =
      Independent.proto(addr.tpe, accessType, sig, atomic).withExplicitArgs(entryBlock, addr)

    /** Returns LoadMemory node independent from memory. */
    def memoryIndependent(accessType: AsmType, sig: SignatureType, atomic: Boolean)(addr: Node): Node =
      Independent.proto(addr.tpe, accessType, sig, atomic)(addr)

    def soft(accessType: AsmType, sig: SignatureType, kind: Int)(addr: Node): Node =
      Soft.proto(addr.tpe, accessType, sig, kind)(addr)

    def apply(accessType: AsmType, sig: SignatureType, atomic: Boolean)(addr: Node): Node =
      Normal.proto(addr.tpe, accessType, sig, atomic)(addr)

    def unapply(lm: LoadMemory) =
      Some(lm.accessType, lm.addr, lm.atomic)

    def retType(accessType: AsmType, sig: SignatureType) = {
      if (sig.isInterface && !sig.isDeferred) {
        EopType.Eop(sig.symType)
      } else {
        ValueType(sig)
      }
    }
  }

  class StoreMemory private (proto: StoreMemory.Proto)
        extends LoadStoreMemoryAccess(proto) with PutMemoryOperation with NotProducesValue {
    override protected def inValueArgIdx = 3
  }

  object StoreMemory {
    case class Proto private[StoreMemory] (addrTpe: Type, accessType: AsmType, signature: SignatureType, atomic: Boolean)
          extends LoadStoreMemoryAccess.Proto[StoreMemory](ControlType, MemoryType, addrTpe, ValueType(signature, eopTypeForInterfaces = true, instantiateRich = true))(ControlType)
          with ControlMemoryTagged[StoreMemory] {

      def newInstance() = new StoreMemory(this)

      def addrIdx: Int = 2
    }

    def proto(addrTpe: Type, accessType: AsmType, sig: SignatureType, atomic: Boolean): Proto =
      Prototype.intern(Proto(addrTpe, accessType, sig, atomic))

    def apply(accessType: AsmType, sig: SignatureType, atomic: Boolean)(addr: Node, value: Node): StoreMemory =
      proto(addr.tpe, accessType, sig, atomic)(addr, value)

    def unapply(x: StoreMemory) = Some(x.addr)

    object InValueEdge extends EdgeMatcher[StoreMemory](3)
  }


  /** Zeroes reference fields of stack allocated record passed to it. */
  // TODO: express this and similar nodes (e.g. InitObj) as frame slot properties
  class ZeroRefs private (proto: ZeroRefs.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with NotProducesValue {
    def sa = arg(ZeroRefs.RecordEdge.index).asInstanceOf[StackAlloc]
    def recordType = proto.recordType
  }

  object ZeroRefs {
    case class Proto private[ZeroRefs] (recordType: SignatureType)
      extends FixedArgs[ZeroRefs](ControlType, MemoryType, RecordAddrType(recordType))(ControlType) with ControlMemoryTagged[ZeroRefs] {

      def newInstance() = new ZeroRefs(this)
    }

    object RecordEdge extends EdgeMatcher[ZeroRefs](2)

    def proto(recordType: SignatureType) = Prototype.intern(Proto(recordType ensuring (_.isRecord)))

    def apply(sa: StackAlloc) = (sa.tpe: @unchecked) match {
      case RecordAddrType(recordType: SignatureType) => proto(recordType)(sa)
    }
  }

  class InitObj extends NodeWithFixedArgs(InitObj) with SpinalMemoryNode with NotProducesValue {
    def slot: FrameSlot = arg(InitObj.SlotEdge.index).asInstanceOf[StackAlloc].slot
  }

  object InitObj extends FixedArgs[InitObj](ControlType, MemoryType, AddrType)(ControlType) with ControlMemoryTagged[InitObj] {
    def newInstance() = new InitObj

    object SlotEdge extends EdgeMatcher[InitObj](2)

    def apply(sa: StackAlloc) = super.apply(sa)
  }

  class StackZeroing private(proto: StackZeroing.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with NotProducesValue {
    def slot: FrameSlot = proto.slot(this)
    def extraOffset: Int = proto.extraOffset
    def size: Int = proto.size

    def isSizeAndSlotDefined: Boolean = proto.isSizeAndSlotDefined
  }

  object StackZeroing {
    abstract class Proto (additionalArgTypes: Type*)
      extends FixedArgs[StackZeroing](Seq(ControlType, MemoryType) ++ additionalArgTypes: _*)(ControlType) with ControlMemoryTagged[StackZeroing] {

      override protected def newInstance() = new StackZeroing(this)

      def slot(sz: StackZeroing): FrameSlot
      def extraOffset: Int
      def size: Int
      def isSizeAndSlotDefined: Boolean
    }

    case class Single(extraOffset: Int, size: Int) extends Proto(AddrType) {
      override def slot(sz: StackZeroing): FrameSlot = sz.arg(Single.SlotEdge.index).asInstanceOf[StackAlloc].slot
      override def isSizeAndSlotDefined = true
    }

    object Single {
      object SlotEdge extends EdgeMatcher[StackZeroing](2)
    }

    object Massive extends Proto {
      private var created: Boolean = false
      private var _slot: FrameSlot = _
      private var _size: Int = -1
      override def isSizeAndSlotDefined: Boolean = (_slot != null) && (_size != -1)

      def setProperties(slot: FrameSlot, size: Int): Unit = {
        assert(created)
        assert(!isSizeAndSlotDefined)
        _slot = slot
        _size = size
      }

      override def slot(sz: StackZeroing) = _slot ensuring { _ => isSizeAndSlotDefined }
      override def extraOffset = 0
      override def size = _size ensuring { _ => isSizeAndSlotDefined }

      def apply(): Node = {
        assert(!created)
        created = true
        super.apply()
      }
    }
  }


  /////////////////////////////////////////
  // Math intrinsics

  class MathIntrinsic private (proto: MathIntrinsic.Proto) extends FloatingNodeWithFixedArgs(proto) {
    def kind: Java.Lang.MathIntrinsic = proto.kind
    def isBinary: Boolean = kind.isBinary

    def l = { assert(isBinary); arg(0) }
    def r = { assert(isBinary); arg(1) }
  }

  object MathIntrinsic {
    case class Proto private[MathIntrinsic](kind: Java.Lang.MathIntrinsic)
      extends FixedArgs[MathIntrinsic](Seq.fill(kind.argsCount)(ValueType(kind.typeKind)): _*)(ValueType(kind.typeKind)) {
      def newInstance() = new MathIntrinsic(this)
    }

    def apply(kind: Java.Lang.MathIntrinsic): Proto = Proto(kind)
    def unapply(node: MathIntrinsic) = Some(node.kind)
  }

  /////////////////////////////////////////
  // CompareAndSwap node.

  class CAS private (proto: CAS.Proto) extends RawMemoryAccess(proto) with PutMemoryOperation with ProducesValue {
    protected def inValueArgIdx = CAS.NewValueEdge.index

    /** Returns expected memory value as original node, it may exceed actual storage.
      * This unrefined value is adjusted during comparing with memory in case of short integral `keyTpe`.
      */
    def expectedValue0 = arg(CAS.ExpectedValueEdge.index)

    /** Returns new memory value as original node, it may exceed actual storage.
      * This unrefined value is adjusted during storing into memory in case of short integral `keyTpe`.
      */
    def newValue0 = arg(CAS.NewValueEdge.index)
  }

  object CAS {
    case class Proto private[CAS] (accessType: AsmType)
        extends RawMemoryAccess.Proto[CAS](ControlType, MemoryType, AddrType, ValueType(accessType), ValueType(accessType))(ValueType(accessType))
          with ControlMemoryValueTagged[CAS] {

      def newInstance() = new CAS(this)

      override def addrIdx = AddrEdge.index
    }

    def apply(tpe: AsmType) = Prototype.intern(Proto(tpe))

    object AddrEdge extends EdgeMatcher[CAS](2)
    object ExpectedValueEdge extends EdgeMatcher[CAS](3)
    object NewValueEdge extends EdgeMatcher[CAS](4)
  }


  /////////////////////////////////////////
  // Atomic intrinsic
  // TODO maybe we have to change SpinalNode to PutMemoryOperation?
  class MemAtomic private (proto: MemAtomic.Proto) extends RawMemoryAccess(proto) with SpinalMemoryNode with ProducesValue {
    def kind: MemAtomic.Kind = proto.kind

    def value: Node = arg(MemAtomic.ValueEdge.index)

    def rtMethod(): symlevel.Method = {
      import Com.Huawei.Excelsior.Aj.Internal.AtomicIntrinsics._
      import MemAtomic.Kind._

      (kind, accessType) match {
        case (AND,  AsmType.I8) => fetchAndByte
        case (OR,   AsmType.I8) => fetchOrByte
        case (XOR,  AsmType.I8) => fetchXorByte
        case (ADD,  AsmType.I8) => fetchAddByte
        case (MIN,  AsmType.I8) => fetchMinByte
        case (MAX,  AsmType.I8) => fetchMaxByte
        case (SWAP, AsmType.I8) => swapByte

        case (AND,  AsmType.I16) => fetchAndShort
        case (OR,   AsmType.I16) => fetchOrShort
        case (XOR,  AsmType.I16) => fetchXorShort
        case (ADD,  AsmType.I16) => fetchAddShort
        case (MIN,  AsmType.I16) => fetchMinShort
        case (MAX,  AsmType.I16) => fetchMaxShort
        case (SWAP, AsmType.I16) => swapShort

        case (AND,  AsmType.I32) => fetchAndInt
        case (OR,   AsmType.I32) => fetchOrInt
        case (XOR,  AsmType.I32) => fetchXorInt
        case (ADD,  AsmType.I32) => fetchAddInt
        case (MIN,  AsmType.I32) => fetchMinInt
        case (MAX,  AsmType.I32) => fetchMaxInt
        case (SWAP, AsmType.I32) => swapInt

        case (AND,  AsmType.I64) => fetchAndLong
        case (OR,   AsmType.I64) => fetchOrLong
        case (XOR,  AsmType.I64) => fetchXorLong
        case (ADD,  AsmType.I64) => fetchAddLong
        case (MIN,  AsmType.I64) => fetchMinLong
        case (MAX,  AsmType.I64) => fetchMaxLong
        case (SWAP, AsmType.I64) => swapLong

        case (x, y) =>
          shouldNotReachHere("feel free to extend Com.Huawei.Excelsior.Aj.Internal.MemAtomic description for: " + x + " / " + y)
      }
    }
  }

  object MemAtomic {
    enum Kind {
      case ADD, ANDNOT, AND, OR, XOR, MIN, UMIN, MAX, UMAX, SWAP
    }

    case class Proto private[MemAtomic](kind: Kind, accessType: AsmType)
      extends RawMemoryAccess.Proto[MemAtomic](ControlType, MemoryType, AddrType, ValueType(accessType))(ValueType(accessType))
        with ControlMemoryValueTagged[MemAtomic] {
      def newInstance() = new MemAtomic(this)

      override def addrIdx = AddrEdge.index
    }

    object AddrEdge extends EdgeMatcher[MemAtomic](2)
    object ValueEdge extends EdgeMatcher[MemAtomic](3)

    def apply(kind: Kind, tpe: AsmType): Proto = Proto(kind, tpe)
    def unapply(ai: MemAtomic) = Some((ai.kind, ai.tpe))
  }


  /////////////////////////////////////////
  // EOP conversion nodes.

  sealed trait EOPOperation {
    def obj: Node
  }

  sealed abstract class EOPConvert protected (proto: EOPConvert.Proto[_ <: EOPConvert])
    extends FloatingNodeWithFixedArgs(proto) with ArgDependentTypeNode with EOPOperation {

    def interfaceType = proto.interfaceType
    def obj = arg(proto.objIdx)

    def isTypeDependency(edge: Edge): Boolean = {
      edge.targetArgIndex == proto.objIdx
    }

  }

  object EOPConvert {
    abstract class Proto[N <: EOPConvert] protected (argTypes: Type*)(resType: Type) extends FixedArgs[N](argTypes: _*)(resType) {
      def interfaceType: symlevel.Type
      def objIdx = 0
    }

    def unapply(n: EOPConvert): Option[Node] = Some(n.obj)

    @tailrec
    def skip(n: Node): Node = n match {
      case EOPConvert(x) => skip(x)
      case _ => n
    }

    object Skipped {
      def unapply(n: Node): Option[Node] = Some(skip(n))
    }
  }

  sealed trait EnrichOperation extends Node with EOPOperation {
    def enrichment: Node
  }

  /** Converts plain EOP to rich one. */
  class Enrich private (proto: Enrich.Proto) extends EOPConvert(proto) with EnrichOperation {
    require(!interfaceType.isDeferred)
    private def enrichmentArg = Enrich.EnrichmentEdge.index
    def enrichment = arg(enrichmentArg)
    def enrichment_=(n: Node): Unit = { updateArg(enrichmentArg, n) }
  }

  object Enrich {
    case class Proto private[Enrich] (interfaceType: symlevel.Type)
      extends EOPConvert.Proto[Enrich](ValueType(interfaceType), AddrType)(EopType.Eop(interfaceType)) {

      def newInstance() = new Enrich(this)
    }

    def apply(interfaceType: symlevel.Type) = Prototype.intern(Proto(interfaceType))
    def unapply(e: Enrich): Option[(symlevel.Type, Node, Node)] = Some((e.interfaceType, e.obj, e.enrichment))

    object EnrichmentEdge extends EdgeMatcher[Enrich](1)
  }

  /** Version of [[Enrich]] with return type erased to [[EopType.Any]]. */
  class RawEnrich private extends FloatingNodeWithFixedArgs(RawEnrich) with CompositeNode with EnrichOperation {
    def obj = arg(0)
    def enrichment = arg(1)
  }

  object RawEnrich extends FixedArgs[RawEnrich](TRefType, AddrType)(EopType.Any) {
    def newInstance() = new RawEnrich
  }


  sealed trait DepriveOperation extends Node with EOPOperation {
    def isUnchecked: Boolean
  }

  /** Converts enriched EOP to plain. */
  class Deprive private (proto: Deprive.Proto) extends EOPConvert(proto) with DepriveOperation {

    // Note that "deferred" Deprive accepts both rich and not rich `obj`.
    // As for cangjie interfaces, there are problems when overriding method
    // returns class instead of an interface (see JET-14374).
    def isUnchecked = interfaceType.isDeferred || interfaceType.isCangjieType

    // Lowering hint
    var isLoweredWithWeakCast = false
  }

  object Deprive {
    case class Proto private[Deprive] (interfaceType: symlevel.Type) extends EOPConvert.Proto[Deprive](ValueType(interfaceType, eopTypeForInterfaces = true, instantiateRich = true))(TRefType) {
      def newInstance() = new Deprive(this)
    }

    def apply(interfaceType: symlevel.Type) = Prototype.intern(Proto(interfaceType))
    def unapply(x: Deprive): Option[(symlevel.Type, Node)] = Some((x.interfaceType, x.obj))
  }

  /** Version of [[Deprive]] that doesn't require a type of deprived object. */
  class RawDeprive private extends FloatingNodeWithFixedArgs(RawDeprive) with CompositeNode with DepriveOperation {
    def obj = arg(0)
    def isUnchecked = true
  }

  object RawDeprive extends FixedArgs[RawDeprive](EopType.Any)(TRefType) {
    def newInstance() = new RawDeprive
  }


  class ExtractEnrichment private extends FloatingNodeWithFixedArgs(ExtractEnrichment) with CompositeNode with EOPOperation {
    def obj = arg(0)
  }

  object ExtractEnrichment extends FixedArgs[ExtractEnrichment](EopType.Any)(AddrType) {
    def newInstance() = new ExtractEnrichment
  }


  /////////////////////////////////////////
  // Stack allocation node.

  trait HasFrameSlot extends Node {
    var slot: FrameSlot = _
    def kind: FrameSlot.Kind
  }

  class StackAlloc private (val kind: FrameSlot.Kind)
    extends LeafNode[StackAlloc](StackAlloc.tpeByKind(kind)) with HasFrameSlot with StructurallyUnique with Constant {

    def size = kind.size
    def alignment = kind.alignment
    def zeroed = kind.zeroed

    override def name = s"$simpleName[$kind]"
  }

  object StackAlloc {

    object Local {
      def apply(t: SignatureType) = StackAlloc(FrameSlot.Local(t))
      def apply(t: SignatureType, workaroundForNonZeroedTraceableRecords: Boolean) =
        StackAlloc(FrameSlot.Local(t, workaroundForNonZeroedTraceableRecords))
      def unapply(x: StackAlloc) = condOpt(x.kind) {
        case FrameSlot.Local(allocType, _) => allocType
      }
    }

    object DebugVar {
      def apply(t: SignatureType, info: DebugLocalVar) = StackAlloc(FrameSlot.DebugVar(t, info))
      def unapply(x: StackAlloc) = condOpt(x.kind) {
        case FrameSlot.DebugVar(tpe, info) => (tpe, info)
      }
    }

    object OffHeapMemory {
      def apply(t: SignatureType): StackAlloc = StackAlloc(FrameSlot.OffHeapMemory(t))
      def unapply(x: StackAlloc) = condOpt(x.kind) {
        case FrameSlot.OffHeapMemory(allocType) => allocType
      }
    }

    def tpeByKind(kind: FrameSlot.Kind): Type = kind match {
      case FrameSlot.Local(t, _) if !t.isPrimitive && !isStandalone => ValueType.fromSig(t)
      case FrameSlot.DebugVar(t, _) if !t.isPrimitive && !isStandalone => ValueType.fromSig(t)
      case FrameSlot.Typed(t) if t.isRecord => RecordAddrType(t)
      case FrameSlot.OffHeapMemory(t) => ValueType.fromSig(t)
      case _ => AddrType
    }

    def apply(kind: FrameSlot.Kind) = new StackAlloc(kind)()
    def raw(size: Int, alignment: Int) = apply(FrameSlot.Raw(size, alignment))

    def unapply(x: StackAlloc) = Some(x.kind)
  }

  //////////////////////////////////////////////////////////
  // High-level copying node

  class CopyStructure private(proto: CopyStructure.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CompositeNode with NotProducesValue {
    def dst = arg(2)
    def src = arg(3)
    def src_=(x: Node): Unit = updateArg(3, x)

    def isPrimitive: Boolean = proto.primitive

    def structureType = proto.structureType
  }

  object CopyStructure {
    case class Proto private[CopyStructure](structureType: SignatureType, primitive: Boolean)
      extends FixedArgs[CopyStructure](ControlType, MemoryType, ValueType(structureType), ValueType(structureType))(ControlType)
        with ControlMemoryTagged[CopyStructure] {

      override def newInstance() = new CopyStructure(this)
    }

    def proto(x: SignatureType) = Prototype.intern(Proto(x, false))

    def apply(x: SignatureType)(dst: Node, src: Node) = proto(x)(dst, src)

    def unapply(x: CopyStructure) = Some(x.structureType, x.dst, x.src)

    def primitive(x: SignatureType) = Prototype.intern(Proto(x, true))
  }

  class CopyStructureCBC private(proto: CopyStructureCBC.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CompositeNode with NotProducesValue {
    require(!env.enabled(UseIsa12) || !(proto.hasStaticDst && proto.hasStaticSrc)) // both dst and src cannot be static fields in the same time
    require(!env.enabled(UseIsa12) || !(dst.isInstanceOf[RecordArrayGet] && src.isInstanceOf[RecordArrayGet])) // both dst and src cannot be record arrays in the same time

    def dst = arg(2)
    def src = arg(3)

    def dstFields = proto.dstFields
    def srcFields = proto.srcFields

    def hasComplexDst = proto.hasStaticDst || dst.isInstanceOf[RecordArrayGet]
    def hasComplexSrc = proto.hasStaticSrc || src.isInstanceOf[RecordArrayGet]

    def structureType = proto.structureType
  }

  object CopyStructureCBC {
    case class Proto private[CopyStructureCBC](structureType: SignatureType, dstRefClassType: Type, srcRefClassType: Type,
                                               dstFields: Array[FieldReference], srcFields: Array[FieldReference], hasStaticDst: Boolean, hasStaticSrc: Boolean)
      extends FixedArgs[CopyStructureCBC](ControlType, MemoryType, dstRefClassType, srcRefClassType)(ControlType)
        with ControlMemoryTagged[CopyStructureCBC] {

      override def newInstance() = new CopyStructureCBC(this)
    }

    def proto(x: SignatureType, dstRefClassType: Type, srcRefClassType: Type, dstFields: List[FieldReference], srcFields: List[FieldReference], isDstStatic: Boolean, isSrcStatic: Boolean) =
      Prototype.intern(Proto(x, dstRefClassType, srcRefClassType, Array.from(dstFields), Array.from(srcFields), isDstStatic, isSrcStatic))

    def apply(x: SignatureType, dstRefClassType: Type, srcRefClassType: Type, dstFields: List[FieldReference], srcFields: List[FieldReference], isDstStatic: Boolean, isSrcStatic: Boolean)
             (dstObj: Node, srcObj: Node) =
      proto(x, dstRefClassType, srcRefClassType, dstFields, srcFields, isDstStatic, isSrcStatic)(dstObj, srcObj)
  }

  //////////////////////////////////////////////////////////
  // Nodes for transition between traced and untraced refs

  /** Conceals traced reference from an outside observer known as runtime (including GC).
    *
    * This has two consequences:
    *
    *   - Runtime can't modify this traced reference and therefore we can safely access and modify
    *     its internal representation. That's why the result type is AddrType which is already
    *     untraced reference - an internal representation of the original traced one.
    *   - As the reference is concealed from the outside observer, the corresponding object can be moved or collected by GC.
    *
    * Note that it '''must''' be controlled node to ensure that all uses of the untraced ref remain below its control dependency.
    */
  class ConcealRef extends NodeWithFixedArgs(ConcealRef) with ControlledNode with FloatingNode with Transfer {
    override protected def transferArgIdx: Int = 1
    def tracedRef = arg(1)
  }

  object ConcealRef extends FixedArgs[ConcealRef](ControlType, EopType.Any)(AddrType) {
    override def newInstance() = new ConcealRef

    def unapply(n: ConcealRef): Option[Node] = Some(n.tracedRef)
  }


  /** Publishes untraced reference making it visible to an outside observer known as runtime (including GC).
    * The result is a traced reference internally represented as the original untraced ref.
    *
    * After publishing runtime can modify this traced reference and therefore we can't safely access or modify
    * its internal representation directly without concealing it with [[ConcealRef]].
    *
    * Note that it '''must''' be spinal node because all uses of the original untraced ref must remain above it.
    */
  class PublishRef extends NodeWithFixedArgs(PublishRef) with SpinalNode with Transfer with ProducesValue {
    override protected def transferArgIdx: Int = 1
    def untracedRef = arg(1)
  }

  object PublishRef extends FixedArgs[PublishRef](ControlType, AddrType)(TRefType) with ControlValueTagged[PublishRef] {
    override def newInstance() = new PublishRef

    def unapply(n: PublishRef): Option[Node] = Some(n.untracedRef)
  }


  class BeginLocalUnmovable extends NodeWithFixedArgs(BeginLocalUnmovable) with SpinalNode with ProducesValue {
    def obj = arg(BeginLocalUnmovable.Object.index)
  }

  object BeginLocalUnmovable extends FixedArgs[BeginLocalUnmovable](ControlType, TRefType)(TRefType) with ControlValueTagged[BeginLocalUnmovable] {
    override def newInstance() = new BeginLocalUnmovable

    object Object extends EdgeMatcher[BeginLocalUnmovable](1)

    def unapply(n: BeginLocalUnmovable): Option[Node] = Some(n.obj)
  }

  class EndLocalUnmovable extends NodeWithFixedArgs(EndLocalUnmovable) with SpinalNode with NotProducesValue {
    def obj = arg(EndLocalUnmovable.Object.index)
  }

  object EndLocalUnmovable extends FixedArgs[EndLocalUnmovable](ControlType, TRefType)(ControlType) with ControlTagged[EndLocalUnmovable] {
    override def newInstance() = new EndLocalUnmovable

    object Object extends EdgeMatcher[EndLocalUnmovable](1)

    def unapply(n: EndLocalUnmovable): Option[Node] = Some(n.obj)
  }


  class AcquireRawData extends NodeWithFixedArgs(AcquireRawData) with SpinalNode with CompositeNode with ProducesValue {
    def array = arg(AcquireRawData.Array.index)
  }

  object AcquireRawData extends FixedArgs[AcquireRawData](ControlType, TRefType)(AddrType) with ControlValueTagged[AcquireRawData] {
    override def newInstance() = new AcquireRawData

    object Array extends EdgeMatcher[AcquireRawData](1)

    def unapply(n: AcquireRawData) = Some(n.array)
  }

  class ReleaseRawData extends NodeWithFixedArgs(ReleaseRawData) with SpinalNode with CompositeNode with NotProducesValue {
    def array = arg(ReleaseRawData.Array.index)
    def pointer = arg(ReleaseRawData.Pointer.index)
  }

  object ReleaseRawData extends FixedArgs[ReleaseRawData](ControlType, TRefType, AddrType)(ControlType) with ControlTagged[ReleaseRawData] {
    override def newInstance() = new ReleaseRawData

    object Array extends EdgeMatcher[ReleaseRawData](1)
    object Pointer extends EdgeMatcher[ReleaseRawData](2)

    def unapply(n: ReleaseRawData) = Some(n.array, n.pointer)
  }
}
