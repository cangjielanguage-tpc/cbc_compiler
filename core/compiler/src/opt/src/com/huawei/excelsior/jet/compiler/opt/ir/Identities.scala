/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import java.lang.{Double as jlDouble, Float as jlFloat, Integer as jlInteger, Long as jlLong}
import com.huawei.excelsior.jet.compiler.{StatsKind, symlevel}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.LightInterfCalls
import com.huawei.excelsior.jet.compiler.opt.util.DivByConstMagicNumberComputation
import com.huawei.excelsior.jet.compiler.symlevel.ClassType
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import xscala.util.MathUtils.*
import xscala.util.{MathUtils, UInt, ULong}

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
import scala.annotation.{nowarn, tailrec}


/**
 * Algebraic identities of the nodes.
 *
 * @author paul
 * @author cypok
 * @author kit
 * @author alexm
 */
trait Identities extends DivByConstMagicNumberComputation with NodeAliases with LightInterfCalls with Nodes { self: Universe =>

  import BitFieldExtract._

  private def isCompileTimeComputable(node: Node): Boolean =
    node.valueArgs.nonEmpty && node.valueArgs.forall { _.isInstanceOf[CompileTimeValue] }

  protected def isApplicableToConstFold(node: Node): Boolean = node match {
    case IDivRemByConstOp(0) => false

    case _: IDivRemOp =>
      // IDivRemOp is a controlled node but it is applicable to const fold except division by zero
      isCompileTimeComputable(node)

    case _: HasInMemory | _: HasInControl =>
      // Node has control or memory argument
      false

    case ReinterpretCast(AddrType, ThinType, IntegralConst(x)) if x != 0 =>
      // Exclusion from constants folding
      false

    case ReinterpretCast(AddrType, _: RecordAddrType, IntegralConst(_)) =>
      // Exclusion from constants folding // TODO: investigate
      false

    case Lea.AnyWithBase(_: AnyNull, _) =>
      // Such lea are absolutely normal (e.g., produced by crossroads optimization). We would not optimize them, because:
      // 1) It is hard to create [[IntraReferenceType]]-d constant
      // 2) It will complicate PhiWebsTranslation
      false

    case _: SynchronizedRegion =>
      // sync regions form tree-structure with constant root. Should not be folded.
      false

    case ValueConvert(F16, _, _) | ValueConvert(_, F16, _) =>
      // no converters yet
      false

    case _: UniversalGeneric.ConvertHolder =>
      // No folding for holder conversions
      false

    case _: Phi =>
      // No constant folding for phies, it is covered with other optimizations
      false

    case _ =>
      // Node has non empty list of value arguments and all fo them are compile-time value nodes
      // Also, node may have SymlevelNode argument (e.g. InstanceOf)
      isCompileTimeComputable(node)
  }

  /** Compile-time values folding. */
  @nowarn("msg=match may not be exhaustive")
  // TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
  @nowarn("msg=The outer reference in this type test cannot be checked at run time")
  private object ConstFold {

    def apply(n: Node): Node = n match {
      case x: Cast        => apply(x)
      case x: Add         => apply(x)
      case x: Sub         => apply(x)
      case x: Neg         => apply(x)
      case x: Mul         => apply(x)
      case x: MulH        => apply(x)
      case x: UMulH       => apply(x)
      case x: IDivRemOp   => apply(x)
      case x: FDiv        => apply(x)
      case x: And         => apply(x)
      case x: Or          => apply(x)
      case x: Xor         => apply(x)
      case x: Shift       => apply(x)
      case x: BitCount    => apply(x)
      case x: Cmp         => apply(x)
      case x: ThreeCmp    => apply(x)
      case x: Not         => apply(x)
      case x: CondVal     => apply(x)
      case x: Lea         => apply(x)

      case x: InstanceOf  => assertNull(x.obj); IConst(0)
      case x: WeakCast    => assertNull(x.obj); IntegralConst(AddrIntType)(0)
      case x: EOPOperation => assertNull(x.obj); AnyNull(x.tpe)

      case x: MathIntrinsic => apply(x)

      case x: BitFieldExtract => apply(x)

      case x =>
        var info = "unknown node for ConstFold: " + x
        for (arg <- x.valueArgs) {
          info += "\n  " + arg
        }
        shouldNotReachHere(info)
    }

    @annotation.strictfp
    private def apply(cast: Cast): Node = {
      cast match {
        case ReinterpretCast(fromType, toType, arg) =>
          (fromType, toType, arg) match {
            case _ if fromType == toType || arg.tpe == toType => arg

            case (AddrType | _: StructureType, AddrType | _: StructureType, IntegralConst(0) | _: AnyNull) => ZeroValueNode(toType)
            case (FloatType, IntType, FConst(v)) => IConst(floatToRawIntBits(v))
            case (DoubleType, LongType, DConst(v)) => LConst(doubleToRawLongBits(v))
            case (IntType, FloatType, IConst(v)) => FConst(intBitsToFloat(v))
            case (LongType, DoubleType, LConst(v)) => DConst(longBitsToDouble(v))
          }

        case ValueConvert(fromType, toType, arg) =>
          (fromType, toType, arg) match {
            case _ if fromType == toType => arg

            case (I32, F32, IConst(v)) => FConst(v.toFloat)
            case (I32, F64, IConst(v)) => DConst(v.toDouble)

            case (I64, F32, LConst(v)) => FConst(v.toFloat)
            case (I64, F64, LConst(v)) => DConst(v.toDouble)

            case (F32, I32, FConst(v)) => IConst(v.toInt)
            case (F32, I64, FConst(v)) => LConst(v.toLong)
            case (F32, F64, FConst(v)) => DConst(v.toDouble)

            case (F64, I32, DConst(v)) => IConst(v.toInt)
            case (F64, I64, DConst(v)) => LConst(v.toLong)
            case (F64, F32, DConst(v)) => FConst(v.toFloat)

            case _ => shouldNotReachHere("Unsupported conversion " + cast + " from " + arg)
          }
      }
    }

    @annotation.strictfp
    private def apply(bfx: BitFieldExtract): Node = {
      bfx match {
        case BFX(offset, size, sx, IntegralConst(v)) =>
          val bitField = if (size > 0) bits(v, offset, offset + size - 1) else 0
          IntegralConst(bfx.tpe)(if (sx) signExtend(bitField, size) else bitField)
      }
    }

    @annotation.strictfp
    private def apply(add: Add): Node = {
      (add.l, add.r) match {
        case (IntegralConst(v1), IntegralConst(v2)) => IntegralConst(add.tpe)(v1 + v2)
        case (FConst(v1), FConst(v2)) => FConst(v1 + v2)
        case (DConst(v1), DConst(v2)) => DConst(v1 + v2)
      }
    }

    @annotation.strictfp
    private def apply(sub: Sub): Node = {
      (sub.l, sub.r) match {
        case (IntegralConst(v1), IntegralConst(v2)) => IntegralConst(sub.tpe)(v1 - v2)
        case (FConst(v1), FConst(v2)) => FConst(v1 - v2)
        case (DConst(v1), DConst(v2)) => DConst(v1 - v2)
      }
    }

    @annotation.strictfp
    private def apply(neg: Neg): Node = {
      neg.arg match {
        case IntegralConst(v) => IntegralConst(neg.tpe)(-v)
        case FConst(v) => FConst(-v)
        case DConst(v) => DConst(-v)
      }
    }

    @annotation.strictfp
    private def apply(mul: Mul): Node = {
      (mul.l, mul.r) match {
        case (IntegralConst(v1), IntegralConst(v2)) => IntegralConst(mul.tpe)(v1 * v2)
        case (FConst(v1), FConst(v2)) => FConst(v1 * v2)
        case (DConst(v1), DConst(v2)) => DConst(v1 * v2)
      }
    }

    private def apply(mulh: MulH): Node = {
      (mulh.l, mulh.r) match {
        case (IConst(v1), IConst(v2)) => IConst(((v1.toLong * v2.toLong) >> 32).toInt)
        case (LConst(v1), LConst(v2)) => LConst(MathUtils.mulh(v1, v2))
      }
    }

    private def apply(umulh: UMulH): Node = {
      (umulh.l, umulh.r) match {
        case (IConst(v1), IConst(v2)) => IConst(((UInt(v1).toLong * UInt(v2).toLong) >> 32).toInt)
        case (LConst(v1), LConst(v2)) => LConst(MathUtils.umulh(v1, v2))
      }
    }

    private def apply(op: IDivRemOp): Node = {
      (op.l, op.r, op.isUnsigned, op.isDiv) match {
        case (IConst(v1), IConst(v2), false, true ) => IConst(v1 / v2)
        case (LConst(v1), LConst(v2), false, true ) => LConst(v1 / v2)
        case (IConst(v1), IConst(v2), false, false) => IConst(v1 % v2)
        case (LConst(v1), LConst(v2), false, false) => LConst(v1 % v2)
        case (IConst(v1), IConst(v2), true , true ) => IConst(udiv(v1, v2))
        case (LConst(v1), LConst(v2), true , true ) => LConst(udiv(v1, v2))
        case (IConst(v1), IConst(v2), true , false) => IConst(urem(v1, v2))
        case (LConst(v1), LConst(v2), true , false) => LConst(urem(v1, v2))
      }
    }

    @annotation.strictfp
    private def apply(div: FDiv): Node = {
      (div.l, div.r) match {
        case (FConst(v1), FConst(v2)) => FConst(v1 / v2)
        case (DConst(v1), DConst(v2)) => DConst(v1 / v2)
      }
    }

    private def apply(and: And): Node = {
      (and.l, and.r) match {
        case (IntegralConst(v1), IntegralConst(v2)) => IntegralConst(and.tpe)(v1 & v2)
      }
    }

    private def apply(or: Or): Node = {
      (or.l, or.r) match {
        case (IntegralConst(v1), IntegralConst(v2)) => IntegralConst(or.tpe)(v1 | v2)
      }
    }

    private def apply(xor: Xor): Node = {
      (xor.l, xor.r) match {
        case (IntegralConst(v1), IntegralConst(v2)) => IntegralConst(xor.tpe)(v1 ^ v2)
      }
    }

    private def apply(shift: Shift): Node = {
      (shift.value, shift.num, shift.op) match {
        case (IConst(v1), IConst(v2), ArithOp.LSL) => IConst(v1 << v2)
        case (IConst(v1), IConst(v2), ArithOp.ASR) => IConst(v1 >> v2)
        case (IConst(v1), IConst(v2), ArithOp.LSR) => IConst(v1 >>> v2)
        case (LConst(v1), IConst(v2), ArithOp.LSL) => LConst(v1 << v2)
        case (LConst(v1), IConst(v2), ArithOp.ASR) => LConst(v1 >> v2)
        case (LConst(v1), IConst(v2), ArithOp.LSR) => LConst(v1 >>> v2)
      }
    }

    private def apply(bitCount: BitCount): Node = {
      import BitCount.Kind._
      (bitCount.arg, bitCount.kind) match {
        case (IConst(v), BIT_COUNT)      => IConst(jlInteger.bitCount(v))
        case (LConst(v), BIT_COUNT)      => IConst(jlLong.bitCount(v))
        case (IConst(v), LEADING_ZEROS)  => IConst(jlInteger.numberOfLeadingZeros(v))
        case (IConst(v), TRAILING_ZEROS) => IConst(jlInteger.numberOfTrailingZeros(v))
        case (LConst(v), LEADING_ZEROS)  => IConst(jlLong.numberOfLeadingZeros(v))
        case (LConst(v), TRAILING_ZEROS) => IConst(jlLong.numberOfTrailingZeros(v))
        case (_, HIGHEST_BIT) => shouldNotReachHere(bitCount)
      }
    }

    @annotation.strictfp
    private def unordered(v1: Float, v2: Float) = jlFloat.isNaN(v1) || jlFloat.isNaN(v2)

    @annotation.strictfp
    private def unordered(v1: Double, v2: Double) = jlDouble.isNaN(v1) || jlDouble.isNaN(v2)

    @annotation.strictfp
    private def apply(cmp: Cmp): Node = {
      import Condition._

      ConstCondition((cmp.l, cmp.r) match {
        case (IntegralConst(v1), IntegralConst(v2)) =>
          cmp.op match {
            case EQ => v1 == v2
            case NE => v1 != v2
            case GE => v1 >= v2
            case GT => v1 >  v2
            case LT => v1 <  v2
            case LE => v1 <= v2
            case UGE => ugeq(v1, v2)
            case UGT => ugtr(v1, v2)
            case ULT => ulss(v1, v2)
            case ULE => uleq(v1, v2)
          }
        case (FConst(v1), FConst(v2)) =>
          cmp.op match {
            case EQ => v1 == v2
            case NE => v1 != v2
            case GE => v1 >= v2
            case GT => v1 >  v2
            case LT => v1 <  v2
            case LE => v1 <= v2
            case GE_OR_UNORDERED => (v1 >= v2) || unordered(v1, v2)
            case GT_OR_UNORDERED => (v1 >  v2) || unordered(v1, v2)
            case LT_OR_UNORDERED => (v1 <  v2) || unordered(v1, v2)
            case LE_OR_UNORDERED => (v1 <= v2) || unordered(v1, v2)
          }
        case (DConst(v1), DConst(v2)) =>
          cmp.op match {
            case EQ => v1 == v2
            case NE => v1 != v2
            case GE => v1 >= v2
            case GT => v1 >  v2
            case LT => v1 <  v2
            case LE => v1 <= v2
            case GE_OR_UNORDERED => (v1 >= v2) || unordered(v1, v2)
            case GT_OR_UNORDERED => (v1 >  v2) || unordered(v1, v2)
            case LT_OR_UNORDERED => (v1 <  v2) || unordered(v1, v2)
            case LE_OR_UNORDERED => (v1 <= v2) || unordered(v1, v2)
          }
        case (Null(), Null()) => evalCmpFromEqualArgs(cmp.op) // TODO: investigate it
        case (v1, v2) if v1 == v2 => evalCmpFromEqualArgs(cmp.op)
      })
    }

    @annotation.strictfp
    private def apply(threeCmp: ThreeCmp): Node = {
      val result = (threeCmp.l, threeCmp.r, threeCmp.op) match {
        case (LConst(v1), LConst(v2), ArithOp.CMP)  => if (v1 > v2)  1 else if (v1 == v2) 0 else -1
        case (FConst(v1), FConst(v2), ArithOp.CMPL) => if (v1 > v2)  1 else if (v1 == v2) 0 else -1
        case (DConst(v1), DConst(v2), ArithOp.CMPL) => if (v1 > v2)  1 else if (v1 == v2) 0 else -1
        case (FConst(v1), FConst(v2), ArithOp.CMPG) => if (v1 < v2) -1 else if (v1 == v2) 0 else  1
        case (DConst(v1), DConst(v2), ArithOp.CMPG) => if (v1 < v2) -1 else if (v1 == v2) 0 else  1
      }
      IConst(result)
    }

    private def apply(not: Not): Node = {
      val arg = not.arg.asInstanceOf[ConstCondition]
      ConstCondition(!arg.value)
    }

    private def apply(cv: CondVal): Node = {
      (cv.condition, cv.negated) match {
        case (True(),  false) | (False(), true) => IConst(1)
        case (False(), false) | (True(),  true) => IConst(0)
      }
    }

    private def assertNull(x: Node): Unit = assert(x.isInstanceOf[AnyNull])

    @annotation.strictfp
    private def apply(x: MathIntrinsic): Node = {
      import Java.Lang.MathIntrinsic._

      def dArg = x.arg.asInstanceOf[DConst].value
      def fArg = x.arg.asInstanceOf[FConst].value
      def dL = x.l.asInstanceOf[DConst].value
      def dR = x.r.asInstanceOf[DConst].value
      def fL = x.l.asInstanceOf[FConst].value
      def fR = x.r.asInstanceOf[FConst].value

      x.kind match {
        case D_SIN    => DConst(math.sin(dArg))
        case D_COS    => DConst(math.cos(dArg))
        case D_TAN    => DConst(math.tan(dArg))
        case D_ASIN   => DConst(math.asin(dArg))
        case D_ACOS   => DConst(math.acos(dArg))
        case D_ATAN   => DConst(math.atan(dArg))
        case D_EXP    => DConst(math.exp(dArg))
        case D_LOG    => DConst(math.log(dArg))
        case D_SQRT   => DConst(math.sqrt(dArg))
        case F_SQRT   => FConst(math.sqrt(fArg.toDouble).toFloat)
        case D_CEIL   => DConst(math.ceil(dArg))
        case D_FLOOR  => DConst(math.floor(dArg))
        case D_RINT   => DConst(math.rint(dArg))
        case D_ABS    => DConst(math.abs(dArg))
        case F_ABS    => FConst(math.abs(fArg))
        case D_ATAN2  => DConst(math.atan2(dL, dR))
        case D_POW    => DConst(math.pow(dL, dR))
        case D_REM1   => DConst(math.IEEEremainder(dL, dR))
        case D_REM    => DConst(dL % dR)
        case F_REM    => FConst(fL % fR)
      }
    }

    private def apply(lea: Lea): Node = lea match {
      case Lea.Baseless(index, scale, disp) =>
        val ValueType(asmType) = index.tpe
        Add(Mul(Extend(lea.tpe, asmType, signExtension = false, index), IntegralConst(lea.tpe)(scale)), IntegralConst(lea.tpe)(disp))
      case Lea.Base(base, disp) =>
        Add(base, IntegralConst(lea.tpe)(disp))
      case Lea.AnyWithBase(base, _) =>
        shouldNotReachHere(s"unexpected constant base $base at $lea")
    }
  }

  private def phiIdentity(phi: Phi): Node = phiLikeIdentity(phi, phi.args)

  private def castIdentity(cast: Cast): Node = {
    cast match {
      case ValueConvert(from, to, arg) if from == to => assert(from.isSigned || from == U16, s"$from"); arg
      case ReinterpretCast(from, to, arg) if from == to || arg.tpe == to => arg
      case ReinterpretCast(_, to, ReinterpretCast(from, _, x)) => ReinterpretCast(from, to)(x)

      // Upcast to EopType is redundant.
      case ReinterpretCast(_: EopType, EopType.Any, arg) => arg

      // Cast is idempotent.
      case _ if cast.proto == cast.arg.proto => cast.arg

      // Cangjie JET was given permission to perform all Float16 operations on Float32 values
      // despite possible loss of precision.
      // So we eliminate intermediate h2f and f2h casts which are inserted implicitly
      // in case of complex arithmetic expressions.
      case ValueConvert(F32, F16, ValueConvert(F16, F32, arg)) if rootDeclaringClass.isCangjieType => arg

      case _ => cast
    }
  }

  private def bfxIdentity(bfx: BitFieldExtract): Node = bfx match {
    // full value extract
    case BFX(offset, size, _, arg) if arg.tpe == bfx.tpe && size == typeSizeInBits(bfx.tpe) =>
      assert(offset == 0)
      arg

    // empty value extract
    case BFX(_, 0, _, _) => IntegralConst(bfx.tpe)(0)

    // redundant short integrals extend after memory read
    case JavaShortIntegralExtend(tpe, x: GetMemoryOperation) if x.accessType == tpe =>
      x

    case bfx @ BFX(0, size, sign, x: CheckedOp) =>
      size match {
        case 8 | 16 | 32 | 64 if bfx.tpe == IntType && AsmType.integral(Width(size / 8), sign) == x.asmType => x
        case _ => bfx
      }

    // Idea: signExtendI8(CondVal) == CondVal, it's a pattern in Cangjie bitcode
    case BFX(0, size, _, x: CondVal) if bfx.tpe == IntType && size > 1 => x

    case BFX(offset, size, _, ShiftByConst(ArithOp.LSL, _, x)) if offset + size <= x =>
      IntegralConst(bfx.tpe)(0)

    case BFX(offset, size, sx, ShiftByConst(ArithOp.LSL, arg, x)) if offset >= x && size + offset <= typeSizeInBits(arg.tpe) - x =>
      BFX(bfx.tpe, offset - x, size, sx, arg)

    case BFX(offset1, size1, sx1, BFX(_, size2, sx2, _)) if offset1 == 0 && bfx.tpe == bfx.argType && size1 == size2 && sx1 == sx2 =>
      bfx.arg

    case BFX(offset1, size1, sx1, BFX(offset2, size2, _, arg)) if offset1 + size1 <= size2 =>
      BFX(bfx.tpe, offset1 + offset2, size1, sx1, arg)

    case BFX(offset1, size1, sx1, BFX(offset2, size2, sx2, arg)) if offset1 < size2 && ((sx1 == sx2) || bfx.extensionIrrelevant) =>
      BFX(bfx.tpe, offset1 + offset2, math.min(size1, size2 - offset1), sx2, arg)

    case _ => bfx
  }

  private object CommutativeAssociativeOp {
    def apply(node: BinaryOp): Node = {
      if (!node.tpe.isIntegralType) return node
      (node.l, node.r) match {
        case (arg @ BinaryOp(x, y: CompileTimeValue), z: CompileTimeValue)  if arg.proto == node.proto => arg.proto(x, arg.proto(y, z))
        case (arg @ BinaryOp(x, y: CompileTimeValue), z)                    if arg.proto == node.proto => arg.proto(y, arg.proto(x, z))
        case (x, arg @ BinaryOp(y, z: CompileTimeValue))                    if arg.proto == node.proto => arg.proto(z, arg.proto(x, y))
        case _ => node
      }

      // TODO: reimplement this optimization to cover annihilating arguments case. E.g. (a + -(b)) + (b + c) => (a + c)
    }
  }

  private def combinedAddSubNegIdentity(op: Node): Node = {
    val tpe = op.tpe

    if (tpe.isIntegralType) { // TODO: consider to use the following rules for the floating-point types

      // Integral Add/Sub/Neg nodes optimized with the following idea: all possible Add operations are moved outside
      // of formulas to be applicable to CommutativeAssociativeOp optimization and Lea construction. Thus we transform
      // Sub to Add and Neg of Adds to Add of Negs.

      // Basic rules:
      //  1)    x + 0     => x
      //  2)    -(-x)     => x
      //  3)    (-x) + x  => 0
      //  4)    x - y     => x + (-y)
      //  5)    -(x + y)  => (-x) + (-y)
      //  6)    AddrConst(s, c1) + c2   =>  AddrConst(s, c1 + c2) // TODO: use LEA instead of this
      //  7.1)  CW(0, x) + CW(y, z)     =>  CW(y, x + z)
      //  7.2)  CW(x, y) + CW(0, z)     =>  CW(x, y + z)
      //  8)    (x + y) - y             =>  x
      //  9)    (x + y) - x             =>  y
      //  10)   x + y                   =>  CommutativeAssociativeOp optimization

      // Derived rules, covered by basic:
      //  11)   x - 0                   =>  x  (combination of 4th rule, const fold and 1st rule)
      //  12)   x - x                   =>  0  (combination of 4th and 3rd rules)
      //  13)   AddrConst(s, c1) - c2   =>  AddrConst(s, c1 - c2) (combination of 4th and 6th rules)
      // TODO: consider to use derived rules as fast paths

      op match {
        case Add(x, IntegralConst(0)) => x
        case Neg(Neg(x)) => x
        case Add(Neg(x), y) if x == y => IntegralConst(tpe)(0)
        case Sub(x, y) => Add(Neg(tpe)(y), x)
        case Neg(Add(x, y)) => Add(Neg(tpe)(x), Neg(tpe)(y))
        case Add(AddrConst(ctrl, symbol, c1), DWordConst(c2)) => AddrConst(ctrl, symbol, c1 + c2)
        case Add(Neg(x), Add(y, z)) if x == y => z
        case Add(Neg(x), Add(y, z)) if x == z => y
        case add: Add => CommutativeAssociativeOp(add)
        case _ => op
      }
    } else {

      // Here described subset of rules from previous part, applicable to floating-point types
      op match {
        case Neg(Neg(x)) => x
        case add: Add => CommutativeAssociativeOp(add)
        case _ => op
      }
    }
  }

  private def mulIdentity(mul: Mul): Node = {
    (mul.l, mul.r) match {
      case (_, z @ IntegralConst(0)) => z
      case (x, IntegralConst(1)) => x
      case (x, IntegralConst(-1)) => Neg(mul.tpe)(x)
      case (x, IntegralConst(PowerOfTwo(p))) => lsl(x, p)
      case _ => CommutativeAssociativeOp(mul)
    }
  }

  private def divRemIdentity(op: IDivRemOp): Node = {
    (op.l, op.r) match {
      case (_, IntegralConst(0)) => op
      case (z @ IntegralConst(0), _) => z
      case (x, y) if x == y => IntegralConst(op.tpe)(if (op.isDiv) 1 else 0)

      case (x, IntegralConst(1)) => if (op.isDiv) x else IntegralConst(op.tpe)(0)
      case (x, IntegralConst(-1)) if !op.isUnsigned => if (op.isDiv) Neg(op.tpe)(x) else IntegralConst(op.tpe)(0)

      case (SignExtend(n), LConst(d)) if !op.isUnsigned && d.toInt == d =>
        // Note: divisor -1 must be matched earlier
        SignExtend(IDivRemOp(IntType, isUnsigned = false, isDiv = op.isDiv).withExplicitArgs(op.inCtrl, n, IConst(d.toInt)))
      case _ => op
    }
  }

  private def fDivIdentity(fdiv: FDiv): Node = {
    def isPowerOf2F(f: Float) = {
      val l = f.toLong
      f == l && l != 0 && isPowerOf2(l.abs)
    }

    def isPowerOf2D(d: Double) = {
      val l = d.toLong
      d == l && l != 0 && isPowerOf2(l.abs)
    }

    (fdiv.l, fdiv.r) match {
      case (_, FConst(fc)) if fc.isNaN => FConst(Float.NaN)
      case (_, FConst(fc)) if fc.isInfinite || isPowerOf2F(fc) => Mul(fdiv.l, FConst(1.0f/fc))
      case (_, DConst(dc)) if dc.isNaN => DConst(Double.NaN)
      case (_, DConst(dc)) if dc.isInfinite || isPowerOf2D(dc) => Mul(fdiv.l, DConst(1.0d/dc))
      case _ => fdiv
    }
  }

  private def andIdentity(and: And): Node = {
    (and.l, and.r) match {
      case (x, y) if x == y => x
      case (x, IntegralConst(-1)) => x
      case (_, z @ IntegralConst(0)) => z
      case _ => CommutativeAssociativeOp(and)
    }
  }

  private def orIdentity(or: Or): Node = {
    (or.l, or.r) match {
      case (x, y) if x == y => x
      case (_, m1 @ IntegralConst(-1)) => m1
      case (x, IntegralConst(0)) => x
      case _ => CommutativeAssociativeOp(or)
    }
  }

  private def xorIdentity(xor: Xor): Node = {
    (xor.l, xor.r) match {
      case (x, y) if x == y => IntegralConst(x.tpe)(0)
      case (x, IntegralConst(0)) => x
      case (x: CondVal, IConst(1)) => CondVal(!x.negated)(x.condition)
      //TODO: introduce BinNeg and optimize x^-1 to BinNeg
      case _ => CommutativeAssociativeOp(xor)
    }
  }

  private def shiftIdentity(shift: Shift): Node = {
    (shift.value, shift.num) match {
      case (z @ IntegralConst(0), _) => z
      case (m1 @ IntegralConst(-1), _) if shift.op == ArithOp.ASR => m1
      case (x, IntegralConst(0)) => x
      case (x, IConst(n)) if n != ShiftByConst.masked(n, shift.tpe) || shift.op != ArithOp.LSL =>
        ShiftByConst(shift.tpe, shift.op, n, x)

      /* Handle special case of integer division optimization on 64-bit platforms */
      case (Truncate(ShiftByConst(ArithOp.LSL, n, longShift)), IConst(intShift)) if shift.op == ArithOp.LSL =>
        if (longShift + intShift < 64) {
          Truncate(Shift(shift.op, n, IConst(longShift + intShift)))
        } else {
          shift
        }

      case _ => shift
    }
  }

  private def testIdentity(test: Test): Node = {
    import Condition._

    (test.l, test.r) match {
      case (x, v @ IntegralConst(y)) if y >= 0 =>
        (test.op: @unchecked) match {
          case EQ | NE  => test
          case LT | ULT => False()
          case LE | ULE => Test(x.tpe, EQ)(x, v)
          case GT | UGT => Test(x.tpe, NE)(x, v)
          case GE | UGE => True()
        }

      case _ => test
    }
  }


  private def cmpIdentity(cmp: Cmp): Node = {
    import Condition._

    // Although Cmp is not commutative operation, it's arguments can still be swapped with additional `cmp.op` negation.
    if (!areArgsSorted(cmp.l, cmp.r)) {
      // Note: here a new node is created, so identity will be called implicitly during commit.
      return Cmp.withSwappedArgs(cmp)
    }

    object NonNullAddress {
      @tailrec
      def unapply(x: Node): Boolean = x match {
        case _: SymbolAddress | _: AddrConst | _: StackAlloc => true
        case ReinterpretCast(_, _, y) => unapply(y)
        case _ => false
      }
    }

    (cmp.l, cmp.r) match {
      case (x, v @ (IntegralConst(0) | _: AnyNull)) if cmp.op.isUnsigned =>
        (cmp.op: @unchecked) match {
          case UGE => True()
          case ULT => False()
          case UGT => Cmp(cmp.keyType, NE)(x, v)
          case ULE => Cmp(cmp.keyType, EQ)(x, v)
        }

      // TODO: consider making SymbolAddress "extends Constant" like some reference consts (e.g. ClassObject)
      case (NonNullAddress(), IntegralConst(0) | _: AnyNull) =>
        (cmp.op: @unchecked) match {
          case NE => True()
          case EQ => False()
        }

      case (_, IntegralConstMaxValue()) if cmp.op == LE => True()
      case (_, IntegralConstMinValue()) if cmp.op == GE => True()
      case (_, IntegralConstMaxValue()) if cmp.op == GT => False()
      case (_, IntegralConstMinValue()) if cmp.op == LT => False()

      case (_, UnsignedIntegralConstMaxValue()) if cmp.op == ULE => True()
      case (_, UnsignedIntegralConstMinValue()) if cmp.op == UGE => True()
      case (_, UnsignedIntegralConstMaxValue()) if cmp.op == UGT => False()
      case (_, UnsignedIntegralConstMinValue()) if cmp.op == ULT => False()

      case (v1, v2) if (v1 == v2) && !v1.tpe.isFloatingPointType =>
        ConstCondition(evalCmpFromEqualArgs(cmp.op))

      case (ThreeCmp(ArithOp.CMP, v1, v2), IConst(0)) =>
        cmp.op match {
          case EQ | NE | GE | GT | LT | LE => Cmp(LongType, cmp.op)(v1, v2)
          case _ => cmp
        }

      case (ThreeCmp(ArithOp.CMPL, v1, v2), IConst(0)) =>
        assert ((v1.tpe == v2.tpe) && v1.tpe.isFloatingPointType)
        val cmpOp = cmp.op match {
          case LT => LT_OR_UNORDERED
          case LE => LE_OR_UNORDERED
          case op => op
        }
        Cmp(v1.tpe, cmpOp)(v1, v2)

      case (ThreeCmp(ArithOp.CMPG, v1, v2), IConst(0)) =>
        assert ((v1.tpe == v2.tpe) && v1.tpe.isFloatingPointType)
        val cmpOp = cmp.op match {
          case GT => GT_OR_UNORDERED
          case GE => GE_OR_UNORDERED
          case op => op
        }
        Cmp(v1.tpe, cmpOp)(v1, v2)

      // These conversions do not change null values.
      case (EOPConvert(v), Null()) => Cmp(v.tpe, cmp.op)(v, AnyNull(v.tpe))

      // After this optimization context types of Thin nodes may be weakened leading to assert in their recalculation.
      // For more details look at JET-13144.
      // TODO: analyze the problem and try to find better solution.
      // case (ReinterpretCast(f, t, v), n: AnyNull) => Cmp(v.tpe, cmp.op)(v, ReinterpretCast(t, f)(n))

      // Idea: (Neg(v) == 0) == (v == 0).
      case (Neg(v), IConst(0)) => cmp.op match {
        case NE | EQ => Cmp(IntType, cmp.op)(v, IConst(0))
        case _ => cmp
      }

      case (CondVal(negated, cond), IConst(0)) =>
        (cmp.op, negated) match {
          case (NE, false) | (EQ, true) => cond
          case (EQ, false) | (NE, true) => Not(cond)
          case _  => cmp // it's possible to simplify cmp with any OP and with any integral const R, but who needs it?
        }

      case (CondVal(negated, cond), IConst(1)) if rootDeclaringClass.isCangjieType =>
        (cmp.op, negated) match {
          case (NE, true) | (EQ, false) => cond
          case (EQ, true) | (NE, false) => Not(cond)
          case _  => cmp // it's possible to simplify cmp with any OP and with any integral const R, but who needs it?
        }

      case (SignExtend(x), LConst(y)) if y == y.toInt =>
        Cmp(IntType, cmp.op)(x, IConst(y.toInt))

      case (AddrConst(_, sym1, ofs1), AddrConst(_, sym2, ofs2)) =>
        (sym1 == sym2, ofs1 == ofs2) match {
          case (true, true) =>
            shouldNotReachHere("should be already covered by cmp with equivalent arguments optimization")

          case (true, false) => Cmp(IntType, cmp.op)(IConst(ofs1), IConst(ofs2))

          case (false, _) => cmp // nothing can do
        }

      case (l: StackAlloc, r: StackAlloc) if l.size > 0 && r.size > 0 =>
        assert(l != r)
        cmp.op match {
          case NE => True()
          case EQ => False()
          case _ => cmp
        }

      case (_: StackAlloc, _) | (_, _: StackAlloc) =>
        cmp

      case (l: Constant, r: Constant) =>
        def valueFromRefConst(n: Constant) = n match {
          case _: AnyNull | IntegralConst(0) => null
          case n: ConstString => n.stringValue
          case n: ClassObject => n.symType
          case n: AJString => (n.str, n.bstr)
        }

        val (v1, v2) = (valueFromRefConst(l), valueFromRefConst(r))

        ConstCondition((cmp.op: @unchecked) match {
          case EQ => v1 == v2
          case NE => v1 != v2
        })

      case _ => cmp
    }
  }

  private def evalCmpFromEqualArgs(op: Condition) = {
    import Condition._
    (op: @unchecked) match {
      case EQ | GE | LE | UGE | ULE => true
      case NE | GT | LT | UGT | ULT => false
    }
  }

  private def notIdentity(not: Not): Node = {
    not.arg match {
      case Not(x) => x
      case cmp: Cmp => Cmp(cmp.keyType, cmp.op.negate(cmp.keyType.isFloatingPointType))(cmp.l, cmp.r)
      case _ => not
    }
  }

  private def arrayLengthIdentity(arrayLength: ArrayLength): Node = {
    arrayLength.array match {
      case anyNewArray: AnyNewArray => anyNewArray.lengths.head
      case newArrayRT: NewArrayRT => newArrayRT.length
      case newArrayCopy: NewArrayCopy => newArrayCopy.length
      case newArrayCopyRT: NewArrayCopyRT => Sub(newArrayCopyRT.to, newArrayCopyRT.from)
      case _ => arrayLength
    }
  }

  private def getStaticIdentity(get: GetStatic): Node = {
    get.inMemory match {
      case put: PutStatic if put.field == get.field => getMemIdentity(get, put)
      case _ => get
    }
  }

  private def getFieldIdentity(get: GetField): Node = {
    get match {
      case GetField(f, _, _, box: BoxedValue) if box.boxType.value == f =>
        stats.count(StatsKind.MemOpt, s"${get.simpleName} eliminated trivial", get)
        box.primitiveValue()
      case _ => get.inMemory match {
        case put: PutField if put.field == get.field && put.obj == get.obj => getMemIdentity(get, put)
        case _ => get
      }
    }
  }

  private def arrayGetIdentity(get: ArrayGet): Node = {
    get.inMemory match {
      case put: ArrayPut if !put.arrayType.isRecordArray && put.array == get.array && put.idx == get.idx &&
        put.accessType == get.accessType => getMemIdentity(get, put)
      case _ => get
    }
  }

  private def getMemIdentity(get: GetMemoryOperation, put: PutMemoryOperation) = {
    stats.count(StatsKind.MemOpt, s"${get.simpleName} eliminated trivial", get)
    put.storedValue()
  }

  private def condValIdentity(cval: CondVal) = {
    cval.condition match {
      case Not(x) => CondVal(!cval.negated)(x)
      case _ => cval
    }
  }

  private def enrichIdentity(x: Enrich): Node = x.obj match {
    case Null() => AnyNull(x.tpe)
    case n: NoValue => n
    case Deprive(t, n) if t == x.interfaceType => n
    case n => producesRich(n) match {
      case EnrichmentDecision.No => x
      case d @ EnrichmentDecision.Yes(_) => shouldNotReachHere(d)
      case d @ EnrichmentDecision.DoNotKnow if typeChecksEnabled => shouldNotReachHere(d)
      case _ => x
    }
  }

  private def depriveIdentity(x: DepriveOperation): Node = x.obj match {
    case Null() => AnyNull(EopType.Plain)
    case n: NoValue => n
    case Enrich(_, n, _) => n
    case n => producesRich(n) match {
      case EnrichmentDecision.Yes(_) | EnrichmentDecision.DoNotKnow => x
      case EnrichmentDecision.No => assert(x.isUnchecked); n
    }
  }

  private def weakCastIdentity(x: WeakCast): Node = {
    lightInterfCast(x.obj, asClassType(x.targetType)) match {
      case Some(lcast) =>
        lcast
      case _ if x.hasSpoiledDominatingCheck =>
        // Spoiled checks are useless and may prevent dead code elimination.
        WeakCast(x.targetType)(x.obj, WeakCast.NoCheck())
      case _ =>
        x
    }
  }

  private def leaIdentity(lea: Lea): Node = lea match {
    // [base + 0] => base
    case Lea.Base(_, 0) =>
      shouldNotReachHere("covered in Lea.Base.apply")

    // [index * 1 + 0] => index
    case Lea.Baseless(index, 1, 0) if lea.tpe == index.tpe =>
      shouldNotReachHere("covered in Lea.Baseless.apply")

    // [[... + base.disp] + disp] => [... + (base.disp + disp)]
    case Lea.Base(base: Lea, disp) if base.checkDispInc(disp) =>
      base.withDisp(base.disp + disp)

    // [[base + disp1] + ... + disp2] => [base + ... + (disp1 + disp2)]
    case Lea.AnyWithBase(Lea.Base(base, disp1), disp2) if lea.checkDispInc(disp1) =>
      lea.withBaseAndDisp(base, disp1 + disp2)

    // [(base + disp1) + ... + disp2] => [base + ... + (disp1 + disp2)]
    // This rule is similar with previous except for fact, that `add` was not converted to `lea` yet.
    case Lea.AnyWithBase(Add(base, DWordConst(disp1)), disp2) if lea.checkDispInc(disp1) =>
      lea.withBaseAndDisp(base, disp1 + disp2)

    // [base + constIndex * scale + disp] => [base + (constIndex * scale + disp)]
    case Lea.Scaled(base, DWordConst(index), scale, disp) if lea.checkDispInc(index, scale) =>
      Lea.Base(base, index * scale + disp)

    // [base + [index * scale + disp1] * 1 + disp2] => [base + index * scale + (disp1 + disp2)]
    //
    // This optimization may be potentially pessimization. E.g.:
    //   1) `index` has no uses below `baseless`
    //   2) `baseless` has uses above and below `lea`
    // By this transformation we will increase RP in range [`baseless`, `lea`].
    //
    // Currently this problem is not an issue, because Baseless Lea are very rare and in most cases they will not
    // have any uses except `lea` one. But in the future, when we will improve Lea transformations with index
    // deconstruction we should address this problem.
    case Lea.Scaled(base, Lea.Baseless(index, scale, disp1), 1, disp2) if !lea.undefinedForNegativeIndex && lea.checkDispInc(disp1) =>
      Lea.Scaled(base, index, scale, disp1 + disp2)

    case _ => lea
  }

  def mutFuncCombineIdentity(x: MutFunc.Combine): Node = x match {
    case MutFunc.Combine(combHost, MutFunc.Offset(host, record)) if combHost == host => record
    case _ => x
  }

  /** Sorted args: constants are last, negs are first, then by id. */
  def areArgsSorted(l: Node, r: Node) = (l, r) match {
    case (_: CompileTimeValue, _: CompileTimeValue) => l.id <= r.id
    case (_, _: CompileTimeValue) => true
    case (_: CompileTimeValue, _) => false
    case (Neg(_), Neg(_)) => l.id <= r.id
    case (Neg(_), _) => true
    case (_, Neg(_)) => false
    case _ => l.id <= r.id
  }

  /** Should be used only while commit procedure.
    * May commit (many) new nodes instead of given raw node.
    */
  private [ir] def identity(n: Node): Node = {
    if (n.hasUndefinedArgs) return n

    if (isApplicableToConstFold(n)) {
      val result = ConstFold(n)
      assert(result.isInstanceOf[CompileTimeValue])
      return result
    }

    n match {
      case op: CommutativeBinaryOp if !areArgsSorted(op.l, op.r) => op.swapArgs()
      case _ =>
    }

    n match {
      case x: Phi => phiIdentity(x)

      case x: Cast => castIdentity(x)
      case x: BitFieldExtract => bfxIdentity(x)

      case _: Add | _: Sub | _: Neg => combinedAddSubNegIdentity(n)
      case x: Mul => mulIdentity(x)

      case x: IDivRemOp => divRemIdentity(x)

      case x: FDiv => fDivIdentity(x)

      case x: And   => andIdentity(x)
      case x: Or    => orIdentity(x)
      case x: Xor   => xorIdentity(x)
      case x: Shift => shiftIdentity(x)

      case x: Cmp      => cmpIdentity(x)
      case x: Test     => testIdentity(x)
      case x: Not      => notIdentity(x)

      case x: ArrayLength => arrayLengthIdentity(x)

      case x: GetStatic => getStaticIdentity(x)
      case x: GetField  => getFieldIdentity(x)
      case x: ArrayGet  => arrayGetIdentity(x)

      case x: CondVal => condValIdentity(x)

      case x: Enrich           => enrichIdentity(x)
      case x: DepriveOperation => depriveIdentity(x)
      case x: WeakCast         => weakCastIdentity(x)

      case x: Lea => leaIdentity(x)

      case x: MutFunc.Combine => mutFuncCombineIdentity(x)

      case _ => n
    }
  }

}
