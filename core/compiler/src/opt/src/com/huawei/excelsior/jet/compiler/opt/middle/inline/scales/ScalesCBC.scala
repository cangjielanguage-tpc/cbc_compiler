/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline.scales

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

// TODO this is just a copy of arm64 code - tune it for CBC
trait ScalesCBC extends Scales { self: Universe =>

  override def nodeWeight(node: Node): Double = ScalesCBCImpl.nodeWeight(node)

  private [scales] object ScalesCBCImpl extends ScalesImpl {
    val instrSize = 2
    val stackAllocWeight = instrSize

    val javaStackAllocWeight = stackAllocWeight + 4*instrSize /* zeroing */ + getPutWeight /* put td */

    override def ldrLiteralWeight = instrSize /* ldr */ + 4 /* mem stub */

    override def jmpWeight = instrSize
    override def jccWeight = instrSize
    override def movWeight = instrSize

    override def intCmpWeight = instrSize

    override def execEnvWeight = 0
    override def frameHeaderWeight = movWeight

    override def getPutWeight = instrSize
    override def trapCheckWeight = instrSize

    override def clinitWeight = ldrLiteralWeight /* TD */ + getPutWeight /* clinit flag check */ + intCmpWeight /* cmp with 0 */ +
      jccWeight /* jmp to clinit stub */ + ldrLiteralWeight /* TDI32 */ + directCallWeight(1) + jmpWeight

    // 24 bytes - weight of fast class check for non final class (-4 for final, +16 for targetType.inheritanceLevel > rts.TD_BASE_INLINE)
    override def fastTypeCheck = getPutWeight + ldrLiteralWeight /* target TD */ + getPutWeight + intCmpWeight + jccWeight

    override def directCallWeight(paramsCount: Int) = instrSize + paramsCount * instrSize
    override def indirectCallWeight(paramsCount: Int) = instrSize + paramsCount * instrSize
    override def getVirtualMethodAddrWeight = getPutWeight * 2
    override def getInterfMethodAddrWeight = getPutWeight * 2 + instrSize


    override def checkedOpIntConstWeight(v: Long) = instrSize
    override def checkedOpIntDivRemWeight(n: Node) = instrSize
    override def checkedOpIntHighMulWeight(n: Node) = instrSize
    override def checkedOpIntLogicWeight = instrSize
    override def checkedOpIntAddSubWeight = instrSize
    override def checkedOpIntMulWeight = instrSize
    override def checkedOpShiftWeight = instrSize
    override def divisorCheckWeight = instrSize

    /** Weight of one node as if method body is inlined into caller. */
    override def nodeWeight(n: Node): Double = n match {

      case _: AnyNull => 0
      case IConst(_) => instrSize
      case _: LConst => 0 // TODO: update after real implementation
      case FConst(_) => instrSize
      case DConst(_) => instrSize

      case _: ImportedIndex   => instrSize
      case _: ConstString     => instrSize

      case _: GCPoint => instrSize

      case _: If => ifWeight
      case n: Switch => n.cases.size * (intCmpWeight + ifWeight)

      case _: Neg | _: Add | _: Sub | _: Mul | _: And | _: Or | _: Xor => instrSize
      case _: Shift => instrSize
      case _: MulH | _: UMulH => 0 // TODO: update after real implementation
      case IRem() | URem() => instrSize
      case IDiv() | UDiv() => instrSize
      case _: DivisorCheck => instrSize
      case _: FDiv => instrSize

      case _: BitFieldExtract => instrSize
      case ValueConvert(F16, F32 | F64, _) => instrSize * 2
      case ValueConvert(F64 | F32, F16, _) => instrSize * 3
      case n: Cast => (n.from, n.to) match {
        case (_: FloatingPointType, _: IntegralType) |
             (_: IntegralType, _: FloatingPointType) => instrSize * 2
        case _ => instrSize
      }

      case cmp: Cmp => if (cmp.keyType.isFloatingPointType) instrSize * 2 else intCmpWeight
      case _: ThreeCmp => 2 * (ifWeight + intCmpWeight) + movWeight * 3

      case n: BitCount =>
        import BitCount.Kind._
        val intWeight = n.kind match {
          case LEADING_ZEROS => instrSize
          case TRAILING_ZEROS => 2 * instrSize
          case HIGHEST_BIT => shouldNotReachHere()
          case BIT_COUNT => 4 * instrSize
        }
        n.argTpe match {
          case IntType => intWeight
          case LongType => 2 * intWeight + 3 * instrSize
          case t => shouldNotReachHere(t)
        }

      case _: BitSwap => instrSize // TODO: update after real implementation

      case n: GetStatic => ldrLiteralWeight + instrSize + (if (!n.field.isAJFlat) instrSize else 0)
      case _: PutStatic => ldrLiteralWeight + instrSize * 2
      case _: ArrayLength => instrSize

      case _: AbstractNullCheck => instrSize
      case _: AnyClassObject => ldrLiteralWeight + directCallWeight(2)

      case _: ArrayPut | _: ArrayGet => getPutWeight + instrSize /* indexed addr mode calculation */
      case _: UArrayPut | _: UArrayGet => getPutWeight

      case _: CFuncWrapperAddr => ldrLiteralWeight + getPutWeight + getPutWeight + instrSize

      case _: AJCallerClass => ldrLiteralWeight
      case _: StackAlloc => stackAllocWeight
      case _: AJString => ldrLiteralWeight
      case _: VarArguments => instrSize

      case _: NewStackAllocated => javaStackAllocWeight
      case _: NewArrayStackAllocated => javaStackAllocWeight + 4 * getPutWeight /* put tsWord, bundle, length, elemType */

      case _: Catch => instrSize * 3

      case _: MemBarrier => instrSize

      case _: MonitorEnter => 64
      case _: MonitorExit => 44

      case n: MathIntrinsic =>
        import Java.Lang.MathIntrinsic._
        n.kind match {
          case D_ABS | F_ABS | D_SQRT | F_SQRT => instrSize

          case D_SIN | D_COS | D_TAN | D_ATAN | D_LOG | D_RINT | D_ATAN2 | D_REM1 |
               D_REM | F_REM | D_ASIN | D_ACOS | D_EXP | D_POW | D_CEIL | D_FLOOR  => directCallWeight(n.kind.argsCount)
        }

      case n: MemAtomic =>
        import MemAtomic.Kind._
        n.kind match {
          case ADD | OR | XOR | AND | MIN | UMIN | MAX | UMAX | SWAP => instrSize
          case _ => shouldNotReachHere()
        }

      case _: Prefetch => instrSize

      case _: CAS => instrSize

      case _: BitcodeDeferred => Infinity

      // FIXME-UG
      case _: UniversalGeneric => instrSize

      case _ => super.nodeWeight(n)
    }
  }
}
