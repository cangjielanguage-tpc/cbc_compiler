/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline.scales

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.codeemitter.BarrierKind.STORE_LOAD
import com.huawei.excelsior.jet.compiler.SymbolLinker
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.types.Guards.*

trait ScalesAmd64 extends Scales { self: Universe =>

  override def nodeWeight(node: Node): Double = ScalesAmd64Impl.nodeWeight(node)

  private [scales] object ScalesAmd64Impl extends ScalesImpl {
    def fixupWeight = 4
    def stackAllocWeight = 4

    def javaStackAllocWeight = stackAllocWeight + 10 /* zeroing */ + 4 /* to plain eop */ + getPutWeight /* put td */

    override def ldrLiteralWeight = 3 + fixupWeight

    override def jmpWeight = 2
    override def jccWeight = 2
    override def movWeight = 2

    override def intCmpWeight = 2

    override def execEnvWeight = 3 // TODO: should be nop with zero weight
    override def frameHeaderWeight = 3

    override def getPutWeight = 4
    override def trapCheckWeight = 4
    override def clinitWeight = 4 + intCmpWeight + jccWeight + fixupWeight + directCallWeight(1) + jmpWeight
    override def fastTypeCheck = 16

    override def directCallWeight(paramsCount: Int) = 4 * paramsCount + 1 + fixupWeight
    override def indirectCallWeight(paramsCount: Int) = 4 * paramsCount + 1 + 1
    override def getVirtualMethodAddrWeight = 11
    override def getInterfMethodAddrWeight = 4

    def iconstWeight(v: Long): Double = {
      if (Byte.MinValue <= v && v <= Byte.MaxValue) {
        1
      } else if (Int.MinValue <= v && v <= Int.MaxValue) {
        4
      } else {
        10 // long mov here
      }
    }

    override def checkedOpIntConstWeight(v: Long) = iconstWeight(v)
    override def checkedOpIntDivRemWeight(n: Node) = if (n.tpe == LongType) 5 else 4
    override def checkedOpIntHighMulWeight(n: Node) = if (n.tpe == LongType) 3 else 2
    override def checkedOpShiftWeight = 3
    override def checkedOpIntLogicWeight = 2
    override def checkedOpIntAddSubWeight = 2
    override def checkedOpIntMulWeight = 3
    override def divisorCheckWeight = intCmpWeight + jccWeight

    /** Weight of one node as if method body is inlined into caller. */
    override def nodeWeight(n: Node): Double = n match {

      case _: AnyNull => 1
      case IntegralConst(v) => iconstWeight(v)
      case _: FConst | _: DConst => fixupWeight

      case _: ImportedIndex   => 4
      case _: ConstString     => 3 + fixupWeight

      case _: GCPoint => 11 // TODO: may vary depending on used register

      case n: If => ifWeight + (n.selector match { case c: Cmp if c.l.isFP => jccWeight; case _ => 0 })
      case n: Switch => n.cases.foldLeft(0.0) { (acc, label) => acc + ifWeight + intCmpWeight + iconstWeight(label) }

      case _: And | _: Or | _: Xor => 2
      case _: Shift => 3
      case _: Neg => if (n.isFP) 8 else 2
      case _: Add | _: Sub => if (n.isFP) 4 else 2
      case _: Mul => if (n.isFP) 4 else 3
      case _: MulH | _: UMulH => if (n.tpe == LongType) 3 else 2
      case n: IDivRemOp => if (n.tpe == LongType) 5 else 4
      case _: FDiv => 4

      case _: BitFieldExtract => 3
      case ValueConvert(F16, F32 | F64, _) => 10
      case ValueConvert(F64 | F32, F16, _) => 15
      case n: Cast => (n.from, n.to) match {
        case (_: FloatingPointType, _: IntegralType) => 25
        case _ => 4
      }

      case cmp: Cmp =>
        if (cmp.keyType == IntType) intCmpWeight
        else if (cmp.keyType == LongType) 3
        else if (cmp.keyType.isStructureType) 3
        else if (cmp.keyType == FloatType) 3
        else if (cmp.keyType == DoubleType) 4
        else shouldNotReachHere(cmp.keyType)
      case _: ThreeCmp => 2 * (ifWeight + intCmpWeight) + (/* 0 */ 2 + /* -1 */ 5 + /* 1 */ 5)

      case n: BitCount =>
        import BitCount.Kind._
        n.kind match {
          case LEADING_ZEROS => 16
          case TRAILING_ZEROS => 12
          case BIT_COUNT => 11
          case HIGHEST_BIT => shouldNotReachHere()
        }

      case _: GetStatic | _: PutStatic => 2 + fixupWeight
      case _: ArrayLength => 3

      case _: AbstractNullCheck => 3
      case _: AnyClassObject => fixupWeight + directCallWeight(2)

      case _: ArrayPut | _: UArrayPut | _: ArrayGet | _: UArrayGet => 2 + 4

      case _: CFuncWrapperAddr => 3 + fixupWeight + getPutWeight + 2 + 4

      case _: AJCallerClass => 3 + fixupWeight
      case _: StackAlloc => stackAllocWeight
      case _: AJString => 3 + fixupWeight
      case _: VarArguments => 4

      case _: NewStackAllocated => javaStackAllocWeight
      case _: NewArrayStackAllocated => javaStackAllocWeight + 4 * getPutWeight /* put tsWord, bundle, length, elemType */

      case _: Catch => 9

      case n: MemBarrier => if (n.kinds contains STORE_LOAD) 17 else 0

      case _: BitSwap => 2

      case _: CAS => 5

      case _: MonitorEnter => 44
      case _: MonitorExit => 32

      case n: MathIntrinsic =>
        import Java.Lang.MathIntrinsic._
        n.kind match {
          case D_SIN    => 44
          case D_COS    => 44
          case D_TAN    => 44
          case D_ATAN   => 20
          case D_LOG    => 20
          case D_SQRT   => 4
          case F_SQRT   => 4
          case D_RINT   => 18
          case D_ABS    => 8
          case F_ABS    => 7
          case D_ATAN2  => 26
          case D_REM1 | D_REM | F_REM => 35
          case D_ASIN | D_ACOS | D_EXP | D_POW | D_CEIL | D_FLOOR  => directCallWeight(n.kind.argsCount)
        }

      case _: Prefetch => 4

      case n: MemAtomic =>
        import MemAtomic.Kind._
        n.kind match {
          case AND | OR | XOR => if (n.hasValueUses) directCallWeight(2) else 5
          case ADD => 5
          case SWAP => 4
          case _ => shouldNotReachHere()
        }

      case _ => super.nodeWeight(n)
    }
  }
}
