/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode.intrinsics

import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.intrinsics.IntrinsicWithBody
import com.huawei.excelsior.jet.compiler.intrinsics.IntrinsicWithBody.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.Method

import scala.PartialFunction.condOpt

trait IntrinsicsArm64 extends Intrinsics { self: Universe =>
  import MemAtomic.Kind._

  override def loadIntrinsicWithBody(target: Method, itype: IntrinsicWithBody, caller: Method, args: Seq[Node]): Option[Node] = condOpt(itype) {
    case UInt64_div => DivisorCheck(trusted = true)(args(1)); UDiv(LongType)(args: _*)
    case UInt64_mod => DivisorCheck(trusted = true)(args(1)); URem(LongType)(args: _*)
    case UInt64_leq => CondVal(Cmp(LongType, Condition.ULE)(args: _*))
    case UInt64_lss => CondVal(Cmp(LongType, Condition.ULT)(args: _*))

    case MemAtomic_casLong   => CAS(I64)(args: _*)

    case MemAtomic_minInt    => MemAtomic(MIN,  I32)(args: _*)
    case MemAtomic_uminInt   => MemAtomic(UMIN, U32)(args: _*)
    case MemAtomic_maxInt    => MemAtomic(MAX,  I32)(args: _*)
    case MemAtomic_umaxInt   => MemAtomic(UMAX, U32)(args: _*)

    case MemAtomic_addLong   => MemAtomic(ADD,  I64)(args: _*)
    case MemAtomic_andLong   => MemAtomic(AND,  I64)(args: _*)
    case MemAtomic_orLong    => MemAtomic(OR,   I64)(args: _*)
    case MemAtomic_xorLong   => MemAtomic(XOR,  I64)(args: _*)
    case MemAtomic_minLong   => MemAtomic(MIN,  I64)(args: _*)
    case MemAtomic_uminLong  => MemAtomic(UMIN, U64)(args: _*)
    case MemAtomic_maxLong   => MemAtomic(MAX,  I64)(args: _*)
    case MemAtomic_umaxLong  => MemAtomic(UMAX, U64)(args: _*)
    case MemAtomic_swapLong  => MemAtomic(SWAP, I64)(args: _*)

    case Half_fromFloat => ValueConvert(F32, F16)(args: _*)
    case Half_toFloat   => ValueConvert(F16, F32)(args: _*)

  } orElse {
    super.loadIntrinsicWithBody(target, itype, caller, args)
  }

}
