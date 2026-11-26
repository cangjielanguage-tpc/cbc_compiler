/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.{Assembler, SymbolAdapter}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AtomicReferenceLivenessSuite extends AnyFunSuite with Matchers {

  private def checkAtomicLiveness(fieldType: Signature, reference: Boolean): Unit = {
    val asm = new Assembler with SymbolAdapter {
      def adapt(symbol: Symbol): BytecodeReference = throw new UnsupportedOperationException
    }
    val liveness = new LivenessAnalyzer(strict = true)
    asm.analyzer = liveness
    val field = SingleFieldReference(refType = TypeSignature.ref("test:Atomic"), name = "value", fieldType = fieldType)

    liveness.op {
      liveness.ref(IR1)
      if (reference) liveness.ref(IR2) else liveness.prim(IR2)
      if (reference) liveness.ref(IR3) else liveness.prim(IR3)
    }
    val code = asm.withNewSegment {
      asm.atomicStore(IR2, IR1, field)
      asm.atomicLoad(IR4, IR1, field)
      asm.cas(IR5, IR1, IR2, IR3, field)
      asm.atomicSwap(IR6, IR1, IR2, field)
    }
    liveness.op {
      if (reference) liveness.useRef(IR4) else liveness.usePrim(IR4)
      liveness.usePrim(IR5)
      if (reference) liveness.useRef(IR6) else liveness.usePrim(IR6)
    }
    code.length shouldBe 14
  }

  for (payload <- Seq(ClassTypeVariable(0), FuncTypeVariable(0))) {
    test(s"atomic generic Option<$payload> operands and results remain live references") {
      checkAtomicLiveness(OptionSignature("std.core:Option", Seq(payload), isReference = false), reference = true)
    }
  }

  test("atomic generic payloads remain live references") {
    checkAtomicLiveness(ClassTypeVariable(0), reference = true)
  }

  test("atomic concrete nullable options remain live references") {
    checkAtomicLiveness(OptionSignature("std.core:Option", Seq(TypeSignature.ref("test:Item")), isReference = true), reference = true)
  }

  test("atomic integer operands and results remain primitive") {
    checkAtomicLiveness(BuiltinSignature.I64, reference = false)
  }
}
