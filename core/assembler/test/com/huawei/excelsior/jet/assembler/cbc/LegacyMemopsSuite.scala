/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 */
package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessAnalyzer
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.{Assembler, SymbolAdapter}
import org.scalatest.funsuite.AnyFunSuite

class LegacyMemopsSuite extends AnyFunSuite {
  private def assembler() = {
    val asm = new Assembler with SymbolAdapter {
      override def adapt(symbol: Symbol): BytecodeReference = throw new AssertionError(symbol)
    }
    asm.setUp()
    asm.analyzer = new LivenessAnalyzer(strict = true)
    asm
  }

  test("array index defines a record pointer from a reference and a primitive index") {
    val asm = assembler()
    asm.instr { asm.analyzer.ref(IR1); asm.analyzer.prim(IR2) }
    asm.instr { asm.index(IR3, IR1, IR2, CangjieArray(BuiltinSignature.I64)) }
    asm.instr { asm.analyzer.useRec(IR3); asm.analyzer.useRef(IR1) }
  }

  test("generic index defines a fresh record pointer") {
    val asm = assembler()
    asm.instr { asm.analyzer.rec(IR1); asm.analyzer.prim(IR2); asm.analyzer.prim(IR4) }
    asm.instr { asm.index(IR3, IR1, IR2, IR4) }
    asm.instr { asm.analyzer.useRec(IR3) }
  }

  test("copy retains the two base references and record pointers") {
    val asm = assembler()
    asm.instr {
      asm.analyzer.ref(IR1); asm.analyzer.rec(IR2)
      asm.analyzer.ref(IR3); asm.analyzer.rec(IR4); asm.analyzer.prim(IR5)
    }
    asm.instr { asm.copy(IR1, IR2, IR3, IR4, BuiltinSignature.I64) }
    asm.instr { asm.copy(IR1, IR2, IR3, IR4, IR5) }
    asm.instr {
      asm.analyzer.useRef(IR1); asm.analyzer.useRec(IR2)
      asm.analyzer.useRef(IR3); asm.analyzer.useRec(IR4)
    }
  }
}
