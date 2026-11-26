/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.{CangjieFieldReference, SignatureType}

class AtomicOptionReferenceSuite extends CompilerSuite with GlobalNodesBuilder {

  private def checkAtomicTypes(fieldType: SignatureType, valueType: Type): Unit = {
    val field = CangjieFieldReference(tCangjieType.sigType, fieldType)
    currentScope.inState(entryBlock, entryMemory) {
      val obj = Fake(TRefType)
      val value = Fake(valueType)
      val compare = Fake(valueType)
      val load = AtomicOps.Load.proto(TRefType, field).exact(obj)
      val store = AtomicOps.Store.proto(TRefType, field).exact(obj, PutMemoryOperation.adjustValue(fieldType.toAsm, value))
      val cas = AtomicOps.CAS.proto(TRefType, field).exact(obj, compare, value)
      val swap = AtomicOps.Simple.proto(AtomicOps.Simple.Kind.SWAP, TRefType, field).exact(obj, value)

      load.tpe shouldBe valueType
      load.accessType shouldBe fieldType.toAsm
      store.value shouldBe value
      cas.tpe shouldBe IntType
      cas.compareValue shouldBe compare
      cas.swapValue shouldBe value
      swap.tpe shouldBe valueType
      swap.value shouldBe value
      for (node <- Seq(load, store, cas, swap)) {
        node.field shouldBe field
        node.fieldType shouldBe fieldType.toAsm
      }
    }
  }

  for (payload <- Seq(SignatureType.ClassTypeVariable(0), SignatureType.LocalTypeVariable(0))) {
    test(s"atomic Option<$payload> uses traced references for load, store, CAS and swap") {
      val option = SignatureType.OptionLikeEnum("std.core:Option", Seq(payload), payload)
      option.toAsm shouldBe AsmType.PTR
      checkAtomicTypes(option, TRefType)
    }
  }

  test("atomic reference and concrete nullable option retain reference types") {
    val payload = tCangjieType.sigType
    checkAtomicTypes(payload, TRefType)
    checkAtomicTypes(SignatureType.OptionLikeEnum("std.core:Option", Seq(payload), payload), TRefType)
  }

  test("atomic type variables retain traced reference types") {
    checkAtomicTypes(SignatureType.ClassTypeVariable(0), TRefType)
  }

  test("atomic integer operations retain primitive types") {
    checkAtomicTypes(SignatureType.Int32, IntType)
    checkAtomicTypes(SignatureType.Int64, LongType)
  }
}
