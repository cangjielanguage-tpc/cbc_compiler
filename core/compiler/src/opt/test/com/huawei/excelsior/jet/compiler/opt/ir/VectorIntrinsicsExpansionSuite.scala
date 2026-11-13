/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.frontend.cangjie.VectorIntrinsicsExpansion
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import org.scalatest.Inside.inside

class VectorIntrinsicsExpansionSuite extends CompilerSuite
  with GlobalNodesBuilder
  with CFGTransformationDSL
  with VectorIntrinsicsExpansion {

  override def transformation(): Unit = {}

  override def makeDebug: Boolean = false

  private val byteArrayType = SignatureType.CangjieArray(SignatureType.UInt8)

  private def checkByteLoad(b: LoadFieldSeq): Unit = {
    inside(b.fields) {
      case Seq(fr: IndexFieldReference) =>
        fr.refType shouldBe byteArrayType
        fr.fieldType shouldBe SignatureType.UInt8
        fr.idx shouldBe an[Add]
    }
  }

  test("VectorCompare32 unrolls into a byte compare loop") {
    makeCFG(0)

    makeNodes { at =>
      at(0)
      val placeholder = VectorCompare32(byteArrayType)(Fake(TRefType), Fake(LongType), Fake(TRefType), Fake(LongType))
      n("vc") = placeholder
      n("use") = FakeSpinalUnary(LongType)(placeholder)
    }

    val vc = n("vc").asInstanceOf[VectorCompare32]
    replaceByCode(vc) {
      expandVectorCompare32(vc)
    }

    // The placeholder is replaced by the unrolled loop.
    vc.isCommitted shouldBe false
    all[VectorCompare32] shouldBe empty

    // The use consumes the result phi merging the found index with the miss constant 32.
    inside(n("use").asInstanceOf[FakeSpinalUnary].inValue) {
      case Phi(exitBlock: BBlock, loopPhi: Phi, missValue) =>
        exitBlock.inputs should have size 2
        missValue shouldBe IntegralConst(LongType)(32)

        // Loop header: index phi from 0 to 31, guarded by idx < 32.
        val loopBlock = loopPhi.block
        loopBlock.inputs should have size 2
        loopPhi.argsSeq.toSeq should matchPattern {
          case Seq(IntegralConst(0), Add(`loopPhi`, IntegralConst(1))) =>
        }
        loopBlock.blockEnd should matchPattern {
          case If(Cmp(Condition.LT, `loopPhi`, IntegralConst(32))) =>
        }

        // Body: two unchecked byte loads compared for inequality.
        val bodyBlock = loopBlock.blockEnd.asInstanceOf[If].trueExit.target
        inside(bodyBlock.blockEnd) {
          case If(Cmp(Condition.NE, b1: LoadFieldSeq, b2: LoadFieldSeq)) =>
            checkByteLoad(b1)
            checkByteLoad(b2)
        }
    }

    checkIRConsistency(CheckLevels.Important)
  }

  test("VectorIndexByte32 unrolls into a byte search loop") {
    makeCFG(0)

    makeNodes { at =>
      at(0)
      val placeholder = VectorIndexByte32(byteArrayType)(Fake(TRefType), Fake(LongType), Fake(IntType))
      n("vi") = placeholder
      n("use") = FakeSpinalUnary(LongType)(placeholder)
    }

    val vi = n("vi").asInstanceOf[VectorIndexByte32]
    val targetNode = vi.target
    replaceByCode(vi) {
      expandVectorIndexByte32(vi)
    }

    // The placeholder is replaced by the unrolled loop.
    vi.isCommitted shouldBe false
    all[VectorIndexByte32] shouldBe empty

    // The use consumes the result phi merging the found index with the miss constant 32.
    inside(n("use").asInstanceOf[FakeSpinalUnary].inValue) {
      case Phi(exitBlock: BBlock, loopPhi: Phi, missValue) =>
        exitBlock.inputs should have size 2
        missValue shouldBe IntegralConst(LongType)(32)

        // Loop header: index phi from 0 to 31, guarded by idx < 32.
        val loopBlock = loopPhi.block
        loopBlock.inputs should have size 2
        loopPhi.argsSeq.toSeq should matchPattern {
          case Seq(IntegralConst(0), Add(`loopPhi`, IntegralConst(1))) =>
        }
        loopBlock.blockEnd should matchPattern {
          case If(Cmp(Condition.LT, `loopPhi`, IntegralConst(32))) =>
        }

        // Body: one unchecked byte load compared with the target byte
        // (identity pass may swap the compared values, keeping the op).
        val bodyBlock = loopBlock.blockEnd.asInstanceOf[If].trueExit.target
        inside(bodyBlock.blockEnd) {
          case If(Cmp(Condition.EQ, t, b: LoadFieldSeq)) =>
            checkByteLoad(b)
            t shouldBe targetNode
        }
    }

    checkIRConsistency(CheckLevels.Important)
  }
}
