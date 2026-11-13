/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.cangjie

import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType

/** Unrolls the 32-byte block vector intrinsics into primitive IR loops.
  *
  * The placeholder nodes are created during CHIR parsing and are expanded
  * right after it (same phase, like ArrayBuiltInCopyTo), so they never reach
  * the backend or serialization.
  */
trait VectorIntrinsicsExpansion { self: Universe =>

  protected def expandVectorCompare32(n: VectorCompare32): Node = {
    val idxTpe = n.off1.tpe

    val entryGoto = Goto()

    val loopBlock = BBlock(entryGoto)
    val idx = Phi.raw(idxTpe)(loopBlock, IntegralConst(idxTpe)(0))
    val loopContinue = If(Cmp(idxTpe, Condition.LT)(idx, IntegralConst(idxTpe)(32)))

    val bodyBlock = BBlock(loopContinue.trueExit)
    val b1 = loadByte(n.arrayType, n.arr1, Add(n.off1, idx))
    val b2 = loadByte(n.arrayType, n.arr2, Add(n.off2, idx))
    val differs = If(Cmp(b1.tpe, Condition.NE)(b1, b2))

    val foundBlock = BBlock(differs.trueExit)
    val foundGoto = Goto()

    val loopBackBlock = BBlock(differs.falseExit)
    val loopBackGoto = Goto()

    loopBlock.addArg(loopBackGoto)
    idx.addArg(Add(idx, IntegralConst(idxTpe)(1)))
    commit(idx)

    val exitBlock = BBlock(foundGoto, loopContinue.falseExit)
    Phi(idxTpe)(exitBlock, idx, IntegralConst(idxTpe)(32))
  }

  protected def expandVectorIndexByte32(n: VectorIndexByte32): Node = {
    val idxTpe = n.off.tpe

    val entryGoto = Goto()

    val loopBlock = BBlock(entryGoto)
    val idx = Phi.raw(idxTpe)(loopBlock, IntegralConst(idxTpe)(0))
    val loopContinue = If(Cmp(idxTpe, Condition.LT)(idx, IntegralConst(idxTpe)(32)))

    val bodyBlock = BBlock(loopContinue.trueExit)
    val b = loadByte(n.arrayType, n.arr, Add(n.off, idx))
    val hits = If(Cmp(b.tpe, Condition.EQ)(b, n.target))

    val foundBlock = BBlock(hits.trueExit)
    val foundGoto = Goto()

    val loopBackBlock = BBlock(hits.falseExit)
    val loopBackGoto = Goto()

    loopBlock.addArg(loopBackGoto)
    idx.addArg(Add(idx, IntegralConst(idxTpe)(1)))
    commit(idx)

    val exitBlock = BBlock(foundGoto, loopContinue.falseExit)
    Phi(idxTpe)(exitBlock, idx, IntegralConst(idxTpe)(32))
  }

  private def loadByte(arrayType: SignatureType, array: Node, offset: Node): Node = {
    val elemType = arrayType.getArrayElemType
    assert(!elemType.containsTypeVariables, arrayType)
    val field = IndexFieldReference(arrayType, elemType)(offset)
    LoadFieldSeq(maybeDerivedPtrBase(array), array, field)
  }

  private def maybeDerivedPtrBase(rcv: Node): Node = DerivedPtr.Proxy.resolve(rcv)
}
