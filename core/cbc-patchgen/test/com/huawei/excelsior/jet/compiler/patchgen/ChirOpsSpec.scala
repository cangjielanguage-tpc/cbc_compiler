/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

/** Tests for the CHIR mutation primitives (port of Expression.cpp / Value.cpp /
  * CHIRBuilder.h semantics).
  */
class ChirOpsSpec extends AnyFunSuite {

  private val tI32 = TypeImpl(TypeKind.Int32)
  private val tBool = TypeImpl(TypeKind.Boolean)
  private val tUnit = TypeImpl(TypeKind.Unit)

  private def funcType = FuncType(false, false, ArrayBuffer(tI32, tUnit))

  /** A function `@T.f` with one empty entry block. */
  private def mkFunc(identifier: String = "@T.f"): (Func, BlockGroup, Block) = {
    val f = Func(ArrayBuffer(), None, 0L, Some(funcType), identifier, "f", identifier, "T",
      None, ArrayBuffer(), None, None, FuncKind.Default, false, false, None,
      ArrayBuffer(), None, None, ArrayBuffer(), None, None, 11L, 1L, 1L)
    val bg = BlockGroup(ArrayBuffer(), None, 0L, None, "0", None, ArrayBuffer(), Some(f), None)
    val b0 = Block(ArrayBuffer(), None, 0L, None, "0", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.entryBlock = Some(b0)
    bg.blocks += b0
    f.body = Some(bg)
    (f, bg, b0)
  }

  private def pkgOf(f: Func): Package = {
    val pkg = new Package()
    pkg.globalFuncs += f
    pkg
  }

  private def alloc(tpe: Type, owner: Block): AllocateExpr = {
    val e = AllocateExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None, tpe,
      isTry = false, ArrayBuffer())
    e.resultVar = Some(LocalVar(ArrayBuffer(), None, 0L, Some(ExprBuilder.refTy(tpe)),
      "%1", Some(e), isRetVal = false, ""))
    ChirOps.appendExpression(owner, e)
    e
  }

  private def store(value: Value, location: Value, owner: Block): StoreExpr = {
    val e = StoreExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None, value, location)
    e.resultVar = Some(LocalVar(ArrayBuffer(), None, 0L, Some(tUnit), "%2", Some(e), isRetVal = false, ""))
    ChirOps.appendExpression(owner, e)
    e
  }

  // ---------------------------------------------------------------- traversal

  test("visitFuncExprs visits blocks/expressions/lambda bodies in order") {
    val (f, bg, b0) = mkFunc()
    val b1 = Block(ArrayBuffer(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.blocks += b1
    val e0 = alloc(tI32, b0)
    val e1 = alloc(tI32, b1)

    // a lambda in b1 with one expression of its own
    val lambda = LambdaExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(),
      Some(LocalVar(ArrayBuffer(), None, 0L, Some(funcType), "%3", None, isRetVal = false, "")),
      funcType, isLocalFunc = true, "@T.f.lam", "<closure>",
      ArrayBuffer(), ArrayBuffer(), None, None, isCompileTimeValue = false)
    val lamBg = BlockGroup(ArrayBuffer(), None, 0L, None, "0", None, ArrayBuffer(), None, Some(lambda))
    val lb0 = Block(ArrayBuffer(), None, 0L, None, "0", Some(lamBg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    lamBg.entryBlock = Some(lb0)
    lamBg.blocks += lb0
    lambda.body = Some(lamBg)
    lambda.blockGroups += lamBg
    val lamExpr = alloc(tI32, lb0)
    ChirOps.appendExpression(b1, lambda)

    val visited = ArrayBuffer[Expression]()
    ChirOps.visitFuncExprs(f)(visited += _)
    assert(visited.toList == List[Expression](e0, e1, lambda, lamExpr))
    assert(e0.owner.contains(b0))
    assert(lambda.owner.contains(b1))
  }

  test("visitFuncExprs iterates a snapshot: removing the current expr keeps the walk alive") {
    val (f, bg, b0) = mkFunc()
    val b1 = Block(ArrayBuffer(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.blocks += b1
    alloc(tI32, b0)
    alloc(tI32, b0)
    alloc(tI32, b1)
    alloc(tI32, b1)

    val visited = ArrayBuffer[Expression]()
    ChirOps.visitFuncExprs(f) { e =>
      visited += e
      ChirOps.removeSelfFromBlock(e) // destructive pre-visit, like genPatches
    }
    assert(visited.size == 4)
    assert(b0.exprs.isEmpty && b1.exprs.isEmpty)
  }

  // ---------------------------------------------------------------- operands

  test("operandsOf order matches C++ Expression::GetOperands") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(ArrayBuffer(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.blocks += b1
    val lv = alloc(tI32, b0).resultVar.get

    val store = StoreExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None, lv, lv)
    assert(ChirOps.operandsOf(store).toList == Seq[Value](lv, lv))

    val callee = Func(ArrayBuffer(), None, 0L, None, "@T.g", "g", "@T.g", "T", None,
      ArrayBuffer(), None, None, FuncKind.Default, false, false, None, ArrayBuffer(), None,
      None, ArrayBuffer(), None, None, 1L, 1L, 1L)
    val apply = ApplyExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None,
      ArrayBuffer(), None, callee, ArrayBuffer(lv), isSuperCall = false, isTry = false, ArrayBuffer())
    assert(ChirOps.operandsOf(apply).toList == Seq[Value](callee, lv))

    val tryApply = ApplyExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None,
      ArrayBuffer(), None, callee, ArrayBuffer(lv), isSuperCall = false, isTry = true,
      ArrayBuffer(b1))
    assert(ChirOps.operandsOf(tryApply).toList == Seq[Value](callee, lv, b1))

    val invoke = InvokeExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None,
      ArrayBuffer(), None, callee, lv, ArrayBuffer(lv), OverflowStrategy.Na, isTry = false,
      ArrayBuffer())
    assert(ChirOps.operandsOf(invoke).toList == Seq[Value](callee, lv, lv))

    val mbranch = MultiBranchExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None, lv,
      ArrayBuffer(0L, 1L), b1, ArrayBuffer(b0, b1))
    assert(ChirOps.operandsOf(mbranch).toList == Seq[Value](lv, b1, b0, b1))

    val branch = BranchExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None, lv, b0, b1,
      SourceExprTag.IfExpr)
    assert(ChirOps.operandsOf(branch).toList == Seq[Value](lv, b0, b1))

    val exit = ExitExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None)
    assert(ChirOps.operandsOf(exit).isEmpty)

    val lambda = LambdaExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None, funcType,
      isLocalFunc = true, "@T.f.lam", "<closure>", ArrayBuffer(), ArrayBuffer(), None, None,
      isCompileTimeValue = false)
    assert(ChirOps.operandsOf(lambda).isEmpty)
  }

  // ---------------------------------------------------------------- users

  test("usersOf finds uses inside lambdas, identity-based") {
    val (f, _, b0) = mkFunc()
    val pkg = pkgOf(f)
    val lv = alloc(tI32, b0).resultVar.get
    val s = store(lv, lv, b0)

    // second user inside a lambda body
    val lambda = LambdaExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(),
      Some(LocalVar(ArrayBuffer(), None, 0L, Some(funcType), "%3", None, isRetVal = false, "")),
      funcType, isLocalFunc = true, "@T.f.lam", "<closure>",
      ArrayBuffer(), ArrayBuffer(), None, None, isCompileTimeValue = false)
    val lamBg = BlockGroup(ArrayBuffer(), None, 0L, None, "0", None, ArrayBuffer(), None, Some(lambda))
    val lb0 = Block(ArrayBuffer(), None, 0L, None, "0", Some(lamBg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    lamBg.entryBlock = Some(lb0)
    lamBg.blocks += lb0
    lambda.body = Some(lamBg)
    lambda.blockGroups += lamBg
    val s2 = store(lv, lv, lb0)
    ChirOps.appendExpression(b0, lambda)

    val users = ChirOps.usersOf(lv, pkg)
    assert(users.toSet == Set[Expression](s, s2))

    val unused = LocalVar(ArrayBuffer(), None, 0L, Some(tI32), "%99", None, isRetVal = false, "")
    assert(ChirOps.usersOf(unused, pkg).isEmpty)
  }

  // ---------------------------------------------------------------- replaceOperand

  test("replaceOperand replaces scalar and repeated array operands") {
    val (_, _, b0) = mkFunc()
    val oldV = alloc(tI32, b0).resultVar.get
    val newV = alloc(tI32, b0).resultVar.get

    val tuple = TupleExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None,
      ArrayBuffer(oldV, newV, oldV))
    ChirOps.replaceOperand(tuple, oldV, newV)
    assert(tuple.elementValues.toSeq == Seq[Value](newV, newV, newV))

    val bin = BinaryExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None, BinaryOp.Add,
      OverflowStrategy.Na, oldV, newV, isTry = false, ArrayBuffer())
    ChirOps.replaceOperand(bin, oldV, newV)
    assert(bin.leftOperand eq newV)
  }

  test("replaceOperand on a block operand updates predecessor edges (deduped)") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(ArrayBuffer(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    val b2 = Block(ArrayBuffer(), None, 0L, None, "2", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.blocks ++= ArrayBuffer(b1, b2)
    b1.predecessors += b0

    val goto = GotoExpr(ArrayBuffer(), None, 0L, Some(b0), ArrayBuffer(), None, b1)
    ChirOps.replaceOperand(goto, b1, b2)
    assert(goto.destination eq b2)
    assert(!b1.predecessors.exists(_ eq b0))
    assert(b2.predecessors.count(_ eq b0) == 1)

    // replacing with the same value is a no-op
    val goto2 = GotoExpr(ArrayBuffer(), None, 0L, Some(b0), ArrayBuffer(), None, b1)
    b1.predecessors += b0
    ChirOps.replaceOperand(goto2, b1, b1)
    assert(b1.predecessors.count(_ eq b0) == 1)
  }

  // ---------------------------------------------------------------- block primitives

  test("appendExpression sets owner, appends, adds successor preds; re-append moves") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(ArrayBuffer(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.blocks += b1

    val e = AllocateExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None, tI32,
      isTry = false, ArrayBuffer())
    e.resultVar = Some(LocalVar(ArrayBuffer(), None, 0L, Some(ExprBuilder.refTy(tI32)),
      "%1", Some(e), isRetVal = false, ""))
    ChirOps.appendExpression(b0, e)
    assert(e.owner.contains(b0))
    assert(b0.exprs.last eq e)

    // a goto in b0 targets b1; appending goto must add the pred edge
    val goto = GotoExpr(ArrayBuffer(), None, 0L, Some(b0), ArrayBuffer(), None, b1)
    goto.resultVar = None
    ChirOps.appendExpression(b0, goto)
    assert(b1.predecessors.count(_ eq b0) == 1)

    // appending the same expr to b1 moves it (removes from b0)
    ChirOps.appendExpression(b1, goto)
    assert(goto.owner.contains(b1))
    assert(b0.exprs.last eq e)
    assert(b1.exprs.last eq goto)
  }

  test("removeSelfFromBlock drops the expr and its successors' pred edges") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(ArrayBuffer(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.blocks += b1
    b1.predecessors += b0
    val goto = GotoExpr(ArrayBuffer(), None, 0L, Some(b0), ArrayBuffer(), None, b1)
    goto.resultVar = None
    ChirOps.appendExpression(b0, goto)

    ChirOps.removeSelfFromBlock(goto)
    assert(goto.owner.isEmpty)
    assert(!b0.exprs.contains(goto))
    assert(b1.predecessors.isEmpty)
  }

  test("removeSelfFromBlockGroup unlinks the block and its expressions") {
    val (f, bg, b0) = mkFunc()
    val e = alloc(tI32, b0)
    val b1 = Block(ArrayBuffer(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.blocks += b1

    ChirOps.removeSelfFromBlockGroup(b1)
    assert(!bg.blocks.contains(b1))
    assert(b1.parentGroup.isEmpty)
    assert(b1.exprs.isEmpty)

    ChirOps.removeSelfFromBlockGroup(b0)
    assert(bg.entryBlock.isEmpty)
    assert(!b0.exprs.contains(e))
    assert(e.owner.isEmpty)
  }

  // ---------------------------------------------------------------- replaceWith

  test("replaceWith rewires users to the new result and keeps the position") {
    val (f, bg, b0) = mkFunc()
    val pkg = pkgOf(f)
    val oldAlloc = alloc(tI32, b0)
    val s = store(oldAlloc.resultVar.get, oldAlloc.resultVar.get, b0)

    val newAlloc = alloc(tI32, b0)
    ChirOps.replaceWith(oldAlloc, newAlloc, pkg)

    // users of the old result now use the new one
    assert(s.value eq newAlloc.resultVar.get)
    assert(s.location eq newAlloc.resultVar.get)
    // old removed from block, new at the same position
    assert(!b0.exprs.contains(oldAlloc))
    assert(b0.exprs.indexOf(newAlloc) == 0)
    assert(newAlloc.owner.contains(b0))
    // position: [newAlloc, store] — old alloc gone, new at index 0
    assert(b0.exprs.size == 2)
    assert(b0.exprs.last eq s)
    assert(bg.entryBlock.contains(b0))
  }

  // ---------------------------------------------------------------- move*

  test("moveBefore/moveAfter reposition within and across blocks") {
    val (_, _, b0) = mkFunc()
    val e1 = alloc(tI32, b0)
    val e2 = alloc(tI32, b0)
    val e3 = alloc(tI32, b0)

    ChirOps.moveAfter(e1, e3)
    assert(b0.exprs.toSeq == Seq[Expression](e2, e3, e1))

    ChirOps.moveBefore(e1, e2)
    assert(b0.exprs.toSeq == Seq[Expression](e1, e2, e3))
  }

  test("moveAfter a terminator carries predecessor edges to the target block") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(ArrayBuffer(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    val b2 = Block(ArrayBuffer(), None, 0L, None, "2", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.blocks ++= ArrayBuffer(b1, b2)

    val goto = GotoExpr(ArrayBuffer(), None, 0L, Some(b0), ArrayBuffer(), None, b1)
    goto.resultVar = None
    ChirOps.appendExpression(b0, goto)
    assert(b1.predecessors.count(_ eq b0) == 1)

    ChirOps.moveAfter(goto, alloc(tI32, b2)) // move into b2
    assert(goto.owner.contains(b2))
    assert(!b1.predecessors.exists(_ eq b0))
    assert(b1.predecessors.exists(_ eq b2))
  }

  test("moveTo appends to the target block with pred-edge fixup") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(ArrayBuffer(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg.blocks += b1
    val goto = GotoExpr(ArrayBuffer(), None, 0L, Some(b0), ArrayBuffer(), None, b1)
    goto.resultVar = None
    ChirOps.appendExpression(b0, goto)

    ChirOps.moveTo(goto, b1)
    assert(goto.owner.contains(b1))
    assert(b1.exprs.last eq goto)
  }

  // ---------------------------------------------------------------- function bodies

  test("destroyFuncBody clears blocks' exprs and retVal (blocks stay in the group, like C++)") {
    val (f, bg, b0) = mkFunc()
    val e = alloc(tI32, b0)
    f.retVal = Some(e.resultVar.get)

    ChirOps.destroyFuncBody(f)
    assert(f.body.isEmpty)
    assert(f.retVal.isEmpty)
    assert(bg.blocks.toSeq == Seq[Block](b0)) // C++ leaves the empty blocks in place
    assert(b0.exprs.isEmpty)
    assert(e.owner.isEmpty)
  }

  test("replaceBody sets the new body and its owner") {
    val (f, _, _) = mkFunc()
    val bg2 = BlockGroup(ArrayBuffer(), None, 0L, None, "0", None, ArrayBuffer(), None, None)
    val nb0 = Block(ArrayBuffer(), None, 0L, None, "0", Some(bg2), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    bg2.entryBlock = Some(nb0)
    bg2.blocks += nb0

    ChirOps.replaceBody(f, bg2)
    assert(f.body.contains(bg2))
    assert(bg2.ownedFunc.contains(f))
  }

  // ---------------------------------------------------------------- identifiers

  test("mintResult mints %<n> from the top-level localId counter") {
    val (f, _, b0) = mkFunc() // next id to mint = 11
    val e = AllocateExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(), None, tI32,
      isTry = false, ArrayBuffer())
    val lv = ChirOps.mintResult(f, ExprBuilder.refTy(tI32), e)
    val refTpe = lv.tpe.get.asInstanceOf[TypeImpl]
    assert(refTpe.kind == TypeKind.Ref && (refTpe.argTys.head eq tI32))
    assert(lv.associatedExpr.contains(e))
    assert(f.localId == 12L)
    assert(lv.identifier == "%11")

    val lv2 = ChirOps.mintResult(f, tI32, e)
    assert(lv2.identifier == "%12")
  }

  test("topLevelFunc resolves through a lambda expression") {
    val (f, _, b0) = mkFunc()
    val lambda = LambdaExpr(ArrayBuffer(), None, 0L, None, ArrayBuffer(),
      Some(LocalVar(ArrayBuffer(), None, 0L, Some(funcType), "%3", None, isRetVal = false, "")),
      funcType, isLocalFunc = true, "@T.f.lam", "<closure>",
      ArrayBuffer(), ArrayBuffer(), None, None, isCompileTimeValue = false)
    val lamBg = BlockGroup(ArrayBuffer(), None, 0L, None, "0", None, ArrayBuffer(), None, Some(lambda))
    lambda.body = Some(lamBg)
    lambda.blockGroups += lamBg
    ChirOps.appendExpression(b0, lambda)

    assert(ChirOps.topLevelFunc(lamBg).contains(f))
    assert(ChirOps.topLevelFunc(f.body.get).contains(f))
  }
}
