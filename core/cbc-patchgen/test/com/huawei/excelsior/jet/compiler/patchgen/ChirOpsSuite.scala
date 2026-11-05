/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

/** Tests for the CHIR mutation primitives (port of Expression.cpp / Value.cpp /
  * CHIRBuilder.h semantics).
  */
class ChirOpsSuite extends AnyFunSuite with Matchers {

  private val tI32 = TypeImpl(TypeKind.Int32)
  private val tBool = TypeImpl(TypeKind.Boolean)
  private val tUnit = TypeImpl(TypeKind.Unit)

  private def funcType = FuncType(false, false, ArrayBuffer(tI32, tUnit))

  /** A function `@T.f` with one empty entry block. */
  private def mkFunc(identifier: String = "@T.f"): (Func, BlockGroup, Block) = {
    val f = Func(tpe = Some(funcType), identifier = identifier, srcCodeIdentifier = "f",
      rawMangledName = identifier, packageName = "T", localId = 11L, blockId = 1L, blockGroupId = 1L)
    val bg = BlockGroup(identifier = "0", ownedFunc = Some(f))
    val b0 = Block(identifier = "0", parentGroup = Some(bg))
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
    val e = AllocateExpr(allocatedType = tpe)
    e.resultVar = Some(LocalVar(tpe = Some(Clone.refTy(tpe)), identifier = "%1",
      associatedExpr = Some(e)))
    ChirOps.appendExpression(owner, e)
    e
  }

  private def store(value: Value, location: Value, owner: Block): StoreExpr = {
    val e = StoreExpr(value0 = value, location0 = location)
    e.resultVar = Some(LocalVar(tpe = Some(tUnit), identifier = "%2", associatedExpr = Some(e)))
    ChirOps.appendExpression(owner, e)
    e
  }

  // ---------------------------------------------------------------- traversal

  test("visitFuncExprs visits blocks/expressions/lambda bodies in order") {
    val (f, bg, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
    bg.blocks += b1
    val e0 = alloc(tI32, b0)
    val e1 = alloc(tI32, b1)

    // a lambda in b1 with one expression of its own
    val lambda = LambdaExpr(resultVar = Some(LocalVar(tpe = Some(funcType), identifier = "%3")),
      funcTy = funcType, isLocalFunc = true, identifier = "@T.f.lam", srcCodeName = "<closure>")
    val lamBg = BlockGroup(identifier = "0", ownedExpression = Some(lambda))
    val lb0 = Block(identifier = "0", parentGroup = Some(lamBg))
    lamBg.entryBlock = Some(lb0)
    lamBg.blocks += lb0
    lambda.body = Some(lamBg)
    lambda.blockGroups += lamBg
    val lamExpr = alloc(tI32, lb0)
    ChirOps.appendExpression(b1, lambda)

    val visited = ArrayBuffer[Expression]()
    ChirOps.visitFuncExprs(f)(visited += _)
    visited.toList shouldBe List[Expression](e0, e1, lambda, lamExpr)
    e0.owner.contains(b0) shouldBe true
    lambda.owner.contains(b1) shouldBe true
  }

  test("visitFuncExprs iterates a snapshot: removing the current expr keeps the walk alive") {
    val (f, bg, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
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
    visited.size shouldBe 4
    (b0.exprs.isEmpty && b1.exprs.isEmpty) shouldBe true
  }

  // ---------------------------------------------------------------- operands

  test("operandsOf order matches C++ Expression::GetOperands") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
    bg.blocks += b1
    val lv = alloc(tI32, b0).resultVar.get

    val store = StoreExpr(value0 = lv, location0 = lv)
    ChirOps.operandsOf(store).toList shouldBe Seq[Value](lv, lv)

    val callee = Func(identifier = "@T.g", srcCodeIdentifier = "g", rawMangledName = "@T.g",
      packageName = "T", localId = 1L, blockId = 1L, blockGroupId = 1L)
    val apply = ApplyExpr(callee0 = callee, args0 = ArrayBuffer(lv))
    ChirOps.operandsOf(apply).toList shouldBe Seq[Value](callee, lv)

    val tryApply = ApplyExpr(callee0 = callee, args0 = ArrayBuffer(lv), isTry = true,
      handlerBlocks0 = ArrayBuffer(b1))
    ChirOps.operandsOf(tryApply).toList shouldBe Seq[Value](callee, lv, b1)

    val invoke = InvokeExpr(callee0 = callee, caller0 = lv, args0 = ArrayBuffer(lv),
      overflowStrategy = OverflowStrategy.Na)
    ChirOps.operandsOf(invoke).toList shouldBe Seq[Value](callee, lv, lv)

    val mbranch = MultiBranchExpr(condition0 = lv, caseValues = ArrayBuffer(0L, 1L),
      defaultBlock0 = b1, normalBlocks0 = ArrayBuffer(b0, b1))
    ChirOps.operandsOf(mbranch).toList shouldBe Seq[Value](lv, b1, b0, b1)

    val branch = BranchExpr(condition0 = lv, trueBlock0 = b0, falseBlock0 = b1,
      sourceExpr = SourceExprTag.IfExpr)
    ChirOps.operandsOf(branch).toList shouldBe Seq[Value](lv, b0, b1)

    val exit = ExitExpr()
    ChirOps.operandsOf(exit).isEmpty shouldBe true

    val lambda = LambdaExpr(funcTy = funcType, isLocalFunc = true, identifier = "@T.f.lam",
      srcCodeName = "<closure>")
    ChirOps.operandsOf(lambda).isEmpty shouldBe true
  }

  // ---------------------------------------------------------------- users

  test("usersOf finds uses inside lambdas, identity-based") {
    val (f, _, b0) = mkFunc()
    val pkg = pkgOf(f)
    val lv = alloc(tI32, b0).resultVar.get
    val s = store(lv, lv, b0)

    // second user inside a lambda body
    val lambda = LambdaExpr(resultVar = Some(LocalVar(tpe = Some(funcType), identifier = "%3")),
      funcTy = funcType, isLocalFunc = true, identifier = "@T.f.lam", srcCodeName = "<closure>")
    val lamBg = BlockGroup(identifier = "0", ownedExpression = Some(lambda))
    val lb0 = Block(identifier = "0", parentGroup = Some(lamBg))
    lamBg.entryBlock = Some(lb0)
    lamBg.blocks += lb0
    lambda.body = Some(lamBg)
    lambda.blockGroups += lamBg
    val s2 = store(lv, lv, lb0)
    ChirOps.appendExpression(b0, lambda)

    val users = ChirOps.usersOf(lv, pkg)
    users.toSet shouldBe Set[Expression](s, s2)

    val unused = LocalVar(tpe = Some(tI32), identifier = "%99")
    ChirOps.usersOf(unused, pkg).isEmpty shouldBe true
  }

  // ---------------------------------------------------------------- replaceOperand

  test("replaceOperand replaces scalar and repeated array operands") {
    val (_, _, b0) = mkFunc()
    val oldV = alloc(tI32, b0).resultVar.get
    val newV = alloc(tI32, b0).resultVar.get

    val tuple = TupleExpr(elementValues0 = ArrayBuffer(oldV, newV, oldV))
    ChirOps.replaceOperand(tuple, oldV, newV)
    tuple.elementValues.toSeq shouldBe Seq[Value](newV, newV, newV)

    val bin = BinaryExpr(op = BinaryOp.Add, overflowStrategy = OverflowStrategy.Na,
      leftOperand0 = oldV, rightOperand0 = newV)
    ChirOps.replaceOperand(bin, oldV, newV)
    bin.leftOperand shouldBe newV
  }

  test("replaceOperand on a block operand updates predecessor edges (deduped)") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
    val b2 = Block(identifier = "2", parentGroup = Some(bg))
    bg.blocks ++= ArrayBuffer(b1, b2)
    b1.predecessors += b0

    val goto = GotoExpr(owner = Some(b0), destination0 = b1)
    ChirOps.replaceOperand(goto, b1, b2)
    goto.destination shouldBe b2
    b1.predecessors.exists(_ eq b0) shouldBe false
    b2.predecessors.count(_ eq b0) shouldBe 1

    // replacing with the same value is a no-op
    val goto2 = GotoExpr(owner = Some(b0), destination0 = b1)
    b1.predecessors += b0
    ChirOps.replaceOperand(goto2, b1, b1)
    b1.predecessors.count(_ eq b0) shouldBe 1
  }

  // ---------------------------------------------------------------- block primitives

  test("appendExpression sets owner, appends, adds successor preds; re-append moves") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
    bg.blocks += b1

    val e = AllocateExpr(allocatedType = tI32)
    e.resultVar = Some(LocalVar(tpe = Some(Clone.refTy(tI32)), identifier = "%1",
      associatedExpr = Some(e)))
    ChirOps.appendExpression(b0, e)
    e.owner.contains(b0) shouldBe true
    b0.exprs.last shouldBe e

    // a goto in b0 targets b1; appending goto must add the pred edge
    val goto = GotoExpr(owner = Some(b0), destination0 = b1)
    goto.resultVar = None
    ChirOps.appendExpression(b0, goto)
    b1.predecessors.count(_ eq b0) shouldBe 1

    // appending the same expr to b1 moves it (removes from b0)
    ChirOps.appendExpression(b1, goto)
    goto.owner.contains(b1) shouldBe true
    b0.exprs.last shouldBe e
    b1.exprs.last shouldBe goto
  }

  test("removeSelfFromBlock drops the expr and its successors' pred edges") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
    bg.blocks += b1
    b1.predecessors += b0
    val goto = GotoExpr(owner = Some(b0), destination0 = b1)
    goto.resultVar = None
    ChirOps.appendExpression(b0, goto)

    ChirOps.removeSelfFromBlock(goto)
    goto.owner.isEmpty shouldBe true
    b0.exprs.contains(goto) shouldBe false
    b1.predecessors.isEmpty shouldBe true
  }

  test("removeSelfFromBlockGroup unlinks the block and its expressions") {
    val (f, bg, b0) = mkFunc()
    val e = alloc(tI32, b0)
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
    bg.blocks += b1

    ChirOps.removeSelfFromBlockGroup(b1)
    bg.blocks.contains(b1) shouldBe false
    b1.parentGroup.isEmpty shouldBe true
    b1.exprs.isEmpty shouldBe true

    ChirOps.removeSelfFromBlockGroup(b0)
    bg.entryBlock.isEmpty shouldBe true
    b0.exprs.contains(e) shouldBe false
    e.owner.isEmpty shouldBe true
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
    s.value shouldBe newAlloc.resultVar.get
    s.location shouldBe newAlloc.resultVar.get
    // old removed from block, new at the same position
    b0.exprs.contains(oldAlloc) shouldBe false
    b0.exprs.indexOf(newAlloc) shouldBe 0
    newAlloc.owner.contains(b0) shouldBe true
    // position: [newAlloc, store] — old alloc gone, new at index 0
    b0.exprs.size shouldBe 2
    b0.exprs.last shouldBe s
    bg.entryBlock.contains(b0) shouldBe true
  }

  // ---------------------------------------------------------------- move*

  test("moveBefore/moveAfter reposition within and across blocks") {
    val (_, _, b0) = mkFunc()
    val e1 = alloc(tI32, b0)
    val e2 = alloc(tI32, b0)
    val e3 = alloc(tI32, b0)

    ChirOps.moveAfter(e1, e3)
    b0.exprs.toSeq shouldBe Seq[Expression](e2, e3, e1)

    ChirOps.moveBefore(e1, e2)
    b0.exprs.toSeq shouldBe Seq[Expression](e1, e2, e3)
  }

  test("moveAfter a terminator carries predecessor edges to the target block") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
    val b2 = Block(identifier = "2", parentGroup = Some(bg))
    bg.blocks ++= ArrayBuffer(b1, b2)

    val goto = GotoExpr(owner = Some(b0), destination0 = b1)
    goto.resultVar = None
    ChirOps.appendExpression(b0, goto)
    b1.predecessors.count(_ eq b0) shouldBe 1

    ChirOps.moveAfter(goto, alloc(tI32, b2)) // move into b2
    goto.owner.contains(b2) shouldBe true
    b1.predecessors.exists(_ eq b0) shouldBe false
    b1.predecessors.exists(_ eq b2) shouldBe true
  }

  test("moveTo appends to the target block with pred-edge fixup") {
    val (_, bg, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
    bg.blocks += b1
    val goto = GotoExpr(owner = Some(b0), destination0 = b1)
    goto.resultVar = None
    ChirOps.appendExpression(b0, goto)

    ChirOps.moveTo(goto, b1)
    goto.owner.contains(b1) shouldBe true
    b1.exprs.last shouldBe goto
  }

  // ---------------------------------------------------------------- function bodies

  test("destroyFuncBody clears blocks' exprs and retVal (blocks stay in the group, like C++)") {
    val (f, bg, b0) = mkFunc()
    val e = alloc(tI32, b0)
    f.retVal = Some(e.resultVar.get)

    ChirOps.destroyFuncBody(f)
    f.body.isEmpty shouldBe true
    f.retVal.isEmpty shouldBe true
    bg.blocks.toSeq shouldBe Seq[Block](b0) // C++ leaves the empty blocks in place
    b0.exprs.isEmpty shouldBe true
    e.owner.isEmpty shouldBe true
  }

  test("replaceBody sets the new body and its owner") {
    val (f, _, _) = mkFunc()
    val bg2 = BlockGroup(identifier = "0")
    val nb0 = Block(identifier = "0", parentGroup = Some(bg2))
    bg2.entryBlock = Some(nb0)
    bg2.blocks += nb0

    ChirOps.replaceBody(f, bg2)
    f.body.contains(bg2) shouldBe true
    bg2.ownedFunc.contains(f) shouldBe true
  }

  // ---------------------------------------------------------------- identifiers

  test("mintResult mints %<n> from the top-level localId counter") {
    val (f, _, b0) = mkFunc() // next id to mint = 11
    val e = AllocateExpr(allocatedType = tI32)
    val lv = ChirOps.mintResult(f, Clone.refTy(tI32), e)
    val refTpe = lv.tpe.get.asInstanceOf[TypeImpl]
    refTpe.kind shouldBe TypeKind.Ref
    refTpe.argTys.head shouldBe tI32
    lv.associatedExpr.contains(e) shouldBe true
    f.localId shouldBe 12L
    lv.identifier shouldBe "%11"

    val lv2 = ChirOps.mintResult(f, tI32, e)
    lv2.identifier shouldBe "%12"
  }

  test("topLevelFunc resolves through a lambda expression") {
    val (f, _, b0) = mkFunc()
    val lambda = LambdaExpr(resultVar = Some(LocalVar(tpe = Some(funcType), identifier = "%3")),
      funcTy = funcType, isLocalFunc = true, identifier = "@T.f.lam", srcCodeName = "<closure>")
    val lamBg = BlockGroup(identifier = "0", ownedExpression = Some(lambda))
    lambda.body = Some(lamBg)
    lambda.blockGroups += lamBg
    ChirOps.appendExpression(b0, lambda)

    ChirOps.topLevelFunc(lamBg).contains(f) shouldBe true
    ChirOps.topLevelFunc(f.body.get).contains(f) shouldBe true
  }
}
