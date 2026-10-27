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

/** Tests for expression cloning / creation (port of the C++ per-type Clone impls
  * and CHIRBuilder factories).
  */
class CloneSpec extends AnyFunSuite {

  private val tI32 = TypeImpl(TypeKind.Int32)
  private val tBool = TypeImpl(TypeKind.Boolean)
  private val tUnit = TypeImpl(TypeKind.Unit)

  private def funcType = FuncType(false, false, ArrayBuffer(tI32, tUnit))

  /** A function `@T.f` with one empty entry block, localId = 10. */
  private def mkFunc(): (Func, BlockGroup, Block) = {
    val f = Func(tpe = Some(funcType), identifier = "@T.f", srcCodeIdentifier = "f",
      rawMangledName = "@T.f", packageName = "T", localId = 11L, blockId = 1L, blockGroupId = 1L)
    val bg = BlockGroup(identifier = "0", ownedFunc = Some(f))
    val b0 = Block(identifier = "0", parentGroup = Some(bg))
    bg.entryBlock = Some(b0)
    bg.blocks += b0
    f.body = Some(bg)
    (f, bg, b0)
  }

  private def alloc(tpe: Type, owner: Block): AllocateExpr = {
    val e = AllocateExpr(allocatedType = tpe)
    e.resultVar = Some(LocalVar(tpe = Some(ExprBuilder.refTy(tpe)), identifier = "%1",
      associatedExpr = Some(e)))
    ChirOps.appendExpression(owner, e)
    e
  }

  test("cloneExpr mints a fresh result and shares operands") {
    val (f, _, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(f.body.get))
    f.body.get.blocks += b1

    val orig = alloc(tI32, b0)
    orig.resultVar.get.attributes = Attribute.Public

    val clone = ExprBuilder.cloneExpr(orig, b1).asInstanceOf[AllocateExpr]
    // fresh result, identifier minted from the top-level counter (next = 11)
    assert(clone ne orig)
    assert(clone.resultVar.get ne orig.resultVar.get)
    assert(clone.resultVar.get.identifier == "%11")
    assert(clone.resultVar.get.tpe.contains(orig.resultVar.get.tpe.get))
    // result attributes merged, expression attributes lost (C++ CopyBaseInfoFrom)
    assert((clone.resultVar.get.attributes & Attribute.Public) != 0L)
    assert(clone.attributes == 0L)
    // operand (allocated type) shared
    assert(clone.allocatedType eq orig.allocatedType)
    // owner/position
    assert(clone.owner.contains(b1))
    assert(b1.exprs.last eq clone)
  }

  test("cloneExpr copies annos and loc via copyBaseInfoFrom") {
    val (f, _, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(f.body.get))
    f.body.get.blocks += b1

    val orig = alloc(tI32, b0)
    orig.annos += LinkTypeInfo(Linkage.Internal)
    orig.loc = Some(DebugLocation("F.cj", 3, Some(Pos(1, 2)), Some(Pos(3, 4)), ArrayBuffer()))

    val clone = ExprBuilder.cloneExpr(orig, b1)
    assert(ExprBuilder.getAnno(clone, classOf[LinkTypeInfo]).isDefined)
    assert(clone.loc == orig.loc)
  }

  test("cloneExpr of a Constant shares literals but mints a fresh NullLiteral") {
    val (f, _, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(f.body.get))
    f.body.get.blocks += b1

    val intLit = IntLiteral(tpe = Some(tI32), value = 7L)
    val c1 = ConstantExpr(literal0 = intLit)
    c1.resultVar = Some(LocalVar(tpe = Some(tI32), identifier = "%5", associatedExpr = Some(c1)))
    ChirOps.appendExpression(b0, c1)
    val clone1 = ExprBuilder.cloneExpr(c1, b1).asInstanceOf[ConstantExpr]
    assert(clone1.literal eq intLit)

    val nullLit = NullLiteral(tpe = Some(ExprBuilder.refTy(tI32)))
    val c2 = ConstantExpr(literal0 = nullLit)
    c2.resultVar = Some(LocalVar(tpe = Some(ExprBuilder.refTy(tI32)), identifier = "%6", associatedExpr = Some(c2)))
    ChirOps.appendExpression(b0, c2)
    val clone2 = ExprBuilder.cloneExpr(c2, b1).asInstanceOf[ConstantExpr]
    assert(clone2.literal ne nullLit)
    assert(clone2.literal.isInstanceOf[NullLiteral])
    assert(clone2.literal.asInstanceOf[NullLiteral].tpe.contains(nullLit.tpe.get))
  }

  test("cloneExpr of terminators without results yields no result var") {
    val (f, _, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(f.body.get))
    val b2 = Block(identifier = "2", parentGroup = Some(f.body.get))
    f.body.get.blocks ++= ArrayBuffer(b1, b2)

    val branch = BranchExpr(condition0 = alloc(tI32, b0).resultVar.get, trueBlock0 = b1,
      falseBlock0 = b2, sourceExpr = SourceExprTag.IfExpr)
    val clone = ExprBuilder.cloneExpr(branch, b1).asInstanceOf[BranchExpr]
    assert(clone.resultVar.isEmpty)
    assert(clone.trueBlock eq b1)
    assert(clone.falseBlock eq b2)
  }

  test("cloneExpr of NumericCast copies NeedCheckCast and the result EnumCaseIndex") {
    val (f, _, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(f.body.get))
    f.body.get.blocks += b1

    val lv = alloc(tI32, b0).resultVar.get
    val orig = NumericCastExpr(overflowStrategy = OverflowStrategy.Throwing, value0 = lv)
    orig.resultVar = Some(LocalVar(annos = ArrayBuffer(EnumCaseIndex(5L)),
      tpe = Some(TypeImpl(TypeKind.Int64)), identifier = "%7", associatedExpr = Some(orig)))
    orig.annos += NeedCheckCast(true)
    ChirOps.appendExpression(b0, orig)

    val clone = ExprBuilder.cloneExpr(orig, b1).asInstanceOf[NumericCastExpr]
    assert(ExprBuilder.getAnno(clone, classOf[NeedCheckCast]).exists(_.need))
    assert(clone.resultVar.get.annos.collectFirst { case e: EnumCaseIndex => e.index }.contains(5L))
    assert(clone.overflowStrategy == OverflowStrategy.Throwing)
  }

  test("cloneExpr of a StaticCast: base info (annos) copies for all casts; EnumCaseIndex only for casts") {
    val (f, _, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(f.body.get))
    f.body.get.blocks += b1
    val lv = alloc(tI32, b0).resultVar.get

    val staticCast = CastExpr(annos = ArrayBuffer(NeedCheckCast(true)), op = CastOp.StaticCast, value0 = lv)
    staticCast.resultVar = Some(LocalVar(tpe = Some(ExprBuilder.refTy(tI32)), identifier = "%8",
      associatedExpr = Some(staticCast)))
    ChirOps.appendExpression(b0, staticCast)
    val sc = ExprBuilder.cloneExpr(staticCast, b1).asInstanceOf[CastExpr]
    assert(ExprBuilder.getAnno(sc, classOf[NeedCheckCast]).exists(_.need))

    // UnboxToValue clone: the wholesale anno copy (C++ CopyBaseInfoFrom, done by
    // genPatches/Block::Clone after Clone) carries orig annos incl. NeedCheckCast
    val unbox = CastExpr(annos = ArrayBuffer(NeedCheckCast(true)), op = CastOp.UnboxToValue, value0 = lv)
    unbox.resultVar = Some(LocalVar(tpe = Some(tI32), identifier = "%9", associatedExpr = Some(unbox)))
    ChirOps.appendExpression(b0, unbox)
    val ub = ExprBuilder.cloneExpr(unbox, b1).asInstanceOf[CastExpr]
    assert(ExprBuilder.getAnno(ub, classOf[NeedCheckCast]).exists(_.need))
  }

  test("cloneExpr of a Lambda mints a fresh identifier, params, body and retVal") {
    val (f, _, b0) = mkFunc()
    val b1 = Block(identifier = "1", parentGroup = Some(f.body.get))
    f.body.get.blocks += b1

    val orig = LambdaExpr(resultVar = Some(LocalVar(tpe = Some(funcType), identifier = "%10")),
      funcTy = funcType, isLocalFunc = true, identifier = "@T.f.lam.3", srcCodeName = "<closure>")
    val p = Parameter(tpe = Some(tI32), identifier = "%11", ownedLambda = Some(orig), srcCodeIdentifier = "x")
    orig.params += p
    val origBody = BlockGroup(identifier = "0", ownedExpression = Some(orig))
    val ob0 = Block(identifier = "0", parentGroup = Some(origBody))
    origBody.entryBlock = Some(ob0)
    origBody.blocks += ob0
    val origAlloc = alloc(tI32, ob0)
    orig.body = Some(origBody)
    orig.blockGroups += origBody
    orig.retVal = Some(origAlloc.resultVar.get)
    origAlloc.resultVar.get.isRetVal = true
    ChirOps.appendExpression(b0, orig)

    val clone = ExprBuilder.cloneExpr(orig, b1).asInstanceOf[LambdaExpr]
    assert(clone ne orig)
    assert(clone.identifier.startsWith("@T.f.lam.3."))
    assert(clone.params.size == 1)
    val np = clone.params.head
    assert(np ne p)
    assert((np.attributes & Attribute.Readonly) != 0L)
    assert(np.ownedLambda.contains(clone))
    // fresh body with cloned contents; operands shared
    val nb = clone.body.get
    assert(nb ne origBody)
    assert(nb.ownedExpression.contains(clone))
    assert(nb.entryBlock.isDefined)
    assert(nb.blocks.size == 1)
    val nAlloc = nb.blocks.head.exprs(0).asInstanceOf[AllocateExpr]
    assert(nAlloc.allocatedType eq origAlloc.allocatedType)
    // C++ Lambda::Clone does not set IsRetVal on the fresh result
    assert(!nAlloc.resultVar.get.isRetVal)
    assert(clone.retVal.contains(nAlloc.resultVar.get))
    assert(clone.blockGroups.head eq nb)
    // the original is untouched
    assert(orig.body.get eq origBody)
    assert(orig.params.head eq p)
  }

  test("factory newAllocate gives the result a Ref type and appends to the parent") {
    val (f, _, b0) = mkFunc()
    val e = ExprBuilder.newAllocate(f, tI32, b0)
    assert(b0.exprs.last eq e)
    assert(e.owner.contains(b0))
    val res = e.resultVar.get
    assert(res.tpe.get.isInstanceOf[TypeImpl])
    res.tpe.get.asInstanceOf[TypeImpl].kind match {
      case TypeKind.Ref => assert(res.tpe.get.asInstanceOf[TypeImpl].argTys.head eq tI32)
      case k            => fail(s"expected Ref type, got $k")
    }
    assert(res.associatedExpr.contains(e))
    assert(f.localId == 12L)
  }

  test("factory newStore/newBoolConstant/newTuple/newApply create well-formed exprs") {
    val (f, _, b0) = mkFunc()
    val lv = alloc(tI32, b0).resultVar.get

    val st = ExprBuilder.newStore(f, lv, lv, b0)
    assert(st.resultVar.get.tpe.get.asInstanceOf[TypeImpl].kind == TypeKind.Unit)
    assert(st.value eq lv)
    assert(st.location eq lv)
    assert(b0.exprs.last eq st)

    val bc = ExprBuilder.newBoolConstant(f, v = true, b0)
    assert(bc.literal.isInstanceOf[BoolLiteral] && bc.literal.asInstanceOf[BoolLiteral].value)
    assert(bc.resultVar.get.tpe.get.asInstanceOf[TypeImpl].kind == TypeKind.Boolean)

    val tup = ExprBuilder.newTuple(f, tI32, Seq(lv, lv), b0)
    assert(tup.elementValues.toSeq == Seq[Value](lv, lv))

    val callee = Func(identifier = "@T.g", srcCodeIdentifier = "g", rawMangledName = "@T.g",
      packageName = "T", localId = 1L, blockId = 1L, blockGroupId = 1L)
    val ap = ExprBuilder.newApply(f, tI32, callee, Seq(lv), parent = b0)
    assert(ap.callee eq callee)
    assert(ap.args.toSeq == Seq[Value](lv))
    assert(!ap.isTry && !ap.isSuperCall)
    assert(ap.resultVar.get.tpe.get.asInstanceOf[TypeImpl].kind == TypeKind.Int32)
    assert(b0.exprs.last eq ap)
  }

  test("getAnno/setAnno replace the annotation of the given class") {
    val (f, _, b0) = mkFunc()
    val e = alloc(tI32, b0)
    assert(ExprBuilder.getAnno(e, classOf[LinkTypeInfo]).isEmpty)

    ExprBuilder.setAnno(e, LinkTypeInfo(Linkage.WeakOdr))
    assert(ExprBuilder.getAnno(e, classOf[LinkTypeInfo]).exists(_.linkage == Linkage.WeakOdr))

    ExprBuilder.setAnno(e, LinkTypeInfo(Linkage.Internal))
    assert(e.annos.collect { case l: LinkTypeInfo => l }.size == 1)
    assert(ExprBuilder.getAnno(e, classOf[LinkTypeInfo]).exists(_.linkage == Linkage.Internal))
  }
}
