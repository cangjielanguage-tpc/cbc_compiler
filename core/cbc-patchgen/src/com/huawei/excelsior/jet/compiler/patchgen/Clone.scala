/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/** Expression cloning and creation, ported from the C++ per-type `Expression::Clone`
  * implementations and `CHIRBuilder::CreateExpression`/`CreateConstantExpression`.
  *
  * Faithful-port notes:
  *  - A cloned expression gets a FRESH result local var (identifier minted from the
  * top-level function's localId counter); operand references are shared with the
  * original. The fresh result merges the original result's attribute bits.
  *  - The cloned expression's own annos/loc are NOT set by Clone; callers copy them
  * via `copyBaseInfoFrom` (C++ `Base::CopyBaseInfoFrom` copies anno+loc only, not
  * attributes — a cloned expression loses its own attribute bits, like C++).
  *  - Cloning a `Constant` mints a fresh literal value only for NullLiteral; other
  * literals are shared.
  *  - Terminators without results (Branch/GoTo/Exit/MultiBranch/RaiseException) are
  * cloned without a result; Try* forms carry results and are cloned like the rest.
  *  - Cloning a Lambda mints a new identifier `<old>.<n>` (global counter), a fresh
  * body (blocks/expressions cloned recursively), fresh parameters, and sets the
  * fresh return value from the cloned body.
  */
object Clone {

  /** Global counter used for cloned lambda identifiers (C++ g_lambdaClonedIdx). */
  private var lambdaClonedIdx: Int = 0

  // =================================================================================
  // basic helpers
  // =================================================================================

  private def empty = ArrayBuffer[Annotation]()

  def boolTy: Type = TypeImpl(TypeKind.Boolean)
  def unitTy: Type = TypeImpl(TypeKind.Unit)
  def stringTy: Type = TypeImpl(TypeKind.CString)

  /** Port of `builder->GetType<RefType>(ty)`. */
  def refTy(base: Type): Type = TypeImpl(TypeKind.Ref, ArrayBuffer(base))

  /** Port of `CHIRBuilder::CreateExpression` + `parent.AppendExpression`. */
  private def finish(e: Expression, parent: Block): e.type = {
    e.owner = Some(parent)
    ChirOps.appendExpression(parent, e)
    e
  }

  /** Mint result + append + copy base info + merge result attrs — the common tail
  * of every C++ Clone impl.
    */
  private def finishCloned(e: Expression, orig: Expression, parent: Block): e.type = {
    val topLevel = ChirOps.topLevelFunc(parent.parentGroup.get)
      .getOrElse(throw new IllegalStateException("no top-level function for block"))
    // terminators without results (Goto/Branch/MultiBranch/Exit/RaiseException)
    // are cloned without minting (C++ asserts result == nullptr)
    for (res <- orig.resultVar) e.resultVar = Some(ChirOps.mintResult(topLevel, res.tpe.get, e))
    ChirOps.appendExpression(parent, e)
    copyBaseInfoFrom(e, orig)
    for (origRes <- orig.resultVar; res <- e.resultVar) res.attributes |= origRes.attributes
    e
  }

  /** Port of `Base::CopyBaseInfoFrom` (annos + loc, not attributes). */
  def copyBaseInfoFrom(dst: Expression, src: Expression): Unit = {
    dst.annos = ArrayBuffer(src.annos.toSeq*)
    dst.loc = src.loc
  }

  /** Reads an annotation of the given class from the model annos list
  * (port of `Base::Get<T>()`).
    */
  def getAnno[T <: Annotation : ClassTag](e: Expression): Option[T] =
    e.annos.collectFirst { case a: T => a }

  /** Port of `Base::Set<T>(anno)` — replaces the annotation of the given class. */
  def setAnno[T <: Annotation : ClassTag](e: Expression, anno: T): Unit = {
    e.annos --= e.annos.collect { case a: T => a }
    e.annos += anno
  }

  // =================================================================================
  // per-type clone (port of every C++ Expression::Clone)
  // =================================================================================

  def cloneExpr(e: Expression, parent: Block): Expression = e match {
    case e: GotoExpr => finishCloned(GotoExpr(destination0 = e.destination), e, parent)
    case _: ExitExpr => finishCloned(ExitExpr(), e, parent)
    case e: RaiseExceptionExpr =>
      val ne = RaiseExceptionExpr(exceptionValue0 = e.exceptionValue, exceptionBlock0 = e.exceptionBlock)
      finishCloned(ne, e, parent)
    case e: LoadExpr =>
      finishCloned(LoadExpr(location0 = e.location), e, parent)
    case e: StoreExpr =>
      finishCloned(StoreExpr(value0 = e.value, location0 = e.location), e, parent)
    case e: ConstantExpr =>
      // C++ Constant::Clone: NullLiteral gets a fresh literal value, others shared
      val lit = e.literal match {
        case n: NullLiteral => NullLiteral(tpe = n.tpe)
        case other          => other
      }
      finishCloned(ConstantExpr(literal0 = lit), e, parent)
    case e: TupleExpr =>
      finishCloned(TupleExpr(elementValues0 = ArrayBuffer.from(e.elementValues)), e, parent)
    case e: GetExceptionExpr =>
      finishCloned(GetExceptionExpr(), e, parent)
    case e: GetRTTIExpr =>
      finishCloned(GetRTTIExpr(obj0 = e.obj), e, parent)
    case e: GetRTTIStaticExpr =>
      finishCloned(GetRTTIStaticExpr(rttiType = e.rttiType), e, parent)
    case e: VArrayExpr =>
      finishCloned(VArrayExpr(elementValues0 = ArrayBuffer.from(e.elementValues)), e, parent)
    case e: VArrayBuilderExpr =>
      finishCloned(VArrayBuilderExpr(size0 = e.size, initValue0 = e.initValue, initializer0 = e.initializer), e, parent)
    case e: RawArrayLiteralInitExpr =>
      finishCloned(RawArrayLiteralInitExpr(array0 = e.array, elementValues0 = ArrayBuffer.from(e.elementValues)), e, parent)
    case e: RawArrayInitByValueExpr =>
      finishCloned(RawArrayInitByValueExpr(array0 = e.array, size0 = e.size, initValue0 = e.initValue), e, parent)
    case e: AllocateExpr =>
      val ne = AllocateExpr(allocatedType = e.allocatedType, isTry = e.isTry,
        handlerBlocks0 = ArrayBuffer.from(e.handlerBlocks))
      finishCloned(ne, e, parent)
    case e: UnaryExpr =>
      val ne = UnaryExpr(op = e.op, overflowStrategy = e.overflowStrategy, operand0 = e.operand,
        isTry = e.isTry, handlerBlocks0 = ArrayBuffer.from(e.handlerBlocks))
      finishCloned(ne, e, parent)
    case e: BinaryExpr =>
      val ne = BinaryExpr(op = e.op, overflowStrategy = e.overflowStrategy, leftOperand0 = e.leftOperand,
        rightOperand0 = e.rightOperand, isTry = e.isTry, handlerBlocks0 = ArrayBuffer.from(e.handlerBlocks))
      finishCloned(ne, e, parent)
    case e: NumericCastExpr =>
      val ne = NumericCastExpr(overflowStrategy = e.overflowStrategy, value0 = e.value, isTry = e.isTry,
        handlerBlocks0 = ArrayBuffer.from(e.handlerBlocks))
      finishCloned(ne, e, parent)
      // C++ NumericCast::Clone also copies NeedCheckCast / EnumCaseIndex
      getAnno[NeedCheckCast](e).foreach(a => ne.annos += NeedCheckCast(a.need))
      (e.resultVar, ne.resultVar) match {
        case (Some(o), Some(n)) =>
          o.annos.collectFirst { case a: EnumCaseIndex => n.annos += EnumCaseIndex(a.index) }
        case _ => ()
      }
      ne
    case e: CastExpr =>
      val ne = CastExpr(op = e.op, value0 = e.value)
      finishCloned(ne, e, parent)
      if (e.op == CastOp.StaticCast) {
        // C++ ClassStaticCast::Clone also copies NeedCheckCast / EnumCaseIndex
        getAnno[NeedCheckCast](e).foreach(a => ne.annos += NeedCheckCast(a.need))
        (e.resultVar, ne.resultVar) match {
          case (Some(o), Some(n)) =>
            o.annos.collectFirst { case a: EnumCaseIndex => n.annos += EnumCaseIndex(a.index) }
          case _ => ()
        }
      }
      ne
    case e: ApplyExpr =>
      val ne = ApplyExpr(instantiatedTypeArgs = ArrayBuffer.from(e.instantiatedTypeArgs), objType = e.objType,
        callee0 = e.callee, args0 = ArrayBuffer.from(e.args), isSuperCall = e.isSuperCall, isTry = e.isTry,
        handlerBlocks0 = ArrayBuffer.from(e.handlerBlocks))
      finishCloned(ne, e, parent)
    case e: InvokeExpr =>
      val ne = InvokeExpr(instantiatedTypeArgs = ArrayBuffer.from(e.instantiatedTypeArgs), objType = e.objType,
        callee0 = e.callee, caller0 = e.caller, args0 = ArrayBuffer.from(e.args),
        overflowStrategy = e.overflowStrategy, isTry = e.isTry,
        handlerBlocks0 = ArrayBuffer.from(e.handlerBlocks))
      finishCloned(ne, e, parent)
    case e: IntrinsicExpr =>
      val ne = IntrinsicExpr(instantiatedTypeArgs = ArrayBuffer.from(e.instantiatedTypeArgs), objType = e.objType,
        intrinsicKind = e.intrinsicKind, args0 = ArrayBuffer.from(e.args), isTry = e.isTry,
        handlerBlocks0 = ArrayBuffer.from(e.handlerBlocks))
      finishCloned(ne, e, parent)
    case e: RawArrayAllocateExpr =>
      val ne = RawArrayAllocateExpr(elementType = e.elementType, size0 = e.size, isTry = e.isTry,
        handlerBlocks0 = ArrayBuffer.from(e.handlerBlocks))
      finishCloned(ne, e, parent)
    case e: SpawnExpr =>
      val ne = SpawnExpr(obj0 = e.obj, spawnArg0 = e.spawnArg, executeClosure = e.executeClosure,
        isTry = e.isTry, handlerBlocks0 = ArrayBuffer.from(e.handlerBlocks))
      finishCloned(ne, e, parent)
    case e: BranchExpr =>
      val ne = BranchExpr(condition0 = e.condition, trueBlock0 = e.trueBlock,
        falseBlock0 = e.falseBlock, sourceExpr = e.sourceExpr)
      finishCloned(ne, e, parent)
      ne.resultVar = None // Branch has no result (C++ asserts result == nullptr)
      ne
    case e: MultiBranchExpr =>
      val ne = MultiBranchExpr(condition0 = e.condition, caseValues = ArrayBuffer.from(e.caseValues),
        defaultBlock0 = e.defaultBlock, normalBlocks0 = ArrayBuffer.from(e.normalBlocks))
      finishCloned(ne, e, parent)
      ne.resultVar = None
      ne
    case e: GetElementRefExpr =>
      finishCloned(GetElementRefExpr(base0 = e.base, path = ArrayBuffer.from(e.path)), e, parent)
    case e: StoreElementRefExpr =>
      finishCloned(StoreElementRefExpr(value0 = e.value, location0 = e.location, path = ArrayBuffer.from(e.path)), e, parent)
    case e: GetElementByNameExpr =>
      finishCloned(GetElementByNameExpr(base0 = e.base, fieldNames = ArrayBuffer.from(e.fieldNames)), e, parent)
    case e: StoreElementByNameExpr =>
      finishCloned(StoreElementByNameExpr(value0 = e.value, location0 = e.location, fieldNames = ArrayBuffer.from(e.fieldNames)), e, parent)
    case e: FieldExpr =>
      finishCloned(FieldExpr(base0 = e.base, path = ArrayBuffer.from(e.path)), e, parent)
    case e: FieldByNameExpr =>
      finishCloned(FieldByNameExpr(base0 = e.base, fieldNames = ArrayBuffer.from(e.fieldNames)), e, parent)
    case e: InstanceOfExpr =>
      finishCloned(InstanceOfExpr(obj0 = e.obj, targetType = e.targetType), e, parent)
    case e: GetInstantiateValueExpr =>
      finishCloned(GetInstantiateValueExpr(instVal0 = e.instVal, instantiateTypes = ArrayBuffer.from(e.instantiateTypes)), e, parent)
    case e: DebugExpr =>
      finishCloned(DebugExpr(srcCodeName = e.srcCodeName, value0 = e.value), e, parent)
    case e: LambdaExpr =>
      cloneLambda(e, parent)
  }
  /** Port of `Lambda::Clone` + `BlockGroup::Clone` + `Block::Clone`. */
  private def cloneLambda(x: LambdaExpr, parent: Block): LambdaExpr = {
    val topLevel = ChirOps.topLevelFunc(parent.parentGroup.get)
      .getOrElse(throw new IllegalStateException("no top-level function for block"))
    val newIdentifier = x.identifier + "." + { lambdaClonedIdx += 1; lambdaClonedIdx - 1 }
    val ne = LambdaExpr(funcTy = x.funcTy, isLocalFunc = x.isLocalFunc, identifier = newIdentifier,
      srcCodeName = x.srcCodeName, genericTypeParams = ArrayBuffer.from(x.genericTypeParams),
      isCompileTimeValue = x.isCompileTimeValue)
    ne.resultVar = Some(ChirOps.mintResult(topLevel, x.resultVar.get.tpe.get, ne))
    ChirOps.appendExpression(parent, ne)
    copyBaseInfoFrom(ne, x)
    ne.resultVar.foreach(_.attributes |= x.resultVar.get.attributes)

    // fresh body
    val newBody = cloneBlockGroup(x.body.get, ne, topLevel)
    ne.body = Some(newBody)
    ne.blockGroups += newBody

    // fresh params (READONLY attr, like CHIRBuilder::CreateParameter)
    for (p <- x.params) {
      val id = topLevel.generateLocalId()
      val np = Parameter(attributes = Attribute.Readonly, tpe = p.tpe, identifier = "%" + id,
        ownedLambda = Some(ne), srcCodeIdentifier = p.srcCodeIdentifier, annoInfo = p.annoInfo)
      np.id = id.toInt
      ne.params += np
    }
    // fresh return value: C++ finds the orig body's ret alloc by index and takes the
    // same position in the new body (the clone's result is fresh, no IsRetVal flag set)
    getAllocExprOfRetVal(x.body.get).foreach { (bi, ei) =>
      ne.retVal = Some(newBody.blocks(bi).exprs(ei).resultVar.get)
    }
    ne
  }

  /** Port of `BlockGroup::Clone(builder, Lambda&)`. */
  private def cloneBlockGroup(orig: BlockGroup, lambda: LambdaExpr, topLevel: Func): BlockGroup = {
    val id = topLevel.generateBlockGroupId()
    val bg = BlockGroup(identifier = id.toString, ownedExpression = Some(lambda))
    bg.id = id.toInt
    bg.attributes |= orig.attributes
    // CloneBlocks
    for (block <- orig.blocks) {
      val nb = cloneBlock(block, bg, topLevel)
      if (orig.entryBlock.exists(_ eq block)) bg.entryBlock = Some(nb)
    }
    bg
  }

  /** Port of `Block::Clone` (C++ `CreateBlock` adds the block to the group). */
  private def cloneBlock(orig: Block, newGroup: BlockGroup, topLevel: Func): Block = {
    val id = topLevel.generateBlockId()
    val nb = Block(identifier = "#" + id, parentGroup = Some(newGroup),
      isLandingPadBlock = orig.isLandingPadBlock, exceptionCatchList = ArrayBuffer.from(orig.exceptionCatchList))
    newGroup.blocks += nb
    nb.id = id.toInt
    nb.attributes |= orig.attributes
    for (expr <- orig.exprs.toList) {
      val ne = cloneExpr(expr, nb)
    }
    nb
  }

  /** Port of `GetAllocExprOfRetVal` (returns block/expr indexes, like C++). */
  private def getAllocExprOfRetVal(body: BlockGroup): Option[(Int, Int)] = {
    for (bi <- 0 until body.blocks.size; ei <- 0 until body.blocks(bi).exprs.size) {
      if (body.blocks(bi).exprs(ei).resultVar.exists(_.isRetVal)) {
        return Some((bi, ei))
      }
    }
    None
  }

  // =================================================================================
  // expression factories (port of CHIRBuilder::CreateExpression / CreateConstantExpression)
  // =================================================================================

  private def minted(topLevel: Func, resultTy: Type, e: Expression, parent: Block): Expression = {
    e.resultVar = Some(ChirOps.mintResult(topLevel, resultTy, e))
    ChirOps.appendExpression(parent, e)
    e
  }

  /** Allocate(resultTy = RefType(allocatedTy), allocatedTy) — C++ `Allocate(ty, allocatedTy, parent)`. */
  def newAllocate(topLevel: Func, allocatedTy: Type, parent: Block): AllocateExpr = {
    val e = AllocateExpr(allocatedType = allocatedTy)
    minted(topLevel, refTy(allocatedTy), e, parent).asInstanceOf[AllocateExpr]
  }

  /** C++ `CreateExpression<ClassStaticCast>(RefType(ty), operand, parent)`. */
  def newClassStaticCast(topLevel: Func, baseTy: Type, operand: Value, parent: Block): CastExpr = {
    val e = CastExpr(op = CastOp.StaticCast, value0 = operand)
    minted(topLevel, refTy(baseTy), e, parent).asInstanceOf[CastExpr]
  }

  /** C++ `CreateConstantExpression<BoolLiteral>(boolTy, parent, v)`. */
  def newBoolConstant(topLevel: Func, v: Boolean, parent: Block): ConstantExpr = {
    val lit = BoolLiteral(tpe = Some(boolTy), value = v)
    val e = ConstantExpr(literal0 = lit)
    minted(topLevel, boolTy, e, parent).asInstanceOf[ConstantExpr]
  }

  /** C++ `CreateExpression<Tuple>(ty, values, parent)`. */
  def newTuple(topLevel: Func, resultTy: Type, values: Seq[Value], parent: Block): TupleExpr = {
    val e = TupleExpr(elementValues0 = ArrayBuffer.from(values))
    minted(topLevel, resultTy, e, parent).asInstanceOf[TupleExpr]
  }

  /** C++ `CreateExpression<Store>(unitTy, value, location, parent)`. */
  def newStore(topLevel: Func, value: Value, location: Value, parent: Block): StoreExpr = {
    val e = StoreExpr(value0 = value, location0 = location)
    minted(topLevel, unitTy, e, parent).asInstanceOf[StoreExpr]
  }

  /** C++ `CreateExpression<Apply>(retTy, callee, FuncCallContext{...}, parent)`. */
  def newApply(topLevel: Func, resultTy: Type, callee: Value, args: Seq[Value],
               instTypeArgs: ArrayBuffer[Type] = ArrayBuffer(), thisType: Option[Type] = None,
               parent: Block): ApplyExpr = {
    val e = ApplyExpr(instantiatedTypeArgs = instTypeArgs, objType = thisType, callee0 = callee,
      args0 = ArrayBuffer.from(args))
    minted(topLevel, resultTy, e, parent).asInstanceOf[ApplyExpr]
  }
}
