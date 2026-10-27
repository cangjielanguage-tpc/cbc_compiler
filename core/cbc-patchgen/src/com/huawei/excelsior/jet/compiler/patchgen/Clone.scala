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
object ExprBuilder {

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
    e match {
      case e: ExprBase => e.owner_=(Some(parent))
    }
    ChirOps.appendExpression(parent, e)
    e
  }

  /** Mint result + append + copy base info + merge result attrs — the common tail
    * of every C++ Clone impl.
    */
  private def finishCloned(e: Expression, orig: Expression, parent: Block): e.type = {
    val topLevel = ChirOps.topLevelFunc(parent.parentGroup.get)
      .getOrElse(throw new IllegalStateException("no top-level function for block"))
    e match {
      case eb: ExprBase =>
        eb.resultVar = Some(ChirOps.mintResult(topLevel, orig.resultVar.get.tpe.get, e))
    }
    ChirOps.appendExpression(parent, e)
    copyBaseInfoFrom(e, orig)
    orig.resultVar.foreach { origRes =>
      e.resultVar.foreach { res =>
        res.attributes |= origRes.attributes
      }
    }
    e
  }

  /** Port of `Base::CopyBaseInfoFrom` (annos + loc, not attributes). */
  def copyBaseInfoFrom(dst: Expression, src: Expression): Unit =
    dst match {
      case dst: ExprBase =>
        dst.annos = ArrayBuffer(src.annos.toSeq*)
        dst.loc = src.loc
    }
  /** Reads an annotation of the given class from the model annos list
    * (port of `Base::Get<T>()`).
    */
  def getAnno[T <: Annotation](e: Expression, cls: Class[T]): Option[T] =
    e.annos.collectFirst { case a if cls.isInstance(a) => a.asInstanceOf[T] }

  /** Port of `Base::Set<T>(anno)` — replaces the annotation of the given class. */
  def setAnno[T <: Annotation](e: Expression, anno: T)(using reflectable: scala.reflect.ClassTag[T]): Unit = {
    e.annos --= e.annos.collect { case a: T => a }
    e.annos += anno
  }

  // =================================================================================
  // per-type clone (port of every C++ Expression::Clone)
  // =================================================================================

  def cloneExpr(e: Expression, parent: Block): Expression = e match {
    case x: GotoExpr => finishCloned(GotoExpr(empty, None, 0L, None, ArrayBuffer(), None, x.destination), x, parent)
    case _: ExitExpr => finishCloned(ExitExpr(empty, None, 0L, None, ArrayBuffer(), None), e, parent)
    case x: RaiseExceptionExpr =>
      val ne = RaiseExceptionExpr(empty, None, 0L, None, ArrayBuffer(), None, x.exceptionValue, x.exceptionBlock)
      finishCloned(ne, x, parent)
    case x: LoadExpr =>
      finishCloned(LoadExpr(empty, None, 0L, None, ArrayBuffer(), None, x.location), x, parent)
    case x: StoreExpr =>
      finishCloned(StoreExpr(empty, None, 0L, None, ArrayBuffer(), None, x.value, x.location), x, parent)
    case x: ConstantExpr =>
      // C++ Constant::Clone: NullLiteral gets a fresh literal value, others shared
      val lit = x.literal match {
        case n: NullLiteral => NullLiteral(ArrayBuffer(), None, 0L, n.tpe, "")
        case other          => other
      }
      finishCloned(ConstantExpr(empty, None, 0L, None, ArrayBuffer(), None, lit), x, parent)
    case x: TupleExpr =>
      finishCloned(TupleExpr(empty, None, 0L, None, ArrayBuffer(), None, ArrayBuffer(x.elementValues.toSeq*)), x, parent)
    case x: GetExceptionExpr =>
      finishCloned(GetExceptionExpr(empty, None, 0L, None, ArrayBuffer(), None), x, parent)
    case x: GetRTTIExpr =>
      finishCloned(GetRTTIExpr(empty, None, 0L, None, ArrayBuffer(), None, x.obj), x, parent)
    case x: GetRTTIStaticExpr =>
      finishCloned(GetRTTIStaticExpr(empty, None, 0L, None, ArrayBuffer(), None, x.rttiType), x, parent)
    case x: VArrayExpr =>
      finishCloned(VArrayExpr(empty, None, 0L, None, ArrayBuffer(), None, ArrayBuffer(x.elementValues.toSeq*)), x, parent)
    case x: VArrayBuilderExpr =>
      finishCloned(VArrayBuilderExpr(empty, None, 0L, None, ArrayBuffer(), None, x.size, x.initValue, x.initializer), x, parent)
    case x: RawArrayLiteralInitExpr =>
      finishCloned(RawArrayLiteralInitExpr(empty, None, 0L, None, ArrayBuffer(), None, x.array, ArrayBuffer(x.elementValues.toSeq*)), x, parent)
    case x: RawArrayInitByValueExpr =>
      finishCloned(RawArrayInitByValueExpr(empty, None, 0L, None, ArrayBuffer(), None, x.array, x.size, x.initValue), x, parent)
    case x: AllocateExpr =>
      val ne = AllocateExpr(empty, None, 0L, None, ArrayBuffer(), None, x.allocatedType, x.isTry, ArrayBuffer(x.handlerBlocks.toSeq*))
      finishCloned(ne, x, parent)
    case x: UnaryExpr =>
      val ne = UnaryExpr(empty, None, 0L, None, ArrayBuffer(), None, x.op, x.overflowStrategy, x.operand, x.isTry, ArrayBuffer(x.handlerBlocks.toSeq*))
      finishCloned(ne, x, parent)
    case x: BinaryExpr =>
      val ne = BinaryExpr(empty, None, 0L, None, ArrayBuffer(), None, x.op, x.overflowStrategy, x.leftOperand, x.rightOperand, x.isTry, ArrayBuffer(x.handlerBlocks.toSeq*))
      finishCloned(ne, x, parent)
    case x: NumericCastExpr =>
      val ne = NumericCastExpr(empty, None, 0L, None, ArrayBuffer(), None, x.overflowStrategy, x.value, x.isTry, ArrayBuffer(x.handlerBlocks.toSeq*))
      finishCloned(ne, x, parent)
      // C++ NumericCast::Clone also copies NeedCheckCast / EnumCaseIndex
      getAnno(x, classOf[NeedCheckCast]).foreach(a => ne.annos += NeedCheckCast(a.need))
      (x.resultVar, ne.resultVar) match {
        case (Some(o), Some(n)) =>
          o.annos.collectFirst { case a: EnumCaseIndex => n.annos += EnumCaseIndex(a.index) }
        case _ => ()
      }
      ne
    case x: CastExpr =>
      val ne = CastExpr(empty, None, 0L, None, ArrayBuffer(), None, x.op, x.value)
      finishCloned(ne, x, parent)
      if (x.op == CastOp.StaticCast) {
        // C++ ClassStaticCast::Clone also copies NeedCheckCast / EnumCaseIndex
        getAnno(x, classOf[NeedCheckCast]).foreach(a => ne.annos += NeedCheckCast(a.need))
        (x.resultVar, ne.resultVar) match {
          case (Some(o), Some(n)) =>
            o.annos.collectFirst { case a: EnumCaseIndex => n.annos += EnumCaseIndex(a.index) }
          case _ => ()
        }
      }
      ne
    case x: ApplyExpr =>
      val ne = ApplyExpr(empty, None, 0L, None, ArrayBuffer(), None, ArrayBuffer(x.instantiatedTypeArgs.toSeq*), x.objType, x.callee, ArrayBuffer(x.args.toSeq*), x.isSuperCall, x.isTry, ArrayBuffer(x.handlerBlocks.toSeq*))
      finishCloned(ne, x, parent)
    case x: InvokeExpr =>
      val ne = InvokeExpr(empty, None, 0L, None, ArrayBuffer(), None, ArrayBuffer(x.instantiatedTypeArgs.toSeq*), x.objType, x.callee, x.caller, ArrayBuffer(x.args.toSeq*), x.overflowStrategy, x.isTry, ArrayBuffer(x.handlerBlocks.toSeq*))
      finishCloned(ne, x, parent)
    case x: IntrinsicExpr =>
      val ne = IntrinsicExpr(empty, None, 0L, None, ArrayBuffer(), None, ArrayBuffer(x.instantiatedTypeArgs.toSeq*), x.objType, x.intrinsicKind, ArrayBuffer(x.args.toSeq*), x.isTry, ArrayBuffer(x.handlerBlocks.toSeq*))
      finishCloned(ne, x, parent)
    case x: RawArrayAllocateExpr =>
      val ne = RawArrayAllocateExpr(empty, None, 0L, None, ArrayBuffer(), None, x.elementType, x.size, x.isTry, ArrayBuffer(x.handlerBlocks.toSeq*))
      finishCloned(ne, x, parent)
    case x: SpawnExpr =>
      val ne = SpawnExpr(empty, None, 0L, None, ArrayBuffer(), None, x.obj, x.spawnArg, x.executeClosure, x.isTry, ArrayBuffer(x.handlerBlocks.toSeq*))
      finishCloned(ne, x, parent)
    case x: BranchExpr =>
      val ne = BranchExpr(empty, None, 0L, None, ArrayBuffer(), None, x.condition, x.trueBlock, x.falseBlock, x.sourceExpr)
      finishCloned(ne, x, parent)
      ne.resultVar = None // Branch has no result (C++ asserts result == nullptr)
      ne
    case x: MultiBranchExpr =>
      val ne = MultiBranchExpr(empty, None, 0L, None, ArrayBuffer(), None, x.condition, ArrayBuffer(x.caseValues.toSeq*), x.defaultBlock, ArrayBuffer(x.normalBlocks.toSeq*))
      finishCloned(ne, x, parent)
      ne.resultVar = None
      ne
    case x: GetElementRefExpr =>
      finishCloned(GetElementRefExpr(empty, None, 0L, None, ArrayBuffer(), None, x.base, ArrayBuffer(x.path.toSeq*)), x, parent)
    case x: StoreElementRefExpr =>
      finishCloned(StoreElementRefExpr(empty, None, 0L, None, ArrayBuffer(), None, x.value, x.location, ArrayBuffer(x.path.toSeq*)), x, parent)
    case x: GetElementByNameExpr =>
      finishCloned(GetElementByNameExpr(empty, None, 0L, None, ArrayBuffer(), None, x.base, ArrayBuffer(x.fieldNames.toSeq*)), x, parent)
    case x: StoreElementByNameExpr =>
      finishCloned(StoreElementByNameExpr(empty, None, 0L, None, ArrayBuffer(), None, x.value, x.location, ArrayBuffer(x.fieldNames.toSeq*)), x, parent)
    case x: FieldExpr =>
      finishCloned(FieldExpr(empty, None, 0L, None, ArrayBuffer(), None, x.base, ArrayBuffer(x.path.toSeq*)), x, parent)
    case x: FieldByNameExpr =>
      finishCloned(FieldByNameExpr(empty, None, 0L, None, ArrayBuffer(), None, x.base, ArrayBuffer(x.fieldNames.toSeq*)), x, parent)
    case x: InstanceOfExpr =>
      finishCloned(InstanceOfExpr(empty, None, 0L, None, ArrayBuffer(), None, x.obj, x.targetType), x, parent)
    case x: GetInstantiateValueExpr =>
      finishCloned(GetInstantiateValueExpr(empty, None, 0L, None, ArrayBuffer(), None, x.instVal, ArrayBuffer(x.instantiateTypes.toSeq*)), x, parent)
    case x: DebugExpr =>
      finishCloned(DebugExpr(empty, None, 0L, None, ArrayBuffer(), None, x.srcCodeName, x.value), x, parent)
    case x: LambdaExpr =>
      cloneLambda(x, parent)

  }
  /** Port of `Lambda::Clone` + `BlockGroup::Clone` + `Block::Clone`. */
  private def cloneLambda(x: LambdaExpr, parent: Block): LambdaExpr = {
    val topLevel = ChirOps.topLevelFunc(parent.parentGroup.get)
      .getOrElse(throw new IllegalStateException("no top-level function for block"))
    val newIdentifier = x.identifier + "." + { lambdaClonedIdx += 1; lambdaClonedIdx - 1 }
    val ne = LambdaExpr(empty, None, 0L, None, ArrayBuffer(), None,
      x.funcTy, x.isLocalFunc, newIdentifier, x.srcCodeName,
      ArrayBuffer(), ArrayBuffer(x.genericTypeParams.toSeq*), None, None, x.isCompileTimeValue)
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
      val np = Parameter(ArrayBuffer(), None, Attribute.Readonly, p.tpe, "%" + id, None, Some(ne), p.srcCodeIdentifier, p.annoInfo.map(a => a))
      np.id = id.toInt
      ne.params += np

    }
    // fresh return value: the result of the alloc expr of the ret in the new body
    getAllocExprOfRetVal(newBody).foreach { res =>
      ne.retVal = Some(res)
      res.isRetVal = true
    }
    ne
  }

  /** Port of `BlockGroup::Clone(builder, Lambda&)`. */
  private def cloneBlockGroup(orig: BlockGroup, lambda: LambdaExpr, topLevel: Func): BlockGroup = {
    val id = topLevel.generateBlockGroupId()
    val bg = BlockGroup(ArrayBuffer(), None, 0L, None, id.toString, None, ArrayBuffer(), None, Some(lambda))
    bg.id = id.toInt
    bg.attributes |= orig.attributes
    // CloneBlocks
    for (block <- orig.blocks) {
      val nb = cloneBlock(block, bg, topLevel)
      if (orig.entryBlock.exists(_ eq block)) bg.entryBlock = Some(nb)
    }
    bg
  }

  /** Port of `Block::Clone`. */
  private def cloneBlock(orig: Block, newGroup: BlockGroup, topLevel: Func): Block = {
    val id = topLevel.generateBlockId()
    val nb = Block(ArrayBuffer(), None, 0L, None, "#" + id, Some(newGroup), ArrayBuffer(), ArrayBuffer(), orig.isLandingPadBlock, ArrayBuffer(orig.exceptionCatchList.toSeq*))
    nb.id = id.toInt
    nb.attributes |= orig.attributes
    for (expr <- orig.exprs.toList) {
      val ne = cloneExpr(expr, nb)
    }
    nb
  }

  /** Port of `GetAllocExprOfRetVal`. */
  private def getAllocExprOfRetVal(body: BlockGroup): Option[LocalVar] = {
    for (block <- body.blocks; expr <- block.exprs; res <- expr.resultVar if res.isRetVal) {
      return Some(res)
    }
    None
  }

  // =================================================================================
  // expression factories (port of CHIRBuilder::CreateExpression / CreateConstantExpression)
  // =================================================================================

  private def minted(topLevel: Func, resultTy: Type, e: Expression): Expression = {
    e match {
      case e: ExprBase =>
        e.resultVar = Some(ChirOps.mintResult(topLevel, resultTy, e))
    }
    e
  }

  /** Allocate(resultTy = RefType(allocatedTy), allocatedTy) — C++ `Allocate(ty, allocatedTy, parent)`. */
  def newAllocate(topLevel: Func, allocatedTy: Type, parent: Block): AllocateExpr = {
    val e = AllocateExpr(empty, None, 0L, None, ArrayBuffer(), None, allocatedTy, isTry = false, ArrayBuffer())
    minted(topLevel, refTy(allocatedTy), e).asInstanceOf[AllocateExpr]
  }

  /** C++ `CreateExpression<ClassStaticCast>(RefType(ty), operand, parent)`. */
  def newClassStaticCast(topLevel: Func, baseTy: Type, operand: Value, parent: Block): CastExpr = {
    val e = CastExpr(empty, None, 0L, None, ArrayBuffer(), None, CastOp.StaticCast, operand)
    minted(topLevel, refTy(baseTy), e).asInstanceOf[CastExpr]
  }

  /** C++ `CreateConstantExpression<BoolLiteral>(boolTy, parent, v)`. */
  def newBoolConstant(topLevel: Func, v: Boolean, parent: Block): ConstantExpr = {
    val lit = BoolLiteral(ArrayBuffer(), None, 0L, Some(boolTy), "", v)
    val e = ConstantExpr(empty, None, 0L, None, ArrayBuffer(), None, lit)
    minted(topLevel, boolTy, e).asInstanceOf[ConstantExpr]
  }

  /** C++ `CreateExpression<Tuple>(ty, values, parent)`. */
  def newTuple(topLevel: Func, resultTy: Type, values: Seq[Value], parent: Block): TupleExpr = {
    val e = TupleExpr(empty, None, 0L, None, ArrayBuffer(), None, ArrayBuffer(values*))
    minted(topLevel, resultTy, e).asInstanceOf[TupleExpr]
  }

  /** C++ `CreateExpression<Store>(unitTy, value, location, parent)`. */
  def newStore(topLevel: Func, value: Value, location: Value, parent: Block): StoreExpr = {
    val e = StoreExpr(empty, None, 0L, None, ArrayBuffer(), None, value, location)
    minted(topLevel, unitTy, e).asInstanceOf[StoreExpr]
  }

  /** C++ `CreateExpression<Apply>(retTy, callee, FuncCallContext{...}, parent)`. */
  def newApply(topLevel: Func, resultTy: Type, callee: Value, args: Seq[Value],
               instTypeArgs: ArrayBuffer[Type] = ArrayBuffer(), thisType: Option[Type] = None,
               parent: Block): ApplyExpr =
    val e = ApplyExpr(empty, None, 0L, None, ArrayBuffer(), None, instTypeArgs, thisType, callee, ArrayBuffer(args*), isSuperCall = false, isTry = false, ArrayBuffer())
    minted(topLevel, resultTy, e).asInstanceOf[ApplyExpr]
}
