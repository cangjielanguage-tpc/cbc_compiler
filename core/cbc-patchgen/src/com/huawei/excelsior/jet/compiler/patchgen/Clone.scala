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
        // terminators without results (Goto/Branch/MultiBranch/Exit/RaiseException)
        // are cloned without minting (C++ asserts result == nullptr)
        orig.resultVar.foreach { res =>
          eb.resultVar = Some(ChirOps.mintResult(topLevel, res.tpe.get, e))
        }
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
    case x: GotoExpr => finishCloned(GotoExpr(destination0 = x.destination), x, parent)
    case _: ExitExpr => finishCloned(ExitExpr(), e, parent)
    case x: RaiseExceptionExpr =>
      val ne = RaiseExceptionExpr(exceptionValue0 = x.exceptionValue, exceptionBlock0 = x.exceptionBlock)
      finishCloned(ne, x, parent)
    case x: LoadExpr =>
      finishCloned(LoadExpr(location0 = x.location), x, parent)
    case x: StoreExpr =>
      finishCloned(StoreExpr(value0 = x.value, location0 = x.location), x, parent)
    case x: ConstantExpr =>
      // C++ Constant::Clone: NullLiteral gets a fresh literal value, others shared
      val lit = x.literal match {
        case n: NullLiteral => NullLiteral(tpe = n.tpe)
        case other          => other
      }
      finishCloned(ConstantExpr(literal0 = lit), x, parent)
    case x: TupleExpr =>
      finishCloned(TupleExpr(elementValues0 = ArrayBuffer.from(x.elementValues)), x, parent)
    case x: GetExceptionExpr =>
      finishCloned(GetExceptionExpr(), x, parent)
    case x: GetRTTIExpr =>
      finishCloned(GetRTTIExpr(obj0 = x.obj), x, parent)
    case x: GetRTTIStaticExpr =>
      finishCloned(GetRTTIStaticExpr(rttiType = x.rttiType), x, parent)
    case x: VArrayExpr =>
      finishCloned(VArrayExpr(elementValues0 = ArrayBuffer.from(x.elementValues)), x, parent)
    case x: VArrayBuilderExpr =>
      finishCloned(VArrayBuilderExpr(size0 = x.size, initValue0 = x.initValue, initializer0 = x.initializer), x, parent)
    case x: RawArrayLiteralInitExpr =>
      finishCloned(RawArrayLiteralInitExpr(array0 = x.array, elementValues0 = ArrayBuffer.from(x.elementValues)), x, parent)
    case x: RawArrayInitByValueExpr =>
      finishCloned(RawArrayInitByValueExpr(array0 = x.array, size0 = x.size, initValue0 = x.initValue), x, parent)
    case x: AllocateExpr =>
      val ne = AllocateExpr(allocatedType = x.allocatedType, isTry = x.isTry,
        handlerBlocks0 = ArrayBuffer.from(x.handlerBlocks))
      finishCloned(ne, x, parent)
    case x: UnaryExpr =>
      val ne = UnaryExpr(op = x.op, overflowStrategy = x.overflowStrategy, operand0 = x.operand,
        isTry = x.isTry, handlerBlocks0 = ArrayBuffer.from(x.handlerBlocks))
      finishCloned(ne, x, parent)
    case x: BinaryExpr =>
      val ne = BinaryExpr(op = x.op, overflowStrategy = x.overflowStrategy, leftOperand0 = x.leftOperand,
        rightOperand0 = x.rightOperand, isTry = x.isTry, handlerBlocks0 = ArrayBuffer.from(x.handlerBlocks))
      finishCloned(ne, x, parent)
    case x: NumericCastExpr =>
      val ne = NumericCastExpr(overflowStrategy = x.overflowStrategy, value0 = x.value, isTry = x.isTry,
        handlerBlocks0 = ArrayBuffer.from(x.handlerBlocks))
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
      val ne = CastExpr(op = x.op, value0 = x.value)
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
      val ne = ApplyExpr(instantiatedTypeArgs = ArrayBuffer.from(x.instantiatedTypeArgs), objType = x.objType,
        callee0 = x.callee, args0 = ArrayBuffer.from(x.args), isSuperCall = x.isSuperCall, isTry = x.isTry,
        handlerBlocks0 = ArrayBuffer.from(x.handlerBlocks))
      finishCloned(ne, x, parent)
    case x: InvokeExpr =>
      val ne = InvokeExpr(instantiatedTypeArgs = ArrayBuffer.from(x.instantiatedTypeArgs), objType = x.objType,
        callee0 = x.callee, caller0 = x.caller, args0 = ArrayBuffer.from(x.args),
        overflowStrategy = x.overflowStrategy, isTry = x.isTry,
        handlerBlocks0 = ArrayBuffer.from(x.handlerBlocks))
      finishCloned(ne, x, parent)
    case x: IntrinsicExpr =>
      val ne = IntrinsicExpr(instantiatedTypeArgs = ArrayBuffer.from(x.instantiatedTypeArgs), objType = x.objType,
        intrinsicKind = x.intrinsicKind, args0 = ArrayBuffer.from(x.args), isTry = x.isTry,
        handlerBlocks0 = ArrayBuffer.from(x.handlerBlocks))
      finishCloned(ne, x, parent)
    case x: RawArrayAllocateExpr =>
      val ne = RawArrayAllocateExpr(elementType = x.elementType, size0 = x.size, isTry = x.isTry,
        handlerBlocks0 = ArrayBuffer.from(x.handlerBlocks))
      finishCloned(ne, x, parent)
    case x: SpawnExpr =>
      val ne = SpawnExpr(obj0 = x.obj, spawnArg0 = x.spawnArg, executeClosure = x.executeClosure,
        isTry = x.isTry, handlerBlocks0 = ArrayBuffer.from(x.handlerBlocks))
      finishCloned(ne, x, parent)
    case x: BranchExpr =>
      val ne = BranchExpr(condition0 = x.condition, trueBlock0 = x.trueBlock,
        falseBlock0 = x.falseBlock, sourceExpr = x.sourceExpr)
      finishCloned(ne, x, parent)
      ne.resultVar = None // Branch has no result (C++ asserts result == nullptr)
      ne
    case x: MultiBranchExpr =>
      val ne = MultiBranchExpr(condition0 = x.condition, caseValues = ArrayBuffer.from(x.caseValues),
        defaultBlock0 = x.defaultBlock, normalBlocks0 = ArrayBuffer.from(x.normalBlocks))
      finishCloned(ne, x, parent)
      ne.resultVar = None
      ne
    case x: GetElementRefExpr =>
      finishCloned(GetElementRefExpr(base0 = x.base, path = ArrayBuffer.from(x.path)), x, parent)
    case x: StoreElementRefExpr =>
      finishCloned(StoreElementRefExpr(value0 = x.value, location0 = x.location, path = ArrayBuffer.from(x.path)), x, parent)
    case x: GetElementByNameExpr =>
      finishCloned(GetElementByNameExpr(base0 = x.base, fieldNames = ArrayBuffer.from(x.fieldNames)), x, parent)
    case x: StoreElementByNameExpr =>
      finishCloned(StoreElementByNameExpr(value0 = x.value, location0 = x.location, fieldNames = ArrayBuffer.from(x.fieldNames)), x, parent)
    case x: FieldExpr =>
      finishCloned(FieldExpr(base0 = x.base, path = ArrayBuffer.from(x.path)), x, parent)
    case x: FieldByNameExpr =>
      finishCloned(FieldByNameExpr(base0 = x.base, fieldNames = ArrayBuffer.from(x.fieldNames)), x, parent)
    case x: InstanceOfExpr =>
      finishCloned(InstanceOfExpr(obj0 = x.obj, targetType = x.targetType), x, parent)
    case x: GetInstantiateValueExpr =>
      finishCloned(GetInstantiateValueExpr(instVal0 = x.instVal, instantiateTypes = ArrayBuffer.from(x.instantiateTypes)), x, parent)
    case x: DebugExpr =>
      finishCloned(DebugExpr(srcCodeName = x.srcCodeName, value0 = x.value), x, parent)
    case x: LambdaExpr =>
      cloneLambda(x, parent)

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
    e match {
      case e: ExprBase =>
        e.resultVar = Some(ChirOps.mintResult(topLevel, resultTy, e))
    }
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
