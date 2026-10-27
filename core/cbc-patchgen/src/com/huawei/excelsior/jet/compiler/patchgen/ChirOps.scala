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

/** CHIR mutation primitives, ported from the C++ IR implementation
  * (`src/CHIR/IR/Expression/Expression.cpp`, `src/CHIR/IR/Value/Value.cpp`,
  * `include/cangjie/CHIR/IR/CHIRBuilder.h`) onto the mutable chir-lib model.
  *
  * The chir-lib model has no use-lists; users of a value are found by scanning
  * live bodies on demand. Predecessor lists ARE maintained, like in C++.
  */
object ChirOps {

  // =================================================================================
  // traversal (port of Visitor + SimpleIterator; snapshots make mutation safe)
  // =================================================================================

  /** Port of `Visitor::Visit(func, pre)`: visits every expression of the function
    * body (including expressions inside lambda bodies) in block order, over
    * snapshots of the expression lists (C++ `Iterator::Iterate` returns by value),
    * so expressions appended during the walk are not revisited.
    */
  def visitFuncExprs(func: Func)(pre: Expression => Unit): Unit =
    func.body.foreach(visitBlockGroupExprs)

  def visitBlockGroupExprs(bg: BlockGroup)(pre: Expression => Unit): Unit =
    for (block <- bg.blocks.toList) visitBlockExprs(block)(pre)

  def visitBlockExprs(block: Block)(pre: Expression => Unit): Unit =
    for (expr <- block.exprs.toList) {
      pre(expr)
      visitExprChildren(expr)(pre)

    }
  /** Port of `Walk(expr, pre)`: descends into the expression's (live) block groups. */
  def visitExprChildren(expr: Expression)(pre: Expression => Unit): Unit =
    for (bg <- expr.blockGroups.toList) visitBlockGroupExprs(bg)(pre)

  // =================================================================================
  // operands (flat, in C++ Expression::GetOperands() order)
  // =================================================================================

  def operandsOf(e: Expression): ArrayBuffer[Value] = {
    val ops = ArrayBuffer[Value]()
    e match {
      case e: GotoExpr            => ops += e.destination
      case e: BranchExpr          => ops ++= Seq(e.condition, e.trueBlock, e.falseBlock)
      case e: MultiBranchExpr     => ops ++= e.condition +: e.defaultBlock +: e.normalBlocks
      case e: RaiseExceptionExpr  => ops += e.exceptionValue; e.exceptionBlock.foreach(ops += _)
      case e: AllocateExpr        => if (e.isTry) ops ++= e.handlerBlocks
      case e: ApplyExpr           => ops += e.callee; ops ++= e.args; if (e.isTry) ops ++= e.handlerBlocks
      case e: InvokeExpr          => ops ++= Seq(e.callee, e.caller); ops ++= e.args; if (e.isTry) ops ++= e.handlerBlocks
      case e: IntrinsicExpr       => ops ++= e.args; if (e.isTry) ops ++= e.handlerBlocks
      case e: UnaryExpr           => ops += e.operand
      case e: BinaryExpr          => ops ++= Seq(e.leftOperand, e.rightOperand)
      case e: NumericCastExpr     => ops += e.value
      case e: CastExpr            => ops += e.value
      case e: LoadExpr            => ops += e.location
      case e: StoreExpr           => ops ++= Seq(e.value, e.location)
      case e: GetElementRefExpr   => ops += e.base
      case e: GetElementByNameExpr => ops += e.base
      case e: StoreElementRefExpr => ops ++= Seq(e.value, e.location)
      case e: StoreElementByNameExpr => ops ++= Seq(e.value, e.location)
      case e: FieldExpr           => ops += e.base
      case e: FieldByNameExpr     => ops += e.base
      case e: InstanceOfExpr      => ops += e.obj
      case e: GetRTTIExpr         => ops += e.obj
      case e: ConstantExpr        => ops += e.literal
      case e: DebugExpr           => ops += e.value
      case e: TupleExpr           => ops ++= e.elementValues
      case e: RawArrayAllocateExpr => ops += e.size
      case e: RawArrayLiteralInitExpr => ops += e.array; ops ++= e.elementValues
      case e: RawArrayInitByValueExpr => ops ++= Seq(e.array, e.size, e.initValue)
      case e: VArrayExpr          => ops ++= e.elementValues
      case e: VArrayBuilderExpr   => ops ++= Seq(e.size, e.initValue, e.initializer)
      case e: GetInstantiateValueExpr => ops += e.instVal
      case e: SpawnExpr           => ops += e.obj; e.spawnArg.foreach(ops += _)
      case _: ExitExpr            => ()
      case _: GetRTTIStaticExpr   => ()
      case _: LambdaExpr          => ()
      case _: GetExceptionExpr    => ()
    }
    ops
  }

  /** Users of a value: live expressions whose operands reference it, found by
    * scanning all function bodies of the package (C++ maintains use-lists instead).
    */
  def usersOf(v: Value, pkg: Package): ArrayBuffer[Expression] = {
    val users = ArrayBuffer[Expression]()
    def scanFunc(f: Func): Unit = f.body.foreach { bg =>
      for (block <- bg.blocks) {
        for (expr <- block.exprs) {
          if (operandsOf(expr).exists(_ eq v)) users += expr
          for (lambdaBg <- expr.blockGroups) scanBlockGroup(lambdaBg)
        }
      }
    }
    def scanBlockGroup(bg: BlockGroup): Unit =
      for (block <- bg.blocks) {
        for (expr <- block.exprs) {
          if (operandsOf(expr).exists(_ eq v)) users += expr
          for (lambdaBg <- expr.blockGroups) scanBlockGroup(lambdaBg)
        }
      }
    pkg.globalFuncs.foreach(scanFunc)
    users
  }

  /** Port of `Expression::ReplaceOperand`. */
  def replaceOperand(e: Expression, oldV: Value, newV: Value): Unit =
    if ((oldV ne newV) && operandsOf(e).exists(_ eq oldV)) {
      def rep(v: Value): Value = if (v eq oldV) newV else v
      def repBuf[T <: Value](buf: ArrayBuffer[T]): Unit =
        for (i <- buf.indices) if (buf(i) eq oldV) buf(i) = newV.asInstanceOf[T]
      e match {
        case e: GotoExpr            => e.destination = rep(e.destination).asInstanceOf[Block]
        case e: BranchExpr          => e.condition = rep(e.condition); e.trueBlock = rep(e.trueBlock).asInstanceOf[Block]; e.falseBlock = rep(e.falseBlock).asInstanceOf[Block]
        case e: MultiBranchExpr     => e.condition = rep(e.condition); if (e.defaultBlock eq oldV) e.defaultBlock = newV.asInstanceOf[Block]; repBuf(e.normalBlocks)
        case e: RaiseExceptionExpr  => e.exceptionValue = rep(e.exceptionValue); e.exceptionBlock.foreach(b => if (b eq oldV) e.exceptionBlock = Some(newV.asInstanceOf[Block]))
        case e: AllocateExpr        => if (e.isTry) repBuf(e.handlerBlocks)
        case e: ApplyExpr           => if (e.callee eq oldV) e.callee = newV; repBuf(e.args); if (e.isTry) repBuf(e.handlerBlocks)
        case e: InvokeExpr          => if (e.callee eq oldV) e.callee = newV.asInstanceOf[Func]; if (e.caller eq oldV) e.caller = newV; repBuf(e.args); if (e.isTry) repBuf(e.handlerBlocks)
        case e: IntrinsicExpr       => repBuf(e.args); if (e.isTry) repBuf(e.handlerBlocks)
        case e: UnaryExpr           => e.operand = rep(e.operand)
        case e: BinaryExpr          => e.leftOperand = rep(e.leftOperand); e.rightOperand = rep(e.rightOperand)
        case e: NumericCastExpr     => e.value = rep(e.value)
        case e: CastExpr            => e.value = rep(e.value)
        case e: LoadExpr            => e.location = rep(e.location)
        case e: StoreExpr           => e.value = rep(e.value); e.location = rep(e.location)
        case e: GetElementRefExpr   => e.base = rep(e.base)
        case e: GetElementByNameExpr => e.base = rep(e.base)
        case e: StoreElementRefExpr => e.value = rep(e.value); e.location = rep(e.location)
        case e: StoreElementByNameExpr => e.value = rep(e.value); e.location = rep(e.location)
        case e: FieldExpr           => e.base = rep(e.base)
        case e: FieldByNameExpr     => e.base = rep(e.base)
        case e: InstanceOfExpr      => e.obj = rep(e.obj)
        case e: GetRTTIExpr         => e.obj = rep(e.obj)
        case e: ConstantExpr        => e.literal = rep(e.literal)
        case e: DebugExpr           => e.value = rep(e.value)
        case e: TupleExpr           => repBuf(e.elementValues)
        case e: RawArrayAllocateExpr => e.size = rep(e.size)
        case e: RawArrayLiteralInitExpr => e.array = rep(e.array); repBuf(e.elementValues)
        case e: RawArrayInitByValueExpr => e.array = rep(e.array); e.size = rep(e.size); e.initValue = rep(e.initValue)
        case e: VArrayExpr          => repBuf(e.elementValues)
        case e: VArrayBuilderExpr   => e.size = rep(e.size); e.initValue = rep(e.initValue); e.initializer = rep(e.initializer)
        case e: GetInstantiateValueExpr => e.instVal = rep(e.instVal)
        case e: SpawnExpr           => e.obj = rep(e.obj); e.spawnArg.foreach(a => if (a eq oldV) e.spawnArg = Some(newV))
        case _: ExitExpr            => ()
        case _: GetRTTIStaticExpr   => ()
        case _: LambdaExpr          => ()
        case _: GetExceptionExpr    => ()
      }
      // block operands update CFG edges (port of the C++ predecessor bookkeeping)
      oldV match {
        case b: Block => e.owner.foreach(o => removePredecessor(b, o))
        case _         =>
      }
      newV match {
        case n: Block => e.owner.foreach(o => addPredecessor(n, o))
        case _         =>

      }
    }
  // =================================================================================
  // block / block group primitives
  // =================================================================================

  def addPredecessor(block: Block, pred: Block): Unit =
    if (!block.predecessors.exists(_ eq pred)) block.predecessors += pred

  def removePredecessor(block: Block, pred: Block): Unit =
    block.predecessors -= pred

  /** Port of `Block::RemoveExprOnly`. */
  def removeExprOnly(block: Block, expr: Expression): Unit =
    block.exprs -= expr

  /** Port of `Block::AppendExpression`. */
  def appendExpression(block: Block, expr: Expression): Unit = {
    expr.owner.foreach(removeExprOnly(_, expr))
    expr match {
      case expr: ExprBase => expr.owner_=(Some(block))
    }
    block.exprs += expr
    for (suc <- expr.successors) addPredecessor(suc, block)
  }

  /** Port of `Expression::RemoveSelfFromBlock`. */
  def removeSelfFromBlock(expr: Expression): Unit = {
    for (suc <- expr.successors) expr.owner.foreach(removePredecessor(suc, _))
    expr.owner.foreach(removeExprOnly(_, expr))
    setOwner(expr, None)
  }

  /** Port of `Block::RemoveSelfFromBlockGroup`. */
  def removeSelfFromBlockGroup(block: Block): Unit = {
    block.parentGroup.foreach { group =>
      group.blocks -= block
      if (group.entryBlock.exists(_ eq block)) group.entryBlock = None
    }
    block.parentGroup = None
    block.predecessors.clear()
    for (expr <- block.exprs.toList) removeSelfFromBlock(expr)
    block.exprs.clear()
  }

  // =================================================================================
  // expression primitives
  // =================================================================================

  private def setOwner(expr: Expression, owner: Option[Block]): Unit = expr match {
    case expr: ExprBase => expr.owner_=(owner)
  }
  /** Port of `Expression::ReplaceWith`: rewires all users of the result to the new
    * expression's result, removes self from its block (dropping predecessor edges
    * of the successors) and inserts the new expression at the same position.
    */
  def replaceWith(expr: Expression, newExpr: Expression, pkg: Package): Unit = {
    // 1. replace result
    expr.resultVar.foreach { result =>
      newExpr.resultVar.foreach { newResult =>
        for (user <- usersOf(result, pkg).toList) {
          replaceOperand(user, result, newResult)
        }
      }
    }
    // 2-3. remember position, remove self
    val tempParent = expr.owner.getOrElse(throw new IllegalStateException("expression has no parent block"))
    val idx = tempParent.exprs.indexWhere(_ eq expr)
    if (idx < 0) throw new IllegalStateException("expression not found in its parent block")
    removeSelfFromBlock(expr)
    // 4. detach the new expression from its old parent (no pred edge updates, like C++)
    newExpr match {
      case newExpr: ExprBase =>
        newExpr.owner.foreach(removeExprOnly(_, newExpr))
        newExpr.owner_=(Some(tempParent))
    }
    // 5-6. insert at the same position
    tempParent.exprs.insert(idx, newExpr)
    // 7. add the successors of the new expression to the parent block
    for (suc <- newExpr.successors) addPredecessor(suc, tempParent)
  }

  /** Port of `Expression::MoveBefore`. */
  def moveBefore(expr: Expression, target: Expression): Unit = {
    if (expr eq target) return
    expr.owner.foreach(removeExprOnly(_, expr))
    val targetParent = target.owner.getOrElse(throw new IllegalStateException("target has no parent block"))
    val idx = targetParent.exprs.indexWhere(_ eq target)
    targetParent.exprs.insert(idx, expr)
    setOwner(expr, Some(targetParent))
  }

  /** Port of `Expression::MoveAfter`. */
  def moveAfter(expr: Expression, target: Expression): Unit = {
    expr.owner.foreach { old =>
      removeExprOnly(old, expr)
      for (suc <- expr.successors) removePredecessor(suc, old)
    }
    val targetParent = target.owner.getOrElse(throw new IllegalStateException("target has no parent block"))
    val idx = targetParent.exprs.indexWhere(_ eq target)
    targetParent.exprs.insert(idx + 1, expr)
    setOwner(expr, Some(targetParent))
    for (suc <- expr.successors) addPredecessor(suc, targetParent)
  }

  /** Port of `Expression::MoveTo`. */
  def moveTo(expr: Expression, block: Block): Unit = {
    expr.owner.foreach { old =>
      removeExprOnly(old, expr)
      for (suc <- expr.successors) removePredecessor(suc, old)
    }
    appendExpression(block, expr)
  }

  // =================================================================================
  // function body primitives
  // =================================================================================

  /** Port of `Function::DestroyFuncBody`. */
  def destroyFuncBody(func: Func): Unit = {
    func.retVal = None
    func.body.foreach { bg =>
      for (block <- bg.blocks) {
        for (expr <- block.exprs.toList) removeSelfFromBlock(expr)
      }
    }
    func.body = None
  }

  /** Port of `Function::ReplaceBody`. */
  def replaceBody(func: Func, newBody: BlockGroup): Unit = {
    destroyFuncBody(func)
    func.body = Some(newBody)
    newBody.ownedFunc = Some(func)
  }

  /** Port of `BlockGroup::SetEntryBlock`. */
  def setEntryBlock(bg: BlockGroup, block: Block): Unit =
    bg.entryBlock = Some(block)

  // =================================================================================
  // identifier minting (port of CHIRBuilder value creation)
  // =================================================================================

  /** The top-level function owning a block group, either directly or through a
    * lambda expression (port of `Block::GetTopLevelFunc`).
    */
  def topLevelFunc(bg: BlockGroup): Option[Func] =
    bg.ownedFunc match {
      case Some(f) => Some(f)
      case None    =>
        bg.ownedExpression.flatMap(_.owner).flatMap(_.parentGroup).flatMap(topLevelFunc)

    }
  /** Mints a fresh result local var for an expression, like C++
    * `CHIRBuilder::CreateExpression` (identifier "%<n>" from the top-level
    * function's localId counter).
    */
  def mintResult(topLevel: Func, tpe: Type, expr: Expression): LocalVar = {
    val id = topLevel.generateLocalId()
    val lv = LocalVar(ArrayBuffer(), None, 0L, Some(tpe), "%" + id, Some(expr), isRetVal = false, "")
    lv.id = id.toInt // transient; the serializer renumbers
    lv
  }

}
