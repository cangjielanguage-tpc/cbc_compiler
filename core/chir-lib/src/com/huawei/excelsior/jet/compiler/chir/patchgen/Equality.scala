package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.CHIR.*

object Equality {

  /** Compares corresponding blocks and their expressions, including terminators, in order. */
  def sameCode(left: BlockGroup, right: BlockGroup): Boolean = {
    val leftBlocks = left.blocks
    val rightBlocks = right.blocks
    leftBlocks.size == rightBlocks.size &&
      leftBlocks.iterator.zip(rightBlocks.iterator).forall { case (a, b) =>
        val leftExprs = a.expressions
        val rightExprs = b.expressions
        leftExprs.size == rightExprs.size &&
          leftExprs.iterator.zip(rightExprs.iterator).forall { case (a, b) => sameExprs(a, b) }
      }
  }

  /** Compares declarations using their equality, literals by type and payload, and locals
    * and parameters by identifiers and type. Blocks are ignored.
    */
  private def sameValues(left: Value, right: Value): Boolean = (left, right) match {
    case (a: Func, b: Func) => a == b
    case (a: GlobalVar, b: GlobalVar) => a == b
    case (a: LocalVar, b: LocalVar) =>
      a.identifier == b.identifier && a.srcCodeIdentifier == b.srcCodeIdentifier && a.tpe == b.tpe
    case (a: Parameter, b: Parameter) =>
      a.identifier == b.identifier && a.srcCodeIdentifier == b.srcCodeIdentifier && a.tpe == b.tpe
    case (a: NullLiteral, b: NullLiteral) => a.tpe == b.tpe
    case (a: IntLiteral, b: IntLiteral) => a.tpe == b.tpe && a.value == b.value
    case (a: FloatLiteral, b: FloatLiteral) =>
      a.tpe == b.tpe &&
        java.lang.Double.doubleToLongBits(a.value) == java.lang.Double.doubleToLongBits(b.value)
    case (a: BoolLiteral, b: BoolLiteral) => a.tpe == b.tpe && a.value == b.value
    case (a: RuneLiteral, b: RuneLiteral) => a.tpe == b.tpe && a.value == b.value
    case (a: StringLiteral, b: StringLiteral) => a.tpe == b.tpe && a.value == b.value
    case (_: Block, _: Block) => true
    case (_: BlockGroup, _: BlockGroup) => true
    case _ => false
  }

  private def sameValues(left: Seq[Value], right: Seq[Value]): Boolean =
    left.size == right.size && left.iterator.zip(right.iterator).forall {
      case (a, b) => sameValues(a, b)
    }

  private def sameValues(left: Option[Value], right: Option[Value]): Boolean =
    (left, right) match {
      case (Some(a), Some(b)) => sameValues(a, b)
      case (None, None) => true
      case _ => false
    }

  /** Compares operations through the CHIR trait API. Debug locations, result-variable
    * identities, and block targets are ignored; value operands use [[sameValues]].
    */
  private def sameExprs(left: Expression, right: Expression): Boolean = (left, right) match {
    case (a: Cast, b: Cast) =>
      val sameKind = (a, b) match {
        case (a: NumericCast, b: NumericCast) => a.overflowStrategy == b.overflowStrategy
        case (_: StaticCast, _: StaticCast) |
             (_: UnboxToRef, _: UnboxToRef) |
             (_: UnboxToValue, _: UnboxToValue) |
             (_: Box, _: Box) |
             (_: CastToConcrete, _: CastToConcrete) |
             (_: CastToGeneric, _: CastToGeneric) => true
        case _ => false
      }
      sameKind && sameValues(a.value, b.value) && a.targetTpe == b.targetTpe
    case (a: Unary, b: Unary) =>
      a.kind == b.kind && sameValues(a.operand, b.operand) &&
        a.overflowStrategy == b.overflowStrategy
    case (a: Binary, b: Binary) =>
      a.kind == b.kind && sameValues(a.leftOperand, b.leftOperand) &&
        sameValues(a.rightOperand, b.rightOperand) && a.overflowStrategy == b.overflowStrategy
    case (a: Allocate, b: Allocate) => a.allocatedType == b.allocatedType
    case (a: RawArrayAllocate, b: RawArrayAllocate) =>
      a.elementType == b.elementType && sameValues(a.size, b.size)
    case (a: GetElementRef, b: GetElementRef) => sameValues(a.base, b.base) && a.path == b.path
    case (a: StoreElementRef, b: StoreElementRef) =>
      sameValues(a.value, b.value) && sameValues(a.location, b.location) && a.path == b.path
    case (a: Field, b: Field) => sameValues(a.base, b.base) && a.path == b.path
    case (a: Apply, b: Apply) =>
      sameValues(a.callee, b.callee) && a.thisType == b.thisType &&
        (a.thisType.isEmpty || sameValues(a.thisArg, b.thisArg)) &&
        a.instantiatedTypeArgs == b.instantiatedTypeArgs && sameValues(a.args, b.args)
    case (a: Invoke, b: Invoke) =>
      a.callee == b.callee && a.thisType == b.thisType && sameValues(a.thisArg, b.thisArg) &&
        a.instantiatedTypeArgs == b.instantiatedTypeArgs &&
        a.overflowStrategy == b.overflowStrategy && sameValues(a.args, b.args)
    case (a: GetRTTIStatic, b: GetRTTIStatic) => a.rttiType == b.rttiType
    case (a: GetRTTI, b: GetRTTI) => sameValues(a.obj, b.obj)
    case (a: InstanceOf, b: InstanceOf) => sameValues(a.obj, b.obj) && a.testType == b.testType
    case (a: Intrinsic, b: Intrinsic) => a.kind == b.kind && sameValues(a.args, b.args)
    case (a: Spawn, b: Spawn) => sameValues(a.obj, b.obj) && sameValues(a.executeClosure, b.executeClosure)
    case (_: Debug, _: Debug) => true
    case (a: Constant, b: Constant) => sameValues(a.literal, b.literal)
    case (a: Load, b: Load) => sameValues(a.location, b.location)
    case (a: Store, b: Store) => sameValues(a.value, b.value) && sameValues(a.location, b.location)
    case (a: Tuple, b: Tuple) => sameValues(a.elementValues, b.elementValues)
    case (_: GetException, _: GetException) => true
    case (a: VArray, b: VArray) => sameValues(a.elementValues, b.elementValues)
    case (a: VArrayBuilder, b: VArrayBuilder) =>
      sameValues(a.size, b.size) && sameValues(a.initValue, b.initValue) && sameValues(a.initializer, b.initializer)
    case (a: RawArrayInitByValue, b: RawArrayInitByValue) =>
      sameValues(a.array, b.array) && sameValues(a.size, b.size) && sameValues(a.initValue, b.initValue)
    case (a: RawArrayLiteralInit, b: RawArrayLiteralInit) =>
      sameValues(a.array, b.array) && sameValues(a.elementValues, b.elementValues)
    case (a: Branch, b: Branch) => sameValues(a.condition, b.condition)
    case (_: Exit, _: Exit) => true
    case (_: Goto, _: Goto) => true
    case (a: MultiBranch, b: MultiBranch) =>
      sameValues(a.condition, b.condition) && a.caseValues == b.caseValues
    case (a: RaiseException, b: RaiseException) =>
      sameValues(a.exceptionValue, b.exceptionValue)
    case _ => false
  }
}
