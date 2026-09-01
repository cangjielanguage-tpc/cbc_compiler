package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR.{Binary, Intrinsic, Unary}
import com.huawei.excelsior.jet.compiler.chir.v1_0.CHIRUtils.toSeq
import com.huawei.excelsior.jet.compiler.chir.v1_0.PackageFormat.{AllocateBase, ApplyBase, BinaryExpressionBase, Branch, CHIRExprKind, Debug, Expression, Field, FieldByName, GetElementRef, GetRTTIStatic, InstanceOf, IntrinsicBase, IntrinsicKind, InvokeBase, MultiBranch, NumericCastBase, OverflowStrategy, RawArrayAllocateBase, SpawnBase, StoreElementRef, UnaryExpressionBase}

class AllocateImpl(e: AllocateBase)(implicit provider: CHIRItemProvider) extends CHIR.Allocate {
  override def allocatedType: CHIR.Type = provider.getType[CHIR.Type](e.allocatedType).get
}

final class TryAllocateImpl(e: AllocateBase)(implicit provider: CHIRItemProvider) extends AllocateImpl(e) with CHIR.TryAllocate {
  override lazy val successors: Seq[CHIR.Block] = takeLastTwoBlocks(mapOperands(e.base))
}

class ApplyImpl(e: ApplyBase)(implicit provider: CHIRItemProvider) extends CHIR.Apply {
  private val fc = e.base
  private val ex = fc.base
  private lazy val operands: Seq[CHIR.Value] = mapOperands(ex).ensuring(_.nonEmpty)  // at least callee should be here
  override def callee: CHIR.Func = operands.head.asInstanceOf[CHIR.Func]
  override def thisType: Option[CHIR.Type] = provider.getType[CHIR.Type](fc.objType)
  override def instantiatedTypeArgs: Seq[CHIR.Type] = {
    for (idx <- fc.instantiatedTypeArgsVector.toSeq) yield {
      provider.getType[CHIR.Type](idx).get
    }
  }
  override def args: Seq[CHIR.Value] = operands.tail
  override def resultTpe: CHIR.Type = provider.getType[CHIR.Type](ex.resultTy).get
}

final class TryApplyImpl(e: ApplyBase)(implicit provider: CHIRItemProvider) extends ApplyImpl(e) with CHIR.TryApply {
  override def args: Seq[CHIR.Value] = super.args.dropRight(2)
  override def successors: Seq[CHIR.Block] = takeLastTwoBlocks(super.args)
}

class BinaryImpl(e: BinaryExpressionBase)(implicit provider: CHIRItemProvider) extends CHIR.Binary {
  private val ex = e.base
  protected lazy val operands: Seq[CHIR.Value] = mapOperands(e.base).ensuring(_.size >= 2) // at least left and right operands should be here

  override def kind: Binary.Kind = ex.kind match {
    case CHIRExprKind.Add | CHIRExprKind.TryAdd => Binary.Kind.Add
    case CHIRExprKind.Sub | CHIRExprKind.TrySub => Binary.Kind.Sub
    case CHIRExprKind.Mul | CHIRExprKind.TryMul => Binary.Kind.Mul
    case CHIRExprKind.Div | CHIRExprKind.TryDiv => Binary.Kind.Div
    case CHIRExprKind.Mod | CHIRExprKind.TryMod => Binary.Kind.Mod
    case CHIRExprKind.Exp | CHIRExprKind.TryExp => Binary.Kind.Exp
    case CHIRExprKind.LShift | CHIRExprKind.TryLShift => Binary.Kind.LShift
    case CHIRExprKind.RShift | CHIRExprKind.TryRShift => Binary.Kind.RShift
    case CHIRExprKind.And => Binary.Kind.And
    case CHIRExprKind.Or => Binary.Kind.Or
    case CHIRExprKind.BitXor => Binary.Kind.Xor
    case CHIRExprKind.LT => Binary.Kind.Lt
    case CHIRExprKind.GT => Binary.Kind.Gt
    case CHIRExprKind.LE => Binary.Kind.Le
    case CHIRExprKind.GE => Binary.Kind.Ge
    case CHIRExprKind.Equal => Binary.Kind.Eq
    case CHIRExprKind.NotEqual => Binary.Kind.NotEq
  }
  override def overflowStrategy: CHIR.OverflowStrategy = mapOverflowStrategy(e.overflowStrategy)
  override def leftOperand: CHIR.Value = operands.head
  override def rightOperand: CHIR.Value = operands.tail.head
  override def resultTpe: CHIR.Type = provider.getType[CHIR.Type](ex.resultTy).get
}

final class TryBinaryImpl(e: BinaryExpressionBase)(implicit provider: CHIRItemProvider) extends BinaryImpl(e) with CHIR.TryBinary {
  override def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

final class BranchImpl(e: Branch)(implicit provider: CHIRItemProvider) extends CHIR.Branch {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e.base).ensuring(_.size == 3)

  override def condition: CHIR.Value = operands.head
  override def trueBlock: CHIR.Block = operands.tail.head.asInstanceOf[CHIR.Block]
  override def falseBlock: CHIR.Block = operands.last.asInstanceOf[CHIR.Block]
  override def successors: Seq[CHIR.Block] = Seq(trueBlock, falseBlock)
}

final class DebugImpl(e: Debug)(implicit provider: CHIRItemProvider) extends CHIR.Debug {
}

final class FieldImpl(e: Field)(implicit provider: CHIRItemProvider) extends CHIR.Field {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e.base).ensuring(_.size == 1)
  override def base: CHIR.Value = operands.head
  override def path: Seq[Long] = e.pathVector.toSeq
}

final class GetElementRefImpl(e: GetElementRef)(implicit provider: CHIRItemProvider) extends CHIR.GetElementRef {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e.base).ensuring(_.size == 1)
  override def base: CHIR.Value = operands.head
  override def path: Seq[Long] = e.pathVector.toSeq
}

final class GetRTTIStaticImpl(e: GetRTTIStatic)(implicit provider: CHIRItemProvider) extends CHIR.GetRTTIStatic {
}

final class InstanceOfImpl(e: InstanceOf)(implicit provider: CHIRItemProvider) extends CHIR.InstanceOf {
  private lazy val operands: Seq[CHIR.Value] = {
    val op = for (idx <- e.base.operandsVector.toSeq) yield {
      provider.getValue[CHIR.Value](idx).get
    }
    assert(op.size == 1)
    op
  }
  override def obj: CHIR.Value = operands.head
  override def testType: CHIR.Type = provider.getType[CHIR.Type](e.targetType).get
}

class IntrinsicImpl(e: IntrinsicBase)(implicit provider: CHIRItemProvider) extends CHIR.Intrinsic {
  private val ex = e.base.base
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e.base.base)

  override def kind: Intrinsic.Kind = e.intrinsicKind match {
    case IntrinsicKind.ABS => Intrinsic.Kind.Abs
    case IntrinsicKind.ARRAY_ACQUIRE_RAW_DATA => Intrinsic.Kind.ArrayAcquireRawData
    case IntrinsicKind.ARRAY_GET_UNCHECKED => Intrinsic.Kind.ArrayGetUnchecked
    case IntrinsicKind.ARRAY_GET_REF_UNCHECKED => Intrinsic.Kind.ArrayGetRefUnchecked
    case IntrinsicKind.ARRAY_GET => Intrinsic.Kind.ArrayGet
    case IntrinsicKind.ARRAY_RELEASE_RAW_DATA => Intrinsic.Kind.ArrayReleaseRawData
    case IntrinsicKind.ARRAY_SET_UNCHECKED => Intrinsic.Kind.ArraySetUnchecked
    case IntrinsicKind.ARRAY_SET => Intrinsic.Kind.ArraySet
    case IntrinsicKind.ARRAY_SIZE => Intrinsic.Kind.ArraySize
    case IntrinsicKind.ATOMIC_FETCH_AND => Intrinsic.Kind.AtomicFetchAnd
    case IntrinsicKind.ATOMIC_FETCH_ADD => Intrinsic.Kind.AtomicFetchAdd
    case IntrinsicKind.ATOMIC_FETCH_OR => Intrinsic.Kind.AtomicFetchOr
    case IntrinsicKind.ATOMIC_FETCH_SUB => Intrinsic.Kind.AtomicFetchSub
    case IntrinsicKind.ATOMIC_FETCH_XOR => Intrinsic.Kind.AtomicFetchXor
    case IntrinsicKind.ATOMIC_COMPARE_AND_SWAP => Intrinsic.Kind.AtomicCAS
    case IntrinsicKind.ATOMIC_LOAD => Intrinsic.Kind.AtomicLoad
    case IntrinsicKind.ATOMIC_STORE => Intrinsic.Kind.AtomicStore
    case IntrinsicKind.ATOMIC_SWAP => Intrinsic.Kind.AtomicSwap
    case IntrinsicKind.BEGIN_CATCH => Intrinsic.Kind.BeginCatch
    case IntrinsicKind.PREINITIALIZE => Intrinsic.Kind.Preinitialize
    case IntrinsicKind.CPOINTER_READ => Intrinsic.Kind.CPointerRead
    case IntrinsicKind.CPOINTER_WRITE => Intrinsic.Kind.CPointerWrite
    case IntrinsicKind.OBJECT_ZERO_VALUE => Intrinsic.Kind.ObjectZeroValue
    case IntrinsicKind.SQRT => Intrinsic.Kind.Sqrt
  }
  override def args: Seq[CHIR.Value] = operands
  override def resultTpe: CHIR.Type = provider.getType[CHIR.Type](ex.resultTy).get
}

final class TryIntrinsicImpl(e: IntrinsicBase)(implicit provider: CHIRItemProvider) extends IntrinsicImpl(e) with CHIR.TryIntrinsic {
  override def args: Seq[CHIR.Value] = super.args.dropRight(2)
  override def successors: Seq[CHIR.Block] = takeLastTwoBlocks(super.args)
}

class InvokeImpl(e: InvokeBase)(implicit provider: CHIRItemProvider) extends CHIR.Invoke {
  private val fc = e.base
  private val ex = fc.base
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e.base.base).ensuring(_.size >= 2) // at least callee and this should be here
  override def callee: CHIR.Func = operands.head.asInstanceOf[CHIR.Func]
  override def thisType: CHIR.Type = provider.getType[CHIR.Type](fc.objType).get
  override def thisArg: CHIR.Value = args.head
  override def instantiatedTypeArgs: Seq[CHIR.Type] = {
    for (idx <- fc.instantiatedTypeArgsVector.toSeq) yield {
      provider.getType[CHIR.Type](idx).get
    }
  }
  override def args: Seq[CHIR.Value] = operands.tail
  override def resultTpe: CHIR.Type = provider.getType[CHIR.Type](ex.resultTy).get
}

final class TryInvokeImpl(e: InvokeBase)(implicit provider: CHIRItemProvider) extends InvokeImpl(e) with CHIR.TryInvoke {
  override def args: Seq[CHIR.Value] = super.args.dropRight(2)
  override def successors: Seq[CHIR.Block] = takeLastTwoBlocks(super.args)
}

final class MultiBranchImpl(e: MultiBranch)(implicit provider: CHIRItemProvider) extends CHIR.MultiBranch {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e.base).ensuring(_.size >= 2) // at least condition and default block should be here

  override def condition: CHIR.Value = operands.head
  override def defaultBlock: CHIR.Block = operands.tail.head.asInstanceOf[CHIR.Block]
  override def normalBlocks: Seq[CHIR.Block] = {
    val blocks = operands.drop(2)
    blocks.collect {
      case t: CHIR.Block => t
    }.ensuring(_.size == blocks.size)
  }
  override def caseValues: Seq[Long] = e.caseValuesVector.toSeq
  override def successors: Seq[CHIR.Block] = defaultBlock +: normalBlocks
}

trait CastImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.Cast {
  protected lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty) // at least operand should be here

  override def value: CHIR.Value = operands.head
  override def targetTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
}

class NumericCastImpl(e: NumericCastBase)(implicit provider: CHIRItemProvider) extends CastImpl(e.base) with CHIR.NumericCast {
  override def overflowStrategy: CHIR.OverflowStrategy = mapOverflowStrategy(e.overflowStrategy)
}

final class TryNumericCastImpl(e: NumericCastBase)(implicit provider: CHIRItemProvider) extends NumericCastImpl(e) with CHIR.TryNumericCast {
  override def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

class RawArrayAllocateImpl(e: RawArrayAllocateBase)(implicit provider: CHIRItemProvider) extends CHIR.RawArrayAllocate {
  protected lazy val operands: Seq[CHIR.Value] = mapOperands(e.base).ensuring(_.nonEmpty) // at least size should be here

  override def elementType: CHIR.Type = provider.getType[CHIR.Type](e.elementType).get
  override def size: CHIR.Value = operands.head
}

final class TryRawArrayAllocateImpl(e: RawArrayAllocateBase)(implicit provider: CHIRItemProvider) extends RawArrayAllocateImpl(e) with CHIR.TryRawArrayAllocate {
  override def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

class SpawnImpl(e: SpawnBase)(implicit provider: CHIRItemProvider) extends CHIR.Spawn {
  protected lazy val operands: Seq[CHIR.Value] = mapOperands(e.base).ensuring(_.nonEmpty)  // at least obj should be here

  override def obj: CHIR.Value = operands.head
  override def executeClosure: Option[CHIR.Func] = provider.getValue[CHIR.Func](e.executeClosure)
  override def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.base.resultTy).get
}

final class TrySpawnImpl(e: SpawnBase)(implicit provider: CHIRItemProvider) extends SpawnImpl(e) with CHIR.TrySpawn {
  override def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

final class StoreElementRefImpl(e: StoreElementRef)(implicit provider: CHIRItemProvider) extends CHIR.StoreElementRef {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e.base).ensuring(_.size == 2)

  override def value: CHIR.Value = operands.head
  override def location: CHIR.Value = operands.last
  override def path: Seq[Long] = e.pathVector.toSeq
}

class UnaryImpl(e: UnaryExpressionBase)(implicit provider: CHIRItemProvider) extends CHIR.Unary {
  protected lazy val operands: Seq[CHIR.Value] = mapOperands(e.base).ensuring(_.nonEmpty) // at least operand should be here

  override def operand: CHIR.Value = operands.head
  override def kind: Unary.Kind = e.base.kind match {
    case CHIRExprKind.BitNot => Unary.Kind.BitNot
    case CHIRExprKind.Not => Unary.Kind.Not
    case CHIRExprKind.Neg => Unary.Kind.Neg
  }
  override def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.base.resultTy).get
}

final class TryUnaryImpl(e: UnaryExpressionBase)(implicit provider: CHIRItemProvider) extends UnaryImpl(e) with CHIR.TryUnary {
  override def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

final class GotoImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.Goto {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.size == 1)
  override def destination: CHIR.Block = operands.head.asInstanceOf[CHIR.Block]
  override def successors: Seq[CHIR.Block] = Seq(destination)
}

final class ExitImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.Exit {
}

final class RaiseExceptionImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.RaiseException {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty)

  override def exceptionValue: CHIR.Value = operands.head
  override def exceptionBlock: Option[CHIR.Block] = operands.tail.headOption.collect {
    case t: CHIR.Block => t
  }
  override def successors: Seq[CHIR.Block] = exceptionBlock.toSeq
}

final class StaticCastImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CastImpl(e) with CHIR.StaticCast {
}

final class BoxImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CastImpl(e) with CHIR.Box {
}

final class UnboxToValueImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CastImpl(e) with CHIR.UnboxToValue {
}

final class CastToConcreteImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CastImpl(e) with CHIR.CastToConcrete {
}

final class CastToGenericImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CastImpl(e) with CHIR.CastToGeneric {
}

final class LoadImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.Load {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.size == 1)

  override def location: CHIR.Value = operands.head
}

final class StoreImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.Store {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.size == 2)

  override def value: CHIR.Value = operands.head
  override def location: CHIR.Value = operands.last
}

final class RawArrayLiteralInitImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.RawArrayLiteralInit {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty)

  override def array: CHIR.Value = operands.head
  override def elementValues: Seq[CHIR.Value] = operands.tail
}

final class RawArrayInitByValueImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.RawArrayInitByValue {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.size == 3)

  override def array: CHIR.Value = operands.head
  override def size: CHIR.Value = operands.tail.head
  override def initValue: CHIR.Value = operands.last
}

final class ConstantImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.Constant {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.size == 1)
  
  override def literal: CHIR.Value = operands.head
  override def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
}

final class TupleImpl(e: Expression)(implicit provider: CHIRItemProvider) extends CHIR.Tuple {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e)
  
  override def elementValues: Seq[CHIR.Value] = operands
  override def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
}

private def mapOverflowStrategy(os: Int): CHIR.OverflowStrategy = os match {
  case OverflowStrategy.NA => CHIR.OverflowStrategy.Na
  case OverflowStrategy.WRAPPING => CHIR.OverflowStrategy.Wrapping
  case OverflowStrategy.THROWING => CHIR.OverflowStrategy.Throwing
  case OverflowStrategy.SATURATING => CHIR.OverflowStrategy.Saturating
}

private def mapOperands(e: Expression)(implicit provider: CHIRItemProvider): Seq[CHIR.Value] = {
  for (idx <- e.operandsVector.toSeq) yield {
    provider.getValue[CHIR.Value](idx).get
  }
}

private def takeLastTwoBlocks(operands: Seq[CHIR.Value]): Seq[CHIR.Block] = {
  operands.takeRight(2).collect {
    case t: CHIR.Block => t
  }.ensuring(_.size == 2) 
}
