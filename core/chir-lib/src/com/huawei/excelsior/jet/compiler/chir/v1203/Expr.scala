package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.*
import com.huawei.excelsior.jet.compiler.chir.CHIR.{Binary, Intrinsic, Unary}
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v1203.CHIRUtils.{toSeq, toTypeSeq, toValueSeq}

class AllocateImpl(val a: AllocateBase)(implicit val provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Allocate {
  val e: Expression = a.base
  def allocatedType: CHIR.Type = provider.getType[CHIR.Type](a.allocatedType).get
}

final class TryAllocateImpl(allocation: AllocateBase)(implicit provider: CHIRItemProvider) extends AllocateImpl(allocation) with CHIR.TryAllocate {
  lazy val successors: Seq[CHIR.Block] = takeLastTwoBlocks(mapOperands(e))
}

class ApplyImpl(a: ApplyBase)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Apply {
  private val fc = a.base
  val e: Expression = fc.base
  lazy val Seq(callee: CHIR.Func, args: _*) = mapOperands(e)
  def thisType: Option[CHIR.Type] = provider.getType[CHIR.Type](fc.objType)
  def thisArg: CHIR.Value = args.head
  def instantiatedTypeArgs = fc.instantiatedTypeArgsVector.toTypeSeq[CHIR.Type]
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.resultLocalVar).get
}

final class TryApplyImpl(e: ApplyBase)(implicit provider: CHIRItemProvider) extends ApplyImpl(e) with CHIR.TryApply {
  override def args: Seq[CHIR.Value] = super.args.dropRight(2)
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(super.args)
}

class BinaryImpl(b: BinaryExpressionBase)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Binary {
  val e = b.base
  lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.size >= 2) // at least left and right operands should be here

  def kind: Binary.Kind = e.kind match {
    case CHIRExprKind.Add | CHIRExprKind.TryAdd => Binary.Kind.Add
    case CHIRExprKind.Sub | CHIRExprKind.TrySub => Binary.Kind.Sub
    case CHIRExprKind.Mul | CHIRExprKind.TryMul => Binary.Kind.Mul
    case CHIRExprKind.Div | CHIRExprKind.TryDiv => Binary.Kind.Div
    case CHIRExprKind.Mod | CHIRExprKind.TryMod => Binary.Kind.Mod
    case CHIRExprKind.Exp | CHIRExprKind.TryExp => Binary.Kind.Exp
    case CHIRExprKind.LShift | CHIRExprKind.TryLShift => Binary.Kind.LShift
    case CHIRExprKind.RShift | CHIRExprKind.TryRShift => Binary.Kind.RShift
    case CHIRExprKind.BitAnd => Binary.Kind.And
    case CHIRExprKind.BitOr => Binary.Kind.Or
    case CHIRExprKind.BitXor => Binary.Kind.Xor
    case CHIRExprKind.LT => Binary.Kind.Lt
    case CHIRExprKind.GT => Binary.Kind.Gt
    case CHIRExprKind.LE => Binary.Kind.Le
    case CHIRExprKind.GE => Binary.Kind.Ge
    case CHIRExprKind.Equal => Binary.Kind.Eq
    case CHIRExprKind.NotEqual => Binary.Kind.NotEq
  }
  def overflowStrategy: CHIR.OverflowStrategy = mapOverflowStrategy(b.overflowStrategy)
  def leftOperand: CHIR.Value = operands.head
  def rightOperand: CHIR.Value = operands(1)
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.resultLocalVar).get
}

final class TryBinaryImpl(e: BinaryExpressionBase)(implicit provider: CHIRItemProvider) extends BinaryImpl(e) with CHIR.TryBinary {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

final class BranchImpl(b: Branch)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Branch {
  val e: Expression = b.base
  lazy val Seq(condition: CHIR.Value, trueBlock: CHIR.Block, falseBlock: CHIR.Block) = mapOperands(e)
  def successors: Seq[CHIR.Block] = Seq(trueBlock, falseBlock)
}

final class DebugImpl(d: Debug)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Debug {
  val e: Expression = d.base
}

final class FieldImpl(f: Field)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Field {
  val e: Expression = f.base
  lazy val Seq(base: CHIR.Value) = mapOperands(e)
  def path: Seq[Long] = f.pathVector.toSeq
}

final class GetElementRefImpl(g: GetElementRef)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.GetElementRef {
  val e: Expression = g.base
  lazy val Seq(base: CHIR.Value) = mapOperands(e)
  def path: Seq[Long] = g.pathVector.toSeq
}

final class GetRTTIStaticImpl(g: GetRTTIStatic)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.GetRTTIStatic {
  val e: Expression = g.base
}

final class InstanceOfImpl(i: InstanceOf)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.InstanceOf {
  val e: Expression = i.base
  lazy val Seq(obj: CHIR.Value) = mapOperands(e)
  def testType: CHIR.Type = provider.getType[CHIR.Type](i.targetType).get
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.resultLocalVar).get
}

class IntrinsicImpl(in: IntrinsicBase)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Intrinsic {
  val e = in.base.base
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e)

  def kind: Intrinsic.Kind = in.intrinsicKind match {
    case IntrinsicKind.ABS => Intrinsic.Kind.Abs
    case IntrinsicKind.FABS => Intrinsic.Kind.Fabs
    case IntrinsicKind.ARRAY_ACQUIRE_RAW_DATA => Intrinsic.Kind.ArrayAcquireRawData
    case IntrinsicKind.ARRAY_BUILT_IN_COPY_TO => Intrinsic.Kind.ArrayBuiltinCopyTo
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
    case IntrinsicKind.POW => Intrinsic.Kind.Pow
  }
  def args: Seq[CHIR.Value] = operands
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.resultLocalVar).get
}

final class TryIntrinsicImpl(e: IntrinsicBase)(implicit provider: CHIRItemProvider) extends IntrinsicImpl(e) with CHIR.TryIntrinsic {
  override def args: Seq[CHIR.Value] = super.args.dropRight(2)
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(super.args)
}

class InvokeImpl(i: InvokeBase)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Invoke {
  private val fc = i.base
  val e: Expression = fc.base
  lazy val callee: CHIR.FuncSig = FuncSigImpl(i.virMethodCtx)
  lazy val operands: Seq[CHIR.Value] = mapOperands(e)
  def args: Seq[CHIR.Value] = operands
  def thisType: CHIR.Type = provider.getType[CHIR.Type](fc.objType).get
  def thisArg: CHIR.Value = args.head
  def instantiatedTypeArgs = fc.instantiatedTypeArgsVector.toTypeSeq[CHIR.Type]
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.resultLocalVar).get
}

final class TryInvokeImpl(e: InvokeBase)(implicit provider: CHIRItemProvider) extends InvokeImpl(e) with CHIR.TryInvoke {
  override def args: Seq[CHIR.Value] = super.args.dropRight(2)
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(super.args)
}

final class MultiBranchImpl(m: MultiBranch)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.MultiBranch {
  val e: Expression = m.base
  lazy val Seq(condition: CHIR.Value, defaultBlock: CHIR.Block, _normalBlocks: _*) = mapOperands(e)
  def normalBlocks: Seq[CHIR.Block] = {
    val blocks = _normalBlocks
    blocks.collect {
      case t: CHIR.Block => t
    }.ensuring(_.size == blocks.size)
  }
  def caseValues: Seq[Long] = m.caseValuesVector.toSeq
  def successors: Seq[CHIR.Block] = defaultBlock +: normalBlocks
}

trait CastImpl extends ExpressionImpl with CHIR.Cast {
  implicit def provider: CHIRItemProvider

  lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty)

  def value: CHIR.Value = operands.head
  def targetTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
}

class NumericCastImpl(n: NumericCastBase)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.NumericCast {
  val e: Expression = n.base
  def overflowStrategy: CHIR.OverflowStrategy = mapOverflowStrategy(n.overflowStrategy)
}

final class TryNumericCastImpl(e: NumericCastBase)(implicit provider: CHIRItemProvider) extends NumericCastImpl(e) with CHIR.TryNumericCast {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

class RawArrayAllocateImpl(r: RawArrayAllocateBase)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.RawArrayAllocate {
  val e: Expression = r.base
  lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty) // at least size should be here

  def elementType: CHIR.Type = provider.getType[CHIR.Type](r.elementType).get
  def size: CHIR.Value = operands.head
}

final class TryRawArrayAllocateImpl(e: RawArrayAllocateBase)(implicit provider: CHIRItemProvider) extends RawArrayAllocateImpl(e) with CHIR.TryRawArrayAllocate {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

class SpawnImpl(s: SpawnBase)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Spawn {
  val e: Expression = s.base
  lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty)  // at least obj should be here

  def obj: CHIR.Value = operands.head
  def executeClosure: Option[CHIR.Func] = provider.getValue[CHIR.Func](s.executeClosure)
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.resultLocalVar).get
}

final class TrySpawnImpl(e: SpawnBase)(implicit provider: CHIRItemProvider) extends SpawnImpl(e) with CHIR.TrySpawn {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

final class StoreElementRefImpl(s: StoreElementRef)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.StoreElementRef {
  val e: Expression = s.base
  lazy val Seq(value: CHIR.Value, location: CHIR.Value) = mapOperands(e)
  def path: Seq[Long] = s.pathVector.toSeq
}

class UnaryImpl(u: UnaryExpressionBase)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Unary {
  val e: Expression = u.base
  lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty) // at least operand should be here

  def operand: CHIR.Value = operands.head
  def kind: Unary.Kind = e.kind match {
    case CHIRExprKind.BitNot => Unary.Kind.BitNot
    case CHIRExprKind.Not => Unary.Kind.Not
    case CHIRExprKind.Neg | CHIRExprKind.TryNeg => Unary.Kind.Neg
  }
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.resultLocalVar).get
  def overflowStrategy: CHIR.OverflowStrategy = mapOverflowStrategy(u.overflowStrategy)
}

final class TryUnaryImpl(e: UnaryExpressionBase)(implicit provider: CHIRItemProvider) extends UnaryImpl(e) with CHIR.TryUnary {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

final class GotoImpl(val e: Expression)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Goto {
  lazy val Seq(destination: CHIR.Block) = mapOperands(e)
  def successors: Seq[CHIR.Block] = Seq(destination)
}

final class RaiseExceptionImpl(val e: Expression)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.RaiseException {
  private lazy val operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty)

  def exceptionValue: CHIR.Value = operands.head
  def exceptionBlock: Option[CHIR.Block] = operands.tail.headOption.collect {
    case t: CHIR.Block => t
  }
  def successors: Seq[CHIR.Block] = exceptionBlock.toSeq
}

final class StaticCastImpl(val e: Expression)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.StaticCast {
}

final class BoxImpl(val e: Expression)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.Box {
}

final class UnboxToValueImpl(val e: Expression)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.UnboxToValue {
}

final class CastToConcreteImpl(val e: Expression)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.CastToConcrete {
}

final class CastToGenericImpl(val e: Expression)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.CastToGeneric {
}

final class LoadImpl(val e: Expression)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Load {
  lazy val Seq(location: CHIR.Value) = mapOperands(e)
}

final class StoreImpl(val e: Expression)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Store {
  lazy val Seq(value: CHIR.Value, location: CHIR.Value) = mapOperands(e)
}

final class RawArrayLiteralInitImpl(val e: Expression)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.RawArrayLiteralInit {
  lazy val Seq(array: CHIR.Value, elementValues: _*) = mapOperands(e)
}

final class RawArrayInitByValueImpl(val e: Expression)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.RawArrayInitByValue {
  lazy val Seq(array: CHIR.Value, size: CHIR.Value, initValue: CHIR.Value) = mapOperands(e)
}

final class ConstantImpl(val e: Expression)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Constant {
  lazy val Seq(literal: CHIR.Value) = mapOperands(e)
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.resultLocalVar).get
}

final class TupleImpl(val e: Expression)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Tuple {
  def elementValues: Seq[CHIR.Value] = mapOperands(e)
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.resultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.resultLocalVar).get
}

final class GetRTTIImpl(val e: Expression) extends ExpressionImpl with CHIR.GetRTTI {
}

final class GetExceptionImpl(val e: Expression) extends ExpressionImpl with CHIR.GetException {
}

final class ExitImpl(val e: Expression) extends ExpressionImpl with CHIR.Exit {
}

trait ExpressionImpl extends CHIR.Expression {
  def e: Expression

  def debugLoc: Option[CHIR.DebugLocation] = {
    def getLine(b: Base)(posMapper: DebugLocation => Pos) = Option(b.loc).map(posMapper).map(_.line)
    
    val b = e.base
    getLine(b)(_.beginPos).zip(getLine(b)(_.endPos))
      .map((st, end) => CHIR.DebugLocation(st, end))
  }
}

private def mapOverflowStrategy(os: Int): CHIR.OverflowStrategy = os match {
  case OverflowStrategy.NA => CHIR.OverflowStrategy.Na
  case OverflowStrategy.CHECKED => CHIR.OverflowStrategy.Checked
  case OverflowStrategy.WRAPPING => CHIR.OverflowStrategy.Wrapping
  case OverflowStrategy.THROWING => CHIR.OverflowStrategy.Throwing
  case OverflowStrategy.SATURATING => CHIR.OverflowStrategy.Saturating
}

private def mapOperands(e: Expression)(implicit provider: CHIRItemProvider) = {
  e.operandsVector.toValueSeq[CHIR.Value]
}

private def takeLastTwoBlocks(operands: Seq[CHIR.Value]): Seq[CHIR.Block] = {
  operands.takeRight(2).collect {
    case t: CHIR.Block => t
  }.ensuring(_.size == 2)
}
