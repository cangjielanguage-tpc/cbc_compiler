package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.*
import com.huawei.excelsior.jet.compiler.chir.CHIR.{Binary, Intrinsic, Unary}
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v1203.CHIRUtils.{toTypeSeq, toValueSeq}

class AllocateImpl(val a: AllocateBaseT)(implicit val provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Allocate {
  def e: ExpressionT = a.getBase
  def allocatedType: CHIR.Type = provider.getType[CHIR.Type](a.getAllocatedType).get
}

final class TryAllocateImpl(allocation: AllocateBaseT)(implicit provider: CHIRItemProvider) extends AllocateImpl(allocation) with CHIR.TryAllocate {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(mapOperands(e))
}

class ApplyImpl(a: ApplyBaseT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Apply {
  private def fc = a.getBase
  def e: ExpressionT = fc.getBase
  def callee: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def args = mapOperands(e).drop(1)
  def thisType: Option[CHIR.Type] = provider.getType[CHIR.Type](fc.getObjType)
  def thisArg: CHIR.Value = args.head
  def instantiatedTypeArgs = fc.getInstantiatedTypeArgs.toTypeSeq[CHIR.Type]
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

final class TryApplyImpl(e: ApplyBaseT)(implicit provider: CHIRItemProvider) extends ApplyImpl(e) with CHIR.TryApply {
  override def args: Seq[CHIR.Value] = super.args.dropRight(2)
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(super.args)
}

class BinaryImpl(b: BinaryExpressionBaseT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Binary {
  def e = b.getBase
  def operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.size >= 2) // at least left and right operands should be here
  def kind: Binary.Kind = e.getKind match {
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
  def overflowStrategy: CHIR.OverflowStrategy = mapOverflowStrategy(b.getOverflowStrategy)
  def leftOperand: CHIR.Value = operands.head
  def rightOperand: CHIR.Value = operands(1)
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

final class TryBinaryImpl(e: BinaryExpressionBaseT)(implicit provider: CHIRItemProvider) extends BinaryImpl(e) with CHIR.TryBinary {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

final class BranchImpl(b: BranchT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Branch {
  def e: ExpressionT = b.getBase
  def condition: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def trueBlock: CHIR.Block = mapOperands(e).apply(1).asInstanceOf[CHIR.Block]
  def falseBlock: CHIR.Block = mapOperands(e).apply(2).asInstanceOf[CHIR.Block]
  def successors: Seq[CHIR.Block] = Seq(trueBlock, falseBlock)
}

final class DebugImpl(d: DebugT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Debug {
  def e: ExpressionT = d.getBase
}

final class FieldImpl(f: FieldT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Field {
  def e: ExpressionT = f.getBase
  def base: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def path: Seq[Long] = Option(f.getPath).toSeq.flatten
}

final class GetElementRefImpl(g: GetElementRefT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.GetElementRef {
  def e: ExpressionT = g.getBase
  def base: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def path: Seq[Long] = Option(g.getPath).toSeq.flatten
}

final class GetRTTIStaticImpl(g: GetRTTIStaticT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.GetRTTIStatic {
  def e: ExpressionT = g.getBase
  def rttiType: CHIR.Type = provider.getType[CHIR.Type](g.getRttiType).get
}

final class InstanceOfImpl(i: InstanceOfT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.InstanceOf {
  def e: ExpressionT = i.getBase
  def obj: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def testType: CHIR.Type = provider.getType[CHIR.Type](i.getTargetType).get
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

class IntrinsicImpl(in: IntrinsicBaseT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Intrinsic {
  def e = in.getBase.getBase
  private def operands: Seq[CHIR.Value] = mapOperands(e)
  def kind: Intrinsic.Kind = in.getIntrinsicKind match {
    case IntrinsicKind.ABS => Intrinsic.Kind.Abs
    case IntrinsicKind.FABS => Intrinsic.Kind.Fabs
    case IntrinsicKind.ARRAY_ACQUIRE_RAW_DATA => Intrinsic.Kind.ArrayAcquireRawData
    case IntrinsicKind.ARRAY_BUILT_IN_COPY_TO => Intrinsic.Kind.ArrayBuiltinCopyTo
    case IntrinsicKind.ARRAY_GET_UNCHECKED => Intrinsic.Kind.ArrayGetUnchecked
    case IntrinsicKind.ARRAY_GET_REF_UNCHECKED => Intrinsic.Kind.ArrayGetRefUnchecked
    case IntrinsicKind.ARRAY_GET => Intrinsic.Kind.ArrayGet
    case IntrinsicKind.VARRAY_GET => Intrinsic.Kind.VArrayGet
    case IntrinsicKind.VARRAY_SET => Intrinsic.Kind.VArraySet
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
    case IntrinsicKind.CPOINTER_INIT0 => Intrinsic.Kind.CPointerInit0
    case IntrinsicKind.CPOINTER_INIT1 => Intrinsic.Kind.CPointerInit1
    case IntrinsicKind.CPOINTER_READ => Intrinsic.Kind.CPointerRead
    case IntrinsicKind.CPOINTER_WRITE => Intrinsic.Kind.CPointerWrite
    case IntrinsicKind.OBJECT_ZERO_VALUE => Intrinsic.Kind.ObjectZeroValue
    case IntrinsicKind.SQRT => Intrinsic.Kind.Sqrt
    case IntrinsicKind.POW => Intrinsic.Kind.Pow
    case IntrinsicKind.SIN => Intrinsic.Kind.Sin
    case IntrinsicKind.COS => Intrinsic.Kind.Cos
  }
  def args: Seq[CHIR.Value] = operands
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

final class TryIntrinsicImpl(e: IntrinsicBaseT)(implicit provider: CHIRItemProvider) extends IntrinsicImpl(e) with CHIR.TryIntrinsic {
  override def args: Seq[CHIR.Value] = super.args.dropRight(2)
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(super.args)
}

class InvokeImpl(i: InvokeBaseT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Invoke {
  private def fc = i.getBase
  def e: ExpressionT = fc.getBase
  def callee: CHIR.FuncSig = FuncSigImpl(i.getVirMethodCtx)
  def args: Seq[CHIR.Value] = mapOperands(e)
  def thisType: CHIR.Type = provider.getType[CHIR.Type](fc.getObjType).get
  def thisArg: CHIR.Value = args.head
  def instantiatedTypeArgs = fc.getInstantiatedTypeArgs.toTypeSeq[CHIR.Type]
  def overflowStrategy = CHIR.OverflowStrategy.Na
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

final class TryInvokeImpl(e: InvokeBaseT)(implicit provider: CHIRItemProvider) extends InvokeImpl(e) with CHIR.TryInvoke {
  override def args: Seq[CHIR.Value] = super.args.dropRight(2)
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(super.args)
}

final class MultiBranchImpl(m: MultiBranchT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.MultiBranch {
  def e: ExpressionT = m.getBase
  def condition: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def defaultBlock: CHIR.Block = mapOperands(e).apply(1).asInstanceOf[CHIR.Block]
  def _normalBlocks = mapOperands(e).drop(2)
  def normalBlocks: Seq[CHIR.Block] = {
    val blocks = _normalBlocks
    blocks.collect {
      case t: CHIR.Block => t
    }.ensuring(_.size == blocks.size)
  }
  def caseValues: Seq[Long] = Option(m.getCaseValues).toSeq.flatten
  def successors: Seq[CHIR.Block] = defaultBlock +: normalBlocks
}

trait CastImpl extends ExpressionImpl with CHIR.Cast {
  implicit def provider: CHIRItemProvider
  def operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty)
  def value: CHIR.Value = operands.head
  def targetTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
}

class NumericCastImpl(n: NumericCastBaseT)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.NumericCast {
  def e: ExpressionT = n.getBase
  def overflowStrategy: CHIR.OverflowStrategy = mapOverflowStrategy(n.getOverflowStrategy)
}

final class TryNumericCastImpl(e: NumericCastBaseT)(implicit provider: CHIRItemProvider) extends NumericCastImpl(e) with CHIR.TryNumericCast {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

class RawArrayAllocateImpl(r: RawArrayAllocateBaseT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.RawArrayAllocate {
  def e: ExpressionT = r.getBase
  def operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty) // at least size should be here
  def elementType: CHIR.Type = provider.getType[CHIR.Type](r.getElementType).get
  def size: CHIR.Value = operands.head
}

final class TryRawArrayAllocateImpl(e: RawArrayAllocateBaseT)(implicit provider: CHIRItemProvider) extends RawArrayAllocateImpl(e) with CHIR.TryRawArrayAllocate {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

class SpawnImpl(s: SpawnBaseT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Spawn {
  def e: ExpressionT = s.getBase
  def operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty)  // at least obj should be here
  def obj: CHIR.Value = operands.head
  def executeClosure: Option[CHIR.Func] = provider.getValue[CHIR.Func](s.getExecuteClosure)
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

final class TrySpawnImpl(e: SpawnBaseT)(implicit provider: CHIRItemProvider) extends SpawnImpl(e) with CHIR.TrySpawn {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

final class StoreElementRefImpl(s: StoreElementRefT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.StoreElementRef {
  def e: ExpressionT = s.getBase
  def value: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def location: CHIR.Value = mapOperands(e).apply(1).asInstanceOf[CHIR.Value]
  def path: Seq[Long] = Option(s.getPath).toSeq.flatten
}

class UnaryImpl(u: UnaryExpressionBaseT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Unary {
  def e: ExpressionT = u.getBase
  def operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty) // at least operand should be here
  def operand: CHIR.Value = operands.head
  def kind: Unary.Kind = e.getKind match {
    case CHIRExprKind.BitNot => Unary.Kind.BitNot
    case CHIRExprKind.Not => Unary.Kind.Not
    case CHIRExprKind.Neg | CHIRExprKind.TryNeg => Unary.Kind.Neg
  }
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
  def overflowStrategy: CHIR.OverflowStrategy = mapOverflowStrategy(u.getOverflowStrategy)
}

final class TryUnaryImpl(e: UnaryExpressionBaseT)(implicit provider: CHIRItemProvider) extends UnaryImpl(e) with CHIR.TryUnary {
  def successors: Seq[CHIR.Block] = takeLastTwoBlocks(operands)
}

final class GotoImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Goto {
  def destination: CHIR.Block = mapOperands(e).apply(0).asInstanceOf[CHIR.Block]
  def successors: Seq[CHIR.Block] = Seq(destination)
}

final class RaiseExceptionImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.RaiseException {
  private def operands: Seq[CHIR.Value] = mapOperands(e).ensuring(_.nonEmpty)
  def exceptionValue: CHIR.Value = operands.head
  def exceptionBlock: Option[CHIR.Block] = operands.tail.headOption.collect {
    case t: CHIR.Block => t
  }
  def successors: Seq[CHIR.Block] = exceptionBlock.toSeq
}

final class StaticCastImpl(val e: ExpressionT)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.StaticCast {
}

final class BoxImpl(val e: ExpressionT)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.Box {
}

final class UnboxToRefImpl(val e: ExpressionT)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.UnboxToRef {
}

final class UnboxToValueImpl(val e: ExpressionT)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.UnboxToValue {
}

final class CastToConcreteImpl(val e: ExpressionT)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.CastToConcrete {
}

final class CastToGenericImpl(val e: ExpressionT)(implicit val provider: CHIRItemProvider) extends CastImpl with CHIR.CastToGeneric {
}

final class LoadImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Load {
  def location: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
}

final class StoreImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Store {
  def value: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def location: CHIR.Value = mapOperands(e).apply(1).asInstanceOf[CHIR.Value]
}

final class VArrayImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.VArray {
  def elementValues: Seq[CHIR.Value] = mapOperands(e)
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

final class VArrayBuilderImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.VArrayBuilder {
  def size: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def initValue: CHIR.Value = mapOperands(e).apply(1).asInstanceOf[CHIR.Value]
  def initializer: CHIR.Value = mapOperands(e).apply(2).asInstanceOf[CHIR.Value]
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

final class RawArrayLiteralInitImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.RawArrayLiteralInit {
  def array: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def elementValues = mapOperands(e).drop(1)
}

final class RawArrayInitByValueImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.RawArrayInitByValue {
  def array: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def size: CHIR.Value = mapOperands(e).apply(1).asInstanceOf[CHIR.Value]
  def initValue: CHIR.Value = mapOperands(e).apply(2).asInstanceOf[CHIR.Value]
}

final class ConstantImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Constant {
  def literal: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

final class TupleImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.Tuple {
  def elementValues: Seq[CHIR.Value] = mapOperands(e)
  def resultTpe: CHIR.Type = provider.getType[CHIR.Type](e.getResultTy).get
  def resultVar: CHIR.LocalVar = provider.getValue[CHIR.LocalVar](e.getResultLocalVar).get
}

final class GetRTTIImpl(val e: ExpressionT)(implicit provider: CHIRItemProvider) extends ExpressionImpl with CHIR.GetRTTI {
  def obj: CHIR.Value = mapOperands(e).apply(0).asInstanceOf[CHIR.Value]
}

final class GetExceptionImpl(val e: ExpressionT) extends ExpressionImpl with CHIR.GetException {
}

final class ExitImpl(val e: ExpressionT) extends ExpressionImpl with CHIR.Exit {
}

trait ExpressionImpl extends CHIR.Expression {
  def e: ExpressionT
  def debugLoc: Option[CHIR.DebugLocation] = PackageImpl.location(e)

}
private def mapOverflowStrategy(os: Int): CHIR.OverflowStrategy = os match {
  case OverflowStrategy.NA => CHIR.OverflowStrategy.Na
  case OverflowStrategy.CHECKED => CHIR.OverflowStrategy.Checked
  case OverflowStrategy.WRAPPING => CHIR.OverflowStrategy.Wrapping
  case OverflowStrategy.THROWING => CHIR.OverflowStrategy.Throwing
  case OverflowStrategy.SATURATING => CHIR.OverflowStrategy.Saturating
}
private def mapOperands(e: ExpressionT)(implicit provider: CHIRItemProvider) = {
  e.getOperands.toValueSeq[CHIR.Value]
}
private def takeLastTwoBlocks(operands: Seq[CHIR.Value]): Seq[CHIR.Block] = {
  operands.takeRight(2).collect {
    case t: CHIR.Block => t
  }.ensuring(_.size == 2)
}
