/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering.cbc

import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.Env.isStandalone
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.lowering.arch64.LoweringArch64
import com.huawei.excelsior.jet.compiler.opt.middle.Optimize
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.{RTSProc, symlevel}

trait LoweringCBC extends LoweringArch64 with PreLoweringCBC { self: Universe with Optimize =>

  override protected def MaxArrayFillSizeForSplitting = 10

  import LoweringKind.*
  override protected def shouldBeLoweredCases(node: Node) = node match {
    case x: PureCheck if x.trusted => super.shouldBeLoweredCases(x)
    case x: MathIntrinsic if procForMathIntrinsic(x).isDefined => super.shouldBeLoweredCases(node)
    case _: AcquireRawData | _: ReleaseRawData => super.shouldBeLoweredCases(node)
    case _: GetElementPtr => super.shouldBeLoweredCases(node)
    case _: ArrayFill => super.shouldBeLoweredCases(node)
    case _: SymbolAddress | _: InstanceDescriptorBy | _: ThisTypeInfo | _: ThisTypeInfoBy => super.shouldBeLoweredCases(node)
    case ValueConvert(F16, U32 | U64, _) | ValueConvert(U32 | U64, F16, _) => shouldNotReachHere(s"$node") // TODO debug
    case ValueConvert(F16, I32 | I64, _) | ValueConvert(I32 | I64, F16, _) => super.shouldBeLoweredCases(node)

    case _: NewStackAllocated => SPINAL

    case ValueConvert(F16, F64, _) | ValueConvert(F64, F16, _) => FLOATING

    case pf: PutJavaFieldOperation if pf.field.isAJFlat => SPINAL

    case _: NewArrayCopy => super.shouldBeLoweredCases(node)
    case _: NewArrayMimic | _: AJArrayFill => COMPLEX
    case _: AbstractNullCheck => super.shouldBeLoweredCases(node)

    case _: ErrorRTSCall | _: PreparationCheck => super.shouldBeLoweredCases(node)

    case d: Deprive if d.isLoweredWithWeakCast => COMPLEX
    case _: WeakCast => COMPLEX

    case _: TauSwitch => COMPLEX
    case n: InstanceOf if n.targetType.symKindErased.isClass => COMPLEX
    case n: BitcodeDeferred.InstanceOf if n.targetType.symKindErased.isClass => COMPLEX

    case e: Evacuate => COMPLEX

    case _: WriteBarrier | _: VerificationWriteBarrier => super.shouldBeLoweredCases(node)

    case _: Switch => COMPLEX

    case cs: CopyStructure if isStandalone && !cs.isPrimitive => COMPLEX

    case node @ IDivRemByConstOp(_) if !node.isDiv => FLOATING

    case _ => NONE // no lowering for CBC
  }

  override private [lowering] def procForMathIntrinsic(node: MathIntrinsic): Option[symlevel.Method] = {
    import Java.Lang.MathIntrinsic.*
    node.kind match {
      case D_SQRT => None
      case F_SQRT => None
      case D_ABS => None
      case F_ABS => None
      case _ => super.procForMathIntrinsic(node)
    }
  }

  override def copyRecord(t: SignatureType, dst: Node, src: Node): Unit = copyRecordByFields(t, dst, src)

  override def decomposeNode(node: Node): Node = node match {
    case x: PutJavaFieldOperation => lowerPutFlatField(x); null
    case x: InstanceOf => lowerClassInstanceOf(x)
    case x: BitcodeDeferred.InstanceOf => lowerClassInstanceOf(x)
    case e: Evacuate => lowerEvacuate(e)
    case node @ IDivRemByConstOp(_) => lowerIntegralRem(node)
    case _ => super.decomposeNode(node)
  }

  private def lowerClassInstanceOf(obj: Node, tpe: SignatureType): Node = {
    assert(tpe.isClass, s"$tpe")
    val nullExit = makeNullTest(obj)
    val instanceOf = ControlledInstanceOf(tpe)(obj)
    join(IConst(0) at nullExit, instanceOf at Goto())
  }

  private def lowerClassInstanceOf(x: BitcodeDeferred.InstanceOf): Node = lowerClassInstanceOf(x.obj, x.targetType)

  private def lowerClassInstanceOf(x: InstanceOf): Node = lowerClassInstanceOf(x.obj, x.targetType)

  private def lowerPutFlatField(pf: PutJavaFieldOperation): Unit = {
    assert(pf.field.isAJFlat)
    val fieldType = pf.field.getType
    val recordAddr = pf match {
      case _: PutStatic => GetStatic(pf.field)
      case pf: PutField => GetField(pf.field)(pf.obj)
    }
    copyRecord(fieldType, recordAddr, pf.inValue0)
  }

  override private[lowering] def lowerAcquireRawData(n: AcquireRawData) = {
    RTSCall(RTSProc.CJ_AcquireRawData)(n.array)
  }

  override private[lowering] def lowerReleaseRawData(n: ReleaseRawData) = {
    RTSCall(RTSProc.CJ_ReleaseRawData)(n.array, n.pointer)
  }

  override private [lowering] def lowerValueConvert(cast: ValueConvert): Node = (cast.fromAsm, cast.toAsm) match {
    case (F16, F64) =>
      ValueConvert(F32, F64)(ValueConvert(F16, F32)(cast.arg))
    case (F64, F16) =>
      ValueConvert(F32, F16)(ValueConvert(F64, F32)(cast.arg))

    case _ => super.lowerValueConvert(cast)
  }

  private def lowerIntegralRem(op: IDivRemOp): Node = {
    assert(!op.isDiv)
    val quotient = if (op.isUnsigned) {
      UDiv(op.tpe)(op.l, op.r)
    } else {
      IDiv(op.tpe)(op.l, op.r)
    }
    Sub(op.l, Mul(op.r, quotient))
  }

  override protected def useCacheForBackupWeakCast = true

  override def genWeakCast(obj: Node, itype: symlevel.Type): Node = InterfaceCastCBC(itype)(obj) // Unlike other platforms, CBC interpreter/jit should choose cache locations by themselves

  override def genWeakCastNoCache(obj: Node, itype: symlevel.Type): Node = shouldNotCallThis()

  override protected def genCheckRich(n: Node): (If, Node, Node) = {
    val enrichment = ExtractEnrichment(n)
    val checkRich = If(Cmp(AddrType, Condition.NE)(enrichment, addrNull))
    (checkRich, null, enrichment)
  }

  override def genExtractObject(rich: Node, bits: Node, enrichment: Node): Node = {
    // `MiscOps.lowerRawDeprive` uses `genExtractObject` itself,
    // but in case of CBC it won't lead to uncontrollable recursion,
    // since `RawDeprive` nodes aren't lowered.
    RawDeprive(rich)
  }

  override def genMakeCIAO(itype: symlevel.Type, plain: Node, enrichment: Node): Node = enrichment

  private def rewriteInterfaceChecks(typeCheck: AbstractTypeCheck): Node = {
    assert(typeCheck.targetType.isInterface)

    val itableOffset = genWeakCast(typeCheck.obj, typeCheck.targetType.symType)

    adjustDependingWeakCasts(typeCheck, itableOffset)

    itableOffset
  }

  override protected def lowerInterfaceCheckCast(checkCast: CheckCast): Unit = {
    val itableOffset = rewriteInterfaceChecks(checkCast)

    val itableBranch = ifAddrEq(itableOffset, IntegralConst(AddrIntType)(0))
    val castSucceed = itableBranch.falseExit
    val throwEdge = itableBranch.trueExit

    val (rtsProc, args) = checkCast.throwInfo
    coldBlockWithErrorRTSCallAndHalt(throwEdge)(checkCast, rtsProc, args*)
    continue(castSucceed)
  }

  override protected def lowerInterfaceInstanceOf(instanceOf: InstanceOf): Node = {
    val itableOffset = rewriteInterfaceChecks(instanceOf)

    CondVal(Cmp(AddrIntType, Condition.GE)(itableOffset, IntegralConst(AddrIntType)(0)))
  }

  override protected[lowering] def genTypeGuardTest(guard: TypeGuard, obj: Node) =
    TypeTest(guard, TauInfo.Unknown)(obj)

  override private[lowering] def lowerGetElementPtr(n: GetElementPtr) =
    Add(ReinterpretCast(n.base.tpe, AddrType)(n.base), IntegralConst(AddrType)(n.field.getInstanceFieldOffset))

  override private[lowering] def lowerNewStackAllocated(newOp: NewStackAllocated) = {
    val tpe = newOp.allocType
    assert(tpe.isClass)

    val sa = StackAlloc(FrameSlot.NewOnStack(tpe))

    InitObj(sa)
    PublishRef(sa)
  }

  override private[lowering] def lowerEvacuate(e: Evacuate): Node = {
    val objSrc = e.obj

    val nullBranch = makeNullTest(objSrc)
    val evacuateUnderNullTest = safeLoweredNodeClone(e)

    join(objSrc at nullBranch, evacuateUnderNullTest at Goto())
  }

  override private[lowering] def lowerThisTypeInfo(n: ThisTypeInfo): Node = ThisTypeInfoCBC(n.target)

  override private[lowering] def lowerThisTypeInfoBy(n: ThisTypeInfoBy): Node = ThisTypeInfoByCBC(n.obj)

  protected def clearHigh16Bits(node: Node): Node = node
}
