/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.model
import com.huawei.excelsior.jet.compiler.chir.model.*
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.{
  AllocateBaseT, AnnoInfoT, Annotation as FAnnotation, AnnotationUnion, ApplyBaseT, BaseT,
  BinaryExpressionBaseT, BlockGroupT, BlockT, BoolLiteralT, BranchT, CHIRPackageT, CHIRExprKind,
  CHIRTypeKind, ClassDefT, ConstantValueKind, CustomAnnoInstanceT, CustomTypeDefElem, CustomTypeDefT,
  CustomTypeT, DebugLocationT, DebugT, EnumCaseIndexT, EnumCtorInfoT, EnumDefT, ExpressionElem,
  ExpressionT, ExtendDefT, FieldByNameT, FieldT, FloatLiteralT, FuncCallT, FuncSigInfoT, FuncTypeT,
  FunctionT, GeneratedFromForInT, GenericTypeT, GetElementByNameT, GetElementRefT, GetInstantiateValueT,
  GetRTTIStaticT, GlobalValueT, GlobalVarT, InstanceOfT, IntLiteralT, IntrinsicBaseT, InvokeBaseT,
  IsAutoEnvClassT, IsCapturedClassInCCT, LambdaT, LinkTypeInfoT, LiteralValueT, LocalVarT,
  MemberVarInfoT, MultiBranchT, NeedCheckArrayBoundT, NeedCheckCastT, NeverOverflowInfoT, NullLiteralT,
  NumericCastBaseT, OverrideSrcFuncTypeT, ParameterT, PosT, RawArrayAllocateBaseT, RawArrayTypeT,
  RuneLiteralT, SkipCheckT, SpawnBaseT, StoreElementByNameT, StoreElementRefT, StringLiteralT,
  StructDefT, TypeElem, TypeT, UnaryExpressionBaseT, UnitLiteralT, ValueElem, ValueT,
  VArrayTypeT, VirMethodOffsetT, VirtualMethodInfoT, VTableInTypeT, WrappedRawMethodT,
}
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.TypeElemUnion
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.ValueElemUnion
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.ExpressionElemUnion
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.CustomTypeDefElemUnion
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.AnnotationUnion

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer

/** Serializes a mutable [[model.Package]] into the v1203 .chir format.
  *
  * Port of the C++ `CHIRSerializer::CHIRSerializerImpl` (worklist discovery-order
  * id assignment over the flatbuffer Object API T-classes):
  *  - `Initialize` seeds the value queue with the package global vars (in file order)
  *    followed by global functions, and the def queue with all custom type defs
  *    (imported first: structs, classes, enums, extends; then current package);
  *    def ids are allocated first, then the seeded value ids.
  *  - `Dispatch` drains the queues (types, then values, then expressions, then defs),
  *    assigning 1-based ids on first encounter and recursively enqueueing referenced
  *    entities — mirroring the C++ BFS worklist exactly, including the drain order.
  *  - The root `CHIRPackage` is written with `path = ""` and the `phase` field.
  *
  * NOTE: keep structurally in sync with the other version's serializer and
  * Materializer1203 (schema deltas: see the model PackageFormat.fbs files).
  */
object Serializer1203 {

  /** Serializes `pkg` and writes the bytes to `filename`. */
  def serialize(pkg: model.Package, filename: String, phase: model.Phase): Unit =
    Files.write(Paths.get(filename), toBytes(pkg, phase))

  /** Serializes `pkg` into .chir bytes. */
  def toBytes(pkg: model.Package, phase: model.Phase): Array[Byte] =
    new Impl(pkg).serialize(phase)

  private final class Impl(pkg: model.Package) {

    // identity-keyed id maps (C++ uses pointer identity)
    private val type2Id = new java.util.IdentityHashMap[AnyRef, Integer]
    private val value2Id = new java.util.IdentityHashMap[AnyRef, Integer]
    private val expr2Id = new java.util.IdentityHashMap[AnyRef, Integer]
    private val def2Id = new java.util.IdentityHashMap[AnyRef, Integer]

    // FIFO worklists, drained with a cursor (entries are appended during dispatch)
    private val typeQueue = ArrayBuffer[model.Type]()
    private val valueQueue = ArrayBuffer[model.Value]()
    private val exprQueue = ArrayBuffer[model.Expression]()
    private val defQueue = ArrayBuffer[model.CustomTypeDef]()
    private var typeHead = 0
    private var valueHead = 0
    private var exprHead = 0
    private var defHead = 0

    // union element arrays indexed by (id - 1); placeholders are filled by dispatch
    private val allType = ArrayBuffer[TypeElemUnion]()
    private val allValue = ArrayBuffer[ValueElemUnion]()
    private val allExpression = ArrayBuffer[ExpressionElemUnion]()
    private val allCustomTypeDef = ArrayBuffer[CustomTypeDefElemUnion]()

    private var typeCount = 0
    private var valueCount = 0
    private var exprCount = 0
    private var defCount = 0

    // =================================================================================
    // entry point
    // =================================================================================

    def serialize(phase: model.Phase): Array[Byte] = {
      initialize()
      dispatch()
      val packageInitFunc = valueId(pkg.packageInitFunc.orNull)
      val packageLiteralInitFunc = valueId(pkg.packageLiteralInitFunc.orNull)

      val root = new CHIRPackageT()
      root.setName(pkg.name)
      root.setPath("")
      root.setPkgAccessLevel(pkg.accessLevel.ordinal)
      root.setTypes(allType.toArray)
      root.setValues(allValue.toArray)
      root.setExprs(allExpression.toArray)
      root.setDefs(allCustomTypeDef.toArray)
      root.setPackageInitFunc(packageInitFunc)
      root.setPackageLiteralInitFunc(packageLiteralInitFunc)
      root.setPhase(phase.ordinal)
      root.serializeToBinary()
    }

    /** C++ `CHIRSerializerImpl::Initialize`. */
    private def initialize(): Unit = {
      pkg.globalVars.foreach(valueQueue += _)
      pkg.globalFuncs.foreach(valueQueue += _)
      pkg.allCustomTypeDefs.foreach(defQueue += _)

      // allocate def ids earlier
      for (d <- defQueue.toList) {
        defCount += 1
        def2Id.put(d, Integer.valueOf(defCount))
        allCustomTypeDef += null

      }
      // allocate value ids earlier
      for (v <- valueQueue.toList) {
        valueCount += 1
        value2Id.put(v, Integer.valueOf(valueCount))
        allValue += null
      }
    }

    /** C++ `CHIRSerializerImpl::Dispatch` (the drain loop). */
    private def dispatch(): Unit = {
      while (typeHead < typeQueue.length || valueHead < valueQueue.length ||
          exprHead < exprQueue.length || defHead < defQueue.length) {
        while (typeHead < typeQueue.length) {
          val t = typeQueue(typeHead)
          typeHead += 1
          allType(typeId(t).toInt - 1) = dispatchType(t)
        }
        while (valueHead < valueQueue.length) {
          val v = valueQueue(valueHead)
          valueHead += 1
          allValue(valueId(v).toInt - 1) = dispatchValue(v)
        }
        while (exprHead < exprQueue.length) {
          val e = exprQueue(exprHead)
          exprHead += 1
          allExpression(exprId(e).toInt - 1) = dispatchExpr(e)
        }
        while (defHead < defQueue.length) {
          val d = defQueue(defHead)
          defHead += 1
          allCustomTypeDef(defId(d).toInt - 1) = dispatchDef(d)
        }
      }
    }

    // =================================================================================
    // id fetchers (assign on first encounter, enqueue for dispatch)
    // =================================================================================

    private def typeId(t: model.Type): Long =
      if (t == null) 0L
      else if (type2Id.containsKey(t)) type2Id.get(t).longValue()
      else {
        typeCount += 1
        type2Id.put(t, Integer.valueOf(typeCount))
        allType += null
        typeQueue += t
        typeCount.toLong
      }
    private def valueId(v: model.Value): Long =
      if (v == null) 0L
      else if (value2Id.containsKey(v)) value2Id.get(v).longValue()
      else {
        valueCount += 1
        value2Id.put(v, Integer.valueOf(valueCount))
        allValue += null
        valueQueue += v
        valueCount.toLong
      }
    private def exprId(e: model.Expression): Long =
      if (e == null) 0L
      else if (expr2Id.containsKey(e)) expr2Id.get(e).longValue()
      else {
        exprCount += 1
        expr2Id.put(e, Integer.valueOf(exprCount))
        allExpression += null
        exprQueue += e
        exprCount.toLong
      }
    private def defId(d: model.CustomTypeDef): Long =
      if (d == null) 0L
      else if (def2Id.containsKey(d)) def2Id.get(d).longValue()
      else {
        defCount += 1
        def2Id.put(d, Integer.valueOf(defCount))
        allCustomTypeDef += null
        defQueue += d
        defCount.toLong
      }
    private def typeIds(ts: scala.collection.Seq[model.Type]): Array[Long] =
      if (ts.isEmpty) null else ts.map(typeId).toArray

    private def valueIds(vs: scala.collection.Seq[model.Value]): Array[Long] =
      if (vs.isEmpty) null else vs.map(valueId).toArray

    private def exprIds(es: scala.collection.Seq[model.Expression]): Array[Long] =
      if (es.isEmpty) null else es.map(exprId).toArray

    // =================================================================================
    // type dispatch / serialization
    // =================================================================================

    private def dispatchType(t: model.Type): TypeElemUnion = {
      val (tag, obj) = t match {
        case t: FuncType =>
          val base = new TypeT()
          base.setKind(CHIRTypeKind.FUNC)
          base.setArgTys(typeIds(t.argTys))
          val u = new FuncTypeT()
          u.setBase(base)
          u.setIsCfuncType(t.isCFuncType)
          u.setHasVarArg(t.hasVarArg)
          (TypeElem.FuncType, u)
        case t: RawArrayType =>
          val base = new TypeT()
          base.setKind(CHIRTypeKind.RAWARRAY)
          base.setArgTys(Array(typeId(t.elementType)))
          val u = new RawArrayTypeT()
          u.setBase(base)
          u.setDims(t.dims)
          (TypeElem.RawArrayType, u)
        case t: VArrayType =>
          val base = new TypeT()
          base.setKind(CHIRTypeKind.VARRAY)
          base.setArgTys(Array(typeId(t.elementType)))
          val u = new VArrayTypeT()
          u.setBase(base)
          u.setSize(t.size)
          (TypeElem.VArrayType, u)
        case t: CustomType =>
          val base = new TypeT()
          base.setKind(t.customTypeDef match {
            case Some(d) => d.kind match {
              case CustomDefKind.Struct => CHIRTypeKind.STRUCT
              case CustomDefKind.Enum   => CHIRTypeKind.ENUM
              case CustomDefKind.Class  => CHIRTypeKind.CLASS
              case k => sys.error(s"Unexpected CustomType def kind $k")
            }
            case None => sys.error("CustomType without custom type def cannot be serialized")
          })
          base.setArgTys(typeIds(t.argTys))
          val u = new CustomTypeT()
          u.setBase(base)
          u.setCustomTypeDef(t.customTypeDef.map(defId).getOrElse(0L))
          (TypeElem.CustomType, u)
        case t: GenericType =>
          val base = new TypeT()
          base.setKind(CHIRTypeKind.GENERIC)
          val u = new GenericTypeT()
          u.setBase(base)
          u.setIdentifier(t.identifier)
          u.setSrcCodeIdentifier(t.srcCodeIdentifier)
          u.setUpperBounds(typeIds(t.upperBounds))
          (TypeElem.GenericType, u)
        case t: TypeImpl =>
          val u = new TypeT()
          u.setKind(t.kind.ordinal)
          u.setArgTys(typeIds(t.argTys))
          (TypeElem.Type, u)
      }
      typeUnion(tag, obj)
    }

    // =================================================================================
    // value dispatch / serialization
    // =================================================================================

    private def dispatchValue(v: model.Value): ValueElemUnion = {
      val (tag, obj) = v match {
        case v: BoolLiteral =>
          val u = new BoolLiteralT()
          u.setBase(literalValueT(v, ConstantValueKind.BOOL))
          u.setVal(v.value)
          (ValueElem.BoolLiteral, u)
        case v: RuneLiteral =>
          val u = new RuneLiteralT()
          u.setBase(literalValueT(v, ConstantValueKind.RUNE))
          u.setVal(v.value)
          (ValueElem.RuneLiteral, u)
        case v: StringLiteral =>
          val u = new StringLiteralT()
          u.setBase(literalValueT(v, ConstantValueKind.STRING))
          u.setVal(v.value)
          (ValueElem.StringLiteral, u)
        case v: IntLiteral =>
          val u = new IntLiteralT()
          u.setBase(literalValueT(v, ConstantValueKind.INT))
          u.setVal(v.value)
          (ValueElem.IntLiteral, u)
        case v: FloatLiteral =>
          val u = new FloatLiteralT()
          u.setBase(literalValueT(v, ConstantValueKind.FLOAT))
          u.setVal(v.value)
          (ValueElem.FloatLiteral, u)
        case v: UnitLiteral =>
          val u = new UnitLiteralT()
          u.setBase(literalValueT(v, ConstantValueKind.UNIT))
          (ValueElem.UnitLiteral, u)
        case v: NullLiteral =>
          val u = new NullLiteralT()
          u.setBase(literalValueT(v, ConstantValueKind.NULL))
          (ValueElem.NullLiteral, u)
        case p: Parameter =>
          val u = new ParameterT()
          u.setBase(valueT(p))
          u.setOwnedFunc(p.ownedFunc.map(valueId).getOrElse(0L))
          u.setOwnedLambda(p.ownedLambda.map(exprId).getOrElse(0L))
          u.setSrcCodeIdentifier(p.srcCodeIdentifier)
          u.setAnnoInfo(annoInfoT(p.annoInfo))
          (ValueElem.Parameter, u)
        case v: LocalVar =>
          val u = new LocalVarT()
          u.setBase(valueT(v))
          u.setAssociatedExpr(v.associatedExpr.map(exprId).getOrElse(0L))
          u.setIsRetVal(v.isRetVal)
          u.setSrcCodeIdentifier(v.srcCodeIdentifier)
          (ValueElem.LocalVar, u)
        case v: GlobalVar =>
          val u = new GlobalVarT()
          u.setBase(globalValueT(v))
          u.setInitializer(v.initializer.map(valueId).getOrElse(0L))
          (ValueElem.GlobalVar, u)
        case f: Func =>
          (ValueElem.Function, functionT(f))
        case b: Block =>
          val u = new BlockT()
          u.setBase(valueT(b))
          u.setParentGroup(b.parentGroup.map(valueId).getOrElse(0L))
          u.setExprs(exprIds(b.exprs))
          u.setPredecessors(valueIds(b.predecessors))
          u.setIsLandingPadBlock(b.isLandingPadBlock)
          u.setExceptionCatchList(
            if (b.isLandingPadBlock) typeIds(b.exceptionCatchList) else null)
          (ValueElem.Block, u)
        case v: BlockGroup =>
          val u = new BlockGroupT()
          u.setBase(valueT(v))
          u.setEntryBlock(v.entryBlock.map(valueId).getOrElse(0L))
          u.setBlocks(valueIds(v.blocks))
          u.setOwnedFunc(v.ownedFunc.map(valueId).getOrElse(0L))
          u.setOwnedExpression(v.ownedExpression.map(exprId).getOrElse(0L))
          (ValueElem.BlockGroup, u)
      }
      valueUnion(tag, obj)
    }

    private def literalValueT(v: ValueBase, kind: Int): LiteralValueT = {
      val u = new LiteralValueT()
      u.setBase(valueT(v))
      u.setLiteralKind(kind)
      u
    }

    private def valueT(v: ValueBase): ValueT = {
      val u = new ValueT()
      u.setBase(baseT(v.annos, v.loc, v.attributes))
      u.setType(v.tpe.map(typeId).getOrElse(0L))
      u.setIdentifier(v.identifier)
      u.setKind(v.kind.ordinal)
      u
    }

    private def globalValueT(v: GlobalValueBase): GlobalValueT = {
      val u = new GlobalValueT()
      u.setBase(valueT(v))
      u.setSrcCodeIdentifier(v.srcCodeIdentifier)
      u.setRawMangledName(v.rawMangledName)
      u.setPackageName(v.packageName)
      u.setDeclaredParent(v.declaredParent.map(defId).getOrElse(0L))
      if (v.features.nonEmpty) u.setFeatures(v.features.toArray)
      u.setAnnoInfo(annoInfoT(v.annoInfo))
      u
    }

    private def functionT(f: Func): FunctionT = {
      val u = new FunctionT()
      u.setBase(globalValueT(f))
      // skip serializing genericDecl when it is an imported func (no body)
      u.setGenericDecl(f.genericDecl match {
        case Some(g: Func) if g.body.isDefined => valueId(g)
        case _ => 0L
      })
      u.setFuncKind(f.funcKind.ordinal)
      u.setIsFastNative(f.isFastNative)
      u.setIsCffiwrapper(f.isCFFIWrapper)
      f.originalLambdaInfo.filter(_ => f.isLambda).filter(_.funcType != null).foreach { si =>
        u.setOriginalLambdaInfo(funcSigInfoT(si))
      }
      u.setGenericTypeParams(typeIds(f.genericTypeParams))
      u.setParamDftValHostFunc(f.paramDftValHostFunc.map(valueId).getOrElse(0L))
      u.setParams(valueIds(f.params))
      if (f.body.isDefined) {
        u.setBody(f.body.map(valueId).get)
        u.setRetVal(f.retVal.map(valueId).getOrElse(0L))
      }
      u.setPropLoc(debugLocationT(f.propLoc))
      u.setLocalId(f.localId)
      u.setBlockId(f.blockId)
      u.setBlockGroupId(f.blockGroupId)
      u
    }

    private def funcSigInfoT(si: FuncSigInfo): FuncSigInfoT = {
      val u = new FuncSigInfoT()
      u.setFuncName(si.funcName)
      u.setFuncType(typeId(si.funcType))
      u.setGenericTypeParams(typeIds(si.genericTypeParams))
      u
    }

    // =================================================================================
    // expression dispatch / serialization
    // =================================================================================

    private def dispatchExpr(e: model.Expression): ExpressionElemUnion = {
      val (tag, obj) = e match {
        case e: GotoExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.Goto, Seq(valueId(e.destination))))
        case e: ExitExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.Exit, Seq.empty))
        case e: RaiseExceptionExpr =>
          val ops = Seq(valueId(e.exceptionValue)) ++ e.exceptionBlock.map(valueId).toSeq
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.RaiseException, ops))
        case e: LoadExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.Load, Seq(valueId(e.location))))
        case e: StoreExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.Store, Seq(valueId(e.value), valueId(e.location))))
        case e: ConstantExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.Constant, Seq(valueId(e.literal))))
        case e: TupleExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.Tuple, e.elementValues.map(valueId)))
        case e: GetExceptionExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.GetException, Seq.empty))
        case e: GetRTTIExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.GetRtti, Seq(valueId(e.obj))))
        case e: VArrayExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.VArrayExpr, e.elementValues.map(valueId)))
        case e: VArrayBuilderExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.VArrayBuilder,
            Seq(valueId(e.size), valueId(e.initValue), valueId(e.initializer))))
        case e: RawArrayLiteralInitExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.RawArrayLiteralInit,
            valueId(e.array) +: e.elementValues.map(valueId)))
        case e: RawArrayInitByValueExpr =>
          (ExpressionElem.Expression, exprT(e, CHIRExprKind.RawArrayInitByValue,
            Seq(valueId(e.array), valueId(e.size), valueId(e.initValue))))
        case e: CastExpr =>
          val kind = e.op match {
            case CastOp.StaticCast     => CHIRExprKind.StaticCast
            case CastOp.Box            => CHIRExprKind.Box
            case CastOp.UnboxToValue   => CHIRExprKind.UnboxToValue
            case CastOp.UnboxToRef     => CHIRExprKind.UnboxToRef
            case CastOp.CastToConcrete => CHIRExprKind.CastToConcrete
            case CastOp.CastToGeneric  => CHIRExprKind.CastToGeneric
          }
          (ExpressionElem.Expression, exprT(e, kind, Seq(valueId(e.value))))
        case e: AllocateExpr =>
          val u = new AllocateBaseT()
          u.setBase(exprT(e, tryKind(e.isTry, CHIRExprKind.Allocate, CHIRExprKind.TryAllocate),
            if (e.isTry) e.handlerBlocks.map(valueId) else Seq.empty))
          u.setAllocatedType(typeId(e.allocatedType))
          (ExpressionElem.AllocateBase, u)
        case e: UnaryExpr =>
          val kind = e.op match {
            case UnaryOp.Neg    => tryKind(e.isTry, CHIRExprKind.Neg, CHIRExprKind.TryNeg)
            case UnaryOp.Not    => CHIRExprKind.Not
            case UnaryOp.BitNot => CHIRExprKind.BitNot
          }
          val ops = Seq(valueId(e.operand)) ++ (if (e.isTry) e.handlerBlocks.map(valueId) else Seq.empty)
          val u = new UnaryExpressionBaseT()
          u.setBase(exprT(e, kind, ops))
          u.setOverflowStrategy(ofsWire(e.overflowStrategy))
          (ExpressionElem.UnaryExpressionBase, u)
        case e: BinaryExpr =>
          val kind = e.op match {
            case BinaryOp.Add    => tryKind(e.isTry, CHIRExprKind.Add, CHIRExprKind.TryAdd)
            case BinaryOp.Sub    => tryKind(e.isTry, CHIRExprKind.Sub, CHIRExprKind.TrySub)
            case BinaryOp.Mul    => tryKind(e.isTry, CHIRExprKind.Mul, CHIRExprKind.TryMul)
            case BinaryOp.Div    => tryKind(e.isTry, CHIRExprKind.Div, CHIRExprKind.TryDiv)
            case BinaryOp.Mod    => tryKind(e.isTry, CHIRExprKind.Mod, CHIRExprKind.TryMod)
            case BinaryOp.Exp    => tryKind(e.isTry, CHIRExprKind.Exp, CHIRExprKind.TryExp)
            case BinaryOp.LShift => tryKind(e.isTry, CHIRExprKind.LShift, CHIRExprKind.TryLShift)
            case BinaryOp.RShift => tryKind(e.isTry, CHIRExprKind.RShift, CHIRExprKind.TryRShift)
            case BinaryOp.And    => CHIRExprKind.BitAnd
            case BinaryOp.Or     => CHIRExprKind.BitOr
            case BinaryOp.Xor    => CHIRExprKind.BitXor
            case BinaryOp.Lt     => CHIRExprKind.LT
            case BinaryOp.Gt     => CHIRExprKind.GT
            case BinaryOp.Le     => CHIRExprKind.LE
            case BinaryOp.Ge     => CHIRExprKind.GE
            case BinaryOp.Eq     => CHIRExprKind.Equal
            case BinaryOp.NotEq  => CHIRExprKind.NotEqual
          }
          val ops = Seq(valueId(e.leftOperand), valueId(e.rightOperand)) ++
            (if (e.isTry) e.handlerBlocks.map(valueId) else Seq.empty)
          val u = new BinaryExpressionBaseT()
          u.setBase(exprT(e, kind, ops))
          u.setOverflowStrategy(ofsWire(e.overflowStrategy))
          (ExpressionElem.BinaryExpressionBase, u)
        case e: ApplyExpr =>
          val ops = Seq(valueId(e.callee)) ++ e.args.map(valueId) ++
            (if (e.isTry) e.handlerBlocks.map(valueId) else Seq.empty)
          val fc = funcCallT(e, tryKind(e.isTry, CHIRExprKind.Apply, CHIRExprKind.TryApply), ops)
          val u = new ApplyBaseT()
          u.setBase(fc)
          u.setIsSuperCall(!e.isTry && e.isSuperCall)
          (ExpressionElem.ApplyBase, u)
        case e: InvokeExpr =>
          val ops = Seq(valueId(e.callee), valueId(e.caller)) ++ e.args.map(valueId) ++
            (if (e.isTry) e.handlerBlocks.map(valueId) else Seq.empty)
          val fc = funcCallT(e, tryKind(e.isTry, CHIRExprKind.Invoke, CHIRExprKind.TryInvoke), ops)
          val u = new InvokeBaseT()
          u.setBase(fc)
          // v1203 InvokeBase carries the virtual-method context instead of overflowStrategy
          e.virMethodCtx.foreach(ctx => u.setVirMethodCtx(funcSigInfoT(ctx)))
          (ExpressionElem.InvokeBase, u)
        case e: IntrinsicExpr =>
          // IntrinsicBase writes funcCall with thisType = 0 (mirrors the C++ serializer)
          val ops = e.args.map(valueId) ++
            (if (e.isTry) e.handlerBlocks.map(valueId) else Seq.empty)
          val fc = new FuncCallT()
          fc.setBase(exprT(e, tryKind(e.isTry, CHIRExprKind.Intrinsic, CHIRExprKind.TryIntrinsic), ops))
          fc.setInstantiatedTypeArgs(typeIds(e.instantiatedTypeArgs))
          fc.setObjType(0L)
          val u = new IntrinsicBaseT()
          u.setBase(fc)
          u.setIntrinsicKind(e.intrinsicKind)
          (ExpressionElem.IntrinsicBase, u)
        case e: NumericCastExpr =>
          val ops = Seq(valueId(e.value)) ++ (if (e.isTry) e.handlerBlocks.map(valueId) else Seq.empty)
          val u = new NumericCastBaseT()
          u.setBase(exprT(e, tryKind(e.isTry, CHIRExprKind.NumericCast, CHIRExprKind.TryNumericCast), ops))
          u.setOverflowStrategy(ofsWire(e.overflowStrategy))
          (ExpressionElem.NumericCastBase, u)
        case e: RawArrayAllocateExpr =>
          val ops = Seq(valueId(e.size)) ++ (if (e.isTry) e.handlerBlocks.map(valueId) else Seq.empty)
          val u = new RawArrayAllocateBaseT()
          u.setBase(exprT(e, tryKind(e.isTry, CHIRExprKind.RawArrayAllocate, CHIRExprKind.TryRawArrayAllocate), ops))
          u.setElementType(typeId(e.elementType))
          (ExpressionElem.RawArrayAllocateBase, u)
        case e: SpawnExpr =>
          val ops = Seq(valueId(e.obj)) ++ e.spawnArg.map(valueId).toSeq ++
            (if (e.isTry) e.handlerBlocks.map(valueId) else Seq.empty)
          val u = new SpawnBaseT()
          u.setBase(exprT(e, tryKind(e.isTry, CHIRExprKind.Spawn, CHIRExprKind.TrySpawn), ops))
          u.setExecuteClosure(e.executeClosure.map(valueId).getOrElse(0L))
          (ExpressionElem.SpawnBase, u)
        case e: BranchExpr =>
          val u = new BranchT()
          u.setBase(exprT(e, CHIRExprKind.Branch,
            Seq(valueId(e.condition), valueId(e.trueBlock), valueId(e.falseBlock))))
          u.setSourceExpr(e.sourceExpr)
          (ExpressionElem.Branch, u)
        case e: MultiBranchExpr =>
          val ops = Seq(valueId(e.condition), valueId(e.defaultBlock)) ++ e.normalBlocks.map(valueId)
          val u = new MultiBranchT()
          u.setBase(exprT(e, CHIRExprKind.MultiBranch, ops))
          if (e.caseValues.nonEmpty) u.setCaseValues(e.caseValues.toArray)
          (ExpressionElem.MultiBranch, u)
        case e: GetElementRefExpr =>
          val u = new GetElementRefT()
          u.setBase(exprT(e, CHIRExprKind.GetElementRef, Seq(valueId(e.base))))
          if (e.path.nonEmpty) u.setPath(e.path.toArray)
          (ExpressionElem.GetElementRef, u)
        case e: GetElementByNameExpr =>
          val u = new GetElementByNameT()
          u.setBase(exprT(e, CHIRExprKind.GetElementByName, Seq(valueId(e.base))))
          u.setFieldNames(e.fieldNames.toArray)
          (ExpressionElem.GetElementByName, u)
        case e: StoreElementRefExpr =>
          val u = new StoreElementRefT()
          u.setBase(exprT(e, CHIRExprKind.StoreElementRef, Seq(valueId(e.value), valueId(e.location))))
          if (e.path.nonEmpty) u.setPath(e.path.toArray)
          (ExpressionElem.StoreElementRef, u)
        case e: StoreElementByNameExpr =>
          val u = new StoreElementByNameT()
          u.setBase(exprT(e, CHIRExprKind.StoreElementByName, Seq(valueId(e.value), valueId(e.location))))
          u.setFieldNames(e.fieldNames.toArray)
          (ExpressionElem.StoreElementByName, u)
        case e: FieldExpr =>
          val u = new FieldT()
          u.setBase(exprT(e, CHIRExprKind.Field, Seq(valueId(e.base))))
          if (e.path.nonEmpty) u.setPath(e.path.toArray)
          (ExpressionElem.Field, u)
        case e: FieldByNameExpr =>
          val u = new FieldByNameT()
          u.setBase(exprT(e, CHIRExprKind.FieldByName, Seq(valueId(e.base))))
          u.setFieldNames(e.fieldNames.toArray)
          (ExpressionElem.FieldByName, u)
        case e: InstanceOfExpr =>
          val u = new InstanceOfT()
          u.setBase(exprT(e, CHIRExprKind.InstanceOf, Seq(valueId(e.obj))))
          u.setTargetType(typeId(e.targetType))
          (ExpressionElem.InstanceOf, u)
        case e: GetInstantiateValueExpr =>
          val u = new GetInstantiateValueT()
          u.setBase(exprT(e, CHIRExprKind.GetInstantiateValue, Seq(valueId(e.instVal))))
          u.setInstantiateTypes(typeIds(e.instantiateTypes))
          (ExpressionElem.GetInstantiateValue, u)
        case e: GetRTTIStaticExpr =>
          val u = new GetRTTIStaticT()
          u.setBase(exprT(e, CHIRExprKind.GetRttiStatic, Seq.empty))
          u.setRttiType(typeId(e.rttiType))
          (ExpressionElem.GetRTTIStatic, u)
        case e: DebugExpr =>
          val u = new DebugT()
          u.setBase(exprT(e, CHIRExprKind.Debug, Seq(valueId(e.value))))
          u.setSrcCodeName(e.srcCodeName)
          (ExpressionElem.Debug, u)
        case e: LambdaExpr =>
          val u = new LambdaT()
          u.setBase(exprT(e, CHIRExprKind.Lambda, Seq.empty))
          u.setFuncTy(typeId(e.funcTy))
          u.setIsLocalFunc(e.isLocalFunc)
          u.setIdentifier(e.identifier)
          u.setSrcCodeName(e.srcCodeName)
          u.setParams(valueIds(e.params))
          u.setGenericTypeParams(typeIds(e.genericTypeParams))
          u.setBody(e.body.map(valueId).getOrElse(0L))
          u.setRetVal(e.retVal.map(valueId).getOrElse(0L))
          u.setIsCompileTimeValue(e.isCompileTimeValue)
          (ExpressionElem.Lambda, u)
      }
      exprUnion(tag, obj)
    }

    /** Common flatbuffer `Expression` table part (mirrors C++ Serialize(Expression)). */
    private def exprT(e: model.Expression, kind: Int, operands: scala.collection.Seq[Long]): ExpressionT = {
      val u = new ExpressionT()
      u.setBase(baseT(e.annos, e.loc, e.attributes))
      u.setKind(kind)
      if (operands.nonEmpty) u.setOperands(operands.toArray)
      if (e.blockGroups.nonEmpty) u.setBlockGroups(e.blockGroups.map(valueId).toArray)
      u.setOwner(e.owner.map(valueId).getOrElse(0L))
      u.setResultLocalVar(e.resultVar.map(valueId).getOrElse(0L))
      u.setResultTy(e.resultVar.flatMap(_.tpe).map(typeId).getOrElse(0L))
      u
    }

    private def funcCallT(e: FuncCallExpr, kind: Int, operands: scala.collection.Seq[Long]): FuncCallT = {
      val u = new FuncCallT()
      u.setBase(exprT(e, kind, operands))
      u.setInstantiatedTypeArgs(typeIds(e.instantiatedTypeArgs))
      u.setObjType(e.objType.map(typeId).getOrElse(0L))
      u
    }

    private def tryKind(isTry: Boolean, normal: Int, tryKind: Int): Int =
      if (isTry) tryKind else normal

    /** model OverflowStrategy -> v1203 wire ordinal (CHECKED at ordinal 1) */
    private def ofsWire(o: OverflowStrategy): Int = o match {
      case OverflowStrategy.Na        => 0
      case OverflowStrategy.Checked   => 1
      case OverflowStrategy.Wrapping  => 2
      case OverflowStrategy.Throwing  => 3
      case OverflowStrategy.Saturating => 4
    }

    // =================================================================================
    // def dispatch / serialization
    // =================================================================================

    private def dispatchDef(d: model.CustomTypeDef): CustomTypeDefElemUnion = {
      val base = customTypeDefT(d)
      val (tag, obj): (Byte, AnyRef) = d match {
        case d: StructDef =>
          val u = new StructDefT()
          u.setBase(base)
          u.setIsCstruct(d.isCStruct)
          (CustomTypeDefElem.StructDef, u)
        case d: EnumDef =>
          val u = new EnumDefT()
          u.setBase(base)
          val ctors = d.ctors.map { c =>
            // C++ EnumCtorInfo carries no annoInfo
            val cu = new EnumCtorInfoT()
            cu.setSrcCodeName(c.srcCodeName)
            cu.setMangledName(c.mangledName)
            cu.setFuncType(typeId(c.funcType))
            cu
          }
          u.setCtors(ctors.toArray) // always written (possibly empty)
          u.setNonExhaustive(d.nonExhaustive)
          (CustomTypeDefElem.EnumDef, u)
        case d: ClassDef =>
          val u = new ClassDefT()
          u.setBase(base)
          u.setIsClass(d.isClass)
          u.setIsAnnotation(d.isAnnotation)
          if (d.isAnnotation) u.setAnnotationTargets(valueIds(d.annotationTargets))
          u.setSuperClass(d.superClass.map(typeId).getOrElse(0L))
          (CustomTypeDefElem.ClassDef, u)
        case d: ExtendDef =>
          val u = new ExtendDefT()
          u.setBase(base)
          u.setExtendedType(d.extendedType.map(typeId).getOrElse(0L))
          u.setGenericParams(typeIds(d.genericParams))
          (CustomTypeDefElem.ExtendDef, u)
      }
      defUnion(tag, obj)
    }

    private def customTypeDefT(d: model.CustomTypeDef): CustomTypeDefT = {
      val u = new CustomTypeDefT()
      u.setBase(baseT(d.annos, d.loc, d.attributes))
      u.setKind(d.kind.ordinal)
      u.setCustomTypeDefId(defId(d))
      u.setSrcCodeIdentifier(d.srcCodeIdentifier)
      u.setIdentifier(d.identifier)
      u.setPackageName(d.packageName)
      u.setType(d.tpe.map(typeId).getOrElse(0L))
      u.setGenericDecl(d.genericDecl.map(defId).getOrElse(0L))
      u.setMethods(valueIds(d.methods))
      u.setImplementedInterfaces(typeIds(d.implementedInterfaces))
      u.setInstanceMemberVars(d.instanceMemberVars.map(memberVarInfoT).toArray) // always written (possibly empty)
      u.setStaticMemberVars(valueIds(d.staticMemberVars))
      u.setAnnoInfo(annoInfoT(d.annoInfo))
      u.setVtable(d.vtable.map(vTableInTypeT).toArray) // always written (possibly empty)
      u.setInstanceVarInitFunc(d.instanceVarInitFunc.map(valueId).getOrElse(0L))
      u
    }

    private def memberVarInfoT(m: MemberVarInfo): MemberVarInfoT = {
      val u = new MemberVarInfoT()
      u.setName(m.name)
      u.setRawMangledName(m.rawMangledName)
      u.setType(typeId(m.tpe))
      u.setAttributes(m.attributes)
      u.setLoc(debugLocationT(m.loc))
      u.setAnnoInfo(annoInfoT(m.annoInfo))
      u.setInitializerFunc(m.initializerFunc.map(valueId).getOrElse(0L))
      u.setOuterDef(m.outerDef.map(defId).getOrElse(0L))
      u
    }

    private def vTableInTypeT(v: VTableInType): VTableInTypeT = {
      val u = new VTableInTypeT()
      u.setSrcParentType(typeId(v.srcParentType))
      val virtualMethods = v.virtualMethods.map { m =>
        val mu = new VirtualMethodInfoT()
        mu.setFuncName(m.funcName)
        mu.setSigType(typeId(m.sigType))
        mu.setMethodGenericTypeParams(typeIds(m.methodGenericTypeParams))
        mu.setInstance(m.instance.map(valueId).getOrElse(0L))
        mu.setAttributes(m.attributes)
        mu.setOriginalType(typeId(m.originalType))
        mu.setParentType(typeId(m.parentType))
        mu.setReturnType(typeId(m.returnType))
        mu
      }.toArray
      u.setVirtualMethods(virtualMethods) // always written (possibly empty)
      u
    }

    // =================================================================================
    // misc serializers
    // =================================================================================

    private def baseT(annos: scala.collection.Seq[Annotation], loc: Option[DebugLocation], attributes: Long): BaseT = {
      val u = new BaseT()
      // C++ always writes the (possibly empty) annos vector; the deserializer
      // dereferences it without a null check
      u.setAnnos(annos.flatMap(annoUnion).toArray)
      loc.foreach(l => u.setLoc(debugLocationT(Some(l))))
      u.setAttributes(attributes)
      u
    }

    private def annoUnion(a: Annotation): Option[AnnotationUnion] = {
      val (tag, value): (Byte, AnyRef) = a match {
        case a: NeedCheckArrayBound =>
          val u = new NeedCheckArrayBoundT(); u.setNeed(a.need)
          (FAnnotation.needCheckArrayBound, u)
        case a: NeedCheckCast =>
          val u = new NeedCheckCastT(); u.setNeed(a.need)
          (FAnnotation.needCheckCast, u)
        case a: DebugLocationInfoForWarning =>
          (FAnnotation.debugLocationInfoForWarning, debugLocationT(Some(a.loc)))
        case a: GeneratedFromForIn =>
          val u = new GeneratedFromForInT(); u.setValue(a.value)
          (FAnnotation.generatedFromForIn, u)
        case a: IsAutoEnvClass =>
          val u = new IsAutoEnvClassT(); u.setValue(a.value)
          (FAnnotation.isAutoEnvClass, u)
        case a: IsCapturedClassInCC =>
          val u = new IsCapturedClassInCCT(); u.setValue(a.value)
          (FAnnotation.isCapturedClassInCC, u)
        case a: LinkTypeInfo =>
          val u = new LinkTypeInfoT(); u.setLinkage(a.linkage.ordinal.toShort)
          (FAnnotation.linkTypeInfo, u)
        case a: SkipCheck =>
          val u = new SkipCheckT(); u.setSkipKind(a.skipKind.ordinal)
          (FAnnotation.skipCheck, u)
        case a: NeverOverflowInfo =>
          val u = new NeverOverflowInfoT(); u.setNeverOverflow(a.neverOverflow)
          (FAnnotation.neverOverflowInfo, u)
        case a: EnumCaseIndex =>
          val u = new EnumCaseIndexT(); u.setIndex(a.index)
          (FAnnotation.enumCaseIndex, u)
        case a: VirMethodOffset =>
          val u = new VirMethodOffsetT(); u.setOffset(a.offset)
          (FAnnotation.virMethodOffset, u)
        case a: WrappedRawMethod =>
          // WrappedRawMethod of a body-less (removed) imported func is not serialized
          a.rawMethod match {
            case f: Func if f.body.isEmpty => return None
            case _ =>
          }
          val u = new WrappedRawMethodT(); u.setRawMethod(valueId(a.rawMethod))
          (FAnnotation.wrappedRawMethod, u)
        case a: OverrideSrcFuncType =>
          val u = new OverrideSrcFuncTypeT(); u.setType(typeId(a.tpe))
          (FAnnotation.overrideSrcFuncType, u)
      }
      Some(annoUnionOf(tag, value))
    }

    private def debugLocationT(loc: Option[DebugLocation]): DebugLocationT = {
      val u = new DebugLocationT()
      loc.foreach { l =>
        u.setFilePath(l.filePath)
        u.setFileId(l.fileId.toLong)
        // C++ always writes begin/end pos and the (possibly empty) scope vector
        u.setBeginPos(posT(l.beginPos.getOrElse(Pos(0, 0))))
        u.setEndPos(posT(l.endPos.getOrElse(Pos(0, 0))))
        u.setScope(l.scope.toArray)
      }
      u
    }

    private def posT(p: Pos): PosT = {
      val u = new PosT()
      u.setLine(p.line.toLong)
      u.setColumn(p.column.toLong)
      u
    }

    private def annoInfoT(info: Option[AnnoInfo]): AnnoInfoT = {
      val u = new AnnoInfoT()
      info.foreach { ai =>
        u.setMangledName(ai.mangledName)
        if (ai.annoInstances.nonEmpty) {
          u.setAnnoInstances(ai.annoInstances.map { inst =>
            val iu = new CustomAnnoInstanceT()
            iu.setAnnoClassName(inst.annoClassName)
            if (inst.argValues.nonEmpty) iu.setArgValues(inst.argValues.toArray)
            inst.loc.foreach(l => iu.setLoc(debugLocationT(Some(l))))
            iu
          }.toArray)
        }
      }
      u
    }

    // =================================================================================
    // union helpers
    // =================================================================================

    private def typeUnion(tag: Byte, value: AnyRef): TypeElemUnion = {
      val u = new TypeElemUnion()
      u.setType(tag)
      u.setValue(value)
      u
    }

    private def valueUnion(tag: Byte, value: AnyRef): ValueElemUnion = {
      val u = new ValueElemUnion()
      u.setType(tag)
      u.setValue(value)
      u
    }

    private def exprUnion(tag: Byte, value: AnyRef): ExpressionElemUnion = {
      val u = new ExpressionElemUnion()
      u.setType(tag)
      u.setValue(value)
      u
    }

    private def defUnion(tag: Byte, value: AnyRef): CustomTypeDefElemUnion = {
      val u = new CustomTypeDefElemUnion()
      u.setType(tag)
      u.setValue(value)
      u
    }

    private def annoUnionOf(tag: Byte, value: AnyRef): AnnotationUnion = {
      val u = new AnnotationUnion()
      u.setType(tag)
      u.setValue(value)
      u
    }
  }
}
