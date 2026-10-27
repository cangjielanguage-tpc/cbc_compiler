/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.{
  AllocateBase => FAllocateBase, Annotation => FAnnotation, AnnoInfo => FAnnoInfo, ApplyBase => FApplyBase,
  Base => FBase, BinaryExpressionBase => FBinaryExpressionBase, BoolLiteral => FBoolLiteral, Branch => FBranch,
  Block => FBlock, BlockGroup => FBlockGroup, CHIRPackage, CHIRExprKind, CustomAnnoInstance => FCustomAnnoInstance,
  CustomType => FCustomType, CustomTypeDefElem, Debug => FDebug,
  DebugLocation => FDebugLocation, EnumCaseIndex => FEnumCaseIndex, EnumCtorInfo => FEnumCtorInfo,
  EnumDef => FEnumDef, Expression => FExpression, ExpressionElem, FloatLiteral => FFloatLiteral,
  FuncCall => FFuncCall, FuncSigInfo => FFuncSigInfo, FuncType => FFuncType, Function => FFunction,
  GeneratedFromForIn => FGeneratedFromForIn, GenericType => FGenericType, GetElementByName => FGetElementByName,
  GetElementRef => FGetElementRef, GetInstantiateValue => FGetInstantiateValue, GetRTTIStatic => FGetRTTIStatic,
  GlobalVar => FGlobalVar, GlobalValue => FGlobalValue, IntrinsicBase => FIntrinsicBase, IntLiteral => FIntLiteral,
  InvokeBase => FInvokeBase, IsAutoEnvClass => FIsAutoEnvClass, IsCapturedClassInCC => FIsCapturedClassInCC,
  Lambda => FLambda, LinkTypeInfo => FLinkTypeInfo, LiteralValue => FLiteralValue, LocalVar => FLocalVar,
  MemberVarInfo => FMemberVarInfo, MultiBranch => FMultiBranch, NeedCheckArrayBound => FNeedCheckArrayBound,
  NeedCheckCast => FNeedCheckCast, NeverOverflowInfo => FNeverOverflowInfo, NullLiteral => FNullLiteral,
  NumericCastBase => FNumericCastBase, OverrideSrcFuncType => FOverrideSrcFuncType, Parameter => FParameter,
  Pos => FPos, RawArrayAllocateBase => FRawArrayAllocateBase, RawArrayType => FRawArrayType,
  RuneLiteral => FRuneLiteral, SkipCheck => FSkipCheck, SpawnBase => FSpawnBase,
  StoreElementByName => FStoreElementByName, StoreElementRef => FStoreElementRef,
  StringLiteral => FStringLiteral, StructDef => FStructDef, Type => FType, TypeElem,
  UnaryExpressionBase => FUnaryExpressionBase, UnitLiteral => FUnitLiteral, Value => FValue,
  ValueElem, VArrayType => FVArrayType, VirMethodOffset => FVirMethodOffset, VTableInType => FVTableInType,
  VirtualMethodInfo => FVirtualMethodInfo, WrappedRawMethod => FWrappedRawMethod, ClassDef => FClassDef,
  ExtendDef => FExtendDef, Field => FField, FieldByName => FFieldByName, InstanceOf => FInstanceOf,
  AllocateBaseT, AnnoInfoT, AnnotationUnion, ApplyBaseT, BaseT, BinaryExpressionBaseT,
  CustomAnnoInstanceT, CustomTypeDefT, DebugLocationT, EnumCtorInfoT, ExpressionT, FuncSigInfoT,
  MemberVarInfoT, PosT, VTableInTypeT, VirtualMethodInfoT,
}
import com.huawei.excelsior.jet.compiler.chir.model
import com.huawei.excelsior.jet.compiler.chir.model.*

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer

/** Materializes a v13002 .chir file into a mutable [[model.Package]]
  * (literal read of the flatbuffer, references wired by 1-based ids).
  *
  * Uses the flatc object API (`.unpack()` T-classes) for field access; cross-table
  * references stay raw uint32 ids and are resolved via the deferred fixup list.
  *
  * NOTE: keep structurally in sync with Materializer1203 (v1203 differs only in
  * InvokeBase: virMethodCtx instead of overflowStrategy, and SkipKind CHECKED).
  */
object Materializer13002 {

  def load(bytes: Array[Byte]): Package = new Impl(bytes).load()

  private final class Impl(bytes: Array[Byte]) {
    private val root = CHIRPackage.getRootAsCHIRPackage(ByteBuffer.wrap(bytes))
    private val rootT = root.unpack()

    private val pkg = new Package()
    private val deferred = ArrayBuffer[() => Unit]()

    private val types = new Array[Type](root.typesLength())
    private val values = new Array[Value](root.valuesLength())
    private val exprs = new Array[Expression](root.exprsLength())
    private val defs = new Array[CustomTypeDef](root.defsLength())

    def load(): Package = {
      pkg.name = orEmpty(rootT.getName)
      pkg.accessLevel = PackageAccessLevel.fromOrdinal(rootT.getPkgAccessLevel)
      pkg.phase = Phase.fromOrdinal(rootT.getPhase)

      materializeTypes()
      materializeValues()
      materializeExprs()
      materializeDefs()
      wirePackageLevel()
      deferred.foreach(_.apply())
      pkg
    }

    // ===============================================================================
    // types
    // ===============================================================================

    private def materializeTypes(): Unit =
      for (i <- types.indices) {
        val id = i + 1
        root.typesType(i) match {
          case TypeElem.GenericType =>
            val t = root.types(new FGenericType(), i).asInstanceOf[FGenericType].unpack()
            val gt = new GenericType(orEmpty(t.getIdentifier), orEmpty(t.getSrcCodeIdentifier))
            deferred += { () =>
              for (j <- orNull(t.getUpperBounds).indices) gt.upperBounds += ty(t.getUpperBounds()(j))
            }
            types(i) = gt
          case TypeElem.FuncType =>
            val t = root.types(new FFuncType(), i).asInstanceOf[FFuncType].unpack()
            val ft = new FuncType(t.getIsCfuncType, t.getHasVarArg, ArrayBuffer())
            deferred += { () =>
              for (j <- orNull(t.getBase.getArgTys).indices) ft.argTys += ty(t.getBase.getArgTys()(j))
            }
            types(i) = ft
          case TypeElem.CustomType =>
            val t = root.types(new FCustomType(), i).asInstanceOf[FCustomType].unpack()
            val ct = new CustomType(null)
            deferred += { () =>
              ct.customTypeDef = defRef(t.getCustomTypeDef)
              for (j <- orNull(t.getBase.getArgTys).indices) ct.argTys += ty(t.getBase.getArgTys()(j))
            }
            types(i) = ct
          case TypeElem.RawArrayType =>
            val t = root.types(new FRawArrayType(), i).asInstanceOf[FRawArrayType].unpack()
            val rt = new RawArrayType(null, t.getDims)
            deferred += { () => rt.elementType = ty(t.getBase.getArgTys()(0)) }
            types(i) = rt
          case TypeElem.VArrayType =>
            val t = root.types(new FVArrayType(), i).asInstanceOf[FVArrayType].unpack()
            val vt = new VArrayType(null, t.getSize)
            deferred += { () => vt.elementType = ty(t.getBase.getArgTys()(0)) }
            types(i) = vt
          case TypeElem.Type =>
            val t = root.types(new FType(), i).asInstanceOf[FType].unpack()
            // wildcard Type table: builtins + Tuple/CPointer/Ref/Box composites
            val kind = TypeKind.fromOrdinal(t.getKind)
            val mt = TypeImpl(kind, ArrayBuffer())
            if (kind == TypeKind.Tuple || kind == TypeKind.CPointer || kind == TypeKind.Ref || kind == TypeKind.Box) {
              deferred += { () =>
                for (j <- orNull(t.getArgTys).indices) mt.argTys += ty(t.getArgTys()(j))
              }
            }
            types(i) = mt
        }
        types(i).id = id
      }

    // ===============================================================================
    // values
    // ===============================================================================

    private def materializeValues(): Unit =
      for (i <- values.indices) {
        val id = i + 1
        values(i) = root.valuesType(i) match {
          case ValueElem.BoolLiteral =>
            val v = root.values(new FBoolLiteral(), i).asInstanceOf[FBoolLiteral].unpack()
            val vb = v.getBase.getBase() // Value table
            val b = vb.getBase() // Base table
            new BoolLiteral(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), v.getVal())
          case ValueElem.RuneLiteral =>
            val v = root.values(new FRuneLiteral(), i).asInstanceOf[FRuneLiteral].unpack()
            val vb = v.getBase.getBase()
            val b = vb.getBase()
            new RuneLiteral(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), v.getVal())
          case ValueElem.StringLiteral =>
            val v = root.values(new FStringLiteral(), i).asInstanceOf[FStringLiteral].unpack()
            val vb = v.getBase.getBase()
            val b = vb.getBase()
            new StringLiteral(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), orEmpty(v.getVal))
          case ValueElem.IntLiteral =>
            val v = root.values(new FIntLiteral(), i).asInstanceOf[FIntLiteral].unpack()
            val vb = v.getBase.getBase()
            val b = vb.getBase()
            new IntLiteral(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), v.getVal())
          case ValueElem.FloatLiteral =>
            val v = root.values(new FFloatLiteral(), i).asInstanceOf[FFloatLiteral].unpack()
            val vb = v.getBase.getBase()
            val b = vb.getBase()
            new FloatLiteral(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), v.getVal())
          case ValueElem.UnitLiteral =>
            val v = root.values(new FUnitLiteral(), i).asInstanceOf[FUnitLiteral].unpack()
            val vb = v.getBase.getBase()
            val b = vb.getBase()
            new UnitLiteral(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier))
          case ValueElem.NullLiteral =>
            val v = root.values(new FNullLiteral(), i).asInstanceOf[FNullLiteral].unpack()
            val vb = v.getBase.getBase()
            val b = vb.getBase()
            new NullLiteral(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier))
          case ValueElem.Parameter =>
            val v = root.values(new FParameter(), i).asInstanceOf[FParameter].unpack()
            val vb = v.getBase()
            val b = vb.getBase()
            val p = new Parameter(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), None, None, orEmpty(v.getSrcCodeIdentifier),
              readAnnoInfo(v.getAnnoInfo))
            deferred += { () =>
              p.ownedFunc = vref[Func](v.getOwnedFunc)
              p.ownedLambda = eref[LambdaExpr](v.getOwnedLambda)
            }
            p
          case ValueElem.LocalVar =>
            val v = root.values(new FLocalVar(), i).asInstanceOf[FLocalVar].unpack()
            val vb = v.getBase()
            val b = vb.getBase()
            val lv = new LocalVar(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), None, v.getIsRetVal, orEmpty(v.getSrcCodeIdentifier))
            deferred += { () => lv.associatedExpr = eref[Expression](v.getAssociatedExpr) }
            lv
          case ValueElem.GlobalVar =>
            val v = root.values(new FGlobalVar(), i).asInstanceOf[FGlobalVar].unpack()
            val gv = v.getBase() // GlobalValue
            val vb = gv.getBase() // Value
            val b = vb.getBase() // Base
            val g = new GlobalVar(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), orEmpty(gv.getSrcCodeIdentifier), orEmpty(gv.getRawMangledName),
              orEmpty(gv.getPackageName), None, readStrings(gv.getFeatures),
              readAnnoInfo(gv.getAnnoInfo), None)
            deferred += { () =>
              g.declaredParent = defRef(gv.getDeclaredParent)
              g.initializer = vref[Value](v.getInitializer)
            }
            g
          case ValueElem.Function =>
            val v = root.values(new FFunction(), i).asInstanceOf[FFunction].unpack()
            val gv = v.getBase()
            val vb = gv.getBase()
            val b = vb.getBase()
            val f = new Func(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), orEmpty(gv.getSrcCodeIdentifier), orEmpty(gv.getRawMangledName),
              orEmpty(gv.getPackageName), None, readStrings(gv.getFeatures),
              readAnnoInfo(gv.getAnnoInfo), None, FuncKind.fromOrdinal(v.getFuncKind),
              v.getIsFastNative, v.getIsCffiwrapper, readFuncSigInfo(v.getOriginalLambdaInfo),
              ArrayBuffer(), None, None, ArrayBuffer(), None, readLoc(v.getPropLoc),
              v.getLocalId, v.getBlockId, v.getBlockGroupId)
            deferred += { () =>
              f.declaredParent = defRef(gv.getDeclaredParent)
              f.genericDecl = vref[Value](v.getGenericDecl)
              f.paramDftValHostFunc = vref[Func](v.getParamDftValHostFunc)
              f.body = vref[BlockGroup](v.getBody)
              for (j <- orNull(v.getParams).indices) f.params += values((v.getParams()(j) - 1).toInt).asInstanceOf[Parameter]
              f.retVal = vref[LocalVar](v.getRetVal)
              for (j <- orNull(v.getGenericTypeParams).indices) {
                f.genericTypeParams += ty(v.getGenericTypeParams()(j)).asInstanceOf[GenericType]
              }
            }
            f
          case ValueElem.Block =>
            val v = root.values(new FBlock(), i).asInstanceOf[FBlock].unpack()
            val vb = v.getBase()
            val b = vb.getBase()
            val bl = new Block(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), None, ArrayBuffer(), ArrayBuffer(), v.getIsLandingPadBlock,
              ArrayBuffer())
            deferred += { () =>
              bl.parentGroup = vref[BlockGroup](v.getParentGroup)
              for (j <- orNull(v.getExprs).indices) bl.exprs += exprs((v.getExprs()(j) - 1).toInt)
              for (j <- orNull(v.getPredecessors).indices) {
                bl.predecessors += values((v.getPredecessors()(j) - 1).toInt).asInstanceOf[Block]
              }
              for (j <- orNull(v.getExceptionCatchList).indices) bl.exceptionCatchList += ty(v.getExceptionCatchList()(j))
            }
            bl
          case ValueElem.BlockGroup =>
            val v = root.values(new FBlockGroup(), i).asInstanceOf[FBlockGroup].unpack()
            val vb = v.getBase()
            val b = vb.getBase()
            val bg = new BlockGroup(annosOf(b), locOf(b), b.getAttributes, tyOpt(vb.getType),
              orEmpty(vb.getIdentifier), None, ArrayBuffer(), None, None)
            deferred += { () =>
              bg.entryBlock = vref[Block](v.getEntryBlock)
              for (j <- orNull(v.getBlocks).indices) bg.blocks += values((v.getBlocks()(j) - 1).toInt).asInstanceOf[Block]
              bg.ownedFunc = vref[Func](v.getOwnedFunc)
              bg.ownedExpression = eref[LambdaExpr](v.getOwnedExpression)
            }
            bg
        }
        values(i).id = id
      }

    // ===============================================================================
    // exprs
    // ===============================================================================

    private def materializeExprs(): Unit =
      for (i <- exprs.indices) {
        val id = i + 1
        exprs(i) = root.exprsType(i) match {
          case ExpressionElem.Expression => wildcardExpr(i)
          case ExpressionElem.AllocateBase =>
            val e = root.exprs(new FAllocateBase(), i).asInstanceOf[FAllocateBase].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val isTry = isTryKind(eb.getKind)
            new AllocateExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), ty(e.getAllocatedType),
              isTry, tryBlocks(eb, isTry))
          case ExpressionElem.UnaryExpressionBase =>
            val e = root.exprs(new FUnaryExpressionBase(), i).asInstanceOf[FUnaryExpressionBase].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val isTry = isTryKind(eb.getKind)
            val ops = opRefs(eb)
            new UnaryExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), unaryOp(eb.getKind),
              OverflowStrategy.fromOrdinal(e.getOverflowStrategy), ops.head, isTry,
              ArrayBuffer.from(ops.drop(1).map(_.asInstanceOf[Block])))
          case ExpressionElem.BinaryExpressionBase =>
            val e = root.exprs(new FBinaryExpressionBase(), i).asInstanceOf[FBinaryExpressionBase].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val isTry = isTryKind(eb.getKind)
            val ops = opRefs(eb)
            new BinaryExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), binaryOp(eb.getKind),
              OverflowStrategy.fromOrdinal(e.getOverflowStrategy), ops(0), ops(1), isTry,
              ArrayBuffer.from(ops.drop(2).map(_.asInstanceOf[Block])))
          case ExpressionElem.ApplyBase =>
            val e = root.exprs(new FApplyBase(), i).asInstanceOf[FApplyBase].unpack()
            val fc = e.getBase()
            val eb = fc.getBase()
            val b = eb.getBase()
            val isTry = isTryKind(eb.getKind)
            val ops = opRefs(eb)
            new ApplyExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar),
              tyRefs(fc.getInstantiatedTypeArgs), tyOpt(fc.getObjType), ops.head,
              ArrayBuffer.from(ops.tail.dropRight(if (isTry) 2 else 0)),
              e.getIsSuperCall, isTry, tryBlocks(eb, isTry))
          case ExpressionElem.InvokeBase =>
            val e = root.exprs(new FInvokeBase(), i).asInstanceOf[FInvokeBase].unpack()
            val fc = e.getBase()
            val eb = fc.getBase()
            val b = eb.getBase()
            val isTry = isTryKind(eb.getKind)
            val ops = opRefs(eb)
            val nArgs = ops.length - 2 - (if (isTry) 2 else 0) // minus callee, caller (+ try tail)
            new InvokeExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar),
              tyRefs(fc.getInstantiatedTypeArgs), tyOpt(fc.getObjType), ops(0).asInstanceOf[Func],
              ops(1), ArrayBuffer.from(ops.slice(2, 2 + nArgs)),
              OverflowStrategy.fromOrdinal(e.getOverflowStrategy), isTry, tryBlocks(eb, isTry))
          case ExpressionElem.IntrinsicBase =>
            val e = root.exprs(new FIntrinsicBase(), i).asInstanceOf[FIntrinsicBase].unpack()
            val fc = e.getBase()
            val eb = fc.getBase()
            val b = eb.getBase()
            val isTry = isTryKind(eb.getKind)
            val ops = opRefs(eb)
            new IntrinsicExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar),
              tyRefs(fc.getInstantiatedTypeArgs), tyOpt(fc.getObjType), e.getIntrinsicKind,
              ArrayBuffer.from(ops.dropRight(if (isTry) 2 else 0)), isTry, tryBlocks(eb, isTry))
          case ExpressionElem.NumericCastBase =>
            val e = root.exprs(new FNumericCastBase(), i).asInstanceOf[FNumericCastBase].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val isTry = isTryKind(eb.getKind)
            val ops = opRefs(eb)
            new NumericCastExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar),
              OverflowStrategy.fromOrdinal(e.getOverflowStrategy), ops.head, isTry,
              ArrayBuffer.from(ops.drop(1).map(_.asInstanceOf[Block])))
          case ExpressionElem.RawArrayAllocateBase =>
            val e = root.exprs(new FRawArrayAllocateBase(), i).asInstanceOf[FRawArrayAllocateBase].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val isTry = isTryKind(eb.getKind)
            val ops = opRefs(eb)
            new RawArrayAllocateExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), ty(e.getElementType),
              ops.head, isTry, ArrayBuffer.from(ops.drop(1).map(_.asInstanceOf[Block])))
          case ExpressionElem.SpawnBase =>
            val e = root.exprs(new FSpawnBase(), i).asInstanceOf[FSpawnBase].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val isTry = isTryKind(eb.getKind)
            val ops = opRefs(eb)
            val spawnArg = ops.drop(1).headOption.filterNot(_.isInstanceOf[Block])
            new SpawnExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), ops.head, spawnArg,
              funcRef(e.getExecuteClosure), isTry, tryBlocks(eb, isTry))
          case ExpressionElem.Branch =>
            val e = root.exprs(new FBranch(), i).asInstanceOf[FBranch].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val ops = opRefs(eb)
            new BranchExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), ops(0),
              ops(1).asInstanceOf[Block], ops(2).asInstanceOf[Block], e.getSourceExpr)
          case ExpressionElem.MultiBranch =>
            val e = root.exprs(new FMultiBranch(), i).asInstanceOf[FMultiBranch].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val ops = opRefs(eb)
            val caseValues = ArrayBuffer[Long]()
            for (j <- orNull(e.getCaseValues).indices) caseValues += e.getCaseValues()(j)
            new MultiBranchExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), ops.head, caseValues,
              ops(1).asInstanceOf[Block], ArrayBuffer.from(ops.drop(2).map(_.asInstanceOf[Block])))
          case ExpressionElem.GetElementRef =>
            val e = root.exprs(new FGetElementRef(), i).asInstanceOf[FGetElementRef].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            new GetElementRefExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), opRef(eb, 0),
              readLongs(e.getPath))
          case ExpressionElem.GetElementByName =>
            val e = root.exprs(new FGetElementByName(), i).asInstanceOf[FGetElementByName].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            new GetElementByNameExpr(annosOf(b), locOf(b), b.getAttributes(),
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), opRef(eb, 0),
              readStrings(e.getFieldNames))
          case ExpressionElem.StoreElementRef =>
            val e = root.exprs(new FStoreElementRef(), i).asInstanceOf[FStoreElementRef].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            new StoreElementRefExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), opRef(eb, 0), opRef(eb, 1),
              readLongs(e.getPath))
          case ExpressionElem.StoreElementByName =>
            val e = root.exprs(new FStoreElementByName(), i).asInstanceOf[FStoreElementByName].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            new StoreElementByNameExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), opRef(eb, 0), opRef(eb, 1),
              readStrings(e.getFieldNames))
          case ExpressionElem.InstanceOf =>
            val e = root.exprs(new FInstanceOf(), i).asInstanceOf[FInstanceOf].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            new InstanceOfExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), opRef(eb, 0),
              ty(e.getTargetType))
          case ExpressionElem.Debug =>
            val e = root.exprs(new FDebug(), i).asInstanceOf[FDebug].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val ops = opRefs(eb)
            new DebugExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar),
              orEmpty(e.getSrcCodeName), ops(0))
          case ExpressionElem.Field =>
            val e = root.exprs(new FField(), i).asInstanceOf[FField].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            new FieldExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), opRef(eb, 0),
              readLongs(e.getPath))
          case ExpressionElem.FieldByName =>
            val e = root.exprs(new FFieldByName(), i).asInstanceOf[FFieldByName].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            new FieldByNameExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), opRef(eb, 0),
              readStrings(e.getFieldNames))
          case ExpressionElem.GetInstantiateValue =>
            val e = root.exprs(new FGetInstantiateValue(), i).asInstanceOf[FGetInstantiateValue].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            new GetInstantiateValueExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), opRef(eb, 0),
              tyRefs(e.getInstantiateTypes))
          case ExpressionElem.GetRTTIStatic =>
            val e = root.exprs(new FGetRTTIStatic(), i).asInstanceOf[FGetRTTIStatic].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            new GetRTTIStaticExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar), ty(e.getRttiType))
          case ExpressionElem.Lambda =>
            val e = root.exprs(new FLambda(), i).asInstanceOf[FLambda].unpack()
            val eb = e.getBase()
            val b = eb.getBase()
            val lam = new LambdaExpr(annosOf(b), locOf(b), b.getAttributes,
              blockRef(eb.getOwner), bgRefs(eb), localVarRef(eb.getResultLocalVar),
              ty(e.getFuncTy).asInstanceOf[FuncType], e.getIsLocalFunc, orEmpty(e.getIdentifier),
              orEmpty(e.getSrcCodeName), ArrayBuffer(), tyRefs(e.getGenericTypeParams),
              None, None, e.getIsCompileTimeValue)
            // params/body/retVal are wired from the Parameter/BlockGroup values owning this lambda
            deferred += { () =>
              for (j <- orNull(e.getParams).indices) lam.params += values((e.getParams()(j) - 1).toInt).asInstanceOf[Parameter]
              lam.body = vref[BlockGroup](e.getBody)
              lam.retVal = vref[LocalVar](e.getRetVal)
            }
            lam
        }
        exprs(i).id = id
      }

    /** Wildcard Expression table entry: kind picks the concrete class. */
    private def wildcardExpr(i: Int): Expression = {
      val e = root.exprs(new FExpression(), i).asInstanceOf[FExpression].unpack()
      val b = e.getBase()
      val annosV = annosOf(b)
      val locV = locOf(b)
      val attrsV = b.getAttributes
      val ownerV = blockRef(e.getOwner)
      val resultVarV = localVarRef(e.getResultLocalVar)
      val bgsV = bgRefs(e)
      val ops = opRefs(e)
      e.getKind match {
        case CHIRExprKind.Goto => new GotoExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ops(0).asInstanceOf[Block])
        case CHIRExprKind.Exit => new ExitExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV)
        case CHIRExprKind.RaiseException =>
          new RaiseExceptionExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ops(0),
            ops.lift(1).map(_.asInstanceOf[Block]))
        case CHIRExprKind.Load => new LoadExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ops(0))
        case CHIRExprKind.Store => new StoreExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ops(0), ops(1))
        case CHIRExprKind.Constant => new ConstantExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ops(0))
        case CHIRExprKind.Tuple => new TupleExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ArrayBuffer.from(ops))
        case CHIRExprKind.GetException => new GetExceptionExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV)
        case CHIRExprKind.GetRtti => new GetRTTIExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ops(0))
        case CHIRExprKind.VArrayExpr => new VArrayExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ArrayBuffer.from(ops))
        case CHIRExprKind.VArrayBuilder =>
          new VArrayBuilderExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ops(0), ops(1), ops(2))
        case CHIRExprKind.RawArrayLiteralInit =>
          new RawArrayLiteralInitExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ops.head,
            ArrayBuffer.from(ops.tail))
        case CHIRExprKind.RawArrayInitByValue =>
          new RawArrayInitByValueExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, ops(0), ops(1), ops(2))
        case CHIRExprKind.StaticCast => new CastExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, CastOp.StaticCast, ops(0))
        case CHIRExprKind.Box => new CastExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, CastOp.Box, ops(0))
        case CHIRExprKind.UnboxToValue => new CastExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, CastOp.UnboxToValue, ops(0))
        case CHIRExprKind.UnboxToRef => new CastExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, CastOp.UnboxToRef, ops(0))
        case CHIRExprKind.CastToConcrete => new CastExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, CastOp.CastToConcrete, ops(0))
        case CHIRExprKind.CastToGeneric => new CastExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, CastOp.CastToGeneric, ops(0))
        case CHIRExprKind.Debug =>
          // An Expression-table entry carries no string field; the C++ read of
          // srcCodeName on such an entry always yields the empty string.
          new DebugExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, "", ops(0))
        case other => sys.error(s"Unexpected wildcard Expression kind $other (expr #${i + 1})")
      }
    }

    // ===============================================================================
    // defs
    // ===============================================================================

    private def materializeDefs(): Unit =
      for (i <- defs.indices) {
        val id = i + 1
        val d: DefBase = root.defsType(i) match {
          case CustomTypeDefElem.EnumDef =>
            val d = root.defs(new FEnumDef(), i).asInstanceOf[FEnumDef].unpack()
            val b = d.getBase.getBase()
            new EnumDef(annosOf(b), locOf(b), b.getAttributes, orEmpty(d.getBase.getSrcCodeIdentifier),
              orEmpty(d.getBase.getIdentifier), orEmpty(d.getBase.getPackageName),
              tyOpt(d.getBase.getType), None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(),
              ArrayBuffer(), readAnnoInfo(d.getBase.getAnnoInfo), ArrayBuffer(), None,
              readEnumCtors(d), d.getNonExhaustive)
          case CustomTypeDefElem.StructDef =>
            val d = root.defs(new FStructDef(), i).asInstanceOf[FStructDef].unpack()
            val b = d.getBase.getBase()
            new StructDef(annosOf(b), locOf(b), b.getAttributes, orEmpty(d.getBase.getSrcCodeIdentifier),
              orEmpty(d.getBase.getIdentifier), orEmpty(d.getBase.getPackageName),
              tyOpt(d.getBase.getType), None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(),
              ArrayBuffer(), readAnnoInfo(d.getBase.getAnnoInfo), ArrayBuffer(), None, d.getIsCstruct)
          case CustomTypeDefElem.ClassDef =>
            val d = root.defs(new FClassDef(), i).asInstanceOf[FClassDef].unpack()
            val b = d.getBase.getBase()
            val cd = new ClassDef(annosOf(b), locOf(b), b.getAttributes, orEmpty(d.getBase.getSrcCodeIdentifier),
              orEmpty(d.getBase.getIdentifier), orEmpty(d.getBase.getPackageName),
              tyOpt(d.getBase.getType), None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(),
              ArrayBuffer(), readAnnoInfo(d.getBase.getAnnoInfo), ArrayBuffer(), None,
              d.getIsClass, d.getIsAnnotation, ArrayBuffer(), None)
            deferred += { () =>
              for (j <- orNull(d.getAnnotationTargets).indices) {
                cd.annotationTargets += values((d.getAnnotationTargets()(j) - 1).toInt).asInstanceOf[GlobalVar]
              }
              cd.superClass = tyOpt(d.getSuperClass)
            }
            cd
          case CustomTypeDefElem.ExtendDef =>
            val d = root.defs(new FExtendDef(), i).asInstanceOf[FExtendDef].unpack()
            val b = d.getBase.getBase()
            val ed = new ExtendDef(annosOf(b), locOf(b), b.getAttributes, orEmpty(d.getBase.getSrcCodeIdentifier),
              orEmpty(d.getBase.getIdentifier), orEmpty(d.getBase.getPackageName),
              tyOpt(d.getBase.getType), None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(),
              ArrayBuffer(), readAnnoInfo(d.getBase.getAnnoInfo), ArrayBuffer(), None, None,
              ArrayBuffer())
            deferred += { () =>
              ed.extendedType = tyOpt(d.getExtendedType)
              for (j <- orNull(d.getGenericParams).indices) ed.genericParams += ty(d.getGenericParams()(j))
            }
            ed
        }
        d.id = id
        defs(i) = d

        // wire common contents
        val base = root.defsType(i) match {
          case CustomTypeDefElem.EnumDef => root.defs(new FEnumDef(), i).asInstanceOf[FEnumDef].base().unpack()
          case CustomTypeDefElem.StructDef => root.defs(new FStructDef(), i).asInstanceOf[FStructDef].base().unpack()
          case CustomTypeDefElem.ClassDef => root.defs(new FClassDef(), i).asInstanceOf[FClassDef].base().unpack()
          case CustomTypeDefElem.ExtendDef => root.defs(new FExtendDef(), i).asInstanceOf[FExtendDef].base().unpack()
        }
        deferred += { () => d.genericDecl = defRef(base.getGenericDecl) }
        for (j <- orNull(base.getMethods).indices) d.methods += values((base.getMethods()(j) - 1).toInt).asInstanceOf[Func]
        for (j <- orNull(base.getImplementedInterfaces).indices) {
          d.implementedInterfaces += ty(base.getImplementedInterfaces()(j))
        }
        for (j <- orNull(base.getStaticMemberVars).indices) {
          d.staticMemberVars += values((base.getStaticMemberVars()(j) - 1).toInt).asInstanceOf[GlobalVar]
        }
        d.instanceVarInitFunc = vref[Func](base.getInstanceVarInitFunc)
        for (j <- orNull(base.getInstanceMemberVars).indices) {
          d.instanceMemberVars += readMemberVarInfo(base.getInstanceMemberVars()(j))
        }
        for (j <- orNull(base.getVtable).indices) d.vtable += readVTable(base.getVtable()(j))
      }

    private def wirePackageLevel(): Unit = {
      for (i <- values.indices) values(i) match {
        case gv: GlobalVar => pkg.globalVars += gv
        case f: Func => pkg.globalFuncs += f
        case _ =>
      }
      for (d <- defs) d match {
        case d: StructDef => if (d.isImported) pkg.importedStructs += d else pkg.structs += d
        case d: ClassDef => if (d.isImported) pkg.importedClasses += d else pkg.classes += d
        case d: EnumDef => if (d.isImported) pkg.importedEnums += d else pkg.enums += d
        case d: ExtendDef => if (d.isImported) pkg.importedExtends += d else pkg.extendsDefs += d
      }
      pkg.packageInitFunc = vref[Func](rootT.getPackageInitFunc)
      pkg.packageLiteralInitFunc = vref[Func](rootT.getPackageLiteralInitFunc)
    }

    // ===============================================================================
    // reference helpers (1-based flatbuffer ids, 0 = null)
    // ===============================================================================

    private def ty(id: Long): Type = types((id - 1).toInt)
    private def tyOpt(id: Long): Option[Type] = if (id == 0) None else Some(ty(id))

    private def tyRefs(ids: Array[Long]): ArrayBuffer[Type] = {
      val r = ArrayBuffer[Type]()
      if (ids != null) for (j <- ids.indices) r += ty(ids(j))
      r
    }

    private def vref[V >: Null <: Value](id: Long): Option[V] =
      if (id == 0) None else Some(values((id - 1).toInt).asInstanceOf[V])

    private def eref[E >: Null <: Expression](id: Long): Option[E] =
      if (id == 0) None else Some(exprs((id - 1).toInt).asInstanceOf[E])

    private def funcRef(id: Long): Option[Func] = vref[Func](id)

    private def defRef(id: Long): Option[CustomTypeDef] =
      if (id == 0) None else Some(defs((id - 1).toInt))

    private def blockRef(id: Long): Option[Block] = vref[Block](id)

    private def localVarRef(id: Long): Option[LocalVar] = vref[LocalVar](id)

    /** Resolves the flatbuffer operand vector (value ids), including Try handler blocks. */
    private def opRefs(e: ExpressionT): ArrayBuffer[Value] = {
      val r = ArrayBuffer[Value]()
      val ops = e.getOperands
      if (ops != null) for (j <- ops.indices) r += values((ops(j) - 1).toInt)
      r
    }

    private def opRef(e: ExpressionT, j: Int): Value = values((e.getOperands()(j) - 1).toInt)

    /** Last two operands of a Try expression are [normalBlock, exceptionBlock]. */
    private def tryBlocks(e: ExpressionT, isTry: Boolean): ArrayBuffer[Block] =
      if (!isTry) ArrayBuffer()
      else {
        val ops = e.getOperands
        val n = ops.length
        ArrayBuffer(values((ops(n - 2) - 1).toInt).asInstanceOf[Block],
          values((ops(n - 1) - 1).toInt).asInstanceOf[Block])
      }

    private def bgRefs(e: ExpressionT): ArrayBuffer[BlockGroup] = {
      val r = ArrayBuffer[BlockGroup]()
      val bgs = e.getBlockGroups
      if (bgs != null) for (j <- bgs.indices) r += values((bgs(j) - 1).toInt).asInstanceOf[BlockGroup]
      r
    }

    /** TryApply(5)..TryRShift(21) are the exception-throwing kinds. */
    private def isTryKind(kind: Int): Boolean = kind >= CHIRExprKind.TryApply && kind <= CHIRExprKind.TryRShift

    private def unaryOp(kind: Int): UnaryOp = kind match {
      case CHIRExprKind.Neg | CHIRExprKind.TryNeg => UnaryOp.Neg
      case CHIRExprKind.Not => UnaryOp.Not
      case CHIRExprKind.BitNot => UnaryOp.BitNot
    }

    private def binaryOp(kind: Int): BinaryOp = kind match {
      case CHIRExprKind.Add | CHIRExprKind.TryAdd => BinaryOp.Add
      case CHIRExprKind.Sub | CHIRExprKind.TrySub => BinaryOp.Sub
      case CHIRExprKind.Mul | CHIRExprKind.TryMul => BinaryOp.Mul
      case CHIRExprKind.Div | CHIRExprKind.TryDiv => BinaryOp.Div
      case CHIRExprKind.Mod | CHIRExprKind.TryMod => BinaryOp.Mod
      case CHIRExprKind.Exp | CHIRExprKind.TryExp => BinaryOp.Exp
      case CHIRExprKind.LShift | CHIRExprKind.TryLShift => BinaryOp.LShift
      case CHIRExprKind.RShift | CHIRExprKind.TryRShift => BinaryOp.RShift
      case CHIRExprKind.BitAnd => BinaryOp.And
      case CHIRExprKind.BitOr => BinaryOp.Or
      case CHIRExprKind.BitXor => BinaryOp.Xor
      case CHIRExprKind.LT => BinaryOp.Lt
      case CHIRExprKind.GT => BinaryOp.Gt
      case CHIRExprKind.LE => BinaryOp.Le
      case CHIRExprKind.GE => BinaryOp.Ge
      case CHIRExprKind.Equal => BinaryOp.Eq
      case CHIRExprKind.NotEqual => BinaryOp.NotEq
      case other => sys.error(s"Unexpected binary op kind $other")
    }

    // ===============================================================================
    // misc readers
    // ===============================================================================

    private def annosOf(b: BaseT): ArrayBuffer[Annotation] = {
      val r = ArrayBuffer[Annotation]()
      val annos = Option(b.getAnnos).getOrElse(Array.empty[com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.AnnotationUnion])
      for (j <- annos.indices) {
        val obj: AnyRef = annos(j).getType match {
          case FAnnotation.needCheckArrayBound => annos(j).asneedCheckArrayBound()
          case FAnnotation.needCheckCast => annos(j).asneedCheckCast()
          case FAnnotation.debugLocationInfoForWarning => annos(j).asdebugLocationInfoForWarning()
          case FAnnotation.generatedFromForIn => annos(j).asgeneratedFromForIn()
          case FAnnotation.isAutoEnvClass => annos(j).asisAutoEnvClass()
          case FAnnotation.isCapturedClassInCC => annos(j).asisCapturedClassInCC()
          case FAnnotation.linkTypeInfo => annos(j).aslinkTypeInfo()
          case FAnnotation.skipCheck => annos(j).asskipCheck()
          case FAnnotation.neverOverflowInfo => annos(j).asneverOverflowInfo()
          case FAnnotation.enumCaseIndex => annos(j).asenumCaseIndex()
          case FAnnotation.virMethodOffset => annos(j).asvirMethodOffset()
          case FAnnotation.wrappedRawMethod => annos(j).aswrappedRawMethod()
          case FAnnotation.overrideSrcFuncType => annos(j).asoverrideSrcFuncType()
          case _ => null
        }
        if (obj != null) r += (obj match {
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.NeedCheckArrayBoundT => new model.NeedCheckArrayBound(o.getNeed)
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.NeedCheckCastT => new model.NeedCheckCast(o.getNeed)
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.DebugLocationT => new model.DebugLocationInfoForWarning(readLoc(o).orNull)
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.GeneratedFromForInT => new model.GeneratedFromForIn(o.getValue)
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.IsAutoEnvClassT => new model.IsAutoEnvClass(o.getValue)
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.IsCapturedClassInCCT => new model.IsCapturedClassInCC(o.getValue)
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.LinkTypeInfoT => new model.LinkTypeInfo(Linkage.fromOrdinal(o.getLinkage))
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.SkipCheckT => new model.SkipCheck(SkipKind.fromOrdinal(o.getSkipKind))
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.NeverOverflowInfoT => new model.NeverOverflowInfo(o.getNeverOverflow)
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.EnumCaseIndexT => new model.EnumCaseIndex(o.getIndex)
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.VirMethodOffsetT => new model.VirMethodOffset(o.getOffset)
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.WrappedRawMethodT =>
            val a = new model.WrappedRawMethod(null)
            val vid = o.getRawMethod
            deferred += { () => a.rawMethod = values((vid - 1).toInt) }
            a
          case o: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.OverrideSrcFuncTypeT =>
            val a = new model.OverrideSrcFuncType(null)
            val tid = o.getType
            deferred += { () => a.tpe = ty(tid) }
            a
        })
      }
      r
    }

    private def locOf(b: BaseT): Option[DebugLocation] = readLoc(b.getLoc)

    private def readLoc(loc: DebugLocationT): Option[DebugLocation] = {
      if (loc == null) return None
      val dl = DebugLocation(orEmpty(loc.getFilePath), loc.getFileId.toInt,
        readPos(loc.getBeginPos), readPos(loc.getEndPos), ArrayBuffer())
      val v = loc.getScope
      if (v != null) for (j <- v.indices) dl.scope += v(j)
      Some(dl)
    }

    private def readPos(p: PosT): Option[Pos] =
      if (p == null) None else Some(Pos(p.getLine.toInt, p.getColumn.toInt))

    private def orEmpty(s: String): String = if (s == null) "" else s

    private def readAnnoInfo(info: AnnoInfoT): Option[AnnoInfo] = {
      if (info == null) return None
      val ai = AnnoInfo(orEmpty(info.getMangledName), ArrayBuffer())
      val insts = Option(info.getAnnoInstances).getOrElse(Array.empty[CustomAnnoInstanceT])
      for (j <- insts.indices) {
        val inst = insts(j)
        val args = ArrayBuffer[String]()
        val argVals = Option(inst.getArgValues).getOrElse(Array.empty[String])
        for (k <- argVals.indices) args += orEmpty(argVals(k))
        ai.annoInstances += CustomAnnoInstance(orEmpty(inst.getAnnoClassName), args, readLoc(inst.getLoc))
      }
      Some(ai)
    }

    private def readStrings(v: Array[String]): ArrayBuffer[String] = {
      val r = ArrayBuffer[String]()
      if (v != null) for (j <- v.indices) r += orEmpty(v(j))
      r
    }

    private def readLongs(v: Array[Long]): ArrayBuffer[Long] = {
      val r = ArrayBuffer[Long]()
      if (v != null) for (j <- v.indices) r += v(j)
      r
    }

    private def orNull(a: Array[?]): Array[?] = if (a == null) Array.empty[AnyRef] else a

    private def readFuncSigInfo(info: FuncSigInfoT): Option[FuncSigInfo] = {
      if (info == null) return None
      val si = FuncSigInfo(orEmpty(info.getFuncName), null)
      deferred += { () =>
        si.funcType = ty(info.getFuncType)
        for (j <- orNull(info.getGenericTypeParams).indices) si.genericTypeParams += ty(info.getGenericTypeParams()(j))
      }
      Some(si)
    }

    private def readMemberVarInfo(m: MemberVarInfoT): MemberVarInfo = {
      val mv = MemberVarInfo(orEmpty(m.getName), orEmpty(m.getRawMangledName), null, m.getAttributes,
        readLoc(m.getLoc), readAnnoInfo(m.getAnnoInfo), None, None)
      deferred += { () =>
        mv.tpe = ty(m.getType)
        mv.initializerFunc = vref[Func](m.getInitializerFunc)
        mv.outerDef = defRef(m.getOuterDef)
      }
      mv
    }

    private def readVTable(v: VTableInTypeT): VTableInType = {
      val vt = VTableInType(null, ArrayBuffer())
      deferred += { () => vt.srcParentType = ty(v.getSrcParentType) }
      val methods = Option(v.getVirtualMethods).getOrElse(Array.empty[VirtualMethodInfoT])
      for (j <- methods.indices) {
        val m = methods(j)
        val mi = VirtualMethodInfo(orEmpty(m.getFuncName), null, ArrayBuffer(), None, m.getAttributes,
          null, null, null)
        deferred += { () =>
          mi.sigType = ty(m.getSigType)
          mi.originalType = ty(m.getOriginalType)
          mi.parentType = ty(m.getParentType)
          mi.returnType = ty(m.getReturnType)
          mi.instance = vref[Func](m.getInstance)
          for (k <- orNull(m.getMethodGenericTypeParams).indices) {
            mi.methodGenericTypeParams += ty(m.getMethodGenericTypeParams()(k))
          }
        }
        vt.virtualMethods += mi
      }
      vt
    }

    private def readEnumCtors(d: com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.EnumDefT): ArrayBuffer[EnumCtorInfo] = {
      val r = ArrayBuffer[EnumCtorInfo]()
      val ctors = Option(d.getCtors).getOrElse(Array.empty[EnumCtorInfoT])
      for (j <- ctors.indices) {
        val c = ctors(j)
        val ci = EnumCtorInfo(orEmpty(c.getSrcCodeName), orEmpty(c.getMangledName), null, readAnnoInfo(c.getAnnoInfo))
        deferred += { () => ci.funcType = ty(c.getFuncType) }
        r += ci
      }
      r
    }
  }
}
