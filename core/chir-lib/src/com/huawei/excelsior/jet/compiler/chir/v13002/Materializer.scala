/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v13002

import com.google.flatbuffers.{IntVector, LongVector, StringVector}
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.{
  AllocateBase => FAllocateBase, AnnoInfo => FAnnoInfo, Annotation => FAnnotation, ApplyBase => FApplyBase,
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
}
import com.huawei.excelsior.jet.compiler.chir.model
import com.huawei.excelsior.jet.compiler.chir.model.*

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer

/** Materializes a v13002 .chir file into a mutable [[model.Package]]
  * (literal read of the flatbuffer, references wired by 1-based ids).
  */
object Materializer13002 {

  def load(bytes: Array[Byte]): Package = new Impl(bytes).load()

  private final class Impl(bytes: Array[Byte]) {
    private val root = CHIRPackage.getRootAsCHIRPackage(ByteBuffer.wrap(bytes))

    private val pkg = new Package()
    private val deferred = ArrayBuffer[() => Unit]()

    private val types = new Array[Type](root.typesLength())
    private val values = new Array[Value](root.valuesLength())
    private val exprs = new Array[Expression](root.exprsLength())
    private val defs = new Array[CustomTypeDef](root.defsLength())

    def load(): Package = {
      pkg.name = root.name()
      pkg.accessLevel = PackageAccessLevel.fromOrdinal(root.pkgAccessLevel())
      pkg.phase = Phase.fromOrdinal(root.phase())

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
      for (i <- 0 until root.typesLength()) {
        val id = i + 1
        root.typesType(i) match {
          case TypeElem.GenericType =>
            val t = root.types(new FGenericType(), i).asInstanceOf[FGenericType]
            val gt = new GenericType(orEmpty(t.identifier()), orEmpty(t.srcCodeIdentifier()))
            deferred += { () =>
              for (j <- 0 until t.upperBoundsLength()) gt.upperBounds += ty(t.upperBounds(j))
            }
            types(i) = gt
          case TypeElem.FuncType =>
            val t = root.types(new FFuncType(), i).asInstanceOf[FFuncType]
            val ft = new FuncType(t.isCfuncType(), t.hasVarArg(), ArrayBuffer())
            deferred += { () =>
              for (j <- 0 until t.base().argTysLength()) ft.argTys += ty(t.base().argTys(j))
            }
            types(i) = ft
          case TypeElem.CustomType =>
            val t = root.types(new FCustomType(), i).asInstanceOf[FCustomType]
            val ct = new CustomType(null)
            deferred += { () =>
              ct.customTypeDef = defRef(t.customTypeDef())
              for (j <- 0 until t.base().argTysLength()) ct.argTys += ty(t.base().argTys(j))
            }
            types(i) = ct
          case TypeElem.RawArrayType =>
            val t = root.types(new FRawArrayType(), i).asInstanceOf[FRawArrayType]
            val rt = new RawArrayType(null, t.dims())
            deferred += { () => rt.elementType = ty(t.base().argTys(0)) }
            types(i) = rt
          case TypeElem.VArrayType =>
            val t = root.types(new FVArrayType(), i).asInstanceOf[FVArrayType]
            val vt = new VArrayType(null, t.size())
            deferred += { () => vt.elementType = ty(t.base().argTys(0)) }
            types(i) = vt
          case TypeElem.Type =>
            val t = root.types(new FType(), i).asInstanceOf[FType]
            // wildcard Type table: builtins + Tuple/CPointer/Ref/Box composites
            val kind = TypeKind.fromOrdinal(t.kind())
            val mt = TypeImpl(kind, ArrayBuffer())
            if (kind == TypeKind.Tuple || kind == TypeKind.CPointer || kind == TypeKind.Ref || kind == TypeKind.Box) {
              deferred += { () =>
                for (j <- 0 until t.argTysLength()) mt.argTys += ty(t.argTys(j))
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
      for (i <- 0 until root.valuesLength()) {
        val id = i + 1
        values(i) = root.valuesType(i) match {
          case ValueElem.BoolLiteral =>
            val v = root.values(new FBoolLiteral(), i).asInstanceOf[FBoolLiteral]
            val vb = v.base().base() // Value table
            val b = vb.base() // Base table
            new BoolLiteral(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), v.`val`())
          case ValueElem.RuneLiteral =>
            val v = root.values(new FRuneLiteral(), i).asInstanceOf[FRuneLiteral]
            val vb = v.base().base()
            val b = vb.base()
            new RuneLiteral(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), v.`val`())
          case ValueElem.StringLiteral =>
            val v = root.values(new FStringLiteral(), i).asInstanceOf[FStringLiteral]
            val vb = v.base().base()
            val b = vb.base()
            new StringLiteral(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), orEmpty(v.`val`()))
          case ValueElem.IntLiteral =>
            val v = root.values(new FIntLiteral(), i).asInstanceOf[FIntLiteral]
            val vb = v.base().base()
            val b = vb.base()
            new IntLiteral(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), v.`val`())
          case ValueElem.FloatLiteral =>
            val v = root.values(new FFloatLiteral(), i).asInstanceOf[FFloatLiteral]
            val vb = v.base().base()
            val b = vb.base()
            new FloatLiteral(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), v.`val`())
          case ValueElem.UnitLiteral =>
            val v = root.values(new FUnitLiteral(), i).asInstanceOf[FUnitLiteral]
            val vb = v.base().base()
            val b = vb.base()
            new UnitLiteral(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()))
          case ValueElem.NullLiteral =>
            val v = root.values(new FNullLiteral(), i).asInstanceOf[FNullLiteral]
            val vb = v.base().base()
            val b = vb.base()
            new NullLiteral(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()))
          case ValueElem.Parameter =>
            val v = root.values(new FParameter(), i).asInstanceOf[FParameter]
            val vb = v.base()
            val b = vb.base()
            val p = new Parameter(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), None, None, orEmpty(v.srcCodeIdentifier()),
              readAnnoInfo(v.annoInfo()))
            deferred += { () =>
              p.ownedFunc = vref[Func](v.ownedFunc())
              p.ownedLambda = eref[LambdaExpr](v.ownedLambda())
            }
            p
          case ValueElem.LocalVar =>
            val v = root.values(new FLocalVar(), i).asInstanceOf[FLocalVar]
            val vb = v.base()
            val b = vb.base()
            val lv = new LocalVar(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), None, v.isRetVal(), orEmpty(v.srcCodeIdentifier()))
            deferred += { () => lv.associatedExpr = eref[Expression](v.associatedExpr()) }
            lv
          case ValueElem.GlobalVar =>
            val v = root.values(new FGlobalVar(), i).asInstanceOf[FGlobalVar]
            val gv = v.base() // GlobalValue
            val vb = gv.base() // Value
            val b = vb.base() // Base
            val g = new GlobalVar(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), orEmpty(gv.srcCodeIdentifier()), orEmpty(gv.rawMangledName()),
              orEmpty(gv.packageName()), None, readStrings(gv.featuresVector()),
              readAnnoInfo(gv.annoInfo()), None)
            deferred += { () =>
              g.declaredParent = defRef(gv.declaredParent())
              g.initializer = vref[Value](v.initializer())
            }
            g
          case ValueElem.Function =>
            val v = root.values(new FFunction(), i).asInstanceOf[FFunction]
            val gv = v.base()
            val vb = gv.base()
            val b = vb.base()
            val f = new Func(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), orEmpty(gv.srcCodeIdentifier()), orEmpty(gv.rawMangledName()),
              orEmpty(gv.packageName()), None, readStrings(gv.featuresVector()),
              readAnnoInfo(gv.annoInfo()), None, FuncKind.fromOrdinal(v.funcKind()),
              v.isFastNative(), v.isCffiwrapper(), readFuncSigInfo(v.originalLambdaInfo()),
              ArrayBuffer(), None, None, ArrayBuffer(), None, readLoc(v.propLoc()),
              v.localId(), v.blockId(), v.blockGroupId())
            deferred += { () =>
              f.declaredParent = defRef(gv.declaredParent())
              f.genericDecl = vref[Value](v.genericDecl())
              f.paramDftValHostFunc = vref[Func](v.paramDftValHostFunc())
              f.body = vref[BlockGroup](v.body())
              for (j <- 0 until v.paramsLength()) f.params += values(v.params(j).toInt - 1).asInstanceOf[Parameter]
              f.retVal = vref[LocalVar](v.retVal())
              for (j <- 0 until v.genericTypeParamsLength()) {
                f.genericTypeParams += ty(v.genericTypeParams(j)).asInstanceOf[GenericType]
              }
            }
            f
          case ValueElem.Block =>
            val v = root.values(new FBlock(), i).asInstanceOf[FBlock]
            val vb = v.base()
            val b = vb.base()
            val bl = new Block(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), None, ArrayBuffer(), ArrayBuffer(), v.isLandingPadBlock(),
              ArrayBuffer())
            deferred += { () =>
              bl.parentGroup = vref[BlockGroup](v.parentGroup())
              for (j <- 0 until v.exprsLength()) bl.exprs += exprs(v.exprs(j).toInt - 1)
              for (j <- 0 until v.predecessorsLength()) {
                bl.predecessors += values(v.predecessors(j).toInt - 1).asInstanceOf[Block]
              }
              for (j <- 0 until v.exceptionCatchListLength()) bl.exceptionCatchList += ty(v.exceptionCatchList(j))
            }
            bl
          case ValueElem.BlockGroup =>
            val v = root.values(new FBlockGroup(), i).asInstanceOf[FBlockGroup]
            val vb = v.base()
            val b = vb.base()
            val bg = new BlockGroup(annosOf(b), locOf(b), b.attributes(), tyOpt(vb.`type`()),
              orEmpty(vb.identifier()), None, ArrayBuffer(), None, None)
            deferred += { () =>
              bg.entryBlock = vref[Block](v.entryBlock())
              for (j <- 0 until v.blocksLength()) bg.blocks += values(v.blocks(j).toInt - 1).asInstanceOf[Block]
              bg.ownedFunc = vref[Func](v.ownedFunc())
              bg.ownedExpression = eref[LambdaExpr](v.ownedExpression())
            }
            bg
        }
        values(i).id = id
      }

    // ===============================================================================
    // exprs
    // ===============================================================================

    private def materializeExprs(): Unit =
      for (i <- 0 until root.exprsLength()) {
        val id = i + 1
        exprs(i) = root.exprsType(i) match {
          case ExpressionElem.Expression => wildcardExpr(i)
          case ExpressionElem.AllocateBase =>
            val e = root.exprs(new FAllocateBase(), i).asInstanceOf[FAllocateBase]
            val eb = e.base()
            val b = eb.base()
            val isTry = isTryKind(eb.kind())
            new AllocateExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), ty(e.allocatedType()),
              isTry, tryBlocks(eb, isTry))
          case ExpressionElem.UnaryExpressionBase =>
            val e = root.exprs(new FUnaryExpressionBase(), i).asInstanceOf[FUnaryExpressionBase]
            val eb = e.base()
            val b = eb.base()
            val isTry = isTryKind(eb.kind())
            val ops = opRefs(eb)
            new UnaryExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), unaryOp(eb.kind()),
              OverflowStrategy.fromOrdinal(e.overflowStrategy()), ops.head, isTry,
              ArrayBuffer.from(ops.drop(1).map(_.asInstanceOf[Block])))
          case ExpressionElem.BinaryExpressionBase =>
            val e = root.exprs(new FBinaryExpressionBase(), i).asInstanceOf[FBinaryExpressionBase]
            val eb = e.base()
            val b = eb.base()
            val isTry = isTryKind(eb.kind())
            val ops = opRefs(eb)
            new BinaryExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), binaryOp(eb.kind()),
              OverflowStrategy.fromOrdinal(e.overflowStrategy()), ops(0), ops(1), isTry,
              ArrayBuffer.from(ops.drop(2).map(_.asInstanceOf[Block])))
          case ExpressionElem.ApplyBase =>
            val e = root.exprs(new FApplyBase(), i).asInstanceOf[FApplyBase]
            val fc = e.base()
            val eb = fc.base()
            val b = eb.base()
            val isTry = isTryKind(eb.kind())
            val ops = opRefs(eb)
            new ApplyExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()),
              tyRefs(fc.instantiatedTypeArgsVector()), tyOpt(fc.objType()), ops.head,
              ArrayBuffer.from(ops.tail.dropRight(if (isTry) 2 else 0)),
              e.isSuperCall(), isTry, tryBlocks(eb, isTry))
          case ExpressionElem.InvokeBase =>
            val e = root.exprs(new FInvokeBase(), i).asInstanceOf[FInvokeBase]
            val fc = e.base()
            val eb = fc.base()
            val b = eb.base()
            val isTry = isTryKind(eb.kind())
            val ops = opRefs(eb)
            val nArgs = ops.length - 2 - (if (isTry) 2 else 0) // minus callee, caller (+ try tail)
            new InvokeExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()),
              tyRefs(fc.instantiatedTypeArgsVector()), tyOpt(fc.objType()), ops(0).asInstanceOf[Func],
              ops(1), ArrayBuffer.from(ops.slice(2, 2 + nArgs)),
              OverflowStrategy.fromOrdinal(e.overflowStrategy()), isTry, tryBlocks(eb, isTry))
          case ExpressionElem.IntrinsicBase =>
            val e = root.exprs(new FIntrinsicBase(), i).asInstanceOf[FIntrinsicBase]
            val fc = e.base()
            val eb = fc.base()
            val b = eb.base()
            val isTry = isTryKind(eb.kind())
            val ops = opRefs(eb)
            new IntrinsicExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()),
              tyRefs(fc.instantiatedTypeArgsVector()), tyOpt(fc.objType()), e.intrinsicKind(),
              ArrayBuffer.from(ops.dropRight(if (isTry) 2 else 0)), isTry, tryBlocks(eb, isTry))
          case ExpressionElem.NumericCastBase =>
            val e = root.exprs(new FNumericCastBase(), i).asInstanceOf[FNumericCastBase]
            val eb = e.base()
            val b = eb.base()
            val isTry = isTryKind(eb.kind())
            val ops = opRefs(eb)
            new NumericCastExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()),
              OverflowStrategy.fromOrdinal(e.overflowStrategy()), ops.head, isTry,
              ArrayBuffer.from(ops.drop(1).map(_.asInstanceOf[Block])))
          case ExpressionElem.RawArrayAllocateBase =>
            val e = root.exprs(new FRawArrayAllocateBase(), i).asInstanceOf[FRawArrayAllocateBase]
            val eb = e.base()
            val b = eb.base()
            val isTry = isTryKind(eb.kind())
            val ops = opRefs(eb)
            new RawArrayAllocateExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), ty(e.elementType()),
              ops.head, isTry, ArrayBuffer.from(ops.drop(1).map(_.asInstanceOf[Block])))
          case ExpressionElem.SpawnBase =>
            val e = root.exprs(new FSpawnBase(), i).asInstanceOf[FSpawnBase]
            val eb = e.base()
            val b = eb.base()
            val isTry = isTryKind(eb.kind())
            val ops = opRefs(eb)
            val spawnArg = ops.drop(1).headOption.filterNot(_.isInstanceOf[Block])
            new SpawnExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), ops.head, spawnArg,
              funcRef(e.executeClosure()), isTry, tryBlocks(eb, isTry))
          case ExpressionElem.Branch =>
            val e = root.exprs(new FBranch(), i).asInstanceOf[FBranch]
            val eb = e.base()
            val b = eb.base()
            val ops = opRefs(eb)
            new BranchExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), ops(0),
              ops(1).asInstanceOf[Block], ops(2).asInstanceOf[Block], e.sourceExpr())
          case ExpressionElem.MultiBranch =>
            val e = root.exprs(new FMultiBranch(), i).asInstanceOf[FMultiBranch]
            val eb = e.base()
            val b = eb.base()
            val ops = opRefs(eb)
            val caseValues = ArrayBuffer[Long]()
            for (j <- 0 until e.caseValuesLength()) caseValues += e.caseValues(j)
            new MultiBranchExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), ops.head, caseValues,
              ops(1).asInstanceOf[Block], ArrayBuffer.from(ops.drop(2).map(_.asInstanceOf[Block])))
          case ExpressionElem.GetElementRef =>
            val e = root.exprs(new FGetElementRef(), i).asInstanceOf[FGetElementRef]
            val eb = e.base()
            val b = eb.base()
            new GetElementRefExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), opRef(eb, 0),
              readLongs(e.pathVector()))
          case ExpressionElem.GetElementByName =>
            val e = root.exprs(new FGetElementByName(), i).asInstanceOf[FGetElementByName]
            val eb = e.base()
            val b = eb.base()
            new GetElementByNameExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), opRef(eb, 0),
              readStrings(e.fieldNamesVector()))
          case ExpressionElem.StoreElementRef =>
            val e = root.exprs(new FStoreElementRef(), i).asInstanceOf[FStoreElementRef]
            val eb = e.base()
            val b = eb.base()
            new StoreElementRefExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), opRef(eb, 0), opRef(eb, 1),
              readLongs(e.pathVector()))
          case ExpressionElem.StoreElementByName =>
            val e = root.exprs(new FStoreElementByName(), i).asInstanceOf[FStoreElementByName]
            val eb = e.base()
            val b = eb.base()
            new StoreElementByNameExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), opRef(eb, 0), opRef(eb, 1),
              readStrings(e.fieldNamesVector()))
          case ExpressionElem.InstanceOf =>
            val e = root.exprs(new FInstanceOf(), i).asInstanceOf[FInstanceOf]
            val eb = e.base()
            val b = eb.base()
            new InstanceOfExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), opRef(eb, 0),
              ty(e.targetType()))
          case ExpressionElem.Debug =>
            val e = root.exprs(new FDebug(), i).asInstanceOf[FDebug]
            val eb = e.base()
            val b = eb.base()
            val ops = opRefs(eb)
            new DebugExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()),
              orEmpty(e.srcCodeName()), ops(0))
          case ExpressionElem.Field =>
            val e = root.exprs(new FField(), i).asInstanceOf[FField]
            val eb = e.base()
            val b = eb.base()
            new FieldExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), opRef(eb, 0),
              readLongs(e.pathVector()))
          case ExpressionElem.FieldByName =>
            val e = root.exprs(new FFieldByName(), i).asInstanceOf[FFieldByName]
            val eb = e.base()
            val b = eb.base()
            new FieldByNameExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), opRef(eb, 0),
              readStrings(e.fieldNamesVector()))
          case ExpressionElem.GetInstantiateValue =>
            val e = root.exprs(new FGetInstantiateValue(), i).asInstanceOf[FGetInstantiateValue]
            val eb = e.base()
            val b = eb.base()
            new GetInstantiateValueExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), opRef(eb, 0),
              tyRefs(e.instantiateTypesVector()))
          case ExpressionElem.GetRTTIStatic =>
            val e = root.exprs(new FGetRTTIStatic(), i).asInstanceOf[FGetRTTIStatic]
            val eb = e.base()
            val b = eb.base()
            new GetRTTIStaticExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()), ty(e.rttiType()))
          case ExpressionElem.Lambda =>
            val e = root.exprs(new FLambda(), i).asInstanceOf[FLambda]
            val eb = e.base()
            val b = eb.base()
            val lam = new LambdaExpr(annosOf(b), locOf(b), b.attributes(),
              blockRef(eb.owner()), bgRefs(eb), localVarRef(eb.resultLocalVar()),
              ty(e.funcTy()).asInstanceOf[FuncType], e.isLocalFunc(), orEmpty(e.identifier()),
              orEmpty(e.srcCodeName()), ArrayBuffer(), tyRefs(e.genericTypeParamsVector()),
              None, None, e.isCompileTimeValue())
            // params/body/retVal are wired from the Parameter/BlockGroup values owning this lambda
            deferred += { () =>
              for (j <- 0 until e.paramsLength()) lam.params += values(e.params(j).toInt - 1).asInstanceOf[Parameter]
              lam.body = vref[BlockGroup](e.body())
              lam.retVal = vref[LocalVar](e.retVal())
            }
            lam
        }
        exprs(i).id = id
      }

    /** Wildcard Expression table entry: kind picks the concrete class. */
    private def wildcardExpr(i: Int): Expression = {
      val e = root.exprs(new FExpression(), i).asInstanceOf[FExpression]
      val b = e.base()
      val annosV = annosOf(b)
      val locV = locOf(b)
      val attrsV = b.attributes()
      val ownerV = blockRef(e.owner())
      val resultVarV = localVarRef(e.resultLocalVar())
      val bgsV = bgRefs(e)
      val ops = opRefs(e)
      e.kind() match {
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
          val d = root.exprs(new FDebug(), i).asInstanceOf[FDebug]
          new DebugExpr(annosV, locV, attrsV, ownerV, bgsV, resultVarV, orEmpty(d.srcCodeName()), ops(0))
        case other => sys.error(s"Unexpected wildcard Expression kind $other (expr #${i + 1})")
      }
    }

    // ===============================================================================
    // defs
    // ===============================================================================

    private def materializeDefs(): Unit =
      for (i <- 0 until root.defsLength()) {
        val id = i + 1
        val d: DefBase = root.defsType(i) match {
          case CustomTypeDefElem.EnumDef =>
            val d = root.defs(new FEnumDef(), i).asInstanceOf[FEnumDef]
            val b = d.base().base()
            new EnumDef(annosOf(b), locOf(b), b.attributes(), orEmpty(d.base().srcCodeIdentifier()),
              orEmpty(d.base().identifier()), orEmpty(d.base().packageName()),
              tyOpt(d.base().`type`()), None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(),
              ArrayBuffer(), readAnnoInfo(d.base().annoInfo()), ArrayBuffer(), None,
              readEnumCtors(d), d.nonExhaustive())
          case CustomTypeDefElem.StructDef =>
            val d = root.defs(new FStructDef(), i).asInstanceOf[FStructDef]
            val b = d.base().base()
            new StructDef(annosOf(b), locOf(b), b.attributes(), orEmpty(d.base().srcCodeIdentifier()),
              orEmpty(d.base().identifier()), orEmpty(d.base().packageName()),
              tyOpt(d.base().`type`()), None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(),
              ArrayBuffer(), readAnnoInfo(d.base().annoInfo()), ArrayBuffer(), None, d.isCstruct())
          case CustomTypeDefElem.ClassDef =>
            val d = root.defs(new FClassDef(), i).asInstanceOf[FClassDef]
            val b = d.base().base()
            val cd = new ClassDef(annosOf(b), locOf(b), b.attributes(), orEmpty(d.base().srcCodeIdentifier()),
              orEmpty(d.base().identifier()), orEmpty(d.base().packageName()),
              tyOpt(d.base().`type`()), None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(),
              ArrayBuffer(), readAnnoInfo(d.base().annoInfo()), ArrayBuffer(), None,
              d.isClass(), d.isAnnotation(), ArrayBuffer(), None)
            deferred += { () =>
              for (j <- 0 until d.annotationTargetsLength()) {
                cd.annotationTargets += values(d.annotationTargets(j).toInt - 1).asInstanceOf[GlobalVar]
              }
              cd.superClass = tyOpt(d.superClass())
            }
            cd
          case CustomTypeDefElem.ExtendDef =>
            val d = root.defs(new FExtendDef(), i).asInstanceOf[FExtendDef]
            val b = d.base().base()
            val ed = new ExtendDef(annosOf(b), locOf(b), b.attributes(), orEmpty(d.base().srcCodeIdentifier()),
              orEmpty(d.base().identifier()), orEmpty(d.base().packageName()),
              tyOpt(d.base().`type`()), None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(),
              ArrayBuffer(), readAnnoInfo(d.base().annoInfo()), ArrayBuffer(), None, None,
              ArrayBuffer())
            deferred += { () =>
              ed.extendedType = tyOpt(d.extendedType())
              for (j <- 0 until d.genericParamsLength()) ed.genericParams += ty(d.genericParams(j))
            }
            ed
        }
        d.id = id
        defs(i) = d

        // wire common contents
        val base = root.defsType(i) match {
          case CustomTypeDefElem.EnumDef => root.defs(new FEnumDef(), i).asInstanceOf[FEnumDef].base()
          case CustomTypeDefElem.StructDef => root.defs(new FStructDef(), i).asInstanceOf[FStructDef].base()
          case CustomTypeDefElem.ClassDef => root.defs(new FClassDef(), i).asInstanceOf[FClassDef].base()
          case CustomTypeDefElem.ExtendDef => root.defs(new FExtendDef(), i).asInstanceOf[FExtendDef].base()
        }
        deferred += { () => d.genericDecl = defRef(base.genericDecl()) }
        for (j <- 0 until base.methodsLength()) d.methods += values(base.methods(j).toInt - 1).asInstanceOf[Func]
        for (j <- 0 until base.implementedInterfacesLength()) {
          d.implementedInterfaces += ty(base.implementedInterfaces(j))
        }
        for (j <- 0 until base.staticMemberVarsLength()) {
          d.staticMemberVars += values(base.staticMemberVars(j).toInt - 1).asInstanceOf[GlobalVar]
        }
        d.instanceVarInitFunc = vref[Func](base.instanceVarInitFunc())
        for (j <- 0 until base.instanceMemberVarsLength()) {
          d.instanceMemberVars += readMemberVarInfo(base.instanceMemberVars(j))
        }
        for (j <- 0 until base.vtableLength()) d.vtable += readVTable(base.vtable(j))
      }

    private def wirePackageLevel(): Unit = {
      for (i <- 0 until root.valuesLength()) values(i) match {
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
      pkg.packageInitFunc = vref[Func](root.packageInitFunc())
      pkg.packageLiteralInitFunc = vref[Func](root.packageLiteralInitFunc())
    }

    // ===============================================================================
    // reference helpers (1-based flatbuffer ids, 0 = null)
    // ===============================================================================

    private def ty(id: Long): Type = types((id - 1).toInt)
    private def tyOpt(id: Long): Option[Type] = if (id == 0) None else Some(ty(id))

    private def tyRefs(v: IntVector): ArrayBuffer[Type] = {
      val r = ArrayBuffer[Type]()
      if (v != null) for (j <- 0 until v.length()) r += ty(v.get(j))
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
    private def opRefs(e: FExpression): ArrayBuffer[Value] = {
      val r = ArrayBuffer[Value]()
      val v = e.operandsVector()
      if (v != null) for (j <- 0 until v.length()) r += values((v.get(j) - 1).toInt)
      r
    }

    private def opRef(e: FExpression, j: Int): Value = values((e.operands(j) - 1).toInt)

    /** Last two operands of a Try expression are [normalBlock, exceptionBlock]. */
    private def tryBlocks(e: FExpression, isTry: Boolean): ArrayBuffer[Block] =
      if (!isTry) ArrayBuffer()
      else {
        val n = e.operandsLength()
        ArrayBuffer(values((e.operands(n - 2) - 1).toInt).asInstanceOf[Block],
          values((e.operands(n - 1) - 1).toInt).asInstanceOf[Block])
      }

    private def bgRefs(e: FExpression): ArrayBuffer[BlockGroup] = {
      val r = ArrayBuffer[BlockGroup]()
      val v = e.blockGroupsVector()
      if (v != null) for (j <- 0 until v.length()) r += values((v.get(j) - 1).toInt).asInstanceOf[BlockGroup]
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

    private def annosOf(b: FBase): ArrayBuffer[Annotation] = {
      val r = ArrayBuffer[Annotation]()
      for (j <- 0 until b.annosTypeLength()) {
        val obj: AnyRef = b.annosType(j) match {
          case FAnnotation.needCheckArrayBound => b.annos(new FNeedCheckArrayBound(), j)
          case FAnnotation.needCheckCast => b.annos(new FNeedCheckCast(), j)
          case FAnnotation.debugLocationInfoForWarning => b.annos(new FDebugLocation(), j)
          case FAnnotation.generatedFromForIn => b.annos(new FGeneratedFromForIn(), j)
          case FAnnotation.isAutoEnvClass => b.annos(new FIsAutoEnvClass(), j)
          case FAnnotation.isCapturedClassInCC => b.annos(new FIsCapturedClassInCC(), j)
          case FAnnotation.linkTypeInfo => b.annos(new FLinkTypeInfo(), j)
          case FAnnotation.skipCheck => b.annos(new FSkipCheck(), j)
          case FAnnotation.neverOverflowInfo => b.annos(new FNeverOverflowInfo(), j)
          case FAnnotation.enumCaseIndex => b.annos(new FEnumCaseIndex(), j)
          case FAnnotation.virMethodOffset => b.annos(new FVirMethodOffset(), j)
          case FAnnotation.wrappedRawMethod => b.annos(new FWrappedRawMethod(), j)
          case FAnnotation.overrideSrcFuncType => b.annos(new FOverrideSrcFuncType(), j)
          case _ => null
        }
        if (obj != null) r += (obj match {
          case o: FNeedCheckArrayBound => new model.NeedCheckArrayBound(o.need())
          case o: FNeedCheckCast => new model.NeedCheckCast(o.need())
          case o: FDebugLocation => new model.DebugLocationInfoForWarning(readLoc(o).orNull)
          case o: FGeneratedFromForIn => new model.GeneratedFromForIn(o.value())
          case o: FIsAutoEnvClass => new model.IsAutoEnvClass(o.value())
          case o: FIsCapturedClassInCC => new model.IsCapturedClassInCC(o.value())
          case o: FLinkTypeInfo => new model.LinkTypeInfo(Linkage.fromOrdinal(o.linkage()))
          case o: FSkipCheck => new model.SkipCheck(SkipKind.fromOrdinal(o.skipKind()))
          case o: FNeverOverflowInfo => new model.NeverOverflowInfo(o.neverOverflow())
          case o: FEnumCaseIndex => new model.EnumCaseIndex(o.index())
          case o: FVirMethodOffset => new model.VirMethodOffset(o.offset())
          case o: FWrappedRawMethod =>
            val a = new model.WrappedRawMethod(null)
            val vid = o.rawMethod()
            deferred += { () => a.rawMethod = values((vid - 1).toInt) }
            a
          case o: FOverrideSrcFuncType =>
            val a = new model.OverrideSrcFuncType(null)
            val tid = o.`type`()
            deferred += { () => a.tpe = ty(tid) }
            a
        })
      }
      r
    }

    private def locOf(b: FBase): Option[DebugLocation] = readLoc(b.loc())

    private def readLoc(loc: FDebugLocation): Option[DebugLocation] = {
      if (loc == null) return None
      val dl = DebugLocation(orEmpty(loc.filePath()), loc.fileId().toInt,
        readPos(loc.beginPos()), readPos(loc.endPos()), ArrayBuffer())
      val v = loc.scopeVector()
      if (v != null) for (j <- 0 until v.length()) dl.scope += v.get(j)
      Some(dl)
    }

    private def readPos(p: FPos): Option[Pos] =
      if (p == null) None else Some(Pos(p.line().toInt, p.column().toInt))

    private def orEmpty(s: String): String = if (s == null) "" else s

    private def readAnnoInfo(info: FAnnoInfo): Option[AnnoInfo] = {
      if (info == null) return None
      val ai = AnnoInfo(orEmpty(info.mangledName()), ArrayBuffer())
      for (j <- 0 until info.annoInstancesLength()) {
        val inst = info.annoInstances(j)
        val args = ArrayBuffer[String]()
        for (k <- 0 until inst.argValuesLength()) args += orEmpty(inst.argValues(k))
        ai.annoInstances += CustomAnnoInstance(orEmpty(inst.annoClassName()), args, readLoc(inst.loc()))
      }
      Some(ai)
    }

    private def readStrings(v: StringVector): ArrayBuffer[String] = {
      val r = ArrayBuffer[String]()
      if (v != null) for (j <- 0 until v.length()) r += orEmpty(v.get(j))
      r
    }

    private def readLongs(v: LongVector): ArrayBuffer[Long] = {
      val r = ArrayBuffer[Long]()
      if (v != null) for (j <- 0 until v.length()) r += v.get(j)
      r
    }

    private def readFuncSigInfo(info: FFuncSigInfo): Option[FuncSigInfo] = {
      if (info == null) return None
      val si = FuncSigInfo(orEmpty(info.funcName()), null)
      deferred += { () =>
        si.funcType = ty(info.funcType())
        for (j <- 0 until info.genericTypeParamsLength()) si.genericTypeParams += ty(info.genericTypeParams(j))
      }
      Some(si)
    }

    private def readMemberVarInfo(m: FMemberVarInfo): MemberVarInfo = {
      val mv = MemberVarInfo(orEmpty(m.name()), orEmpty(m.rawMangledName()), null, m.attributes(),
        readLoc(m.loc()), readAnnoInfo(m.annoInfo()), None, None)
      deferred += { () =>
        mv.tpe = ty(m.`type`())
        mv.initializerFunc = vref[Func](m.initializerFunc())
        mv.outerDef = defRef(m.outerDef())
      }
      mv
    }

    private def readVTable(v: FVTableInType): VTableInType = {
      val vt = VTableInType(null, ArrayBuffer())
      deferred += { () => vt.srcParentType = ty(v.srcParentType()) }
      for (j <- 0 until v.virtualMethodsLength()) {
        val m = v.virtualMethods(j)
        val mi = VirtualMethodInfo(orEmpty(m.funcName()), null, ArrayBuffer(), None, m.attributes(),
          null, null, null)
        deferred += { () =>
          mi.sigType = ty(m.sigType())
          mi.originalType = ty(m.originalType())
          mi.parentType = ty(m.parentType())
          mi.returnType = ty(m.returnType())
          mi.instance = vref[Func](m.instance())
          for (k <- 0 until m.methodGenericTypeParamsLength()) {
            mi.methodGenericTypeParams += ty(m.methodGenericTypeParams(k))
          }
        }
        vt.virtualMethods += mi
      }
      vt
    }

    private def readEnumCtors(d: FEnumDef): ArrayBuffer[EnumCtorInfo] = {
      val r = ArrayBuffer[EnumCtorInfo]()
      for (j <- 0 until d.ctorsLength()) {
        val c = d.ctors(j)
        val ci = EnumCtorInfo(orEmpty(c.srcCodeName()), orEmpty(c.mangledName()), null, readAnnoInfo(c.annoInfo()))
        deferred += { () => ci.funcType = ty(c.funcType()) }
        r += ci
      }
      r
    }
  }
}
