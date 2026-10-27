/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.model

import scala.collection.mutable.ArrayBuffer

/** Version-agnostic mutable CHIR model with full flatbuffer-schema field coverage.
  *
  * This is the common layer used by `cbc-patchgen` to modify deserialized CHIR and
  * serialize it back (mirroring the C++ CHIRBuilder-based patch generation).
  *
  * Design notes:
  *  - Entities store flatbuffer-shaped data (e.g. Try* expressions keep their two handler
  *    blocks as the last operand values); typed accessors are views over them.
  *    Materialization is literal, serialization writes what was read.
  *  - Ids (`var id`) are 1-based positions in the flatbuffer vectors, assigned by the
  *    materializer on load. They are transient (used for tests/debugging); the serializer
  *    renumbers everything in discovery order, like the C++ CHIRSerializer.
  *  - No use-lists: users of a value are found by scanning bodies on demand.
  *    Predecessor lists ARE maintained data (the serializer writes them).
  *  - Enum ordinals mirror the flatbuffer schema enum values exactly.
  */

// =====================================================================================
// Enums (mirror the flatbuffer schema enums; ordinals must match schema values)
// =====================================================================================

/** CHIRTypeKind */
enum TypeKind {
  case Invalid,
    Int8, Int16, Int32, Int64, IntNative,
    UInt8, UInt16, UInt32, UInt64, UIntNative,
    Float16, Float32, Float64,
    Rune, Boolean, Unit, Nothing, Void,
    Tuple, Struct, Enum, Func, Class,
    RawArray, VArray, CPointer, CString, Generic, Ref, Box, This
}

/** Linkage (int16) */
enum Linkage {
  case WeakOdr, External, Internal, LinkonceOdr, ExternalWeak
}

/** SkipKind */
enum SkipKind {
  case NoSkip, SkipDceWarning, SkipForinExit, SkipVic
}

/** OverflowStrategy */
enum OverflowStrategy {
  case Na, Wrapping, Throwing, Saturating
}

/** ConstantValueKind */
enum ConstantValueKind {
  case Bool, Rune, Int, Float, String, Unit, Null, Func
}

/** ValueKind */
enum ValueKind {
  case Literal, GlobalVar, Parameter, LocalVar, Func, Block, BlockGroup
}

/** FuncKind */
enum FuncKind {
  case Default, Getter, Setter, Lambda, ClassCtor, PrimalClassCtor, StructCtor, PrimalStructCtor,
    GlobalVarInit, Finalizer, MainEntry, AnnoFactory, Macro, DefaultParameter, InstanceVarInit
}

/** CustomDefKind */
enum CustomDefKind {
  case Struct, Enum, Class, Extend
}

/** PackageAccessLevel */
enum PackageAccessLevel {
  case Invalid, Internal, Protected, Public
}

/** Phase */
enum Phase {
  case Raw, Opt, Plugin, AnalysisForCjlint
}

/** SourceExpr — raw values mirror the C++ enum (which has MATCH_CASE=8, OTHER=9;
  * the chir-lib schema enum stops at OTHER=8, so raw ints are used).
  * Branch.sourceExpr defaults to 0 (IF_EXPR) when absent; MultiBranch.sourceExpr
  * (not covered by the chir-lib schema) is treated as OTHER(9) — the C++ toolchain
  * never emits MultiBranch in OPT-phase .chir files.
  */
object SourceExprTag {
  val IfExpr: Int = 0
  val WhileExpr: Int = 1
  val DoWhileExpr: Int = 2
  val MatchExpr: Int = 3
  val IfLetOrWhileLet: Int = 4
  val Quest: Int = 5
  val Binary: Int = 6
  val ForInExpr: Int = 7
  val MatchCase: Int = 8
  val Other: Int = 9
}

/** CHIRExprKind — full mirror of the schema enum (raw value = ordinal). */
enum ExprKind {
  case Invalid,
    // terminator
    Goto, Branch, MultiBranch, Exit,
    TryApply, TryInvoke, TryIntrinsic, RaiseException, TrySpawn, TryNumericCast, TryAllocate,
    TryRawArrayAllocate, TryNeg, TryAdd, TrySub, TryMul, TryDiv, TryMod, TryExp, TryLShift, TryRShift,
    // unary expr
    Neg, Not, BitNot,
    // binary expr
    Add, Sub, Mul, Div, Mod, Exp, LShift, RShift, BitAnd, BitOr, BitXor, LT, GT, LE, GE, Equal, NotEqual, And, Or,
    // type cast
    StaticCast, Box, UnboxToValue, UnboxToRef, NumericCast, CastToConcrete, CastToGeneric,
    // memory expr
    Allocate, Load, Store, GetElementByName, GetElementRef, StoreElementByName, StoreElementRef,
    Field, FieldByName,
    // array
    RawArrayAllocate, RawArrayLiteralInit, RawArrayInitByValue, VArrayExpr, VArrayBuilder,
    // others
    Constant, Debug, Tuple, InstanceOf, GetException, Spawn, Lambda, GetInstantiateValue,
    Apply, Invoke, Intrinsic, GetRtti, GetRttiStatic
}

/** IntrinsicKind is too large to mirror as an enum; raw values are stored and compared. */
object IntrinsicKind {
  val BeginCatch: Int = 404
  val Preinitialize: Int = 426
}

/** Binary expression op kinds (derived from CHIRExprKind; Try* variants are `isTry`). */
enum BinaryOp {
  case Add, Sub, Mul, Div, Mod, Exp, LShift, RShift, And, Or, Xor, Lt, Gt, Le, Ge, Eq, NotEq
}

/** Unary expression op kinds (derived from CHIRExprKind). */
enum UnaryOp {
  case Neg, Not, BitNot
}

/** Cast kinds stored in the wildcard Expression table kind. */
enum CastOp {
  case StaticCast, Box, UnboxToValue, UnboxToRef, CastToConcrete, CastToGeneric
}

/** Flatbuffer ExpressionElem union tag (raw value = ordinal). */
enum ExprTable {
  case Expression, AllocateBase, ApplyBase, BinaryExpressionBase, Branch, Debug, Field, FieldByName,
    GetElementByName, GetElementRef, GetInstantiateValue, GetRTTIStatic, InstanceOf, IntrinsicBase,
    InvokeBase, Lambda, MultiBranch, NumericCastBase, RawArrayAllocateBase, SpawnBase,
    StoreElementByName, StoreElementRef, UnaryExpressionBase
}

// =====================================================================================
// Types
// =====================================================================================

sealed trait Type {
  var id: Int = 0
}

/** Builtin/primitive types (INT8..THIS), plus composite "generic" Type table entries:
  * Tuple, CPointer, Ref, Box (kind + argTys).
  */
final class TypeImpl(var kind: TypeKind, val argTys: ArrayBuffer[Type] = ArrayBuffer()) extends Type

final class RawArrayType(var elementType: Type, var dims: Long) extends Type

final class VArrayType(var elementType: Type, var size: Long) extends Type

/** argTys = param types :+ return type (as in the flatbuffer FuncType). */
final class FuncType(var isCFuncType: Boolean, var hasVarArg: Boolean, val argTys: ArrayBuffer[Type]) extends Type {
  def paramTypes: ArrayBuffer[Type] = argTys.dropRight(1)
  def returnType: Type = argTys.last
  def receiverType: Type = argTys.head
  def paramTypesWithoutReceiver: ArrayBuffer[Type] = argTys.dropRight(1).drop(1)
}

final class CustomType(var customTypeDef: Option[CustomTypeDef] = None) extends Type {
  /** Generic instantiation arguments (flatbuffer argTys). */
  val argTys: ArrayBuffer[Type] = ArrayBuffer()
}

final class GenericType(var identifier: String, var srcCodeIdentifier: String,
                        val upperBounds: ArrayBuffer[Type] = ArrayBuffer()) extends Type

// =====================================================================================
// Annotations (union Annotation)
// =====================================================================================

sealed trait Annotation

final class NeedCheckArrayBound(var need: Boolean) extends Annotation
final class NeedCheckCast(var need: Boolean) extends Annotation
final class DebugLocationInfoForWarning(val loc: DebugLocation) extends Annotation
final class GeneratedFromForIn(var value: Boolean) extends Annotation
final class IsAutoEnvClass(var value: Boolean) extends Annotation
final class IsCapturedClassInCC(var value: Boolean) extends Annotation
final class LinkTypeInfo(var linkage: Linkage) extends Annotation
final class SkipCheck(var skipKind: SkipKind) extends Annotation
final class NeverOverflowInfo(var neverOverflow: Boolean) extends Annotation
final class EnumCaseIndex(var index: Long) extends Annotation
final class VirMethodOffset(var offset: Long) extends Annotation
final class WrappedRawMethod(var rawMethod: Value) extends Annotation
final class OverrideSrcFuncType(var tpe: Type) extends Annotation

// =====================================================================================
// Misc structures
// =====================================================================================

final class Pos(var line: Int, var column: Int)

final class DebugLocation(var filePath: String, var fileId: Int,
                          var beginPos: Option[Pos], var endPos: Option[Pos],
                          var scope: ArrayBuffer[Int] = ArrayBuffer()) {
  def this() = this("", 0, None, None)
}

final class FuncSigInfo(var funcName: String, var funcType: Type,
                        var genericTypeParams: ArrayBuffer[Type] = ArrayBuffer())

final class CustomAnnoInstance(var annoClassName: String, var argValues: ArrayBuffer[String],
                               var loc: Option[DebugLocation])

final class AnnoInfo(var mangledName: String, var annoInstances: ArrayBuffer[CustomAnnoInstance] = ArrayBuffer())

final class MemberVarInfo(var name: String, var rawMangledName: String, var tpe: Type, var attributes: Long,
                          var loc: Option[DebugLocation], var annoInfo: Option[AnnoInfo],
                          var initializerFunc: Option[Func], var outerDef: Option[CustomTypeDef])

final class VirtualMethodInfo(var funcName: String, var sigType: Type,
                              var methodGenericTypeParams: ArrayBuffer[Type], var instance: Option[Func],
                              var attributes: Long, var originalType: Type, var parentType: Type,
                              var returnType: Type)

final class VTableInType(var srcParentType: Type, var virtualMethods: ArrayBuffer[VirtualMethodInfo])

final class EnumCtorInfo(var srcCodeName: String, var mangledName: String, var funcType: Type,
                         var annoInfo: Option[AnnoInfo])

// =====================================================================================
// Values
// =====================================================================================

sealed trait Value {
  var id: Int = 0
}

/** Common part of every flatbuffer `Value` table entry (base: Base, type, identifier, kind). */
sealed abstract class ValueBase(var annos: ArrayBuffer[Annotation], var loc: Option[DebugLocation],
                                var attributes: Long, var tpe: Option[Type], var identifier: String)
  extends Value {
  def kind: ValueKind
}

sealed abstract class Literal(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation],
                              attributes: Long, tpe: Option[Type], identifier: String)
  extends ValueBase(annos, loc, attributes, tpe, identifier) {
  def kind: ValueKind = ValueKind.Literal
}

final class BoolLiteral(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                        tpe: Option[Type], identifier: String, var value: Boolean)
  extends Literal(annos, loc, attributes, tpe, identifier)

final class RuneLiteral(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                        tpe: Option[Type], identifier: String, var value: Long)
  extends Literal(annos, loc, attributes, tpe, identifier)

final class StringLiteral(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                          tpe: Option[Type], identifier: String, var value: String)
  extends Literal(annos, loc, attributes, tpe, identifier)

final class IntLiteral(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                       tpe: Option[Type], identifier: String, var value: Long)
  extends Literal(annos, loc, attributes, tpe, identifier)

final class FloatLiteral(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                         tpe: Option[Type], identifier: String, var value: Double)
  extends Literal(annos, loc, attributes, tpe, identifier)

final class UnitLiteral(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                        tpe: Option[Type], identifier: String)
  extends Literal(annos, loc, attributes, tpe, identifier)

final class NullLiteral(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                        tpe: Option[Type], identifier: String)
  extends Literal(annos, loc, attributes, tpe, identifier)

final class Parameter(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      tpe: Option[Type], identifier: String,
                      var ownedFunc: Option[Func], var ownedLambda: Option[LambdaExpr],
                      var srcCodeIdentifier: String, var annoInfo: Option[AnnoInfo])
  extends ValueBase(annos, loc, attributes, tpe, identifier) {
  def kind: ValueKind = ValueKind.Parameter
}

final class LocalVar(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                     tpe: Option[Type], identifier: String,
                     var associatedExpr: Option[Expression], var isRetVal: Boolean,
                     var srcCodeIdentifier: String)
  extends ValueBase(annos, loc, attributes, tpe, identifier) {
  def kind: ValueKind = ValueKind.LocalVar
}

/** Common part of GlobalVar/Func (flatbuffer GlobalValue table). */
sealed abstract class GlobalValueBase(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation],
                                      attributes: Long, tpe: Option[Type], identifier: String,
                                      var srcCodeIdentifier: String, var rawMangledName: String,
                                      var packageName: String, var declaredParent: Option[CustomTypeDef],
                                      var features: ArrayBuffer[String], var annoInfo: Option[AnnoInfo])
  extends ValueBase(annos, loc, attributes, tpe, identifier)

final class GlobalVar(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      tpe: Option[Type], identifier: String, srcCodeIdentifier: String,
                      rawMangledName: String, packageName: String, declaredParent: Option[CustomTypeDef],
                      features: ArrayBuffer[String], annoInfo: Option[AnnoInfo],
                      var initializer: Option[Value])
  extends GlobalValueBase(annos, loc, attributes, tpe, identifier, srcCodeIdentifier, rawMangledName,
    packageName, declaredParent, features, annoInfo) {
  def kind: ValueKind = ValueKind.GlobalVar
}

final class Func(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                 tpe: Option[Type], identifier: String, srcCodeIdentifier: String,
                 rawMangledName: String, packageName: String, declaredParent: Option[CustomTypeDef],
                 features: ArrayBuffer[String], annoInfo: Option[AnnoInfo],
                 var genericDecl: Option[Value], var funcKind: FuncKind,
                 var isFastNative: Boolean, var isCFFIWrapper: Boolean,
                 var originalLambdaInfo: Option[FuncSigInfo],
                 var genericTypeParams: ArrayBuffer[GenericType],
                 var paramDftValHostFunc: Option[Func],
                 var body: Option[BlockGroup], var params: ArrayBuffer[Parameter],
                 var retVal: Option[LocalVar], var propLoc: Option[DebugLocation],
                 var localId: Long, var blockId: Long, var blockGroupId: Long)
  extends GlobalValueBase(annos, loc, attributes, tpe, identifier, srcCodeIdentifier, rawMangledName,
    packageName, declaredParent, features, annoInfo) {
  def kind: ValueKind = ValueKind.Func

  def isImported: Boolean = (attributes & (1L << 14)) != 0L // Attribute.IMPORTED
  def isLambda: Boolean = funcKind == FuncKind.Lambda

  /** Mints a fresh local var id, like C++ Function::GenerateLocalId. */
  def generateLocalId(): Long = { localId += 1; localId }

  def generateBlockId(): Long = { blockId += 1; blockId }

  def generateBlockGroupId(): Long = { blockGroupId += 1; blockGroupId }
}

final class Block(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                  tpe: Option[Type], identifier: String,
                  var parentGroup: Option[BlockGroup], var exprs: ArrayBuffer[Expression],
                  var predecessors: ArrayBuffer[Block], var isLandingPadBlock: Boolean,
                  var exceptionCatchList: ArrayBuffer[Type])
  extends ValueBase(annos, loc, attributes, tpe, identifier) {
  def kind: ValueKind = ValueKind.Block
}

final class BlockGroup(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                       tpe: Option[Type], identifier: String,
                       var entryBlock: Option[Block], var blocks: ArrayBuffer[Block],
                       var ownedFunc: Option[Func], var ownedExpression: Option[LambdaExpr])
  extends ValueBase(annos, loc, attributes, tpe, identifier) {
  def kind: ValueKind = ValueKind.BlockGroup
}

// =====================================================================================
// Expressions
// =====================================================================================

sealed trait Expression {
  var id: Int = 0

  def annos: ArrayBuffer[Annotation]
  def loc: Option[DebugLocation]
  def attributes: Long
  def owner: Option[Block]
  def blockGroups: ArrayBuffer[BlockGroup]
  def resultVar: Option[LocalVar]

  def isTerminator: Boolean = this match {
    case _: GotoExpr | _: BranchExpr | _: MultiBranchExpr | _: RaiseExceptionExpr | _: ExitExpr => true
    case e: AllocateExpr => e.isTry
    case e: ApplyExpr => e.isTry
    case e: UnaryExpr => e.isTry
    case e: BinaryExpr => e.isTry
    case e: IntrinsicExpr => e.isTry
    case e: InvokeExpr => e.isTry
    case e: NumericCastExpr => e.isTry
    case e: RawArrayAllocateExpr => e.isTry
    case e: SpawnExpr => e.isTry
    case _ => false
  }

  def successors: Seq[Block] = this match {
    case e: GotoExpr => Seq(e.destination)
    case e: BranchExpr => Seq(e.trueBlock, e.falseBlock)
    case e: MultiBranchExpr => e.defaultBlock +: e.normalBlocks.toSeq
    case e: RaiseExceptionExpr => e.exceptionBlock.toSeq
    case e: AllocateExpr if e.isTry => e.handlerBlocks.toSeq
    case e: ApplyExpr if e.isTry => e.handlerBlocks.toSeq
    case e: UnaryExpr if e.isTry => e.handlerBlocks.toSeq
    case e: BinaryExpr if e.isTry => e.handlerBlocks.toSeq
    case e: IntrinsicExpr if e.isTry => e.handlerBlocks.toSeq
    case e: InvokeExpr if e.isTry => e.handlerBlocks.toSeq
    case e: NumericCastExpr if e.isTry => e.handlerBlocks.toSeq
    case e: RawArrayAllocateExpr if e.isTry => e.handlerBlocks.toSeq
    case e: SpawnExpr if e.isTry => e.handlerBlocks.toSeq
    case _ => Seq.empty
  }
}

/** Common flatbuffer `Expression` table part (base: Base, owner, resultLocalVar). */
sealed abstract class ExprBase(var annos: ArrayBuffer[Annotation], var loc: Option[DebugLocation],
                               var attributes: Long, var owner: Option[Block],
                               var blockGroups: ArrayBuffer[BlockGroup],
                               var resultVar: Option[LocalVar]) extends Expression

// --- wildcard Expression table entries (raw kind picks the concrete class) ---

final class GotoExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                     owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                     resultVar: Option[LocalVar], var destination: Block)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class ExitExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                     owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                     resultVar: Option[LocalVar])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class RaiseExceptionExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                               owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                               resultVar: Option[LocalVar], var exceptionValue: Value,
                               var exceptionBlock: Option[Block])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class LoadExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                     owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                     resultVar: Option[LocalVar], var location: Value)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class StoreExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                      resultVar: Option[LocalVar], var value: Value, var location: Value)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class ConstantExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                         owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                         resultVar: Option[LocalVar], var literal: Value)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class TupleExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                      resultVar: Option[LocalVar], var elementValues: ArrayBuffer[Value])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class GetExceptionExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                             owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                             resultVar: Option[LocalVar])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class GetRTTIExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                        owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                        resultVar: Option[LocalVar], var obj: Value)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class VArrayExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                       owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                       resultVar: Option[LocalVar], var elementValues: ArrayBuffer[Value])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class VArrayBuilderExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                              owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                              resultVar: Option[LocalVar], var size: Value, var initValue: Value,
                              var initializer: Value)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class RawArrayLiteralInitExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation],
                                    attributes: Long, owner: Option[Block],
                                    blockGroups: ArrayBuffer[BlockGroup], resultVar: Option[LocalVar],
                                    var array: Value, var elementValues: ArrayBuffer[Value])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class RawArrayInitByValueExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation],
                                    attributes: Long, owner: Option[Block],
                                    blockGroups: ArrayBuffer[BlockGroup], resultVar: Option[LocalVar],
                                    var array: Value, var size: Value, var initValue: Value)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

// --- cast kinds stored in the wildcard Expression table kind ---

final class CastExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                     owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                     resultVar: Option[LocalVar], val op: CastOp, var value: Value)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar) {
  /** For casts, the target type is the result var's type (as in chir-lib CastImpl). */
  def targetTpe: Option[Type] = resultVar.flatMap(_.tpe)
}

// --- dedicated tables ---

final class AllocateExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                         owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                         resultVar: Option[LocalVar], var allocatedType: Type, var isTry: Boolean,
                         var handlerBlocks: ArrayBuffer[Block] = ArrayBuffer())
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class UnaryExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                      resultVar: Option[LocalVar], val op: UnaryOp,
                      var overflowStrategy: OverflowStrategy, var operand: Value, var isTry: Boolean,
                      var handlerBlocks: ArrayBuffer[Block] = ArrayBuffer())
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class BinaryExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                       owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                       resultVar: Option[LocalVar], val op: BinaryOp,
                       var overflowStrategy: OverflowStrategy,
                       var leftOperand: Value, var rightOperand: Value, var isTry: Boolean,
                       var handlerBlocks: ArrayBuffer[Block] = ArrayBuffer())
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

/** Common part of Apply/Invoke/Intrinsic (flatbuffer FuncCall table). */
sealed abstract class FuncCallExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation],
                                   attributes: Long, owner: Option[Block],
                                   blockGroups: ArrayBuffer[BlockGroup], resultVar: Option[LocalVar],
                                   var instantiatedTypeArgs: ArrayBuffer[Type],
                                   var objType: Option[Type])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class ApplyExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                      resultVar: Option[LocalVar], instantiatedTypeArgs: ArrayBuffer[Type],
                      objType: Option[Type], var callee: Value, var args: ArrayBuffer[Value],
                      var isSuperCall: Boolean, var isTry: Boolean,
                      var handlerBlocks: ArrayBuffer[Block] = ArrayBuffer())
  extends FuncCallExpr(annos, loc, attributes, owner, blockGroups, resultVar, instantiatedTypeArgs, objType) {
  def thisArg: Option[Value] = args.headOption
}

final class InvokeExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                       owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                       resultVar: Option[LocalVar], instantiatedTypeArgs: ArrayBuffer[Type],
                       objType: Option[Type], var callee: Func, var args: ArrayBuffer[Value],
                       var overflowStrategy: OverflowStrategy, var isTry: Boolean,
                       var handlerBlocks: ArrayBuffer[Block] = ArrayBuffer())
  extends FuncCallExpr(annos, loc, attributes, owner, blockGroups, resultVar, instantiatedTypeArgs, objType) {
  def thisArg: Option[Value] = args.headOption
}

final class IntrinsicExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                          owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                          resultVar: Option[LocalVar], instantiatedTypeArgs: ArrayBuffer[Type],
                          objType: Option[Type], var intrinsicKind: Int, var args: ArrayBuffer[Value],
                          var isTry: Boolean, var handlerBlocks: ArrayBuffer[Block] = ArrayBuffer())
  extends FuncCallExpr(annos, loc, attributes, owner, blockGroups, resultVar, instantiatedTypeArgs, objType)

final class NumericCastExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                            owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                            resultVar: Option[LocalVar], var overflowStrategy: OverflowStrategy,
                            var value: Value, var isTry: Boolean,
                            var handlerBlocks: ArrayBuffer[Block] = ArrayBuffer())
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar) {
  def targetTpe: Option[Type] = resultVar.flatMap(_.tpe)
}

final class RawArrayAllocateExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                                 owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                                 resultVar: Option[LocalVar], var elementType: Type, var size: Value,
                                 var isTry: Boolean, var handlerBlocks: ArrayBuffer[Block] = ArrayBuffer())
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class SpawnExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                      resultVar: Option[LocalVar], var obj: Value,
                      var spawnArg: Option[Value], var executeClosure: Option[Func], var isTry: Boolean,
                      var handlerBlocks: ArrayBuffer[Block] = ArrayBuffer())
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class BranchExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                       owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                       resultVar: Option[LocalVar], var condition: Value, var trueBlock: Block,
                       var falseBlock: Block, var sourceExpr: Int)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class MultiBranchExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                            owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                            resultVar: Option[LocalVar], var condition: Value,
                            var caseValues: ArrayBuffer[Long], var defaultBlock: Block,
                            var normalBlocks: ArrayBuffer[Block])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class GetElementRefExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                              owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                              resultVar: Option[LocalVar], var base: Value, var path: ArrayBuffer[Long])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class StoreElementRefExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                                owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                                resultVar: Option[LocalVar], var value: Value, var location: Value,
                                var path: ArrayBuffer[Long])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class GetElementByNameExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                                 owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                                 resultVar: Option[LocalVar], var base: Value,
                                 var fieldNames: ArrayBuffer[String])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class StoreElementByNameExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                                   owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                                   resultVar: Option[LocalVar], var value: Value, var location: Value,
                                   var fieldNames: ArrayBuffer[String])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class FieldExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                      resultVar: Option[LocalVar], var base: Value, var path: ArrayBuffer[Long])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class FieldByNameExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                            owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                            resultVar: Option[LocalVar], var base: Value,
                            var fieldNames: ArrayBuffer[String])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class InstanceOfExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                           owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                           resultVar: Option[LocalVar], var obj: Value, var targetType: Type)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class GetRTTIStaticExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                              owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                              resultVar: Option[LocalVar], var rttiType: Type)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class GetInstantiateValueExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation],
                                    attributes: Long, owner: Option[Block],
                                    blockGroups: ArrayBuffer[BlockGroup], resultVar: Option[LocalVar],
                                    var instantiateTypes: ArrayBuffer[Type])
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class DebugExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                      resultVar: Option[LocalVar], var srcCodeName: String, var value: Value)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

final class LambdaExpr(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                       owner: Option[Block], blockGroups: ArrayBuffer[BlockGroup],
                       resultVar: Option[LocalVar], var funcTy: FuncType, var isLocalFunc: Boolean,
                       var identifier: String, var srcCodeName: String,
                       var params: ArrayBuffer[Parameter], var genericTypeParams: ArrayBuffer[Type],
                       var body: Option[BlockGroup], var retVal: Option[LocalVar],
                       var isCompileTimeValue: Boolean)
  extends ExprBase(annos, loc, attributes, owner, blockGroups, resultVar)

// =====================================================================================
// Custom type defs
// =====================================================================================

sealed trait CustomTypeDef {
  var id: Int = 0

  def annos: ArrayBuffer[Annotation]
  def loc: Option[DebugLocation]
  def attributes: Long
  def kind: CustomDefKind
  def srcCodeIdentifier: String
  def identifier: String
  def packageName: String
  def tpe: Option[Type]
  def genericDecl: Option[CustomTypeDef]
  def methods: ArrayBuffer[Func]
  def implementedInterfaces: ArrayBuffer[Type]
  def instanceMemberVars: ArrayBuffer[MemberVarInfo]
  def staticMemberVars: ArrayBuffer[GlobalVar]
  def annoInfo: Option[AnnoInfo]
  def vtable: ArrayBuffer[VTableInType]
  def instanceVarInitFunc: Option[Func]

  def isImported: Boolean = (attributes & (1L << 14)) != 0L // Attribute.IMPORTED
}

/** Common flatbuffer CustomTypeDef table part. */
sealed abstract class DefBase(var annos: ArrayBuffer[Annotation], var loc: Option[DebugLocation],
                              var attributes: Long, val kind: CustomDefKind,
                              var srcCodeIdentifier: String, var identifier: String,
                              var packageName: String, var tpe: Option[Type],
                              var genericDecl: Option[CustomTypeDef],
                              var methods: ArrayBuffer[Func],
                              var implementedInterfaces: ArrayBuffer[Type],
                              var instanceMemberVars: ArrayBuffer[MemberVarInfo],
                              var staticMemberVars: ArrayBuffer[GlobalVar],
                              var annoInfo: Option[AnnoInfo],
                              var vtable: ArrayBuffer[VTableInType],
                              var instanceVarInitFunc: Option[Func]) extends CustomTypeDef

final class EnumDef(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                    srcCodeIdentifier: String, identifier: String, packageName: String,
                    tpe: Option[Type], genericDecl: Option[CustomTypeDef], methods: ArrayBuffer[Func],
                    implementedInterfaces: ArrayBuffer[Type],
                    instanceMemberVars: ArrayBuffer[MemberVarInfo],
                    staticMemberVars: ArrayBuffer[GlobalVar], annoInfo: Option[AnnoInfo],
                    vtable: ArrayBuffer[VTableInType], instanceVarInitFunc: Option[Func],
                    var ctors: ArrayBuffer[EnumCtorInfo], var nonExhaustive: Boolean)
  extends DefBase(annos, loc, attributes, CustomDefKind.Enum, srcCodeIdentifier, identifier, packageName,
    tpe, genericDecl, methods, implementedInterfaces, instanceMemberVars, staticMemberVars,
    annoInfo, vtable, instanceVarInitFunc)

final class StructDef(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      srcCodeIdentifier: String, identifier: String, packageName: String,
                      tpe: Option[Type], genericDecl: Option[CustomTypeDef], methods: ArrayBuffer[Func],
                      implementedInterfaces: ArrayBuffer[Type],
                      instanceMemberVars: ArrayBuffer[MemberVarInfo],
                      staticMemberVars: ArrayBuffer[GlobalVar], annoInfo: Option[AnnoInfo],
                      vtable: ArrayBuffer[VTableInType], instanceVarInitFunc: Option[Func],
                      var isCStruct: Boolean)
  extends DefBase(annos, loc, attributes, CustomDefKind.Struct, srcCodeIdentifier, identifier, packageName,
    tpe, genericDecl, methods, implementedInterfaces, instanceMemberVars, staticMemberVars,
    annoInfo, vtable, instanceVarInitFunc)

final class ClassDef(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                     srcCodeIdentifier: String, identifier: String, packageName: String,
                     tpe: Option[Type], genericDecl: Option[CustomTypeDef], methods: ArrayBuffer[Func],
                     implementedInterfaces: ArrayBuffer[Type],
                     instanceMemberVars: ArrayBuffer[MemberVarInfo],
                     staticMemberVars: ArrayBuffer[GlobalVar], annoInfo: Option[AnnoInfo],
                     vtable: ArrayBuffer[VTableInType], instanceVarInitFunc: Option[Func],
                     var isClass: Boolean, var isAnnotation: Boolean,
                     var annotationTargets: ArrayBuffer[GlobalVar], var superClass: Option[Type])
  extends DefBase(annos, loc, attributes, CustomDefKind.Class, srcCodeIdentifier, identifier, packageName,
    tpe, genericDecl, methods, implementedInterfaces, instanceMemberVars, staticMemberVars,
    annoInfo, vtable, instanceVarInitFunc)

final class ExtendDef(annos: ArrayBuffer[Annotation], loc: Option[DebugLocation], attributes: Long,
                      srcCodeIdentifier: String, identifier: String, packageName: String,
                      tpe: Option[Type], genericDecl: Option[CustomTypeDef], methods: ArrayBuffer[Func],
                      implementedInterfaces: ArrayBuffer[Type],
                      instanceMemberVars: ArrayBuffer[MemberVarInfo],
                      staticMemberVars: ArrayBuffer[GlobalVar], annoInfo: Option[AnnoInfo],
                      vtable: ArrayBuffer[VTableInType], instanceVarInitFunc: Option[Func],
                      var extendedType: Option[Type], var genericParams: ArrayBuffer[Type])
  extends DefBase(annos, loc, attributes, CustomDefKind.Extend, srcCodeIdentifier, identifier, packageName,
    tpe, genericDecl, methods, implementedInterfaces, instanceMemberVars, staticMemberVars,
    annoInfo, vtable, instanceVarInitFunc) {
  def genericTypeParams: ArrayBuffer[Type] = genericParams
}

// =====================================================================================
// Package
// =====================================================================================

final class Package() {
  var name: String = ""
  var accessLevel: PackageAccessLevel = PackageAccessLevel.Invalid
  var phase: Phase = Phase.Raw
  var packageInitFunc: Option[Func] = None
  var packageLiteralInitFunc: Option[Func] = None

  /** All global vars of the package (incl. src-code-imported), in file order. */
  val globalVars: ArrayBuffer[GlobalVar] = ArrayBuffer()
  /** All global functions of the package (incl. imported), in file order. */
  val globalFuncs: ArrayBuffer[Func] = ArrayBuffer()

  // Decl lists, mirroring the C++ Package (imported lists + current package lists).
  val importedStructs: ArrayBuffer[StructDef] = ArrayBuffer()
  val importedClasses: ArrayBuffer[ClassDef] = ArrayBuffer()
  val importedEnums: ArrayBuffer[EnumDef] = ArrayBuffer()
  val importedExtends: ArrayBuffer[ExtendDef] = ArrayBuffer()
  val structs: ArrayBuffer[StructDef] = ArrayBuffer()
  val classes: ArrayBuffer[ClassDef] = ArrayBuffer()
  val enums: ArrayBuffer[EnumDef] = ArrayBuffer()
  val extendsDefs: ArrayBuffer[ExtendDef] = ArrayBuffer()

  def allCustomTypeDefs: Seq[CustomTypeDef] =
    Seq(importedStructs, importedClasses, importedEnums, importedExtends,
      structs, classes, enums, extendsDefs).flatten

  def curPkgCustomTypeDefs: Seq[CustomTypeDef] =
    Seq(structs, classes, enums, extendsDefs).flatten

  def getFunc(identifier: String): Option[Func] = {
    val prefixed = if (identifier.startsWith("@")) identifier else "@" + identifier
    globalFuncs.find(_.identifier == prefixed)
  }

  def getDef(identifier: String): Option[CustomTypeDef] = {
    val prefixed = if (identifier.startsWith("@")) identifier else "@" + identifier
    allCustomTypeDefs.find(_.identifier == prefixed)
  }
}
