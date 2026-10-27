package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*
import scala.collection.mutable.ArrayBuffer

/** Port of C++ `Equality.{h,cpp}` (namespace PatchGenerator). */
object Equality {

  // ===================================================================================
  // hashing keys (ports of CustomDefHasher / FunctionHasher / GlobalVarHasher +
  // CustomDefEquality / FunctionEquality / GlobalVarEquality)
  // ===================================================================================

  final case class CustomDefKey(kind: CustomDefKind, packageName: String, identifier: String,
                                srcCodeIdentifier: String)

  final case class FunctionKey(isFuncWithBody: Boolean, isImportedFunc: Boolean, packageName: String,
                               identifier: String, srcCodeIdentifier: String)

  final case class GlobalVarKey(identifier: String, srcCodeIdentifier: String, packageName: String)

  def customDefKey(d: CustomTypeDef): CustomDefKey =
    CustomDefKey(d.kind, d.packageName, d.identifier, d.srcCodeIdentifier)

  def functionKey(f: Func): FunctionKey =
    FunctionKey(f.body.isDefined, f.isImported, f.packageName, f.identifier, f.srcCodeIdentifier)

  def globalVarKey(v: GlobalVar): GlobalVarKey =
    GlobalVarKey(v.identifier, v.srcCodeIdentifier, v.packageName)

  def areEqual(l: CustomTypeDef, r: CustomTypeDef): Boolean =
    (l eq r) || customDefKey(l) == customDefKey(r)

  def areEqual(l: Func, r: Func): Boolean = {
    (l eq r) || (functionKey(l) == functionKey(r) && {
      val lt = l.tpe.map(_.asInstanceOf[FuncType])
      val rt = r.tpe.map(_.asInstanceOf[FuncType])
      areEqualOptType(lt, rt) &&
        areEqualOptDef(l.declaredParent, r.declaredParent)
    })
  }

  def areEqual(l: GlobalVar, r: GlobalVar): Boolean =
    (l eq r) || globalVarKey(l) == globalVarKey(r)

  // ===================================================================================
  // type equality
  // ===================================================================================

  def areEqualTypes(l: Seq[Type], r: Seq[Type]): Boolean =
    l.size == r.size && l.zip(r).forall { (a, b) => areEqual(a, b) }

  def areEqual(l: Type, r: Type): Boolean = {
    if (l eq r) {
      return true
    }
    (l, r) match {
      case (l: CustomType, r: CustomType) =>
        // C++: kind equal + customDef equal + generic args equal
        defKindOf(l) == defKindOf(r) && areEqualOptDef(l.customTypeDef, r.customTypeDef) &&
          areEqualTypes(l.argTys.toSeq, r.argTys.toSeq)
      case (l: FuncType, r: FuncType) =>
        l.isCFuncType == r.isCFuncType && l.hasVarArg == r.hasVarArg &&
          areEqual(l.returnType, r.returnType) && areEqualTypes(l.paramTypes.toSeq, r.paramTypes.toSeq)
      case (l: GenericType, r: GenericType) =>
        l.identifier == r.identifier && l.srcCodeIdentifier == r.srcCodeIdentifier
      case (l: TypeImpl, r: TypeImpl) =>
        // plain Type branch: kind + type args
        l.kind == r.kind && areEqualTypes(l.argTys.toSeq, r.argTys.toSeq)
      case (l: RawArrayType, r: RawArrayType) =>
        // C++ default branch: kind (RawArray) + type args ([elementType]); dims not compared
        areEqual(l.elementType, r.elementType)
      case (l: VArrayType, r: VArrayType) =>
        // C++ default branch: kind (VArray) + type args ([elementType]); size not compared
        areEqual(l.elementType, r.elementType)
      case _ => false // variant index mismatch
    }
  }

  private def defKindOf(t: CustomType): Option[CustomDefKind] = t.customTypeDef.map(_.kind)

  private def areEqualOptDef(l: Option[CustomTypeDef], r: Option[CustomTypeDef]): Boolean =
    (l, r) match {
      case (None, None) => true
      case (Some(l), Some(r)) => areEqual(l, r)
      case _ => false
    }

  private def areEqualOptType(l: Option[Type], r: Option[Type]): Boolean =
    (l, r) match {
      case (None, None) => true
      case (Some(l), Some(r)) => areEqual(l, r)
      case _ => false
    }

  // ===================================================================================
  // function-code equality (C++ FuncCodeEquality)
  // ===================================================================================

  private def areEqualValues(l: Seq[Value], r: Seq[Value])(fce: FuncCodeEquality): Boolean =
    l.size == r.size && l.zip(r).forall { (a, b) => fce.areEqual(a, b) }

  final class FuncCodeEquality {
    /** C++ keeps a plain set of Value pointers; model classes have identity equals. */
    private val visitedValues: java.util.Set[Value] =
      java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Value, java.lang.Boolean]())

    def areEqual(first: Value, second: Value): Boolean = {
      if (first eq second) {
        return true
      }
      if (!visitedValues.add(first)) {
        return true
      }
      (first, second) match {
        case (l: BoolLiteral, r: BoolLiteral) =>
          l.value == r.value && l.attributes == r.attributes
        case (l: Block, r: Block) =>
          l.attributes == r.attributes && areEqualExprs(l.exprs, r.exprs)
        case (l: BlockGroup, r: BlockGroup) =>
          areEqualBlocks(l.blocks, r.blocks)
        case (l: FloatLiteral, r: FloatLiteral) =>
          l.value == r.value && l.attributes == r.attributes && areEqualOptType(l.tpe, r.tpe)
        case (l: Func, r: Func) =>
          l.attributes == r.attributes && Equality.areEqual(l, r)
        case (l: GlobalVar, r: GlobalVar) =>
          l.packageName == r.packageName && l.identifier == r.identifier &&
            l.srcCodeIdentifier == r.srcCodeIdentifier && l.attributes == r.attributes &&
            areEqualOptType(l.tpe, r.tpe)
        case (l: IntLiteral, r: IntLiteral) =>
          l.value == r.value && l.attributes == r.attributes && areEqualOptType(l.tpe, r.tpe)
        case (l: NullLiteral, r: NullLiteral) =>
          l.attributes == r.attributes && areEqualOptType(l.tpe, r.tpe)
        case (l: LocalVar, r: LocalVar) =>
          l.attributes == r.attributes && l.identifier == r.identifier &&
            areEqualOptExpr(l.associatedExpr, r.associatedExpr) && areEqualOptType(l.tpe, r.tpe)
        case (l: Parameter, r: Parameter) =>
          l.identifier == r.identifier && areEqualOptFunc(l.ownedFunc, r.ownedFunc) &&
            areEqualOptExpr(l.ownedLambda, r.ownedLambda) && l.attributes == r.attributes &&
            areEqualOptType(l.tpe, r.tpe)
        case (l: RuneLiteral, r: RuneLiteral) =>
          l.value == r.value && l.attributes == r.attributes
        case (l: StringLiteral, r: StringLiteral) =>
          l.value == r.value && l.attributes == r.attributes
        case (l: UnitLiteral, r: UnitLiteral) =>
          areEqualOptType(l.tpe, r.tpe) && l.attributes == r.attributes
        case _ => false
      }
    }

    private def areEqualBlocks(l: ArrayBuffer[Block], r: ArrayBuffer[Block]): Boolean =
      l.size == r.size && l.zip(r).forall { (a, b) => areEqual(a, b) }

    private def areEqualExprs(l: ArrayBuffer[Expression], r: ArrayBuffer[Expression]): Boolean =
      l.size == r.size && l.zip(r).forall { (a, b) => areEqual(a, b) }

    private def areEqualOptFunc(l: Option[Func], r: Option[Func]): Boolean =
      (l, r) match {
        case (None, None) => true
        case (Some(l), Some(r)) => areEqual(l, r)
        case _ => false
      }

    private def areEqualOptExpr(l: Option[Expression], r: Option[Expression]): Boolean =
      (l, r) match {
        case (None, None) => true
        case (Some(l), Some(r)) => areEqual(l, r)
        case _ => false
      }

    def areEqual(first: Expression, second: Expression): Boolean = {
      if (first eq second) {
        return true
      }
      // C++ compares ExprKind first (TRY_* kinds are distinct); the model stores the
      // try-ness as a flag on the shared class
      if (!sameExprKind(first, second)) {
        return false
      }
      if (!areEqualValues(ChirOps.operandsOf(first).toSeq, ChirOps.operandsOf(second).toSeq)(this)) {
        return false
      }
      (first, second) match {
        case (l: AllocateExpr, r: AllocateExpr) => Equality.areEqual(l.allocatedType, r.allocatedType)
        case (l: ApplyExpr, r: ApplyExpr) =>
          // C++ quirk: the Apply branch calls areEqualsDynamicDispatches(l, l) — it compares
          // the left side with itself, so effectively only IsSuperCall is compared
          l.isSuperCall == r.isSuperCall
        case (l: BinaryExpr, r: BinaryExpr) => l.overflowStrategy == r.overflowStrategy
        case (l: BranchExpr, r: BranchExpr) => l.sourceExpr == r.sourceExpr
        case (l: DebugExpr, r: DebugExpr) => l.srcCodeName == r.srcCodeName
        case (l: InvokeExpr, r: InvokeExpr) =>
          // GetMethodName = OverflowStrategyPrefix(strategy) + callee.srcCodeIdentifier,
          // and the prefix is injective per strategy
          l.callee.srcCodeIdentifier == r.callee.srcCodeIdentifier &&
            l.overflowStrategy == r.overflowStrategy &&
            areEqualOptType(l.objType, r.objType) &&
            areEqualTypes(l.instantiatedTypeArgs.toSeq, r.instantiatedTypeArgs.toSeq)
        case (l: FieldExpr, r: FieldExpr) => l.path == r.path
        case (l: FieldByNameExpr, r: FieldByNameExpr) => l.fieldNames == r.fieldNames
        case (l: GetElementByNameExpr, r: GetElementByNameExpr) => l.fieldNames == r.fieldNames
        case (l: GetElementRefExpr, r: GetElementRefExpr) => l.path == r.path
        case (l: GetInstantiateValueExpr, r: GetInstantiateValueExpr) =>
          areEqualTypes(l.instantiateTypes.toSeq, r.instantiateTypes.toSeq)
        case (l: GetRTTIStaticExpr, r: GetRTTIStaticExpr) => Equality.areEqual(l.rttiType, r.rttiType)
        case (l: InstanceOfExpr, r: InstanceOfExpr) => Equality.areEqual(l.targetType, r.targetType)
        case (l: IntrinsicExpr, r: IntrinsicExpr) =>
          l.intrinsicKind == r.intrinsicKind && areEqualOptType(l.objType, r.objType) &&
            areEqualTypes(l.instantiatedTypeArgs.toSeq, r.instantiatedTypeArgs.toSeq)
        case (l: LambdaExpr, r: LambdaExpr) =>
          l.isLocalFunc == r.isLocalFunc && l.identifier == r.identifier &&
            l.srcCodeName == r.srcCodeName && Equality.areEqual(l.funcTy, r.funcTy) &&
            areEqualTypes(l.genericTypeParams.toSeq, r.genericTypeParams.toSeq)
        case (l: MultiBranchExpr, r: MultiBranchExpr) => l.caseValues == r.caseValues
        case (l: NumericCastExpr, r: NumericCastExpr) => l.overflowStrategy == r.overflowStrategy
        case (l: RawArrayAllocateExpr, r: RawArrayAllocateExpr) => Equality.areEqual(l.elementType, r.elementType)
        case (l: SpawnExpr, r: SpawnExpr) => areEqualOptFunc(l.executeClosure, r.executeClosure)
        case (l: StoreElementByNameExpr, r: StoreElementByNameExpr) => l.fieldNames == r.fieldNames
        case (l: StoreElementRefExpr, r: StoreElementRefExpr) => l.path == r.path
        case (l: UnaryExpr, r: UnaryExpr) => l.overflowStrategy == r.overflowStrategy
        case _ => true // C++ default Expression branch
      }
    }

    private def sameExprKind(l: Expression, r: Expression): Boolean =
      l.getClass == r.getClass && exprTry(l) == exprTry(r)

    private def exprTry(e: Expression): Boolean = e match {
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
  }
}
