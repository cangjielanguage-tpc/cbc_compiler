package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageImpl.*
import scala.collection.mutable

/** Check schema IDs through typed getters, including annotations and vtables.
  * Numeric literals, counters and debug metadata are not entity references.
  */
private[v1203] object SchemaValidation {
  def references(p: CHIRPackageT): Seq[ValidationIssue] = {
    import EntityTable.*
    val problems = mutable.ArrayBuffer.empty[ValidationIssue]

    def tableSize(table: EntityTable): Int = table match {
      case Types => if (p.getTypes == null) 0 else p.getTypes.length
      case Values => if (p.getValues == null) 0 else p.getValues.length
      case Expressions => if (p.getExprs == null) 0 else p.getExprs.length
      case Definitions => if (p.getDefs == null) 0 else p.getDefs.length
    }

    def reference(table: EntityTable, id: Long, field: String, ref: Option[EntityRef]): Unit = {
      if (id < 0 || id > tableSize(table)) {
        problems += ValidationIssue(s"$field has invalid $table ID $id", ref)
      }
    }

    def referenceIds(table: EntityTable, ids: Array[Long], field: String, ref: Option[EntityRef]): Unit = {
      if (ids != null) {
        for (id <- ids) {
          reference(table, id, field, ref)
        }
      }
    }

    def walk(obj: Object, ref: Option[EntityRef]): Unit = obj match {
      case t: AllocateBaseT =>
        walk(t.getBase, ref)
        reference(Types, t.getAllocatedType, "AllocateBase.allocatedType", ref)
      case t: ApplyBaseT =>
        walk(t.getBase, ref)
      case t: BaseT =>
        val annos = t.getAnnos
        if (annos != null) annos.foreach(walk(_, ref))
      case t: BinaryExpressionBaseT =>
        walk(t.getBase, ref)
      case t: BlockGroupT =>
        walk(t.getBase, ref)
        reference(Values, t.getEntryBlock, "BlockGroup.entryBlock", ref)
        referenceIds(Values, t.getBlocks, "BlockGroup.blocks", ref)
        reference(Values, t.getOwnedFunc, "BlockGroup.ownedFunc", ref)
        reference(Expressions, t.getOwnedExpression, "BlockGroup.ownedExpression", ref)
      case t: BlockT =>
        walk(t.getBase, ref)
        reference(Values, t.getParentGroup, "Block.parentGroup", ref)
        referenceIds(Expressions, t.getExprs, "Block.exprs", ref)
        referenceIds(Values, t.getPredecessors, "Block.predecessors", ref)
        referenceIds(Types, t.getExceptionCatchList, "Block.exceptionCatchList", ref)
      case t: BoolLiteralT =>
        walk(t.getBase, ref)
      case t: BranchT =>
        walk(t.getBase, ref)
      case t: CHIRPackageT =>
        val types = t.getTypes
        if (types != null) types.foreach(walk(_, ref))
        val values = t.getValues
        if (values != null) values.foreach(walk(_, ref))
        val exprs = t.getExprs
        if (exprs != null) exprs.foreach(walk(_, ref))
        val defs = t.getDefs
        if (defs != null) defs.foreach(walk(_, ref))
      case t: ClassDefT =>
        walk(t.getBase, ref)
        referenceIds(Values, t.getAnnotationTargets, "ClassDef.annotationTargets", ref)
        reference(Types, t.getSuperClass, "ClassDef.superClass", ref)
      case t: CustomTypeDefT =>
        walk(t.getBase, ref)
        reference(Definitions, t.getCustomTypeDefId, "CustomTypeDef.customTypeDefId", ref)
        reference(Types, t.getType, "CustomTypeDef.type", ref)
        reference(Definitions, t.getGenericDecl, "CustomTypeDef.genericDecl", ref)
        referenceIds(Values, t.getMethods, "CustomTypeDef.methods", ref)
        referenceIds(Types, t.getImplementedInterfaces, "CustomTypeDef.implementedInterfaces", ref)
        val instanceMemberVars = t.getInstanceMemberVars
        if (instanceMemberVars != null) instanceMemberVars.foreach(walk(_, ref))
        referenceIds(Values, t.getStaticMemberVars, "CustomTypeDef.staticMemberVars", ref)
        val vtable = t.getVtable
        if (vtable != null) vtable.foreach(walk(_, ref))
        reference(Values, t.getInstanceVarInitFunc, "CustomTypeDef.instanceVarInitFunc", ref)
      case t: CustomTypeT =>
        walk(t.getBase, ref)
        reference(Definitions, t.getCustomTypeDef, "CustomType.customTypeDef", ref)
      case t: DebugT =>
        walk(t.getBase, ref)
      case t: EnumCtorInfoT =>
        reference(Types, t.getFuncType, "EnumCtorInfo.funcType", ref)
      case t: EnumDefT =>
        walk(t.getBase, ref)
        val ctors = t.getCtors
        if (ctors != null) ctors.foreach(walk(_, ref))
      case t: ExpressionT =>
        walk(t.getBase, ref)
        referenceIds(Values, t.getOperands, "Expression.operands", ref)
        referenceIds(Values, t.getBlockGroups, "Expression.blockGroups", ref)
        reference(Values, t.getOwner, "Expression.owner", ref)
        reference(Values, t.getResultLocalVar, "Expression.resultLocalVar", ref)
        reference(Types, t.getResultTy, "Expression.resultTy", ref)
      case t: ExtendDefT =>
        walk(t.getBase, ref)
        reference(Types, t.getExtendedType, "ExtendDef.extendedType", ref)
        referenceIds(Types, t.getGenericParams, "ExtendDef.genericParams", ref)
      case t: FieldByNameT =>
        walk(t.getBase, ref)
      case t: FieldT =>
        walk(t.getBase, ref)
      case t: FloatLiteralT =>
        walk(t.getBase, ref)
      case t: FuncCallT =>
        walk(t.getBase, ref)
        referenceIds(Types, t.getInstantiatedTypeArgs, "FuncCall.instantiatedTypeArgs", ref)
        reference(Types, t.getObjType, "FuncCall.objType", ref)
      case t: FuncSigInfoT =>
        reference(Types, t.getFuncType, "FuncSigInfo.funcType", ref)
        referenceIds(Types, t.getGenericTypeParams, "FuncSigInfo.genericTypeParams", ref)
      case t: FuncTypeT =>
        walk(t.getBase, ref)
      case t: FunctionT =>
        walk(t.getBase, ref)
        reference(Values, t.getGenericDecl, "Function.genericDecl", ref)
        walk(t.getOriginalLambdaInfo, ref)
        referenceIds(Types, t.getGenericTypeParams, "Function.genericTypeParams", ref)
        reference(Values, t.getParamDftValHostFunc, "Function.paramDftValHostFunc", ref)
        reference(Values, t.getBody, "Function.body", ref)
        referenceIds(Values, t.getParams, "Function.params", ref)
        reference(Values, t.getRetVal, "Function.retVal", ref)
      case t: GenericTypeT =>
        walk(t.getBase, ref)
        referenceIds(Types, t.getUpperBounds, "GenericType.upperBounds", ref)
      case t: GetElementByNameT =>
        walk(t.getBase, ref)
      case t: GetElementRefT =>
        walk(t.getBase, ref)
      case t: GetInstantiateValueT =>
        walk(t.getBase, ref)
        referenceIds(Types, t.getInstantiateTypes, "GetInstantiateValue.instantiateTypes", ref)
      case t: GetRTTIStaticT =>
        walk(t.getBase, ref)
        reference(Types, t.getRttiType, "GetRTTIStatic.rttiType", ref)
      case t: GlobalValueT =>
        walk(t.getBase, ref)
        reference(Definitions, t.getDeclaredParent, "GlobalValue.declaredParent", ref)
      case t: GlobalVarT =>
        walk(t.getBase, ref)
        reference(Values, t.getInitializer, "GlobalVar.initializer", ref)
      case t: InstanceOfT =>
        walk(t.getBase, ref)
        reference(Types, t.getTargetType, "InstanceOf.targetType", ref)
      case t: IntLiteralT =>
        walk(t.getBase, ref)
      case t: IntrinsicBaseT =>
        walk(t.getBase, ref)
      case t: InvokeBaseT =>
        walk(t.getBase, ref)
        walk(t.getVirMethodCtx, ref)
      case t: LambdaT =>
        walk(t.getBase, ref)
        reference(Types, t.getFuncTy, "Lambda.funcTy", ref)
        referenceIds(Values, t.getParams, "Lambda.params", ref)
        referenceIds(Types, t.getGenericTypeParams, "Lambda.genericTypeParams", ref)
        reference(Values, t.getBody, "Lambda.body", ref)
        reference(Values, t.getRetVal, "Lambda.retVal", ref)
      case t: LiteralValueT =>
        walk(t.getBase, ref)
      case t: LocalVarT =>
        walk(t.getBase, ref)
        reference(Expressions, t.getAssociatedExpr, "LocalVar.associatedExpr", ref)
      case t: MemberVarInfoT =>
        reference(Types, t.getType, "MemberVarInfo.type", ref)
        reference(Values, t.getInitializerFunc, "MemberVarInfo.initializerFunc", ref)
        reference(Definitions, t.getOuterDef, "MemberVarInfo.outerDef", ref)
      case t: MultiBranchT =>
        walk(t.getBase, ref)
      case t: NullLiteralT =>
        walk(t.getBase, ref)
      case t: NumericCastBaseT =>
        walk(t.getBase, ref)
      case t: OverrideSrcFuncTypeT =>
        reference(Types, t.getType, "OverrideSrcFuncType.type", ref)
      case t: ParameterT =>
        walk(t.getBase, ref)
        reference(Values, t.getOwnedFunc, "Parameter.ownedFunc", ref)
        reference(Expressions, t.getOwnedLambda, "Parameter.ownedLambda", ref)
      case t: RawArrayAllocateBaseT =>
        walk(t.getBase, ref)
        reference(Types, t.getElementType, "RawArrayAllocateBase.elementType", ref)
      case t: RawArrayTypeT =>
        walk(t.getBase, ref)
      case t: RuneLiteralT =>
        walk(t.getBase, ref)
      case t: SpawnBaseT =>
        walk(t.getBase, ref)
        reference(Values, t.getExecuteClosure, "SpawnBase.executeClosure", ref)
      case t: StoreElementByNameT =>
        walk(t.getBase, ref)
      case t: StoreElementRefT =>
        walk(t.getBase, ref)
      case t: StringLiteralT =>
        walk(t.getBase, ref)
      case t: StructDefT =>
        walk(t.getBase, ref)
      case t: TypeT =>
        referenceIds(Types, t.getArgTys, "Type.argTys", ref)
      case t: UnaryExpressionBaseT =>
        walk(t.getBase, ref)
      case t: UnitLiteralT =>
        walk(t.getBase, ref)
      case t: VArrayTypeT =>
        walk(t.getBase, ref)
      case t: VTableInTypeT =>
        reference(Types, t.getSrcParentType, "VTableInType.srcParentType", ref)
        val virtualMethods = t.getVirtualMethods
        if (virtualMethods != null) virtualMethods.foreach(walk(_, ref))
      case t: ValueT =>
        walk(t.getBase, ref)
        reference(Types, t.getType, "Value.type", ref)
      case t: VirtualMethodInfoT =>
        reference(Types, t.getSigType, "VirtualMethodInfo.sigType", ref)
        referenceIds(Types, t.getMethodGenericTypeParams, "VirtualMethodInfo.methodGenericTypeParams", ref)
        reference(Values, t.getInstance, "VirtualMethodInfo.instance", ref)
        reference(Types, t.getOriginalType, "VirtualMethodInfo.originalType", ref)
        reference(Types, t.getParentType, "VirtualMethodInfo.parentType", ref)
        reference(Types, t.getReturnType, "VirtualMethodInfo.returnType", ref)
      case t: WrappedRawMethodT =>
        reference(Values, t.getRawMethod, "WrappedRawMethod.rawMethod", ref)
      case u: AnnotationUnion => u.getValue match {
        case t: NeedCheckArrayBoundT if u.getType == Annotation.needCheckArrayBound => ()
        case t: NeedCheckCastT if u.getType == Annotation.needCheckCast => ()
        case t: DebugLocationT if u.getType == Annotation.debugLocationInfoForWarning => ()
        case t: GeneratedFromForInT if u.getType == Annotation.generatedFromForIn => ()
        case t: IsAutoEnvClassT if u.getType == Annotation.isAutoEnvClass => ()
        case t: IsCapturedClassInCCT if u.getType == Annotation.isCapturedClassInCC => ()
        case t: LinkTypeInfoT if u.getType == Annotation.linkTypeInfo => ()
        case t: SkipCheckT if u.getType == Annotation.skipCheck => ()
        case t: NeverOverflowInfoT if u.getType == Annotation.neverOverflowInfo => ()
        case t: EnumCaseIndexT if u.getType == Annotation.enumCaseIndex => ()
        case t: VirMethodOffsetT if u.getType == Annotation.virMethodOffset => ()
        case t: WrappedRawMethodT if u.getType == Annotation.wrappedRawMethod => walk(t, ref)
        case t: OverrideSrcFuncTypeT if u.getType == Annotation.overrideSrcFuncType => walk(t, ref)
        case _ => problems += ValidationIssue("Annotation union tag/payload mismatch", ref)
      }
      case u: TypeElemUnion => u.getValue match {
        case t: TypeT if u.getType == TypeElem.Type => walk(t, ref)
        case t: RawArrayTypeT if u.getType == TypeElem.RawArrayType => walk(t, ref)
        case t: VArrayTypeT if u.getType == TypeElem.VArrayType => walk(t, ref)
        case t: FuncTypeT if u.getType == TypeElem.FuncType => walk(t, ref)
        case t: CustomTypeT if u.getType == TypeElem.CustomType => walk(t, ref)
        case t: GenericTypeT if u.getType == TypeElem.GenericType => walk(t, ref)
        case _ => problems += ValidationIssue("Type union tag/payload mismatch", ref)
      }
      case u: CustomTypeDefElemUnion => u.getValue match {
        case t: EnumDefT if u.getType == CustomTypeDefElem.EnumDef => walk(t, ref)
        case t: StructDefT if u.getType == CustomTypeDefElem.StructDef => walk(t, ref)
        case t: ClassDefT if u.getType == CustomTypeDefElem.ClassDef => walk(t, ref)
        case t: ExtendDefT if u.getType == CustomTypeDefElem.ExtendDef => walk(t, ref)
        case _ => problems += ValidationIssue("Definition union tag/payload mismatch", ref)
      }
      case u: ExpressionElemUnion => u.getValue match {
        case t: ExpressionT if u.getType == ExpressionElem.Expression => walk(t, ref)
        case t: AllocateBaseT if u.getType == ExpressionElem.AllocateBase => walk(t, ref)
        case t: ApplyBaseT if u.getType == ExpressionElem.ApplyBase => walk(t, ref)
        case t: BinaryExpressionBaseT if u.getType == ExpressionElem.BinaryExpressionBase => walk(t, ref)
        case t: BranchT if u.getType == ExpressionElem.Branch => walk(t, ref)
        case t: DebugT if u.getType == ExpressionElem.Debug => walk(t, ref)
        case t: FieldT if u.getType == ExpressionElem.Field => walk(t, ref)
        case t: FieldByNameT if u.getType == ExpressionElem.FieldByName => walk(t, ref)
        case t: GetElementByNameT if u.getType == ExpressionElem.GetElementByName => walk(t, ref)
        case t: GetElementRefT if u.getType == ExpressionElem.GetElementRef => walk(t, ref)
        case t: GetInstantiateValueT if u.getType == ExpressionElem.GetInstantiateValue => walk(t, ref)
        case t: GetRTTIStaticT if u.getType == ExpressionElem.GetRTTIStatic => walk(t, ref)
        case t: InstanceOfT if u.getType == ExpressionElem.InstanceOf => walk(t, ref)
        case t: IntrinsicBaseT if u.getType == ExpressionElem.IntrinsicBase => walk(t, ref)
        case t: InvokeBaseT if u.getType == ExpressionElem.InvokeBase => walk(t, ref)
        case t: LambdaT if u.getType == ExpressionElem.Lambda => walk(t, ref)
        case t: MultiBranchT if u.getType == ExpressionElem.MultiBranch => walk(t, ref)
        case t: NumericCastBaseT if u.getType == ExpressionElem.NumericCastBase => walk(t, ref)
        case t: RawArrayAllocateBaseT if u.getType == ExpressionElem.RawArrayAllocateBase => walk(t, ref)
        case t: SpawnBaseT if u.getType == ExpressionElem.SpawnBase => walk(t, ref)
        case t: StoreElementByNameT if u.getType == ExpressionElem.StoreElementByName => walk(t, ref)
        case t: StoreElementRefT if u.getType == ExpressionElem.StoreElementRef => walk(t, ref)
        case t: UnaryExpressionBaseT if u.getType == ExpressionElem.UnaryExpressionBase => walk(t, ref)
        case _ => problems += ValidationIssue("Expression union tag/payload mismatch", ref)
      }
      case u: ValueElemUnion => u.getValue match {
        case t: BoolLiteralT if u.getType == ValueElem.BoolLiteral => walk(t, ref)
        case t: RuneLiteralT if u.getType == ValueElem.RuneLiteral => walk(t, ref)
        case t: StringLiteralT if u.getType == ValueElem.StringLiteral => walk(t, ref)
        case t: IntLiteralT if u.getType == ValueElem.IntLiteral => walk(t, ref)
        case t: FloatLiteralT if u.getType == ValueElem.FloatLiteral => walk(t, ref)
        case t: UnitLiteralT if u.getType == ValueElem.UnitLiteral => walk(t, ref)
        case t: NullLiteralT if u.getType == ValueElem.NullLiteral => walk(t, ref)
        case t: ParameterT if u.getType == ValueElem.Parameter => walk(t, ref)
        case t: LocalVarT if u.getType == ValueElem.LocalVar => walk(t, ref)
        case t: GlobalVarT if u.getType == ValueElem.GlobalVar => walk(t, ref)
        case t: FunctionT if u.getType == ValueElem.Function => walk(t, ref)
        case t: BlockT if u.getType == ValueElem.Block => walk(t, ref)
        case t: BlockGroupT if u.getType == ValueElem.BlockGroup => walk(t, ref)
        case _ => problems += ValidationIssue("Value union tag/payload mismatch", ref)
      }
      case null => ()
      case _ => problems += ValidationIssue("Unsupported schema payload", ref)
    }

    def entries[A <: Object](table: EntityTable, values: Array[A]): Unit = {
      if (values != null) {
        var i = 0
        while (i < values.length) {
          if (values(i) == null) problems += ValidationIssue("Missing schema payload", Some(EntityRef(table, i + 1L)))
          else walk(values(i), Some(EntityRef(table, i + 1L)))
          i += 1
        }
      }
    }

    entries(Types, p.getTypes)
    entries(Values, p.getValues)
    entries(Expressions, p.getExprs)
    entries(Definitions, p.getDefs)
    reference(Values, p.getPackageInitFunc, "packageInitFunc", None)
    reference(Values, p.getPackageLiteralInitFunc, "packageLiteralInitFunc", None)
    problems.toSeq
  }
}
