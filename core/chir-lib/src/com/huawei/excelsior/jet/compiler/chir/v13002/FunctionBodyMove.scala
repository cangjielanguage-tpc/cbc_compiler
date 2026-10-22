package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*

/** Versioned in-memory transfer with parameter and generic-type substitution. */
private[v13002] object FunctionBodyMove {
  final case class References(expressions: Seq[Long], parameters: Map[Long, Long], remapType: Long => Long)

  def move(pkg: CHIRPackageT, source: CHIR.Func, target: CHIR.Func): References = {

    val sourceId = source.id
    val targetId = target.id
    require(sourceId != targetId, "Source and destination must differ")
    require(source.body.nonEmpty, "Source has no body")
    require(source.kind == CHIR.Func.Kind.Default && target.kind == CHIR.Func.Kind.Default,
      "Only ordinary functions are supported")
    require(target.params.size == source.params.size + 1,
      "Destination must have one extra leading parameter")
    require(!source.tpe.hasVarArg && !target.tpe.hasVarArg &&
      source.tpe.isC == target.tpe.isC, "Incompatible calling conventions")

    def value(id: Long): ValueElemUnion = pkg.getValues()(index(id))
    def function(id: Long): FunctionT = {
      val v = value(id)
      require(v.getType == ValueElem.Function, s"Value $id is not a function")
      v.asFunction()
    }
    val from = function(source.id)
    val to = function(target.id)
    require(from.getBase.getDeclaredParent == 0,
      "A member source requires explicit handling of its original receiver")
    // The destination may be a Patch instance method. Its leading receiver is
    // deliberately left unused; retain its signature and declaredParent.
    if (to.getBase.getDeclaredParent != 0) {
      require(target.params.head.tpe == target.tpe.receiverType,
        "The first destination parameter must match its receiver type")
    }
    require(from.getGenericDecl == 0 && to.getGenericDecl == 0,
      "Generic instances are unsupported")
    val sourceGenerics = ids(from.getGenericTypeParams)
    val targetGenerics = ids(to.getGenericTypeParams)
    require(sourceGenerics.length == targetGenerics.length,
      "Functions must have the same number of generic parameters")
    val remapType = new TypeRemapper(pkg, sourceGenerics.zip(targetGenerics).toMap)
    sourceGenerics.zip(targetGenerics).foreach { (a, b) =>
      require(pkg.getTypes()(index(a)).getType == TypeElem.GenericType &&
        pkg.getTypes()(index(b)).getType == TypeElem.GenericType, "Expected generic types")
      val sourceBounds = pkg.getTypes()(index(a)).asGenericType().getUpperBounds
      val targetBounds = pkg.getTypes()(index(b)).asGenericType().getUpperBounds
      require(ids(sourceBounds).map(remapType.apply).sameElements(ids(targetBounds)),
        "Generic upper bounds must match after substitution")
    }
    require(ids(from.getParams).zip(ids(to.getParams).drop(1)).forall { (a, b) =>
      remapType(value(a).asParameter().getBase.getType) == value(b).asParameter().getBase.getType
    }, "Corresponding parameter types must match after substitution")
    def returnType(f: FunctionT): Long =
      pkg.getTypes()(index(f.getBase.getBase.getType)).asFuncType().getBase.getArgTys.last
    require(remapType(returnType(from)) == returnType(to), "Return types must match after substitution")
    require(pkg.getPackageInitFunc != sourceId && pkg.getPackageLiteralInitFunc != sourceId,
      "Cannot remove a package initializer body")

    // 2. Build the source-parameter-ID -> target-parameter-ID mapping.
    val parameterMap = ids(from.getParams).zip(ids(to.getParams).drop(1)).toMap
    parameterMap.foreach { (oldId, newId) =>
      require(value(oldId).getType == ValueElem.Parameter &&
        value(newId).getType == ValueElem.Parameter, "Invalid parameter IDs")
      require(value(oldId).asParameter().getOwnedFunc == sourceId &&
        value(newId).asParameter().getOwnedFunc == targetId, "Invalid parameter owners")
    }

    val root = value(from.getBody)
    require(root.getType == ValueElem.BlockGroup, "Invalid body ID")
    val group = root.asBlockGroup()
    require(group.getOwnedFunc == sourceId && group.getOwnedExpression == 0,
      "Expected a function-owned root group")
    val blocks = ids(group.getBlocks).map { id =>
      val v = value(id)
      require(v.getType == ValueElem.Block, "Invalid block ID")
      val b = v.asBlock()
      require(b.getParentGroup == from.getBody, "Invalid block owner")
      id -> b
    }
    require(blocks.exists(_._1 == group.getEntryBlock), "Invalid entry block")
    val expressions = blocks.flatMap { (blockId, block) =>
      ids(block.getExprs).map { id =>
        val e = expressionBase(pkg.getExprs()(index(id)).getValue)
        require(e.getOwner == blockId, "Invalid expression owner")
        require(ids(e.getBlockGroups).isEmpty, "Nested block groups are unsupported")
        require(!ids(e.getOperands).contains(sourceId),
          "Self references need explicit handling of the extra argument")
        e
      }
    }

    given changes: Changes = new Changes

    // 3. Reuse the moved graph's IDs. Locals, branch targets, expression owners,
    // predecessors, and associatedExpr links stay valid. Remap parameter and
    // type references, then transfer ownership. No body is shared between functions.
    expressions.foreach { e =>
      changes.set(ids(e.getOperands).map(id => parameterMap.getOrElse(id, id)))(e.setOperands)
      changes.set(remapType(e.getResultTy))(e.setResultTy)
      remapAnnotations(e.getBase, remapType.apply)
    }
    blocks.foreach { (_, b) =>
      changes.set(remapType(b.getBase.getType))(b.getBase.setType)
      remapAnnotations(b.getBase.getBase, remapType.apply)
      changes.set(ids(b.getExceptionCatchList).map(remapType.apply))(b.setExceptionCatchList)
      ids(b.getExprs).foreach { id =>
        remapExpressionTypes(pkg.getExprs()(index(id)).getValue, remapType.apply)
      }
    }
    changes.set(remapType(group.getBase.getType))(group.getBase.setType)
    remapAnnotations(group.getBase.getBase, remapType.apply)
    // Printed local identifiers are function-scoped too. Make room for the
    // receiver, without changing global values-table IDs or associatedExpr IDs.
    val localIds = (expressions.map(_.getResultLocalVar) :+ from.getRetVal)
      .filter(_ != 0).distinct
      .sortBy(id => value(id).asLocalVar().getBase.getIdentifier.stripPrefix("%").toLong)
    var nextLocal = target.params.size.toLong
    localIds.foreach { id =>
      require(value(id).getType == ValueElem.LocalVar, "Expected a result local")
      val base = value(id).asLocalVar().getBase
      changes.set(s"%$nextLocal")(base.setIdentifier)
      changes.set(remapType(base.getType))(base.setType)
      remapAnnotations(base.getBase, remapType.apply)
      nextLocal += 1
    }
    changes.set(targetId)(group.setOwnedFunc)
    changes.set(from.getBody)(to.setBody)
    changes.set(from.getRetVal)(to.setRetVal)
    changes.set(nextLocal)(to.setLocalId)
    changes.set(math.max(to.getBlockId, from.getBlockId))(to.setBlockId)
    changes.set(math.max(to.getBlockGroupId, from.getBlockGroupId))(to.setBlockGroupId)
    changes.set(0L)(from.setBody)
    changes.set(0L)(from.setRetVal)

    changes.commit()
    References(blocks.flatMap((_, b) => ids(b.getExprs)).toSeq, parameterMap, remapType.apply)
  }

  /** Resolve all substitutions and counters before committing any table edits. */
  private class Changes {
    private val pending = scala.collection.mutable.ArrayBuffer.empty[() => Unit]
    def set[A](next: A)(setter: A => Unit): Unit = pending += (() => setter(next))
    def commit(): Unit = pending.foreach(_())
  }

  private def index(id: Long): Int = {
    require(id > 0, "CHIR entity IDs start at 1")
    Math.toIntExact(id - 1)
  }
  private def ids(xs: Array[Long]): Array[Long] =
    if (xs == null) Array.emptyLongArray else xs

  // Reuse existing destination type entries rather than mutating shared source
  // types. E.g. T_source -> T_target, and Ref<T_source> -> Ref<T_target>.
  // Reject a missing composite specialization instead of retaining source T.
  private class TypeRemapper(pkg: CHIRPackageT, generics: Map[Long, Long]) {
    private val memo = scala.collection.mutable.Map.from(generics)
    private val active = scala.collection.mutable.Set.empty[Long]
    def apply(id: Long): Long = {
      if (id == 0) return 0
      memo.get(id) match {
        case Some(mapped) => mapped
        case None =>
          require(!active(id), "Recursive composite type substitution is unsupported")
          active += id
          val original = pkg.getTypes()(index(id))
          val args = ids(typeBase(original.getValue).getArgTys)
          val mappedArgs = args.map(apply)
          val mapped = if (args.sameElements(mappedArgs)) id else {
            val candidate = pkg.getTypes().indexWhere { t =>
              t.getType == original.getType &&
                typeShape(t.getValue) == typeShape(original.getValue) &&
                ids(typeBase(t.getValue).getArgTys).sameElements(mappedArgs)
            }
            require(candidate >= 0, s"Missing substituted composite type for type ID $id")
            candidate.toLong + 1
          }
          active -= id
          memo(id) = mapped
          mapped
      }
    }
  }

  private def typeBase(x: Object): TypeT = x match {
    case t: TypeT => t
    case t: RawArrayTypeT => t.getBase
    case t: VArrayTypeT => t.getBase
    case t: FuncTypeT => t.getBase
    case t: CustomTypeT => t.getBase
    case t: GenericTypeT => t.getBase
    case _ => throw new IllegalArgumentException("Unsupported type table")
  }

  private def typeShape(x: Object): Any = x match {
    case t: RawArrayTypeT => (t.getBase.getKind, t.getDims)
    case t: VArrayTypeT => (t.getBase.getKind, t.getSize)
    case t: FuncTypeT => (t.getBase.getKind, t.getIsCfuncType, t.getHasVarArg)
    case t: CustomTypeT => (t.getBase.getKind, t.getCustomTypeDef)
    case t: GenericTypeT => (t.getBase.getKind, t.getIdentifier)
    case t: TypeT => t.getKind
  }

  private def remapAnnotations(base: BaseT, remap: Long => Long)(using changes: Changes): Unit = {
    if (base != null && base.getAnnos != null) base.getAnnos.foreach { a =>
      a.getValue match {
        case t: OverrideSrcFuncTypeT => changes.set(remap(t.getType))(t.setType)
        case _ => ()
      }
    }
  }

  private def remapExpressionTypes(x: Object, remap: Long => Long)(using changes: Changes): Unit = {
    def call(c: FuncCallT): Unit = {
      changes.set(remap(c.getObjType))(c.setObjType)
      changes.set(ids(c.getInstantiatedTypeArgs).map(remap))(c.setInstantiatedTypeArgs)
    }
    x match {
      case e: AllocateBaseT => changes.set(remap(e.getAllocatedType))(e.setAllocatedType)
      case e: ApplyBaseT => call(e.getBase)
      case e: InvokeBaseT => call(e.getBase)
      case e: IntrinsicBaseT => call(e.getBase)
      case e: InstanceOfT => changes.set(remap(e.getTargetType))(e.setTargetType)
      case e: RawArrayAllocateBaseT => changes.set(remap(e.getElementType))(e.setElementType)
      case e: GetInstantiateValueT => changes.set(ids(e.getInstantiateTypes).map(remap))(e.setInstantiateTypes)
      case e: GetRTTIStaticT => changes.set(remap(e.getRttiType))(e.setRttiType)
      case _ => ()
    }
  }

  // All supported union alternatives ultimately contain an ExpressionT.
  private def expressionBase(x: Object): ExpressionT = x match {
    case e: ExpressionT => e
    case e: UnaryExpressionBaseT => e.getBase
    case e: BinaryExpressionBaseT => e.getBase
    case e: AllocateBaseT => e.getBase
    case e: ApplyBaseT => e.getBase.getBase
    case e: InvokeBaseT => e.getBase.getBase
    case e: IntrinsicBaseT => e.getBase.getBase
    case e: NumericCastBaseT => e.getBase
    case e: BranchT => e.getBase
    case e: MultiBranchT => e.getBase
    case e: GetElementRefT => e.getBase
    case e: GetElementByNameT => e.getBase
    case e: StoreElementRefT => e.getBase
    case e: StoreElementByNameT => e.getBase
    case e: InstanceOfT => e.getBase
    case e: FieldT => e.getBase
    case e: FieldByNameT => e.getBase
    case e: RawArrayAllocateBaseT => e.getBase
    case e: DebugT => e.getBase
    case e: GetInstantiateValueT => e.getBase
    case e: GetRTTIStaticT => e.getBase
    case _ => throw new IllegalArgumentException(
      s"Unsupported expression: ${Option(x).map(_.getClass.getSimpleName)}")
  }
}
