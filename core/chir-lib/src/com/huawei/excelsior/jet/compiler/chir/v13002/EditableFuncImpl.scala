package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.{CHIR, EditableCHIR}
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*

/** A function view whose body edits are stored in the unpacked FlatBuffer.
  *
  * The enclosing editable package supplies its registry and resolvers. The
  * existing read-only PackageImpl does not construct this wrapper yet.
  * Unchanged signature/metadata getters delegate to the original read view;
  * body and retVal resolve the current backing IDs on every access.
  */
final class EditableFuncImpl(
  original: CHIR.Func,
  val ownerPackage: EditableCHIR.Package,
  data: CHIRPackageT,
  resolveGroup: Long => EditableCHIR.BlockGroup,
  resolveLocal: Long => CHIR.LocalVar,
  afterReplace: () => Unit = () => ()
                            
) extends EditableCHIR.Func {
  def id: Long = original.id
  def tpe: CHIR.FuncType = original.tpe
  def identifier: String = original.identifier
  def srcCodeIdentifier: String = original.srcCodeIdentifier
  def packageName: String = original.packageName
  def kind: CHIR.Func.Kind = original.kind
  def genericTypeParams: Seq[CHIR.GenericType] = original.genericTypeParams
  def params: Seq[CHIR.Parameter] = original.params
  def sourceFile: Option[String] = original.sourceFile
  def annotations: Seq[CHIR.Annotation] = original.annotations
  def attributes: Seq[CHIR.Attribute] = original.attributes
  def declaringDef: Option[CHIR.CustomTypeDef] = original.declaringDef

  private def table: FunctionT = FunctionBodyReplacement.function(data, id)

  def body: Option[EditableCHIR.BlockGroup] =
    Option.when(table.getBody != 0)(resolveGroup(table.getBody))

  def retVal: Option[CHIR.LocalVar] =
    Option.when(table.getRetVal != 0)(resolveLocal(table.getRetVal))

  def replaceBody(
    body: Option[EditableCHIR.BlockGroup],
    retVal: Option[CHIR.LocalVar]
  ): Unit = {
    require(ownerPackage.formatVersion == CHIR.defaultVersion, "Expected a v13002 package")
    body.foreach { group =>
      require(group.ownerPackage eq ownerPackage, "Body belongs to another package")
    }
    // Registry lookup rejects foreign/unregistered locals and groups. Resolve
    // every argument before mutating anything, including the previous owner.
    val bodyId = body.map(ownerPackage.valueId).getOrElse(0L)
    val retValId = retVal.map(ownerPackage.valueId).getOrElse(0L)
    FunctionBodyReplacement.replace(data, id, bodyId, retValId)
    afterReplace()
  }
}

/** Versioned storage operation, separated so its graph checks and FlatBuffers
  * round trip can be tested without a complete editable-package implementation.
  */
private[v13002] object FunctionBodyReplacement {
  def function(data: CHIRPackageT, id: Long): FunctionT = {
    val entry = value(data, id)
    require(entry.getType == ValueElem.Function, s"Value $id is not a function")
    entry.asFunction()
  }

  def replace(data: CHIRPackageT, functionId: Long, bodyId: Long, retValId: Long): Unit = {
    val target = function(data, functionId)
    require(bodyId != 0 || retValId == 0, "A declaration cannot have a return local")
    val oldBody = Option.when(target.getBody != 0)(group(data, target.getBody))
    oldBody.foreach { g =>
      require(g.getOwnedFunc == functionId && g.getOwnedExpression == 0,
        "Existing body has inconsistent ownership")
    }
    val oldRetVal = Option.when(target.getRetVal != 0)(local(data, target.getRetVal))
    val replacement = Option.when(bodyId != 0)(group(data, bodyId))
    val newRetVal = Option.when(retValId != 0)(local(data, retValId))

    val groups = scala.collection.mutable.Set.empty[Long]
    val blocks = scala.collection.mutable.Set.empty[Long]
    val expressions = scala.collection.mutable.Set.empty[Long]
    val resultLocals = scala.collection.mutable.Set.empty[Long]
    replacement.foreach { root =>
      require(root.getOwnedExpression == 0,
        "Body is owned by another function or expression; explicitly move/clone it first")
      require(!data.getValues.exists { entry =>
        entry.getType == ValueElem.Function &&
          (entry.asFunction() ne target) && entry.asFunction().getBody == bodyId
      }, "Another function still references the body")

      def visit(groupId: Long, enclosingExpression: Long): Unit = {
        require(groups.add(groupId), "Body contains a shared or cyclic block group")
        val current = group(data, groupId)
        if (enclosingExpression != 0) {
          require(current.getOwnedExpression == enclosingExpression && current.getOwnedFunc == 0,
            "Nested block group has inconsistent ownership")
        }
        val blockIds = ids(current.getBlocks)
        require(blockIds.distinct.length == blockIds.length, "Duplicate blocks in body group")
        require(blockIds.contains(current.getEntryBlock), "Entry block is not in the body group")
        blockIds.foreach { blockId =>
          require(blocks.add(blockId), "Block is shared between body groups")
          val entry = value(data, blockId)
          require(entry.getType == ValueElem.Block, "Body contains a non-block value")
          val block = entry.asBlock()
          require(block.getParentGroup == groupId, "Block has inconsistent parent group")
          ids(block.getExprs).foreach { expressionId =>
            require(expressions.add(expressionId), "Expression belongs to multiple blocks")
            val payload = expression(data, expressionId)
            val e = expressionBase(payload)
            require(e.getOwner == blockId, "Expression has inconsistent block owner")
            if (e.getResultLocalVar != 0) {
              val result = local(data, e.getResultLocalVar)
              require(result.getAssociatedExpr == expressionId, "Result local points to another expression")
              require(resultLocals.add(e.getResultLocalVar), "Expressions share a result local")
            }
            val nested = ids(e.getBlockGroups) ++ (payload match {
              case lambda: LambdaT if lambda.getBody != 0 => Array(lambda.getBody)
              case _ => Array.emptyLongArray
            })
            nested.distinct.foreach(visit(_, expressionId))
          }
        }
      }
      visit(bodyId, 0L)

      newRetVal.foreach { _ =>
        require(resultLocals(retValId), "Return local is not defined by the replacement body")
      }
      expressions.foreach { expressionId =>
        ids(expressionBase(expression(data, expressionId)).getOperands).foreach { operand =>
          val entry = value(data, operand)
          if (entry.getType == ValueElem.Parameter) {
            val p = entry.asParameter()
            require((p.getOwnedLambda == 0 && p.getOwnedFunc == functionId) ||
              (p.getOwnedLambda != 0 && expressions(p.getOwnedLambda)),
              "Body refers to another function's parameter; remap it first")
          } else if (entry.getType == ValueElem.LocalVar) {
            require(resultLocals(operand), "Body refers to a local outside the replacement body")
          } else if (entry.getType == ValueElem.Block) {
            require(blocks(operand), "Body branches to a block outside the replacement body")
          }
        }
      }
    }

    def nextCounter(current: Long, identifiers: Iterable[String], prefix: String): Long =
      identifiers.foldLeft(current) { (next, name) =>
        if (name == null || !name.startsWith(prefix)) next
        else name.substring(prefix.length).toLongOption match {
          case Some(n) if n >= 0 => math.max(next, Math.addExact(n, 1L))
          case _ => next
        }
      }
    // Calculate counters before committing, so failures leave the graph intact.
    val localCounter = nextCounter(target.getLocalId,
      resultLocals.toSeq.map(id => local(data, id).getBase.getIdentifier) ++
        ids(target.getParams).toSeq.map(id => value(data, id).asParameter().getBase.getIdentifier), "%")
    val blockCounter = nextCounter(target.getBlockId,
      blocks.toSeq.map(id => value(data, id).asBlock().getBase.getIdentifier), "#")
    val groupCounter = nextCounter(target.getBlockGroupId,
      groups.toSeq.map(id => group(data, id).getBase.getIdentifier), "")

    // Commit only after all references, ownership and counters have been checked.
    if (target.getBody != bodyId) oldBody.foreach(_.setOwnedFunc(0L))
    if (target.getRetVal != retValId) oldRetVal.foreach(_.setIsRetVal(false))
    replacement.foreach(_.setOwnedFunc(functionId))
    newRetVal.foreach(_.setIsRetVal(true))
    target.setBody(bodyId)
    target.setRetVal(retValId)
    target.setLocalId(localCounter)
    target.setBlockId(blockCounter)
    target.setBlockGroupId(groupCounter)
  }

  private def ids(xs: Array[Long]): Array[Long] = if (xs == null) Array.emptyLongArray else xs

  private def value(data: CHIRPackageT, id: Long): ValueElemUnion = {
    require(data.getValues != null && id > 0 && id <= data.getValues.length, s"Invalid value ID $id")
    data.getValues()((id - 1).toInt)
  }

  private def group(data: CHIRPackageT, id: Long): BlockGroupT = {
    val entry = value(data, id)
    require(entry.getType == ValueElem.BlockGroup, s"Value $id is not a block group")
    entry.asBlockGroup()
  }

  private def local(data: CHIRPackageT, id: Long): LocalVarT = {
    val entry = value(data, id)
    require(entry.getType == ValueElem.LocalVar, s"Value $id is not a local variable")
    entry.asLocalVar()
  }

  private def expression(data: CHIRPackageT, id: Long): Object = {
    require(id > 0 && data.getExprs != null && id <= data.getExprs.length,
      s"Invalid expression ID $id")
    data.getExprs()((id - 1).toInt).getValue
  }

  private[v13002] def expressionBase(x: Object): ExpressionT = x match {
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
    case e: LambdaT => e.getBase
    case e: SpawnBaseT => e.getBase
    case _ => throw new IllegalArgumentException("Unsupported expression table")
  }
}
