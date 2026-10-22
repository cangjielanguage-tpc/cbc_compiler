package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*

import scala.collection.mutable
import scala.util.control.NonFatal

/** Structural validation of the complete schema plus executable body graphs.
  * This checks reference categories, result types and Boolean conditions, not
  * the full language type system or generic constraint solver.
  */
private[v13002] object EditableValidation {
  import EditableStorage.*
  enum EntityTable {
    case Types, Values, Expressions, Definitions
  }
  final case class EntityRef(table: EntityTable, id: Long)
  final case class ValidationIssue(message: String, entity: Option[EntityRef] = None)
  import EntityTable.*

  def validate(pkg: EditablePackageImpl): Seq[ValidationIssue] = {
    val data = pkg.data
    val problems = mutable.ArrayBuffer.empty[ValidationIssue]
    def report(message: String, ref: Option[EntityRef] = None): Unit = problems += ValidationIssue(message, ref)
    def check(test: Boolean, message: String, ref: Option[EntityRef] = None): Unit = if (!test) report(message, ref)
    def tableSize(table: EntityTable): Int = table match {
      case Types => length(data.getTypes)
      case Values => length(data.getValues)
      case Expressions => length(data.getExprs)
      case Definitions => length(data.getDefs)
    }
    def reference(table: EntityTable, id: Long, field: String, ref: Option[EntityRef]): Unit =
      check(id >= 0 && id <= tableSize(table), s"$field has invalid $table ID $id", ref)

    // Generated object fields are visited recursively so annotations, lambda
    // metadata, vtables and other fields not exposed by CHIR are also checked.
    // Only schema ID fields are interpreted as references; counters/bitsets and
    // arbitrary numeric literal values must never be treated as entity IDs.
    def walk(obj: Object, ref: Option[EntityRef]): Unit = {
      if (obj == null) { report("Missing schema payload", ref); return }
      val kind = obj.getClass.getSimpleName
      obj.getClass.getDeclaredFields.filterNot(f => java.lang.reflect.Modifier.isStatic(f.getModifiers)).foreach { f =>
        f.setAccessible(true)
        val field = f.get(obj)
        referenceFields.get((kind, f.getName)) match {
          case Some(table) => field match {
            case id: java.lang.Long => reference(table, id.longValue(), s"$kind.${f.getName}", ref)
            case ids: Array[Long] => ids.foreach(reference(table, _, s"$kind.${f.getName}", ref))
            case null => ()
            case _ => report(s"Unexpected reference representation: $kind.${f.getName}", ref)
          }
          case None => field match {
            case children: Array[?] => children.foreach {
              case child: Object if isSchema(child) => walk(child, ref)
              case _ => ()
            }
            case child: Object if isSchema(child) => walk(child, ref)
            case _ => ()
          }
        }
      }
    }
    def safely(ref: Option[EntityRef])(action: => Unit): Unit =
      try action catch { case NonFatal(e) => report(Option(e.getMessage).getOrElse(e.getClass.getSimpleName), ref) }

    Option(data.getTypes).toSeq.flatMap(_.toSeq).zipWithIndex.foreach { (u, i) =>
      val ref = Some(EntityRef(Types, i + 1L))
      safely(ref) { check(typeTags.get(u.getType).contains(u.getValue.getClass.getSimpleName), "Type union tag/payload mismatch", ref); walk(u.getValue, ref) }
    }
    Option(data.getValues).toSeq.flatMap(_.toSeq).zipWithIndex.foreach { (u, i) =>
      val ref = Some(EntityRef(Values, i + 1L))
      safely(ref) { check(valueTags.get(u.getType).contains(u.getValue.getClass.getSimpleName), "Value union tag/payload mismatch", ref); walk(u.getValue, ref); valueBase(u.getValue) }
    }
    Option(data.getExprs).toSeq.flatMap(_.toSeq).zipWithIndex.foreach { (u, i) =>
      val ref = Some(EntityRef(Expressions, i + 1L))
      safely(ref) {
        val expected = ExpressionElem.names.lift(u.getType.toInt).map(_ + "T")
        check(expected.contains(u.getValue.getClass.getSimpleName), "Expression union tag/payload mismatch", ref)
        walk(u.getValue, ref); expressionBase(u.getValue)
      }
    }
    Option(data.getDefs).toSeq.flatMap(_.toSeq).zipWithIndex.foreach { (u, i) =>
      val ref = Some(EntityRef(Definitions, i + 1L))
      safely(ref) {
        val expected = CustomTypeDefElem.names.lift(u.getType.toInt).map(_ + "T")
        check(expected.contains(u.getValue.getClass.getSimpleName), "Definition union tag/payload mismatch", ref)
        walk(u.getValue, ref)
      }
    }
    reference(Values, data.getPackageInitFunc, "packageInitFunc", None)
    reference(Values, data.getPackageLiteralInitFunc, "packageLiteralInitFunc", None)
    // Avoid cascading invalid-index failures during graph validation.
    if (problems.nonEmpty) return problems.toSeq

    val usedGroups = mutable.Set.empty[Long]
    def validateGroup(id: Long, functionId: Long, enclosing: Long,
                      localUses: mutable.Buffer[(Long, EntityRef)],
                      lambdaUses: mutable.Buffer[(Long, EntityRef)]): Set[Long] = {
      val ref = Some(EntityRef(Values, id))
      require(usedGroups.add(id), s"Block group $id has multiple owners or a cyclic nesting")
      val g = pkg.groupTable(id)
      check(if (enclosing == 0) g.getOwnedFunc == functionId && g.getOwnedExpression == 0
        else g.getOwnedExpression == enclosing && g.getOwnedFunc == 0, "Incorrect block-group owner", ref)
      val members = ids(g.getBlocks)
      check(members.distinct.length == members.length, "Duplicate blocks in group", ref)
      check(members.contains(g.getEntryBlock), "Entry block is not in group", ref)
      val expected = mutable.LongMap.empty[mutable.Set[Long]]
      val expressionIds = mutable.Set.empty[Long]
      val successors = mutable.LongMap.empty[Array[Long]]
      members.foreach { bid =>
        val b = pkg.blockTable(bid)
        val bref = Some(EntityRef(Values, bid))
        check(b.getParentGroup == id, "Incorrect block parent", bref)
        val exprs = ids(b.getExprs)
        check(exprs.distinct.length == exprs.length, "Duplicate expressions in block", bref)
        exprs.zipWithIndex.foreach { (eid, position) =>
          check(expressionIds.add(eid), "Expression shared between blocks", bref)
          val payload = pkg.expressionTable(eid).getValue
          val e = expressionBase(payload)
          val eref = Some(EntityRef(Expressions, eid))
          check(e.getOwner == bid, "Incorrect expression owner", eref)
          check(!isTerminator(e) || position == exprs.length - 1, "Terminator before end of block", eref)
          ids(e.getOperands).foreach { operand =>
            check(operand != 0, "Null expression operand", eref)
            if (operand > 0) {
              val v = pkg.valueTable(operand)
              if (v.getType == ValueElem.Parameter) {
                val p = v.asParameter()
                check(p.getOwnedFunc == functionId || p.getOwnedLambda != 0,
                  "Expression uses another function's parameter", eref)
                if (p.getOwnedLambda != 0) lambdaUses += ((p.getOwnedLambda, eref.get))
              } else if (v.getType == ValueElem.LocalVar) {
                localUses += ((operand, eref.get))
              }
            }
          }
          if (e.getResultLocalVar != 0) {
            val v = pkg.valueTable(e.getResultLocalVar)
            check(v.getType == ValueElem.LocalVar, "Expression result is not a local", eref)
            if (v.getType == ValueElem.LocalVar) {
              check(v.asLocalVar().getAssociatedExpr == eid, "Incorrect local definition link", eref)
              check(e.getResultTy == 0 || v.asLocalVar().getBase.getType == e.getResultTy,
                "Expression/local result types disagree", eref)
            }
          }
          if (e.getKind == CHIRExprKind.Goto) {
            val ops = ids(e.getOperands)
            check(ops.length == 1, "Goto needs one operand", eref)
            if (ops.length == 1 && ops(0) > 0)
              check(pkg.valueTable(ops(0)).getType == ValueElem.Block, "Goto target is not a block", eref)
          }
          if (e.getKind == CHIRExprKind.Branch) {
            val ops = ids(e.getOperands)
            check(ops.length == 3, "Branch needs condition and two targets", eref)
            if (ops.length == 3 && ops(0) > 0) {
              val typeId = valueBase(pkg.valueTable(ops(0)).getValue).getType
              check(typeId > 0 && typeBase(data.getTypes()((typeId - 1).toInt).getValue).getKind == CHIRTypeKind.BOOLEAN,
                "Branch condition is not Boolean", eref)
            }
            if (ops.length == 3) ops.drop(1).foreach { target =>
              check(target > 0 && pkg.valueTable(target).getType == ValueElem.Block,
                "Branch target is not a block", eref)
            }
          }
          nestedGroups(payload).foreach(gid => expressionIds ++= validateGroup(gid, functionId, eid, localUses, lambdaUses))
        }
        val edges = exprs.lastOption.map(eid => pkg.successorIds(expressionBase(pkg.expressionTable(eid).getValue)))
          .getOrElse(Array.emptyLongArray)
        successors(bid) = edges
        edges.foreach { destination =>
          check(members.contains(destination), "Control-flow target is outside group", bref)
          expected.getOrElseUpdate(destination, mutable.Set.empty) += bid
        }
      }
      members.foreach { bid =>
        check(ids(pkg.blockTable(bid).getPredecessors).toSet == expected.get(bid).map(_.toSet).getOrElse(Set.empty),
          "Predecessors disagree with successors", Some(EntityRef(Values, bid)))
      }
      val reachable = mutable.Set.empty[Long]
      val todo = mutable.Stack(g.getEntryBlock)
      while (todo.nonEmpty) {
        val bid = todo.pop()
        if (members.contains(bid) && reachable.add(bid)) {
          val exprs = ids(pkg.blockTable(bid).getExprs)
          check(exprs.nonEmpty && isTerminator(expressionBase(pkg.expressionTable(exprs.last).getValue)),
            "Reachable block has no terminator", Some(EntityRef(Values, bid)))
          todo.pushAll(successors.getOrElse(bid, Array.emptyLongArray))
        }
      }
      expressionIds.toSet
    }
    Option(data.getValues).toSeq.flatMap(_.toSeq).zipWithIndex.foreach { (v, i) =>
      if (v.getType == ValueElem.Function) {
        val id = i + 1L
        val ref = Some(EntityRef(Values, id))
        safely(ref) {
          val f = v.asFunction()
          ids(f.getParams).foreach(pid => check(pkg.valueTable(pid).getType == ValueElem.Parameter, "Function parameter is not a parameter", ref))
          if (f.getBody == 0) check(f.getRetVal == 0, "Declaration has a return local", ref)
          else {
            val localUses = mutable.ArrayBuffer.empty[(Long, EntityRef)]
            val lambdaUses = mutable.ArrayBuffer.empty[(Long, EntityRef)]
            val expressionIds = validateGroup(f.getBody, id, 0, localUses, lambdaUses)
            localUses.foreach { (localId, owner) =>
              val definition = pkg.valueTable(localId).asLocalVar().getAssociatedExpr
              check(expressionIds(definition) &&
                expressionBase(pkg.expressionTable(definition).getValue).getResultLocalVar == localId,
                "Local operand is defined outside the body or has an invalid result link", Some(owner))
            }
            lambdaUses.foreach { (lambdaId, owner) =>
              check(expressionIds(lambdaId) && pkg.expressionTable(lambdaId).getType == ExpressionElem.Lambda,
                "Parameter belongs to a lambda outside the body", Some(owner))
            }
            if (f.getRetVal != 0) {
              val result = pkg.valueTable(f.getRetVal)
              check(result.getType == ValueElem.LocalVar, "Return value is not a local", ref)
              if (result.getType == ValueElem.LocalVar) {
                check(result.asLocalVar().getIsRetVal, "Return local is missing its return flag", ref)
                check(expressionIds(result.asLocalVar().getAssociatedExpr), "Return local is defined outside body", ref)
              }
            }
          }
        }
      }
    }
    problems.toSeq
  }

  private def isSchema(obj: Object): Boolean = obj.getClass.getPackage.getName.endsWith("v13002.PackageFormat")
  private val typeTags = Map[Byte, String](TypeElem.Type -> "TypeT", TypeElem.RawArrayType -> "RawArrayTypeT",
    TypeElem.VArrayType -> "VArrayTypeT", TypeElem.FuncType -> "FuncTypeT", TypeElem.CustomType -> "CustomTypeT", TypeElem.GenericType -> "GenericTypeT")
  private val valueTags = Map[Byte, String](ValueElem.BoolLiteral -> "BoolLiteralT", ValueElem.RuneLiteral -> "RuneLiteralT",
    ValueElem.StringLiteral -> "StringLiteralT", ValueElem.IntLiteral -> "IntLiteralT", ValueElem.FloatLiteral -> "FloatLiteralT",
    ValueElem.UnitLiteral -> "UnitLiteralT", ValueElem.NullLiteral -> "NullLiteralT", ValueElem.Parameter -> "ParameterT",
    ValueElem.LocalVar -> "LocalVarT", ValueElem.GlobalVar -> "GlobalVarT", ValueElem.Function -> "FunctionT",
    ValueElem.Block -> "BlockT", ValueElem.BlockGroup -> "BlockGroupT")

  private val referenceFields: Map[(String, String), EntityTable] = {
    def fields(table: EntityTable, entries: (String, String)*): Seq[((String, String), EntityTable)] =
      entries.flatMap((name, fields) => fields.split(" ").map(field => (name + "T", field) -> table))
    (fields(Types, "Type" -> "argTys", "GenericType" -> "upperBounds", "OverrideSrcFuncType" -> "type",
      "Value" -> "type", "FuncSigInfo" -> "funcType genericTypeParams", "Function" -> "genericTypeParams",
      "Block" -> "exceptionCatchList", "MemberVarInfo" -> "type",
      "VirtualMethodInfo" -> "sigType methodGenericTypeParams originalType parentType returnType",
      "VTableInType" -> "srcParentType", "CustomTypeDef" -> "type implementedInterfaces",
      "EnumCtorInfo" -> "funcType", "ExtendDef" -> "extendedType genericParams", "ClassDef" -> "superClass",
      "Expression" -> "resultTy", "AllocateBase" -> "allocatedType", "FuncCall" -> "instantiatedTypeArgs objType",
      "InstanceOf" -> "targetType", "RawArrayAllocateBase" -> "elementType", "Lambda" -> "funcTy genericTypeParams",
      "GetInstantiateValue" -> "instantiateTypes", "GetRTTIStatic" -> "rttiType") ++
      fields(Values, "WrappedRawMethod" -> "rawMethod", "Parameter" -> "ownedFunc", "GlobalVar" -> "initializer",
        "Function" -> "genericDecl paramDftValHostFunc body params retVal", "Block" -> "parentGroup predecessors",
        "BlockGroup" -> "entryBlock blocks ownedFunc", "MemberVarInfo" -> "initializerFunc", "VirtualMethodInfo" -> "instance",
        "CustomTypeDef" -> "methods staticMemberVars instanceVarInitFunc", "ClassDef" -> "annotationTargets",
        "Expression" -> "operands blockGroups owner resultLocalVar", "SpawnBase" -> "executeClosure", "Lambda" -> "params body retVal") ++
      fields(Expressions, "Parameter" -> "ownedLambda", "LocalVar" -> "associatedExpr", "Block" -> "exprs", "BlockGroup" -> "ownedExpression") ++
      fields(Definitions, "CustomType" -> "customTypeDef", "GlobalValue" -> "declaredParent", "MemberVarInfo" -> "outerDef",
        "CustomTypeDef" -> "customTypeDefId genericDecl")).toMap
  }
}
