package com.huawei.excelsior.jet.compiler.chir.v13002

import com.google.flatbuffers.FlatBufferBuilder
import com.huawei.excelsior.jet.compiler.chir.{CHIR, EditableCHIR}
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*

import java.nio.ByteBuffer
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.IdentityHashMap
import java.lang.reflect.{InvocationTargetException, Proxy}
import scala.collection.mutable
import scala.reflect.ClassTag

object EditablePackageImpl {
  def read(input: Path): EditablePackageImpl = fromBytes(Files.readAllBytes(input))

  def fromBytes(bytes: Array[Byte]): EditablePackageImpl = {
    val snapshot = bytes.clone()
    val buffer = ByteBuffer.wrap(snapshot)
    val identifier = CHIRPackage.CHIRPackageBufferHasIdentifier(buffer)
    new EditablePackageImpl(CHIRPackage.getRootAsCHIRPackage(buffer).unpack(), snapshot, identifier)
  }
}

/** An editable v13002 package retaining the complete generated object model.
  * Registered nodes have stable identities and table IDs. Existing payloads
  * without a specialized CHIR interface are preserved and exposed as generic
  * expressions; editing never discards their serialized fields.
  * Like the existing reader, this class is not thread-safe.
  */
final class EditablePackageImpl private (
  private[v13002] val data: CHIRPackageT,
  bytes: Array[Byte],
  hasIdentifier: Boolean
) extends EditableCHIR.Package with CHIRItemProvider {
  import EditableStorage.*

  private val valueViews = mutable.LongMap.empty[CHIR.Value]
  private val expressionViews = mutable.LongMap.empty[CHIR.Expression]
  // Read implementations retain the original FlatBuffer. Project their typed
  // getters through accumulated substitutions so held expression views remain
  // current after an in-memory move, without packing/reloading the package.
  private val referenceProjections = mutable.LongMap.empty[List[FunctionBodyMove.References]]
  private val valueIds = new IdentityHashMap[CHIR.Value, java.lang.Long]()
  private val expressionIds = new IdentityHashMap[CHIR.Expression, java.lang.Long]()
  private val typeIds = new IdentityHashMap[CHIR.Type, java.lang.Long]()
  private val definitionIds = new IdentityHashMap[CHIR.CustomTypeDef, java.lang.Long]()
  private lazy val reader = PackageImpl.snapshot(bytes, this)
  private val originalExpressionCount = length(data.getExprs)
  private val originalValueCount = length(data.getValues)
  private val historicalExpressionOwners = mutable.LongMap.empty[Long]
  private val historicalBlockParents = mutable.LongMap.empty[Long]
  private val historicalGroupOwners = mutable.LongMap.empty[(Long, Long)]
  private var nextBlockName = nextName(valuesOf(ValueElem.Block).map(_.asBlock().getBase.getIdentifier), "#")
  private var nextGroupName = nextName(valuesOf(ValueElem.BlockGroup).map(_.asBlockGroup().getBase.getIdentifier), "")
  rememberOwners()

  def formatVersion: Int = 13002
  def name: String = data.getName
  def values: Iterator[CHIR.Value] = (1L to length(data.getValues).toLong).iterator.map(value)
  def typeDefs: Iterator[CHIR.CustomTypeDef] =
    (1L to length(data.getDefs).toLong).iterator.map(id => getDef[CHIR.CustomTypeDef](id).get)
  def function(idx: Int): EditableCHIR.Func = getValue[EditableCHIR.Func](idx).getOrElse(
    throw new IllegalArgumentException(s"Value $idx is not a function"))
  def packageInitFunc: EditableCHIR.Func = function(Math.toIntExact(data.getPackageInitFunc))
  def packageInitLiteralFunc: EditableCHIR.Func = function(Math.toIntExact(data.getPackageLiteralInitFunc))
  def getFunc(identifier: String): Option[EditableCHIR.Func] = {
    val name = normalized(identifier)
    values.collectFirst { case f: EditableCHIR.Func if f.identifier == name => f }
  }
  def getDef(identifier: String): Option[CHIR.CustomTypeDef] = {
    val name = normalized(identifier)
    typeDefs.find(_.identifier == name)
  }

  def edit(function: CHIR.Func): EditableCHIR.Func = value(valueId(function)).asInstanceOf[EditableCHIR.Func]
  def edit(blockGroup: CHIR.BlockGroup): EditableCHIR.BlockGroup = group(valueId(blockGroup))
  def edit(block: CHIR.Block): EditableCHIR.Block = blockView(valueId(block))
  def valueId(value: CHIR.Value): Long = registered(valueIds, value, "value")
  def expressionId(expression: CHIR.Expression): Long = registered(expressionIds, expression, "expression")
  def typeId(tpe: CHIR.Type): Long = {
    // Primitive type objects are shared singletons. Use the first equivalent
    // table entry as their canonical reference, without rewriting loaded IDs.
    if (!typeIds.containsKey(tpe)) (1L to length(data.getTypes).toLong).foreach(getType[CHIR.Type])
    registered(typeIds, tpe, "type")
  }
  def definitionId(definition: CHIR.CustomTypeDef): Long = registered(definitionIds, definition, "definition")

  def getType[T >: Null <: CHIR.Type: ClassTag](id: Long): Option[T] = {
    val result = reader.getType[T](id)
    result.foreach(t => if (!typeIds.containsKey(t)) typeIds.put(t, Long.box(id)))
    result
  }
  def getDef[T >: Null <: CHIR.CustomTypeDef: ClassTag](id: Long): Option[T] = {
    val result = reader.getDef[T](id)
    result.foreach(d => definitionIds.put(d, Long.box(id)))
    result
  }
  def getValue[T >: Null <: CHIR.Value: ClassTag](id: Long): Option[T] =
    if (id <= 0) None else Some(value(id)).collect { case t: T => t }

  private def value(id: Long): CHIR.Value = valueViews.getOrElseUpdate(id, {
    val entry = valueTable(id)
    val result: CHIR.Value = entry.getType match {
      case ValueElem.Function =>
        val original = reader.getValue[CHIR.Func](id).get
        valueIds.put(original, Long.box(id))
        new EditableFuncImpl(original, this, data, group, local, () => rememberOwners())
      case ValueElem.Block => new EditableBlockImpl(this, id)
      case ValueElem.BlockGroup => new EditableBlockGroupImpl(this, id)
      case ValueElem.LocalVar => new CHIR.LocalVar {
        private def table = valueTable(id).asLocalVar()
        def identifier: String = table.getBase.getIdentifier
        def srcCodeIdentifier: String = table.getSrcCodeIdentifier
        def tpe: CHIR.Type = getType[CHIR.Type](table.getBase.getType).get
        def associatedExpr: CHIR.Expression = getExpr[CHIR.Expression](table.getAssociatedExpr)
      }
      case ValueElem.UnitLiteral => new CHIR.UnitLiteralValue {
        def tpe: CHIR.Type = getType[CHIR.Type](entry.asUnitLiteral().getBase.getBase.getType).get
      }
      case _ => reader.getValue[CHIR.Value](id).get
    }
    valueIds.put(result, Long.box(id))
    result
  })

  def getExpr[T >: Null <: CHIR.Expression: ClassTag](id: Long): T = {
    if (id <= 0) return null
    expressionViews.getOrElseUpdate(id, {
      val e = expressionBase(expressionTable(id).getValue)
      val view: CHIR.Expression = e.getKind match {
        case CHIRExprKind.Goto => new CHIR.Goto {
          def destination: CHIR.Block = blockView(ids(e.getOperands).head)
          def successors: Seq[CHIR.Block] = Seq(destination)
          def debugLoc: Option[CHIR.DebugLocation] = location(e)
        }
        case CHIRExprKind.Branch => new CHIR.Branch {
          def condition: CHIR.Value = value(ids(e.getOperands)(0))
          def trueBlock: CHIR.Block = blockView(ids(e.getOperands)(1))
          def falseBlock: CHIR.Block = blockView(ids(e.getOperands)(2))
          def successors: Seq[CHIR.Block] = Seq(trueBlock, falseBlock)
          def debugLoc: Option[CHIR.DebugLocation] = location(e)
        }
        case _ if id <= originalExpressionCount =>
          try projectedExpression(id, reader.getExpr[CHIR.Expression](id))
          catch { case _: MatchError => opaque(e) }
        case _ => opaque(e)
      }
      expressionIds.put(view, Long.box(id))
      view
    }).asInstanceOf[T]
  }

  private def projectedExpression(id: Long, original: CHIR.Expression): CHIR.Expression = {
    def interfaces(c: Class[?]): Seq[Class[?]] =
      if (c == null) Seq.empty
      else c.getInterfaces.toSeq.flatMap(i => i +: interfaces(i)) ++ interfaces(c.getSuperclass)
    val contracts = interfaces(original.getClass).distinct.filter(
      _.getName.startsWith(classOf[CHIR.Expression].getName.stripSuffix("Expression")))
    Proxy.newProxyInstance(getClass.getClassLoader, contracts.toArray, (proxy, method, args) => {
      method.getName match {
        case "equals" => java.lang.Boolean.valueOf(proxy eq args(0))
        case "hashCode" => Integer.valueOf(System.identityHashCode(proxy))
        case "toString" => s"EditableExpression($id)"
        case _ =>
          val result = try method.invoke(original, Option(args).getOrElse(Array.empty[Object])*)
          catch { case e: InvocationTargetException => throw e.getCause }
          def project(x: Any, refs: FunctionBodyMove.References): Any = x match {
            case p: CHIR.Parameter => value(refs.parameters.getOrElse(valueId(p), valueId(p)))
            case t: CHIR.Type => getType[CHIR.Type](refs.remapType(typeId(t))).get
            case xs: Seq[?] => xs.map(project(_, refs))
            case x: Option[?] => x.map(project(_, refs))
            case other => other
          }
          referenceProjections.getOrElse(id, Nil).reverse.foldLeft(result: Any)(project).asInstanceOf[Object]
      }
    }).asInstanceOf[CHIR.Expression]
  }

  def moveBody(source: EditableCHIR.Func, destination: EditableCHIR.Func): Unit = {
    valueId(source); valueId(destination)
    require((source.ownerPackage eq this) && (destination.ownerPackage eq this),
      "Functions belong to another package")
    val refs = FunctionBodyMove.move(data, source, destination)
    refs.expressions.foreach { id =>
      referenceProjections(id) = refs :: referenceProjections.getOrElse(id, Nil)
    }
    rememberOwners()
    rebuildPredecessors()
    refreshCounters()
  }

  private def opaque(e: ExpressionT): CHIR.Expression =
    if (isTerminator(e)) new CHIR.Terminator with CHIR.HasSuccessors {
      def successors: Seq[CHIR.Block] = successorIds(e).map(blockView).toSeq
      def debugLoc: Option[CHIR.DebugLocation] = location(e)
    } else new CHIR.Expression {
      def debugLoc: Option[CHIR.DebugLocation] = location(e)
    }

  def newBlock(isLandingPad: Boolean = false): EditableCHIR.Block = {
    val b = new BlockT
    b.setBase(newValue(s"#$nextBlockName", ValueKind.BLOCK))
    b.setIsLandingPadBlock(isLandingPad)
    nextBlockName = Math.addExact(nextBlockName, 1L)
    blockView(appendValue(ValueElem.Block, b))
  }

  def newBlockGroup(blocks: Seq[EditableCHIR.Block], entryBlock: EditableCHIR.Block): EditableCHIR.BlockGroup = {
    val blockIds = checkedBlocks(0L, blocks, entryBlock)
    val g = new BlockGroupT
    g.setBase(newValue(nextGroupName.toString, ValueKind.BLOCK_GROUP))
    g.setBlocks(blockIds)
    g.setEntryBlock(valueId(entryBlock))
    nextGroupName = Math.addExact(nextGroupName, 1L)
    val id = appendValue(ValueElem.BlockGroup, g)
    blockIds.foreach(blockTable(_).setParentGroup(id))
    rememberOwners()
    rebuildPredecessors()
    group(id)
  }

  def newGoto(destination: CHIR.Block, debugLoc: Option[CHIR.DebugLocation] = None): CHIR.Goto = {
    val destinationId = valueId(destination)
    blockTable(destinationId)
    val e = newExpression(CHIRExprKind.Goto, Array(destinationId), debugLoc)
    getExpr[CHIR.Goto](appendExpression(ExpressionElem.Expression, e))
  }

  def newBranch(condition: CHIR.Value, trueBlock: CHIR.Block, falseBlock: CHIR.Block,
                debugLoc: Option[CHIR.DebugLocation] = None): CHIR.Branch = {
    val conditionId = valueId(condition)
    require(isBoolean(conditionId), "Branch condition must have Boolean type")
    val t = valueId(trueBlock); val f = valueId(falseBlock)
    blockTable(t); blockTable(f)
    val b = new BranchT
    b.setBase(newExpression(CHIRExprKind.Branch, Array(conditionId, t, f), debugLoc))
    getExpr[CHIR.Branch](appendExpression(ExpressionElem.Branch, b))
  }

  private[v13002] def replaceExpressions(blockId: Long, prefix: Seq[CHIR.Expression], term: CHIR.Terminator): Unit = {
    val table = blockTable(blockId)
    val prefixIds = prefix.map(expressionId)
    val lastId = expressionId(term)
    require(prefixIds.forall(id => !isTerminator(expressionBase(expressionTable(id).getValue))),
      "Non-terminator sequence contains a terminator")
    require(isTerminator(expressionBase(expressionTable(lastId).getValue)), "Expected a terminator")
    val replacement = (prefixIds :+ lastId).toArray
    require(replacement.distinct.length == replacement.length, "Duplicate expressions")
    replacement.foreach { id =>
      val e = expressionBase(expressionTable(id).getValue)
      require(!valuesOf(ValueElem.Block).exists(v =>
        (v.asBlock() ne table) && ids(v.asBlock().getExprs).contains(id)),
        "Expression is attached to another block")
      if (e.getResultLocalVar != 0)
        require(valueTable(e.getResultLocalVar).asLocalVar().getAssociatedExpr == id,
          "Result local points to another expression")
    }
    val previous = ids(table.getExprs)
    previous.filterNot(replacement.contains).foreach(id => expressionBase(expressionTable(id).getValue).setOwner(0))
    replacement.foreach(id => expressionBase(expressionTable(id).getValue).setOwner(blockId))
    table.setExprs(replacement)
    rememberOwners()
    rebuildPredecessors()
  }

  private[v13002] def replaceBlocks(groupId: Long, blocks: Seq[EditableCHIR.Block], entry: EditableCHIR.Block): Unit = {
    val table = groupTable(groupId)
    val replacement = checkedBlocks(groupId, blocks, entry)
    ids(table.getBlocks).filterNot(replacement.contains).foreach(blockTable(_).setParentGroup(0))
    replacement.foreach(blockTable(_).setParentGroup(groupId))
    table.setBlocks(replacement)
    table.setEntryBlock(valueId(entry))
    rememberOwners()
    rebuildPredecessors()
    refreshCounters()
  }

  private def checkedBlocks(groupId: Long, blocks: Seq[EditableCHIR.Block], entry: EditableCHIR.Block): Array[Long] = {
    val result = blocks.map(valueId).toArray
    require(result.distinct.length == result.length, "Duplicate blocks")
    require(result.contains(valueId(entry)), "Entry block must be in blocks")
    result.foreach { id =>
      val parent = blockTable(id).getParentGroup
      require(parent == 0 || parent == groupId || !ids(groupTable(parent).getBlocks).contains(id),
        "Block belongs to another group")
      require(!valuesOf(ValueElem.BlockGroup).exists(g =>
        (groupId == 0 || (g.asBlockGroup() ne groupTable(groupId))) && ids(g.asBlockGroup().getBlocks).contains(id)),
        "Another group still references the block")
    }
    result
  }

  def removeUnreachableBlocks(function: EditableCHIR.Func): Unit = {
    valueId(function)
    val seenGroups = mutable.Set.empty[Long]
    def visit(g: EditableCHIR.BlockGroup): Unit = {
      val gid = valueId(g)
      if (!seenGroups.add(gid)) return
      val members = g.blocks.map(valueId).toSet
      val reachable = mutable.Set.empty[Long]
      val todo = mutable.Stack(valueId(g.entryBlock))
      while (todo.nonEmpty) {
        val id = todo.pop()
        require(members(id), "Control-flow edge leaves the body group")
        if (reachable.add(id)) {
          val exprs = ids(blockTable(id).getExprs)
          if (exprs.nonEmpty) todo.pushAll(successorIds(expressionBase(expressionTable(exprs.last).getValue)))
        }
      }
      g.replaceBlocks(g.blocks.filter(b => reachable(valueId(b))), g.entryBlock)
      g.blocks.foreach { b =>
        ids(blockTable(valueId(b)).getExprs).foreach { id =>
          nestedGroups(expressionTable(id).getValue).foreach(gid => visit(group(gid)))
        }
      }
    }
    function.body.foreach(visit)
  }

  def validate(): Seq[EditableCHIR.ValidationIssue] = {
    val staged = mutable.ArrayBuffer.empty[EditableCHIR.ValidationIssue]
    (originalExpressionCount.toLong + 1L to length(data.getExprs).toLong).foreach { id =>
      if (expressionBase(expressionTable(id).getValue).getOwner == 0 && !historicalExpressionOwners.contains(id))
        staged += EditableCHIR.ValidationIssue("New expression has not been attached", Some(EditableCHIR.EntityRef(EditableCHIR.EntityTable.Expressions, id)))
    }
    (originalValueCount.toLong + 1L to length(data.getValues).toLong).foreach { id =>
      val entry = valueTable(id)
      if (entry.getType == ValueElem.Block && entry.asBlock().getParentGroup == 0 && !historicalBlockParents.contains(id))
        staged += EditableCHIR.ValidationIssue("New block has not been attached", Some(EditableCHIR.EntityRef(EditableCHIR.EntityTable.Values, id)))
      if (entry.getType == ValueElem.BlockGroup && entry.asBlockGroup().getOwnedFunc == 0 &&
        entry.asBlockGroup().getOwnedExpression == 0 && !historicalGroupOwners.contains(id))
        staged += EditableCHIR.ValidationIssue("New group has not been attached", Some(EditableCHIR.EntityRef(EditableCHIR.EntityTable.Values, id)))
    }
    staged.toSeq ++ EditableValidation.validate(this)
  }

  def write(output: Path): Unit = {
    val problems = validate()
    require(problems.isEmpty, problems.map(_.message).mkString("Invalid CHIR: ", "; ", ""))
    val builder = new FlatBufferBuilder(1024)
    // Keep table IDs stable, including retired entries. Native chir-dis still
    // dereferences the owner fields of those entries, even when body/block lists
    // no longer contain them. Restore historical links only in the wire copy.
    val wire = CHIRPackageT.deserializeFromBinary(data.serializeToBinary())
    historicalExpressionOwners.foreach { (id, owner) =>
      val e = expressionBase(wire.getExprs()((id - 1).toInt).getValue)
      if (e.getOwner == 0) e.setOwner(owner)
    }
    historicalBlockParents.foreach { (id, parent) =>
      val b = wire.getValues()((id - 1).toInt).asBlock()
      if (b.getParentGroup == 0) b.setParentGroup(parent)
    }
    historicalGroupOwners.foreach { (id, owners) =>
      val g = wire.getValues()((id - 1).toInt).asBlockGroup()
      if (g.getOwnedFunc == 0 && g.getOwnedExpression == 0) {
        g.setOwnedFunc(owners._1); g.setOwnedExpression(owners._2)
      }
    }
    val retiredBlocks = historicalBlockParents.keysIterator.filter(id => blockTable(id).getParentGroup == 0).toArray
    if (retiredBlocks.nonEmpty) {
      // Native deserialization also rebuilds group membership from parentGroup,
      // so putting retired blocks back under their old group resurrects them.
      // Keep their IDs/objects in a separate, unreferenced storage group instead.
      // Clear their instruction lists so they cannot contribute CFG edges to a
      // live block; retained expression/local payloads still have valid owners.
      val functions = wire.getValues.zipWithIndex.filter(_._1.getType == ValueElem.Function)
      val owner = functions.find(_._1.asFunction().getBody != 0).orElse(functions.headOption)
        .getOrElse(throw new IllegalArgumentException("Retired blocks have no function context"))
      val groupName = nextName(wire.getValues.filter(_.getType == ValueElem.BlockGroup)
        .map(_.asBlockGroup().getBase.getIdentifier).toSeq, "")
      val storage = new BlockGroupT
      storage.setBase(newValue(groupName.toString, ValueKind.BLOCK_GROUP))
      storage.setOwnedFunc(owner._2 + 1L)
      storage.setBlocks(retiredBlocks)
      storage.setEntryBlock(retiredBlocks.head)
      val entry = new ValueElemUnion; entry.setType(ValueElem.BlockGroup); entry.setValue(storage)
      wire.setValues(wire.getValues :+ entry)
      val storageId = wire.getValues.length.toLong
      retiredBlocks.foreach { id =>
        val b = wire.getValues()((id - 1).toInt).asBlock()
        b.setParentGroup(storageId)
        b.setExprs(Array.emptyLongArray)
        b.setPredecessors(Array.emptyLongArray)
      }
      val f = owner._1.asFunction()
      f.setBlockGroupId(math.max(f.getBlockGroupId, Math.addExact(groupName, 1L)))
    }
    val root = CHIRPackage.pack(builder, wire)
    if (hasIdentifier) CHIRPackage.finishCHIRPackageBuffer(builder, root) else builder.finish(root)
    val target = output.toAbsolutePath
    val temporary = Files.createTempFile(target.getParent, ".chir-", ".tmp")
    try {
      Files.write(temporary, builder.sizedByteArray())
      try Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      catch { case _: AtomicMoveNotSupportedException => Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING) }
    } finally Files.deleteIfExists(temporary)
  }

  private[v13002] def valueTable(id: Long): ValueElemUnion = {
    require(id > 0 && id <= length(data.getValues), s"Invalid value ID $id")
    data.getValues()((id - 1).toInt)
  }
  private[v13002] def expressionTable(id: Long): ExpressionElemUnion = {
    require(id > 0 && id <= length(data.getExprs), s"Invalid expression ID $id")
    data.getExprs()((id - 1).toInt)
  }
  private[v13002] def blockTable(id: Long): BlockT = {
    require(valueTable(id).getType == ValueElem.Block, s"Value $id is not a block")
    valueTable(id).asBlock()
  }
  private[v13002] def groupTable(id: Long): BlockGroupT = {
    require(valueTable(id).getType == ValueElem.BlockGroup, s"Value $id is not a group")
    valueTable(id).asBlockGroup()
  }
  private[v13002] def group(id: Long): EditableCHIR.BlockGroup = value(id).asInstanceOf[EditableCHIR.BlockGroup]
  private[v13002] def blockView(id: Long): EditableCHIR.Block = value(id).asInstanceOf[EditableCHIR.Block]
  private def local(id: Long): CHIR.LocalVar = value(id).asInstanceOf[CHIR.LocalVar]
  private[v13002] def successorIds(e: ExpressionT): Array[Long] =
    if (!isTerminator(e)) Array.emptyLongArray
    else ids(e.getOperands).filter(id => id > 0 && id <= length(data.getValues) && valueTable(id).getType == ValueElem.Block)

  private[v13002] def rebuildPredecessors(): Unit = {
    val incoming = mutable.LongMap.empty[mutable.LinkedHashSet[Long]]
    valuesOf(ValueElem.Block).foreach(_.asBlock().setPredecessors(Array.emptyLongArray))
    (1L to length(data.getValues).toLong).foreach { id =>
      val v = valueTable(id)
      if (v.getType == ValueElem.Block && v.asBlock().getParentGroup != 0 &&
        ids(groupTable(v.asBlock().getParentGroup).getBlocks).contains(id)) {
        val exprs = ids(v.asBlock().getExprs)
        if (exprs.nonEmpty) successorIds(expressionBase(expressionTable(exprs.last).getValue)).foreach { successor =>
          incoming.getOrElseUpdate(successor, mutable.LinkedHashSet.empty) += id
        }
      }
    }
    incoming.foreach((id, predecessors) => blockTable(id).setPredecessors(predecessors.toArray))
  }

  private def rememberOwners(): Unit = {
    (1L to length(data.getExprs).toLong).foreach { id =>
      val owner = expressionBase(expressionTable(id).getValue).getOwner
      if (owner != 0) historicalExpressionOwners(id) = owner
    }
    (1L to length(data.getValues).toLong).foreach { id =>
      val v = valueTable(id)
      if (v.getType == ValueElem.Block && v.asBlock().getParentGroup != 0)
        historicalBlockParents(id) = v.asBlock().getParentGroup
      if (v.getType == ValueElem.BlockGroup) {
        val g = v.asBlockGroup()
        if (g.getOwnedFunc != 0 || g.getOwnedExpression != 0)
          historicalGroupOwners(id) = (g.getOwnedFunc, g.getOwnedExpression)
      }
    }
  }

  private def refreshCounters(): Unit = valuesOf(ValueElem.Function).foreach { entry =>
    val f = entry.asFunction()
    val visited = mutable.Set.empty[Long]
    def visit(id: Long): Unit = if (id != 0 && visited.add(id)) {
      val g = groupTable(id)
      f.setBlockGroupId(math.max(f.getBlockGroupId, nextName(Seq(g.getBase.getIdentifier), "")))
      ids(g.getBlocks).foreach { bid =>
        val b = blockTable(bid)
        f.setBlockId(math.max(f.getBlockId, nextName(Seq(b.getBase.getIdentifier), "#")))
        ids(b.getExprs).foreach { eid => nestedGroups(expressionTable(eid).getValue).foreach(visit) }
      }
    }
    visit(f.getBody)
  }

  private def isBoolean(valueId: Long): Boolean = {
    val id = valueBase(valueTable(valueId).getValue).getType
    id > 0 && id <= length(data.getTypes) && typeBase(data.getTypes()((id - 1).toInt).getValue).getKind == CHIRTypeKind.BOOLEAN
  }
  private def appendValue(tag: Byte, payload: Object): Long = {
    val entry = new ValueElemUnion; entry.setType(tag); entry.setValue(payload)
    data.setValues(Option(data.getValues).getOrElse(Array.empty[ValueElemUnion]) :+ entry)
    length(data.getValues).toLong
  }
  private def appendExpression(tag: Byte, payload: Object): Long = {
    val entry = new ExpressionElemUnion; entry.setType(tag); entry.setValue(payload)
    data.setExprs(Option(data.getExprs).getOrElse(Array.empty[ExpressionElemUnion]) :+ entry)
    length(data.getExprs).toLong
  }
  private def valuesOf(tag: Byte): Seq[ValueElemUnion] =
    Option(data.getValues).toSeq.flatMap(_.toSeq).filter(_.getType == tag)
  private def normalized(name: String): String = if (name.startsWith("@")) name else "@" + name
  private def registered[T](registry: IdentityHashMap[T, java.lang.Long], node: T, kind: String): Long = {
    require(node != null && registry.containsKey(node), s"Foreign or unregistered $kind")
    registry.get(node).longValue()
  }
}
