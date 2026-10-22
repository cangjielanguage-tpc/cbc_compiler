package com.huawei.excelsior.jet.compiler.chir.v1203

import com.google.flatbuffers.FlatBufferBuilder
import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*

import java.nio.ByteBuffer
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.IdentityHashMap
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.control.NonFatal

trait CHIRItemProvider {
  def getType[T >: Null <: CHIR.Type: ClassTag](id: Long): Option[T]
  def getValue[T >: Null <: CHIR.Value: ClassTag](id: Long): Option[T]
  def getExpr[T >: Null <: CHIR.Expression: ClassTag](id: Long): T
  def getDef[T >: Null <: CHIR.CustomTypeDef: ClassTag](id: Long): Option[T]
}

object PackageImpl {
  def read(input: Path): PackageImpl = fromBytes(Files.readAllBytes(input))
  def fromBytes(bytes: Array[Byte]): PackageImpl = new PackageImpl(load(bytes))

  private def load(bytes: Array[Byte]): (CHIRPackageT, Boolean) = {
    val buffer = ByteBuffer.wrap(bytes)
    (CHIRPackage.getRootAsCHIRPackage(buffer).unpack(), CHIRPackage.CHIRPackageBufferHasIdentifier(buffer))
  }

  private[v1203] enum EntityTable {
    case Types, Values, Expressions, Definitions
  }
  private[v1203] final case class EntityRef(table: EntityTable, id: Long)
  private[v1203] final case class ValidationIssue(message: String, entity: Option[EntityRef] = None)
  private def length[A](xs: Array[A]): Int = if (xs == null) 0 else xs.length
  private[v1203] def ids(xs: Array[Long]): Array[Long] = if (xs == null) Array.emptyLongArray else xs

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
    case e: LambdaT => e.getBase
    case e: SpawnBaseT => e.getBase
    case _ => throw new IllegalArgumentException("Unsupported expression table")
  }

  private def isTerminator(e: ExpressionT): Boolean = e.getKind >= CHIRExprKind.Goto && e.getKind <= CHIRExprKind.TryRShift

  private def nestedGroups(x: Object): Array[Long] = {
    val extra = x match {
      case e: LambdaT if e.getBody != 0 => Array(e.getBody)
      case _ => Array.emptyLongArray
    }
    (ids(expressionBase(x).getBlockGroups) ++ extra).distinct
  }

  private def nextName(names: Iterable[String], prefix: String): Long = names.foldLeft(0L) { (next, name) =>
    if (name == null || !name.startsWith(prefix)) next
    else name.substring(prefix.length).toLongOption match {
      case Some(n) if n >= 0 => math.max(next, Math.addExact(n, 1L))
      case _ => next
    }
  }

  private[v1203] def newValue(identifier: String, kind: Int): ValueT = {
    val value = new ValueT
    value.setBase(newBase()); value.setIdentifier(identifier); value.setKind(kind)
    value
  }

  private[v1203] def newBase(): BaseT = {
    // The native deserializer expects these objects/vectors even for nodes
    // without a source location. Optional fields in the schema are not always
    // optional in that implementation.
    val loc = new DebugLocationT
    loc.setFilePath(""); loc.setBeginPos(new PosT); loc.setEndPos(new PosT)
    loc.setScope(Array.emptyIntArray)
    val b = new BaseT; b.setLoc(loc); b.setAnnos(Array.empty[AnnotationUnion])
    b
  }

  private def newExpression(kind: Int, operands: Array[Long], debug: Option[CHIR.DebugLocation]): ExpressionT = {
    val e = new ExpressionT; e.setKind(kind); e.setOperands(operands)
    val base = newBase()
    debug.foreach { loc =>
      require(loc.startLine >= 0 && loc.endLine >= loc.startLine && loc.endLine <= 0xffffffffL,
        "Invalid debug line range")
      val first = new PosT; first.setLine(loc.startLine)
      val last = new PosT; last.setLine(loc.endLine)
      val position = base.getLoc; position.setBeginPos(first); position.setEndPos(last)
      base.setLoc(position)
    }
    e.setBase(base)
    e
  }

  private[v1203] def location(e: ExpressionT): Option[CHIR.DebugLocation] = for {
    base <- Option(e.getBase)
    loc <- Option(base.getLoc)
    first <- Option(loc.getBeginPos)
    last <- Option(loc.getEndPos)
    if first.getLine > 0 && last.getLine > 0
  } yield CHIR.DebugLocation(first.getLine, last.getLine)

  private def typeBase(x: Object): TypeT = x match {
    case t: TypeT => t
    case t: RawArrayTypeT => t.getBase
    case t: VArrayTypeT => t.getBase
    case t: FuncTypeT => t.getBase
    case t: CustomTypeT => t.getBase
    case t: GenericTypeT => t.getBase
    case _ => throw new IllegalArgumentException("Unsupported type payload")
  }

  private def valueBase(x: Object): ValueT = x match {
    case t: FunctionT => t.getBase.getBase
    case t: GlobalVarT => t.getBase.getBase
    case t: ParameterT => t.getBase
    case t: LocalVarT => t.getBase
    case t: BlockT => t.getBase
    case t: BlockGroupT => t.getBase
    case t: BoolLiteralT => t.getBase.getBase
    case t: RuneLiteralT => t.getBase.getBase
    case t: StringLiteralT => t.getBase.getBase
    case t: IntLiteralT => t.getBase.getBase
    case t: FloatLiteralT => t.getBase.getBase
    case t: UnitLiteralT => t.getBase.getBase
    case t: NullLiteralT => t.getBase.getBase
    case _ => throw new IllegalArgumentException("Unsupported value payload")
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
      case e: InvokeBaseT =>
        call(e.getBase)
        Option(e.getVirMethodCtx).foreach { sig =>
          changes.set(remap(sig.getFuncType))(sig.setFuncType)
          changes.set(ids(sig.getGenericTypeParams).map(remap))(sig.setGenericTypeParams)
        }
      case e: IntrinsicBaseT => call(e.getBase)
      case e: InstanceOfT => changes.set(remap(e.getTargetType))(e.setTargetType)
      case e: RawArrayAllocateBaseT => changes.set(remap(e.getElementType))(e.setElementType)
      case e: GetInstantiateValueT => changes.set(ids(e.getInstantiateTypes).map(remap))(e.setInstantiateTypes)
      case e: GetRTTIStaticT => changes.set(remap(e.getRttiType))(e.setRttiType)
      case _ => ()
    }
  }

}

/** Package-owned views share one mutable generated object model for all reads
  * and edits. No original serialized buffer or secondary reader is retained.
  * Like the previous implementation, this class is not thread-safe.
  */
final class PackageImpl private (loaded: (CHIRPackageT, Boolean))
  extends CHIR.Package with CHIRItemProvider {
  def this(source: String) = this(PackageImpl.load(Files.readAllBytes(java.nio.file.Paths.get(source))))

  private[v1203] val data: CHIRPackageT = loaded._1
  private val hasIdentifier = loaded._2
  private given CHIRItemProvider = this
  import PackageImpl.*

  private val valueViews = mutable.LongMap.empty[CHIR.Value]
  private val expressionViews = mutable.LongMap.empty[CHIR.Expression]
  private val typeViews = mutable.LongMap.empty[CHIR.Type]
  private val definitionViews = mutable.LongMap.empty[CHIR.CustomTypeDef]
  private val valueIds = new IdentityHashMap[CHIR.Value, java.lang.Long]()
  private val expressionIds = new IdentityHashMap[CHIR.Expression, java.lang.Long]()
  private val typeIds = new IdentityHashMap[CHIR.Type, java.lang.Long]()
  private val definitionIds = new IdentityHashMap[CHIR.CustomTypeDef, java.lang.Long]()
  private val originalExpressionCount = length(data.getExprs)
  private val originalValueCount = length(data.getValues)
  private val historicalExpressionOwners = mutable.LongMap.empty[Long]
  private val historicalBlockParents = mutable.LongMap.empty[Long]
  private val historicalGroupOwners = mutable.LongMap.empty[(Long, Long)]
  private var nextBlockName = nextName(valuesOf(ValueElem.Block).map(_.asBlock().getBase.getIdentifier), "#")
  private var nextGroupName = nextName(valuesOf(ValueElem.BlockGroup).map(_.asBlockGroup().getBase.getIdentifier), "")
  rememberOwners()

  def name: String = data.getName
  def values: Iterator[CHIR.Value] = (1L to length(data.getValues).toLong).iterator.map(value)

  def typeDefs: Iterator[CHIR.CustomTypeDef] =
    (1L to length(data.getDefs).toLong).iterator.map(id => getDef[CHIR.CustomTypeDef](id).get)

  def function(idx: Int): CHIR.Func = getValue[CHIR.Func](idx).getOrElse(
    throw new IllegalArgumentException(s"Value $idx is not a function"))

  def packageInitFunc: CHIR.Func = function(Math.toIntExact(data.getPackageInitFunc))
  def packageInitLiteralFunc: CHIR.Func = function(Math.toIntExact(data.getPackageLiteralInitFunc))

  def getFunc(identifier: String): Option[CHIR.Func] = {
    val name = normalized(identifier)
    values.collectFirst { case f: CHIR.Func if f.identifier == name => f }
  }

  def getDef(identifier: String): Option[CHIR.CustomTypeDef] = {
    val name = normalized(identifier)
    typeDefs.find(_.identifier == name)
  }

  def valueId(value: CHIR.Value): Long = registered(valueIds, value, "value")
  def expressionId(expression: CHIR.Expression): Long = registered(expressionIds, expression, "expression")

  def definitionId(definition: CHIR.CustomTypeDef): Long = registered(definitionIds, definition, "definition")

  def getType[T >: Null <: CHIR.Type: ClassTag](id: Long): Option[T] = {
    if (id <= 0) return None
    val result = Some(typeViews.getOrElseUpdate(id,
      createType(data.getTypes()((id - 1).toInt).getValue))).collect { case t: T => t }
    result.foreach(t => if (!typeIds.containsKey(t)) typeIds.put(t, Long.box(id)))
    result
  }

  def getDef[T >: Null <: CHIR.CustomTypeDef: ClassTag](id: Long): Option[T] = {
    if (id <= 0) return None
    val result = Some(definitionViews.getOrElseUpdate(id,
      createDefinition(data.getDefs()((id - 1).toInt).getValue))).collect { case d: T => d }
    result.foreach(d => definitionIds.put(d, Long.box(id)))
    result
  }

  def getValue[T >: Null <: CHIR.Value: ClassTag](id: Long): Option[T] =
    if (id <= 0) None else Some(value(id)).collect { case t: T => t }

  private def value(id: Long): CHIR.Value = valueViews.getOrElseUpdate(id, {
    val entry = valueTable(id)
    val result = createValue(entry.getValue, id)
    valueIds.put(result, Long.box(id))
    result
  })

  def getExpr[T >: Null <: CHIR.Expression: ClassTag](id: Long): T = {
    if (id <= 0) return null
    expressionViews.getOrElseUpdate(id, {
      val e = expressionBase(expressionTable(id).getValue)
      val view = try createExpression(expressionTable(id).getValue)
      catch { case _: MatchError => opaque(e) }
      expressionIds.put(view, Long.box(id))
      view
    }).asInstanceOf[T]
  }

  private[v1203] def moveBody(source: FunctionT, destination: FunctionT): Unit = {
    def functionId(function: FunctionT): Long = {
      val tableIndex = data.getValues.indexWhere(v =>
        v.getType == ValueElem.Function && (v.asFunction() eq function))
      require(tableIndex >= 0, "Function is not registered in this package")
      tableIndex.toLong + 1
    }

    val sourceId = functionId(source)
    val targetId = functionId(destination)
    prepareBodies()
    val from = source
    val to = destination
    require(sourceId != targetId, "Source and destination must differ")
    require(from.getBody != 0, "Source has no body")
    val sourceParams = ids(from.getParams)
    val targetParams = ids(to.getParams)
    // Receivers are parameters too. Equal arities map positionally; a single
    // extra leading parameter can be introduced or, if unused, removed.
    val (mappedSourceParams, mappedTargetParams) = targetParams.length - sourceParams.length match {
      case 0 => (sourceParams, targetParams)
      case 1 => (sourceParams, targetParams.drop(1))
      case -1 => (sourceParams.drop(1), targetParams)
      case _ => throw new IllegalArgumentException("Parameter counts must match or differ by one leading parameter")
    }
    val removedParams = sourceParams.toSet -- mappedSourceParams.toSet
    val sourceType = data.getTypes()(index(from.getBase.getBase.getType)).asFuncType()
    val targetType = data.getTypes()(index(to.getBase.getBase.getType)).asFuncType()
    require(sourceType.getHasVarArg == targetType.getHasVarArg &&
      sourceType.getIsCfuncType == targetType.getIsCfuncType, "Incompatible calling conventions")
    def value(id: Long): ValueElemUnion = valueTable(id)

    require(from.getGenericDecl == 0 && to.getGenericDecl == 0,
      "Generic instances are unsupported")
    val sourceGenerics = ids(from.getGenericTypeParams)
    val targetGenerics = ids(to.getGenericTypeParams)
    require(sourceGenerics.length == targetGenerics.length,
      "Functions must have the same number of generic parameters")
    val remapType = new TypeRemapper(data, sourceGenerics.zip(targetGenerics).toMap)
    sourceGenerics.zip(targetGenerics).foreach { (a, b) =>
      require(data.getTypes()(index(a)).getType == TypeElem.GenericType &&
        data.getTypes()(index(b)).getType == TypeElem.GenericType, "Expected generic types")
      val sourceBounds = data.getTypes()(index(a)).asGenericType().getUpperBounds
      val targetBounds = data.getTypes()(index(b)).asGenericType().getUpperBounds
      require(ids(sourceBounds).map(remapType.apply).sameElements(ids(targetBounds)),
        "Generic upper bounds must match after substitution")
    }
    require(mappedSourceParams.zip(mappedTargetParams).forall { (a, b) =>
      remapType(value(a).asParameter().getBase.getType) == value(b).asParameter().getBase.getType
    }, "Corresponding parameter types must match after substitution")

    def returnType(f: FunctionT): Long =
      data.getTypes()(index(f.getBase.getBase.getType)).asFuncType().getBase.getArgTys.last

    require(remapType(returnType(from)) == returnType(to), "Return types must match after substitution")
    require(data.getPackageInitFunc != sourceId && data.getPackageLiteralInitFunc != sourceId,
      "Cannot remove a package initializer body")

    // 2. Build the source-parameter-ID -> target-parameter-ID mapping.
    val parameterMap = mappedSourceParams.zip(mappedTargetParams).toMap
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
        val payload = expressionTable(id).getValue
        require(!payload.isInstanceOf[LambdaT] && !payload.isInstanceOf[SpawnBaseT],
          s"Unsupported expression: ${Option(payload).map(_.getClass.getSimpleName)}")
        val e = expressionBase(payload)
        require(e.getOwner == blockId, "Invalid expression owner")
        require(ids(e.getBlockGroups).isEmpty, "Nested block groups are unsupported")
        require(!ids(e.getOperands).exists(removedParams.contains),
          "Cannot remove a leading parameter that is used by the body")
        require(!ids(e.getOperands).contains(sourceId),
          "Source self references require explicit handling")
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
        remapExpressionTypes(data.getExprs()(index(id)).getValue, remapType.apply)
      }
    }
    changes.set(remapType(group.getBase.getType))(group.getBase.setType)
    remapAnnotations(group.getBase.getBase, remapType.apply)
    // Printed local identifiers are function-scoped too. Start after the new
    // parameters, without changing global values-table IDs or associatedExpr IDs.
    val localIds = (expressions.map(_.getResultLocalVar) :+ from.getRetVal)
      .filter(_ != 0).distinct
      .sortBy(id => value(id).asLocalVar().getBase.getIdentifier.stripPrefix("%").toLong)
    var nextLocal = ids(to.getParams).length.toLong
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
    rememberOwners()
    rebuildPredecessors()
    refreshCounters()
    requireValid()
  }

  private def opaque(e: ExpressionT): CHIR.Expression =
    if (isTerminator(e)) new CHIR.Terminator with CHIR.HasSuccessors {
      def successors: Seq[CHIR.Block] = successorIds(e).map(blockView).toSeq
      def debugLoc: Option[CHIR.DebugLocation] = location(e)
    } else new CHIR.Expression {
      def debugLoc: Option[CHIR.DebugLocation] = location(e)
    }

  def newGoto(destination: CHIR.Block, debugLoc: Option[CHIR.DebugLocation] = None): CHIR.Goto = {
    val destinationId = valueId(destination)
    blockTable(destinationId)
    val e = newExpression(CHIRExprKind.Goto, Array(destinationId), debugLoc)
    getExpr[CHIR.Goto](appendExpression(ExpressionElem.Expression, e))
  }

  private[v1203] def replaceExpressions(blockId: Long, prefix: Seq[CHIR.Expression], term: CHIR.Terminator): Unit = {
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
    enclosingFunction(table.getParentGroup).foreach(pruneUnreachableBlocks)
  }

  private[v1203] def replaceBlocks(groupId: Long, blocks: Seq[CHIR.Block], entry: CHIR.Block): Unit = {
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

  private def checkedBlocks(groupId: Long, blocks: Seq[CHIR.Block], entry: CHIR.Block): Array[Long] = {
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

  private def pruneUnreachableBlocks(function: CHIR.Func): Unit = {
    valueId(function)
    val seenGroups = mutable.Set.empty[Long]

    def visit(g: CHIR.BlockGroup): Unit = {
      val gid = valueId(g)
      if (!seenGroups.add(gid)) return
      val members = g.blocks.map(valueId).toSet
      val reachable = mutable.Set.empty[Long]
      val todo = mutable.Stack(valueId(g.entryBlock))
      var closed = true
      while (todo.nonEmpty && closed) {
        val id = todo.pop()
        if (!members(id)) closed = false
        else if (reachable.add(id)) {
          val exprs = ids(blockTable(id).getExprs)
          if (exprs.isEmpty) closed = false
          else {
            val last = expressionBase(expressionTable(exprs.last).getValue)
            if (!isTerminator(last)) closed = false
            else todo.pushAll(successorIds(last))
          }
        }
      }
      // An edge to a block not yet added to the group is a staged edit.
      // Do not retire nodes until all reachable blocks form a complete CFG.
      if (closed) g.replaceBlocks(g.blocks.filter(b => reachable(valueId(b))), g.entryBlock)
      g.blocks.foreach { b =>
        ids(blockTable(valueId(b)).getExprs).foreach { id =>
          nestedGroups(expressionTable(id).getValue).foreach(gid => visit(group(gid)))
        }
      }
    }

    function.body.foreach(visit)
  }

  private[v1203] def validationIssues(): Seq[ValidationIssue] = {
    val staged = mutable.ArrayBuffer.empty[ValidationIssue]
    (originalExpressionCount.toLong + 1L to length(data.getExprs).toLong).foreach { id =>
      if (expressionBase(expressionTable(id).getValue).getOwner == 0 && !historicalExpressionOwners.contains(id))
        staged += ValidationIssue("New expression has not been attached", Some(EntityRef(EntityTable.Expressions, id)))
    }
    (originalValueCount.toLong + 1L to length(data.getValues).toLong).foreach { id =>
      val entry = valueTable(id)
      if (entry.getType == ValueElem.Block && entry.asBlock().getParentGroup == 0 && !historicalBlockParents.contains(id))
        staged += ValidationIssue("New block has not been attached", Some(EntityRef(EntityTable.Values, id)))
      if (entry.getType == ValueElem.BlockGroup && entry.asBlockGroup().getOwnedFunc == 0 &&
        entry.asBlockGroup().getOwnedExpression == 0 && !historicalGroupOwners.contains(id))
        staged += ValidationIssue("New group has not been attached", Some(EntityRef(EntityTable.Values, id)))
    }
    staged.toSeq ++ validateModel()
  }

  private def requireValid(): Unit = {
    val problems = validationIssues()
    require(problems.isEmpty, problems.map(_.message).mkString("Invalid CHIR: ", "; ", ""))
  }

  private def prepareBodies(): Unit = {
    // Validate before traversing raw IDs, then check the normalized graph too.
    // Staged/invalid edits cannot silently pass through a move or a write.
    requireValid()
    values.collect { case f: CHIR.Func if f.body.nonEmpty => f }.toVector
      .foreach(pruneUnreachableBlocks)
    requireValid()
  }

  private def enclosingFunction(groupId: Long): Option[CHIR.Func] = {
    val visited = mutable.Set.empty[Long]

    def find(id: Long): Option[CHIR.Func] = {
      if (id == 0 || !visited.add(id)) None
      else {
        val g = groupTable(id)
        if (g.getOwnedFunc != 0) {
          val f = function(Math.toIntExact(g.getOwnedFunc))
          Option.when(f.body.exists(valueId(_) == id))(f)
        } else if (g.getOwnedExpression != 0) {
          val e = expressionBase(expressionTable(g.getOwnedExpression).getValue)
          if (e.getOwner == 0) None else find(blockTable(e.getOwner).getParentGroup)
        } else None
      }
    }

    find(groupId)
  }

  def write(output: Path): Unit = {
    prepareBodies()
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

  private[v1203] def valueTable(id: Long): ValueElemUnion = {
    require(id > 0 && id <= length(data.getValues), s"Invalid value ID $id")
    data.getValues()((id - 1).toInt)
  }

  private[v1203] def expressionTable(id: Long): ExpressionElemUnion = {
    require(id > 0 && id <= length(data.getExprs), s"Invalid expression ID $id")
    data.getExprs()((id - 1).toInt)
  }

  private[v1203] def blockTable(id: Long): BlockT = {
    require(valueTable(id).getType == ValueElem.Block, s"Value $id is not a block")
    valueTable(id).asBlock()
  }

  private[v1203] def groupTable(id: Long): BlockGroupT = {
    require(valueTable(id).getType == ValueElem.BlockGroup, s"Value $id is not a group")
    valueTable(id).asBlockGroup()
  }

  private[v1203] def group(id: Long): CHIR.BlockGroup = value(id).asInstanceOf[CHIR.BlockGroup]
  private[v1203] def blockView(id: Long): CHIR.Block = value(id).asInstanceOf[CHIR.Block]

  private def localTable(id: Long): LocalVarT = {
    val entry = valueTable(id)
    require(entry.getType == ValueElem.LocalVar, s"Value $id is not a local variable")
    entry.asLocalVar()
  }

  private[v1203] def successorIds(e: ExpressionT): Array[Long] =
    if (!isTerminator(e)) Array.emptyLongArray
    else ids(e.getOperands).filter(id => id > 0 && id <= length(data.getValues) && valueTable(id).getType == ValueElem.Block)

  private[v1203] def rebuildPredecessors(): Unit = {
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

  private def hasBlockOperand(operands: Array[Long]): Boolean =
    ids(operands).exists(getValue[CHIR.Block](_).nonEmpty)

  private def createType(node: Object): CHIR.Type = node match {
    case t: GenericTypeT => GenericTypeImpl(t)
    case t: FuncTypeT => FuncTypeImpl(t)
    case t: RawArrayTypeT => RawArrayTypeImpl(t)
    case t: VArrayTypeT => VArrayTypeImpl(t)
    case t: CustomTypeT => t.getBase.getKind match {
      case CHIRTypeKind.CLASS => ClassTypeImpl(t)
      case CHIRTypeKind.STRUCT => StructTypeImpl(t)
      case CHIRTypeKind.ENUM => EnumTypeImpl(t)
    }
    case t: TypeT => t.getKind match {
      case CHIRTypeKind.INT8 => CHIR.BuiltinType.Int8
      case CHIRTypeKind.INT16 => CHIR.BuiltinType.Int16
      case CHIRTypeKind.INT32 => CHIR.BuiltinType.Int32
      case CHIRTypeKind.INT64 => CHIR.BuiltinType.Int64
      case CHIRTypeKind.INT_NATIVE => CHIR.BuiltinType.IntNative
      case CHIRTypeKind.UINT8 => CHIR.BuiltinType.UInt8
      case CHIRTypeKind.UINT16 => CHIR.BuiltinType.UInt16
      case CHIRTypeKind.UINT32 => CHIR.BuiltinType.UInt32
      case CHIRTypeKind.UINT64 => CHIR.BuiltinType.UInt64
      case CHIRTypeKind.UINT_NATIVE => CHIR.BuiltinType.UIntNative
      case CHIRTypeKind.FLOAT16 => CHIR.BuiltinType.Float16
      case CHIRTypeKind.FLOAT32 => CHIR.BuiltinType.Float32
      case CHIRTypeKind.FLOAT64 => CHIR.BuiltinType.Float64
      case CHIRTypeKind.RUNE => CHIR.BuiltinType.Rune
      case CHIRTypeKind.BOOLEAN => CHIR.BuiltinType.Boolean
      case CHIRTypeKind.UNIT => CHIR.BuiltinType.Unit
      case CHIRTypeKind.NOTHING => CHIR.BuiltinType.Nothing
      case CHIRTypeKind.VOID => CHIR.BuiltinType.Void
      case CHIRTypeKind.C_POINTER => new CPointerTypeImpl(t)
      case CHIRTypeKind.C_STRING => CHIR.BuiltinType.CString
      case CHIRTypeKind.REFTYPE => new RefTypeImpl(t)
      case CHIRTypeKind.BOXTYPE => new BoxTypeImpl(t)
      case CHIRTypeKind.TUPLE => new TupleTypeImpl(t)
      case CHIRTypeKind.THIS => CHIR.BuiltinType.This
    }
  }

  private def createValue(node: Object, id: Long): CHIR.Value = node match {
    case v: BoolLiteralT => BoolLiteralImpl(v)
    case v: RuneLiteralT => RuneLiteralImpl(v)
    case v: StringLiteralT => StringLiteralImpl(v)
    case v: IntLiteralT => IntLiteralImpl(v)
    case v: FloatLiteralT => FloatLiteralImpl(v)
    case v: UnitLiteralT => UnitLiteralImpl(v)
    case v: NullLiteralT => NullLiteralImpl(v)
    case v: ParameterT => ParameterImpl(v)
    case v: LocalVarT => LocalVarImpl(v)
    case v: GlobalVarT => GlobalVarImpl(v, id)
    case v: FunctionT => FuncImpl(v, id)
    case v: BlockT => new BlockImpl(this, id)
    case v: BlockGroupT => new BlockGroupImpl(this, id)
  }

  private def createExpression(node: Object): CHIR.Expression = node match {
    case e: AllocateBaseT if hasBlockOperand(e.getBase.getOperands) => new TryAllocateImpl(e)
    case e: AllocateBaseT => new AllocateImpl(e)
    case e: ApplyBaseT if hasBlockOperand(e.getBase.getBase.getOperands) => new TryApplyImpl(e)
    case e: ApplyBaseT => new ApplyImpl(e)
    case e: BinaryExpressionBaseT if hasBlockOperand(e.getBase.getOperands) => new TryBinaryImpl(e)
    case e: BinaryExpressionBaseT => new BinaryImpl(e)
    case e: BranchT => new BranchImpl(e)
    case e: DebugT => new DebugImpl(e)
    case e: FieldT => new FieldImpl(e)
    case e: GetElementRefT => new GetElementRefImpl(e)
    case e: GetRTTIStaticT => new GetRTTIStaticImpl(e)
    case e: InstanceOfT => new InstanceOfImpl(e)
    case e: IntrinsicBaseT if hasBlockOperand(e.getBase.getBase.getOperands) => new TryIntrinsicImpl(e)
    case e: IntrinsicBaseT => new IntrinsicImpl(e)
    case e: InvokeBaseT if hasBlockOperand(e.getBase.getBase.getOperands) => new TryInvokeImpl(e)
    case e: InvokeBaseT => new InvokeImpl(e)
    case e: MultiBranchT => new MultiBranchImpl(e)
    case e: NumericCastBaseT if hasBlockOperand(e.getBase.getOperands) => new TryNumericCastImpl(e)
    case e: NumericCastBaseT => new NumericCastImpl(e)
    case e: RawArrayAllocateBaseT if hasBlockOperand(e.getBase.getOperands) => new TryRawArrayAllocateImpl(e)
    case e: RawArrayAllocateBaseT => new RawArrayAllocateImpl(e)
    case e: SpawnBaseT if hasBlockOperand(e.getBase.getOperands) => new TrySpawnImpl(e)
    case e: SpawnBaseT => new SpawnImpl(e)
    case e: StoreElementRefT => new StoreElementRefImpl(e)
    case e: UnaryExpressionBaseT if hasBlockOperand(e.getBase.getOperands) => new TryUnaryImpl(e)
    case e: UnaryExpressionBaseT => new UnaryImpl(e)
    case e: ExpressionT => e.getKind match {
      case CHIRExprKind.Goto => new GotoImpl(e)
      case CHIRExprKind.Exit => new ExitImpl(e)
      case CHIRExprKind.RaiseException => new RaiseExceptionImpl(e)
      case CHIRExprKind.StaticCast => new StaticCastImpl(e)
      case CHIRExprKind.Box => new BoxImpl(e)
      case CHIRExprKind.UnboxToRef => new UnboxToRefImpl(e)
      case CHIRExprKind.UnboxToValue => new UnboxToValueImpl(e)
      case CHIRExprKind.CastToConcrete => new CastToConcreteImpl(e)
      case CHIRExprKind.CastToGeneric => new CastToGenericImpl(e)
      case CHIRExprKind.Load => new LoadImpl(e)
      case CHIRExprKind.Store => new StoreImpl(e)
      case CHIRExprKind.VArrayExpr => new VArrayImpl(e)
      case CHIRExprKind.VArrayBuilder => new VArrayBuilderImpl(e)
      case CHIRExprKind.RawArrayLiteralInit => new RawArrayLiteralInitImpl(e)
      case CHIRExprKind.RawArrayInitByValue => new RawArrayInitByValueImpl(e)
      case CHIRExprKind.Constant => new ConstantImpl(e)
      case CHIRExprKind.Tuple => new TupleImpl(e)
      case CHIRExprKind.GetException => new GetExceptionImpl(e)
      case CHIRExprKind.GetRtti => new GetRTTIImpl(e)
    }
  }

  private def createDefinition(node: Object): CHIR.CustomTypeDef = node match {
    case t: EnumDefT => EnumDefImpl(t)
    case t: ClassDefT => ClassDefImpl(t)
    case t: StructDefT => StructDefImpl(t)
    case t: ExtendDefT => ExtendDefImpl(t)
  }

  import EntityTable.*

  private def validateModel(): Seq[ValidationIssue] = {
    val pkg = this
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

  private def isSchema(obj: Object): Boolean = obj.getClass.getPackage.getName.endsWith("v1203.PackageFormat")
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

  // Shared implementation of Func.replaceBody and Func.removeBody.
  private[v1203] def replaceFunctionBody(
    functionId: Long,
    body: Option[CHIR.BlockGroup],
    retVal: Option[CHIR.LocalVar]
  ): Unit = {
    val bodyId = body.map(valueId).getOrElse(0L)
    val retValId = retVal.map(valueId).getOrElse(0L)
    val entry = valueTable(functionId)
    require(entry.getType == ValueElem.Function, s"Value $functionId is not a function")
    val target = entry.asFunction()
    require(bodyId != 0 || retValId == 0, "A declaration cannot have a return local")
    val oldBody = Option.when(target.getBody != 0)(groupTable(target.getBody))
    oldBody.foreach { g =>
      require(g.getOwnedFunc == functionId && g.getOwnedExpression == 0,
        "Existing body has inconsistent ownership")
    }
    val oldRetVal = Option.when(target.getRetVal != 0)(localTable(target.getRetVal))
    val replacement = Option.when(bodyId != 0)(groupTable(bodyId))
    val newRetVal = Option.when(retValId != 0)(localTable(retValId))

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
        val current = groupTable(groupId)
        if (enclosingExpression != 0) {
          require(current.getOwnedExpression == enclosingExpression && current.getOwnedFunc == 0,
            "Nested block group has inconsistent ownership")
        }
        val blockIds = ids(current.getBlocks)
        require(blockIds.distinct.length == blockIds.length, "Duplicate blocks in body group")
        require(blockIds.contains(current.getEntryBlock), "Entry block is not in the body group")
        blockIds.foreach { blockId =>
          require(blocks.add(blockId), "Block is shared between body groups")
          val entry = valueTable(blockId)
          require(entry.getType == ValueElem.Block, "Body contains a non-block value")
          val block = entry.asBlock()
          require(block.getParentGroup == groupId, "Block has inconsistent parent group")
          ids(block.getExprs).foreach { expressionId =>
            require(expressions.add(expressionId), "Expression belongs to multiple blocks")
            val payload = expressionTable(expressionId).getValue
            val e = expressionBase(payload)
            require(e.getOwner == blockId, "Expression has inconsistent block owner")
            if (e.getResultLocalVar != 0) {
              val result = localTable(e.getResultLocalVar)
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
        ids(expressionBase(expressionTable(expressionId).getValue).getOperands).foreach { operand =>
          val entry = valueTable(operand)
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
      resultLocals.toSeq.map(id => localTable(id).getBase.getIdentifier) ++
        ids(target.getParams).toSeq.map(id => valueTable(id).asParameter().getBase.getIdentifier), "%")
    val blockCounter = nextCounter(target.getBlockId,
      blocks.toSeq.map(id => valueTable(id).asBlock().getBase.getIdentifier), "#")
    val groupCounter = nextCounter(target.getBlockGroupId,
      groups.toSeq.map(id => groupTable(id).getBase.getIdentifier), "")

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
    rememberOwners()
  }

  // APIs currently called only by tests in this repository (excluding forwarders).
  def typeId(tpe: CHIR.Type): Long = {
    // Primitive type objects are shared singletons. Use the first equivalent
    // table entry as their canonical reference, without rewriting loaded IDs.
    if (!typeIds.containsKey(tpe)) (1L to length(data.getTypes).toLong).foreach(getType[CHIR.Type])
    registered(typeIds, tpe, "type")
  }

  def newBlock(isLandingPad: Boolean = false): CHIR.Block = {
    val b = new BlockT
    b.setBase(newValue(s"#$nextBlockName", ValueKind.BLOCK))
    b.setIsLandingPadBlock(isLandingPad)
    nextBlockName = Math.addExact(nextBlockName, 1L)
    blockView(appendValue(ValueElem.Block, b))
  }

  def newBlockGroup(blocks: Seq[CHIR.Block], entryBlock: CHIR.Block): CHIR.BlockGroup = {
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

  def newBranch(
    condition: CHIR.Value,
    trueBlock: CHIR.Block,
    falseBlock: CHIR.Block,
    debugLoc: Option[CHIR.DebugLocation] = None
  ): CHIR.Branch = {
    val conditionId = valueId(condition)
    require(isBoolean(conditionId), "Branch condition must have Boolean type")
    val t = valueId(trueBlock); val f = valueId(falseBlock)
    blockTable(t); blockTable(f)
    val b = new BranchT
    b.setBase(newExpression(CHIRExprKind.Branch, Array(conditionId, t, f), debugLoc))
    getExpr[CHIR.Branch](appendExpression(ExpressionElem.Branch, b))
  }


}
