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
    val groups = ids(expressionBase(x).getBlockGroups)
    val all = if (extra.isEmpty) groups else groups ++ extra
    if (all.length <= 1) all else all.distinct
  }

  private def nextName(names: Iterable[String], prefix: String, initial: Long = 0L): Long = names.foldLeft(initial) { (next, name) =>
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
    e.setBlockGroups(Array.emptyLongArray)
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

  /** Serialization-only edits are undone even if the generated packer fails. */
  private class WireChanges {
    private val undo = mutable.ArrayBuffer.empty[() => Unit]
    def set[A](previous: A, next: A)(setter: A => Unit): Unit = {
      undo += (() => setter(previous))
      setter(next)
    }
    def restore(): Unit = undo.reverseIterator.foreach(_())
  }

  /** Keep loaded IDs stable and accumulate appended entries without table copies. */
  private class AppendTable[A: ClassTag](initial: Array[A]) extends scala.collection.IndexedSeq[A] {
    private var entries = if (initial == null) Array.empty[A] else initial
    private val pending = mutable.ArrayBuffer.empty[A]
    def length: Int = entries.length + pending.length
    def apply(index: Int): A = {
      if (index < entries.length) entries(index) else pending(index - entries.length)
    }
    override def iterator: Iterator[A] = entries.iterator ++ pending.iterator
    def append(entry: A): Long = {
      pending += entry
      length.toLong
    }
    def array: Array[A] = {
      if (pending.nonEmpty) {
        entries = entries ++ pending
        pending.clear()
      }
      entries
    }
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
      require(id > 0 && id <= length(pkg.getTypes), s"Invalid type ID $id")
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

  private val model: CHIRPackageT = loaded._1
  private val valueEntries = new PackageImpl.AppendTable(model.getValues)
  private val expressionEntries = new PackageImpl.AppendTable(model.getExprs)
  private[v1203] def data: CHIRPackageT = {
    flushTables()
    model
  }
  private def flushTables(): Unit = {
    model.setValues(valueEntries.array)
    model.setExprs(expressionEntries.array)
  }
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
  private val originalExpressionCount = expressionEntries.length
  private val originalValueCount = valueEntries.length
  private val historicalExpressionOwners = mutable.LongMap.empty[Long]
  private val historicalBlockParents = mutable.LongMap.empty[Long]
  private val historicalGroupOwners = mutable.LongMap.empty[(Long, Long)]
  private val insertedTypeObjects = new IdentityHashMap[CHIR.Type, java.lang.Long]()
  private val insertedTypeIds = mutable.Map.empty[Product, Long]
  private var predecessorsDirty = false
  private var nextBlockName = nextName(valuesOf(ValueElem.Block).map(_.asBlock().getBase.getIdentifier), "#")
  private var nextGroupName = nextName(valuesOf(ValueElem.BlockGroup).map(_.asBlockGroup().getBase.getIdentifier), "")
  rememberOwners()

  def name: String = model.getName
  def values: Iterator[CHIR.Value] = (1L to valueEntries.length.toLong).iterator.map(value)

  def typeDefs: Iterator[CHIR.CustomTypeDef] =
    (1L to length(model.getDefs).toLong).iterator.map(id => getDef[CHIR.CustomTypeDef](id).get)

  def function(idx: Int): CHIR.Func = getValue[CHIR.Func](idx).getOrElse(
    throw new IllegalArgumentException(s"Value $idx is not a function"))

  def packageInitFunc: CHIR.Func = function(Math.toIntExact(model.getPackageInitFunc))
  def packageInitLiteralFunc: CHIR.Func = function(Math.toIntExact(model.getPackageLiteralInitFunc))

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
    var result = typeViews.getOrElse(id, null)
    if (result == null) {
      result = createType(model.getTypes()((id - 1).toInt).getValue)
      typeViews(id) = result
      if (!typeIds.containsKey(result)) typeIds.put(result, Long.box(id))
    }
    result match {
      case t: T => Some(t)
      case _ => None
    }
  }

  def getDef[T >: Null <: CHIR.CustomTypeDef: ClassTag](id: Long): Option[T] = {
    if (id <= 0) return None
    var result = definitionViews.getOrElse(id, null)
    if (result == null) {
      result = createDefinition(model.getDefs()((id - 1).toInt).getValue)
      definitionViews(id) = result
      definitionIds.put(result, Long.box(id))
    }
    result match {
      case d: T => Some(d)
      case _ => None
    }
  }

  def getValue[T >: Null <: CHIR.Value: ClassTag](id: Long): Option[T] = {
    if (id <= 0) None else value(id) match {
      case v: T => Some(v)
      case _ => None
    }
  }

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
      catch { case _: MatchError => opaque(expressionTable(id).getValue) }
      expressionIds.put(view, Long.box(id))
      view
    }).asInstanceOf[T]
  }

  private[v1203] def moveBody(sourceId: Long, source: FunctionT, targetId: Long, destination: FunctionT): Unit = {
    require(valueTable(sourceId).getValue eq source, "Source function is not registered at this ID")
    require(valueTable(targetId).getValue eq destination, "Destination function is not registered at this ID")
    val from = source
    val to = destination
    require(sourceId != targetId, "Source and destination must differ")
    require(from.getBody != 0, "Source has no body")
    val destinationReturnType = getValue[CHIR.Func](targetId).get.tpe.returnType
    val structConstructor = destinationReturnType.isInstanceOf[CHIR.StructType] &&
      (from.getFuncKind == FuncKind.STRUCT_CONSTRUCTOR || from.getFuncKind == FuncKind.PRIMAL_STRUCT_CONSTRUCTOR)
    val classConstructor = destinationReturnType == CHIR.BuiltinType.Unit &&
      (from.getFuncKind == FuncKind.CLASS_CONSTRUCTOR || from.getFuncKind == FuncKind.PRIMAL_CLASS_CONSTRUCTOR)
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
    val sourceType = model.getTypes()(index(from.getBase.getBase.getType)).asFuncType()
    val targetType = model.getTypes()(index(to.getBase.getBase.getType)).asFuncType()
    require(sourceType.getHasVarArg == targetType.getHasVarArg &&
      sourceType.getIsCfuncType == targetType.getIsCfuncType, "Incompatible calling conventions")
    def value(id: Long): ValueElemUnion = valueTable(id)

    require(from.getGenericDecl == 0 && to.getGenericDecl == 0,
      "Generic instances are unsupported")
    val sourceGenerics = ids(from.getGenericTypeParams)
    val targetGenerics = ids(to.getGenericTypeParams)
    require(sourceGenerics.length == targetGenerics.length,
      "Functions must have the same number of generic parameters")
    val ownerType = getDef[CHIR.CustomTypeDef](from.getBase.getDeclaredParent).map(d => typeId(d.tpe))
    val thisTypes = if (from.getBase.getDeclaredParent != to.getBase.getDeclaredParent && ownerType.nonEmpty) {
      (1L to length(model.getTypes).toLong).filter(id => getType[CHIR.Type](id).contains(CHIR.BuiltinType.This))
        .map(id => id -> ownerType.get).toMap
    } else Map.empty[Long, Long]
    val remapType = new TypeRemapper(model, sourceGenerics.zip(targetGenerics).toMap ++ thisTypes)
    sourceGenerics.zip(targetGenerics).foreach { (a, b) =>
      require(model.getTypes()(index(a)).getType == TypeElem.GenericType &&
        model.getTypes()(index(b)).getType == TypeElem.GenericType, "Expected generic types")
      val sourceBounds = model.getTypes()(index(a)).asGenericType().getUpperBounds
      val targetBounds = model.getTypes()(index(b)).asGenericType().getUpperBounds
      require(ids(sourceBounds).map(remapType.apply).sameElements(ids(targetBounds)),
        "Generic upper bounds must match after substitution")
    }
    require((if (structConstructor) mappedSourceParams.zip(mappedTargetParams).drop(1)
      else mappedSourceParams.zip(mappedTargetParams)).forall { (a, b) =>
      remapType(value(a).asParameter().getBase.getType) == value(b).asParameter().getBase.getType
    }, "Corresponding parameter types must match after substitution")

    def returnType(f: FunctionT): Long =
      model.getTypes()(index(f.getBase.getBase.getType)).asFuncType().getBase.getArgTys.last

    require(structConstructor || classConstructor || remapType(returnType(from)) == returnType(to),
      "Return types must match after substitution")
    require(model.getPackageInitFunc != sourceId && model.getPackageLiteralInitFunc != sourceId,
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
        require(!payload.isInstanceOf[LambdaT],
          "Nested lambda bodies are unsupported")
        val e = expressionBase(payload)
        require(e.getOwner == blockId, "Invalid expression owner")
        require(ids(e.getBlockGroups).isEmpty, "Nested block groups are unsupported")
        require(!ids(e.getOperands).exists(removedParams.contains),
          "Cannot remove a leading parameter that is used by the body")
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
        remapExpressionTypes(expressionEntries(index(id)).getValue, remapType.apply)
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
    changeFunctionBody(sourceId, from, 0L, 0L)
    changeFunctionBody(targetId, to, from.getBody, from.getRetVal)
    changes.set(nextLocal)(to.setLocalId)
    changes.set(math.max(to.getBlockId, from.getBlockId))(to.setBlockId)
    changes.set(math.max(to.getBlockGroupId, from.getBlockGroupId))(to.setBlockGroupId)

    changes.commit()
    if (structConstructor || (classConstructor && to.getRetVal == 0)) {
      val target = getValue[CHIR.Func](targetId).get
      val block = target.body.get.entryBlock
      val allocated = insert(block, CHIR.Operation.Allocate(target.tpe.returnType), block.expressions.headOption)
      if (to.getRetVal != 0) localTable(to.getRetVal).setIsRetVal(false)
      to.setRetVal(valueId(allocated))
      localTable(valueId(allocated)).setIsRetVal(true)
      if (structConstructor) replaceUses(target, target.params.head, allocated)
    }
    rememberGroupOwner(to.getBody)
  }

  private def opaque(payload: Object): CHIR.Expression = {
    val e = expressionBase(payload)
    def key: Product = payload match {
      case l: LambdaT => (e.getKind, l.getIdentifier, l.getSrcCodeName, l.getIsLocalFunc,
        getType[CHIR.Type](l.getFuncTy).map(com.huawei.excelsior.jet.compiler.chir.patchgen.Equality.typeKey),
        ids(l.getGenericTypeParams).toSeq.map(id => com.huawei.excelsior.jet.compiler.chir.patchgen.Equality.typeKey(getType[CHIR.Type](id).get)))
      case g: GetInstantiateValueT => (e.getKind, ids(g.getInstantiateTypes).toSeq.map(id =>
        com.huawei.excelsior.jet.compiler.chir.patchgen.Equality.typeKey(getType[CHIR.Type](id).get)))
      case f: FieldByNameT => (e.getKind, Option(f.getFieldNames).toSeq.flatten.toVector)
      case g: GetElementByNameT => (e.getKind, Option(g.getFieldNames).toSeq.flatten.toVector)
      case s: StoreElementByNameT => (e.getKind, Option(s.getFieldNames).toSeq.flatten.toVector)
      case _ => Tuple1(e.getKind)
    }
    if (isTerminator(e)) new CHIR.Terminator with CHIR.HasSuccessors with CHIR.OpaqueExpression {
      def successors: Seq[CHIR.Block] = successorIds(e).map(blockView).toSeq
      def debugLoc: Option[CHIR.DebugLocation] = location(e)
      def operationKey: Product = key
      def valueOperands: Seq[CHIR.Value] = ids(e.getOperands).map(value).toSeq
    } else new CHIR.OpaqueExpression {
      def debugLoc: Option[CHIR.DebugLocation] = location(e)
      def operationKey: Product = key
      def valueOperands: Seq[CHIR.Value] = ids(e.getOperands).map(value).toSeq
    }
  }

  override def newGoto(destination: CHIR.Block, debugLoc: Option[CHIR.DebugLocation] = None): CHIR.Goto = {
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
    val replacementIds = replacement.toSet
    require(replacementIds.size == replacement.length, "Duplicate expressions")
    replacement.foreach { id =>
      val e = expressionBase(expressionTable(id).getValue)
      val owner = e.getOwner
      require(owner == 0 || owner == blockId || !ids(blockTable(owner).getExprs).contains(id),
        "Expression is attached to another block")
      if (e.getResultLocalVar != 0)
        require(valueTable(e.getResultLocalVar).asLocalVar().getAssociatedExpr == id,
          "Result local points to another expression")
    }
    val previous = ids(table.getExprs)
    previous.filterNot(replacementIds).foreach(id => expressionBase(expressionTable(id).getValue).setOwner(0))
    replacement.foreach { id =>
      expressionBase(expressionTable(id).getValue).setOwner(blockId)
      historicalExpressionOwners(id) = blockId
    }
    table.setExprs(replacement)
    predecessorsDirty = true
  }

  private[v1203] def replaceBlocks(groupId: Long, blocks: Seq[CHIR.Block], entry: CHIR.Block): Unit = {
    val table = groupTable(groupId)
    val replacement = checkedBlocks(groupId, blocks, entry)
    val replacementIds = replacement.toSet
    ids(table.getBlocks).filterNot(replacementIds).foreach(blockTable(_).setParentGroup(0))
    replacement.foreach { id =>
      blockTable(id).setParentGroup(groupId)
      historicalBlockParents(id) = groupId
    }
    table.setBlocks(replacement)
    table.setEntryBlock(valueId(entry))
    predecessorsDirty = true
  }

  private def checkedBlocks(groupId: Long, blocks: Seq[CHIR.Block], entry: CHIR.Block): Array[Long] = {
    val result = blocks.map(valueId).toArray
    require(result.distinct.length == result.length, "Duplicate blocks")
    require(result.contains(valueId(entry)), "Entry block must be in blocks")
    result.foreach { id =>
      val parent = blockTable(id).getParentGroup
      require(parent == 0 || parent == groupId || !ids(groupTable(parent).getBlocks).contains(id),
        "Block belongs to another group")
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
      if (closed && reachable.size != members.size) {
        g.replaceBlocks(g.blocks.filter(b => reachable(valueId(b))), g.entryBlock)
      }
      g.blocks.foreach { b =>
        ids(blockTable(valueId(b)).getExprs).foreach { id =>
          nestedGroups(expressionTable(id).getValue).foreach(gid => visit(group(gid)))
        }
      }
    }

    function.body.foreach(visit)
  }

  private def referenceIssues(): Seq[ValidationIssue] = {
    flushTables()
    val staged = mutable.ArrayBuffer.empty[ValidationIssue]
    (originalExpressionCount.toLong + 1L to expressionEntries.length.toLong).foreach { id =>
      if (expressionBase(expressionTable(id).getValue).getOwner == 0 && !historicalExpressionOwners.contains(id))
        staged += ValidationIssue("New expression has not been attached", Some(EntityRef(EntityTable.Expressions, id)))
    }
    (originalValueCount.toLong + 1L to valueEntries.length.toLong).foreach { id =>
      val entry = valueTable(id)
      if (entry.getType == ValueElem.Block && entry.asBlock().getParentGroup == 0 && !historicalBlockParents.contains(id))
        staged += ValidationIssue("New block has not been attached", Some(EntityRef(EntityTable.Values, id)))
      if (entry.getType == ValueElem.BlockGroup && entry.asBlockGroup().getOwnedFunc == 0 &&
        entry.asBlockGroup().getOwnedExpression == 0 && !historicalGroupOwners.contains(id))
        staged += ValidationIssue("New group has not been attached", Some(EntityRef(EntityTable.Values, id)))
    }
    staged.toSeq ++ SchemaValidation.references(model)
  }

  private def requireValid(problems: Seq[ValidationIssue]): Unit = {
    require(problems.isEmpty, problems.map(_.message).mkString("Invalid CHIR: ", "; ", ""))
  }

  private def ensurePredecessors(): Unit = {
    if (predecessorsDirty) {
      rebuildPredecessors()
      predecessorsDirty = false
    }
  }

  private[v1203] def validationIssues(): Seq[ValidationIssue] = {
    val problems = referenceIssues()
    if (problems.nonEmpty) problems else {
      ensurePredecessors()
      validateBodies()
    }
  }

  private def prepareBodies(): Unit = {
    // Check IDs before following them. Normalize and rebuild derived data once,
    // after the caller has finished editing, then validate the final graph.
    requireValid(referenceIssues())
    values.collect { case f: CHIR.Func if f.body.nonEmpty => f }.foreach(pruneUnreachableBlocks)
    ensurePredecessors()
    refreshCounters()
    requireValid(validateBodies())
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

  private def serializedBytes(): Array[Byte] = {
    val changes = new WireChanges
    // Find detached blocks before applying historical parents to the same model.
    val retiredBlocks = historicalBlockParents.keysIterator.filter(id => blockTable(id).getParentGroup == 0).toArray
    try {
      // Native readers still follow owners on retired entries. Project their
      // historical links only while packing, without copying the object graph.
      historicalExpressionOwners.foreach { (id, owner) =>
        val e = expressionBase(expressionTable(id).getValue)
        if (e.getOwner == 0) changes.set(e.getOwner, owner)(e.setOwner)
      }
      historicalBlockParents.foreach { (id, parent) =>
        val b = blockTable(id)
        if (b.getParentGroup == 0) changes.set(b.getParentGroup, parent)(b.setParentGroup)
      }
      historicalGroupOwners.foreach { (id, owners) =>
        val g = groupTable(id)
        if (g.getOwnedFunc == 0 && g.getOwnedExpression == 0) {
          changes.set(g.getOwnedFunc, owners._1)(g.setOwnedFunc)
          changes.set(g.getOwnedExpression, owners._2)(g.setOwnedExpression)
        }
      }
      if (retiredBlocks.nonEmpty) {
        // Historical parent links would make the native reader resurrect these
        // blocks. Keep their IDs in an unreferenced storage group on disk only.
        val functions = model.getValues.zipWithIndex.filter(_._1.getType == ValueElem.Function)
        val owner = functions.find(_._1.asFunction().getBody != 0).orElse(functions.headOption)
          .getOrElse(throw new IllegalArgumentException("Retired blocks have no function context"))
        val groupName = nextName(model.getValues.filter(_.getType == ValueElem.BlockGroup)
          .map(_.asBlockGroup().getBase.getIdentifier).toSeq, "")
        val storage = new BlockGroupT
        storage.setBase(newValue(groupName.toString, ValueKind.BLOCK_GROUP))
        storage.setOwnedFunc(owner._2 + 1L)
        storage.setBlocks(retiredBlocks)
        storage.setEntryBlock(retiredBlocks.head)
        val entry = new ValueElemUnion; entry.setType(ValueElem.BlockGroup); entry.setValue(storage)
        changes.set(model.getValues, model.getValues :+ entry)(model.setValues)
        val storageId = model.getValues.length.toLong
        retiredBlocks.foreach { id =>
          val b = blockTable(id)
          changes.set(b.getParentGroup, storageId)(b.setParentGroup)
          changes.set(b.getExprs, Array.emptyLongArray)(b.setExprs)
          changes.set(b.getPredecessors, Array.emptyLongArray)(b.setPredecessors)
        }
        val f = owner._1.asFunction()
        changes.set(f.getBlockGroupId, math.max(f.getBlockGroupId, Math.addExact(groupName, 1L)))(f.setBlockGroupId)
      }
      model.getValues.iterator.filter(_.getType == ValueElem.Function).foreach { entry =>
        val f = entry.asFunction()
        if (f.getBody == 0 && f.getFuncKind == FuncKind.LAMBDA) {
          changes.set(f.getFuncKind, FuncKind.DEFAULT)(f.setFuncKind)
        }
      }
      val builder = new FlatBufferBuilder(1024)
      val root = CHIRPackage.pack(builder, model)
      if (hasIdentifier) CHIRPackage.finishCHIRPackageBuffer(builder, root) else builder.finish(root)
      builder.sizedByteArray()
    } finally changes.restore()
  }

  override def write(output: Path): Unit = {
    prepareBodies()
    val bytes = serializedBytes()
    val target = output.toAbsolutePath
    val temporary = Files.createTempFile(target.getParent, ".chir-", ".tmp")
    try {
      Files.write(temporary, bytes)
      try Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      catch { case _: AtomicMoveNotSupportedException => Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING) }
    } finally Files.deleteIfExists(temporary)
  }

  private[v1203] def valueTable(id: Long): ValueElemUnion = {
    require(id > 0 && id <= valueEntries.length, s"Invalid value ID $id")
    valueEntries((id - 1).toInt)
  }

  private[v1203] def expressionTable(id: Long): ExpressionElemUnion = {
    require(id > 0 && id <= expressionEntries.length, s"Invalid expression ID $id")
    expressionEntries((id - 1).toInt)
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
    else ids(e.getOperands).filter(id => id > 0 && id <= valueEntries.length && valueTable(id).getType == ValueElem.Block)

  private[v1203] def rebuildPredecessors(): Unit = {
    val incoming = mutable.LongMap.empty[mutable.LinkedHashSet[Long]]
    val members = mutable.LongMap.empty[Set[Long]]
    valuesOf(ValueElem.Block).foreach(_.asBlock().setPredecessors(Array.emptyLongArray))
    (1L to valueEntries.length.toLong).foreach { id =>
      val v = valueTable(id)
      if (v.getType == ValueElem.Block && v.asBlock().getParentGroup != 0 &&
        members.getOrElseUpdate(v.asBlock().getParentGroup,
          ids(groupTable(v.asBlock().getParentGroup).getBlocks).toSet).contains(id)) {
        val exprs = ids(v.asBlock().getExprs)
        if (exprs.nonEmpty) successorIds(expressionBase(expressionTable(exprs.last).getValue)).foreach { successor =>
          incoming.getOrElseUpdate(successor, mutable.LinkedHashSet.empty) += id
        }
      }
    }
    incoming.foreach((id, predecessors) => blockTable(id).setPredecessors(predecessors.toArray))
  }

  private def rememberOwners(): Unit = {
    (1L to expressionEntries.length.toLong).foreach { id =>
      val owner = expressionBase(expressionTable(id).getValue).getOwner
      if (owner != 0) historicalExpressionOwners(id) = owner
    }
    (1L to valueEntries.length.toLong).foreach { id =>
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
    id > 0 && id <= length(model.getTypes) && typeBase(model.getTypes()((id - 1).toInt).getValue).getKind == CHIRTypeKind.BOOLEAN
  }

  private def appendValue(tag: Byte, payload: Object): Long = {
    val entry = new ValueElemUnion; entry.setType(tag); entry.setValue(payload)
    valueEntries.append(entry)
  }

  private def appendExpression(tag: Byte, payload: Object): Long = {
    val entry = new ExpressionElemUnion; entry.setType(tag); entry.setValue(payload)
    expressionEntries.append(entry)
  }

  private def valuesOf(tag: Byte): Seq[ValueElemUnion] =
    valueEntries.iterator.filter(_.getType == tag).toVector

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

  private def validateBodies(): Seq[ValidationIssue] = {
    val pkg = this
    val problems = mutable.ArrayBuffer.empty[ValidationIssue]
    def report(message: String, ref: Option[EntityRef] = None): Unit = problems += ValidationIssue(message, ref)
    def check(test: Boolean, message: => String, ref: Option[EntityRef] = None): Unit = {
      if (!test) report(message, ref)
    }
    def safely(ref: Option[EntityRef])(action: => Unit): Unit = {
      try action catch { case NonFatal(e) => report(Option(e.getMessage).getOrElse("Invalid CHIR payload"), ref) }
    }

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
      val memberIds = members.toSet
      check(members.distinct.length == members.length, "Duplicate blocks in group", ref)
      check(memberIds.contains(g.getEntryBlock), "Entry block is not in group", ref)
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
              check(isBoolean(ops(0)), "Branch condition is not Boolean", eref)
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
          check(memberIds.contains(destination), "Control-flow target is outside group", bref)
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
        if (memberIds.contains(bid) && reachable.add(bid)) {
          val exprs = ids(pkg.blockTable(bid).getExprs)
          check(exprs.nonEmpty && isTerminator(expressionBase(pkg.expressionTable(exprs.last).getValue)),
            "Reachable block has no terminator", Some(EntityRef(Values, bid)))
          todo.pushAll(successors.getOrElse(bid, Array.emptyLongArray))
        }
      }
      expressionIds.toSet
    }

    valueEntries.iterator.zipWithIndex.foreach { (v, i) =>
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

  private def rememberGroupOwner(id: Long): Unit = {
    if (id != 0) {
      val g = groupTable(id)
      if (g.getOwnedFunc != 0 || g.getOwnedExpression != 0) {
        historicalGroupOwners(id) = (g.getOwnedFunc, g.getOwnedExpression)
      }
    }
  }

  private def changeFunctionBody(
    functionId: Long,
    function: FunctionT,
    bodyId: Long,
    retValId: Long
  )(using changes: Changes): Unit = {
    if (function.getBody != bodyId && function.getBody != 0)
      changes.set(0L)(groupTable(function.getBody).setOwnedFunc)
    if (function.getRetVal != retValId && function.getRetVal != 0)
      changes.set(false)(localTable(function.getRetVal).setIsRetVal)
    if (bodyId != 0) changes.set(functionId)(groupTable(bodyId).setOwnedFunc)
    if (retValId != 0) changes.set(true)(localTable(retValId).setIsRetVal)
    changes.set(bodyId)(function.setBody)
    changes.set(retValId)(function.setRetVal)
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
    val replacement = Option.when(bodyId != 0)(groupTable(bodyId))
    val newRetVal = Option.when(retValId != 0)(localTable(retValId))

    val groups = scala.collection.mutable.Set.empty[Long]
    val blocks = scala.collection.mutable.Set.empty[Long]
    val expressions = scala.collection.mutable.Set.empty[Long]
    val resultLocals = scala.collection.mutable.Set.empty[Long]
    replacement.foreach { root =>
      require(root.getOwnedExpression == 0,
        "Body is owned by another function or expression; explicitly move/clone it first")
      require(!valueEntries.exists { entry =>
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
            nestedGroups(payload).foreach(visit(_, expressionId))
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

    // Calculate counters before committing, so failures leave the graph intact.
    val localCounter = nextName(
      resultLocals.toSeq.map(id => localTable(id).getBase.getIdentifier) ++
        ids(target.getParams).toSeq.map(id => valueTable(id).asParameter().getBase.getIdentifier), "%", target.getLocalId)
    val blockCounter = nextName(
      blocks.toSeq.map(id => valueTable(id).asBlock().getBase.getIdentifier), "#", target.getBlockId)
    val groupCounter = nextName(
      groups.toSeq.map(id => groupTable(id).getBase.getIdentifier), "", target.getBlockGroupId)

    // Commit only after all references, ownership and counters have been checked.
    given changes: Changes = new Changes
    changeFunctionBody(functionId, target, bodyId, retValId)
    changes.set(localCounter)(target.setLocalId)
    changes.set(blockCounter)(target.setBlockId)
    changes.set(groupCounter)(target.setBlockGroupId)
    changes.commit()
    rememberGroupOwner(bodyId)
  }

  // APIs currently called only by tests in this repository (excluding forwarders).
  def typeId(tpe: CHIR.Type): Long = {
    // Primitive type objects are shared singletons. Use the first equivalent
    // table entry as their canonical reference, without rewriting loaded IDs.
    if (!typeIds.containsKey(tpe)) (1L to length(model.getTypes).toLong).foreach(getType[CHIR.Type])
    registered(typeIds, tpe, "type")
  }

  def newBlock(isLandingPad: Boolean = false): CHIR.Block = {
    val b = new BlockT
    b.setBase(newValue(s"#$nextBlockName", ValueKind.BLOCK))
    b.setIsLandingPadBlock(isLandingPad)
    b.setExprs(Array.emptyLongArray); b.setPredecessors(Array.emptyLongArray)
    b.setExceptionCatchList(Array.emptyLongArray)
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
    blockIds.foreach { blockId =>
      blockTable(blockId).setParentGroup(id)
      historicalBlockParents(blockId) = id
    }
    predecessorsDirty = true
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

  override def expressions(function: CHIR.Func): Seq[CHIR.Expression] = {
    def visit(group: CHIR.BlockGroup): Seq[CHIR.Expression] = group.blocks.flatMap { block =>
      block.expressions.flatMap { e =>
        e +: nestedGroups(expressionTable(expressionId(e)).getValue).toSeq.flatMap(id => visit(this.group(id)))
      }
    }
    function.body.toSeq.flatMap(visit)
  }

  override def isInternal(function: CHIR.Func): Boolean = {
    val base = valueTable(valueId(function)).asFunction().getBase.getBase.getBase
    Option(base.getAnnos).toSeq.flatten.exists { a => a.getValue match {
      case a: LinkTypeInfoT => a.getLinkage == Linkage.INTERNAL
      case _ => false
    } }
  }

  private def replaceUses(function: CHIR.Func, from: CHIR.Value, to: CHIR.Value): Unit = {
    val oldId = valueId(from)
    val newId = valueId(to)
    expressions(function).foreach { e =>
      val table = expressionBase(expressionTable(expressionId(e)).getValue)
      table.setOperands(ids(table.getOperands).map(id => if (id == oldId) newId else id))
    }
  }

  override def setOptimized(): Unit = model.setPhase(Phase.OPT)

  private def ensureType(tpe: CHIR.Type): Long = {
    val cached = insertedTypeObjects.get(tpe)
    if (cached != null) return cached.longValue()
    val key = com.huawei.excelsior.jet.compiler.chir.patchgen.Equality.typeKey(tpe)
    val id = insertedTypeIds.getOrElseUpdate(key, {
      val found = (1L to length(model.getTypes).toLong).find(id => getType[CHIR.Type](id).contains(tpe))
      found.getOrElse {
        val t = new TypeT
        t.setArgTys(Array.emptyLongArray)
        tpe match {
          case tpe: CHIR.RefType => t.setKind(CHIRTypeKind.REFTYPE); t.setArgTys(Array(ensureType(tpe.baseType)))
          case CHIR.BuiltinType.Unit => t.setKind(CHIRTypeKind.UNIT)
          case CHIR.BuiltinType.Boolean => t.setKind(CHIRTypeKind.BOOLEAN)
          case _ => throw new IllegalArgumentException("Missing type for inserted expression: " + tpe)
        }
        val u = new TypeElemUnion
        u.setType(TypeElem.Type); u.setValue(t)
        model.setTypes(model.getTypes :+ u)
        model.getTypes.length.toLong
      }
    })
    insertedTypeObjects.put(tpe, Long.box(id))
    id
  }

  private def operation(operation: CHIR.Operation): (Byte, Object, Long) = {
    import CHIR.Operation.*
    def ref(tpe: CHIR.Type): CHIR.RefType = new CHIR.RefType { def baseType: CHIR.Type = tpe }
    operation match {
      case Allocate(tpe) =>
        val e = new AllocateBaseT
        e.setBase(newExpression(CHIRExprKind.Allocate, Array.emptyLongArray, None))
        e.setAllocatedType(ensureType(tpe))
        (ExpressionElem.AllocateBase, e, ensureType(ref(tpe)))
      case StaticCast(value, tpe) =>
        (ExpressionElem.Expression, newExpression(CHIRExprKind.StaticCast, Array(valueId(value)), None), ensureType(tpe))
      case BooleanConstant(value) =>
        val base = newValue("", ValueKind.LITERAL)
        base.setType(ensureType(CHIR.BuiltinType.Boolean))
        val literal = new LiteralValueT
        literal.setBase(base); literal.setLiteralKind(ConstantValueKind.BOOL)
        val bool = new BoolLiteralT
        bool.setBase(literal); bool.setVal(value)
        val id = appendValue(ValueElem.BoolLiteral, bool)
        (ExpressionElem.Expression, newExpression(CHIRExprKind.Constant, Array(id), None), base.getType)
      case Tuple(values, tpe) =>
        (ExpressionElem.Expression, newExpression(CHIRExprKind.Tuple, values.map(valueId).toArray, None), ensureType(tpe))
      case Store(value, location) =>
        (ExpressionElem.Expression, newExpression(CHIRExprKind.Store, Array(valueId(value), valueId(location)), None),
          ensureType(CHIR.BuiltinType.Unit))
      case Apply(function, args) =>
        val e = new ApplyBaseT
        val call = new FuncCallT
        call.setBase(newExpression(CHIRExprKind.Apply, (function +: args).map(valueId).toArray, None))
        call.setInstantiatedTypeArgs(Array.emptyLongArray)
        e.setBase(call)
        (ExpressionElem.ApplyBase, e, ensureType(function.tpe.returnType))
    }
  }

  override def insert(block: CHIR.Block, op: CHIR.Operation, before: Option[CHIR.Expression]): CHIR.LocalVar = {
    val blockId = valueId(block)
    val function = enclosingFunction(blockTable(blockId).getParentGroup).getOrElse(
      throw new IllegalArgumentException("Insertion requires a function-owned block"))
    val owner = valueTable(valueId(function)).asFunction()
    val (tag, payload, resultType) = operation(op)
    val expression = expressionBase(payload)
    val local = new LocalVarT
    val base = newValue(s"%${owner.getLocalId}", ValueKind.LOCALVAR)
    base.setType(resultType)
    owner.setLocalId(owner.getLocalId + 1L)
    local.setBase(base)
    local.setSrcCodeIdentifier("")
    val localId = appendValue(ValueElem.LocalVar, local)
    val id = appendExpression(tag, payload)
    expression.setOwner(blockId); expression.setResultTy(resultType); expression.setResultLocalVar(localId)
    local.setAssociatedExpr(id)
    val prefix = block.nonTerminatorExpressions
    val position = before.map(expressionId).getOrElse(expressionId(block.terminator))
    val index = block.expressions.indexWhere(e => expressionId(e) == position)
    require(index >= 0, "Insertion point is outside the block")
    block.replaceExpressions(prefix.patch(index, Seq(getExpr[CHIR.Expression](id)), 0), block.terminator)
    getValue[CHIR.LocalVar](localId).get
  }

  override def replace(expression: CHIR.Expression, op: CHIR.Operation): Unit = {
    val id = expressionId(expression)
    val old = expressionBase(expressionTable(id).getValue)
    require(!isTerminator(old), "Cannot replace a terminator with a value operation")
    val (tag, payload, resultType) = operation(op)
    val e = expressionBase(payload)
    e.setBase(old.getBase); e.setOwner(old.getOwner)
    e.setResultLocalVar(old.getResultLocalVar); e.setResultTy(resultType)
    if (old.getResultLocalVar != 0) localTable(old.getResultLocalVar).getBase.setType(resultType)
    val union = expressionTable(id)
    union.setType(tag); union.setValue(payload)
    expressionViews.remove(id)
  }

}
