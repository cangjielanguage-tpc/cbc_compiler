package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*

private[v13002] object EditableStorage {
  def length[A](xs: Array[A]): Int = if (xs == null) 0 else xs.length
  def ids(xs: Array[Long]): Array[Long] = if (xs == null) Array.emptyLongArray else xs
  def expressionBase(x: Object): ExpressionT = FunctionBodyReplacement.expressionBase(x)
  def isTerminator(e: ExpressionT): Boolean = e.getKind >= CHIRExprKind.Goto && e.getKind <= CHIRExprKind.TryRShift
  def nestedGroups(x: Object): Array[Long] = {
    val extra = x match {
      case e: LambdaT if e.getBody != 0 => Array(e.getBody)
      case _ => Array.emptyLongArray
    }
    (ids(expressionBase(x).getBlockGroups) ++ extra).distinct
  }
  def nextName(names: Iterable[String], prefix: String): Long = names.foldLeft(0L) { (next, name) =>
    if (name == null || !name.startsWith(prefix)) next
    else name.substring(prefix.length).toLongOption match {
      case Some(n) if n >= 0 => math.max(next, Math.addExact(n, 1L))
      case _ => next
    }
  }
  def newValue(identifier: String, kind: Int): ValueT = {
    val value = new ValueT
    value.setBase(newBase()); value.setIdentifier(identifier); value.setKind(kind)
    value
  }
  def newBase(): BaseT = {
    // The native deserializer expects these objects/vectors even for nodes
    // without a source location. Optional fields in the schema are not always
    // optional in that implementation.
    val loc = new DebugLocationT
    loc.setFilePath(""); loc.setBeginPos(new PosT); loc.setEndPos(new PosT)
    loc.setScope(Array.emptyIntArray)
    val b = new BaseT; b.setLoc(loc); b.setAnnos(Array.empty[AnnotationUnion])
    b
  }
  def newExpression(kind: Int, operands: Array[Long], debug: Option[CHIR.DebugLocation]): ExpressionT = {
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
  def location(e: ExpressionT): Option[CHIR.DebugLocation] = for {
    base <- Option(e.getBase)
    loc <- Option(base.getLoc)
    first <- Option(loc.getBeginPos)
    last <- Option(loc.getEndPos)
    if first.getLine > 0 && last.getLine > 0
  } yield CHIR.DebugLocation(first.getLine, last.getLine)

  def typeBase(x: Object): TypeT = x match {
    case t: TypeT => t
    case t: RawArrayTypeT => t.getBase
    case t: VArrayTypeT => t.getBase
    case t: FuncTypeT => t.getBase
    case t: CustomTypeT => t.getBase
    case t: GenericTypeT => t.getBase
    case _ => throw new IllegalArgumentException("Unsupported type payload")
  }
  def valueBase(x: Object): ValueT = x match {
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
}
