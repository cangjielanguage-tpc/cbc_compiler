package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*
import scala.collection.mutable.ArrayBuffer

/** Runtime replacements for the C++ compile-time `#if TEST` / `#if DEBUG` blocks,
  * enabled by the `--test` / `--debug` command-line options.
  *
  * - `Debug.log` is the `#if DEBUG std::cout << ...` equivalent: a no-op unless
  *   `--debug` was given. The message is by-name, so disabled runs pay nothing.
  * - `DebugDump.dumpTestInfo` reproduces the `#if TEST` block in C++ main()
  *   (byte-compatible stdout dump of the regenerated patch package).
  * - The value/expression/type renderers below are a compact stand-in for the
  *   C++ `ToString` IR dumps used by DEBUG logging; their text is NOT
  *   byte-compatible with C++ (debug output is never compared by tests).
  */
object Debug {
  private var on = false
  def enabled: Boolean = on
  def setEnabled(v: Boolean): Unit = on = v

  /** `#if DEBUG std::cout << msg << std::endl` equivalent. */
  def log(msg: => String): Unit = if (on) println(msg)
}

object DebugDump {

  /** The `#if TEST` dump from C++ main(): after the patch is generated, the patch
    * file is re-deserialized and the identifiers of its non-imported global vars,
    * functions with body, and current-package custom type defs are printed. The
    * hotfix patch-gen test suite compares exactly this output.
    */
  def dumpTestInfo(pkg: Package): Unit = {
    println("global vars")
    for (v <- pkg.globalVars if (v.attributes & Attribute.Imported) == 0L) {
      println(withoutPrefix(v.identifier))
    }
    println("global funcs")
    for (f <- pkg.globalFuncs if f.body.isDefined) {
      println(withoutPrefix(f.identifier))
    }
    println("types")
    for (d <- pkg.curPkgCustomTypeDefs) {
      println(withoutPrefix(d.identifier))
    }
  }

  /** C++ `Value::GetIdentifierWithoutPrefix()` = identifier.substr(1). */
  def withoutPrefix(identifier: String): String =
    if (identifier.isEmpty) "" else identifier.substring(1)

  // ===================================================================================
  // compact IR rendering (stand-in for the C++ ToString(0) dumps)
  // ===================================================================================

  def ty(t: Type): String = t match {
    case t: TypeImpl =>
      if (t.argTys.isEmpty) t.kind.toString else t.kind.toString + "(" + t.argTys.map(ty).mkString(", ") + ")"
    case t: FuncType =>
      val params = t.paramTypesWithoutReceiver.map(ty)
      "Func(" + ty(t.receiverType) + (if (params.isEmpty) "" else ", " + params.mkString(", ")) +
        (if (t.hasVarArg) ", ..." else "") + ") -> " + ty(t.returnType)
    case t: RawArrayType =>
      "RawArray(" + ty(t.elementType) + ", " + t.dims + ")"
    case t: VArrayType =>
      "VArray(" + ty(t.elementType) + ", " + t.size + ")"
    case t: CustomType =>
      val name = t.customTypeDef.map(d => withoutPrefix(d.identifier)).getOrElse("?")
      if (t.argTys.isEmpty) name else name + "(" + t.argTys.map(ty).mkString(", ") + ")"
    case t: GenericType =>
      withoutPrefix(t.identifier)
  }

  /** Full value dump: functions with body, others one-line. */
  def value(v: Value, indent: String = ""): String = v match {
    case f: Func => func(f, indent)
    case other   => indent + valueLine(other)
  }

  /** One-line rendering of a value; identifiers for locals/params/globals/funcs. */
  def valueLine(v: Value): String = v match {
    case l: BoolLiteral    => "Bool(" + l.value + ")"
    case l: RuneLiteral    => "Rune(" + l.value + ")"
    case l: StringLiteral  => "String(\"" + l.value + "\")"
    case l: IntLiteral     => "Int(" + l.value + ")"
    case l: FloatLiteral   => "Float(" + l.value + ")"
    case _: UnitLiteral    => "Unit"
    case _: NullLiteral    => "Null"
    case f: Func           => f.identifier
    case v: ValueBase =>
      val id = if (v.identifier.isEmpty) "<" + v.getClass.getSimpleName + ">" else v.identifier
      v.tpe.map(t => id + ": " + ty(t)).getOrElse(id)
  }

  /** Operand rendering inside expression lines (blocks by identifier). */
  private def operandRef(v: Value): String = v match {
    case b: Block => b.identifier
    case other    => valueLine(other)
  }

  def expr(e: Expression, indent: String = ""): String = {
    val resPart = e.resultVar match {
      case Some(r) => valueLine(r) + " = "
      case None    => ""
    }
    val kind = e.getClass.getSimpleName.stripSuffix("Expr")
    val ops = if (e.operands.isEmpty) "" else "(" + e.operands.map(operandRef).mkString(", ") + ")"
    indent + resPart + kind + ops
  }

  def block(b: Block, indent: String = ""): String = {
    val sb = new StringBuilder
    sb ++= indent ++= "block " ++= b.identifier ++= ":\n"
    for (e <- b.exprs) {
      sb ++= expr(e, indent + "  ") ++= "\n"
    }
    sb.toString
  }

  def blockGroup(bg: BlockGroup, indent: String = ""): String = {
    val sb = new StringBuilder
    sb ++= indent ++= "blockgroup " ++= bg.identifier ++= ":\n"
    for (b <- bg.blocks) {
      sb ++= block(b, indent + "  ")
    }
    sb.toString
  }

  def funcSig(f: Func, indent: String = ""): String = {
    val sb = new StringBuilder
    sb ++= indent ++= "func " ++= f.identifier
    f.tpe.foreach(t => sb ++= " : " ++= ty(t))
    sb.toString
  }

  def func(f: Func, indent: String = ""): String = {
    val sb = new StringBuilder
    sb ++= funcSig(f, indent) ++= "\n"
    for (p <- f.params) {
      sb ++= indent ++= "  param " ++= valueLine(p) ++= "\n"
    }
    f.retVal.foreach(r => sb ++= indent ++= "  return " ++= valueLine(r) ++= "\n")
    f.body.foreach(bg => sb ++= blockGroup(bg, indent + "  "))
    sb.toString
  }

  def customDef(d: CustomTypeDef, indent: String = ""): String = {
    val sb = new StringBuilder
    sb ++= indent ++= d.kind.toString.toLowerCase ++= " " ++= d.identifier
    if (d.srcCodeIdentifier.nonEmpty) {
      sb ++= " (" ++= d.srcCodeIdentifier ++= ")"
    }
    sb ++= "\n"
    for (m <- d.methods) {
      sb ++= indent ++= "  " ++= funcSig(m) ++= "\n"
    }
    for (v <- d.instanceMemberVars) {
      sb ++= indent ++= "  var " ++= v.name ++= " : " ++= ty(v.tpe) ++= "\n"
    }
    for (v <- d.staticMemberVars) {
      sb ++= indent ++= "  static var " ++= withoutPrefix(v.identifier) ++= " : " ++
        v.tpe.map(ty).getOrElse("?") ++= "\n"
    }
    sb.toString
  }
}
