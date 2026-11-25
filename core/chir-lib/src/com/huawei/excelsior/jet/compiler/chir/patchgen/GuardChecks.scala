package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.CHIR.*
import com.huawei.excelsior.jet.compiler.chir.EditableCHIR

/** The plugin's entry guard is Load(flag), Not, Branch(user code, patch dispatch).
  * Keep the entry block and its return allocation, bypass dispatch, and let the
  * package remove unreachable blocks. No expressions need cloning or renaming.
  */
private[chir] object GuardChecks {
  def getFlag(function: Func): Option[GlobalVar] = {
    function.body.flatMap { body =>
      body.entryBlock.terminator match {
        case Branch(LocalVar(Unary(Unary.Kind.Not, LocalVar(Load(g: GlobalVar)))), _, _)
          if g.srcCodeIdentifier == PatchableName.guardFlag(function) => Some(g)
        case _ => None
      }
    }
  }

  def flag(function: Func): GlobalVar =
    getFlag(function).getOrElse(throw new IllegalArgumentException("guard var flag check was not found"))

  def hasFlag(function: Func): Boolean = getFlag(function).nonEmpty

  private def isInterpreterInitializedCall(call: Apply): Boolean = call.callee match {
    case f: Func => f.identifier.stripPrefix("@") == "is_interpreter_initialized"
    case _ => false
  }

  def remove(function: EditableCHIR.Func): Unit = {
    val pkg = function.pkg
    val initializer = (function eq pkg.packageInitFunc) || (function eq pkg.packageInitLiteralFunc)
    require(initializer || hasFlag(function), "guard var flag check was not found")
    val body = function.body.get
    val entry = if (initializer) {
      body.entryBlock.terminator match {
        case Branch(LocalVar(a: Apply), initialized, _) if isInterpreterInitializedCall(a) =>
          // The native wrapper checks whether the interpreter is initialized before calling the CBC initializer.
          val guardBlock = pkg.edit(initialized)
          body.entryBlock.replaceExpressions(Seq.empty, pkg.newGoto(guardBlock, a.debugLoc))
          guardBlock
        case _ => body.entryBlock
      }
    } else body.entryBlock
    val branch = entry.terminator.asInstanceOf[Branch]
    require(branch.falseBlock.terminator.isInstanceOf[Branch], "guard check was not found")
    val prefix = entry.nonTerminatorExpressions
    require(prefix.size >= 2, "guard load and negation were not found")
    val condition = branch.condition.asInstanceOf[LocalVar].associatedExpr
    val load = condition.asInstanceOf[Unary].operand.asInstanceOf[LocalVar].associatedExpr
    require((prefix.last eq condition) && (prefix(prefix.size - 2) eq load),
      "expected Load(guardVar) and Not at the end of the entry block")
    entry.replaceExpressions(prefix.dropRight(2), pkg.newGoto(branch.trueBlock, branch.debugLoc))
  }
}
