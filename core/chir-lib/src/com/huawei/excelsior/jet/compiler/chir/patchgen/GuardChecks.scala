package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.CHIR.*

/** The plugin's entry guard is Load(flag), Not, Branch(user code, patch dispatch).
  * Keep the entry block and its return allocation, bypass dispatch, and let the
  * package remove unreachable blocks. No expressions need cloning or renaming.
  */
private[chir] object GuardChecks {
  private object Branch {
    def unapply(expression: Expression): Option[Value] = expression match {
      case b: Branch => Some(b.condition)
      case _ => None
    }
  }

  private object LocalVar {
    def unapply(value: Value): Option[Expression] = value match {
      case v: LocalVar => Some(v.associatedExpr)
      case _ => None
    }
  }

  private object Not {
    def unapply(expression: Expression): Option[Value] = expression match {
      case u: Unary if u.kind == Unary.Kind.Not => Some(u.operand)
      case _ => None
    }
  }

  private object Load {
    def unapply(expression: Expression): Option[Value] = expression match {
      case l: Load => Some(l.location)
      case _ => None
    }
  }

  def getFlag(function: Func): Option[GlobalVar] = {
    function.body.flatMap { body =>
      body.entryBlock.terminator match {
        case Branch(LocalVar(Not(LocalVar(Load(g: GlobalVar)))))
          if g.srcCodeIdentifier == PatchableName.guardFlag(function) => Some(g)
        case _ => None
      }
    }
  }

  def flag(function: Func): GlobalVar =
    getFlag(function).getOrElse(throw new IllegalArgumentException("guard var flag check was not found"))

  def hasFlag(function: Func): Boolean = getFlag(function).nonEmpty

  def remove(function: Func): Unit = {
    val pkg = function.ownerPackage
    val initializer = (function eq pkg.packageInitFunc) || (function eq pkg.packageInitLiteralFunc)
    require(initializer || hasFlag(function), "guard var flag check was not found")
    val entry = function.body.get.entryBlock
    val branch = entry.terminator.asInstanceOf[Branch]
    require(branch.falseBlock.terminator.isInstanceOf[Branch], "guard check was not found")
    val prefix = entry.nonTerminatorExpressions
    require(prefix.size >= 2, "guard load and negation were not found")
    val condition = branch.condition.asInstanceOf[LocalVar].associatedExpr
    val load = condition.asInstanceOf[Unary].operand.asInstanceOf[LocalVar].associatedExpr
    require((prefix.last eq condition) && (prefix(prefix.size - 2) eq load),
      "expected Load(guardVar) and Not at the end of the entry block")
    entry.replaceExpressions(prefix.dropRight(2), function.ownerPackage.newGoto(branch.trueBlock, branch.debugLoc))
  }
}
