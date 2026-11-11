package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.CHIR.*

/** The plugin's entry guard is Load(flag), Not, Branch(user code, patch dispatch).
  * Keep the entry block and its return allocation, bypass dispatch, and let the
  * package remove unreachable blocks. No expressions need cloning or renaming.
  */
private[chir] object GuardChecks {
  def flag(function: Func): Option[GlobalVar] = {
    function.body.flatMap { body =>
      body.entryBlock.terminator match {
        case b: Branch => b.condition match {
          case v: LocalVar => v.associatedExpr match {
            case u: Unary if u.kind == Unary.Kind.Not => u.operand match {
              case v: LocalVar => v.associatedExpr match {
                case l: Load => l.location match {
                  case g: GlobalVar if g.srcCodeIdentifier == PatchableName.guardFlag(function) => Some(g)
                  case _ => None
                }
                case _ => None
              }
              case _ => None
            }
            case _ => None
          }
          case _ => None
        }
        case _ => None
      }
    }
  }

  def hasFlag(function: Func): Boolean = flag(function).nonEmpty

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
