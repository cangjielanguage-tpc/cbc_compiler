package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.{CHIR, EditableCHIR}

final class EditableBlockImpl private[v13002] (
  val ownerPackage: EditablePackageImpl,
  private[v13002] val id: Long
) extends EditableCHIR.Block {
  private def table = ownerPackage.blockTable(id)
  def expressions: Seq[CHIR.Expression] =
    EditableStorage.ids(table.getExprs).toSeq.map(ownerPackage.getExpr[CHIR.Expression])
  def nonTerminatorExpressions: Seq[CHIR.Expression] = expressions.lastOption match {
    case Some(_: CHIR.Terminator) => expressions.init
    case _ => expressions
  }
  def terminator: CHIR.Terminator = expressions.lastOption match {
    case Some(t: CHIR.Terminator) => t
    case _ => throw new IllegalStateException(s"Block $id has no terminator")
  }
  def isLandingPadBlock: Boolean = table.getIsLandingPadBlock
  def replaceTerminator(terminator: CHIR.Terminator): Unit =
    replaceExpressions(nonTerminatorExpressions, terminator)
  def replaceExpressions(nonTerminators: Seq[CHIR.Expression], terminator: CHIR.Terminator): Unit =
    ownerPackage.replaceExpressions(id, nonTerminators, terminator)
}
