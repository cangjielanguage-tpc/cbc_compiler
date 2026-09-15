package com.huawei.excelsior.jet.compiler.chir.dsl

import com.huawei.excelsior.jet.compiler.chir.CHIR

import scala.collection.mutable

final class BlockGroup(val blocks: Seq[CHIR.Block], val entryBlock: CHIR.Block) extends CHIR.BlockGroup

final class Block extends CHIR.Block {
  private val exprs = mutable.ArrayBuffer.empty[CHIR.Expression]
  private var term: CHIR.Terminator = _
  private var isLandingPad = false

  def frozen = term != null

  def addExpr(expr: CHIR.Expression): Unit = exprs += expr

  def setTerminator(expr: CHIR.Terminator): Unit = term = expr

  def markAsLandingPad(): Unit = isLandingPad = true

  def expressions: Seq[CHIR.Expression] = {
    assert(frozen)
    exprs.toSeq :+ term
  }

  def nonTerminatorExpressions: Seq[CHIR.Expression] = {
    assert(frozen)
    exprs.toSeq
  }

  def terminator: CHIR.Terminator = {
    assert(frozen)
    term
  }

  def isLandingPadBlock: Boolean = {
    assert(frozen)
    isLandingPad
  }
}

final class LocalVar(val tpe: CHIR.Type, val associatedExpr: CHIR.Expression) extends CHIR.LocalVar
