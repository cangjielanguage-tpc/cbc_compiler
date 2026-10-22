package com.huawei.excelsior.jet.compiler.chir.dsl

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.compiler.chir.CHIR

import scala.collection.mutable

final class BlockGroup(var blocks: Seq[CHIR.Block], var entryBlock: CHIR.Block) extends CHIR.BlockGroup {
  def ownerPackage: CHIR.Package = CodeHelpers.shouldNotReachHere("DSL block groups have no serialized package owner")
  def replaceBlocks(blocks: Seq[CHIR.Block], entryBlock: CHIR.Block): Unit = {
    require(blocks.distinct.size == blocks.size && blocks.contains(entryBlock), "Invalid block group membership")
    this.blocks = blocks.toVector
    this.entryBlock = entryBlock
  }
}

final class Block extends CHIR.Block {
  def ownerPackage: CHIR.Package = CodeHelpers.shouldNotReachHere("DSL blocks have no serialized package owner")
  private val exprs = mutable.ArrayBuffer.empty[CHIR.Expression]
  private var term: CHIR.Terminator = _
  private var isLandingPad = false

  def frozen = term != null

  def addExpr(expr: CHIR.Expression): Unit = exprs += expr

  def setTerminator(expr: CHIR.Terminator): Unit = term = expr

  def replaceTerminator(terminator: CHIR.Terminator): Unit = {
    require(terminator != null, "Missing terminator")
    setTerminator(terminator)
  }

  def replaceExpressions(nonTerminators: Seq[CHIR.Expression], terminator: CHIR.Terminator): Unit = {
    require(terminator != null && nonTerminators.forall(e => e != null && !e.isInstanceOf[CHIR.Terminator]),
      "Expected non-terminators followed by one terminator")
    val replacement = nonTerminators.toVector
    exprs.clear()
    exprs ++= replacement
    term = terminator
  }

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

final class LocalVar(val tpe: CHIR.Type, val associatedExpr: CHIR.Expression,
                     val identifier: String, val srcCodeIdentifier: String) extends CHIR.LocalVar
