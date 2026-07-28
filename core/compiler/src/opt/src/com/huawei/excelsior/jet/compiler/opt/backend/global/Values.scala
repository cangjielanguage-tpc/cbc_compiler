/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.global

import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}

/**
 * Value is an object that associated with unique value, produced by some node (value producer).
 * Value could be represented by other nodes - copies that move this value to different resources
 * and IR-consistency nodes (phi/proxy).
 *
 * @author conwor
 */
trait Values { self: Universe with BackEnd =>

  /** Map from node to value, produced or represented by this node. */
  private val values = Maps[Node].newMMap[Value]

  class Value private[Values] (val producer: Node) {
    /** Appends given `synonym` that moves value produced by `producer` to this value. */
    private [Values] def addSynonym(synonym: Node): Unit = {
      assert(synonym.producesValue)
      assert(!values.contains(synonym))
      values(synonym) = this
    }
  }

  implicit object ValuesSetsAndMaps extends Sets.Default[Value] with Maps.Default[Value]

  /** @return value, produced or represented by given `node`. */
  def valueOf(node: Node): Value = values(node)

  /** @return whether given `node` has already created value. */
  // TODO: remove this method
  def hasValue(node: Node): Boolean = values.contains(node)

  /** Creates initial values table. Should be called once for method at backend start. */
  def createInitialValues(): Unit = {
    assert(values.isEmpty)
    for (node <- allNodes if node.producesValue) {
      values(node) = new Value(node)
    }
  }

  /** Creates proxy node for given `actual` node and makes them synonyms. */
  def makeSynonymProxy(actual: Node, block: Block): Proxy = {
    val proxy = Proxy(actual.tpe)(block)
    assert(values.contains(actual))
    values(actual).addSynonym(proxy)
    proxy
  }

  /** @return whether given `node` is synonym node. */
  def isSynonym(node: Node): Boolean = values.get(node) match {
    case Some(value) => value.producer != node
    case None => false
  }

  /** During the backend stage new operations may appear in the IR. In such a case the values' table should be
    * updated with new synonyms (copies & phi). Proxies also update the table, but in the different method
    * (makeSynonymProxy). */
  def updateValuesOnCommit(node: Node): Unit = {
    node match {
      // Synonyms created during local code generator for unblocking & normalization.
      case copy: Copy =>
        val arg = copy.transferArg
        assert(!copy.hasOwnValue)
        assert(values.contains(arg))
        values(arg).addSynonym(node)

      // Phi functions created during global code generation (SSA-form building)
      case phi: Phi =>
        assert(values.contains(phi.argsSeq.head))
        values(phi.argsSeq.head).addSynonym(phi)

      // Proxies created during global code generation (SSA-form building)
      case _: Proxy =>

      // Created in correctConstraints on post-process
      case _: Constraints =>

      // Created in insertPreCall on post-process
      case _: PreCall =>

      // Created in insertCode
      case _: ScopeAnchor | _: BBlock | _: Return | _: Void =>
    }
  }
}
