/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.testutils.DSLs

import com.huawei.excelsior.jet.util.graph.Graph
import collection.mutable.ArrayBuffer
import scala.language.implicitConversions

/**
 * Utility for checking CFG transformations in terms "before", transform and "after".
 */
trait CFGTransformationDSL extends IRBuilderDSL {

  /**
   * Transformation
   */
  def transformation(): Unit

  /**
   * Debug enabling
   */
  def makeDebug: Boolean

  private var graphBefore: Graph[Int] = _

  private def makeGraphFromCFG(cfg: Graph[Block]): Graph[Int] = {
    val edges = for (b <- all[Block]; s <- b.xSuccBlocks) yield (b.id, s.id)
    createGraph(cfg.start.id, edges.toList)
  }

  private def debug(cfg: Graph[Block], message: String): Unit = {
    if (makeDebug) {
      println("//////////////////////")
      println("// " + message)
      val printer = new StdOutDebugPrinter
      printer.debugCFG(message + " (CFG)")
      printer.debugNodes(message + " (Nodes)")
      printer.debugGraphs(message + " (Graphs)")
      println("// " + message)
      println("//////////////////////")
    }
  }

  def before(start: SubGraph): Unit = {
    before(start, {}, {})
  }

  def beforeWithPre(start: SubGraph, preAction: => Unit): Unit = {
    before(start, preAction, {})
  }

  def beforeWithPost(start: SubGraph, postAction: => Unit): Unit = {
    before(start, {}, postAction)
  }

  def before(start: SubGraph, preAction: => Unit, postAction: => Unit): Unit = {
    makeCFG(start)
    debug(cfg, "initial")
    preAction
    debug(cfg, "before transformation")
    undoGCM()
    transformation()
    debug(cfg, "after transformation")
    postAction
    debug(cfg, "final")
    graphBefore = makeGraphFromCFG(cfg)
    beforeEach()
  }

  private def unpin(n: Node): Unit = { n match {
    case _: PinnedNode =>
    case n: FloatingNode => n atUpperPoint null
  }}

  private def undoGCM(): Unit = {
    allNodes foreach unpin
  }

  class Templates(t0: SubGraph) {
    val all = ArrayBuffer(t0)

    def orElse(t1: SubGraph) = { all += t1; this }
  }

  implicit def asTemplates(template: SubGraph): Templates = new Templates(template)

  def after(template: SubGraph): Unit = {
    after(new Templates(template))
  }

  def after(templates: Templates): Unit = {
    assert(templates.all.nonEmpty)

    val entry = -153 // any arbitrary unused number
    val expectedGraphs = templates.all map { t => createGraph(entry, (entry -> t).edges) }
    if (!(expectedGraphs exists (_ topologicallyEquals graphBefore))) {
      if (makeDebug) {
        for ((t, i) <- templates.all.zipWithIndex) {
          makeCFG(t)
          debug(cfg, s"template $i")
        }
      }
      fail(s"actual graph:\n${graphBefore}\nexpected graphs:\n${(expectedGraphs map (_.toString)).mkString("\n")}")
    }
  }
}