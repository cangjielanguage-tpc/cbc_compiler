/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm.preferred

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.util.graph.ordering.NaturalCFGOrder.LoopOrientation
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.LivenessAnalysis
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.graph.ordering.NaturalCFGOrder

/**
  * Collect information about preferred/unpreferred locations for nodes.
  */
trait Preferred extends LivenessAnalysis { self: Universe with BackEnd =>

  protected type PreferencesMap = Maps[Node]#QMap[ResourceSet]

  // TODO: refactor this and move to machine description
  def preferredLoc(edge: Edge, unPreferred: PreferencesMap, preferred: PreferencesMap): ResourceSet = {
    assert(edge.isValue)

    edge.target match {
      case _: Return => setOf(rootABI.resultLocation)

      case call: Call =>
        val idx = call.invokeArgIdx(edge)
        if (idx >= 0) setOf(call.abi.paramLocations(idx)) else emptySet

      case _ => emptySet
    }
  }

  class PreferredUAI(gcm: GCMEngine) {

    /** Liveness information, calculated after second pass. */
    private val liveness: CFGLiveness = calcCFGLiveness()

    val preferred: PreferencesMap = Maps[Node].newQMap[ResourceSet]
    val unPreferred: PreferencesMap = Maps[Node].newQMap[ResourceSet]

    def debugInfo(node: Node): String = {
      val str = new StringBuilder
      for (preferred <- preferred.get(node)) str.append(" preferred: (" + preferred.toString() + ") ")
      for (unPreferred <- unPreferred.get(node)) str.append(" unPreferred: (" + unPreferred.toString() + ")")
      str.toString
    }

    private def registerPreferred(node: Node, locs: ResourceSet, prefer: Boolean): Unit = {
      val map = if (prefer) preferred else unPreferred
      map(node) = map.get(node) match {
        case Some(old) => old | locs
        case None => locs
      }
      if (!prefer) {
        preferred.get(node) match {
          case Some(old) => preferred(node) = old &~ locs
          case None =>
        }
      }
    }

    private def copyPreferredInfo(from: Node, to: Node): Unit = {
      registerPreferred(to, preferred.getOrElse(from, emptySet), prefer = true)
      registerPreferred(to, unPreferred.getOrElse(from, emptySet), prefer = false)
    }

    def iterate(): Unit = {
      // We cannot process blocks in random order, because `preferred` collection is changed during upward interpretation
      val order = NaturalCFGOrder(cfg, LoopOrientation.HEADER_FIRST).reverse

      for (block <- order) {
        val state = liveness.out(block)
        val isCold = gcm.cold(block)

        for (node <- CodeOrder reversedIn block) {
          if (!isCold) {
            // TODO: consider to take into account only resources volatile on NORMAL ExitKind
            val volatiles = volatileResources(node)
            if (volatiles.nonEmpty) {
              for (ln <- state.iterator if ln != node) {
                registerPreferred(ln, volatiles, prefer = false)
              }
            }

            for (edge <- node.inEdgesByTag(Tag.VALUE)) {
              registerPreferred(edge.source,
                preferredLoc(edge, unPreferred, preferred), prefer = true)
            }

            node match {
              case phi: Phi =>
                phi.args foreach {
                  // TODO: what about backward branch arg?
                  arg => copyPreferredInfo(node, arg)
                }

              case _ =>
                for (Edge(arg, _) <- uniqueBoundEdge(node)) {
                  copyPreferredInfo(node, arg)
                }
            }
          }

          node match {
            case _: Phi =>
            case x if x.isGroupRoot =>
              state --= node.groupedValueResults
              state ++= node.groupedValueArgs
            case _ =>
          }

          node match {
            case sn: SpinalNode if sn.hasXHandler =>
              state ++= liveness.in(sn.xHandler)
              assert(sn.xHandler.phies.isEmpty)
            case _ =>
          }
        }
      }
    }
  }
}
