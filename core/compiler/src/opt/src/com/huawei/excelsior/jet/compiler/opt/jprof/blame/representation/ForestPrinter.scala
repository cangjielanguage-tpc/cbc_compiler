/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.ProfileForest.CallTreeNode
import com.huawei.excelsior.jet.compiler.options.StrOption.OutputName
import xscala.io.TextOutput

import scala.collection.mutable.ArrayBuffer
import scala.util.Using

/** Utilities for printing of given [[ProfileForest]] in text or DOT format. */
object ForestPrinter {
  private var printed = 0

  case class Edge(from: CallTreeNode, to: CallTreeNode)

  var printId = 0

  def printTrees(forest: ProfileForest, name: String): Unit = {
    def getNodesAndEdges(roots: List[CallTreeNode]): (ArrayBuffer[CallTreeNode], ArrayBuffer[Edge]) = {

      val nodes = ArrayBuffer.empty[CallTreeNode]
      val edges = ArrayBuffer.empty[Edge]

      def addChildren(node: CallTreeNode): Unit = {
        nodes ++= node.children
        node.printId = printId
        printId += 1
        node.children foreach (n => edges += Edge(node, n))
        node.children foreach addChildren
      }

      roots foreach addChildren
      (nodes.sortBy(_.id), edges.sortBy(e => (e.from.id, e.to.id).toString()))
    }

    printId = 0
    val (nodes, edges) = getNodesAndEdges(forest.entries)
    printDot(nodes, edges, name)
  }

  private def printDot(nodes: ArrayBuffer[CallTreeNode], edges: ArrayBuffer[Edge], name: String): Unit = {

    def longMethodName(m: Method) = {
      val clid = if (m.classLoaderSID.nonEmpty) m.classLoaderSID + "\\n" else ""
      val vers = if (m.versioned) "versioned for " + m.versionedFor + "\\n" else ""
      val name = if (m.name.contains("_aj_")) "_aj_" else m.name
      clid + m.declaringType + "\\n" + name + "\\n" + vers + m.sig
    }

    def labels(node: CallTreeNode) = {
      val hits = if (node.initialHits > 0) ", hits=" + node.initialHits else ""
      val followupHits = if (node.followupHits > 0) ", followupHits=" + node.followupHits else ""
      val calls = if (node.callCount > 0) ", calls=" + node.callCount else ""
      s"\"printId=${node.printId}, id=${node.id}\n" + 
        s"${longMethodName(node.method)}\n" +
        s"pos=${node.bcInCaller} ${calls} ${hits} ${followupHits}\n\""
    }

    val outputName = {
      val output = env.valueOfOrElse(OutputName, "jprof")
      s"$output.$printed.$name.gv"
    }
    printed += 1
    Using.resource(TextOutput.fromFile(outputName)) { outDot =>
      outDot.println("digraph G {")

      outDot.println("\tnode [shape = box, style = filled];")
      outDot.println()

      for (m <- nodes) {
        val attr = s"[fillcolor = ${
          if (m.preinlined) "orange" else "lawngreen"
        }]"
        outDot.println(s"\t${labels(m)} $attr;")
      }
      outDot.println()


      for (e <- edges) {
        val style = "bold"
        val color = {
          if (e.to.heuristicHits > 0) "blue"
          else if (e.to.preinlined) "grey"
          else "black"
        }
        val label = " "
        outDot.println(s"\t${labels(e.from)} -> ${labels(e.to)} [style = $style,  color = $color];")
      }
      outDot.println()

      outDot.println("}")
    }
  }
}
