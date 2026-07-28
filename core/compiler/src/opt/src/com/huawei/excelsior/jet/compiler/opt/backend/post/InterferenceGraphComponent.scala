/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.post

import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Universe}
import com.huawei.excelsior.jet.util.WhileChanged.whileChanged
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.Worklist
import com.huawei.excelsior.jet.util.graph.MergeGraph

import scala.collection.mutable

/** Interference graph of vertices (ranges and resources).
  *
  * Works by Ershov algorithm (TODO: link article), starting from complete subgraph,
  * merging 2-x neighbours, until there are no vertices to merge. Than if there are
  * some vertices remain unmerged, append one of them to complete subgraph and go on.
  *
  * For frame slots recoloring task, complete subgraph is graph of FRegs and other
  * vertices are spilled live ranges.
  *
  * @author conwor
  */
trait InterferenceGraphComponent { self: Universe with LiveRangesComponent =>

  implicit object IGVertexSetsAndMaps extends Sets.Default[IGVertex] with Maps.Default[IGVertex]
  implicit object RangeVertexSetsAndMaps extends Sets.Default[RangeVertex] with Maps.Default[RangeVertex]

  /** Vertex of base interference graph. */
  abstract class IGVertex extends MergeGraph.Node[IGVertex]

  /** Vertex of live range. */
  case class RangeVertex(range: LiveRange) extends IGVertex {
    override def toString = "range_vertex_" + range.values.next().id
  }

  /** Vertex of resource. */
  case class ResourceVertex(resource: Resource) extends IGVertex {
    override def toString = "resource_vertex_" + resource
  }

  class InterferenceGraph(nodes: IterableOnce[Node]) extends MergeGraph[IGVertex] {

    val completeSubGraph: Sets[IGVertex]#QSet = Sets[IGVertex].newQSet
    val unmerged: Sets[RangeVertex]#QSet = Sets[RangeVertex].newQSet
    appendRanges(nodes)

    private def appendRanges(nodes: IterableOnce[Node]): Unit = {
      val ws = Worklist.from(nodes)
      for (node <- ws.drain) {
        val range = LiveRanges.web(node)
        val vertex = RangeVertex(range)
        connect(vertex, unmerged.iterator filter { _.range intersects range })

        def frameSlotSize(range: LiveRange) = range.resource match {
          case fs: FrameSlot => fs.size
          case _ => 0
        }

        // Workaround for JET-12663
        if (range.mayBeTraceableRef) {
          connect(vertex, unmerged.iterator filter { rv => frameSlotSize(rv.range) > TRefType.size })

        } else if (frameSlotSize(range) > TRefType.size) {
          connect(vertex, unmerged.iterator filter { _.range.mayBeTraceableRef })
        }

        unmerged += vertex
        ws --= range.values
      }
    }

    def appendResources(resources: Iterable[Resource], isMergeableWithResources: RangeVertex => Boolean): Unit = {
      if (resources.isEmpty) return

      val (mergeable, unmergeable) = unmerged partition isMergeableWithResources
      if (mergeable.isEmpty) return

      // Create vertices for all resources and edges between all of them and not mergeable ranges
      val resourceVertices = Maps[Resource].newQMap[ResourceVertex]
      for (resource <- resources) {
        val v = ResourceVertex(resource)
        connect(v, resourceVertices.valuesIterator)
        connect(v, unmergeable)
        resourceVertices(resource) = v
        completeSubGraph += v
      }

      // Iterate all nodes, their results and spoils and create edges to mergeable ranges
      for (node <- allNodes) {
        if (node.mayHaveResource) {
          for (resultVertex <- resourceVertices.get(node.resource)) {
            val nodeRange = LiveRanges.web(node)
            connect(resultVertex, mergeable.iterator filter { _.range intersects nodeRange })
          }
        }

        if (node.mayHaveSpoiled) {
          for (spoil <- node.spoiled; spoilVertex <- resourceVertices.get(spoil)) {
            connect(spoilVertex, mergeable.iterator filter { _.range contains node })
          }
        }
      }
    }

    def simplify(): Unit = {
      while (unmerged.nonEmpty) {
        // 1. Merge all 2-neighbours to already complete graph.
        whileChanged { changed =>
          for (v <- completeSubGraph) focused(v) { f =>
            for (n2 @ RangeVertex(_) <- f.neighbours2) {
              f.merge(n2)
              unmerged -= n2
              changed()
            }
          }
        }

        if (unmerged.nonEmpty) {
          val v = unmerged.head
          if (completeSubGraph.isEmpty || (neighbours(v) exists completeSubGraph.contains)) {
            // 2. There is an unmerged vertex which connected to everyone in completeSubGraph.
            //    Enlarge completeSubgraph by adding such vertex to it.
            checkConsistency(CheckLevels.Optional) {
              val ns = neighboursSet(v)
              completeSubGraph forall ns
            }
            completeSubGraph += v
          } else {
            // 3. There is an unmerged vertex which resides in another connected component.
            // Merge it to first vertex of completeSubgraph
            // (if completeSubgraph contains any register-located vertices, first one is one of them)
            merge(completeSubGraph.head, v)
          }
          unmerged -= v
        }
      }

      checkConsistency(CheckLevels.Optional) {
        completeSubGraph forall { v1 =>
          val ns = neighboursSet(v1)
          completeSubGraph forall { v2 => (v1 == v2) || (ns contains v2)}
        }
      }
    }
  }

  object FastPaths {

    private def filterResources(range: LiveRange, resources: Iterable[Resource]): MutableResourceSet = {
      val resourcesSet = mutableSetOf(resources)

      for (node <- allNodes) {
        if (node.mayHaveResource && resourcesSet(node.resource)) {
          val nodeRange = LiveRanges.web(node)
          if (range intersects nodeRange) {
            resourcesSet -= node.resource
            if (resourcesSet.isEmpty) return resourcesSet
          }
        }

        if (node.mayHaveSpoiled) {
          lazy val nodeContained = range contains node
          for (spoil <- node.spoiled) {
            if (resourcesSet(spoil) && nodeContained) {
              resourcesSet -= spoil
              if (resourcesSet.isEmpty) return resourcesSet
            }
          }
        }
      }

      resourcesSet
    }

    /** Fast path for one range vertex. */
    def oneRange(v: RangeVertex, resources: Iterable[Resource]): Option[Resource] =
      filterResources(v.range, resources).headOption

    /** Fast path for two range vertices. */
    def twoRanges(ig: InterferenceGraph, v1: RangeVertex, v2: RangeVertex, resources: Iterable[Resource]): (Option[Resource], Option[Resource]) = {
      val (s1, s2) = (filterResources(v1.range, resources), filterResources(v2.range, resources))
      if (!ig.adjacent(v1, v2)) {
        (s1.headOption, s2.headOption)
      } else {
        if (!(s1 subsetOf s2)) { s1 &~= s2 }
        val c = s1.headOption
        (c, s2 find (!c.contains(_)))
      }
    }
  }
}
