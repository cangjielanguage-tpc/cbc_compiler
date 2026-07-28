/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.jet.assembler.Label
import com.huawei.excelsior.jet.compiler.ir.MarkedRegion
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenerateMarkedRegions
import com.huawei.excelsior.jet.util.ScalaCollections.{groupBy, mapWith, toMultiMap}
import com.huawei.excelsior.jet.compiler.util.{Log, Maps}
import com.huawei.excelsior.jet.util.graph.*

import scala.collection.mutable.ArrayBuffer

/** Profiling regions calculator.
  *
  * @author conwor
  */
trait ProfilingRegions { self: Universe with BackEnd =>

  /** Returns map from blocks of `graph` to region id (abstract numeration with -1 as a special region).
    *
    * Each region is a subset of blocks which belongs to the same loop and contains some markers which
    * form a chain in both dominance and post-dominance trees. Block belongs to region if region contains
    * marker which is a MSDPD of this block.
    *
    * MSDPD (most specific dominator/post-dominator) of `b` is the closest marker which dominates or
    * post-dominates `b` and belongs to the same or inner loop of `b`.
    *
    * Region has the following semantic: one hit to any block of the region means that all markers from
    * this region should be counted by profiler.
    */
  protected def findProfilingRegions(graph: BiGraph[Block], markers: Seq[Node]): collection.Map[Block, Int] = {

    val markerOf = markers.iterator.map(m => (m.block, m)).toMap
    val doms = graph.dominators
    val postDoms = PostDominators.augmented(graph)
    val loops = graph.loops

    /** Selects most specific marker. */
    def selectMSDPD(b: Block): Option[Node] = {
      val loop = loops.loopOf(b)

      /** Find closest dominating or post-dominating marker for block. */
      def findMarker(blocks: Iterator[Block]): Option[Node] = {
        def loopEqOrInner(x: Loop[Block], y: Loop[Block]): Boolean =
          (x == y) || ((x != null) && x.isInnerOf(y))

        (blocks find { b => markerOf.contains(b) && loopEqOrInner(loops.loopOf(b), loop) }) flatMap markerOf.get
      }

      (findMarker(doms.doms(b)), findMarker(postDoms.postDoms(b))) match {
        case (Some(um), Some(lm)) =>
          val (ub, lb) = (um.block, lm.block)

          (doms.dominates(ub, lb), postDoms.postDominates(lb, ub)) match {

            case (true, false) =>
              //      UM
              //     /  \
              //    B
              //    |
              //    LM
              //
              // Marker in post-dominator is more specific.
              Some(lm)

            case (false, true) =>
              //      UM
              //      |
              //      B
              //       \  /
              //        LM
              //
              // Marker in dominator is more specific.
              Some(um)

            case (true, true) =>
              //      UM
              //      |
              //      B
              //      |
              //      LM
              //
              // Both markers are equally specific and will likely end up in the same region.
              // TODO: think about situation when there is loop border between UM & LM
              Some(um)

            case (false, false) =>
              //      UM
              //     /  \
              //         B
              //          \  /
              //           LM
              //
              // Both markers are equally specific but will not end up in the same region. It will be nice to
              // associate B with both of them (i.e. hit in B means profiler increment of UM and LM by 0.5),
              // but this is not supported now. Select any one of them.
              Some(um)
          }

        case (um, lm) => um orElse lm
      }
    }

    // Each marker M forms a region of blocks whose MSDPD is M. Several such regions may be merged into one.
    // Regions of markers A and B are merged when A dominates B, B post-dominates A and they are in the same loop.

    var lastId = -1
    val regionOfMarker = Maps[Node].newQMap[Int]

    def regionOf(marker: Node): Int = regionOfMarker.getOrElseUpdate(marker, {
      val b = marker.block
      val found = doms.strictDoms(b) find { d =>
        markerOf.contains(d) && postDoms.postDominates(b, d) && loops.inSameLoop(b, d) }
      found match {
        case Some(d) => regionOf(d)
        case _ => lastId += 1; lastId
      }
    })

    mapWith(graph.collectReachableFrom(graph.start)) { b => selectMSDPD(b) map regionOf getOrElse -1 }
  }


  /////////////////////////////////////////////////////////////////////////////

  /** Part of [[CodeGeneratorImpl]], responsible for profiling regions calculation and verification. */
  trait ProfilingRegionsGenerator { self: CodeGeneratorImpl =>

    private[codegen] final def isRegionMarker(node: Node) = {
      env.enabled(GenerateMarkedRegions) &&
        needXSite(node) &&
        !cold(node.block) && // we don't need any markers in cold code
        node.isInstanceOf[Call] // deferred calls also counts
    }

    private def verifyRegionMarkers(regions: collection.Map[Block, Int], markers: Seq[Call]): Unit = {
      def regionOf(n: Node) = regions.getOrElse(n.block, -1)

      val log = Log(Log.Kind.DuplicatePositionMarkers)
      if (log.isEnabled) {
        log.inSession(s"code unit $codeUnit") {
          val possiblyConflictingMarkers = groupBy(markers)(_.pos)

          val conflictingMarkers = possiblyConflictingMarkers.view
            .mapValues { groupBy(_)(regionOf) }
            .filter { case (_, regionMarkers) => regionMarkers.size > 1 }
            .toMap

          for ((pos, regionMarkers) <- conflictingMarkers) {
            log(s"- following markers have the same pos ($pos), but are covered by different regions:")
            for ((regionID, markers) <- regionMarkers) {
              log(s"    region $regionID markers:")
              for (m <- markers) {
                log(s"      $m")
              }
            }
          }
        }
      }
    }

    private[codegen] final def calculateMarkedRegions(methodStart: Label, methodEnd: Label, order: Seq[Block]): Seq[MarkedRegion] = {
      if (!env.enabled(GenerateMarkedRegions)) return Seq.empty

      if (cold(entryBlock)) return Seq.empty

      val graph = cfg filterNot cold
      val markers = all[Call].filter(isRegionMarker).toSeq
      val regions = findProfilingRegions(graph, markers)

      verifyRegionMarkers(regions, markers)

      // regions calculated for graph.reachable() which may be subset of `order`
      def regionOf(b: Block) = regions.getOrElse(b, -1)

      val result = ArrayBuffer.empty[MarkedRegion]
      var currentRegion = regionOf(order.head)
      var start: Label = methodStart
      var end: Label = null

      for (block <- order) {
        val region = regionOf(block)
        if (region != currentRegion) {
          if (currentRegion != -1) {
            result += MarkedRegion(currentRegion, start.position, end.position)
          }
          currentRegion = region
          start = startOf(block)
        }
        end = endOf(block)
      }

      if (currentRegion != -1) {
        if (methodEnd != null) end = methodEnd
        result += MarkedRegion(currentRegion, start.position, end.position)
      }

      val log = Log(Log.Kind.MarkedRegions)
      if (log.isEnabled) {
        val blocks = toMultiMap(regions.iterator map { case (b, id) => (id, b) })
        log.inSession(s"code unit $codeUnit") {
          for (r <- result.toSeq.sortBy(r => (r.id, r.startOffset))) {
            val bids = for {
              b <- blocks(r.id)
              if r.startOffset <= startOf(b).position && endOf(b).position <= r.endOffset
            } yield b.id

            log(s"- region ${r.id} [0x${r.startOffset.toHexString}, 0x${r.endOffset.toHexString}) @ ${bids mkString ", "}")
          }
        }
      }

      result.toSeq
    }
  }
}
