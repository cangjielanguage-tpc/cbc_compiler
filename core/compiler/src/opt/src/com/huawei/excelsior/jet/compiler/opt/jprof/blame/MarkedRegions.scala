/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame

import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.jprof.JProfManager
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.{CallGraph, InlineList, JProf, Method}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{MarkedRegionHotnessIsLocal, UseMarkedRegions}
import com.huawei.excelsior.jet.compiler.options.NumOption.{MarkedRegionsGlobalThresholdPPM, MarkedRegionsLocalThresholdPermille}
import com.huawei.excelsior.jet.compiler.options.StrOption.OutputName
import com.huawei.excelsior.jet.compiler.symlevel.Method as SymMethod
import com.huawei.excelsior.jet.util.ScalaCollections.{groupBy, groupMapReduce, sumBy}
import xscala.io.TextOutput

import scala.collection.mutable
import scala.util.Using

/** Process contents of marked regions entries of jprof
  * to tell whether certain [[Position]]s in compiled code are hot or not.
  *
  * @author ijorch
  */
object MarkedRegions {

  enum Hotness {
    case Unknown, Hot, Cold
  }

  def hotness(pos: Position, inlineRoot: Option[SymMethod]): Hotness = {
    val il = InlineList(pos)
    assert(!il.reversed)
    if (inlineRoot.isDefined) {
      hotness(il, allowLocal = true, subgraphHits.get(Method.fromSymlevel(inlineRoot.get)))
    } else {
      hotness(il, allowLocal = false, None)
    }
  }

  private[blame] def hotness(il: InlineList, allowLocal: Boolean, subGraph: Option[Int] = None): Hotness = {
    if (!env.enabled(UseMarkedRegions)) {
      return Hotness.Unknown
    }

    val hot = if (allowLocal && env.enabled(MarkedRegionHotnessIsLocal)) {
      calcLocalHotness(il) map (_ >= localThreshold)
    } else {
      subGraph match {
        case Some(subgraphHits) =>
          calcGlobalHotness(il, subgraphHits) map (_ >= subgraphThreshold)
        case None =>
          calcGlobalHotness(il, markedRegionsHits) map (_ >= globalThreshold)
      }
    }

    hot match {
      case None => Hotness.Unknown
      case Some(true) => Hotness.Hot
      case Some(false) => Hotness.Cold
    }
  }

  /** Searches for marker that could receive a hit at given inline-list.
    * Such markers are those that have inlineList which is a prefix of `il`.
    * <p>
    * There can be several such markers, but we are interested only in those with the longest inline list
    * as all others are most probably correspond to the calls from unoptimized (baselined/interpreted) code.
    * It is the case because the only reason for inline list of a marker to be non-unitary is static inline,
    * which is considered to be insensitive to call context.
    *
    * TODO: assert that all markers with shorter inline list than the longest one have a single-element inline list.
    */
  private[blame] def correspondingMarker(il: InlineList): Option[(Marker, Int)] = {
    assert(!il.reversed)
    val correspondingMarkers = groupedMarkedRegions.get(il.entries.head)
      .iterator flatMap (_ filter (_._1.inlineList isPrefix il))

    correspondingMarkers maxByOption { case (m, _) => m.inlineList.length }
  }

  /** If `il` can be divided into subsequences for which there are markers in profile,
    * then its local hotness if defined as a product of ratios of each marker hits to the hits into respective host;
    * otherwise local hotness is undefined.
    */
  private def calcLocalHotness(il: InlineList): Option[Double] = if (il.nonEmpty) {
    correspondingMarker(il) flatMap { case (marker, hits) =>
      calcLocalHotness(il.drop(marker.inlineList.length)) map (_ * ratio(hits, marker.host.profileInfo.totalHits))
    }
  } else {
    Some(1)
  }

  /** If `il` starts from position of existing marker,
    * then its global hotness is ratio of that marker's hits to `totalHits`;
    * otherwise global hotness is undefined.
    */
  private def calcGlobalHotness(il: InlineList, totalHits: Int): Option[Double] = {
    correspondingMarker(il) map { case (_, hits) => ratio(hits, totalHits) }
  }

  private def ratio(hits: Int, totalHits: Int) = hits / totalHits.toDouble
  private def localThreshold = env.valueOf(MarkedRegionsLocalThresholdPermille) / 1000.0
  private def globalThreshold = env.valueOf(MarkedRegionsGlobalThresholdPPM) / 1000000.0
  private def subgraphThreshold = globalThreshold // TODO: consider introducing yet another equation

  private[blame] def printAsPlainText(): Unit = {
    val oname = env.valueOfOrElse(OutputName, "inline")
    Using.resource(TextOutput.fromFile(s"$oname.regions")) { out =>
      markedRegions.foreach(x => out.println(x.toString))
    }
    Using.resource(TextOutput.fromFile(s"$oname.grouped.regions")) { out =>
      groupedMarkedRegions.foreach(x => out.println(x.toString))
    }
  }

  private lazy val markedRegions: collection.Map[Marker, Int] = {
    assert(Blame.profileGraph != null) // force profile graph parsing, as it will also properly accumulate all method's hits

    def readRegions(jprof: JProfManager) = JProf.parseBlameSection(jprof, Iterable.empty, JProf.markedRegions)

    JProf.handleMultipleJProfs {
      readRegions(JProfManager.main) to mutable.SeqMap
    } { (acc, jprof) =>
      for ((m, newHits) <- readRegions(jprof)) {
        acc.updateWith(m) {
          case None => Some(newHits)
          case Some(oldHits) => Some(oldHits + newHits)
        }
      }
      acc
    } ensuring verifyRegions _
  }
  private lazy val groupedMarkedRegions = groupBy(markedRegions)(_._1.inlineList.entries.head)

  private lazy val hitsIntoAllMethods = hitsIntoMethods(markedRegions)
  private lazy val markedRegionsHits = sumBy(hitsIntoAllMethods)(_._2)

  private lazy val subgraphHits = {
    val plan = Blame.inlinePlan
    val cropBy = plan.truePGOHostSet
    plan.pgoHostSet.iterator.map { r =>
      val sg = Blame.profileGraph.croppedSubgraph(r, cropBy)
      (r, hits(sg))
    }.toMap
  }

  private[blame] def hits(sg: CallGraph) = {
    sumBy(hitsIntoAllMethods.iterator.filter(sg contains _._1))(_._2)
  }

  private def hitsIntoMethods(regions: Iterable[(Marker, Int)]): Iterable[(Method, Int)] = {
    val byID = groupMapReduce(regions) { case (m, _) => (m.host, m.regionID) } (x => x) { case ((m1, h1), (m2, h2)) =>
      assert(h1 == h2, s"PGO Error: different number of hits ($h1; $h2) into markers ($m1; $m2) covered by same marked region")
      (m1, h1)
    }
    groupMapReduce(byID.valuesIterator) { case (m, _) => m.host } { case (_, hits) => hits } (_ + _)
  }

  /** Verify internal invariants of each region. */
  private[blame] def verifyMarkers(regions: Iterable[(Marker, Int)]): Unit = {
    hitsIntoMethods(regions) // all required verification happens inside
  }

  /** Verify given regions against profile info already read from JProfs. */
  private[blame] def verifyRegions(regions: Iterable[(Marker, Int)]): Boolean = {
    for ((host, hits) <- hitsIntoMethods(regions)) {
      assert(hits <= host.profileInfo.totalHits,
        s"PGO Error: there are more hits into $host's marked regions ($hits) than total hits into that method (${host.profileInfo.totalHits})")
    }
    true
  }

  private[blame] case class Marker(host: Method, regionID: Int, inlineList: InlineList) {
    require(inlineList.nonEmpty) // contains at least real (non-inlined) method which received hit
    require(!inlineList.reversed) // and that method is last in list

    def bcPos = inlineList.entries.head.bcPosInMethod
  }
}
