/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.bytecode.NoPosition
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.MarkedRegions
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.*
import com.huawei.excelsior.jet.compiler.options.StrOption.*
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Numbering
import com.huawei.excelsior.jet.util.WhileChanged.*
import com.huawei.excelsior.jet.util.graph.ordering.TopSort
import xscala.io.TextOutput
import xscala.properties.Properties

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Various analyses of CFG lies here.
  *
  * @author paul
  */
trait CFGAnalysis extends Scales {
  self: Universe =>

  def iterateWhileChanged[N <: ControlNode](order: Iterable[N], transfer: N => Boolean) = {
    whileChanged { changed =>
      for (b <- order) if (transfer(b)) changed()
    }
  }

  /**
    * Calculates the cold property of the CFG blocks, spreading the cold property up and down by control flow.
    * When spreading forward, all blocks dominated by cold blocks (including loops) become cold.
    * On the other hand, spreading cold property backward stops on loop borders.
    */
  def findColdBlocks(): Sets[Block]#QSet = {
    
    // Here we use splitUCFG to differ if-else paths and mark backup paths as cold
    val graph = currentScope.splitUCFG
    val coldMarkers = Sets[ControlNode].newQSet
    coldMarkers ++= all[ColdNode].map(_.block)
    coldMarkers ++= all[XBlock]

    // Consider tau-backup-path as coldMarkers iff its weight is small enough or unknown.
    def isColdBackupPath(fastPathWeights: Seq[Int], backupPathWeight: Int) = {
      val totalWeight = fastPathWeights.sum + backupPathWeight
      backupPathWeight.toDouble / totalWeight * 100 <= env.valueOf(PGIColdBackupPathThreshold)
    }

    coldMarkers ++= all[Branch] flatMap {
      case TauBranch(TauInfo.PGO(trueWeights, falseWeight), _, _, backupPath) =>
        Option.when(isColdBackupPath(trueWeights, falseWeight))(backupPath)
      case TauBranch(_, _, _, backupPath) => Some(backupPath)
      case _ => None
    }

    if (coldMarkers.isEmpty) {
      return Sets[Block].newQSet
    }

    val tsOrder = graph.topSort.order
    val tsReverseOrder = graph.topSort.reverse.order

    if (coldMarkers(graph.start)) {
      return Sets[Block].newQSet(collect[Block](tsOrder))
    }

    // Step 1. Optimistic forward data flow analysis.
    // Neutral property is distributed forward unless known coldMarker block is found.
    // Note: after this analysis, all blocks dominated by coldMarkers remain cold (not neutral).
    val neutralNodes = Sets[ControlNode].newQSet
    neutralNodes += graph.start

    def transferNeutralForward(node: ControlNode): Boolean = {
      if (!coldMarkers(node) && !neutralNodes(node)) {
        val preds = graph.preds(node)
        assert(preds.nonEmpty)
        if (preds exists neutralNodes) {
          neutralNodes += node
          return true
        }
      }
      false
    }

    iterateWhileChanged(tsOrder, transferNeutralForward)

    // Step 2. Pessimistic backward data flow analysis.
    // Starting with correct results of the previous analysis, spread the coldMarkers property backward.
    // Note that loops marked as neutral will not be touched due to backward branches.

    val coldNodes = coldMarkers
    coldNodes ++= tsOrder filterNot neutralNodes

    def transferColdBackward(node: ControlNode): Boolean = {
      if (!coldNodes(node)) {
        assert(!coldMarkers(node))
        val succs = graph.succs(node)
        if (succs.nonEmpty && (succs forall coldNodes)) {
          coldNodes += node
          return true
        }
      }
      false
    }

    iterateWhileChanged(tsReverseOrder, transferColdBackward)

    // Collect results.
    Sets[Block].newQSet(collect[Block](coldNodes))
  }


  private case class AnalysisResults(
                                      coldBlocks: Sets[Block]#QSet,
                                      warmBlocks: Sets[Block]#QSet,
                                      hotBlocks: Sets[Block]#QSet
                                    )

  def allowWarmBlocks = CompilerPhase.PostInline < currentPhase && currentPhase < CompilerPhase.Lowering

  def verbose = env.enabled(VerboseHotnessAnalysis)

  private def analyzeBlocks(): AnalysisResults = {
    val staticColdBlocks = findColdBlocks()

    if (staticColdBlocks(entryBlock)) {
      return AnalysisResults(staticColdBlocks, Sets[Block].newQSet, Sets[Block].newQSet)
    }

    val staticCold = Sets[Block].newQSet(staticColdBlocks)

    // Here we use CFG as we mark, iterate and analyse blocks
    val graph = currentScope.cfg

    val pgoColdMarker = Sets[Block].newQSet(staticColdBlocks)
    val pgoHotMarker = Sets[Block].newQSet

    val ret = Return.unique map (_.block)

    def returns(node: Block): Boolean = ret contains node

    pgoHotMarker += graph.start
    if (!(ret exists staticColdBlocks)) {
      pgoHotMarker ++= ret
    }

    def addPGOHotMarker(block: Block) = {
      if (!staticCold(block)) {
        pgoHotMarker += block
        pgoColdMarker -= block
      }
    }

    def markHotness(n: ControlNode): Unit = {
      profile.getHotness(n) match {
        case MarkedRegions.Hotness.Hot =>
          addPGOHotMarker(n.block)
        case MarkedRegions.Hotness.Cold =>
          pgoColdMarker += n.block
          assert(!pgoHotMarker(n.block))
        case MarkedRegions.Hotness.Unknown =>
      }
    }

    for (n <- all[ControlNode] if n.pos != NoPosition && n.block != null && !pgoHotMarker(n.block))
      markHotness(n)

    def dgiForWarmCodeMarkUp: DGIProvider = {
      DGIProvider { b =>
        if (b.isCold) {
          DGI("cold mark", "dodgerblue")
        } else if (pgoColdMarker(b) && !staticColdBlocks(b)) {
          DGI("pgo cold", "deepskyblue")
        } else if (pgoHotMarker(b)) {
          DGI("hot", "red")
        } else {
          null
        }
      }
    }

    if (verbose) {
      dbgPrinter.debugNodes("hotness analysis", n => s"\t| ${n.pos}")
      dbgPrinter.debugGraphs("graph pgo hotness mark up", printNodesGraph = false, info = dgiForWarmCodeMarkUp)
    }

    val tsOrder = graph.topSort.order
    val tsReverseOrder = graph.topSort.reverse.order

    // Mark loop headers and backward branches as PGO hot.
    val endCycle = mutable.LinkedHashSet.empty[Block]
    val ts = graph.topSort
    for (end <- tsOrder; start <- graph.succs(end) if ts.lteq(start, end)) {
      addPGOHotMarker(start)
      addPGOHotMarker(end)
      endCycle += end
    }

    if (verbose) {
      dbgPrinter.debugGraphs("graph mark up with cycles", printNodesGraph = false, info = dgiForWarmCodeMarkUp)
    }

    val pgoCold = Sets[Block].newQSet(pgoColdMarker)
    val pgoHot = Sets[Block].newQSet(pgoHotMarker)
    pgoHot ++= tsOrder filterNot pgoCold

    def dgiForWarmCodePropagation: DGIProvider = {
      DGIProvider { b =>
        if (b.isCold) {
          DGI("cold", "dodgerblue")
        } else if (pgoColdMarker(b)) {
          DGI("cold", "dodgerblue")
        } else if (pgoHotMarker(b)) {
          DGI("hot", "red")
        } else if (pgoHot(b)) {
          DGI("hot", "orange")
        } else {
          null
        }
      }
    }

    def transferPGOColdForward(block: Block): Boolean = {
      if (!pgoCold(block) && !pgoHotMarker(block) && !returns(block)) {
        val preds = graph.preds(block)
        if (preds.nonEmpty && (preds forall pgoCold)) {
          pgoHot -= block
          pgoCold += block
          return true
        }
      }
      false
    }

    iterateWhileChanged(tsOrder, transferPGOColdForward)

    if (verbose) {
      dbgPrinter.debugGraphs("temperature graph after forward propagation", printNodesGraph = false, info = dgiForWarmCodePropagation)
    }

    def transferPGOColdBackward(block: Block): Boolean = {
      if (!pgoCold(block) && !pgoHotMarker(block)) {
        val succs = graph.succs(block)
        if (succs.nonEmpty && (succs forall pgoCold)) {
          pgoHot -= block
          pgoCold += block
          return true
        }
      }
      false
    }

    iterateWhileChanged(tsReverseOrder, transferPGOColdBackward)

    if (verbose) {
      dbgPrinter.debugGraphs("temperature graph after backward propagation", printNodesGraph = false, info = dgiForWarmCodePropagation)
    }

    def ensurePGOHot(blocks: Iterator[Block]): Boolean = {
      var changed = false
      for (n <- blocks if !staticCold(n) && !pgoHot(n)) {
        pgoCold -= n
        pgoHot += n
        changed = true
      }
      changed
    }

    /** Fold PGOHot property forward through CFG. If a block is PGOHot but all its predecessors are PGOCold this means
      * the inaccuracy. A PGOHot block should have at least one PGOHot predecessor different from the given block and
      * not the loop exit. In case it has not a PGOHot predecessor mark all its predecessors PGOHot
      * (a conservative decision, because we do not know the precise information).
      * Any attempt to mark not all ancestors can lead to performance degradation.
      * If there is no exact information, then it is better to make all predecessors conservatively PGOHot.
      */
    def transferPGOHotForward(block: Block): Boolean = {
      def hasHotPred(x: Block) = graph.preds(x) exists (p => pgoHot(p) && p != block && !endCycle(p))

      pgoHot(block) && graph.preds(block).nonEmpty && !hasHotPred(block) && ensurePGOHot(graph.preds(block))
    }

    iterateWhileChanged(tsOrder, transferPGOHotForward)

    /*
     *  Fold PGOHot property backward through CFG. Each PGOHot block either doesn’t have any successors or
     *  it should have at least one PGOHot block among them. Otherwise, make conservatively all its successors PGOHot.
     */
    def transferPGOHotBackward(block: Block): Boolean = {
      def hasHotSucc(x: Block) = graph.succs(x) exists (s => pgoHot(s) && s != block)

      pgoHot(block) && graph.succs(block).nonEmpty && !hasHotSucc(block) && ensurePGOHot(graph.succs(block))
    }

    iterateWhileChanged(tsReverseOrder, transferPGOHotBackward)

    if (verbose) {
      dbgPrinter.debugGraphs("Final temperature graph", printNodesGraph = false, info = dgiForWarmCodePropagation)
    }

    pgoCold --= staticCold
    require(!(ret exists pgoCold))
    assert(!pgoCold(graph.start))
    assert(!(pgoHot exists staticCold))

    AnalysisResults(staticColdBlocks, pgoCold, pgoHotMarker)
  }

  def findWarmAndColdBlocks(): Sets[Block]#QSet = {
    if (currentPhase == CompilerPhase.PGOStaticAnalysis || (allowWarmBlocks && env.enabled(PGOCodeLayoutOptimization) && profile.isPGOHost)) {
      val results = analyzeBlocks()
      Sets[Block].newQSet(results.warmBlocks ++ results.coldBlocks)
    } else {
      findColdBlocks()
    }
  }

  def findHotCalls(): AbstractCall => Boolean = {
    if (
      currentPhase == CompilerPhase.PGOStaticAnalysis &&
        env.enabled(JProfWarmUpCallSitesOnHotPaths) &&
        (!env.enabled(MarkedRegionHotnessIsLocal) || env.enabled(AllowLocalMarkedRegionHotnessInInlinePlanning))
    ) {
      val hot = analyzeBlocks().hotBlocks
      c => hot(c.block)
    } else {
      Set.empty
    }
  }

  def markWarmBlocks(): Unit = {
    dbgPrinter.debugGraphs("temperature graph before marking warm code", printNodesGraph = false, info = dgiForColdCode)
    assert(allowWarmBlocks)
    if (!env.enabled(PGOCodeLayoutOptimization) || !profile.isPGOHost) {
      return
    }

    val results = analyzeBlocks()
    genUnitTest(results)

    results.warmBlocks foreach (_.markAsWarm())
    dbgPrinter.debugNodes("nodes after marking warm code")
  }

  def markColdBlocks(): Unit = {
    val cold = findColdBlocks()
    // materialize coldness information about tau-backup paths
    // see [[CFGAnalysis.findColdBlocks]]
    for (TauBranch(_, _, _, backupPath) <- all[Branch] if cold(backupPath.target)) {
      assert(cold(backupPath.target))
    }
  }

  def dgiForColdCode: DGIProvider = {
    lazy val cold = findColdBlocks()
    lazy val warmOrCold = findWarmAndColdBlocks()
    DGIProvider { b =>
      if (b.isCold) {
        DGI("cold source", "dodgerblue")
      } else if (cold(b)) {
        DGI("cold", "deepskyblue")
      } else if (warmOrCold(b)) {
        DGI("warm", "lightblue")
      } else {
        null
      }
    }
  }

  private def genUnitTest(results: AnalysisResults): Unit = {
    if (!env.enabled(GenerateWarmPGOAnalysisCFG)) {
      return
    }

    val blocks = Numbering(all[Block].toArray.sortBy(_.id))

    val sb = new StringBuilder
    val path = Properties.get.userDir()
    sb.append("\n  test(\"" + path + "." + rootMethod.getFullName + "\") {\n")
    sb.append("    check(seq(")

    writeGraphWithoutXEdges(blocks, sb)
    sb.append("),\n      ")

    val warmBlockIDs = blocks.order filter results.warmBlocks map blocks.number
    sb.append(warmBlockIDs mkString ", ")
    sb.append("\n    )\n")
    sb.append("  }\n ")

    GenTestOutput.print(env, sb.toString())
  }

  private def writeGraphWithoutXEdges(blocks: Numbering[Block], sb: mutable.StringBuilder): Unit = {

    val cold = Sets[Block].newQSet
    val hot = Sets[Block].newQSet
    val ret = Return.unique map (_.block)

    def returns(b: Block) = ret contains b

    def markHotness(n: Node): Unit = {
      profile.getHotness(n) match {
        case MarkedRegions.Hotness.Hot =>
          hot += n.block
        case MarkedRegions.Hotness.Cold =>
          cold += n.block
        case MarkedRegions.Hotness.Unknown =>
      }
    }

    for (n <- all[ControlNode] if n.pos != NoPosition && n.block != null && !hot(n.block))
      markHotness(n)

    def getAttributes(b: Block): String = {
      val attrs = ArrayBuffer.empty[String]
      if (b.isCold) {
        attrs += "coldcode"
      }
      if (hot(b)) {
        attrs += "hotcall"
      }
      if (cold(b)) {
        attrs += "coldcall"
      }
      if (b.succBlocks.isEmpty && !returns(b)) {
        attrs += "halt"
      }

      attrs map (a => "\"" + a + "()\"") mkString ", "
    }

    val edges = for (b <- all[Block]; s <- b.succBlocks) yield (b, s)
    val hasEdges = edges.nonEmpty
    sb.append(edges map (e => s"${blocks.number(e._1)} -> ${blocks.number(e._2)}") mkString ", ")

    if (hasEdges) {
      sb.append(",")
    }
    sb.append("\n      ")

    val attrs = for (b <- blocks.order; a = getAttributes(b); if a.nonEmpty) yield s"${blocks.number(b)}@@($a)"
    sb.append(attrs mkString ", ")
  }
}

object GenTestOutput {

  var stream: TextOutput = _

  def print(env: Environment, s: String): Unit = {
    if (stream == null) {
      val name = env.valueOf(OutputName) + ".unittest"
      stream = TextOutput.fromFile(name)
    }
    stream.print(s)
    stream.flush()
  }
}