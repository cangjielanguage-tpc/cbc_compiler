/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.Stage.GCPointsInserting
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.collection.mutable

/**
 * Graph marking and GCPoints inserting for precise GC.
 *
 * @author dbg
 * @author ikireev
 */
trait GCPointsInserting extends CountedLoopsRecognizer { self: Universe with IRTransformationsCollection =>

  /**
   * GCPointsExpectance is a property of block that means there is a GCPoint in it or not.
   * Actual - block contains proved GCPoint.
   * Soon - all control paths down on the CFG has (or will have) proved GCPoint.
   * NotSoon - one of control paths down on the CFG hasn't proved GCPoint.
   */
  enum GCPointExpectance {
    case Soon, NotSoon, Actual
  }
  import GCPointExpectance._

  /** Inserts GCPoint node before the given blockEnd. */
  private def insertGCPoint(blockEnd: BlockEnd): Unit = insertCodeBefore(blockEnd) { GCPoint() }

  /** Marks GCPoints expectance over CFG. */
  private def makeGCPointsExpectanceMap(ts: TopSort[Block], blockInCountedLoop: Block => Boolean, innerMethod: Block => Method) = {
    val gcPointsExpectance = mutable.LinkedHashMap.empty[Block, GCPointExpectance]

    def containsGuaranteedGCPoint(node: Node): Boolean = node match {
      case _: Return if rootMethod.shouldContainGCPoints => true
      case DAICall(_) => false
      case call: Call if call.targetRef.hasMethod =>
        val m = call.targetRef.method
        m.shouldContainGCPointInEpilogue || m.isAJRTAllocator || m.shouldContainGCPointBeforeResultTransfer
      case _ => false
    }

    ts.order.reverse.foreach { b => gcPointsExpectance(b) = {
      def gt(x: Block) = ts.gt(x, b)
      def gteq(x: Block) = ts.gteq(x, b)
      def lteq(x: Block) = ts.lteq(x, b)

      val inCounted = blockInCountedLoop(b)

      if (b.points exists containsGuaranteedGCPoint) {
        Actual // Return & Call

      } else if (!innerMethod(b).shouldContainGCPoints) {
        Soon // Block ended in NoGC region (@NoLocalGCPoints or unmanaged method)

      } else if (!inCounted && b.isInstanceOf[BBlock] && (b.predBlocks exists gteq)) {
        assert(b.succBlocks forall gt) // not a self loop
        Soon // LoopHeader of not counted loop

      } else if (inCounted && (b.succBlocks forall lteq)) {
        Soon // BackBranch of counted loop

      } else if (!inCounted && (b.succBlocks exists lteq)) {
        NotSoon // BackBranch of not counted loop

      } else {
        if ((b.succBlocks filter gt).map(gcPointsExpectance).forall(_ == NotSoon)) NotSoon else Soon
      }
    }}
    gcPointsExpectance
  }

  /** Places GCPoints where they are needed. */
  def addGCPoints(): Unit = stage(GCPointsInserting) {
    if (!rootMethod.hasManagedExecEnv) {
      return
    }

    if (rootMethod.shouldContainGCPointInEpilogueBeforeFrameDrop) {
      Return.unique foreach insertGCPoint
    }

    val differentICRegions = irHasDifferentICRegions()

    if (rootMethod.shouldContainGCPoints || differentICRegions) {
      splitCriticalEdges(withXHandlers = true)
      // GC-Points inserting algorithm requires that all loops have at least two blocks
      splitInfiniteSelfLoops()

      val ts = cfg.topSort

      if (cfg.hasBackwardEdges) {
        val loops = cfg.loops
        val counted = detectCountedLoops(loops, collectStats = true).toSet
        def blockInCountedLoop(b: Block) = {
          val loop = loops.loopOf(b)
          (loop != null) && counted(loop)
        }

        val innerMethod = if (differentICRegions) {
          calcICRegionsMap()
        } else {
          Function.const(rootMethod) _
        }

        val gcPointsExpectance = makeGCPointsExpectanceMap(ts, blockInCountedLoop, innerMethod)

        dbgPrinter.debugCFG("CFG after gc-points marking", { _.block match {
          case x if isUnreachableBar(x) => ""
          case x => gcPointsExpectance(x).toString
        }})

        val gcPoints = gcPointsExpectance collect {
          case (b: BBlock, NotSoon) if b.predBlocks.exists(gcPointsExpectance(_) == Soon) => b
        }

        gcPoints foreach { x => insertGCPoint(x.blockEnd) }
      }

      EmptyBlocksElimination()
    }

    dbgPrinter.debugNodes("after gc-points inserting")
  }
}
