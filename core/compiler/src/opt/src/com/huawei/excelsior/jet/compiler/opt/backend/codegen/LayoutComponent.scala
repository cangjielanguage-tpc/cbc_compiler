/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.jet.compiler.Stage.CFGLayout
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.util.graph.ordering.NaturalCFGOrder.LoopOrientation.*
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.CheckLevels.Optional
import com.huawei.excelsior.jet.compiler.opt.ir.{CFGAnalysis, CheckLevels, Universe}
import com.huawei.excelsior.jet.compiler.options.NumOption.{LoopBodyAlignment, TableJumpTargetAlignment}
import com.huawei.excelsior.jet.util.ScalaCollections.{groupBy, groupMap}
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.ordering.NaturalCFGOrder
import com.huawei.excelsior.jet.util.graph.*

import scala.PartialFunction.condOpt
import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

/**
 * Calculates layout of CFG.
 *
 * <p>
 * CFG layout is the CFG linearization suitable for code generation.
 * Blocks are ordered so the following conditions hold:
 * <ul>
 *   <li> Body of a loop is placed contiguously.</li>
 *   <li> Loop ends with conditional branch to avoid extra unconditional branches from the loop end to the loop header.</li>
 *   <li> Jumps between blocks are avoided if possible.</li>
 *   <li> Cold blocks (including exception handlers) are placed out of order at the end
 *        of the method code (in the "Siberian" section).</li>
 * </ul>
 * </p>
 *
 * <p>
 *   The following algorithm is used to layout CFG:
 *   <ol>
 *     <li>Layout of loops is calculated separately for each loop. It guarantees that the loop
 *         body is placed contiguously. The bodies of the inner loops are inserted into the outer loop as a whole.
 *         Also, separate calculation of loop layout allows to process properly ordered loop exits.
 *     </li>
 *
 *     <li>The whole CFG is treated as hierarchy of regions. Layout of each region is calculated separately.
 *         There are two top-level regions - hot code and cold code. Each loop forms its own subregion.
 *     </li>
 *
 *     <li>For each region entry point (loop header, method entry block or entries of cold regions)
 *         layout is topsort order (reverse postorder) of a custom CFG subgraph.
 *         Topsort order provide proper placement of CFG triangles (if-then constructs).
 *         <ul><li>
 *         The whole body of immediately nested subregion (inner loop) is represented by single node.
 *         </li><li>
 *         Edges going to inner loop header become edges to this subregion node.
 *         All other edges going into inner loop (they exist only for irreducible loops) are ignored.
 *         </li><li>
 *         Exit edges from the inner loop become exit edges from subregion node.
 *         </li></ul>
 *     </li>
 *
 *     <li>The loops are rotated so one of the loop exits becomes the last block in the loop's layout.
 *         <ul><li>
 *         For each loop find an exit to a block placed just after the loop.</li>
 *         <li>
 *         If such exit does not exist or if it is already the last block in the loop's layout, we're done.</li>
 *         <li>
 *         Otherwise, loop body is rotated (all loop blocks are shifted towards end and the last blocks are flipped to
 *         the beginning of the loop), so the loop exit becomes the last block.
 *         </li></ul>
 *     </li>
 *   </ol>
 * </p>
 *
 * @author paul
 */
trait LayoutComponent { self: Universe with BackEnd =>

  private def augmentedGraph[N](g: BiGraph[N], newStart: N)
                               (newSuccs: N => Iterator[N])
                               (newPreds: N => Iterator[N]): BiGraph[N] = {
    new BiGraph[N] {
      val start = newStart
      def succs(n: N) = newSuccs(n)
      def preds(n: N) = newPreds(n)
      def invalidNode = g.invalidNode
    }
  }

  private def withRedirection[N](g: BiGraph[N], redirect: N => N): BiGraph[N] = {
    assert(redirect(g.start) == g.start)
    def succs(n: N): Iterator[N] = g.succs(n) map redirect
    def preds(n: N): Iterator[N] = g.preds(n) flatMap { p =>
      if (p != redirect(p)) preds(p) else Iterator.single(p)
    }
    augmentedGraph(g, g.start)(succs)(preds)
  }

  private def subGraph[N](g: BiGraph[N], start: N, nodes: N => Boolean): BiGraph[N] = {
    assert(nodes(start))
    augmentedGraph(g, start) { n => g.succs(n) filter nodes } { n => g.preds(n) filter nodes }
  }

  /** Returns true iff `block` code segment will be empty (not including its block end). */
  private def codeSegmentWillBeEmpty(block: Block): Boolean = {
    if (genDebug) {
      // In debug mode each block has NOP at its end.
      false
    } else {
      CodeOrder.in(block) forall {
        case _: Goto | _: Halt => true
        case n => noCodeShouldBeGenerated(n)
      }
    }
  }

  /** Redirected blocks are ones which can be eliminated from generated code. Block A redirected to block B if:
    * -- A has empty machine code and single exit (goto) to B
    * -- TODO: A, B have identical machine code and exits
    * Returns map of pairs (K -> V), where V is transitively closed redirection of K.
    */
  private def findRedirectedBlocks(): Maps[Block]#QMap[Block] = {
    val redirectMap = Maps[Block].newQMap[Block]

    @tailrec def closure(b: Block): Block = redirectMap.get(b) match {
      case None => b
      case Some(x) => closure(x)
    }

    for {
      block <- all[Block]
      if (block != entryBlock) && !isUnreachableBar(block)
      if codeSegmentWillBeEmpty(block)
      Goto(`block`, target) <- Iterator.single(block.blockEnd)
      if closure(target) != closure(block)
    } {
      stats.count(StatsKind.EmptySegmentOptimization, "optimized")
      redirectMap(block) = target
    }

    redirectMap mapValuesInPlace { (_, x) => closure(x) }
  }

  class Layout(val order: Seq[Block],
               val coldStart: Option[Block],
               val alignment: collection.Map[Block, Int],
               redirectMap: collection.Map[Block, Block]) {

    private val _aliases = groupMap(redirectMap)(_._2)(_._1)

    /** Returns iterator over `block` with all its aliases */
    def withAliases(block: Block): Iterator[Block] = Iterator(block) ++ _aliases.getOrElse(block, Seq.empty)

    def isAliasOf(alias: Block, block: Block): Boolean = (alias == block) || redirectMap.get(alias).contains(block)
  }

  /** Calculates CFG layout and alignments. */
  def makeLayout(redirectMap: collection.Map[Block, Block] = findRedirectedBlocks()): Layout = stage(CFGLayout) {
    val alignment = Maps[Block].newQMap[Int]

    def alignBlock(b: Block, x: Int): Unit = {
      // Avoid adding pointless alignments (i.e. alignments for 1 or less),
      // which might be passed here (e.g. default values of alignment options).
      val noAlignment = 1
      val prevAlignment = alignment.getOrElse(b, noAlignment)
      if (x > prevAlignment) {
        alignment(b) = x
      }
    }

    all[TableJump.Exit] foreach { exit => alignBlock(exit.target, env.valueOf(TableJumpTargetAlignment)) }

    def alignLoop(loop: Loop[Block], body: collection.IndexedSeq[Block]): Unit = {
      alignBlock(body.head, env.valueOf(LoopBodyAlignment))
    }

    // keep header_first orientation - it helps debuggers to properly step around a loop all defined in one line
    val loopOrientation = if (genDebug) HEADER_FIRST else FALLTHROUGH_EXIT
    val redirected: Block => Block = redirectMap orElse { case n => n }
    val redCFG = withRedirection(cfgWithoutXEdges(), redirected)
    val ts = redCFG.topSort((Iterator.single(redCFG.start) ++ all[XBlock]) map redirected)

    // when GenDebug - avoid splitting instructions into hot and cold code like exception handling because as it breaks
    // natural stepping order in debugger and/or leads to multiple breakpoint locations for trivial examples
    val coldBlocks = if (genDebug) ts.order.toSet else findColdBlocks()
    val hotBlocks = if (genDebug) Set.empty[Block] else (ts.order filterNot coldBlocks).toSet

    val hotCodeLayout = if (hotBlocks.isEmpty) IndexedSeq.empty[Block] else {
      val hotCFG = subGraph(redCFG, redCFG.start, hotBlocks)
      NaturalCFGOrder(hotCFG, loopOrientation, alignLoop)
    }

    val coldCodeLayout = if (coldBlocks.isEmpty) IndexedSeq.empty[Block] else {
      val coldEntries = Sets[Block].newQSet(ts.order filter { b =>
        coldBlocks(b) && {
          val preds = redCFG.preds(b)
          preds.isEmpty || (preds exists hotBlocks)
        }
      })
      val fakeStart = BBlock.raw()

      def augSuccs(b: Block) = if (b == fakeStart) coldEntries.iterator else redCFG.succs(b)

      def augPreds(b: Block) = if (coldEntries(b)) Iterator.single(fakeStart) ++ redCFG.preds(b) else redCFG.preds(b)

      val augCFG = augmentedGraph(redCFG, fakeStart)(augSuccs)(augPreds)
      val coldCFG = subGraph(augCFG, fakeStart, coldBlocks | Set(fakeStart))
      val siberia = NaturalCFGOrder(coldCFG, loopOrientation, alignLoop)
      assert(siberia.head == fakeStart)
      siberia.tail
    }

    checkConsistency(Optional) {
      assert(hotCodeLayout.toSet.size == hotCodeLayout.size)
      assert(coldCodeLayout.toSet.size == coldCodeLayout.size)
      assert(hotCodeLayout.size + coldCodeLayout.size == ts.order.size)
      assert(hotCodeLayout forall (b => !coldBlocks(b)))
      assert(coldCodeLayout forall (b => coldBlocks(b)))
    }

    val order = hotCodeLayout.toIndexedSeq ++ coldCodeLayout.toIndexedSeq
    assert(entryBlock == order.head)

    Layout(order, coldCodeLayout.headOption, alignment, redirectMap)
  }
}
