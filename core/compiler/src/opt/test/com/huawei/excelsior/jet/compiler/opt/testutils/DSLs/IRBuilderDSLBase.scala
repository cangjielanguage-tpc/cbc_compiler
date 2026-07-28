/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.testutils.DSLs

import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.{CodeUnit, Stats}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers._
import com.huawei.excelsior.jet.compiler.opt.ir.{FakeNodes, Universe}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.MarkedRegions.Hotness
import com.huawei.excelsior.jet.compiler.opt.platforms.PlatformDependent
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethod, FakeType}
import com.huawei.excelsior.jet.compiler.testutils.EnvProvider
import com.huawei.excelsior.jet.util.DSLs.IntGraphBuilderDSL
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterEach, Suite}

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * DSL to create IR objects.
  */
trait IRBuilderDSLBase extends EnvProvider with Universe with PlatformDependent with IntGraphBuilderDSL with FakeNodes with Matchers with Suite with BeforeAndAfterEach {

  startPhase(CompilerPhase.ZeroPhase)

  override lazy val statsGlobal = new Stats(env)

  def parent = null

  def codeUnit = CodeUnit.of(rootMethod)

  // Stub devirtualization tests which crash on rootMethod without receiver param.
  // Feel free to make better (e.g. more stable) implementation.
  override def rootReceiverType = FakeType("DummyRootReceiverType")

  var _rootMethod: FakeMethod = _
  override def rootMethod: FakeMethod = {
    if (_rootMethod == null) {
      // intentional lazy initialization, field initializer cannot be used
      _rootMethod = new FakeMethod
    }
    _rootMethod
  }

  protected def isPGOHost = false
  protected def getHotness(n: Node): Hotness = Hotness.Unknown
  protected def inlinePlanContains(callSitePos: Position, target: Method) = false
  protected def calledMethods(callSitePos: Position) = Iterator.empty[(Method, Int)]
  protected def plannedMethods(callSitePos: Position) = Iterator.empty[(Method, Int)]
  protected def devirtTargets(callSitePos: Position) = Iterator.empty[(Method, Int)]

  override final val profile = new ProfileInfo {
    override def isPGOHost = IRBuilderDSLBase.this.isPGOHost
    override def getHotness (n: Node): Hotness = IRBuilderDSLBase.this.getHotness(n)
    override def inlinePlanContains(callSitePos: Position, target: Method) = IRBuilderDSLBase.this.inlinePlanContains(callSitePos, target)
    override def calledMethods(callSitePos: Position) = IRBuilderDSLBase.this.calledMethods(callSitePos)
    override def plannedMethods(callSitePos: Position) = IRBuilderDSLBase.this.plannedMethods(callSitePos)
    override def devirtTargets(callSitePos: Position) = IRBuilderDSLBase.this.devirtTargets(callSitePos)
  }

  lazy val BlockIdBase = 1000

  private var blocks: mutable.HashMap[Int, Block] = _

  /** Reset graph builder before new build to suppress interference between builds. */
  override def beforeEach(): Unit = {
    super.beforeEach()

    resetEnvironment()
    resetUniverse()

    blocks = new mutable.HashMap[Int, Block]
  }

  private def index2BlockId(index: Int): Int = index + BlockIdBase
  private def blockId2Index(id: Int): Int = id - BlockIdBase

  protected def getBlock[T <: Block](i: Int, proto: BlockProto[T]): Block = blocks.getOrElseUpdate(i, {
    val block = proto.raw()
    block.id = index2BlockId(i)
    block
  })

  protected implicit def int2Block(i: Int): Block = getBlock(i, BBlock)
  protected implicit def int2SubGraph(i: Int): SubGraph = block2SubGraph(int2Block(i))
  protected implicit def block2Int(b: Block): Int = blockId2Index(b.id) ensuring (blocks(_) == b)
  protected implicit def block2SubGraph(b: Block): SubGraph = node2SubGraph(block2Int(b))

  protected def b(i: Int): BBlock = getBlock(i, BBlock).asInstanceOf[BBlock]
  protected def xb(i: Int): XBlock = getBlock(i, XBlock).asInstanceOf[XBlock]

  protected def bSet(indexes: Int*): Set[Block] = (indexes map int2Block).toSet

  protected object B {
    def unapply(x: Block): Option[Int] = {
      val index = blockId2Index(x.id)
      if (blocks contains index) Some(index) else None
    }
  }

  protected def makeCFG(start: SubGraph): Unit = {
    assert(start.enterNodes.size == 1)
    val (s, e) = (start.enterNodes.head, start.edges)
    val startBlock = blocks(s)
    val edgeBlocks = e map { x => (blocks(x._1), blocks(x._2)) }
    withPos(rootMethodPos) { createCFG(startBlock, edgeBlocks) }
  }

  def createCFG(startNode: Block, edges: Iterable[(Block, Block)]): Unit = {
    def commitAndBuildOutgoingEdges(block: Block): Unit = {
      val succs = edges collect { case (`block`, x) => x }

      if (isUnreachableBar(block)) {
        for (s <- succs) makeUnreachable(s.addArg(null))
        return
      }

      assert(!block.isCommitted)
      assert(block.id > 0)
      val blockID = block.id
      commit(block)
      block.id = blockID

      collect[BBlock](succs).toSeq match {
        case Seq() =>
          Return(block, block, Fake(IntType))

        case Seq(dest) =>
          val goto = Goto(block, block)
          dest.addArg(goto)

        case Seq(dest1, dest2) =>
          val branch = If(block, block, Fake(ConditionType))
          dest1.addArg(branch.trueExit)
          dest2.addArg(branch.falseExit)

        case dests =>
          // note that switch always has an extra default exit
          val switch = Switch(1 until dests.size)(block, block, Fake(IntType))
          assert(dests.size == switch.exits.size)
          for ((dest, exit) <- dests zip switch.exits) {
            dest.addArg(exit)
          }
      }

      collect[XBlock](succs).toSeq match {
        case Seq(xHandler) => HandlerAnchor.create(block, xHandler)
        case Seq() =>
      }
    }

    /** Sort all ingoing edges using order in which they were specified in edges list. */
    def sortIngoingEdges(block: BBlock): Unit = {
      // check that we have built all incoming edges
      val expectedPredBlocks = (edges collect { case (x, `block`) => x }).toSeq
      val actualPredBlocks = block.predBlocks
      assert(expectedPredBlocks.toSet == actualPredBlocks.toSet)

      // now sort them using order of expectedPredBlocks
      val inputs = block.inputs
      val expectedInputs = inputs sortBy { i => expectedPredBlocks indexOf i.block }

      // and update block
      block.updateInputs(expectedInputs)
    }

    // For Gods of ConsistencyChecking.
    def patchXBlocks(block: XBlock): Unit = {
      block.blockEnd match {
        case _: Halt | _: Goto => // already ok
        case x: Return => replaceByHalt(x)
        case x => shouldNotReachHere(x)
      }

      if (collect[Catch](block.paramNodes).isEmpty) {
        Catch(block)
      }
    }

    val allBlocks = (Seq(startNode) ++ edges.map(_._1) ++ edges.map(_._2)).distinct.sortBy(_.id)

    allBlocks foreach commitAndBuildOutgoingEdges
    collect[BBlock](allBlocks) foreach sortIngoingEdges
    collect[XBlock](allBlocks) foreach patchXBlocks

    startNode.addArg(Goto(entryBlock, entryMemory))
  }

  def removeHandlerAnchors(): Unit = {
    for (anchor <- all[HandlerAnchor]) {
      strikeOut(anchor)
    }
  }

  /** Returns SubGraph with unreachable bar node only. */
  protected def UB: SubGraph = {
    val ub = blocks.getOrElseUpdate(blockId2Index(unreachableBar.id), unreachableBar)
    new SubGraph(Set(ub), Set(ub), List.empty)
  }
}
