/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool.Access
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.StructuredLockingAnalyzer.LockingInformation
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.{StructuredLockingAnalysisResult, StructuredLockingAnalyzer}
import com.huawei.excelsior.jet.compiler.bytecode.parsing.{CompleteControlFlowParserAndStructuredLockingAnalyzer, ControlFlowParser, HandlersTreeMap}
import com.huawei.excelsior.jet.compiler.bytecode.{NoPosition, Position}
import com.huawei.excelsior.jet.compiler.opt.ir.{DebugPrinters, NodeGraphs, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.CompilerException
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.symlevel.Method.CodeAttribute
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.compiler.{Domain, symlevel}

/**
 * Java bytecode control flow analysis and CFG builder.
 * Used as first pass for java bytecode parsing.
 *
 * @author paul
 */
trait ControlFlow extends BackwardBranchesProcessor
                     with ExceptionHandlersBuilder
                     with Toolbox
                     with IRTransformationsCollection { self: Universe with DebugPrinters with NodeGraphs =>

  sealed abstract class BlockKind

  /** Ordinary blocks corresponding to the given bytecode range. */
  case class BytecodeRange(startPC: Int, endPC: Int) extends BlockKind

  /** Prolog block. */
  case object PrologBlock extends BlockKind

  sealed abstract class ExceptionStuffBlock extends BlockKind

  /** Exception checking (instanceof) blocks. */
  case class ExceptionCheck(catchType: Access[symlevel.ClassType]) extends ExceptionStuffBlock

  /** Block with throw operation where control comes if any handler cannot handle exception. */
  case object ThrowBlock extends ExceptionStuffBlock

  /** XBlock kind contains Catch operation. */
  case class XBlockKind(domain: Option[Domain]) extends ExceptionStuffBlock

  /** Transition block from ExceptionCheck to bytecode handler. */
  case object TransitionToHandler extends ExceptionStuffBlock

  /** Control flow information extracted from bytecode. */
  class ControlFlowParsingState(val attr: CodeAttribute, val prolog: BBlock) {
    /** Kinds of blocks parsed from bytecode. */
    val kinds = Maps[Block].newQMap[BlockKind]
    if (prolog != null) kinds(prolog) = PrologBlock

    /** Raw (non-optimized) SuffixTree for all blocks handlers sequences. */
    var handlersTree: HandlersTreeMap[BBlock] = _

    import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.onbytecode.{MonitorEnter as SLEnter, MonitorExit as SLExit}

    var lockingInfo: StructuredLockingAnalyzer.LockingInformation[SLEnter[BBlock], SLExit[BBlock]] = _

    lazy val outerMonitorBlockMap: Map[Block, BBlock] = {
      lockingInfo.outerMonitors.map {
        case (inner, outer) => (inner.block, outer.block)
      }.toMap
    }

    lazy val pairedEnterBlockMap: Map[Block, BBlock] = {
      lockingInfo.monitorPairs.map {
        case (exit, enter) => (exit.block, enter.block)
      }.toMap
    }
  }

  /**
   * Parse bytecode and build its CFG.
   * We do it in the following way:
   * <p/>
   * <p/>
   * First, we go through the bytecode and split the bytecode range into multiple basic blocks by targets of control
   * flow bytecode instructions such as goto, branches and switches linking resulting blocks by controlflow.
   * <p/>
   * <p/>
   * Second, we do more splitting by exception table rows: StartPC, EndPC, HandlerPC.
   * <p/>
   * <p/>
   * Finally we make XBlocks by exception table: it is the process of converting exception table into
   * control flow blocks. For more details on this process see
   * [[com.huawei.excelsior.jet.compiler.opt.frontend.bytecode.ExceptionHandlersBuilder ExceptionHandlersBuilder]]
   *
   * @return pair of start basic block and seq of all xblocks.
   */
  def makeCFG(method: Method, prolog: BBlock): ControlFlowParsingState = {
    val attr = method.codeAttribute
    val cfState = new ControlFlowParsingState(attr, prolog)

    new Builder(method, cfState) {
      override protected def afterInitialControlFlowParsing(): Unit =
        dbgPrinter.debugCFG("CFG after initial control flow parsing", handlersInfo)
      override protected def afterSubroutinesInlining(hasSubroutines: Boolean): Unit =
        dbgPrinter.debugCFG("CFG after subroutines inlining", handlersInfo)

      private def handlersInfo(b: Block) =
        blockHandlersTree.get(b.asInstanceOf[BBlock]).map(info => "handlers: " + info.toRootToString).orNull
    }.run()
    dbgPrinter.debugCFG("CFG after complete control flow parsing")

    if (method.isManaged) {
      buildHandlers(method, cfState)
    } else {
      assert(!attr.hasExceptionTable)
    }
    dbgPrinter.debugCFG("CFG after handlers built")

    cfState
  }

  private[bytecode] def removeBlockEnd(blockEnd: BlockEnd): Unit = {
    blockEnd.block.blockEnd = null
    blockEnd.makeUsesUnreachable()
    blockEnd.valueArgs foreach { case proxy: Proxy =>
      assert(proxy.singleUse == blockEnd)
      decommit(proxy)
    }
    decommit(blockEnd)
  }

  private class Builder(method: Method, cfState: ControlFlowParsingState)
    extends CompleteControlFlowParserAndStructuredLockingAnalyzer[BBlock](env, env.asVerifiableMethod(method)) {

    // NOTE: Scala has problems with overriding methods with generic Java arrays in signature.
    // These Array[BBlock with Object] correspond to "B[]" in super class.

    def run(): Unit = {
      parse()

      structuredLockingInfo.state match {
        case StructuredLockingAnalysisResult.STRUCTURED if !rootDeclaringClass.isCangjieType && !isO1Compiled => // TODO: see JET-13451
          cfState.lockingInfo = structuredLockingInfo

        case _ =>
          isUnstructuredLocking = true
          cfState.lockingInfo = StructuredLockingAnalyzer.LockingInformation.empty
      }

      entryBlock addArg Goto(cfState.prolog, cfState.prolog)
      cfState.handlersTree = blockHandlersTree
    }

    private def bcToPos(bc: Int): Position =
      if (bc != ControlFlowParser.NO_BYTECODE_POSITION) currentMethodPos(bc)
      else NoPosition


    override protected def blockHasNormalExit(block: BBlock): Boolean = block.blockEnd.isInstanceOf[Return]

    override protected def setBlockBCRange(block: BBlock, start: Int, end: Int): Unit =
      cfState.kinds(block) = BytecodeRange(start, end)

    private def bytecodeBlockRange(block: BBlock) =
      cfState.kinds(block).asInstanceOf[BytecodeRange]

    override protected def blockStartPC(block: BBlock): Int = bytecodeBlockRange(block).startPC
    override protected def blockEndPC(block: BBlock): Int = bytecodeBlockRange(block).endPC

    override protected def succBlocks(block: BBlock): Iterator[BBlock] = block.succBlocks

    override protected def createBlock(bc: Int) = withPos(bcToPos(bc)) { BBlock() }

    override protected def cloneBlock(block: BBlock): BBlock = {
      val newBlock = withPos(block) { BBlock() }

      Node.clone(block.blockEnd, {
        case `block` => newBlock
        case arg: Proxy => Proxy(arg.tpe)(self.entryBlock)
        case arg => shouldNotReachHere(arg)
      })

      assert(block.spine.isEmpty) // Check that we do not need to copy anything else

      cfState.kinds(newBlock) = cfState.kinds(block)

      newBlock
    }

    override protected def connectClonedBlockToClonedTargets(block: BBlock, targetBlocks: Iterator[BBlock]): Unit = {
      val exits = block.blockEnd.exits.iterator
      val targets = targetBlocks
      for ((exit, target) <- exits.zipAll(targets, null, null)) {
        assert(exit != null && target != null, "number of exits should equal to number of target blocks")
        assert(exit.uses.isEmpty)
        target.addArg(exit)
      }
    }

    override protected def connectJsrRetBlockToRealTarget(block: BBlock, targetBlock: BBlock): Unit = {
      val oldEnd = block.blockEnd
      removeBlockEnd(oldEnd)
      targetBlock addArg withPos(oldEnd) { Goto(block, block) }
    }

    override protected def splitBlock(bc: Int, block: BBlock) = {
      // do not use Block.splitAfter(block) because it requires valid blockEnd
      val prevBlockEnd = block.blockEnd
      val goto = Goto(block, block)
      val newBlock = withPos(bcToPos(bc)) { BBlock(goto) }
      if (prevBlockEnd != null) {
        assert(prevBlockEnd.inCtrl == block)
        prevBlockEnd.inCtrl = newBlock
        prevBlockEnd.inMemory = newBlock
        newBlock.blockEnd = prevBlockEnd
      }
      block.refreshBlockRef()
      newBlock
    }

    private def addBlockEnd[A, N <: BlockEnd](bc: Int, block: BBlock, endProto: BlockEndProto[N]): N = {
      assert(block.blockEnd == null)
      val nodeArgs = Seq.tabulate(endProto.arity)(endProto.argType(_) match {
        case ControlType | MemoryType => block
        case tpe => Proxy(tpe)(self.entryBlock)
      })
      withPos(bcToPos(bc)) { endProto(nodeArgs: _*) }
    }

    override protected def addReturn(bc: Int, block: BBlock): Unit =
      addBlockEnd(bc, block, Return.proto(ValueType.fromSig(method.getReturnType, instantiateRich = true)))

    override protected def addThrow(bc: Int, block: BBlock): Unit =
      addBlockEnd(bc, block, Halt.afterThrow("java bytecode control flow analysis"))

    override protected def addHalt(bc: Int, block: BBlock): Unit =
      addBlockEnd(bc, block, Halt.explained("java bytecode control flow analysis"))

    override protected def addJump(bc: Int, block: BBlock, targetBlock: BBlock): Unit = {
      val end = addBlockEnd(bc, block, Goto)
      targetBlock.addArg(end)
    }

    override protected def addIf(bc: Int, block: BBlock, falseTarget: BBlock, trueTarget: BBlock): Unit = {
      val end = addBlockEnd(bc, block, If)
      falseTarget.addArg(end.falseExit)
      trueTarget.addArg(end.trueExit)
    }

    private def addSwitch(bc: Int, block: BBlock, matches: IndexedSeq[Int], targetBlocks: IndexedSeq[BBlock], defaultBlock: BBlock): Unit = {
      assert(matches.length == targetBlocks.length)
      val filteredPairs = (matches zip targetBlocks) filter { _._2 != defaultBlock }

      val switch = addBlockEnd(bc, block, Switch(filteredPairs.map(_._1)))

      defaultBlock.addArg(switch.defaultExit)
      for (i <- filteredPairs.indices) {
        filteredPairs(i)._2.addArg(switch.caseExits(i))
      }
    }

    override protected def addTableSwitch(bc: Int, block: BBlock, lowMatch: Int, highMatch: Int, targetBlocks: Array[BBlock], defaultBlock: BBlock): Unit =
      addSwitch(bc, block, lowMatch to highMatch, targetBlocks.toIndexedSeq, defaultBlock)

    override protected def addLookupSwitch(bc: Int, block: BBlock, matches: Array[Int], targetBlocks: Array[BBlock], defaultBlock: BBlock): Unit =
      addSwitch(bc, block, matches.toIndexedSeq, targetBlocks.toIndexedSeq, defaultBlock)
  }
}
