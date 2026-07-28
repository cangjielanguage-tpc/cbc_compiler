/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode

import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool
import com.huawei.excelsior.jet.compiler.bytecode.parsing.HandlersTreeMap.wouldCatchAnyException
import com.huawei.excelsior.jet.compiler.bytecode.parsing.{ExceptionHandlersParser, XHInfo}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

import collection.mutable
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.util.SuffixTree

/**
 * ExceptionHandlersBuilder builds exception handlers structure by exception table and handlers blocks.
 *
 * 1 step:
 *   Build [[SuffixTree]] of exception table subtables using [[ExceptionHandlersParser]].
 *
 * 2 step:
 *   With the built SuffixTree of exception table subtables, we build the following blocks tree-sequences for each
 *   exception table row:
 *
 *   * `X_i` - XBlock which has incoming XCONTROL args from nodes that can throw implicit exceptions.
 *     This block contains phies and Catch node which result is catched xobj.
 *     Outgoing edge goes to `Check_i` block.
 *     `cfState.kinds(X_i) = XBlockKind`
 *
 *   * `Check_i` - block which has incoming edge from `X_i`, from blocks which explicitly throws exceptions and from other
 *     `Check_j` blocks.
 *     This block contains check whether incoming xobj is instance of `CatchType_i` which corresponds to this exception
 *     table row. If it is instance of control goes to `Trans_i` otherwise control goes to another `Check_j` such that
 *     j-table row is parent of i-table row in SuffixTree.
 *     `cfState.kinds(Check_i) = ExceptionCheck(catchType)`
 *     If `CatchType_i` is null it means that table row catches all exceptions. In such case this block is
 *     not created at all and all edges leads to `Trans_i` immediately.
 *
 *   * `Trans_i` - block which has only one incoming edge from `Check_i`.
 *     This block is used for correct bytecode parsing: it pushes on stack xobj which came to `Check_i`.
 *     Control then goes to handler block which is specified by exception table row.
 *     `cfState.kinds(Trans_i) = TransitionToHandler`
 *
 *   Specific block is created for throwing exception which is not handled by other `Check_i` handlers:
 *
 *   * `Throw` - block which has incoming edges from other `Check_i` blocks.
 *     This block contains only Throw node which args are incoming xobj and memory.
 *     `cfState.kinds(Throw) = ThrowBlock`
 *
 * @author conwor
 * @author paul
 * @author cypok
 */
trait ExceptionHandlersBuilder { self: Universe with ControlFlow =>

  /** Creates ControlFlowParsingState.xblocks */
  def buildHandlers(method: Method, cfState: ControlFlowParsingState): Unit = {
    if (!cfState.handlersTree.isEmpty) {
      assert(cfState.attr.hasExceptionTable)
      new XBlocksBuilder(method.getDeclaringClass.getClassConstantPool, cfState).run()
    }
  }

  private type XH = XHInfo[BBlock]

  /**
   * Builds exception header blocks (XBlocks)
   */
  private class XBlocksBuilder(cp: ConstantPool, cfState: ControlFlowParsingState) {

    /**
     * Builds XBlock and one of the following:
     * - block for one exception check with goto handler;
     * - unwind.
     *
     * Returns triplet of XBlock, branch where control goes if exception is unhandled (may be null)
     * and sequense of all created BBlocks.
     */
    private def buildXBlockAndSuccs(ex: XH, incoming: Seq[Node]): (XBlock, Node, Seq[BBlock]) = {
      val handler = ex.handler

      val (enterHandler, unhandledExit, allCreatedBBlocks) = withPos(handler) {
        def buildTransBlock() = {
          val transBlock = BBlock()
          cfState.kinds(transBlock) = TransitionToHandler
          handler.addArg(Goto(transBlock, transBlock))
          transBlock
        }

        if (!wouldCatchAnyException(ex)) {
          // catch suitable exceptions
          val instanceofBlock = BBlock()
          val branch = If(instanceofBlock, instanceofBlock, Proxy(ConditionType)(entryBlock))
          cfState.kinds(instanceofBlock) = ExceptionCheck(ex.getCatchType(cp))

          val transBlock = buildTransBlock()
          transBlock.addArg(branch.trueExit)

          (instanceofBlock, branch.falseExit, Seq(instanceofBlock, transBlock))
        } else {
          // catch all exceptions
          val transBlock = buildTransBlock()
          (transBlock, null, Seq(transBlock))
        }
      }
      withPos(enterHandler) {
        val xBlock = XBlock()
        Catch(xBlock)
        val gotoEnter = Goto(xBlock, xBlock)

        // Insert ConvertDomain into handlers that might actually use the exception object
        val optionalDomain = Option.when(ex.catchTypeName != null)(rootMethod.getDomain)
        cfState.kinds(xBlock) = XBlockKind(optionalDomain)

        assert(!(incoming contains null))
        enterHandler.addArgs(gotoEnter +: incoming)

        (xBlock, unhandledExit, allCreatedBBlocks)
      }
    }

    private def setHandler(b: BBlock, h: XBlock): Unit = {
      HandlerAnchor.create(b, h)
    }

    /** Makes all supplementary exception blocks by given exception table. */
    def run(): Unit = {
      val paths = cfState.handlersTree.optimized()
      val root = paths.root

      val xBlocks = new mutable.LinkedHashMap[SuffixTree[XH], XBlock]

      // map from tree element to sequence of its created supporting BBlocks
      val supportingBlocksOfHandler = new mutable.LinkedHashMap[SuffixTree[XH], Seq[BBlock]]

      def buildBlocks(tree: SuffixTree[XH]): Node = {
        val (xBlock, exit, createdHandlerBlocks) = buildXBlockAndSuccs(tree.elem, tree.getChildren.toSeq map buildBlocks)
        xBlocks(tree) = xBlock
        supportingBlocksOfHandler(tree) = createdHandlerBlocks
        exit
      }

      val unhandledExits = root.getChildren.toSeq map buildBlocks filter { _ != null }
      if (unhandledExits.nonEmpty) {
        val block = BBlock(unhandledExits*)
        Halt.empty()(block, block)
        cfState.kinds(block) = ThrowBlock
      }

      for ((block, tree) <- paths.iterator.toArray.sortBy(_._1.id)) {
        setHandler(block, xBlocks(tree))
      }

      // all exceptions from supporting blocks created for handling the exception
      // are catched by the handler of the handler of this exception
      for ((tree, blocks) <- supportingBlocksOfHandler) {
        for (handlerOfHandler <- tree.elem.handler.singleXHandlerOption) {
          blocks foreach (setHandler(_, handlerOfHandler))
        }
      }
    }
  }

}

