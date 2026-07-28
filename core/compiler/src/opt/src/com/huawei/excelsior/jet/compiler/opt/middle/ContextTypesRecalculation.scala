/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.compiler.opt.ir._

trait ContextTypesRecalculation extends LinearNodeOrder { self: Universe =>

  // TODO: eliminate copy-paste with Scope.State

  private class ContextTypesState(val contextTypes: ContextTypesMap) extends AbstractInterpreter.State {
    protected type This = ContextTypesState

    protected def forkImpl(): ContextTypesState = new ContextTypesState(contextTypes.clone())

    private lazy val _makeUnreachableCopy = new ContextTypesState(ContextTypesMap.Unreachable)
    override def makeUnreachableCopy() = _makeUnreachableCopy

    protected def copyOnWriteImpl(): Unit = { }

    def mergeFrom(block: Block, states: Seq[ContextTypesState], identity: Boolean)(mergeFunc: (Type, Seq[Node]) => Node) = {
      contextTypes.merge(states map {_.contextTypes}, block, identity)
      this
    }

    def foreachPair(that: ContextTypesState)(action: (Node, Node) => Unit): Unit = {
      // nothing to do
    }

  }

  private class ContextTypesBuilder extends AbstractInterpreter {
    var changed: Boolean = false

    type State = ContextTypesState

    protected def startInputState(b: Block) =
      new ContextTypesState(new ContextTypesMap())

    protected def interpret(block: Block, state: ContextTypesState): Block = {
      for (sn <- block.spineForward.toList) {
        assert(sn.isCommitted)
        val f = state.contextTypes.makeFilter(sn)
        if (f == null) {
          // - if node is spinal and has xhandler, fork state to xControl
          if (sn.hasXHandler) {
            addXCtrl(sn.xpoint, state.fork())
          }
        } else if (f.isRedundant) {
          // TODO: handle unreachable filter using replaceCheckByThrow()
          ContextTypesStats.updateOnRedundantFilterRemove(f)
          strikeOut(sn)
          changed = true
        } else {
          // - if node is filter, append it in state and xState
          val xState = if (sn.hasXHandler) state.fork() else null
          state.contextTypes.appendFilter(f)
          if (xState != null) {
            val xf = xState.contextTypes.makeFilter(sn.xpoint) ensuring (_ != null)
            xState.contextTypes.appendFilter(xf) // TODO: what about useless checks in xState?
            addXCtrl(sn.xpoint, xState)
          }
        }
      }

      ContextTypesMap.setMapAt(block.blockEnd, state.contextTypes)
      block
    }

    override protected def interpretEdge(blockExit: BlockExit, state: ContextTypesState): ControlNode = {
      val filter = state.contextTypes.makeFilter(blockExit)

      if (filter == null) {
        blockExit

      } else {
        def optimize(makeExit: => ControlNode): ControlNode = {
          ContextTypesStats.updateOnRedundantFilterRemove(filter)
          val exit = makeExit
          ContextTypesMap.setMapAt(exit, state.contextTypes)
          changed = true
          exit
        }

        blockExit match {
          case tauSwitchExit: TauSwitch.Exit if filter.isUnreachable && !tauSwitchExit.isDefault =>
            optimize { AnySwitch.dropExits(tauSwitchExit) }

          case branchExit: Branch.Exit if filter.isRedundant =>
            optimize { replaceByGoto(branchExit) }

          case _ =>
            state.contextTypes.appendFilter(filter)
            ContextTypesMap.setMapAt(blockExit, state.contextTypes)
            blockExit
        }
      }
    }
  }

  def recalculateContextTypes(): Boolean = {
    var changed = false

    val rtfInserted = ContextTypesMap.inContextTypesRecalculationMode {
      // 1. Recalculate context types and optimize filters
      def iterate(): Unit = {
        ContextTypesMap.resetCache()
        val builder = new ContextTypesBuilder()
        builder.iterate()
        changed |= builder.changed
      }

      iterate()
      if (ContextTypesMap.cleanupAfterContextTypesRecalculation()) {
        iterate()
      }

      // 2. Optimize controlled nodes
      for (node <- all[ContextDependentNode]) {
        if (ContextTypesMap.optimizeContextDependentNode(node)) {
          changed = true
        }
      }

      // 3. GetFlatThin nodes optimization. GetFlatThin requires its `base` argument to be passed through
      // GetFlatThinCheck node. It will be great to optimize them like regular controlled nodes using context types
      // mechanism, but there is a problem - `base` argument is AddrType node, which cannot be key of context types.
      for (gft @ GetFlatThin(base, offset, _) <- all[GetFlatThin]) {
        gft.inCtrl match {
          case GetFlatThinCheck(`base`, `offset`) => // nothing to do, check is already actual
          case inCtrl =>
            base.valueUses.collectFirst {
              case check @ GetFlatThinCheck(`base`, `offset`) if check dominates inCtrl => check
            } foreach { check =>
              gft.inCtrl = check
              changed = true
            }
        }
      }
    }
    changed || rtfInserted
  }
}