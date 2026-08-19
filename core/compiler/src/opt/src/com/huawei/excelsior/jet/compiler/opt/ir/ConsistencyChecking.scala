/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.options.NumOption.ConsistencyCheckLevel
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.CheckLevels.Optional
import com.huawei.excelsior.jet.compiler.opt.middle.{UCEComponent, UnmovableAnalysis}
import com.huawei.excelsior.jet.compiler.opt.middle.sync.SynchronizationOptimization
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.graph.BiGraph

/** The consistency check is perfomed only if its level
  * is less or equal than current consistency check level
  * which is set by compiler equation ConsistencyCheckLevel.
  *
  * No checks must be performed if current consistency check level equals 0.
  * All checks must be performed if current consistency check level equals [[com.huawei.excelsior.jet.compiler.opt.ir.CheckLevels.allChecksLevel]].
  */
object CheckLevels {
  val allChecksLevel = 3

  sealed class Level(val n: Int) {
    assert(n <= allChecksLevel)
  }

  case object Important extends Level(1)
  case object Desirable extends Level(2)
  case object Optional extends Level(3)
}

/**
 * Check consistency of whole IR.
 */
trait ConsistencyChecking extends SynchronizationOptimization with UCEComponent with UnmovableAnalysis { self: Universe =>

  private lazy val curCheckLevel = env.valueOf(ConsistencyCheckLevel)

  final def consistencyChecksEnabled(level: CheckLevels.Level) = level.n <= curCheckLevel

  def checkConsistency[R](level: CheckLevels.Level)(action: => R): Unit = {
    if (consistencyChecksEnabled(level)) {
      stage(Stage.ConsistencyChecking) {
      val r = action
        r match {
          case b: Boolean => assert(b)
          case _: Unit =>
          case _ => shouldNotReachHere("unexpected result type")
        }
      }
    }
  }

  def checkDAGsConsistency(level: CheckLevels.Level): Unit = {
    checkConsistency(level) {
      all[Block] foreach { b => checkGraphConsistency(level, DAG(b)) }
    }
  }

  def checkIRConsistency(level: CheckLevels.Level): Unit = checkConsistency(level) {
    assert(entryBlock.inputs.isEmpty)
    checkUnreachableConsistency()

    assert(currentScope.xedgesCount == all[XPoint].count(_.hasHandler))

    for (n <- allNodes) {
      assert(n.isCommitted, n)
      assert(n.args forall { n => n != null && n.isCommitted }, n)

      if (!n.isInstanceOf[Phi]) {
        val uses = Closure(n.valueUses) {
          case _: Phi => Seq.empty
          case x => x.valueUses
        }
        assert(!(uses contains n), s"node $n has data-flow loop without phi-functions")
      }

      val hasMemArgs = n.argsByTag(Tag.MEMORY).nonEmpty
      if (hasMemArgs) {
        assert(n.isInstanceOf[Block] || n.isInstanceOf[HasInMemory], n.simpleName)
      }
      if (n.isInstanceOf[HasInMemory]) {
        assert(hasMemArgs, n.simpleName)
      }

      checkConsistency(Optional) {
        n match {
          case c: Call => c.target match {
            case t: AnyInvokeTarget => assert(c.targetRef == t.targetRef)
            case _ => // not call target or call target without target ref
          }
          case _ =>
        }
      }

      n match {
        case sn: SpinalNode =>
          assert(sn.outCtrl != null)
          assert(sn.inCtrl.block == sn.block && sn.block == sn.outCtrl.block)
          assert(sn.canThrow == sn.hasXPoint)
          if (sn.block.reachable) {
            assert(sn.memoryBefore == sn.inCtrl.memoryAfter)
            assert(sn.memoryAfter == sn.outCtrl.memoryBefore)
          }
          // Note: HandlerAnchor may not have position after parsing.
          if (sn.canThrow && !sn.isInstanceOf[UnreachableThrowing] && !sn.isInstanceOf[HandlerAnchor]) {
            assert(sn.inlineContext.method.isManaged || sn.inlineContext.method.isManual,
              s"unexpected operation ${sn.simpleName} in unmanaged context")
          }

          if (isStructuredLocking && (currentPhase < CompilerPhase.Lowering) && sn.block.reachable) {
            sn match {
              case enter: MonitorEnter =>
                assert(SynchronizedRegion.enclosing(enter) == enter.syncRegion.outer)
                assert(enter.syncRegion.singleMonitorObj forall (_ == enter.obj))
              case exit: MonitorExit =>
                assert(SynchronizedRegion.enclosing(exit) forall (_ == exit.syncRegion))

              case _ =>
            }
          }

          if (sn.isInstanceOf[MemoryNode]) {
            assert(sn.isInstanceOf[SpinalMemoryNode])
          }

          sn match {
            case fieldSeqOp: FieldSeqOperation =>
              checkFieldSeqOperation(fieldSeqOp)
            case _ =>
          }

        case be: BlockEnd =>
          assert(be.block.blockEnd == be)
          if (be.block.reachable) {
            assert(be.memoryBefore == be.inCtrl.memoryAfter)
          }
          if (be.exits.isEmpty) {
            assert(be.tpe == UnreachableControlType)
          } else {
            for (x <- be.exits) {
              assert(x.tpe == ControlType)
              assert(x.uses.size <= 2)
              assert(x.uses.count(_.isInstanceOf[Block]) == 1)
              assert(x.uses.count(_.isInstanceOf[Constraints]) <= 1)
            }
          }

        case b: Block =>
          assert(b.blockEnd.block == b)
          if (b.reachable) {
            assert(b.memoryAfter == b.outCtrl.memoryBefore, s"$b ${b.memoryAfter} ${b.outCtrl} ${b.outCtrl.memoryBefore}")
          }

          Block.verifyBlockControlNums(b)

          assert(b.xHandlers.size <= 1, s"block $b has multiple different xHandlers")
          assert(b.xpoints.distinctBy(_.hasHandler).length <= 1,
            s"xPoints with and without xHandler exist in one block $b")

          b match {
            case xb: XBlock =>
              assert(xb.redefinesMemory)
              assert(xb.blockEnd.isInstanceOf[Goto] || xb.blockEnd.isInstanceOf[Halt] || isO1Compiled)
              assert((currentPhase >= CompilerPhase.Preparation) || collect[Catch](xb.paramNodes).size == 1)
            case _ =>
          }

        case phi: Phi =>
          assert(phi.args.size == phi.block.arity)

        case ref: CangjieReferenceNode =>
          assert(ref.uses.forall(_.isInstanceOf[FieldSeqOperation]))

        case fieldSeqOp: FieldSeqOperation =>
          checkFieldSeqOperation(fieldSeqOp)

        case _ =>
      }
    }

    checkDefUseDominance()
    checkSynchronizationConsistency()
    checkLocalUnmovableConsistency()
  }

  def checkGraphConsistency[A : Sets](level: CheckLevels.Level, graph: BiGraph[A]): Unit = checkConsistency(level) {
    val nodes = Closure(graph.start) { n => graph.succs(n) ++ graph.preds(n) }
    for (n <- nodes) {
      for (pred <- graph.preds(n)) {
        assert(graph.succs(pred).contains(n), s"$pred is pred of $n but $n is not succ of $pred")
      }
      for (succ <- graph.succs(n)) {
        assert(graph.preds(succ).contains(n), s"$succ is succ of $n but $n is not pred of $succ")
      }
    }
  }

  private def checkFieldSeqOperation(node: FieldSeqOperation): Unit = {
    assert(node.fields.init.forall(_.isInstanceOf[CangjieReferenceNode]))
  }
}
