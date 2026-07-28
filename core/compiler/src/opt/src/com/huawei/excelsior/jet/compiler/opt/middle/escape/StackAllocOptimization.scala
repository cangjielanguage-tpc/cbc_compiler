/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.escape

import com.huawei.excelsior.jet.compiler.StatsKind.NewOptimization
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.ir.{EscapeKind, NewEscapeKind}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.middle.{EvacuateAnalysis, UnnecessaryOperationsElimination}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.types.Guards.Guard
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.Loops

/** Replace allocation in heap by allocation on stack.
  * Initially described in JET-9210.
  */
trait StackAllocOptimization extends StackAllocAnalysis with UnnecessaryOperationsElimination with EvacuateAnalysis { self: Universe =>

  def allocateObjectsOnStack(): Boolean = {
    import StackAllocAnalysis._
    assert(currentPhase > CompilerPhase.InterProceduralAnalysis)

    // Some new cold blocks may be created during guarded stack alloc but they do not influence other new operations.
    lazy val coldBlocks = findColdBlocks()

    var changed = false

    def process(n: AnyNew): Unit = {
      val (success, msg) = mayBeAllocatedOnStack(n) match {
        case Failure(x: EscapeKind) if x.containsPotentialEscape && env.enabled(BoolOption.Evacuation) && n.allocType.symType.isEvacuatedType &&
          // If all uses of a lambda function are escape uses, then there is no point in allocating it on stack.
          !newHasOnlyEscapeUses(n) =>

          transform(n).asInstanceOf[NewStackAllocated].stackAllocatedByEvacuateAnalysis = true
          (true, "lambda with global escape")
          
        case Failure(reason) =>
          (false, reason)

        case _ if coldBlocks contains n.block =>
          (false, "cold code")

        case Success =>
          transform(n)
          (true, "no guard")

        case Guarded(GuardKey(obj, guard), invokesAndTypes) =>
          requireNoGlobalCodeMotion() // required by canBeUsedAtPoint()
          if (canBeUsedAtPoint(obj, n)) {
            transformToGuarded(n, obj, guard)
            (true, s"guarded")

          } else {
            (false, "guard object cannot be used")
          }
      }

      changed ||= success
      if (stats.isEnabled(NewOptimization)) {
        val status = if (success) "successful" else "failed"
        val kind = n match {
          case _: New => "object"
          case _: NewArray => "array"
          case _ => shouldNotReachHere()
        }
        stats.count(NewOptimization, s"$status stack alloc of $kind ($msg)", n)
      }
    }

    TauTest.log.inSession("stack alloc", codeUnit) {
      // TODO: stack alloc key strings
      for (n <- all[AnyNew]) {
        n match {
          case _: New | _: NewArray => process(n)
          case _ =>
        }
      }
    }

    if (changed) {
      val loops = cfg.loops
      for (n <- all[AnyNewStackAllocated] if loops.isInLoop(n.block)) {
        n.inLoop = true
      }

      dbgPrinter.debugNodes("after stack alloc")
    }

    changed
  }

  // TODO: consider using incremental GCM instead
  private def canBeUsedAtPoint(node: Node, point: ControlNode): Boolean = node match {
    case node: FloatingNode => node.args forall (canBeUsedAtPoint(_, point))
    case node: PinnedNode => node.point strictDominates point
  }

  private def transform(source: AnyNew): AnyNew = {
    val proto = source match {
      case _: New => NewStackAllocated(source.allocType)
      case _: NewArray => NewArrayStackAllocated(source.allocType)
      case _ => shouldNotReachHere()
    }
    replaceByCode(source) { proto(source.valueArgs.toSeq: _*) }
  }

  private def transformToGuarded(original: AnyNew, obj: Node, guard: Guard): Unit = {
    val guarded = withPos(original) {
      def markTauTest(n: Node): Unit = n match {
        case n: TauTest => n.canBeUsedInDiamondDust = false
        case _ =>
      }
      onCommit.withCallback(markTauTest) {
        ScalaCollections.singleElement(
          replaceByMultiDiamondWithFastPaths(original, obj, TauInfo.Unknown)(guard) { () =>
            original.proto.asInstanceOf[SpinalNodePrototype[_ <: AnyNew]].apply(original.valueArgs.toSeq: _*)
          }
        )
      }
    }

    TauTest.log(s"- insert $guard", original)
    markTauBackupPath(original, obj, guard, s"[StackAlloc]\n codeUnit=$codeUnit.\n node=${original.name}")

    transform(guarded)
  }

}

