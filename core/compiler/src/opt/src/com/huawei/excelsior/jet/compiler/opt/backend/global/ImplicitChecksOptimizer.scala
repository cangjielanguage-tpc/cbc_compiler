/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.global

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.Worklist

import scala.PartialFunction.{cond, condOpt}

/**
 * Generated code post-processor, which optimize explicit checks by wiring them with nodes,
 * that can perform such checks implicitly by their generated instructions, e.g. NullCheck and GetField.
 *
 * @author conwor
 */
trait ImplicitChecksOptimizer { self: Universe with BackEnd =>

  def optimizeImplicitChecks(): Unit = {

    def canBeImplicit(check: PureCheck): Boolean = cond(check) {
      case _: AbstractNullCheck | _: DivisorCheck => true
    }

    object CbcNullCheck {
      def unapply(node: Node): Option[Node] = {
        if (targetArch != CBC) return None

        condOpt(node) {
          case ArrayLength(arr)                                   => arr
          case arrOp: ArrayElementOperation                       => arrOp.array
          case fieldOp: InstanceOperation                         => fieldOp.obj
          case fieldOp: BitcodeDeferred.FieldOp if fieldOp.hasObj => fieldOp.obj

          // PreCall node can only be inserted before Call with XSite
          // and only VMT-calls can have attached null-check
          case call @ AnyVirtualCall() if call.targetRef.hasVirtualMethodSlot && call.hasXSite =>
            call.receiver
          case call @ BitcodeDeferred.Invoke(targetRef) if !targetRef.isDirectCall =>
            call.receiver ensuring call.hasXSite
        }
      }
    }

    val wl = Worklist.from(all[PureCheck] filter canBeImplicit)

    def optimize(check: PureCheck): Boolean = {
      // Set of registers, that should not be touched during going through nodes
      val untouchable = if (!check.hasXHandler || !check.hasConstraints) {
        emptySet
      } else {
        check.constraints.liveResources()
      }

      assert(!check.hasGroup)
      assert(check.resource == InvalidResource)

      val implicitSpoiledOnTrapExit = implicitCheckVolatileResources(check, ExitKind.TRAP)
      if (!(implicitSpoiledOnTrapExit disjointWith untouchable)) {
        return false
      }

      def tryToAttachTo(node: Node): Boolean = {

        /** Returns whether offset of memory access in given `fieldOp` could be encoded in null check info. */
        def offsetCouldBeEncoded(offset: Int) = {
          true  // TODO JET-9023: restore NullCheck-offset check (to compare with decoded offset at runtime)
        }

        assert(!check.trusted)
        val attach = (check, node) match {
          case (_, ic: MayHaveImplicitCheck) if ic.hasImplicitCheck =>
            // Cannot attach implicit check to already grouped node.
            false

          case (nullCheck: AbstractNullCheck, LoadStoreMemoryAccess.Disposed(base, offset)) if offsetCouldBeEncoded(offset) &&
            valueOf(base) == valueOf(nullCheck.obj) =>
            nullCheck.obj = base
            true

          case (nullCheck: NullCheck, CbcNullCheck(obj)) if valueOf(obj) == valueOf(nullCheck.obj) =>
            nullCheck.obj = obj
            true

          case (divisorCheck: DivisorCheck, op: IDivRemOp) if implicitDivisorCheckAllowed && valueOf(op.r) == valueOf(divisorCheck.divisor) =>
            divisorCheck.divisor = op.r
            true

          case _ =>
            false
        }

        if (attach) {
          val implicitSpoiledOnNormalExit = implicitCheckVolatileResources(check, ExitKind.NORMAL)
          if (implicitSpoiledOnNormalExit.nonEmpty) {
            for (spoiled <- implicitSpoiledOnNormalExit) {
              assert(node.isResultOrSpoiledResource(spoiled) && !node.isArgumentResource(spoiled))
            }
          }

          assert(check.block == node.block)
          CodeOrder remove check
          check.attachToGroup(node, Group.AttachReason.IMPLICIT_CHECK_ARG)
          stats.count(StatsKind.ImplicitCheckOptimization, s"${check.simpleName} optimized with ${node.simpleName}")
        }

        attach
      }

      /** Returns true iff the `check` has no control dependence with `node` and could be passed down through it. */
      def couldPass(node: Node): Boolean = {
        def fail(details: String): Boolean = {
          stats.count(StatsKind.ImplicitCheckOptimization,
            s"${check.simpleName} not optimized because of ${node.simpleName}. ($details)")
          false
        }

        node match {
          case xc: PureCheck if xc.isImplicit => shouldNotReachHere() // should not be in code order
          case xc: PureCheck if wl contains xc =>
            wl -= xc
            optimize(xc) || fail("other not optimized check")

          case ic: MayHaveImplicitCheck if ic.hasImplicitCheck => fail("control dependency with other check")

          case _: Constraints => true
          case _: BulldozerHint => true
          case _: ControlNode => fail("control dependency")
          case c: ControlledNode if c.inCtrl == check => fail("controlled dependency")

          case _ if untouchable.isEmpty => true

          case _ if node.allResultResources.exists(untouchable.contains) || node.spoiled.exists(untouchable.contains) =>
            fail("data-flow dependency")

          case _ => true
        }
      }

      for (node <- CodeOrder after check) {
        if (tryToAttachTo(node)) return true
        if (!couldPass(node)) return false
      }

      false
    }

    wl.drain foreach optimize
  }
}
