/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.post

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg}
import com.huawei.excelsior.jet.compiler.Env.{targetArch, targetPlatform}
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.ir.XSiteKind
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.global.ImplicitChecksOptimizer
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Universe}
import com.huawei.excelsior.jet.compiler.symlevel.{BytecodeMethodReference, MethodReferenceAccessKind}
import com.huawei.excelsior.jet.util.ScalaCollections.{groupBy, singleElement}
import com.huawei.excelsior.jet.util.WhileChanged.whileChanged
import com.huawei.excelsior.jet.compiler.{NotImplementedFeature, Stage, StatsKind}

/**
  * Post-process implemented on IR with already calculated backend attributes (resources, order) but before
  * assembler code generated.
  *
  * @author conwor
  */
trait PostProcessComponent extends FrameSlotsColoringComponent with ImplicitChecksOptimizer { self: Universe with BackEnd =>

  /** Changes uses in constraints to nodes (original or synonyms), really used in following blocks. */
  private def correctConstraints(): Unit = {
    // Constraints are used for LiveRange calculation now and in peephole optimizer (redundant copies elimination),
    // so we update them
    // TODO: refactor LiveRanges and remove Constraints before post-process.
    if (isO1Compiled) {
      // O1 regalloc states created from constraints and do not share value traces between blocks, so there
      // is no need to correct constraints. TODO: make check of this assertion
    } else {
      all[Constraints] foreach decommit
      createConstraints(beforeRegAlloc = false)
    }
  }

  /** Removes all bulldozer hint nodes to not interfere copies removing. */
  private def removeBulldozerHintNodes(): Unit = {
    all[BulldozerHint] foreach { node =>
      val upper = node.inCtrl
      node.pinnedNodes filter { _ != node } foreach { _.asInstanceOf[FloatingNode].atUpperPoint(upper) }
      CodeOrder remove node
      strikeOut(node)
    }
  }

  object CodeRange {
    class RangeLimit(val node: Node, val incl: Boolean)
    case class incl(n: Node) extends RangeLimit(n, true)
    case class excl(n: Node) extends RangeLimit(n, false)

    def apply(from: RangeLimit, to: RangeLimit) = {
      assert(from.node.block == to.node.block)
      assert(CodeOrder after from.node contains to.node)
      assert(!to.incl)

      val fromNode = if (from.incl) {
        from.node.groupRoot
      } else {
        CodeOrder next from.node.groupRoot
      }

      new CodeRange(from.node, fromNode, to.node)
    }
  }

  import CodeRange.excl

  /** Code range contains nodes from code order of given nodes' block.
    * Including `from` node and excluding `to` node.
    */
  class CodeRange(holder: Node, from: Node, to: Node) {
    def nodesForward: Iterator[Node] = CodeOrder from from takeWhile (_ != to)

    // Float registers could not be included in GC maps now, so we should check
    // that in region [from, to] there are no GC map points.
    // TODO: remove this patch, when F registers will be used in GC maps
    private def checkGCCorrectnessFor(r: Resource): Boolean = r match {
      case _ if !willBeCollectedInGCMap(holder) => true
      case Immediate => true // immediate will not be collected in the map
      case _: Location.IReg | _: FrameSlot => true // these resources may be collected anywhere
      case _: Location.FReg => !(nodesForward exists needGCMap)
    }

    private def untouched(r: Resource, checkArgs: Boolean): Boolean = {
      (nodesForward forall { n =>
        !n.isResultOrSpoiledResource(r) && !(checkArgs && n.isArgumentResource(r))
      }) && checkGCCorrectnessFor(r)
    }

    def notWritten(r: Resource): Boolean = untouched(r, checkArgs = false)
    def notWrittenOrRead(r: Resource): Boolean = untouched(r, checkArgs = true)
  }

  /** Redundant copies eliminator.
    *
    * There may be pattern in code
    *   nS @ any node: ... -> rS
    *   nT @ copy(nS): rS -> rT
    *
    *   1) If `nT` is trivial (`rS` == `rT`), we can remove it and replace its uses to `nS`.
    *
    *   2) If there are no value uses of `nS` except `nT` and
    *      `nS` node form allowed `rT` as result resource and
    *      no one node in range (`nS`, `nT`) writes to or reads from `rT`
    *      then we replace result resource of `nS` by `rT`, remove `nT` and replace its uses by `nS`.
    *
    *   3) For each use `nU` of `nT` we can check, that if
    *        `nU` node form allowed `rS` as argument resource and
    *        no one node in range (`nT`, `nU`) writes to `rS`
    *        then we can replace replace `nT` use in `nU` by `nS`
    *
    *   4) If `nT` has no uses, we can remove it.
    *
    * ==========================================================================
    *
    * Copies without uses may appear during elimination, but sometimes backend may create them as well. For example:
    *
    * Optimization in LocalRegAlloc::releasingCost (about only one live node use in next DAG point call) may fail, if
    * value from releasing resource (register) are used in normalization copies already inserted in IR. We may check
    * this copies, if they have uses only in this next DAG point call, but:
    *   1) This situation is completely rare - first appearance is on AWT from checking-test
    *   2) It should be recursive check, and it may take a lot of time in more often situations.
    *
    * Loads without uses may occur in backend. For example:
    *   1) Node X occupy frame slot S1, node Y occupy frame slot S2
    *   2) We need to copy X to register R1 and Y to frame slot S3 (e.g. block end constraints)
    *   3) We generate Z: load S1 -> R1
    *   4) When we want to generate copy S2 -> S3, we need free register. If register pressure is high, it may be R1
    *   5) We generate: load: S2 -> R1, store: R1 -> S3, load: S1 -> R1
    *   6) Z become wild load
    *
    * TODO: improve code ordering to avoid such cases
    *
    * Copies without uses may occur in backend. For example:
    *   1) Node X occupy register R1 and frame slot S
    *   2) We try to release R1 register for some reasons in point with low register pressure
    *   3) We chose to move X from R1 to some free R2 register, because in the future
    *      we would like to use register instead of frame slot
    *   4) We generate Y: copy R1 -> R2 and release R1
    *   5) Then, e.g. invoke node (high register pressure) comes in code ordering, and it spoils R2.
    *      We chose to free R2 and use X in the future from frame slot S.
    *
    * In this scenario, Y become wild copy.
    *
    * TODO: improve register allocator to avoid such cases.
    *
    * ==========================================================================
    *
    * Examples:
    *
    *   1)
    *      nS: ... -> RAX                       | nS: ... -> RAX
    *      ...                                  | ...
    *      nT(nS): RAX -> RAX                   | ...
    *      ...                                  | ...
    *      use(nT)                              | use(nS)
    *
    *   2)
    *      nS: ... -> RAX (may change to RBX)   | nS: ... -> RBX
    *      ...                                  | ...
    *      ... <- RBX is not written or read    | ...
    *      ...                                  | ...
    *      nT(nS, single use): RAX -> RBX       | ...
    *      ...                                  | ...
    *      use(nT)                              | use(nS)
    *
    *   3)
    *      nS: ... -> RAX                       | nS: ... -> RAX
    *      ...                                  | ...
    *      nT(nS): RAX -> RBX                   | nT(nS): RAX -> RBX (removed if no other uses remain)
    *      ...                                  | ...
    *      ... <- RAX is not written            | ...
    *      ...                                  | ...
    *      use(nT, RAX is allowed)              | use(nS)
    *
    * */
  private def eliminateRedundantCopies(): Unit = {

    def mayChangeResultTo(n: Node, r: Resource): Boolean = n match {
      case t @ Transfer(from ~~> _) =>
        applicableResourcesForTransfer(r, from, t.transferArg)

      case _: Phi =>
        // TODO: feel free to look at phi arguments and try to change their results too, if you want to
        false

      case _ if isBoundNode(n) =>
        // TODO: feel free to look at bound argument and try to change its result too, if you want to
        false

      case _ =>
        (resultResourcesSet(n) contains r) &&       // result allocation case allow `r`
          !n.groupRoot.isResultOrSpoiledResource(r) // no others allocation cases already allocated to `r`
    }

    def mayChangeArgumentTo(e: Edge, r: Resource, newSource: Node): Boolean = {
      if (e.target.groupRoot != e.target) {
        return false // TODO: think about attached arguments during PostProcessComponent refactoring (JET-CR-3378 task)
      }

      e.target match {
        case t @ Transfer(_ ~~> to) =>
          applicableResourcesForTransfer(to, r, newSource)

        case _: Constraints =>
          // Use in constraints means that source node used in other CFG blocks. We will not change them now (as we
          // do not analyze CFG), so there is no reason to change use in Constraints. It will increase live range of
          // copy source, and will never decrease live range of copy itself.
          false

        case _: Phi =>
          // TODO: feel free to implement it
          false

        case n if isBoundNode(n) =>
          // TODO: feel free to implement it
          false

        case _: Call =>
          // First of all, this is fast path. Second - call nodeForm cannot be used after backend (as
          // callArgStoreResult brutally cast node to CallArgStore).
          false

        case n =>
          val allowed = nodeForm(n).argumentResources(e) contains r
          if (allowed) {
            // At the moment, there are no nodes that spoil their arguments
            // if such arguments aren't allocated to a fixed resource.
            // Bound nodes and calls are filtered above, other fixed arguments aren't allowing argument changing.
            // See the comment for MachineDescription#argumentWillBeSpoiled
            assert(!argumentShouldBeSaved(e), s"$n $e $r")
          }
          allowed
      }
    }

    def codeRangeNotCrossGCSafeRegion(from: Node, to: Node) = {
      CodeRange(excl(from), excl(to)).nodesForward.collect {
        case c: Call if c.gcActions.generateGCSafeRegion => c
      }.isEmpty
    }

    whileChanged { changed =>
      def stat(copy: Copy, msg: String): Unit = {
        stats.count(StatsKind.RedundantCopiesElimination, "redundant " + copy.simpleName + " " + msg)
        changed.apply()
      }

      for (nT @ Copy(rS ~~> rT) <- all[Copy]) {
        val nS = nT.transferArg

        // 1. Check `nT` for trivial and remove if true
        if (rT == rS) {
          stat(nT, "trivial copy eliminated")
          nT replaceBy nS

        } else // 2. Try to change `nS` result resource to `rT` (`nT` will die as trivial)
          if ((nS.block == nT.block) && // may be loosened by CFG analysis
            (nS.valueUses.size == 1) && // may be loosened by other uses analysis
            mayChangeResultTo(nS, rT) &&
            codeRangeNotCrossGCSafeRegion(nS, nT) &&
            CodeRange(excl(nS), excl(nT)).notWrittenOrRead(rT)) {

          stat(nT, "copy argument resource changed to copy resource")

          nS.resource = rT
          nT replaceBy nS

        } else {
          // 3. Try to change `nT` uses to `nS` (if all uses will be changed, `nT` will die as dead code)
          // Its open question, is it efficient to do this partially, if some uses cannot be changed to `rS`.
          // If among uses there are copies, it may be efficient, because these copies may became trivial.
          nT.replaceUses {
            case e @ Edge(_, nU) if (nU.block == nT.block) && // may be loosened by CFG analysis
              mayChangeArgumentTo(e, rS, nS) &&
              codeRangeNotCrossGCSafeRegion(nT, nU.groupRoot) &&
              CodeRange(excl(nT), excl(nU.groupRoot)).notWritten(rS) =>

              stat(nT, "usage taken from copy")
              nS
          }
        }

        if (nT.isCommitted && nT.uses.isEmpty) {
          stat(nT, "removed (no uses)")
          decommit(nT)
        }
      }
    }
  }

  protected def canBePulledUp(copy: Copy): Boolean = {
    val prev = CodeOrder prev copy
    val Copy(from ~~> to) = copy
    assert(!(prev spoils from))
    !prev.isResultResource(from) && !prev.isResultOrSpoiledResource(to) && !prev.isArgumentResource(to) &&
      (prev != copy.block) && !needGCMap(prev) // Doing so we don't have to analyze value transferred by the copy
  }

  private def insertPreCall(): Unit = {
    def needPreCallXSite(call: Call): Boolean = {
      val withImplicitCheck = call.hasImplicitCheck
      val withGCSafeRegion = call.gcActions.generateGCSafeRegion && targetArch == CBC

      if (!call.hasXSite) {
        // We expect PreCall XSite to always be followed by Call XSite.
        assert(!withImplicitCheck && !withGCSafeRegion)
        return false
      }

      if (withImplicitCheck) {
        assert(call.implicitCheck.isInstanceOf[NullCheck])
        return true
      }

      if (withGCSafeRegion) {
        // Lowering JIT will insert a GC-point with XSite before the foreign call,
        // we need a corresponding entry in .cbc XTable for it, bound to the call instruction start offset,
        // so that if any exception is thrown during this foreign call it could obtain a correct stack trace info.
        return true
      }

      // Following types of calls might be intercepted by a hook,
      // PreCall node is to provide gcmaps for proper execution of hook's managed part.
      call.targetRef match {
        case ref: BytecodeMethodReference if ref.isMemberNameInvoke => true
        case ref => ref.accessKind match {
          case MethodReferenceAccessKind.VIRTUAL | MethodReferenceAccessKind.INTERFACE =>
            true // interface calls with deferred reference type are also covered by this
          case MethodReferenceAccessKind.STATIC | MethodReferenceAccessKind.SPECIAL | MethodReferenceAccessKind.MUT =>
            self.xSiteKind(call) == XSiteKind.DEFERRED_CALL
          case MethodReferenceAccessKind.STATIC_VIRTUAL =>
            true
        }
      }
    }

    for (call <- all[Call] filter needPreCallXSite) {
      val preCall = insertCodeBefore(call) { PreCall() }
      CodeOrder.insertBefore(call, preCall)

      call match {
        case WithImplicitCheck(check) =>
          assert(check.block == preCall.block)
          assert(!(CodeOrder contains check))
          // Reattach the implicit check to the newly-created pre-call node.
          check.detachFromGroup()
          check.attachToGroup(preCall, Group.AttachReason.IMPLICIT_CHECK_ARG)
        case _ =>
      }

      // Here, we have to update the local unmovable registry to have a correct unmovable map at the pre-call XSite.
      // We cannot re-run the unmovable analysis here because all unmovable braces are removed before this point of the
      // compiler pipeline. We also cannot insert the pre-call nodes earlier because in this case the compiler may
      // put some nodes between the pre-call node and the call node. So we have to use a hacky way and just copy the
      // data about the unmovable values from the call node.
      //
      // Strictly speaking, this data is not accurate - it doesn't include unmovable values being passed to the call.
      // But such copying is still safe because:
      // 1. Any unmovable value is alive at least until the endLocalUnmovable call. It means that there must be a slot
      //    (register or a stack slot) containing this value in addition to the argument slot and also marking it as
      //    unmovable.
      // 2. From the runtime point of view, one unmovable reference to an object is enough to consider it unmovable.
      copyLocalUnmovable(from = call, to = preCall)
    }
  }

  private def tryToCombineBoundNodesWithMoves(): Unit = {
    if (!combineSomeBoundNodesWithMoves) return

    for (node <- allNodes if mayBeCombinedWithMov(node)) {
      assert(isBoundNode(node) && (singleElement(node.groupedValueResults) == node))

      CodeOrder prev node match {
        case copy @ Copy(from ~~> to) if (to == node.resource) && from.isReg =>
          assert(copy.singleValueUse == node)
          assert(isSelectedBoundEdge(copy.singleOutEdge))
          CodeOrder remove copy
          copy.attachToGroup(node, Group.AttachReason.BOUND_MOV_ARG)

        case _ =>
      }
    }
  }

  /** Backend-specific IR consistency checks. */
  private def checkBackEndSpecificConsistency(): Unit = checkConsistency(CheckLevels.Optional) {
    // 1. Nodes live ranges checks.
    LiveRanges.enableFor {
      def onSharedResource(node: Node) = node.mayHaveResource && (node.resource match {
        case InvalidResource | Immediate => false
        case _: IReg | _: FReg | _: FrameSlot | _: TailSlot => true
        case _ => shouldNotReachHere()
      })

      // 1.1. Check consistency of live ranges themselves.
      for ((_, nodes) <- groupBy(allNodes filter onSharedResource)(_.resource)) {
        nodes foreach LiveRanges.web // To check assertions in at construction
        val ranges = nodes map LiveRanges.ssa
        ranges foreach { r1 => assert(!ranges.exists { r2 => (r1 != r2) && (r1 intersects r2) }) }
      }

      // 1.2. Implement machine-specific live ranges checks.
      liveRangesChecks() foreach { _.check() }
    }

    // 2. Local unmovable analysis check.
    for ((point, unmovable) <- unmovableNodes) {
      assert(point.isCommitted && (unmovable forall (_.isCommitted)))
    }
  }

  protected def machineDependentPostProcess(): Unit = {}

  /** Post-process script for O2 back-end. */
  def postProcessO2(): Unit = {
    step("constraints corrected", correctConstraints())
    checkBackEndSpecificConsistency()

    step("bulldozer hint nodes removed", removeBulldozerHintNodes())
    step("redundant copies removed",     eliminateRedundantCopies())

    stage(Stage.RecolorFrameSlots) {
      val somethingWasRecolored = step("frame slots recolored", recolorFrameSlots())
      if (somethingWasRecolored) {
        step("redundant copies removed (II)", eliminateRedundantCopies())
      }
    }

    step("implicit checks optimized", optimizeImplicitChecks())

    machineDependentPostProcess()

    step("pre-call inserted", insertPreCall())
    step("bound nodes combined with moves", tryToCombineBoundNodesWithMoves())

    checkBackEndSpecificConsistency()
  }

  /** Post-process script for O1 back-end. */
  def postProcessO1(): Unit = {
    step("constraints corrected", correctConstraints())
    step("pre-call inserted",     insertPreCall())

    checkBackEndSpecificConsistency()
  }
}
