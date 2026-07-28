/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.local

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.util.CriteriaTreeUtil
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

/**
 * Simple generator generates node if there are not anti-dependencies cycle by registers.
 *
 * @author conwor
 */
trait CodeOrdering { self: Universe with BackEnd =>

  trait CodeOrderingImpl extends CriteriaTreeUtil { self: LocalGeneratorImpl =>

    //// Criteria tree /////

    type Candidate = NodeGenOptions

    object StubCriterion extends ExclusionCriterion {
      def apply(candidate: NodeGenOptions): Boolean = true
    }

    object NotBadRPEffect extends ExclusionCriterion {
      def apply(candidate: NodeGenOptions): Boolean = {
        candidate.node match {
          case node: FloatingNode if node.isFragilePointer =>
            // Good RP effect criterion is not applicable to such nodes, as we should not generate them
            // above their upper point. Otherwise we cannot be sure if node not intersects invalidation point.
            false
          case _ => candidate.rpEffect <= 0
        }
      }
    }

    object CommonUseReleasingCriterion extends Criterion(3) {
      def level(candidate: NodeGenOptions): Int = {
        (candidate.isReleasedWithCommonUse, candidate.isReleasedWithCommonUseInThisPoint) match {
          case (false, false) => 0
          case (true , false) => 1
          case (true , true)  => 2
          case _ => shouldNotReachHere()
        }
      }
    }

    object ReleasedWithoutSpillCriterion extends ExclusionCriterion {
      def apply(candidate: NodeGenOptions): Boolean = !candidate.isReleasedWithSpill
    }

    object PointAndSynonymCriterion extends Criterion(3) {
      def level(candidate: NodeGenOptions): Int = {
        if (isSynonym(candidate.node)) 0 else {
          if (lowerPoint(candidate.node) == dag.nextPoint) 1 else 2
        }
      }
    }

    object ReadyToGen extends ExclusionCriterion {
      def apply(candidate: NodeGenOptions): Boolean = !candidate.isBlocked && candidate.normalized
    }

    object NotReadyLevel extends Criterion(3) {
      def level(candidate: NodeGenOptions): Int = (candidate.normalized, candidate.isBlocked) match {
        case (true, true)   => 0
        case (false, false) => 1
        case (false, true)  => 2
        case _ => shouldNotReachHere()
      }
    }

    object IsSynonymCriterion extends ExclusionCriterion {
      def apply(candidate: NodeGenOptions): Boolean = isSynonym(candidate.node)
    }

    val criteriaTreeRoot =
      new CriteriaTree(StubCriterion, Seq(
        new CriteriaTree(ReadyToGen, Seq(
          new CriteriaTree(NotBadRPEffect))),

        new CriteriaTree(PointAndSynonymCriterion, Seq(
          new CriteriaTree(ReadyToGen),
          new CriteriaTree(NotReadyLevel, Seq(
            new CriteriaTree(CommonUseReleasingCriterion, Seq(
              new CriteriaTree(ReleasedWithoutSpillCriterion),
              new CriteriaTree(StubCriterion)))))))))

    /** Tries to remove given `node` from DAG, if it is possible. */
    def tryToRemoveNode(node: Node): Boolean = {
      if (!isSynonym(node)) {
        return false
      }

      if (!state.live(node)) {
        decommit(node)
        return true
      }

      node match {
        case st: Copy =>
          assert(!st.hasOwnValue)
          val nodeResources = state.resources(valueOf(node))
          if (!(st.allowedResults disjointWith nodeResources)) {
            if (node.outEdges.nonEmpty) {
              // We should call this before onDecommit callback works because `remove` from DAG updates `node` uses.
              // TODO: refactor backend DAG (make it more like BGCM BAG).
              dag.remove(node)

              val replacement = state((nodeResources find st.allowedResults.contains).get)
              node replaceValueUsesBy replacement
            }
            decommit(node)
            return true
          }

        case _ =>
      }

      false
    }

    /** @return whether given `node` is normalized, means that all value arguments of node contained into applicable resources. */
    def isNormalized(node: Node): Boolean = {
      isSynonym(node) || { node.groupedValueInEdges forall { edge =>
        !(allowedLocations(edge) disjointWith state.resources(valueOf(edge.source)))
      }}
    }

    /** Inserts transfers for node arguments, that are not contained into applicable resources. */
    def insertNormalization(node: Node): Unit = {
      assert(!isSynonym(node))
      for (edge <- node.groupedValueInEdges; value = valueOf(edge.source)) {
        val allowed = allowedLocations(edge)
        if (allowed disjointWith state.resources(value)) {
          assert(!node.isInstanceOf[ExecEnv])
          val exclude =
            if (rootMethod.hasManagedExecEnv) immSet | eeIRegSet
            else immSet
          moveValue(value, allowed &~ exclude, generate = false)
        }
      }
      debugPrint(2)(s"${node.id} normalized")
    }

    /** Select resource for `allocation`, ensuring it is free to use.
      *
      * In case of free allocation it is just its pre-selected resource. In case of blocked allocation generates all
      * required transfers to release it (not touching `busyForStorage` set of resources) and select either pre-selected
      * resource or one of used to releasing (`freeReg` of `allocation`).
      *
      * The last choice required to support `bound-preference` on [[CBC]] architecture. Consider binary operation OP
      * bound to its first argument. Lets first argument live after OP. The code order and register allocation will be
      * like this:
      *   def X: R0
      *   def Y: R1
      *   mov X: R0 -> R2
      *   OP(X, Y): R0 (X preserved on R2)
      *
      * This code is correct, but in case of [[CBC]] generation we want to combine OP with previous mov to single
      * operation. Unfortunately, previous mov do not preallocate register for OP, but save its argument. So, the
      * better register allocation is the following:
      *   def X: R0
      *   def Y: R1
      *   mov X: R0 -> R2
      *   OP(X, Y): R2 (X preserved on R0)
      */
    private def selectResource(allocation: Allocation, busyForStorage: ResourceSet): Resource = (allocation: @unchecked) match {
      case _: FreeAllocation =>
        allocation.resource

      case ba: BlockedAllocation =>
        val value = valueOf(state(ba.resource))
        val freeRegUsed = releaseResource(value, ba.resource, busyForStorage, ba.freeReg, ba.spillReg)
        if (freeRegUsed && (ba.target != null) && isBoundNode(ba.target)) {
          // At this moment value from `ba.resource` located on at least two resources: `ba.resource` and `freeReg`. We
          // can use any of them for selection. E.g. we can re-check preferred resources information.
          ba.freeReg
        } else {
          ba.resource
        }
    }

    /**
      * Finds and sets applicable nodes for input edges of generated nodeGenOptions.
      * Node is applicable, if it holds the same value, which generated node use,
      * and occupy resource, allowed for input edge.
      *
      * If node is bound, one of input arguments should be allocated on the same
      * resource with generated node.
      * */
    private def fixArgs(node: Node, results: Seq[Resource]): Unit = {
      replaceArgumentsToApplicable(node)

      for ((target, result) <- node.groupResults zip results) {
        if (isBoundNode(target)) {
          val boundArg = state(result)
          val Some(boundEdge) = boundEdges(target) find { edge => valueOf(edge.source) == valueOf(boundArg) }
          boundEdge.source = boundArg
        }
      }
    }

    /**
     * Generates given `nodeGenOptions` or normalize it's args, if it is not normalized.
     * @param nodeGenOptions nodeGenOptions, selected for generation or normalization.
     */
    private def generateNode(nodeGenOptions: NodeGenOptions): Unit = {
      changes = true
      val node = nodeGenOptions.node
      if (!nodeGenOptions.normalized) {
        insertNormalization(node)
        return
      }

      // If node has constraints, and this constraints are not normalized, we could not generate node.
      // Instead of this, we normalize node constraints and continue crown processing.
      // TODO: instead of this hack we should implement nodes grouping in generation (used for Div-Rem, Xchg, operations with memory, e.t.c.).
      node match {
        case p: LowerPoint if p.hasConstraints && !isNormalized(p.constraints) =>
          insertNormalization(p.constraints)

        case _ =>
          val allocations = nodeGenOptions.results ++ nodeGenOptions.spoiled
          val busyForStorage = setOf(allocations.map { _.resource })

          val results = nodeGenOptions.results.map(selectResource(_, busyForStorage))
          val spoiled = nodeGenOptions.spoiled.map(selectResource(_, busyForStorage))
          fixArgs(nodeGenOptions.node, results)
          generateNode(node, results, spoiled)
      }
    }


    /** Flag of changes during last process iteration. */
    private var changes: Boolean = true

    /** Implements one step of local code generation and returns whether something has been changed during this step. */
    def makeOneStep(): Boolean = {
      changes = false
      criteriaTreeRoot.cleanTree()
      var bestNodeGenOptions: NodeGenOptions = null
      var bestCost: Int = -1
      dag.processCrown { node =>
        if (!tryToRemoveNode(node)) {
          val nodeGenOptions = NodeGenOptions(node, isNormalized(node))
          val cost = criteriaTreeRoot.calcCost(nodeGenOptions)
          debugPrint(3)(s"${node.id} gen options calculated with cost $cost")
          if (cost == 0) {
            generateNode(nodeGenOptions)
            criteriaTreeRoot.cleanTree()
            dag.dropProcessed()
            bestNodeGenOptions = null
            bestCost = -1
          } else if (cost != -1) {
            val better = (bestCost == -1) ||
              (bestCost > cost) ||
              ((bestCost == cost) && inPreOrder(nodeGenOptions.node, bestNodeGenOptions.node))

            if (better) {
              bestNodeGenOptions = nodeGenOptions
              bestCost = cost
            }
          }
        }
      }
      dag.dropProcessed()
      if (bestNodeGenOptions != null) generateNode(bestNodeGenOptions)
      changes
    }
  }
}
