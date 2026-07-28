/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.local

import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.BulldozerGCM
import com.huawei.excelsior.jet.compiler.opt.backend.fast.FastCodeOrdering
import com.huawei.excelsior.jet.compiler.opt.backend.local.nodegenoptions.*
import com.huawei.excelsior.jet.compiler.opt.backend.{BackEnd, NodesDescription}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.Numbering
import com.huawei.excelsior.jet.util.ScalaCollections.sumBy

/** Local generator could generate one block with given input state, output constraints and exception constraints
  *
  * @author conwor
  */
trait LocalGenerator extends NodeGenOptionsComponent with CodeOrdering with NodesDescription with LocalRegAlloc { self: Universe with BackEnd =>

  class LocalGeneratorImpl(protected val block: Block,
                           protected val state: GeneratorState,
                           protected val globalGenerator: GlobalGeneratorImpl)
    extends CodeOrderingImpl with NodeGenOptionsImpl with LocalRegAllocImpl {

    var dag: GenerationDAG = _

    /** Raw generation of given `node`.
      *   - Saves `results` and `spoiled` information about resources into `node` fields
      *   - Save `node` in resources state
      *   - Ties node in generation list and updates dag crown
      *   - Makes debug print.
      */
    private def generateRaw(node: Node, results: Seq[Resource], spoiled: Seq[Resource]): Unit = {
      assert(node.isGroupRoot)
      dag.tie(node)

      // TODO: spoil different sets of registers on control and xcontrol exits
      for (r <- results ++ spoiled) state.spoilResource(node, r, block)
      node.spoiled = spoiled

      state.freeLocalUsages(node)

      for ((target, result) <- node.groupResults zip results) {
        target.generated = true

        if (hasValue(target)) {
          if (state.live(target)) {
            state.add(target, result)
          } else {
            target.resource = result // allocated resource may be used in code generator assertions
          }
        }
      }

      debugPrint(2)(s"${node.id} generated")
    }

    /** Replaces given `node` arguments to applicable (contained in resources, allowed to arguments). */
    protected def replaceArgumentsToApplicable(node: Node): Unit = {
      if (isSynonym(node)) {
        return
      }

      val isConstraints = node.isInstanceOf[Constraints]
      for (edge <- node.groupedValueInEdges; value = valueOf(edge.source)) {
        val resource = edge.source.resource
        val allowed = allowedLocations(edge)
        val isOnPlace = (resource == Immediate) || (state.contains(resource) && (state(resource) == edge.source))
        if (!isOnPlace || !allowed.contains(resource)) {
          val selectFrom = allowed & state.resources(value)
          val selectedResource = selectFrom find { _.isReg } getOrElse selectFrom.head
          assert(value.producer.isInstanceOf[ExecEnv] == (selectedResource == frame.EER))
          edge.source = state.getNode(value, selectedResource)
        }
      }
    }

    /** Generate given `node`, allocated on given `results`, without any normalization.
      * Updates all live uses and busy resources caches, resources state, dag crown and
      * generation list. Implements some special actions, like additional stack alloc and
      * exception state fork.
      */
    def generateNode(node: Node, results: Seq[Resource], spoiled: Seq[Resource]): Unit = {
      registerNodeInFrame(node)

      def storeValueIfRequired(value: Value): Unit = {
        if (!state.savedInStorage(value)) {
          moveValue(value, setOf(newSpillSlotUsedAsWorkaroundFor15742(value)), generate = true)
        }
      }

      node match {
        case hint: BulldozerHint =>
          val value = valueOf(hint.node)

          if (hint.store) storeValueIfRequired(value)

          if (hint.spillAssert) assert(state.registers(value).isEmpty)

          if (hint.spill) {
            for (r <- state.registers(value).asImmutable) {
              state.takeResourceFromValue(value, r)
            }
          }

        case call: Call if call.gcActions.generateGCSafeRegion && isO1Compiled =>
          // For optimizing back-end these spills inserted in BGCM.
          // TODO-FAST-BE: try to emulate this in FastCodeOrdering, not in RegAlloc.
          val args = call.groupedValueArgs.map(valueOf).toSet
          for (r <- allIRegsSet.iterator if state contains r;
               ref = state(r) if mayBeTraceableReference(ref);
               value = valueOf(ref) if !args(value)) {
            storeValueIfRequired(value)
            state.takeResourceFromValue(value, r)
          }

        case _ =>
      }

      generateRaw(node, results, spoiled)

      node match {
        case p: LowerPoint if p.hasConstraints =>
          val constraints = p.constraints
          assert(isNormalized(constraints))
          replaceArgumentsToApplicable(constraints)
          constraints.generated = true

          p match {
            case sp: SpinalNode if sp.hasXHandler =>
              // TODO: fix possible bug. `node` results are already in state (in `generateRaw`), but they may not be there for xHandler
              val st = state.fork()
              st.startSession()
              globalGenerator.addXCtrl(sp.xpoint, st)

              // We don`t free local usages in forked states, because they will be used in mergeFrom procedure.
              state.freeLocalUsages(constraints)
            case _ =>
          }
        case _ =>
      }
    }

    /** Generates special nodes, like block itself, phi-functions, method arguments, proxies.
      * Used to not create special cases for this nodes in common generation algorithm.
      */
    private def generateSpecialNodes(): Unit = {
      dag.tie(block)
      for (node <- block.pinnedNodes) { node match {
        case _: Phi | _: Param | _: TailPointer | _: Proxy => generateRaw(node, node.allResultResources, node.spoiled)
        case _ =>
      }}
    }

    /** Debug print with given `message`. */
    def debugPrint(level: Int)(message: String): Unit = beDebugPrint(level)(message, { b =>
      if (b == block) {
        "\n" + state.valuesInfo() +
          "\n" + (if (dag != null) "CROWN: " + dag.crown.mkString("(", ", ", ")") + "\n" else "") +
          "\n" + "TREE: " + "\n" + criteriaTreeRoot.debugString("")
      } else {
        ""
      }
    })

    private val codePreOrder = Numbering(CodeOrder in block)

    /** Returns true, iff given `x` and `y` are ordered in code pre-order ([[BulldozerGCM]] or [[FastCodeOrdering]]). */
    def inPreOrder(x: Node, y: Node): Boolean = {
      codePreOrder.contains(x) && codePreOrder.contains(y) && codePreOrder.lt(x, y)
    }

    /** Main procedure of block code generation. Prepares resources state, DAG, live uses and
      * busy resources caches, generates special nodes, normalizes initial nodes and invokes
      * SimpleGenerator.
      */
    def generateBlock(): Unit = {
      debugPrint(1)(s"before block ${block.id} generated")

      CodeOrder clearIn block

      dag = new GenerationDAG(block)
      state.startSession()
      state.initValues(block)

      generateSpecialNodes()
      debugPrint(1)(s"after block ${block.id} prepared")

      dag.withCallbacks {
        val blockConstraints = block.blockEnd.constraintsOption ++ block.spine.flatMap(_.constraintsOption)
        val limit = (block.nodes.size + sumBy(blockConstraints)(_.arity - 1)) * 300
        while (!block.blockEnd.generated) {
          if (dag.size > limit) {
            shouldNotReachHere("local gen limit exceeded")
          }
          if (!makeOneStep()) {
            shouldNotReachHere("nothing to generate")
          }
        }
      }

      debugPrint(1)(s"after block ${block.id} generated")
    }
  }
}
