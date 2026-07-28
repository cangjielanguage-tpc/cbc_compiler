/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, ConstBranchElimination, Universe}
import transformations.{IRTransformationsCollection, LoopsNormalizer}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{CollectFailStats, OptimizationLoop, UnstableSSA}
import com.huawei.excelsior.jet.compiler.options.NumOption.*
import com.huawei.excelsior.jet.compiler.opt.CompilerException
import com.huawei.excelsior.jet.compiler.opt.middle.explosion.{Explosion, PreExplosion}
import com.huawei.excelsior.jet.compiler.opt.middle.sync.*
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.*
import com.huawei.excelsior.jet.compiler.opt.middle.types.CompileTimeComputations
import com.huawei.excelsior.jet.compiler.util.Log
import com.huawei.excelsior.jet.compiler.util.Log.Kind
import com.huawei.excelsior.jet.util.WhileChanged.*
import xscala.time.LocalDateTime

/**
 * IR optimizer.
 *
 * @author conwor
 */

trait Optimize extends IRTransformationsCollection
                  with DiamondMerge
                  with IteratorAbsorption
                  with LoopPeeling
                  with LoopPredication
                  with LoopUnrolling
                  with FullLoopUnrolling
                  with LoopStreamlining
                  with GradientInvariantLifting
                  with VarProcessor
                  with DCEComponent
                  with UCEComponent
                  with IdempotentOperationsOptimizer
                  with InterfaceOperationsOptimizer
                  with SimplifyComponent
                  with ConstBranchElimination
                  with UnnecessaryOperationsElimination
                  with EagerPreparationChecksElimination
                  with MemoryOptimizations
                  with PreExplosion
                  with Explosion
                  with CrossroadsOptimizer
                  with TypeFiltersAbsorption
                  with SwitchDiamondAbsorption
                  with SwitchAggregation
                  with SynchronizationElimination
                  with SynchronizationOptimization
                  with ArrayIndexCheckOptimizer
                  with ArrayIndexCheckLoopVersioning
                  with CompileTimeComputations
                  with EscapeWriteBarriersOptimization
                  with LoopsNormalizer
                  with UselessLoopElimination
                  with CangjieLoopOptimization
                  with BooleanReconstruction
                  with EquivalentPhiesElimination
                  with PairedRawDataAccessOptimization
                  with ContextTypesRecalculation
                  with ExplicitNullCheckFolding
                  with TypeEmptyUnreachableCodeElimination
                  with CheckedOpStrengthReduction { self: Universe =>

  private def log = Log(Kind.Optimize)

  def optimize(): Unit = stage(Stage.OptimizeLoop) { log.inSession("optimize loop", codeUnit) {
    // all optimizations should be done on see of nodes
    requireNoGlobalCodeMotion()

    var iterCount = 1

    def logOpt(name: String) = {
      log(s"[${LocalDateTime.now}] ($iterCount) $name")
    }

    logOpt("start")

    whileChanged { changed =>

      if (iterCount > env.valueOf(MaxOptimizeIterations)) {
        throw new CompilerException(s"Optimizations seem to loop endlessly (already passed ${iterCount - 1} iterations)")
      }

      def optimize(name: String, result: Boolean): Unit = {
        if (result) {
          logOpt(name)
          dbgPrinter.debugNodes("(" + iterCount + ") after " + name)
          checkIRConsistency(CheckLevels.Optional)
          changed()
        } // TODO: else check that IR is not changed
      }

      def transform(tr: IRTransformation): Unit = {
        optimize(tr.toString, tr.apply())
      }

      dbgPrinter.debugGraphs(s"($iterCount) temperature", printNodesGraph = false, info = dgiForColdCode)

      if (!isO1Compiled && env.enabled(OptimizationLoop)) {

        transform(EmptyBlocksElimination)
        transform(MultiEdgeElimination)
        transform(BlocksConnectionTransformation)
        transform(DefaultHandlersElimination)
        transform(PhiToCondValReplacing)
        transform(BoxingEqualitySimplification)
        transform(CheckCastNullCheckSwapping)

        optimize("boolean reconstruction", reconstructBooleanTypes())
        optimize("loops normalization", normalizeAllLoops())


        optimize("SSA completion", completeSSA())

        if (env.enabled(UnstableSSA) && iterCount % env.valueOf(UnstableSSAPeriod) == 1) {
          // Cleanup before SSA destabilization.
          // A cleanup is needed to avoid vars in unreachable or dead code.
          // Note: This is not performance-critical code, so we can simply perform both UCE and DCE here.
          optimize("UCE", eliminateUnreachableCode())
          optimize("DCE", eliminateDeadCode())
          optimize("SSA destabilization", destabilizeSSA())
        }

        optimize("UCE", eliminateUnreachableCode())
        optimize("DCE", eliminateDeadCode())
        optimize("block memory optimization", optimizeBlockMemory())
        optimize("context types recalculation", recalculateContextTypes())
        optimize("eager preparation checks elimination", eliminateEagerPreparationChecks())

        optimize("simplify", simplifyIR())
        optimize("redundant checked op elimination", optimizeRedundantCheckedOp())
        optimize("paired raw data access optimization", optimizePairedRawDataAccesses())
        optimize("write barrier optimization", optimizeWriteBarriers())
        optimize("combine memory loads and casts", combineLoadMemoryAndCast())
        optimize("get memory elimination", optimizeMemoryReads())
        optimize("synchronized region slicing", sliceSynchronizedRegions())
        optimize("nested synchronization elimination", eliminateNestedSynchronization())
        optimize("synchronization elimination on new", eliminateSynchronizationOnNew())
        optimize("synchronization coarsening", mergeSynchronizedBlocks())
        optimize("express explosion", expressExplodeAllObjects())
        optimize("pre-explosion", preExplodeObjects())
        optimize("explosion", explodeAllObjects())
        optimize("diamond dust optimization", optimizeDiamondDust()) // should be called before duplicate ifs optimization
        optimize("duplicate ifs optimization", optimizeDuplicateIfs())
        optimize("consecutive mem barriers optimization", optimizeConsecutiveMemBarriers())
        optimize("equivalent phi elimination", eliminateEquivalentPhies())

        optimize("unnecessary operations elimination", eliminateUnnecessaryOperations())
        optimize("idempotent operations elimination", optimizeIdempotentOperations())
        optimize("interface optimizations", optimizeInterfaceOperations())

        optimize("array index check optimization", optimizeArrayIndexChecks())
        optimize("array index check loop versioning", versionArrayIndexCheckLoops())

        optimize("iterator absorption", absorbIterators())

        optimize("const branch elimination", eliminateConstBranches())
        optimize("specialized crossroads optimization", optimizeSpecializedCrossroads()) // should be called right after const branch elimination
        optimize("diamonds optimization", optimizeDiamonds()) // should be called after duplicate ifs optimization
        optimize("type filters absorption", absorbTypeFilters())
        optimize("switch aggregation", aggregateSwitches())
        optimize("switch diamonds absorption", absorbSwitchDiamonds())

        // Loop predication should be called after idempotent operations elimination (and get memory elimination),
        // and before loop peeling, because it is more expensive than idempotent elimination
        // and less expensive than peeling in terms of code size.
        optimize("loop predication", predicateLoops())
        optimize("explicit null check folding", foldExplicitNullChecks())

        optimize("full loop unrolling", fullyUnrollLoops())

        // Loop peeling should be called after idempotent operations elimination (and get memory elimination),
        // because peeling uses idempotent operations as motivation and inserts Vars,
        // which may prevent idempotent elimination on the same iteration as peeling.
        // So if peeling is done before idempotent elimination,
        // then on the next iteration there will still be motivation for peeling (because idempotents were not eliminated),
        // loops will be peeled again and IR will again have Vars which prevent elimination, and so on...
        optimize("loop peeling", peelLoops())

        optimize("compile time computation", computeCompileTime())

        optimize("cangjie for-in loop simplification", simplifyCangjieForInLoops())
        optimize("simplify residual for-in loops", simplifyResidualCangjieForInLoops())
        optimize("useless loop evaluation", evaluateUselessLoops())
        optimize("useless loop elimination", eliminateUselessLoops())
        optimize("zero loop elimination", eliminateZeroLoops())
        optimize("empty IC regions elimination", eliminateEmptyICRegions())

        optimize("loop unrolling", unrollLoops())

        optimize("loop streamlining", streamlineLoops())
        optimize("gradient invariant lifting", liftGradientInvariants())

      } else {
        // Ensure some IR invariants in case optimization loop was disabled.

        // Lowering cannot tolerate preparation checks in eager preparation mode.
        optimize("eager preparation checks elimination", eliminateEagerPreparationChecks())

        // Backend cannot tolerate node False().
        optimize("const branch elimination", eliminateConstBranches())

        // This is overall useful to not lower nodes in unreachable code.
        optimize("UCE", eliminateUnreachableCode())

        // Backend preparation cannot tolerate dead nodes left after lowering.
        optimize("DCE", eliminateDeadCode())

        // Need to re-commit all nodes because they can be re-committed in Preparation
        // and result in False() reaching Backend again.
        optimize("simplify", simplifyIR())

        // These transformations significantly reduce IR (especially of AJ-inline noodles) and accelerate backend.
        transform(EmptyBlocksElimination)
        transform(MultiEdgeElimination)
        transform(BlocksConnectionTransformation)
      }

      iterCount += 1
    }

    checkIRConsistency(CheckLevels.Desirable)
    if (!isO1Compiled && env.enabled(OptimizationLoop)) {
      checkConsistency(CheckLevels.Desirable) { IdempotentOperationsOptimizer.checkCoverage() }
      checkConsistency(CheckLevels.Desirable) { checkTypeEmptyEliminationHasNoEffect() }
      checkConsistency(CheckLevels.Desirable) { checkTypeEmptyUses() }
    }
  }}

  def collectOptimizationFailStats(): Unit = {
    if (env.enabled(CollectFailStats) && !isO1Compiled) {
      // Fail stats collection disabled for O1 because it surprisingly starts to optimize IR, leaving some AssignVar
      // nodes and other junk. Anyway fail stats collection need only for fun-tests which ignore optimizations stats.
      var changed = false
      changed |= explodeAllObjects(collectFailStats = true)
      changed |= preExplodeObjects(collectFailStats = true)
      changed |= versionArrayIndexCheckLoops(collectFailStats = true)
      changed |= peelLoops(collectFailStats = true)
      changed |= absorbTypeFilters(collectFailStats = true)
      changed |= predicateLoops(collectFailStats = true)
      changed |= fullyUnrollLoops(collectFailStats = true)
      changed |= unrollLoops(collectFailStats = true)
      assert(!changed)
    }
  }
}
