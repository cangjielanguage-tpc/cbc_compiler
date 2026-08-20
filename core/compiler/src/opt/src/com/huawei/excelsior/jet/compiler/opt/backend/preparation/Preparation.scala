/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.preparation

import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.DCEComponent
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenCoverageInCBC

/** Preparation before backend. Machine-specific rematerialization, groups combination, ...
  *
  * @author conwor
  */
trait Preparation extends SimpleSteps with SpecialSteps with RMACombining
  with ArithCombining with FlagProducersPreparation with DCEComponent { self: Universe with BackEnd =>

  /** Preparation step used only in full optimizing compilation mode, not in FastBE mode. */
  protected def optimizeStep[T](name: String, action: => T): Unit = if (!isO1Compiled) step(name, action)

  protected def machineDependentStepsBeforeTypeChecksDisabling(): Unit = {}

  protected def machineDependentStepsAfterValueNumberingDisabled(): Unit = {}

  protected def machineDependentStepsBeforeArithLeaCombining(): Unit = {}

  private def replaceVoidTypesWithVoid(): Unit = {
   for (n <- allNodes if n.tpe == VoidType) (n: @unchecked) match {
     case _: Void =>
     case x: (Call | Param | UniversalGeneric.FromHolder) => x.replaceValueUsesBy(Void())
     case x => assert(x.valueUses.isEmpty, x)
    }
  }

  def prepareIR(): Unit = stage(Stage.Preparation) {
    if (rootMethod.isNonThrowing) {
      assert(all[XPoint].forall(_.hasHandler),
        s"Method ${rootMethod.getFullName} has an @NonThrowing annotation, but has xPoints without xHandler:\n\t" +
          all[XPoint].filterNot(_.hasHandler).map(_.pos).mkString("\n\t"))
    }

    step         ("replace VoidType valueUses with Void node", replaceVoidTypesWithVoid())
    step         ("SOE throws flags updated",                  updateSOEThrowsFlags())
    step         ("catches replaced",                          replaceCatches()) // before unmovable analysis so that it sees new Calls
    step         ("convert domain nodes replaced by calls",    replaceConvertDomainByCalls())
    step         ("local unmovable analysed",                  processLocalUnmovable())
    step         ("phi webs translated and phies optimized",   translatePhiWebs())
    step         ("imported indices resolved",                 resolveImportedIndices())
    step         ("markers removed",                           removeMarkers()) // TODO-FAST-BE: is it really required (in terms of compilation time)?
    optimizeStep ("no trusted checks ensured",                 ensureNoTrustedChecks())
    step         ("explicit stack alloc zeroing inserted",     insertStackAllocZeroing())
    optimizeStep ("fatal errors inserted before halts",        insertFatalErrorBeforeHalt())
    step         ("call targets grouped",                      groupCallTargets())
    step         ("tail parameters prepared",                  prepareTailParameters())
    step         ("unused value args replaced to null",        replaceUnusedValueArgs())

    machineDependentStepsBeforeTypeChecksDisabling()

    if (env.enabled(GenCoverageInCBC) && rootMethod.hasSourceFile) {
      step("coverage counter inserted", insertCoverageCounter())
    }

    disableTypeChecks()

    step         ("redundant casts removed",          removeRedundantCasts())
    step         ("MutFunc nodes preparation",        prepareMutFuncNodes())
    step         ("DerivedPtr preparation",           prepareDerivedPtr())
    step         ("RecordArrayGet preparation",       prepareRecordArrayGet())
    step         ("CangjieReferenceNode preparation", prepareCangjieReferenceNode())
    step         ("Lea created",                      createLeaForRMA())
    step         ("value range filters removed",      removeValueRangeFilters())
    step         ("TDBarriers inserted",              protectNodesWithTDBarriers())

    disableIdentity()

    step("Lea recombined, rematerialized and grouped with RMA", recombineRematerializeAndGroupRMAAndLea())

    disableValueNumbering()
    machineDependentStepsAfterValueNumberingDisabled()

    optimizeStep ("load and bfx grouped",            groupLoadAndBFX())
    optimizeStep ("convert bfx to and",              convertBFXToAnd())
    step         ("CallArgStores inserted",          insertCallArgStores())
    optimizeStep ("Neg operations sifted down",      siftNegsDown())

    machineDependentStepsBeforeArithLeaCombining()

    step         ("Add operations converted to Lea", convertAddToLea())
    step         ("Lea normalized",                  normalizeLea())
    step         ("replace non-Lea ExecEnv uses",    replaceNonLeaEEUses())
    optimizeStep ("flag producers recombined",       recombineFlagProducers())
    step         ("flag producers rematerialized",   rematerializeFlagProducers())
    step         ("dead code eliminated",            eliminateDeadCode())
    optimizeStep ("split huge blocks",               splitHugeBlocks())

    checkIRConsistency(CheckLevels.Important)
  }
}
