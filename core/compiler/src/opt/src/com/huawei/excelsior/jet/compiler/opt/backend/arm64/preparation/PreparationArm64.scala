/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.arm64.preparation

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.StatsKind.MSubPattern
import com.huawei.excelsior.jet.compiler.opt.backend.arm64.BackEndArm64
import com.huawei.excelsior.jet.compiler.opt.backend.preparation.Preparation
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.Arm64CASBackupPath
import xscala.util.MathUtils.isNBits

trait PreparationArm64 extends Preparation { self: Universe with BackEndArm64 =>

  protected def tryToRecombineLeaToGroupItWithMemoryAccess(rma: RawMemoryAccess, lea: Lea): Unit = rma match {
    case _: LoadMemory | _: StoreMemory | _: Prefetch => lea match {
      case _ if memoryAccessCanBeGroupedWithLea(rma, lea) => shouldNotReachHere()

      case Lea.Scaled(_, _, _, disp) if (disp != 0) && isDisplacementAllowed(rma.accessType, disp) =>
        rma.addr = Lea.Base(lea.withDisp(0), disp)

      case Lea.Scaled(base, index, scale, disp) if isScaleAllowed(rma.accessType, scale) =>
        assert(disp != 0)
        rma.addr = Lea.Scaled(Lea.Base(base, disp), index, scale)

      case _ =>
    }

    case _: CAS | _: MemAtomic => // nothing can do

    case _ => shouldNotReachHere("unexpected RMA node: " + rma)
  }

  override protected def machineDependentStepsBeforeTypeChecksDisabling(): Unit = {
    step("mem atomics converted", convertArm64V81MemAtomics())
  }

  override protected def machineDependentStepsBeforeArithLeaCombining(): Unit = {
    optimizeStep("convert msubs", convertMSubs())
  }

  private def convertArm64V81MemAtomics(): Unit = {
    if (!env.enabled(Arm64CASBackupPath)) {
      for (ma <- all[MemAtomic] if ma.kind == MemAtomic.Kind.AND) {
        val xor = Xor(IntegralConst(ma.tpe)(-1), ma.value)
        replaceByCode(ma) { MemAtomic(MemAtomic.Kind.ANDNOT, ma.accessType)(ma.addr, xor) }
      }
    }
  }

  private def convertMSubs(): Unit = {
    for (sub <- all[Sub] if !sub.isFP) sub match {
      case Sub(op3, Mul(op1, op2)) =>
        stats.count(MSubPattern, "Found MSub pattern")
        sub replaceBy MSub(sub.tpe)(op1, op2, op3)
      case _ =>
    }
  }

  /** Lea with huge disp, that does not fit 24 bits, cannot be generated without additional spoiled.
    * Take out such disp into separate based Lea.
    */
  override protected def normalizeLea(): Unit = {
    for (lea <- all[Lea].toList if lea.isGroupRoot) {
      // TODO: this predicate (and some other) already implemented in CodeEmitter
      //  reuse it in some style like emit.canBeGeneratedWithoutScratch
      if (!isNBits(lea.disp, 24) && !isNBits(-lea.disp, 24)) {
        lea.replaceBy(Lea.Scaled(lea.withDisp(0), IntegralConst(lea.tpe)(lea.disp), 1))
      }
    }
  }

  override protected def recombineFlagProducers(): Unit = {
    import Condition.*
    optimizeStep("Cmp with And converted to Test", convertAndToTest(Set(EQ, NE, GE, GT, LT, LE)))
  }
}
