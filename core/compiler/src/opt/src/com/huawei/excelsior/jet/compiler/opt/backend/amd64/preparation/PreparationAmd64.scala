/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.amd64.preparation

import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.BackEndAmd64
import com.huawei.excelsior.jet.compiler.opt.backend.preparation.Preparation
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait PreparationAmd64 extends Preparation { self: Universe with BackEndAmd64 =>

  protected def tryToRecombineLeaToGroupItWithMemoryAccess(rma: RawMemoryAccess, lea: Lea): Unit = rma match {
    case _: LoadMemory | _: StoreMemory | _: Prefetch | _: CAS | _: MemAtomic => shouldNotCallThis("always can be combined")
    case _ => shouldNotReachHere("unexpected RMA node: " + rma)
  }

  private def convertTestToCmp(): Unit = {
    import Condition.*

    for (test <- all[Test].toList) {
      (test.l, test.r, test.op) match {
        case (x, IConst(0x80000000) | LConst(0x8000000000000000L), op @ (EQ | NE)) =>
          val newOp = if (op == EQ) GE else LT
          replaceTransitively(test, Cmp(x.tpe, newOp)(x, IntegralConst(x.tpe)(0)))

        case _ =>
      }
    }
  }

  private def convertBFXToShift(): Unit = {
    for (bfx <- all[BitFieldExtract].toList if bfx.isGroupRoot) {
      bfx match {
        case ShiftByConst(op @ (ArithOp.ASR | ArithOp.LSR), arg, offs) =>
          replaceTransitively(bfx, Shift(op, arg, IConst(offs)))
        case _ =>
      }
    }
  }

  override protected def machineDependentStepsAfterValueNumberingDisabled(): Unit = {
    optimizeStep("BFX converted to Shift", convertBFXToShift())
  }

  def combineCmpCASWithIf(): Unit = {
    import Condition.*

    def casCanBeCombinedWithCmpUsedInIf(cas: CAS, cmp: Cmp, i: If): Boolean = {
      // If there are control nodes between CAS and If, backend could not guarantee survive of FlagRegister
      if (i.inCtrl != cas) return false

      // If there are floating node controlled or memory-dependent from CAS, backend could not guarantee
      // correct order of them, CAS and If. JET-11077.
      if (cas.uses exists { u =>
        u != cmp && u != i &&
          !u.isInstanceOf[ControlNode]  // Control nodes may have memory uses of CAS, e.g. next CFG block ends.
                                        // They will not compromise combining, because all control nodes are
                                        // strictly ordered and we already check, that If is immediately after CAS.
      }) return false

      true
    }

    for (cas <- all[CAS]) {
      val expected = cas.expectedValue0

      def singleCmpUse = cas.valueUses.toSeq match {
        case Seq(cmp @ Cmp(EQ | NE, `cas`, `expected`)) => Some(cmp)
        case Seq(cmp @ Cmp(EQ | NE, `expected`, `cas`)) => Some(cmp)
        case _ => None
      }

      for (cmp: Cmp <- singleCmpUse) {
        cmp.valueUses.toSeq match {
          case Seq(i: If) if casCanBeCombinedWithCmpUsedInIf(cas, cmp, i) =>
            if (cmp.op == NE) {
              // TODO: consider to normalize Cmp(NE) to Cmp(EQ) in IR
              assert(!identityEnabled)
              enableIdentity()
              If.invert(i)
              disableIdentity()
            }
          case _ => // TODO: support CAS-Cmp-ConVal combining
        }
      }

      for (cmp: Cmp <- singleCmpUse) {
        cmp.valueUses.toSeq match {
          case Seq(i: If) if casCanBeCombinedWithCmpUsedInIf(cas, cmp, i) =>
            assert(i.selector.isInstanceOf[Cmp])
            // TODO-REDESIGN-GROUPS
            val cmpCas = CmpCAS(cmp.keyType, cmp.op, cas.accessType)(cas.addr, cas.expectedValue0, cas.newValue0)
            cmp.replaceBy(cmpCas)
            strikeOut(cas)
            cmpCas.attachToGroup(i, Group.AttachReason.COND_BRANCH_ARG_CAS)
          case _ => // TODO: support CAS-Cmp-ConVal combining
        }
      }
    }
  }

  override protected def recombineFlagProducers(): Unit = {
    // TODO: JET-16891
//    optimizeStep ("CAS Cmp result combine with If",      combineCmpCASWithIf())
    optimizeStep ("Cmp of And converted to Test",        convertAndToTest(Condition.values.toSet))
    optimizeStep ("Test with sign bit converted to Cmp", convertTestToCmp())
  }
}
