/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.frame

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.MemBased
import com.huawei.excelsior.jet.compiler.Env.stackSlotSize
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenDebug
import xscala.util.MathUtils.isAligned

/** Part of [[Frame]] encapsulates `DWARF` debug support - prologue/epilogue labels and caller frame info tracking.
  * 
  * TODO: use FP always in [[GenDebug]] mode and simplify caller frame info tracking
  *
  * @author conwor
  * @author gatimosh
  */
trait FrameDebug { self: Frame[_, _, _] =>
  protected val genDebug = env.enabled(GenDebug)

  protected var currentCallerSP: MemBased = _
  protected var currentCallerRA: MemBased = _

  protected def initCallerFrameInfo(): Unit

  protected def isCallerFrameInfoSupported = {
    // TODO-DWARF:  this predicate works for NativeWrapperGenerator as it generates body and performs
    //              allocateFakeParamsArea|addStackPointer before prologue generation (initCallerFrameInfo has not been called).
    //              Because of this for both Cangjie and Java we have incorrect CFI program near any native call
    //              to fix this we need to develop more complex CFI generation scheme that works even when method body
    //              is generated earlier than prologue.
    genDebug && currentCallerSP != null && currentCallerRA != null
  }

  protected def updateCallerFrameInfo(spAddend: Int): Unit = {
    assert(isAligned(spAddend, stackSlotSize))
    if (isCallerFrameInfoSupported) {
      currentCallerSP = currentCallerSP.disposed(spAddend)
      currentCallerRA = currentCallerRA.disposed(spAddend)
      genCallerFrameInfo()
    }
  }

  protected def genCallerFrameInfo(callerRA: Location = currentCallerRA): Unit = if (genDebug) {
    emit.bind(DebugLabels.CallerFrameInfoLabel(currentCallerSP, callerRA))
  }

  protected def prologueEndLabel(): Unit = if (genDebug) {
    // TODO-DWARF-CANGJIE:  for CJ it's disabled, because for Cangjie prologue end bind in CodeGenerator after all argument var assigns emitted
    // TODO-DWARF-JAVA:     for Java it is ON for now as we do not support variables in opt for DWARF so cangjie scheme can not be applied
    // TODO-DWARF:          think about normal way of it for both baseline and opt
    // emit.bind(DebugLabels.PrologueEndLabel())
  }

  protected def epilogueBeginLabel(): Unit = if (genDebug) {
    emit.bind(DebugLabels.EpilogueBeginLabel())
  }
}
