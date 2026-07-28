/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.cangjie

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.ir.nodes.HLIRNodes

trait CangjieParsingCleanup { self: Universe with HLIRNodes =>

  def simplifyLocalVariables(): Boolean = {
    withIncrementalGCM {
      var changed = false
      for (n <- all[StackAlloc]) n.kind match {
        case _: FrameSlot.DebugVar =>
          // nothing to do, all debug var stack allocation slots should be preserved

        case kind: FrameSlot.Typed if !kind.allocType.isRecord && n.uses.forall(_.isInstanceOf[LoadStoreMemoryAccess]) =>
          // Note: local primitive slots can be passed by-reference to C FFI.
          withNewVar(ValueType.fromSig(kind.allocType)) { (assignAt, readAt) =>
            n.uses.toList foreach {
              case u: StoreMemory =>
                assert(u.signature == kind.allocType, s"unexpected $u to $n")
                assignAt(u.inCtrl, u.storedValue())
                // If it is debug variable but we do not want to support its modification in debugger,
                // we may replace LoadMemory but should preserve StoreMemory to track variable value in debugger.
                if (!kind.isInstanceOf[FrameSlot.DebugVar]) {
                  strikeOut(u)
                }

              case u: LoadMemory =>
                assert(u.signature == kind.allocType, s"unexpected $u from $n")
                u.replaceBy(readAt(upperPoint(u)))

              case u => shouldNotReachHere(s"unexpected use of $n: $u")
            }
          }
          changed = true

        case _ =>
      }

      changed
    }
  }

}
