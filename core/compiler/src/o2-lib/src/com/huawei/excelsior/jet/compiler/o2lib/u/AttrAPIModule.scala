/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.compiler.Env.isWorkMode
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule.SymIO
import com.huawei.excelsior.jet.util.ScalaCollections.iterateUntilNull
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

object AttrAPIModule {

  abstract class FEXT {
    var kind: Byte = _
    private[AttrAPIModule] var next: FEXT = _

    def internalize(si: SymIO): Unit
    def externalize(si: SymIO): Unit
  }

  class Attributable {
    private[AttrAPIModule] var _fext: FEXT = _

    def fexts: Iterator[FEXT] = iterateUntilNull(_fext)(_.next)

    def addFEXT(fext: FEXT, kind: Byte): Unit = {
      if (isWorkMode) {
        assert(getFEXT(kind) == null)
      }
      fext.kind = kind
      fext.next = _fext
      _fext = fext
    }

    def getFEXT[F <: FEXT](kind: Byte): F = {
      var f = _fext
      while (f != null && f.kind != kind) {
        f = f.next
      }
      f.asInstanceOf[F]
    }

    def fextOption[F <: FEXT](kind: Byte): Option[F] = Option(getFEXT(kind))

    def hasFEXT(kind: Byte): Boolean = getFEXT[FEXT](kind) != null

    def tryAddFEXT(fext: FEXT, kind: Byte): Unit = if (!hasFEXT(kind)) addFEXT(fext, kind)
    
    def hasFEXTs: Boolean = _fext != null
    
    def cleanFEXTs(): Unit = _fext = null
  }
}
