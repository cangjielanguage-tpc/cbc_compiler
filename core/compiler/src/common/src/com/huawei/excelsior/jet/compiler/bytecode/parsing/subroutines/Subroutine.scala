/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing.subroutines

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Subroutine descriptor. */
final class Subroutine[B](val entryBlock: B) {
  private var _retBlock: B = _
  private val _jsrs = ArrayBuffer.empty[JsrInfo[B]]

  def retBlock: B = {
    assert(hasRet)
    _retBlock
  }

  def jsrs: collection.Seq[JsrInfo[B]] = {
    assert(hasRet)
    _jsrs
  }

  /** Correctly handles multiple connections of the same JSR. Returns if given JSR is a new one. */
  def connectToJsr(jsrInfo: JsrInfo[B]) = {
    if (!_jsrs.contains(jsrInfo)) {
      _jsrs += jsrInfo
      true
    } else {
      false
    }
  }

  def hasRet: Boolean = _retBlock != null

  /** Should be called only once for the subroutine. */
  def connectToRet(retBlock: B): Unit = {
    assert(!hasRet)
    _retBlock = retBlock
    assert(hasRet)
  }
}
