/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

/**
 * Generic loop which repeats operation while there are any changes.
 *
 * @author cypok
 * @author alexm
 */
object WhileChanged {

  /**
   * Callback which can be used to notify that state has changed and more iterations required.
   */
  type SetChanged = (() => Unit)

  /**
   * Repeats body until it runs without calling `SetChanged(true)` callback.
   *
   * @return `true` if at least one change occurred.
   */
  def whileChanged[T](body: SetChanged => T): Boolean = {
    var changed = true
    var wasChanged = false
    while (changed) {
      changed = false
      body(() => { changed = true })
      wasChanged ||= changed
    }
    wasChanged
  }

}
