/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.pdb.archive

import com.huawei.excelsior.jet.common.XString

import Index.*

/** Readable & writable `Index`. Can be seen as mutable (name => entryID) map.
  *
  * @author paul
  */
trait RWIndex extends Index {
  /** Adds new pair (name, id) to this `RWIndex`.
    * If an entry with the same `name` already exists in this `RWIndex`, then:
    * - if `allowReplace` is `false`, do nothing;
    * - otherwise replace existing entry (assign new EntryID to it),
    * Returns previous entryID of existing entry with the same `name` or NO_ENTRY */
  def add(name: XString, id: EntryID, allowReplace: Boolean): EntryID

  //def remove(name: XString): Unit
}