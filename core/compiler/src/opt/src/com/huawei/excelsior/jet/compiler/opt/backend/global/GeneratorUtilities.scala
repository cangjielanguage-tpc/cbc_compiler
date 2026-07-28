/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.global

import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

/**
 * Common utilities of code generation.
 *
 * @author conwor
 */
trait GeneratorUtilities { self: Universe with BackEnd =>

  /** Removes not generated synonyms from given `node` in given `block`. */
  def removeSynonyms(node: Node, block: Block): Unit = {
    val notGeneratedSynonyms = node.uses filter { use => use.block == block && !use.generated && isSynonym(use) }
    for (synonym <- notGeneratedSynonyms.toList) {
      synonym match {
        // In case of removing synonyms from FrameSlot during it's releasing,
        // there could be pairs of (load-store) that should be removed together.
        case s: Copy =>
          assert(!s.hasOwnValue)
          removeSynonyms(s, block)
        case _ =>
      }
      assert(synonym.outEdges.isEmpty)
      decommit(synonym)
    }
  }
}
