/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations

import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.WhileChanged._
import com.huawei.excelsior.jet.util.Worklist

/**
 * Framework for IRTransformations.
 *
 * @author conwor
 * @author paul
 */

trait IRTransformationsFramework { self: Universe =>

  def transform(trs: IRTransformation*): Boolean = trs match {
    case Seq(tr) => // only one transformation => don't need to run `whileChanged` loop
      tr.apply()
    case _ =>
      whileChanged { changed =>
        trs foreach { tr => if (tr.apply()) changed() }
      }
  }

  /**
   * Abstract IR transformation, that match some pattern on IR objects and
   * apply transformation, producing and removing some nodes.
   */
  abstract class IRTransformation {
    import PartialFunction.cond

    private var action: Node => Boolean = _
    protected def register(action: PartialFunction[Node, Boolean]): Unit = {
      assert(this.action == null)
      this.action = cond(_)(action)
    }
    protected def register(action: Node => Boolean): Unit = {
      assert(this.action == null)
      this.action = action
    }

    /** Apply transformation to whole IR until a fixed point is reached. */
    def apply(): Boolean = {
      whileChanged { changed =>
        for (node <- allNodes if action(node)) changed()
      }
    }
  }

}
