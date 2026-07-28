/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.sync

import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait SyncParamsAnalysis { this: Universe =>

  def allSyncedParams: Iterator[Node] = {
    // TODO: rework this approach - JET-13451
    for {
      call @ AnyDirectCall(target) <- allNodes
      if target.getParamsCount > 0
      info <- locallyAnalyzeMethod(target).toList // Option.toList to make monad Gods happy
      syncedParamNum <- info.syncedParams
      arg = call.invokeArgs(syncedParamNum)
    } yield arg
  }
}
