/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

/** Analysis result computed by [[DataFlowAnalyzer]]. */
enum DataFlowMergeResult {

  /** Returned when initial information is merged into bottom-state
    * (or state which is not yet used as input state to process any block).
    * Successors should be processed but no need to reprocess whole graph.
    */
  case INITIALIZED

  /** Returned when new information is merged into existing state.
    * Successors should be reprocessed.
    */
  case CHANGED

  /** Returned when no new information is merged into existing state.
    * Successors should not be processed.
    */
  case UNCHANGED
}
