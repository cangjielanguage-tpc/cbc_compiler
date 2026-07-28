/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

/** Ordinary (undirected) graph.
  *
  * @author conwor
  * @author paul
  */
abstract class UGraph[N] {

  /** Returns iterator over given `node` neighbourhood. May return duplicated elements. */
  def neighbours(node: N): Iterator[N]

  /** Returns set of all neighbours of given `node`. */
  def neighboursSet(node: N): collection.Set[N] = neighbours(node).toSet

  /** Returns true, iff given `x` and `y` nodes are adjacent. */
  def adjacent(x: N, y: N): Boolean = neighbours(x) contains y
}
