/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.devirtualization

/** Encapsulates extra information about the origin of inserted tau-test node. */
sealed abstract class TauInfo

object TauInfo {

  /** Information is missing or lost during optimizations. */
  case object Unknown extends TauInfo

  /** Tau-test was inserted using static heuristics.
    * TODO: provide more info for static cases.
    */
  case object Static extends TauInfo

  /** Tau-test was inserted using JCA directive. */
  case object JCA extends TauInfo

  /** Tau-test was inserted using PGO and we have relative weights for each path after the test.
    *
    * Note that this info supports an arbitrary number of true paths,
    * so it can be used for both tau-test and tau-switch nodes.
    */
  case class PGO(trueWeights: Seq[Int], falseWeight: Int) extends TauInfo {
    require(trueWeights.nonEmpty)

    def ++(that: PGO): PGO = {
      require(falseWeight == that.trueWeights.sum + that.falseWeight)
      PGO(trueWeights ++ that.trueWeights, that.falseWeight)
    }

    def filterByIndex(p: Int => Boolean): PGO = {
      val (filteredWeights, otherWeights) = trueWeights.zipWithIndex partitionMap { case (w, i) => if (p(i)) Left(w) else Right(w) }
      PGO(filteredWeights, otherWeights.sum + falseWeight)
    }
  }

  object PGO {
    def apply(trueWeight: Int, falseWeight: Int): PGO = PGO(Seq(trueWeight), falseWeight)
  }
}
