/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet

import com.huawei.excelsior.jet.util.graph.Graph
import org.scalactic.source
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{AppendedClues, BeforeAndAfterEach}

abstract class CommonSuite extends AnyFunSuite with Matchers with BeforeAndAfterEach with AppendedClues {

  def beIterator[N](el: N*) = new Matcher[Iterator[N]] {
    def apply(actual: Iterator[N]) = {
      val al = actual.toList
      MatchResult(al equals el, "{0} was not equal to {1}", "{0} was equal to {1}", Vector(al, el))
    }
  }

  def beIterator[N](elIt: Iterator[N]) = new Matcher[Iterator[N]] {
    def apply(actual: Iterator[N]) = {
      val al = actual.toList
      val el = elIt.toList
      MatchResult(al equals el, "{0} was not equal to {1}", "{0} was equal to {1}", Vector(al, el))
    }
  }

  def beTopologicallyEqual[N](expected: Graph[N]) = new Matcher[Graph[N]] {
    def apply(actual: Graph[N]) = {
      MatchResult(actual topologicallyEquals expected, "{0} was not equal to {1}", "{0} was equal to {1}", Vector(actual, expected))
    }
  }

  /** Attaches position to test arguments (for bulk tests). */
  def tp[T](x: T)(implicit pos: source.Position): (T, source.Position) = (x, pos)

}
