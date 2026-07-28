/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.util

import scala.collection.mutable.ArrayBuffer

/**
 * Criterion is a generation choice mechanism. Criteria form criteria tree,
 * which is a main element in local code generation.
 *
 * @author conwor
 */
trait CriteriaTreeUtil {

  type Candidate

  /**
   * Criterion is a generation choice mechanism. Each criterion sorts generation candidates
   * to some levels. Levels counted from 0 (the best) to `levelsCount` - 1 (the worst).
   * Special level is -1, means that generation candidate is not applicable to this criterion.
   *
   * Example 1.
   * Criterion "not blocked" sorts candidates to levels:
   *   -1, if candidate is blocked
   *    0, if candidate is not blocked
   *
   * Example 2.
   * Criterion "positive generation effect" sorts candidates to levels:
   *    0, if node is normal (not copy or load)
   *    1, if node is useful copy
   *    2, if node is useful load
   *    3, if node is not useful copy
   *    4, if node is not useful load
   *
   * @param levelsCount count of levels of this criterion
   */
  abstract class Criterion(val levelsCount: Int) {

    /** @return level of given `candidate`, or -1, if `candidate` is not applicable to this criterion. */
    def level(candidate: Candidate): Int

    /** @return name of this criterion (used in debug purposes). */
    def name: String = getClass.getName
  }

  /**
   * Exclusion criterion is a criterion, which have only one applicable level.
   * For example, criterion "not blocked" is exclusion.
   */
  abstract class ExclusionCriterion extends Criterion(1) {

    /** @return whether given `candidate` is applicable to this criterion. */
    def apply(candidate: Candidate): Boolean

    final def level(candidate: Candidate): Int = if (this(candidate)) 0 else -1
  }


  /**
   * Criteria form criteria tree, which is a main element of local code generation.
   * In each session, local code generator iterates current DAG crown and tries to
   * put generation candidates to criteria tree root.
   *
   * Each criteria tree element contains criteria, which sorts generation candidates
   * to levels, and it could contains children criteria tree elements. If node is
   * applicable to criterion (have some positive level), criteria tree element tries
   * to put it into children elements.
   *
   * Leaf elements of criteria tree contains pool of applicable nodes, collected during
   * DAG crown iteration.
   *
   * Putting candidate into criteria tree could be marked as `findBest`. It means, that
   * if some criteria tree branch have already founded applicable candidate, we would
   * not try to put candidate, if it worth then already founded.
   *
   * @param criterion criterion of this criteria tree element
   * @param leaf whether this criteria tree element if leaf
   * @param childrenSize size of children criteria tree elements
   */
  class CriteriaTree private(val criterion: Criterion, leaf: Boolean, childrenSize: Int) {

    // Children of criteria tree element
    private val levels = if (leaf) null else new Array[ArrayBuffer[CriteriaTree]](criterion.levelsCount)

    // Index of the best founded branch (from 0 to childrenSize*levelsSize - 1)
    private var bestFounded: Int = _

    // Cost of generation candidate, selected by this criterion.
    private var cost: Int = -1

    /**
     * Constructs new leaf criteria tree element with given `criterion`.
     *
     * @param criterion criterion of new leaf criteria tree element
     */
    def this(criterion: Criterion) = {
      this(criterion, true, -1)
    }

    /**
     * Constructs new not leaf criteria tree element with given `criterion` and given
     * `children` criteria tree elements.
     *
     * @param criterion criterion of new criteria tree element
     * @param children children criteria tree elements
     */
    def this(criterion: Criterion, children: Iterable[CriteriaTree]) = {
      this(criterion, false, children.size)
      levels(0) = new ArrayBuffer[CriteriaTree]
      levels(0) ++= children
      for (i <- 1 until levels.size) {
        levels(i) = new ArrayBuffer[CriteriaTree]
        levels(i) ++= children map { _.makeCopy() }
      }
    }

    /** @return copy of this criteria tree. */
    private def makeCopy(): CriteriaTree = {
      val copy = new CriteriaTree(criterion, leaf, childrenSize)
      if (!leaf) {
        for (i <- 0 until levels.size) {
          copy.levels(i) = new ArrayBuffer[CriteriaTree]
          copy.levels(i) ++= levels(i) map { _.makeCopy() }
        }
      }
      copy
    }

    /** Cleans all criteria tree. Resets `bestFounded` indices and re-calc costs of leaves. */
    def cleanTree(startCost: Int = 0): Int = {
      if (leaf) {
        cost = startCost
        startCost + 1
      } else {
        bestFounded = childrenSize*criterion.levelsCount - 1
        var x = startCost
        for (l <- levels; c <- l) x = c.cleanTree(x)
        x
      }
    }

    private def putCandidate(candidate: Candidate): CriteriaTree = {
      val l = criterion.level(candidate)
      if (l == -1) null else {
        if (leaf) this else {
          val offs = l * childrenSize
          if (offs > bestFounded) null else {
            var i = 0
            var element: CriteriaTree = null
            while ((element == null) && (offs + i) <= bestFounded) {
              element = levels(l)(i).putCandidate(candidate)
              i += 1
            }
            bestFounded = (offs + i) - 1
            element
          }
        }
      }
    }

    /**
     * Puts candidate to criteria tree. Finds the best branch for this candidate
     * and returns cost of this branch leaf.
     *
     * @param candidate generation candidate
     */
    def calcCost(candidate: Candidate): Int = {
      val element = putCandidate(candidate)
      if (element == null) -1 else element.cost
    }

    def debugString(initString: String): String = {
      var result = initString
      if (leaf) {
        result += "LEAF: " + criterion.name + ", COST: " + cost + "\n"
      } else {
        result += "NODE: " + criterion.name + "\n"
        var i = 0
        for (l <- levels) {
          for (c <- l) result += c.debugString(initString + i + "   ")
          i += 1
        }
      }
      result
    }
  }
}