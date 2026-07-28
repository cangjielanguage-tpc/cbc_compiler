/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.util

import scala.collection.{immutable, mutable}

/**
 * Type class for sets of type `T`.
 * Optimized implementations for some types may be provided by the user.
 *
 * @author paul
 */
@annotation.implicitNotFound(msg = "No implicit Sets defined for ${T}.")
trait Sets[T] {
  /** Immutable set of type `T`.
   * Iteration order of the set's elements is unspecified (and may be non-deterministic).
   */
  type ImmSet >: Null <: immutable.Set[T]
  def newImmSet: ImmSet

  // TODO: ImmSet
  def newImmSet(xs: IterableOnce[T]): immutable.Set[T] = newImmSet ++ xs

  /** Mutable set of type `T`.
   * Iteration order of the set's elements is unspecified (and may be non-deterministic).
   */
  type MSet >: Null <: mutable.Set[T]
  def newMSet: MSet
  def newMSet(xs: IterableOnce[T]): MSet = {
    val set = newMSet
    set ++= xs
    set
  }

  /** Mutable set of type `T` with deterministic queue-like (FIFO) iteration order of its elements.
   */
  type QSet >: Null <: mutable.Set[T]
  def newQSet: QSet
  def newQSet(xs: IterableOnce[T]): QSet = {
    val set = newQSet
    set ++= xs
    set
  }
}

object Sets {
  trait Default[T] extends Sets[T] {
    type ImmSet = immutable.Set[T]
    def newImmSet: ImmSet = immutable.Set.empty[T]

    type MSet = mutable.Set[T]
    def newMSet: MSet = mutable.Set.empty[T]

    type QSet = mutable.LinkedHashSet[T]
    def newQSet: QSet = mutable.LinkedHashSet.empty[T]
  }

  object Defaults {
    implicit def default[T]: Sets[T] = new Default[T] { }
  }

  def apply[T](implicit sets: Sets[T]) = sets
}