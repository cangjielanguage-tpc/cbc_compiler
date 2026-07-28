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
 * Type class for maps with keys of type `K`.
 * Optimized implementations for some types may be provided by the user.
 *
 * @author paul
 */
@annotation.implicitNotFound(msg = "No implicit Maps defined for ${K}.")
trait Maps[K] {
  import language.higherKinds

  /** Immutable map with keys of type `K`.
   * Iteration order of the map's elements is unspecified (and may be non-deterministic).
   */
  type ImmMap[V] >: Null <: immutable.Map[K, V]
  def newImmMap[V]: ImmMap[V]

  // TODO: ImmMap[V]
  def newImmMap[V](xs: IterableOnce[(K, V)]): immutable.Map[K, V] = newImmMap[V] ++ xs

  /** Mutable map with keys of type `K`.
   * Iteration order of the map's elements is unspecified (and may be non-deterministic).
   */
  type MMap[V] >: Null <: mutable.Map[K, V]
  def newMMap[V]: MMap[V]
  def newMMap[V](xs: IterableOnce[(K, V)]): MMap[V] = {
    val map = newMMap[V]
    map ++= xs
    map
  }

  /** Mutable map with keys of type `K` with deterministic queue-like (FIFO) iteration order of its elements.
   */
  type QMap[V] >: Null <: mutable.Map[K, V]
  def newQMap[V]: QMap[V]
  def newQMap[V](xs: IterableOnce[(K, V)]): QMap[V] = {
    val map = newQMap[V]
    map ++= xs
    map
  }

}

object Maps {
  trait Default[K] extends Maps[K] {
    type ImmMap[V] = immutable.Map[K, V]
    def newImmMap[V]: ImmMap[V] = immutable.Map.empty[K, V]

    type MMap[V] = mutable.Map[K, V]
    def newMMap[V]: MMap[V] = mutable.Map.empty[K, V]

    type QMap[V] = mutable.LinkedHashMap[K, V]
    def newQMap[V]: QMap[V] = mutable.LinkedHashMap.empty[K, V]
  }

  object Defaults {
    implicit def default[K]: Maps[K] = new Default[K] { }
  }

  def apply[K](implicit maps: Maps[K]) = maps
}