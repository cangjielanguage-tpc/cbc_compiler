/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.util

import com.huawei.excelsior.jet.compiler.util.Maps

/**
 * Bijection is a two-directed map from one type to another and vice versa.
 *
 * @author conwor
 */
final class MutableBijection[K : Maps, V : Maps] {

  private val k2v = Maps[K].newQMap[V]
  private val v2k = Maps[V].newQMap[K]

  /** @return copy of this bijection */
  def copy(): MutableBijection[K, V] = {
    val result = new MutableBijection[K, V]
    for ((x, y) <- k2v) {
      result.k2v(x) = y
      result.v2k(y) = x
    }
    result
  }

  /** Removes pair with given `key` */
  def removeKey(k: K): Unit = {
    k2v.get(k) foreach { v =>
      k2v.remove(k)
      v2k.remove(v)
    }
  }

  /** Removes pair with given `value` */
  def removeValue(v: V): Unit = {
    v2k.get(v) foreach { k =>
      v2k.remove(v)
      k2v.remove(k)
    }
  }

  /** Adds pair with given `key` and `value` */
  def add(k: K, v: V): Unit = {
    removeKey(k)
    removeValue(v)
    k2v(k) = v
    v2k(v) = k
  }

  /** @return Some(value) if bijection contains pair (`key`, `value`), or None otherwise */
  def valueOption(k: K): Option[V] = k2v.get(k)

  /** @return Some(key) if bijection contains pair (`key`, `value`), or None otherwise */
  def keyOption(v: V): Option[K] = v2k.get(v)

  /** @return value, if bijection contains pair (`key`, `value`), or default otherwise */
  def valueOrElse(k: K, default: => V): V = k2v.getOrElse(k, default)

  /** @return key, if bijection contains pair (`key`, `value`), or default otherwise */
  def keyOrElse(v: V, default: => K): K = v2k.getOrElse(v, default)

  /** @return value, if bijection contains pair (`key`, `value`), or fails otherwise */
  def value(k: K): V = k2v(k)

  /** @return key, if bijection contains pair (`key`, `value`), or fails otherwise */
  def key(v: V): K = v2k(v)

  /** @return whether this bijection contains pair with given `key` */
  def containsKey(k: K): Boolean = k2v.contains(k)

  /** @return whether this bijection contains pair with given `value` */
  def containsValue(v: V): Boolean = v2k.contains(v)

  /** @return iterator over bijection pairs */
  def iterator: Iterator[(K, V)] = k2v.iterator

}
