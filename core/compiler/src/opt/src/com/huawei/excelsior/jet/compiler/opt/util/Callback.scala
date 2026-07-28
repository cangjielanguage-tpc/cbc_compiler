/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.util

import scala.collection.mutable

/**
 * Callback function with one argument.
 *
 * @author cypok
 */
class Callback[A] extends (A => Unit) with CallbackEngine[A => Unit] {

  /** Call all added callbacks. */
  def apply(arg: A): Unit = {
    callbacks foreach { _.apply(arg) }
  }

}

/**
 * Callback function without arguments.
 *
 * @author cypok
 */
class UnitCallback extends (() => Unit) with CallbackEngine[() => Unit] {

  /** Call all added callbacks. */
  def apply(): Unit = {
    callbacks foreach { _.apply() }
  }
}

/**
 * Callback function engine.
 *
 * @author cypok
 */
trait CallbackEngine[F] {

  protected val callbacks = mutable.LinkedHashSet.empty[F]

  /** Add callback function.
   * Returns true on success.
   * */
  def addCallback(f: F): Boolean = {
    callbacks.add(f)
  }

  /** Remove callback function. */
  private def removeCallback(f: F): Unit = {
    callbacks.remove(f) ensuring (_ == true)
  }

  /** Executes given body with enabled given callback. */
  def withCallback[T](f: F)(body: => T): T = {
    val wasAdded = addCallback(f)
    try {
      body
    } finally {
      if (wasAdded) {
        removeCallback(f)
      }
    }
  }
}
