/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.common

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.immutable.WrappedString
import scala.collection.mutable.ArraySeq
import scala.collection.{ArrayOps, StringOps}
import scala.language.implicitConversions

/** This object contains copies of several methods from Predef.scala.
  * It provides a solution for the runtime problem when scala code is executed too early
  * and original Predef.scala cannot be initialized at this time.
  * So this objects provides only necessary methods.
  */
object MyPredef extends MyLowPriorityImplicits {

  @inline implicit def intWrapper(x: Int): runtime.RichInt = new runtime.RichInt(x)
  @inline implicit def augmentString(x: String): StringOps = new StringOps(x)

  def require(requirement: Boolean): Unit = {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed")
  }

  @elidable(ASSERTION)
  @inline
  final def assert(assertion: Boolean, message: => Any): Unit = {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: " + message)
  }

  @elidable(ASSERTION)
  def assert(assertion: Boolean): Unit = {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }

  @inline implicit def byteArrayOps             (xs: Array[Byte])    : ArrayOps[Byte]    = new ArrayOps(xs)
  @inline implicit def refArrayOps[T <: AnyRef] (xs: Array[T])       : ArrayOps[T]       = new ArrayOps(xs)
}

abstract class MyLowPriorityImplicits {
  /** @group conversions-string */
  implicit def wrapString(s: String): WrappedString = if (s ne null) new WrappedString(s) else null

  implicit def wrapByteArray(xs: Array[Byte]): ArraySeq.ofByte = if (xs ne null) new ArraySeq.ofByte(xs) else null
}
