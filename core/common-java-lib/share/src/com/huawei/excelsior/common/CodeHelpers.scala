/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

/** Common class that holds some specific helpers.
  *
  * @author cypok
  *
  * @define notImplementedDoc Call this method to mark code with unimplemented feature.
  * @define shouldNotCallThisDoc Call this method to mark methods that should not be ever called.
  * @define shouldNotReachHereDoc Call this method to mark unreachable code.
  */
object CodeHelpers {

  /** Exceptions thrown on attempts to use not yet implemented compilers features. */
  class NotImplementedException private(featureInfo: String) extends RuntimeException(s"not implemented: $featureInfo") {
    private[CodeHelpers] def this(feature: Any) = this(feature.toString)
    private[CodeHelpers] def this(feature: Any, details: String) = this(s"$feature ($details)")
  }

  /** $notImplementedDoc */
  def notImplemented[T](feature: Any): T = throw new NotImplementedException(feature)
  /** $notImplementedDoc */
  def notImplemented[T](feature: Any, extraInfo: Any): T = throw new NotImplementedException(feature, extraInfo.toString)

  /** Call this method to mark code which relates to unimplemented feature. */
  def noteThatWeDoNotImplement(feature: Any): Unit = {}

  /** $shouldNotCallThisDoc */
  def shouldNotCallThis[T](): T = shouldNot("call this", null)
  /** $shouldNotCallThisDoc */
  def shouldNotCallThis[T](extraInformation: => Any): T = shouldNot("call this", extraInformation)
  /** $shouldNotCallThisDoc */
  def shouldNotCallThis[T](extraInformation: String): T = shouldNotCallThis(extraInformation.asInstanceOf[Any])

  /** $shouldNotReachHereDoc */
  def shouldNotReachHere[T](): T = shouldNot("reach here", null)
  /** $shouldNotReachHereDoc */
  def shouldNotReachHere[T](extraInformation: => Any): T = shouldNot("reach here", extraInformation)
  /** $shouldNotReachHereDoc */
  def shouldNotReachHere[T](extraInformation: String): T = shouldNotReachHere(extraInformation.asInstanceOf[Any])

  private def shouldNot(what: String, extraInformation: Any) = throw new AssertionError(s"should not $what${
    if (extraInformation != null) s" (extra information: $extraInformation)"
    else ""
  }")
}
