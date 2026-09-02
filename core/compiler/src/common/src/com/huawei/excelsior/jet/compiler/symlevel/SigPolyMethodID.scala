/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

/** ID of signature polymorphic method.
  *
  * @author alexm
  */
enum SigPolyMethodID {
  case
    NONE,
    INVOKE,
    INVOKE_EXACT,
    INVOKE_BASIC,
    LINK_TO_VIRTUAL,
    LINK_TO_STATIC,
    LINK_TO_SPECIAL,
    LINK_TO_INTERFACE

  /** Checks if this signature polymorphic method is a method handle invoker. */
  def isMethodHandleInvoker = this == INVOKE || this == INVOKE_EXACT

  /** Checks if this signature polymorphic method is static. */
  def isStatic =
    this == LINK_TO_VIRTUAL ||
      this == LINK_TO_STATIC ||
      this == LINK_TO_SPECIAL ||
      this == LINK_TO_INTERFACE
}
