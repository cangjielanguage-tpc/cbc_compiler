/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.dotty.annot.javaFriendly
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind


/** Method access kind. Corresponds to one of 'invoke' bytecode instructions. */
@javaFriendly
enum MethodAccessKind {
  case STATIC
  case VIRTUAL
  case INTERFACE
  case SPECIAL
  case DYNAMIC

  def hasObjectArg: Boolean = {
    (this == VIRTUAL) || (this == INTERFACE) || (this == SPECIAL)
  }

  def asMethodRefAccessKind = this match {
    case STATIC => MethodReferenceAccessKind.STATIC
    case VIRTUAL => MethodReferenceAccessKind.VIRTUAL
    case INTERFACE => MethodReferenceAccessKind.INTERFACE
    case SPECIAL => MethodReferenceAccessKind.SPECIAL
    case DYNAMIC => shouldNotReachHere(this)
  }
}

object MethodAccessKind {
  val METHOD_ACCESS_KINDS: Array[MethodAccessKind] = MethodAccessKind.values

  def fromMethodRefernceAccessKind(accessKind: MethodReferenceAccessKind) = accessKind match {
    case MethodReferenceAccessKind.STATIC => STATIC
    case MethodReferenceAccessKind.VIRTUAL => VIRTUAL
    case MethodReferenceAccessKind.INTERFACE => INTERFACE
    case MethodReferenceAccessKind.SPECIAL => SPECIAL
    case MethodReferenceAccessKind.MUT |
         MethodReferenceAccessKind.STATIC_VIRTUAL => shouldNotReachHere(accessKind)
  }
}
