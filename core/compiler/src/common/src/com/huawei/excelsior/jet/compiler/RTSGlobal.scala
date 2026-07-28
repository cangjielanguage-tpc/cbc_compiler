/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.dotty.annot.javaFriendly

/** RT global variables
  *
  * @author paul
  */
@javaFriendly
enum RTSGlobal {
  case JR_ComponentDescriptor
  case JR_FLOAT_SIGN_FLIP
  case JR_FLOAT_SIGN_MASK
  case JR_DOUBLE_SIGN_FLIP
  case JR_DOUBLE_SIGN_MASK
  case JR_PI_DIVIDE_FOUR
  case LINK_LocalTypesTable
}
