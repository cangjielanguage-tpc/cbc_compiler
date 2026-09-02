/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

/** Constant pool tag
  *
  * @author cypok
  */
enum Tag {
  case UNDEFINED_0
  case UTF8
  case UNDEFINED_2
  case INTEGER
  case FLOAT
  case LONG
  case DOUBLE
  case CLASS
  case STRING
  case FIELDREF
  case METHODREF
  case INTERFACE_METHODREF
  case NAME_AND_TYPE
  case UNDEFINED_13
  case UNDEFINED_14
  case METHOD_HANDLE
  case METHOD_TYPE
  case UNDEFINED_17
  case INVOKE_DYNAMIC
}
