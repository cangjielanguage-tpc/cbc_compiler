/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

/** Result kind of access action. */
enum ConstantPoolAccessResult {

  /** Ok, constant pool entry is resolved and accessible. */
  case OK

  /** Access action always leads to throwing exception at runtime. */
  case ERROR

  /** Access action is deferred due to some reason (e.g. class is absent). */
  case DEFERRED
}
