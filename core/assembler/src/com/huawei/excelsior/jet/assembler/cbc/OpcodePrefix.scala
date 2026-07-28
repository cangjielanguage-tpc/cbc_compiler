/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

/** CBC opcode prefixes. */
enum OpcodePrefix(val encoding: Int) {
  case UG       extends OpcodePrefix(0x98)
  case Cast     extends OpcodePrefix(0x9A)
  case Checked  extends OpcodePrefix(0x9D)
  case Java     extends OpcodePrefix(0x9E)
}
