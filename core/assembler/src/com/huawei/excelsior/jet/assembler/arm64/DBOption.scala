/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import xscala.util.MathUtils.isNBits

/** ARM64 options of data barrier instructions (DMB and DSB).
  *
  * @author orangebyte256
  */
enum DBOption(val encoding: Int) {
  case SY     extends DBOption(0xF) // 0b1111
  case ST     extends DBOption(0xE) // 0b1110
  case ISH    extends DBOption(0xB) // 0b1011
  case ISHST  extends DBOption(0xA) // 0b1010
  case NSH    extends DBOption(0x7) // 0b0111
  case NSHST  extends DBOption(0x6) // 0b0110
  case OSH    extends DBOption(0x3) // 0b0011
  case OSHST  extends DBOption(0x2) // 0b0010
  case LD     extends DBOption(0xD) // 0b1101
  case ISHLD  extends DBOption(0x9) // 0b1001
  case NSHLD  extends DBOption(0x5) // 0b0101
  case OSHLD  extends DBOption(0x1) // 0b0001

  assert(isNBits(encoding, 4))
}
