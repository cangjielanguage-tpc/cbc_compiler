/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

/** CPU features (instruction subsets)
  *
  * @author paul
  * @author cypok
  */
enum Feature {
  case SSE3
  case SHORTJUMPS // Jumps with short offset generated with 1-byte fixup.
}
