/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

/** FPU Condition codes: used by fcmovcc instructions
  *
  * @author paul
  * @author cypok
  */
enum FPUCC {
  case B,  NB
  case E,  NE
  case BE, NBE
  case U,  NU

  def code = ordinal
}
