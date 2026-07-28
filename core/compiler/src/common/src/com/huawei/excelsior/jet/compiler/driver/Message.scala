/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.driver

private[driver] enum Message:
  case COMPILE, STOP, STOP_BY_ERROR, ERROR, SUCCESS

  def isStop: Boolean = this match {
    case STOP | STOP_BY_ERROR => true
    case _ => false
  }

  def hasPayload: Boolean = this match {
    case COMPILE | ERROR => true
    case _ => false
  }
