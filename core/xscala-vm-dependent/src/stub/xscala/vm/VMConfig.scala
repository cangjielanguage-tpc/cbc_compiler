/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.vm

import scala.annotation.static

class VMConfig
object VMConfig {
  @static def init(): Unit = {
    throw new java.lang.AssertionError("Should be replaced by actual implementation")
  }
}
