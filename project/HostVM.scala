/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package build

/** Virtual machine that will execute compiler code. */
sealed trait HostVM {
  def opposite: HostVM
}

object HostVM {
  case object JET extends HostVM {
    override val toString: String = "jet"
    override val opposite: HostVM = JDK
  }
  case object JDK extends HostVM {
    override val toString: String = "jdk"
    override val opposite: HostVM = JET
  }
}
