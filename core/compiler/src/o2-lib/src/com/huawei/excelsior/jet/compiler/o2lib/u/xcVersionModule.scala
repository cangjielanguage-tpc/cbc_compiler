/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

object xcVersionModule {
  //  MajorJETVersion and MinorJetVersion are BCD encoded versions
  //  and must end with H (hex) as linker expect them in BCD format.
  val MajorJETVersion: Int = 0x15
  val MajorJETVersionStr: String = "15"
  val MinorJETVersion: Int = 0x30
  val MinorJETVersionStr: String = "30"
  // value of JETVER equation, template argument
  // should match <MajorJETVersion><MinorJETVersion>
  val JetVerEquationValue: String = "1530"
  val JETVersionString: String = "v15.3"
  val InternalJETVersion: Int = 1
  val Edition: String = " Enterprise Edition"
  val JetEditionEquationValue: String = "Enterprise"
  val SymFileVersion: Int = 174
}
