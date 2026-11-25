/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

private[cbc] object CbcForeignLibraries {
  def forPackages(aotPackages: Seq[String], cbcPackages: Seq[String]): Seq[String] = {
    // CBC std.core still calls native @C functions (printing, string helpers,
    // math, etc.). The SDK supplies them in stdFFI without AOT std.core code.
    // Keep bootstrap images without std.core free of either dependency.
    if (cbcPackages.contains("std.core")) Seq("stdFFI")
    else if (aotPackages.contains("std.core")) Seq("cangjie-std-core")
    else Seq.empty
  }
}
