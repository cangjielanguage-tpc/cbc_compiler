/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.jet.compiler.CompilerSuite

class CbcForeignLibrariesSuite extends CompilerSuite {
  test("CBC std.core depends on native FFI, not AOT std.core") {
    CbcForeignLibraries.forPackages(Seq.empty, Seq("std.core", "default")) shouldBe Seq("stdFFI")
  }

  test("AOT std.core retains its native symbol provider") {
    CbcForeignLibraries.forPackages(Seq("std.core"), Seq("default")) shouldBe Seq("cangjie-std-core")
  }

  test("Bootstrap images without std.core do not acquire standard-library dependencies") {
    CbcForeignLibraries.forPackages(Seq.empty, Seq("cbc-intrinsics")) shouldBe Seq.empty
  }

  test("Unrelated packages do not imply a std.core dependency") {
    CbcForeignLibraries.forPackages(Seq("std.math"), Seq("default")) shouldBe Seq.empty
  }
}
