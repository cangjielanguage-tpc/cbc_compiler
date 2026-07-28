/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.driver

/** This mode determines which level of optimizations was used for method compilation.
  *
  * @author conwor
  */
enum CompilationMode {
  // Special compilation mode, in which only sym-file metadata generated from compilation set
  case ONoCode

  // O1 - on-commit optimizations, forced inline, some optimization which is better to do and FastBackEnd
  // Used to improve compilation time
  case O1

  // O2 - standard compilation mode, included all optimizations and optimizing back-end with BGCM
  case O2
}
