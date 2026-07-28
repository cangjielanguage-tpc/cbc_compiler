/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

/** Abstract compiler.
  *
  * @author conwor
  */
abstract class Compiler(val env: Environment) {

  /** Generates code for a code unit.
    *
    * @param codeUnit encapsulates method being compiled in some compilation context
    * @throws CannotGenerateOptimizedReplacement if compiler prefers to generate replacement wrapper by itself,
    *                                            but failed to generate it
    */
  def genCode(codeUnit: CodeUnit): Unit

  /** This method should be called after all classes compilation.
    * It is used in Opt compiler for final statistics printing.
    */
  def printFinalStatistics(): Unit
}
