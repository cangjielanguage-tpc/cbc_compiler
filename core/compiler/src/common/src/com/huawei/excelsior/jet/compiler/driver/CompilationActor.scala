/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.driver

/** Performs compilation of a compilation unit.
  *
  * Instances of compilation actors may run in parallel
  * (in different threads or processes).
  *
  * @author kit
  */
trait CompilationActor {
  /** Must be called before [[compile]] to update the compilation progress.
    * The driver [[CompilationDriver]] calls the method before sending a class (represented by `cuId`) to a worker.
    */
  def startCompile(cuId: String, worker: Int): Unit

  /** Performs compilation of `cuId` class. Returns `true` on successful compilation. */
  def compile(cuId: String): Boolean

  /** @return last error message, if last compilation with [[compile]] has failed. */
  def errorMessage: String
}
