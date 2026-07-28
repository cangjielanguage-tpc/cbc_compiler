/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.XString

object CompilationDriverModule {
  type ProjectIterator = Iterator[XString]

  abstract class CompilationDriver {
    def doCompilation(iterator: ProjectIterator, actor: CompilationActor): Boolean
  }

  abstract class CompilationWorker {
    def startWorker(actor: CompilationActor): Unit
  }

  abstract class CompilationActor {
    def getErrorMessage: XString
    def startCompile(cuId: XString, worker: Int): Unit
    def compile(cuId: XString): Boolean
  }

  private var driverImpl: CompilationDriverModule.CompilationDriver = _
  private var workerImpl: CompilationDriverModule.CompilationWorker = _
  def setImpls(driverImpl: CompilationDriver, workerImpl: CompilationWorker): Unit = {
    CompilationDriverModule.driverImpl = driverImpl
    CompilationDriverModule.workerImpl = workerImpl
  }

  def doCompilation(iterator: ProjectIterator, actor: CompilationActor): Boolean =
    driverImpl.doCompilation(iterator, actor)

  def startWorker(actor: CompilationActor): Unit = workerImpl.startWorker(actor)
}
