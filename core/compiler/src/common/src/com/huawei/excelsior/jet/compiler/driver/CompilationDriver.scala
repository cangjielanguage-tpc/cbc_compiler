/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.driver

import com.huawei.excelsior.jet.compiler.driver.Message.*
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.{CompilerWithStats, Environment, Stats}
import xscala.io.*
import xscala.sync.Sync.{Lock, newLock}
import xscala.properties.OS
import xscala.sync.XThread

import java.io.IOException
import scala.collection.mutable.ArrayBuffer

/** Takes a project iterator and [[CompilationActor]] and performs compilation of the project.
  *
  * For parallel compilation, it launches worker processes that receive classes to compile from the driver.
  * TODO: For now instead of sockets we temporary use input-output pipes to manage workes.
  * So the protocol below is outdated. The sockets-based code will be restored when java-free Sockets get available.
  *
  * The driver and a worker communicate via socket by the following protocol:
  *
  *  1. First, connection is established.
  *  1. Worker sends to the driver its number.
  *  1. Driver sends to the worker a class to compile from current compilation set.
  *  1. If the worker successfully compiles the class it sends [[SUCCESS]]
  *     message to the driver. Driver proceeds to 3.
  *  1. If the worker fails to compile it sends the error message to the driver, driver turns compilation process
  *     to the error state, stops other workers and compilation.
  *  1. When there is no more classes to compile or compilation process comes to an error state (some of the workers fail
  *     to compile), the driver sends [[STOP]] message
  *     to the worker and the worker stops compilation and exits.
  *  1. In the end, after successful compilation the worker sends compiler statistics to the driver.
  *
  * @author kit
  */
object CompilationDriver {
  private val CONNECTION_TIMEOUT = 5000
  private val DEBUG_PORT = 5555
}

class CompilationDriver(projectIterator: Iterator[String],
                        compilationActor: CompilationActor,
                        env: Environment,
                        workerExecutor: WorkerProcessExecutor,
                        driverStats: CompilerWithStats) {

  /** Set to `true` when a compilation worker fails.
    *
    * If that happens, the compilation driver should stop all workers and terminate the compilation process,
    * returning the error message received from the failed worker.
    */
  @volatile private[driver] var errorState = false

  private var errorMessages: Array[String] = _
  private[driver] var workersStats: Array[Stats] = _
  private lazy val lock = newLock()

  /** Returns next class to compile from the compilation set or `None`, if there are no more classes to compile,
    * or the compilation process is in error state (some of the workers have failed).
    *
    * Method is synchronized because worker processes get the next job in parallel.
    */
  private[driver] def retrieveNextJob() = lock.sync {
    if (errorState) None else projectIterator.nextOption()
  }

  private def forEachJob(action: String => Unit): Unit = {
    var job = retrieveNextJob()
    while (job.isDefined) {
      action(job.get)
      job = retrieveNextJob()
    }
  }

  /** Stores error message received from `worker`. */
  private[driver] def appendErrorMessage(msg: String, worker: Int): Unit = {
    val oldMsg = errorMessages(worker)
    errorMessages(worker) = if (oldMsg == null) msg else oldMsg + OS.host.lineSeparator + msg
  }

  private def appendError(e: Throwable, worker: Int): Unit = {
    val stackTrace = TextOutput.asString(_.printStackTrace(e))
    appendErrorMessage(stackTrace, worker)
  }

  /** Performs compilation in the driver process.
    *
    * @return `true` iff all classes were successfully compiled, including classes from other workers.
    */
  private def doWorkInCurrentProcess(): Boolean = {
    forEachJob { job =>
      try {
        compilationActor.startCompile(job, 0)
        if (!compilationActor.compile(job)) {
          return false
        }
      } catch { case e: Throwable =>
        errorState = true
        throw e
      }
    }

    !errorState
  }

  /** Joins worker threads. */
  private def waitForThreadsToComplete(latch: CountDownLatch, workerThreads: Iterable[Thread]): Boolean = {
    try {
      latch.await()
      true
    } catch case _: InterruptedException => {
      // clean interrupt flag
      // Thread.interrupted() TODO: xscala
      stderr.println("Interrupted!")
      for (workerThread <- workerThreads) {
        workerThread.interrupt()
      }
      return false
    }
  }

  /** The driver implementation routine. */
  def doCompilation(): Boolean = {
    if (ProjectLogic.parallelismEnabled) {
      // TODO: restore the code that works through Sockets when java-free Sockets get available
      doParallelCompilationViaInOut()
    } else {
      if (!doWorkInCurrentProcess()) {
        errorState = true
        stderr.println("Failed to compile!")
        return false
      }
      assert(!projectIterator.hasNext)
      !errorState
    }
  }

  // TODO: eliminate this method when java-free Sockets get available
  private def doParallelCompilationViaInOut(): Boolean = {
    val workerThreads = ArrayBuffer.empty[Thread]

    // 0 index is served for exceptions from workers that we were unable to connect
    errorMessages = new Array[String](ProjectLogic.workersAmount + 1)
    workersStats = new Array[Stats](ProjectLogic.workersAmount)
    val debugMode = env.enabled(BoolOption.DebugWorkers)

    try {
      val latch = CountDownLatch(ProjectLogic.workersAmount)
      if (debugMode) {
        println(s"Start compiler with args: -worker=1 -portNumber=0")
      } else {
        CompilationWorker.foreach() { worker =>
          val workerThread = XThread(s"Worker $worker") {
            try {
              val res = workerExecutor.executeAndFeedViaInOut(worker, this, compilationActor, driverStats != null)
              if (res.exitCode != 0) {
                if (res.errorMsg.nonEmpty) {
                  appendErrorMessage(res.errorMsg, worker)
                } else {
                  appendErrorMessage(s"Worker $worker has terminated with exit code ${res.exitCode} and without error message", 0)
                }
                latch.cancel()
                errorState = true
              }
            } finally {
              latch.done()
            }
          }
          workerThread.start()
          workerThreads += workerThread
        }
      }

      if (!debugMode && !waitForThreadsToComplete(latch, workerThreads)) {
        return false
      }

      assert(projectIterator.isEmpty)
      if (!errorState && driverStats != null) {
        workersStats foreach driverStats.stats.mergeWith
      }

      !errorState

    } catch {
      case e: IOException =>
        stdout.printStackTrace(e)
        false

    } finally {
      for (i <- errorMessages.indices) {
        val errMsg = errorMessages(i)
        if (errMsg != null) {
          assert(errorState)
          if (i != 0) {
            stderr.println(s"Error from worker $i:")
          }
          stderr.println(errMsg)
        }
      }
    }
  }

  /**
    * Count down latch with cancellation.
    */
  private class CountDownLatch(private var counter: Int) {
    assert(counter >= 0)

    private var cancelled = false
    private val lock = xscala.sync.Sync.newLock()

    def await(): Unit = lock.sync {
      while (counter > 0) {
        lock.await()
      }
      if (cancelled) {
//        Thread.currentThread().interrupt() TODO xscala
        throw new InterruptedException()
      }
    }

    def done(): Unit = lock.sync {
      counter -= 1
      if (counter <= 0) {
        lock.signalAll()
      }
    }

    def cancel(): Unit = lock.sync {
      cancelled = true
      lock.signalAll()
    }
  }
}
