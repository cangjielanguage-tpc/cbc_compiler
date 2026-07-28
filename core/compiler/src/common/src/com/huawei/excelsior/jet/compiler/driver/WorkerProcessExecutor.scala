/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.driver

import com.huawei.excelsior.common.{JetDirs, XProcess}
import com.huawei.excelsior.common.ProcessUtils.sanitizeCommand
import com.huawei.excelsior.jet.compiler.{CompilerWithStats, Environment, Stats}
import com.huawei.excelsior.jet.compiler.driver.CompilationWorker.sendMessage
import com.huawei.excelsior.jet.compiler.driver.Message.*
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.StrOption.*
import xscala.io.*
import xscala.properties.OS
import xscala.sync.XThread

import scala.collection.mutable.ArrayBuffer
import scala.util.Using

/** Executes compilation worker as a separate compiler process with additional args.
  *
  * May print worker output to stdout of the main compiler or create a log file in PDB
  * depending on compilation options (`+PrintWorkersOutput`, `LogWorkersOutputToFile`).
  *
  * @author kit
  */
object WorkerProcessExecutor {
  private[driver] class Result(val exitCode: Int, val errorMsg: String)
}

class WorkerProcessExecutor(
  /** Args of the main compiler process. */
  originalArgs: Array[String],
  env: Environment
) {
  // JET-12684: avoid using Environment from multiple threads until the issue is fixed.
  private val debugIrLogsDir = env.getDebugIrLogsDir
  private val printWorkersOutput = env.enabled(PrintWorkersOutput)
  private val logWorkersOutputToFile = env.enabled(LogWorkersOutputToFile)
  private val pdbName = env.valueOf(PDBName)
  private val errorBuf = new StringBuilder

  /** Executes worker as a separate compiler process.
    * The jobs (compilation commands) are sent through the worker stdin pipe.
    * The responses are inlined into the worker stdout, so the worker feeder-thread filters them.
    *
    * @param worker     number of the worker
    * @param driver     compilation driver to obtain the jobs, save the stats and keep error messages if any
    * @param compActor  actor to notify when a job is sent to worker
    * @param readStats  if it needs to read Stats and store them into the compilation driver
    * @return           the WorkerProcessExecutor.Result wrapping the worker process exit code
    */
  // TODO: this method and its helpers should be dropped as java-free Sockets get available
  def executeAndFeedViaInOut(worker: Int, driver: CompilationDriver, compActor: CompilationActor, readStats: Boolean) = try {
    val cmd = command(worker, CompilationWorker.MAGIC_PORT_MEANS_INOUT)

    val xProc = XProcess.start(sanitizeCommand(cmd))
    val exitCode = Using.resource(getLog(worker)) { log =>
      Using.resources(xProc.stdin, xProc.stdout, xProc.stderr) { (pStdin, pStdout, pStderr) =>
        val outReader: Thread = XThread(s"stdout reader $worker") {
          workerFeeder(worker, log, pStdout, pStdin, driver, compActor)
          if (readStats && !driver.errorState) {
            driver.workersStats(worker - 1) = Stats.deserialize(env, pStdout)
          }
          logForwarder(worker, log, pStdout, isErr = false) // copy rest of worker's stdout to log file
        }
        val errReader: Thread = XThread(s"stderr reader $worker") { logForwarder(worker, log, pStderr, isErr = true) }

        outReader.start()
        errReader.start()
        outReader.join()
        errReader.join()
        xProc.waitFor()
      }
    }

    new WorkerProcessExecutor.Result(exitCode, errorBuf.toString)
  } catch {
    case e: Throwable =>
      val stackTrace = TextOutput.asString(_.printStackTrace(e))
      new WorkerProcessExecutor.Result(-1, s"Worker process $worker has failed to start: $stackTrace")
  }

  private def getLog(worker: Int): TextOutput = if (logWorkersOutputToFile) { 
    TextOutput.from(Path(pdbName) / s"worker-$worker.log")
  } else {
    null
  }

  private def logLine(log: TextOutput, line: String, worker: Int, isErr: Boolean): Unit = {
    if (log != null) {
      log.println(line)
    }

    if (printWorkersOutput) {
      val out = if (isErr) stderr else stdout
      out.flush()
      out.println(s"WORKER $worker: $line")
      out.flush()
    }

    if (isErr) {
      errorBuf.append(line)
      errorBuf.append(OS.host.lineSeparator)
    }
  }

  private def logForwarder(worker: Int, log: TextOutput, in: TextInput, isErr: Boolean): Unit = {
    for (line <- in.getLines()) {
      logLine(log, line, worker, isErr)
    }
  }

  private def workerFeeder(worker: Int, log: TextOutput, in: TextInput, workerStdin: TextOutput,
                           driver: CompilationDriver, compActor: CompilationActor): Unit = {
    import CompilationWorker.*

    def handleErrorAndStopWorker(errCommand: String): Unit = {
      assert(errCommand == WORKERFAILURE || errCommand == WORKERBADCMND)
      // an error message must follow the errCommand
      val errMessage = in.getLine()
      driver.appendErrorMessage(errMessage, worker)
      driver.errorState = true
      sendMessage(STOP_BY_ERROR, workerStdin)
    }

    var wasEmptyLine = false // workaround to manage "\n" prefixes of worker commands
    var line = in.getLine()
    while (line != null) line.substring(0, WORKER_COMMAND_LEN min line.length) match {
      case WORKERFAILURE | WORKERBADCMND =>
        assert(wasEmptyLine)
        wasEmptyLine = false
        handleErrorAndStopWorker(line)
        line = null // stop reading pStdout
      case WORKERSUCCESS | WORKERSTARTED =>
        // wasLN can be false here when a worker prints some chars without line-break at the end
        wasEmptyLine = false
        driver.retrieveNextJob() match {
          case Some(job) =>
            compActor.startCompile(job, worker)
            sendMessage(COMPILE, workerStdin, job)
            line = in.getLine()
          case None =>
            // no more jobs, so send STOP signal and exit the reader thread
            sendMessage(if (driver.errorState) STOP_BY_ERROR else STOP, workerStdin)
            line = null // stop reading pStdout
        }
      case _ =>
        // line is not a command so log empty line if wasEmptyLine is true
        if (wasEmptyLine) logLine(log, "", worker, isErr = false)
        wasEmptyLine = line.isEmpty
        if (!line.isEmpty) {
          logLine(log, line, worker, isErr = false)
        }
        line = in.getLine()
    } // end while
  }

  private def command(worker: Int, portNumber: Int) = {
    val cmd = new ArrayBuffer[String]
    // see JET-16961
    val cjStdLibCompilationEnabled = env.enabled(GenProfileLibrary) && env.valueOf(LibraryName) == "CangJieStdLib"
    cmd += JetDirs.jc(cjStdLibCompilationEnabled)

    cmd ++= originalArgs
    cmd ++= Seq(
      s"-worker=$worker",
      "+donotcallimportresolver",
      s"-portNumber=$portNumber",
      s"-irLogsDir=${debugIrLogsDir.canonicalPath}",
    )
    cmd
  }

}
