/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.driver

import com.huawei.excelsior.jet.compiler.driver.CompilationWorker.*
import com.huawei.excelsior.jet.compiler.driver.Message.*
import com.huawei.excelsior.jet.compiler.options.NumOption.*
import com.huawei.excelsior.jet.compiler.{CompilerWithStats, Environment}
import xscala.io.*

/** Compilation worker for parallel compilation.
  * TODO: For now instead of sockets we temporary use input-output pipes to manage workes.
  * The scaladoc below is outdated. The sockets-based code will be restored when java-free Sockets get available.
  *
  * Connects with the compilation driver via a socket, receives classes to compile from the driver and compiles them.
  * Communication protocol is described in [[CompilationDriver]].
  *
  * @see [[CompilationDriver]]
  * @author kit
  */
object CompilationWorker {
  private val MSGPREFIX = "JC_WORKER_MSGID#"

  private def formatMessage(msg: Message, payload: String = null): String = {
    val code = msg.ordinal
    assert(0 <= code && code <= 9) // one digit; to ease parsing in `parseMessage`
    assert(msg.hasPayload == (payload != null))

    if (msg.hasPayload) s"$MSGPREFIX$code:$payload" else s"$MSGPREFIX$code;"
  }

  private[driver] def sendMessage(msg: Message, to: TextOutput, payload: String = null): Unit = {
    to.println(formatMessage(msg, payload))
    to.flush()
  }

  private def parseMessage(str: String): (Message, String) = {
    val (prefix, rest) = str.splitAt(MSGPREFIX.length)
    assert(prefix == MSGPREFIX && rest.length >= 2)

    val code = rest(0).toInt - '0'
    assert(0 <= code && code <= 9)
    val msg = Message.fromOrdinal(code)

    val payload = rest(1) match {
      case ';' =>
        assert(rest.length == 2)
        null
      case ':' =>
        rest.substring(2)
    }
    assert(msg.hasPayload == (payload != null))
    (msg, payload)
  }

  /** Perform given `action` foreach worker. */
  def foreach()(action: Int => Unit): Unit = {
    if (ProjectLogic.parallelismEnabled) {
      for (worker <- 1 to ProjectLogic.workersAmount) {
        action(worker)
      }
    }
  }

  // constants for interaction with workers via in- and out- pipes
  val MAGIC_PORT_MEANS_INOUT = 0

  val WORKERSTARTED = "WORKERSTARTED"
  val WORKERSUCCESS = "WORKERSUCCESS"
  val WORKERFAILURE = "WORKERFAILURE"
  val WORKERBADCMND = "WORKERBADCMND"

  val WORKER_COMMAND_LEN = WORKERSTARTED.length
  assert(WORKERSUCCESS.length == WORKER_COMMAND_LEN)
  assert(WORKERFAILURE.length == WORKER_COMMAND_LEN)
  assert(WORKERBADCMND.length == WORKER_COMMAND_LEN)
}

class CompilationWorker(compilationActor: CompilationActor, env: Environment, stats: CompilerWithStats) {
  /** Connects with the driver and compiles classes received from the driver with the help of compilation actor. */
  def startWorker(): Unit = {
    val portNumber = env.valueOf(PortNumber)
    assert(portNumber == CompilationWorker.MAGIC_PORT_MEANS_INOUT)
    startWorkerViaInOut()
  }

  // TODO: this should be dropped as java-free Sockets get available
  def startWorkerViaInOut(): Unit = {
    val worker = env.valueOf(Worker)

    val in: TextInput = stdin
    val out: TextOutput = stdout

    def reportFailure(curJob: String, tag: String, message: String): Unit = {
      out.println(s"Worker-$worker failed to compile $curJob")
      out.println(s"\n$tag")
      out.println(message)
      out.flush()
    }

    try {
      out.println("\n" + CompilationWorker.WORKERSTARTED)

      var stop = false
      while (!stop) {
        val nextLine = in.getLine()
        val (msg, nextClass) = parseMessage(nextLine)
        msg match {
          case STOP =>
            stop = true
            if (stats != null) stats.stats.serialize(out)
            out.flush()

          case STOP_BY_ERROR =>
            stop = true
            out.flush()
            sys.exit(worker)

          case COMPILE =>
            try {
              compilationActor.startCompile(nextClass, worker)
              if (!compilationActor.compile(nextClass)) {
                var errMsg = compilationActor.errorMessage
                if (errMsg.isEmpty) {
                  errMsg = "Unknown error!"
                }
                reportFailure(nextClass, CompilationWorker.WORKERFAILURE, errMsg)
              } else {
                out.println("\n" + CompilationWorker.WORKERSUCCESS)
              }
            } catch {
              case e: O2LibFatalError =>
                reportFailure(nextClass, CompilationWorker.WORKERFAILURE, e.getMessage)

              case e: Throwable =>
                stdout.printStackTrace(e)
                val st = TextOutput.asString(_.printStackTrace(e))
                reportFailure(nextClass, CompilationWorker.WORKERFAILURE, st)
            }

          case _ =>
            reportFailure("due to worker bad message", CompilationWorker.WORKERBADCMND, nextLine)
        }
      }
    } catch {
      case e: Throwable =>
        stdout.printStackTrace(e)
        sys.exit(-1)
    }
  }
}
