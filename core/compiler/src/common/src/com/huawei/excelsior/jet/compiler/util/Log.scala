/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.util

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.driver.CompilationWorker
import com.huawei.excelsior.jet.compiler.options.BoolOption.IgnoreNumbersInPositionsOutput
import com.huawei.excelsior.jet.compiler.options.StrOption.OutputName
import com.huawei.excelsior.jet.compiler.options.{NumOption, StrOption}
import com.huawei.excelsior.jet.compiler.util.Log.Kind
import com.huawei.excelsior.jet.compiler.{CodeUnit, Environment}
import xscala.io.{TextInput, TextOutput, stdout}
import xscala.util.StringOps.asciiToLowerCase

import scala.collection.mutable
import scala.util.Using

/**
  * Exclusive log for some statistics.
  *
  * @author cypok
  * @author conwor
  */
abstract class Log {
  def isEnabled: Boolean
  def apply(msg: String): Unit
  def apply(msg: String, posOwner: Position.Owner): Unit = apply(msg, posOwner.posApproximation)
  def apply(msg: String, pos: Position): Unit = apply(s"$msg at ${pos.toString(ignoreNumbers = Log.env.enabled(IgnoreNumbersInPositionsOutput))}")
  def inSession[T](msg: String)(action: => T): T
  def inSession[T](msg: String, codeUnit: CodeUnit)(action: => T): T = inSession(s"$msg in $codeUnit")(action)
  protected def close(): Unit
}

object Log {
  enum Kind {
    case Inline, ClinitAnalysis, AICOpt, EscapeAnalysis, GeneralizedNew, TauOpt, XiTransform, FieldsTypeAnalysis,
        ExtraInfo, RMACombining, Explosion, MarkedRegions, DuplicatePositionMarkers, WriteBarriersOpt,
        Optimize, GlobalInitFields, SwitchAggregation
  }

  private var env: Environment = _
  def setEnv(env: Environment): Unit = { this.env = env }

  // option names and output filename are inherited from the O2 compiler
  private def optionsByKind(kind: Kind) = kind match {
    case Kind.Inline             => (StrOption.InlineStat,          "inl")
    case Kind.ClinitAnalysis     => (StrOption.ClinitAnalysisStat,  "cln")
    case Kind.AICOpt             => (StrOption.AICOptStat,          "aic")
    case Kind.EscapeAnalysis     => (StrOption.EscapeStat,          "esc")
    case Kind.GeneralizedNew     => (StrOption.GNewStat,            "gnew")
    case Kind.TauOpt             => (StrOption.TauOptStat,          "tau")
    case Kind.XiTransform        => (StrOption.XiTransformStat,     "xit")
    case Kind.FieldsTypeAnalysis => (StrOption.FieldsTypeStat,      "fld")
    case Kind.ExtraInfo          => (StrOption.ExtraInfoStat,       "eil")
    case Kind.RMACombining       => (StrOption.RMACombiningStat,    "rma")
    case Kind.Explosion          => (StrOption.ExplosionStat,       "expl")
    case Kind.MarkedRegions      => (StrOption.MarkedRegionsStat,   "mreg")
    case Kind.DuplicatePositionMarkers => (StrOption.DuplicatePositionMarkers, "dpm")
    case Kind.WriteBarriersOpt   => (StrOption.WriteBarriersOptStat, "wb")
    case Kind.Optimize           => (StrOption.OptStat,             "opt")
    case Kind.GlobalInitFields   => (StrOption.GlobalInitFieldsStat, "gifs")
    case Kind.SwitchAggregation  => (StrOption.SwitchAggregationStat, "swag")
  }

  private val logsByKind = new mutable.HashMap[Kind, Log]

  def logName(worker: Int, kind: Kind): String = {
    val (option, logExt) = optionsByKind(kind)
    val defaultLogName = option.toString.toLowerCase

    val workerMode = worker != 0
    val name = env.valueOfOrElse(OutputName, defaultLogName) + (if (workerMode) "-" + worker else "") + "." + logExt
    if (workerMode) {
      env.pdb.getFile(name).absolutePath.toString
    } else {
      name
    }
  }

  private def logByKind(kind: Kind): Log = {
    val (option, _) = optionsByKind(kind)
    val value = env.valueOfOrElse(option, "")
    if (value != "") {
      value(0).asciiToLowerCase match {
        case 's' => // 's' for 'screen'
          return new LogImpl(null, env, kind)

        case 'f' => // 'f' for 'file'
          return new LogImpl(TextOutput.fromFile(logName(env.valueOf(NumOption.Worker), kind)), env, kind)

        case _ =>
      }
    }

    new LogStub
  }

  def apply(kind: Kind): Log = logsByKind.getOrElseUpdate(kind, logByKind(kind))

  def closeAll(): Unit = {
    logsByKind.values foreach { _.close() }
  }
}

private class LogStub extends Log {
  override def isEnabled: Boolean = false
  override def apply(msg: String): Unit = {}
  override def inSession[T](msg: String)(action: => T) = action
  override def close(): Unit = {}
}

private class LogImpl(log: TextOutput, env: Environment, kind: Kind) extends Log {
  private val msgBufs = new mutable.Stack[mutable.Buffer[String]]

  override def isEnabled: Boolean = true

  override def apply(msg: String): Unit =
    msgBufs.head += msg

  override def inSession[T](msg: String)(action: => T) = {
    msgBufs.push(new mutable.ListBuffer[String])
    try {
      action
    } finally {
      val msgs = msgBufs.pop() filter (_ != null)
      if (msgs.nonEmpty) {
        println(msg)
        msgs foreach println
        println("")
      }
    }
  }

  override def close(): Unit =
    if (log != null) {
      val workerMode = env.valueOf(NumOption.Worker) != 0
      if (!workerMode) {
        CompilationWorker.foreach() { worker =>
          // not Using.resource as file might not exist, see JET-14003
          Using(TextInput.fromFile(Log.logName(worker, kind))) { file =>
            // copy worker logs to main log
            for (line <- file.getLines()) {
              log.println(line)
            }
          }
        }
      }
      log.close()
    }

  private def println(msg: String): Unit = {
    if (msg != null) {
      if (log != null) {
        log.println(msg)
      } else {
        stdout.println(msg)
      }
    }
  }

}
