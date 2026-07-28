/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.compiler.Env.isWorkMode
import com.huawei.excelsior.jet.compiler.Stats.KindEvent
import com.huawei.excelsior.jet.compiler.options.StrOption
import xscala.io.{DataInput, DataOutput, Path, TextInput, TextOutput}

import java.io.{FileNotFoundException, IOError, IOException}
import scala.collection.mutable

/** Useful tool for collecting various compile-time stats.
  *
  * @author cypok
  */
object Stats {
  object KindEvent {
    @throws[IOException]
    def deserialize(in: DataInput) = {
      val kind = StatsKind.values(in.getW32())
      val event = if (in.getBoolean()) in.getUTF() else null
      KindEvent(kind, event)
    }
  }

  case class KindEvent(kind: StatsKind, event: String) {
    assert(kind != null)
    // event may be null

    @throws[IOException]
    def serialize(out: DataOutput): Unit = {
      out.putW32(kind.ordinal)
      out.putBoolean(event != null)
      if (event != null) out.putUTF(event)
    }
  }

  private def parseStatsEquation(str: String): Set[StatsKind] = {
    val parsed = if (str != null && str.nonEmpty) {
      str.split(",").map(_.trim).filter(_.nonEmpty).map(StatsKind.fromString).toSet
    } else {
      Set.empty[StatsKind]
    }
    if (isWorkMode) {
      parsed ++ Set(StatsKind.MethComp, StatsKind.Crashes)
    } else {
      parsed
    }
  }

  @throws[IOException]
  def deserialize(env: Environment, in: TextInput): Stats = {
    val stats = new Stats(env)
    // TODO: decode binary data from text stream
    stats
  }

  @throws[IOException]
  def deserialize(env: Environment, in: DataInput): Stats = {
    val stats = new Stats(env)
    stats.deserialize(in)
    stats
  }

  private def openVerboseWriter(env: Environment): TextOutput = try {
    TextOutput.fromFile(env.valueOfOrElse(StrOption.OutputName, "stats") + ".stats")
  } catch {
    case e: FileNotFoundException =>
      throw new IOError(e)
  }
}

final class Stats(env: Environment) {
  private val counts = mutable.Map.empty[KindEvent, Long]
  private val values = mutable.Map.empty[KindEvent, List[Double]]
  private val enabledStats: Set[StatsKind] = Stats.parseStatsEquation(env.valueOf(StrOption.Stats))

  private val verboseWriter: TextOutput =
    if (enabledStats.exists(_.isVerbose)) Stats.openVerboseWriter(env) else null

  def isEnabled(kind: StatsKind) = enabledStats.contains(kind)

  def finishVerbosePrinting(): Unit = if (verboseWriter != null) verboseWriter.close()

  private def verbosePrint(kind: StatsKind, event: String, msg: String): Unit = {
    verboseWriter.println(s"$kind/$event$msg")
    verboseWriter.flush()
  }

  def countResults: collection.Map[KindEvent, Long] = counts

  def valueResults: collection.Map[KindEvent, List[Double]] = values

  def count(kind: StatsKind, event: String, delta: Long = 1, verboseSuffix: String = ""): Unit = {
    if (isEnabled(kind)) {
      assert(delta >= 0L)
      val ke = KindEvent(kind, event)
      counts.updateWith(ke) {
        case Some(value) => Some(delta + value)
        case None => Some(delta)
      }
      if (kind.isVerbose) {
        verbosePrint(kind, event, if (delta != 1) s" ($delta)$verboseSuffix" else verboseSuffix)
      }
    }
  }

  def value(kind: StatsKind, event: String, num: Double, verboseSuffix: String = ""): Unit = {
    if (isEnabled(kind)) {
      val ke = KindEvent(kind, event)
      values.updateWith(ke) {
        case Some(value) => Some(value.appended(num))
        case None => Some(List(num))
      }
      if (kind.isVerbose) {
        verbosePrint(kind, event, s" ($num)$verboseSuffix")
      }
    }
  }

  @throws[IOException]
  def serialize(out: TextOutput): Unit = {
    // TODO: encode binary data into text stream
  }

  @throws[IOException]
  def serialize(out: DataOutput): Unit = {
    out.putW32(counts.size)
    for ((ke, c) <- counts) {
      ke.serialize(out)
      out.putW64(c)
    }
    out.putW32(values.size)
    for ((ke, value) <- values) {
      ke.serialize(out)
      out.putW32(value.size)
      value foreach out.putF64
    }
  }

  @throws[IOException]
  private def deserialize(in: DataInput): Unit = {
    val countsSize = in.getW32()
    for (_ <- 0 until countsSize) {
      val ke = KindEvent.deserialize(in)
      counts.put(ke, in.getW64())
    }
    val valuesSize = in.getW32()
    for (_ <- 0 until valuesSize) {
      val ke = KindEvent.deserialize(in)
      values.put(ke, (0 until in.getW32()).map(_ => in.getF64()).toList)
    }
  }

  def mergeWith(stats: Stats): Unit = {
    def merge[K, V](a: mutable.Map[K, V], b: mutable.Map[K, V])(reduce: (V, V) => V): Unit = {
      for ((k, v) <- b) {
        a.update(k, a.get(k) match {
          case Some(value) => reduce(value, v)
          case None => v
        })
      }
    }

    merge(counts, stats.counts)(_+_)
    merge(values, stats.values)(_++_)
  }
}
