/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.compiler.CompilerWithStats.*
import com.huawei.excelsior.jet.compiler.Stats.KindEvent
import com.huawei.excelsior.jet.compiler.StatsKind.*
import com.huawei.excelsior.jet.compiler.symlevel.Method
import xscala.io.TextOutput

import java.io.{FileNotFoundException, IOError}
import scala.collection.MapView
import scala.util.Using

object CompilerWithStats {
  private val AllMethods = "all"
  private val AllRuntimeMethods = "allRuntime"
  private val AllUnmanagedMethodsCount = "allUnmanaged"
  private val PassedMethods = "passed"
  private val PassedRuntimeMethods = "passedRuntime"
  private val PassedUnmanagedMethods = "passedUnmanaged"

  final private class StatsPrinter(stats: Stats, compilerName: String) {

    def print(): Unit = {
      def filter[T](results: collection.Map[KindEvent, T], f: StatsKind => Boolean): MapView[KindEvent, T] = {
        results.view.filterKeys(k => f(k.kind))
      }

      val valueResults = stats.valueResults
      if (valueResults.nonEmpty) {
        for (k <- Seq(HugeClinitsSize, HugeMethodsSize); values <- valueResults.get(KindEvent(k, null))) {
          printIntHistogram(k, values)
        }
        val otherValueResults = filter(valueResults, k => (k != HugeClinitsSize) && (k != HugeMethodsSize))
        for ((ke, values) <- otherValueResults) {
          printRawValues(ke.kind, ke.event, values)
        }
      }
      val countResults = stats.countResults
      if (countResults.nonEmpty) {
        println()
        println(compilerName + " statistics")
        println("==========================================")
        val methCompResults = filter(countResults, k => k == MethComp)
        val crashesResults = filter(countResults, k => k == Crashes)
        val otherResults = filter(countResults, k => (k != MethComp) && (k != Crashes))
        if (methCompResults.nonEmpty) {
          def getCount(event: String): Long = countResults.getOrElse(KindEvent(MethComp, event), 0L)

          println("Methods compilation stats:")
          printOneLine("All methods", getCount(AllMethods), getCount(PassedMethods))
          printOneLine("AJ runtime ", getCount(AllRuntimeMethods), getCount(PassedRuntimeMethods))
          printOneLine("Unmanaged  ", getCount(AllUnmanagedMethodsCount), getCount(PassedUnmanagedMethods))
          println()
        }
        if (otherResults.nonEmpty) {
          val lines = otherResults map { case (ke, ve) => s"${ke.kind}/${ke.event}: $ve" }

          println("All statistics:")
          for (elem <- lines.toArray.sortInPlace()) {
            println(elem)
          }
          println()
        }
        if (crashesResults.nonEmpty) {
          val lines = crashesResults.toArray sortInPlaceWith (_._2 > _._2)

          println("Crashes messages:")
          for ((ke, ve) <- lines) {
            println(s"$ve: ${ke.event}")
          }
          println()
        }
      }
    }

    private def printIntHistogram(kind: StatsKind, values: List[Double]): Unit = {
      assert(values.nonEmpty)
      try {
        Using.resource(TextOutput.fromFile(s"stats-histogram-$kind.txt")) { out =>
          val intValues = values.map(_.toInt).toArray
          val num = 10
          val min = intValues.min
          val max = intValues.max
          val step = (max - min) / num + 1
          for (i <- 0 until num) {
            val l = min + step * i
            val r = min + step * (i + 1) - 1
            val count = intValues.count(v => l <= v && v <= r)
            out.println(s"$l .. $r: $count")
          }
        }
      } catch {
        case e: FileNotFoundException =>
          throw new IOError(e)
      }
    }

    private def printRawValues(kind: StatsKind, event: String, values: List[Double]): Unit = {
      try {
        Using.resource(TextOutput.fromFile(s"stats-$kind-$event.txt")) { out =>
          values foreach (d => out.println(d.toString))
        }
      } catch {
        case e: FileNotFoundException =>
          throw new IOError(e)
      }
    }

    private def printOneLine(name: String, total: Long, passed: Long): Unit = {
      if (total > 0) {
        println(s"  $name : $passed out of $total  (${100L * passed / total}%)")
      }
    }
  }
}

abstract class CompilerWithStats(_env: Environment) extends Compiler(_env) {

  def stats: Stats

  protected def registerInputMethod(method: Method): Unit = {
    stats.count(MethComp, AllMethods)
    if (method.getDeclaringClass.isJetRuntimeClass) {
      stats.count(MethComp, AllRuntimeMethods)
    }
    if (!method.isManaged) {
      stats.count(MethComp, AllUnmanagedMethodsCount)
    }
  }

  protected def registerCompiledMethod(method: Method): Unit = {
    stats.count(MethComp, PassedMethods)
    if (method.getDeclaringClass.isJetRuntimeClass) {
      stats.count(MethComp, PassedRuntimeMethods)
    }
    if (!method.isManaged) {
      stats.count(MethComp, PassedUnmanagedMethods)
    }
  }

  protected def printStats(compilerName: String): Unit = new StatsPrinter(stats, compilerName).print()
}
