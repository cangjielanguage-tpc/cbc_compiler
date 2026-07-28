/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.options.BoolOption.{PrintGCMapsLength, SilentCompilation}
import xscala.io.ByteBuffer

import scala.annotation.elidable
import scala.collection.mutable

/**
  * Collects statistics about generated GC maps.
  *
  * @author minium
  */
// TODO: Make this logger thread-safe.
object GCMapStatisticCollector {
  private var gcMapSize: Int = 0
  private val recordsGCMap = mutable.ArrayBuffer.empty[String]
  private val regChanges = new CountersArray
  private var cases = 0

  private def enabled(implicit env: Environment) = env.enabled(PrintGCMapsLength)
  
  def recordGCMap(message: => String)(implicit env: Environment): Unit = {
    // We report GC map messages only if PrintGCMapsLength is enabled (see the `collect` method below).
    if (enabled) {
      recordsGCMap += message
    }
  }

  def print()(implicit env: Environment): Unit = {
    if (enabled) {
      println(s"GCMaps size: $gcMapSize")
      println(s"All maps sets: $cases.")
      println("------ Reg. statistic ------")
      regChanges.print()
    }
  }

  def registerChanges(mask: Int)(implicit env: Environment): Unit = {
    if (enabled) {
      regChanges inc Integer.bitCount(mask)
    }
  }

  def collect(gcMap: ByteBuffer)(implicit env: Environment): Unit = {
    if (enabled) {
      val gcMapLen = if (gcMap != null) gcMap.length else 0
      env.println(s"[Encode GCMaps] Length: $gcMapLen byte.")
      gcMapSize += gcMapLen
      cases += 1
      recordsGCMap foreach env.println
      recordsGCMap.clear()
    }
  }

  private class CountersArray {
    private val counters = mutable.Map.empty[Int, Int]

    def inc(index: Int): Unit = {
      counters.updateWith(index) {
        case Some(c) => Some(c + 1)
        case None => Some(1)
      }
    }

    def print(): Unit = {
      for ((k, v) <- counters.toArray.sortInPlaceBy(_._1)) {
        println(s"$k : $v")
      }
    }
  }
}
