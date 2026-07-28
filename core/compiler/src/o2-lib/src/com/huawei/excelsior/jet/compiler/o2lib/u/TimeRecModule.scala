/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.MemoryManagementModule.getTotalGCTime
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FileIOModule as FileIO, MemoryManagementModule as MemoryManagement, TimeModule as Time}
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.UInt

object TimeRecModule {
  private class StageStatistics {
    var pureTime: Long = 0
    var dirtyTime: Long = 0
    var touched = false
    var callCount = 0

    /** Required to calculate dirty time only for last popped frame. If not take it into account, dirty time will be
      * calculated for all frames with this state in stack.
      */
    var recursionLevel = 0
  }

  private class StageInfo(var name: XString, var clearTime: Int, var gcTime: Int)

  private val stageInfoOrdering = Ordering.by[StageInfo, Int](x => -x.clearTime)

  type StageType = Int
  val METHOD_BACKEND = 0
  val CLASS_BACKEND = 1
  val METHOD_MIDDLE = 2
  val CLASS_MIDDLE = 3
  val CLASS_PARSING = 4
  private val LAST_STAGE_TYPE = CLASS_PARSING

  private lazy val maxTopSlows = {
    val maxTopStr = env.config.equation("topslowscount")
    if (maxTopStr != null) js.parseInt(maxTopStr) else 10
  }

  private class TotalStageInfo(val stageType: Int) {
    def logHeader = stageType match {
      case METHOD_BACKEND => "Top slow back-end for procedures"
      case CLASS_BACKEND => "Top slow back-end for classes"
      case METHOD_MIDDLE => "Top slow middle-end for procedures"
      case CLASS_MIDDLE => "Top slow middle-end for classes"
      case CLASS_PARSING => "Top slow parsing for classes"
    }

    val tops = new Array[StageInfo](maxTopSlows)
    var currTopsCount = 0
    var entersCount: Int = 0
    var sumTime: Long = 0
    var sumGCTime: Long = 0

    private var curr: StageInfo = _

    def startStage(name: XString): Unit = {
      curr = new StageInfo(name, Time.getTimeMillisFromMidnight.toInt, getTotalGCTime)
      entersCount += 1
    }

    private def insert(pos: Int, curr: StageInfo): Unit = {
      if (currTopsCount < tops.length) {
        currTopsCount += 1
      }
      Array.copy(tops, pos, tops, pos + 1, currTopsCount - (pos + 1))
      tops(pos) = curr
    }

    def stopStage(name: XString): Unit = {
      assert(curr.name == name)
      curr.gcTime = getTotalGCTime - curr.gcTime
      curr.clearTime = (Time.getTimeMillisFromMidnight.toInt - curr.clearTime) - curr.gcTime
      val pos = tops.search(curr)(stageInfoOrdering).insertionPoint
      if (pos < tops.length) {
        insert(pos, curr)
      }
      sumTime += curr.clearTime.toLong
      sumGCTime += curr.gcTime.toLong
    }
  }

  private var timeToFile: Boolean = false
  private var file: FileIO.FileOutputStream = _
  private var exename: XString = _
  private var compilationStart: UInt = _ // when compilation began (in milliseconds from the beginning of the day)
  private var totalCPUTicks: Long = _    // total compilation time (in CPU ticks)
  private var totalGCTime: UInt = _      // total time spent in GC (in milliseconds)
  private var coef: Long = 0             // totalCPUTicks/totalTime
  private var on: Boolean = false        // whether all this code is on or not
  private var countingReq: Boolean = _
  private val stages = new Array[TotalStageInfo](LAST_STAGE_TYPE + 1)

  private val table = new Array[StageStatistics](Stage.values.length)
  private def stats(stage: Stage) = table(stage.ordinal)

  def setModuleName(s: XString): Unit = {
    exename = js.format("%S.tic", s)
  }

  private def print(format: String, x: Any*): Unit = {
    if (timeToFile) {
      file.fprintf(format, x: _*)
    } else {
      env.info.print(format, x: _*)
    }
  }

  /*  translate CPU ticks to milliseconds */
  /*  call is possible only only after coefficient "coef" is evaluated in Dump () */
  private def getTime(scope: Long): Int = {
    assert(coef != 0)

    val time = scope / coef
    if (time > Int.MaxValue) {
      print("\\nTimeRec Error - Crazy clock\\n")
      return 0
    }

    time.toInt
  }

  private class Frame {
    val start = Time.getHighResolutionTime
    var timeOut = 0L
  }

  private var frames = List.empty[Frame]

  private def enter(s: Stage): Unit = {
    val stat = stats(s)
    stat.touched = true
    stat.callCount += 1
    stat.recursionLevel += 1
    frames = new Frame :: frames
  }

  private def leave(s: Stage): Unit = {
    val stat = stats(s)
    val frame = frames.head
    frames = frames.tail
    val dirtyTime = Time.getHighResolutionTime - frame.start
    stat.pureTime += dirtyTime - frame.timeOut
    stat.recursionLevel -= 1
    if (stat.recursionLevel == 0) {
      stat.dirtyTime += dirtyTime
    }
    if (frames.nonEmpty) {
      frames.head.timeOut += dirtyTime
    }
  }

  def stage[A](s: Stage)(action: => A): A = if (!on) action else {
    enter(s)
    try action finally leave(s)
  }

  def init(): Unit = {
    countingReq = env.config.option("counttopslows")
    if (countingReq) {
      for (st <- METHOD_BACKEND to CLASS_PARSING) {
        stages(st) = new TotalStageInfo(st)
      }
    }

    on = env.config.option("timing")
    if (on) {
      for (s <- Stage.values) {
        table(s.ordinal) = new StageStatistics()
      }
      enter(Stage.Other)
    }

    compilationStart = Time.getTimeMillisFromMidnight
    totalCPUTicks = 0 - Time.getHighResolutionTime
  }

  private def printTime(t: Int, total: Int): Unit = {
    var percent: Int = 0

    if (env.config.option("timerecsecs")) {
      print("%d", t)
    } else {
      if (timeToFile) {
        val s = O2JSupport.div(t, 1000)
        val ms = O2JSupport.mod(t, 1000)
        print("%5d.%03d", s, ms)
      } else {
        val m = O2JSupport.div(t, 60 * 1000)
        val s = O2JSupport.div(O2JSupport.mod(t, 60 * 1000), 1000)
        val ms = O2JSupport.mod(t, 1000)

        if (m == 0) {
          print("%5d.%03d", s, ms)
        } else {
          print("%2d.%02d.%03d", m, s, ms)
        }
      }

      if (total > 0) {
        percent = (t.toLong * 100 * 100 / total.toLong).toInt
      } else {
        print(" total=%d ", total)
        percent = 0
      }

      print(" (%3d.%02d%%)", O2JSupport.div(percent, 100), O2JSupport.mod(percent, 100))
    }
  }

  private def dump(totalTime: UInt): Unit = {
    print("\\n")
    print("----------------------------+ Time Consumed -----+--------------------+----------+\\n")
    print("Stage                       |  Pure Time Spent   |  Dirty Time Spent  |   Call   |\\n")
    print("                            |       (ms)         |        (ms)        |   Count  |\\n")
    print("----------------------------+--------------------+--------------------+----------+\\n")

    for (s <- Stage.values if stats(s).touched) {
      print("%-25.25s   |", s.toString)
      printTime(getTime(stats(s).pureTime), totalTime.toInt)
      print(" |")
      printTime(getTime(stats(s).dirtyTime), totalTime.toInt)
      print(" | %8d |\\n", stats(s).callCount)
    }

    print("----------------------------+--------------------+--------------------+----------+\\n")

    print("\\nSpent in GC: ")
    printTime(totalGCTime.toInt, totalTime.toInt)
    print("\\n\\nTotal: ")
    printTime(totalTime.toInt, totalTime.toInt)
    print("\\n")
  }

  private def countTotalTime(): UInt = {
    var time = Time.getTimeMillisFromMidnight

    // Time.getTimeMillisFromMidnight returns time in ms from the beginning of the day (0:00:00,000)
    // so if we started compilation before midnight and ended it after midnight
    // time will be less than compilationStart
    if (time < compilationStart) {
      time = (time + (24 * 60 * 60 * 1000).toUInt) - compilationStart
    } else {
      time -= compilationStart
    }

    // this mighty trick helps us to avoid division by zero :)
    if (time == UInt(0)) {
      time = UInt(1)
    }

    time
  }

  def done(): Unit = {
    val totalTime = countTotalTime()
    totalCPUTicks += Time.getHighResolutionTime
    totalGCTime = getTotalGCTime.toUInt

    val time = totalTime.toLong
    coef = totalCPUTicks / time

    if (!on) {
      printTopSlows()
      return
    }

    leave(Stage.Other)
    assert(frames.isEmpty)

    var fname = env.config.equation("timingfile")
    timeToFile = if (fname != null) {
      true
    } else if (env.config.option("timingfile") && exename != null) {
      fname = exename
      true
    } else false

    if (timeToFile) {
      file = FileIO.newFileOutputStream(fname)
    }

    dump(totalTime)
    printTopSlows()

    if (timeToFile) {
      file.close()
      timeToFile = false
      print("TimeRec stamp saved to %S\\n", fname)
    }
  }

  //--------------------------------------------------------------

  def startStage(stType: StageType, name: XString): Unit = {
    if (countingReq) {
      stages(stType).startStage(name)
    }
  }

  def stopStage(stType: StageType, name: XString): Unit = {
    if (countingReq) {
      stages(stType).stopStage(name)
    }
  }

  // format: mm:ss.ms
  private def printFormattedTime(label: String, msecsPar: Long): Unit = {
    var msecs = msecsPar
    val width: Int = 6

    val ms = (msecs % 1000).toInt
    msecs = msecs / 1000
    val ss = (msecs % 60).toInt
    val mm = (msecs / 60).toInt

    if (mm == 0) {
      print("  %s:%*d.%03d sec;", label, (width - 4).toUInt.toInt, ss, ms)
    } else {
      print("  %s:%*d:%02d sec;", label, (width - 3).toUInt.toInt, mm, ss)
    }
  }

  private def printTopSlows(): Unit = {
    if (!countingReq) {
      return
    }
    for (st <- METHOD_BACKEND to CLASS_PARSING) {
      print("\\n%s:\\n\\n", stages(st).logHeader)
      print("    enters count: %d\\n", stages(st).entersCount)
      print("  ")
      printFormattedTime("summary time", stages(st).sumTime)
      print("\\n  ")
      printFormattedTime("summary GC time", stages(st).sumGCTime)
      print("\\n  ")
      if (stages(st).entersCount != 0) {
        printFormattedTime("average time", stages(st).sumTime / stages(st).entersCount.toLong)
        print("\\n  ")
        printFormattedTime("average GC time", stages(st).sumGCTime / stages(st).entersCount.toLong)
        print("\\n")
      }
      print("\\n")
      for (i <- 0 until stages(st).currTopsCount) {
        printFormattedTime("clear time", stages(st).tops(i).clearTime.toLong)
        printFormattedTime("GC time", stages(st).tops(i).gcTime.toLong)
        print("  name: %S\\n", stages(st).tops(i).name)
      }
      print("\\n")
    }
  }
}
