/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.delayed

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.PDB2.EntryKind
import com.huawei.excelsior.jet.compiler.driver.CompilationWorker
import com.huawei.excelsior.jet.compiler.options.NumOption
import com.huawei.excelsior.jet.compiler.symlevel.Method

import scala.collection.mutable
import scala.util.Using

/** Tracks classes that use delayed intrinsics to recompile them at time when the intrinsics can be resolved. */
object DelayedIntrinsicsUsageTracker {
  var delayedUsage = mutable.LinkedHashSet[String]()

  var env: Environment = _

  def registerDelayedIntrinsicsUsage(useMethod: Method): Unit = {
    delayedUsage += useMethod.getDeclaringClass.getName
  }

  def serialize(): Unit = {
    val worker = env.valueOf(NumOption.Worker)
    val workerMode = worker != 0
    if (workerMode) {
      serializeOne(s"-$worker")
    } else {
      CompilationWorker.foreach() { worker =>
        deserializeOne(_ => (), s"-$worker")
      }
      serializeOne("")
    }
  }

  private def locationInPdb(infix: String) = EntryKind.DelayedUsage.loc(s"delayedUsage$infix")

  private def serializeOne(infix: String): Unit = {
    if (delayedUsage.nonEmpty) {
      Using.resource(env.pdb.getDataOutput(locationInPdb(infix))) { out =>
        out.putW32(delayedUsage.size)
        // Sort class names to account for parallel compilation instability (see JET-14588)
        delayedUsage.toArray.sorted.foreach(className => out.putUTF(className))
      }
    }
  }

  def deserialize(typeLoader: String => Unit): Unit = {
    deserializeOne(typeLoader, "")
  }

  private def deserializeOne(typeLoader: String => Unit, infix: String): Unit = {
    val rawIn = env.pdb.getDataInputOrNull(locationInPdb(infix))
    if (rawIn != null) {
      Using.resource(rawIn) { in =>
        val count = in.getW32()
        for (_ <- 1 to count) {
          val c = in.getUTF()
          typeLoader.apply(c)
          delayedUsage += c
        }
      }
    }
  }

  def isClassUsedDelayedIntrinsics(className: String): Boolean = delayedUsage contains className

}
