/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.model.*

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer

/** Ad-hoc test main: materialize .chir files given as arguments and print stats. */
object MaterializerTest {

  def main(args: Array[String]): Unit = {
    var ok = 0
    var failed = 0
    val failures = ArrayBuffer[String]()
    for (arg <- args) {
      val path = Paths.get(arg)
      try {
        val bytes = Files.readAllBytes(path)
        val pkg = Materializer13002.load(bytes)
        // basic sanity: walk everything
        val nExprs = countExprs(pkg)
        ok += 1
        if (args.length == 1) {
          println(s"OK $arg: exprs=$nExprs")
        }
      } catch {
        case e: Throwable =>
          failed += 1
          e.printStackTrace()
          failures += s"$arg: ${e.getClass.getSimpleName}: ${e.getMessage}"
      }
    }
    for (f <- failures.take(20)) println(f)
    println(s"ok=$ok failed=$failed")
    if (failed > 0) sys.exit(1)
  }

  private def countExprs(pkg: Package): Int = {
    var n = 0
    def walkExpr(e: Expression): Unit = {
      n += 1
      e match {
        case le: LambdaExpr => le.body.foreach(walkGroup)
        case _ =>
      }
    }
    def walkGroup(bg: BlockGroup): Unit =
      for (b <- bg.blocks) {
        for (e <- b.exprs) walkExpr(e)
      }
    def walkFunc(f: Func): Unit = f.body.foreach(walkGroup)
    for (v <- pkg.globalVars) v.initializer match {
      case Some(f: Func) => walkFunc(f)
      case _ =>
    }
    for (f <- pkg.globalFuncs) walkFunc(f)
    for (d <- pkg.allCustomTypeDefs) {
      for (f <- d.methods) walkFunc(f)
      for (mv <- d.instanceMemberVars) mv.initializerFunc.foreach(walkFunc)
      d.instanceVarInitFunc.foreach(walkFunc)
      for (gv <- d.staticMemberVars) gv.initializer match {
        case Some(f: Func) => walkFunc(f)
        case _ =>
      }
    }
    pkg.packageInitFunc.foreach(walkFunc)
    pkg.packageLiteralInitFunc.foreach(walkFunc)
    n
  }
}
