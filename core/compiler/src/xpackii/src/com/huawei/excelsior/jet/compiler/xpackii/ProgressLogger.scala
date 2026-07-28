/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii

import xscala.io.{Path, TextOutput}

/** A progress logger for packaging actions.
  *
  * Creates a logger with the specified `out` and `err` streams.
  *
  * @param out the stream to print general messages to
  * @param err the stream to print error messages to
  */
class ProgressLogger(out: TextOutput, err: TextOutput) {

  private[xpackii] def progressStart(): Unit = {
    out.println("Creating package...")
  }

  private[xpackii] def progress(msg: String): Unit = {
    out.println(msg)
  }

  private[xpackii] def progressEnd(zipFile: Path): Unit = {
    out.println(s"Zip archive containing the image created at \"${zipFile.canonicalPath}\"")
  }

  /** Reports a fatal error and terminates application. */
  def fatalError(msg: String): Unit = {
    err.println(msg)
    sys.exit(-1)
  }

  /** Reports a fatal exception and terminates application. */
  def fatalError(e: Throwable): Unit = {
    err.println("Terminating packing due to exception.")
    err.printStackTrace(e)
    sys.exit(-1)
  }
}
