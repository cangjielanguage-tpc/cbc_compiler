/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import xscala.io.Path
import xscala.properties.OS

import scala.collection.*

/** Utilities for working with processes, e.g. argument sanitation.
  *
  * @author redteapot
  */
object ProcessUtils {
  private def stripSurroundingQuotes(str: String): String = {
    if (str.startsWith("\"") && str.endsWith("\"")) {
      str.substring(1, str.length - 1)
    } else {
      str
    }
  }

  private def sanitizeArgument(argument: String): String = {
    import OS.*

    OS.host match {
      case LINUX => argument
      case WINDOWS =>
        s"\"${stripSurroundingQuotes(argument).replace("\"", "\\\"")}\""
    }
  }

  private def sanitizeProcessPath(path: String): String = {
    val file = Path(path)
    if (!file.exists) {
      throw new IllegalArgumentException(s"Process path does not exist: $file")
    }
    if (!file.isFile) {
      throw new IllegalArgumentException(s"Process path is not a file: $file")
    }
    if (!file.canExecute) {
      throw new IllegalArgumentException(s"Process path is not an executable file: $file")
    }
    file.canonicalPath.toString
  }

  /** Sanitizes the process name and arguments making sure they are passed correctly.
    *
    * The process path is checked to be an existing executable file.
    * The arguments are checked in an OS-specific way.
    */
  def sanitizeCommand(command: Seq[String]): Seq[String] = {
    Seq(sanitizeProcessPath(command.head)) ++ (command.tail map sanitizeArgument)
  }
}
