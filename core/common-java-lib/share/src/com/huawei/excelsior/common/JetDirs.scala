/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import com.huawei.excelsior.common.Environment.HOST_OS
import xscala.io.Path
import xscala.properties.Properties

/** This class provides facilities for working with JET directories.
  *
  * @author dbg
  */
object JetDirs {
  /** JET home directory. */
  private var _home: Path = _

  /** JET bin directory ending with separator. */
  lazy val bin: Path = _home/"bin"

  /** JET JRE versions directory ending with separator. */
  lazy val versions: Path = bin/"versions"

  /** JET application data directory ending with separator. */
  lazy val userHome: Path = Properties.get.userHome() match {
    case null => bin
    case uh => Path(uh)/".ExcelsiorJET"
  }


  private lazy val jc: Path = (bin/"jc").script
  private lazy val jcJDK: Path = (bin/"jc-jdk").script

  // see JET-16961
  def jc(cjStdLibCompilationEnabled: Boolean): String = {
    if (cjStdLibCompilationEnabled && JetDirs.jcJDK.exists) JetDirs.jcJDK.toString else JetDirs.jc.toString
  }

  /** Returns JET home directory. */
  def jetHome: Path = _home

  def cjcBin: Path = jetHome/".." // jet is located in bin directory of Cangjie directory

  private def looksLikeJetHome(dir: Path) = dir.isDirectory && (dir/"bin/jc.cfg").exists

  private def startsOption(c: Char) =
    c == '-' || (HOST_OS.isWindows && c == '/')

  /** Parses `-jethome` argument from the given arguments.
    *
    * @param args the arguments to search in
    * @return path to JET home, or `null` if none found
    */
  private def parseJetHomeArg(args: Array[String]): Path = {
    if (args == null || args.length < 2) {
      return null
    }
    val arg1 = args(0)
    if (arg1.isEmpty || !startsOption(arg1.charAt(0)) || !"jethome".equalsIgnoreCase(arg1.substring(1))) {
      return null
    }
    val jethomeDir = Path(args(1))
    if (!looksLikeJetHome(jethomeDir)) {
      return null
    }

    jethomeDir
  }

  /** Tries to obtain jet home directory from -jethome program argument. Returns advanced arguments (without -jethome
    * argument). Failed if jet home directory not found.
    */
  def obtainJetHome(args: Array[String]): Array[String] = {
    _home = parseJetHomeArg(args)
    assert(_home != null, s"-jethome argument is missing or corrupted, args: ${args.mkString("(", ", ", ")")}")
    args drop 2
  }
}
