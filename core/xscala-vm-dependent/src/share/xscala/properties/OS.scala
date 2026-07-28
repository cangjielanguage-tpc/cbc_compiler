/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.properties

import xscala.properties.Properties as Props
import xscala.util.StringOps.*

/** Operating systems supported by JET framework. */
enum OS(name: String, val displayName: String) {
  case WINDOWS extends OS("windows", "Windows")
  case LINUX   extends OS("linux",   "Linux")

  override def toString = name

  /** Returns whether this OS is [[# WINDOWS]]. */
  def isWindows = this == WINDOWS

  /** Returns whether this OS is [[# LINUX]]. */
  def isLinux = this == LINUX

  /** Returns the family name of this OS, that is either "no_family" (for Windows) or "posix" (for Linux). */
  def familyName = this match {
    case WINDOWS => "no_family"
    case LINUX   => "posix"
  }

  /** Returns the line separator string for this OS, that is "\r\n" for Windows and "\n" for Linux. */
  def lineSeparator: String = this match {
    case WINDOWS => "\r\n"
    case LINUX   => "\n"
  }

  /** Returns the file separator character for this OS, that is '\' for Windows and '/' for Linux. */
  def fileSeparator: Char = this match {
    case WINDOWS => '\\'
    case LINUX   => '/'
  }

  /** Returns the path separator character for this OS, that is ';' for Windows and ':' for Linux. */
  def pathSeparator: Char = this match {
    case WINDOWS => ';'
    case LINUX   => ':'
  }

  /** Returns the general batch files extension for this OS, that is ".bat" for Windows and ".sh" for Linux. */
  def getBatchFileExtension = this match {
    case WINDOWS => ".bat"
    case LINUX   => ".sh"
  }

  /** Returns the general executable files extension for this OS, that is ".exe" for Windows
    * and empty string for Linux.
    */
  def getExeFileExtension = this match {
    case WINDOWS => ".exe"
    case LINUX   => ""
  }

  /** If this OS allows script files with the extension of executable files, returns that extension;
    * otherwise, returns the same extension as [[OS.getBatchFileExtension]]
    */
  def getExeLikeScriptExtension = this match {
    case WINDOWS => ".bat"
    case LINUX   => ""
  }

  /** Returns the extension of dynamically loaded libraries, that is ".dll" for Windows and ".so" for Linux. */
  def getDllFileExtension = this match {
    case WINDOWS => ".dll"
    case LINUX   => ".so"
  }

  /** Returns the prefix of dynamically loaded library names, that is empty for Windows and "lib" for Linux. */
  def getDllFilePrefix = this match {
    case WINDOWS => ""
    case LINUX   => "lib"
  }

  /** Adds the executable files extension to the given filename. */
  def mangleExeName(exe: String) = s"$exe$getExeFileExtension"

  /** Adds the prefix and extension of dynamically loaded libraries to the given filename. */
  def mangleDllName(dll: String) = s"$getDllFilePrefix$dll$getDllFileExtension"

  /** Adds the extension of exe-like scripts to the given filename. */
  def mangleExeLikeScriptName(script: String) = s"$script$getExeLikeScriptExtension"
}

object OS {
  def apply(name: String): OS = OS.valueOf(name.trim.asciiToUpperCase)

  /** The host [[OS]] for the executing application. */
  val host: OS = Props.get.osName() match {
    case x if x.startsWith("Windows") => OS.WINDOWS
    case x if x.startsWith("Linux") => OS.LINUX
    case x if x.startsWith("Mac OS") => OS.LINUX // Does not matter for CBC (TODO: support)
    case x => throw new IllegalArgumentException(s"Unknown OS: '$x'")
  }
}
