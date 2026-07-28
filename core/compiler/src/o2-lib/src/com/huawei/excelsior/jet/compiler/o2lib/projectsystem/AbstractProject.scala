/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.CPEntryModes.CPEntryMode

/**
  * Project is a set of input files for compilation.
  *
  * Project may be filled by input files using compiler arguments or by reading a project file.
  *
  * {@link com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners.ScanPro} reads project files in XDS format and
  * may fill an AbstractProject object using append* methods.
  *
  * @tparam F project element that may be returned by append* methods or nothing (Unit) if there is no need for the result
  */
abstract class AbstractProject[F] {

  /** appends an input file to the project  */
  def appendFile(fileName: XString): F

  // bellow are methods that came from legacy XDS source code:

  /** handles !classloaderentry projects directives */
  def appendClassloaderEntry(cpentry: XString, cpentrymode: CPEntryMode, bidInInternalForm: Boolean, userDef: Boolean): F

  /** handles !classpathentry projects directives */
  def appendClasspathEntry(cpentry: XString): F

  /** Special method for adding classes by their names to the project (used by .usg, .fus readers).
    * The method looks for class files by class names and if succeeds then appends found file to the project.
    *
    * @param class0 name of the class to find and add to the project
    * @param doError means if should issue an error if the class is not found by its name or just ignore it.
    * */
  def findClassAndAppend(class0: XString, doError: Boolean): Unit

  /** opens PDB for the project if it is required */
  def openPDB(): Unit

  /** Sets the project to erroneous state. The method is called by a project reader when a (syntax) error found. */
  def setErr(): Unit
}
