/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii

import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import com.huawei.excelsior.jet.compiler.xminizip.Minizip.ZwInMemHandle
import xscala.io.{Files, Path}

import java.io.IOException

/** Interface for filtering contents of a directory to package.
  * Also allows to filter the content of particular files
  * and optionally reset mtime to DOSTIME_BASE.
  */
object Filter {
  val ACCEPT_ALL = new Filter() {}

  val RESET_MTIME = new Filter() {
    override def filterToBytes(f: Path): Array[Byte] = Files.readAllBytes(f).toByteArray
    override def resetMtime(f: Path) = true
  }
}

abstract class Filter {
  /** Returns whether the file or directory shall be included.
    *
    * Note that the contents of the file still may be filtered.
    *
    * @param f the file or directory to check
    * @return `true` iff the file or directory shall be included
    * @throws IOException if an I/O error occurred
    */
  def accept(f: Path): Boolean = true

  /** Optionally filters the contents of the file.
    *
    * @param f the file to filter
    * @return the filtered contents of the file, or `null` if the whole (unfiltered) contents is to be included
    * @throws IOException if an I/O error occurred
    */
  def filterToBytes(f: Path): Array[Byte] = {
    assert(f.isFile)
    null // full file is accepted by default
  }

  /** Returns whether mtime should be set to DOSTIME_BASE.
    *
    * Note that the contents of the file still may be filtered.
    *
    * @param f the file or directory to check
    * @return `true` iff the mtime attribute for the file should be changed to DOSTIME_BASE
    * @throws IOException if an I/O error occurred
    */
  def resetMtime(f: Path): Boolean = false

  /**
    * Optionally filters the contents of the file.
    *
    * @param f the file to filter
    * @return the id of zipInMem with the filtered contents of the file (@see Minizip#ZipWriterInMemHandle),
    *         or Minizip.ZIP_IN_MEM_INVALID if the whole (unfiltered) contents is to be included
    * @throws IOException if an I/O error occurred
    */
  def filterToZipInMem(f: Path): ZwInMemHandle = {
    assert(f.isFile)
    Minizip.ZIP_IN_MEM_INVALID // full file is accepted by default
  }
}