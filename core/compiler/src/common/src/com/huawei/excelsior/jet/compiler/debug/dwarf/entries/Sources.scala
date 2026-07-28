/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.entries

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Table of source files used in one compilation unit.
  *
  * Compilation unit and it's LNP (Line Number Program) use indices from this table as references to files.
  *
  * The table itself generated as a part of LNP header.
  *
  * @author gatimosh
  * @author conwor
  */
final class Sources extends Dwarf.Entry { sources =>
  private val cache = new mutable.LinkedHashMap[XString, Int]
  def id(file: XString): Int = cache.getOrElseUpdate(file, { cache.size + 1 })

  override def close(): Segment = {
    val dirs = new mutable.LinkedHashMap[XString, Int]
    def dirId(dir: XString) = dirs.getOrElseUpdate(dir, { dirs.size + 1 })

    val files = new ArrayBuffer[(XString, Int)]
    for (file <- cache.keysIterator) {
      val (name, dir) = file.lastIndexOf('/') match {
        case -1 => (file, 0) // 0 stands for "current directory of the compilation"
        case x => (file.substring(x + 1), dirId(file.substring(0, x)))
      }
      assert(!name.isEmpty)
      files += ((name, dir))
    }

    dirs.keysIterator foreach nullTerminatedString  // include_directories
    ubyte(0)                                        // include_directories termination
    for ((name, dir) <- files) {
      nullTerminatedString(name)  // name of a source file
      uleb128(dir)                // directory index of a directory in the include_directories section
      uleb128(0)                  // time of last modification
      uleb128(0)                  // length in bytes of the file
    }
    ubyte(0) // file_names termination
    super.close()
  }
}