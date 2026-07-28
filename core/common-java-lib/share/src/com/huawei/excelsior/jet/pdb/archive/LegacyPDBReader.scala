/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.pdb.archive

import com.huawei.excelsior.jet.common.XString
import xscala.io.*

import scala.collection.mutable
import scala.math.toIntExact
import scala.util.Using

object LegacyPDBReader {
  private val MAGIC = 0x504442DB
  private val VERSION = 2

  class ArchiveEntry (val name: XString, val time: Int, val pos: Long, val size: Int)
}

class LegacyPDBReader {
  import LegacyPDBReader.*

  /* PDB archive has the following format (all fields are in BigEndian order):
   *   0: MAGIC   (4 bytes)
   *   4: VERSION (4 bytes)
   *   8: Index position (8 bytes)
   *  f1: file1 (n1 bytes)
   *  f2: file2 (n2 bytes)
   *    ...
   *  fN: fileN  (nN bytes)
   *   L: Index (L = Index position)
   *  , where fileI is contents of I-th file in the archive.
   *
   * Index has the following format:
   *   L+0: MAGIC
   *   L+4: N (number of files)
   *   L+8: ARRAY N OF ArchiveEntry
   *
   * ArchiveEntry has the following format:
   *   E+0: name (zero-terminated UTF8 string)
   *   X+0: time of modification in seconds (4 bytes)
   *   X+4: position of the contents of the I-th file in the archive -- fI (8 bytes)
   *  X+12: length of the contents of the I-th file in the archive -- nI (4 bytes)
   *
   * NOTE: files more than 2G inside PDB archives are not supported yet.
   */

  def openArchive(filename: String): Unit = {
    val fsize = Files.size(Path(filename))
    Using.resource(DataInput.from(Path(filename), buffered = true)) { in =>
      //val fsize = file.size
      val be = BigEndian(in)

      errorIf(be.getW32() != MAGIC, "Bad magic")
      val ver = be.getW32()
      println(s"version: $ver")
      errorIf(ver != VERSION, "Unsupported version")
      val indexPos = be.getW64()
      //file.cursor = indexPos
      in.skip(toIntExact(indexPos - 16))

      errorIf(be.getW32() != MAGIC, "Bad magic")
      val indexLength = be.getW32()

      for (i <- 0 until indexLength) {
        val name = readZString(in)
        consumeEntry(name, be.getW32(), be.getW64(), be.getW32())
      }
      //errorIf(file.cursor != fsize, "Garbage at the end of file")
      errorIf(in.available != 0, "Garbage at the end of file")

      // fill stats
      fileSize = fsize
      dataSize = indexPos
      indexSize = fileSize - dataSize
      entriesCount = indexLength
    }
  }

  private val strbuf = ByteBuffer()
  private def readZString(in: DataInput): XString = {
    strbuf.reset() // TODO: make utils for more efficient reading of such patterns (explicit InputBuffer or BufferedInput with mark/reset)
    var ch: Int = -1
    while ({ ch = in.getUW8(); ch != 0 }) {
      strbuf.putByte(ch)
    }
    XString.slice(strbuf.getBytesPointer, 0, strbuf.length)
  }

  private def errorIf(cond: Boolean, msg: String): Unit = { if (cond) throw new Error(msg) }

  val entries = mutable.HashMap.empty[XString, ArchiveEntry]

  private def consumeEntry(name: XString, time: Int, pos: Long, size: Int): Unit = {
    nameCharsCount += name.length
    nameCharsSize += (name.length + 7) / 8 * 8
    entries.put(name, new ArchiveEntry(name, time, pos, size))
  }

  // stats
  var fileSize: Long = -1
  var dataSize: Long = -1
  var indexSize: Long = -1
  var entriesCount = -1
  var nameCharsCount = 0
  var nameCharsSize = 0
}
