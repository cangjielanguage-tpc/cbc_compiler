/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.pdb.archive

import com.huawei.excelsior.jet.common.XString
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe
import xscala.io.Path
import xscala.time.unixMilliseconds

import scala.math.toIntExact

class LegacyPDBReaderSuite extends AnyFunSuite {
  import Utils.sizeStr as sz
  
  private def O2HT_capacity(size: Int): Int = {
    var cap = 11
    val loadFactor = 0.75f
    while ({ val threshold = (cap.toFloat * loadFactor).toInt; size > threshold }) {
      cap = cap * 2 + 1
    }
    cap
  }

  private def measure[A](name: String)(action: => A): A = {
    println(s"measuring $name...")
    val before = unixMilliseconds
    val result = action
    val after = unixMilliseconds
    val time = (after - before) / 1000d
    println(s"... done in $time sec [$name]")
    result
  }
  private def pdbStats(filename: String, allidx: MemIndex, sumsz: Array[Int]): Unit = {
    val r = new LegacyPDBReader
    val path = Path(filename)
    path.exists shouldBe true
    measure(s"openArchive($filename)") { r.openArchive(filename) }

    val XSTRING_SIZE = 40 /*XString*/ + 32 /*byte[]*/
    val AENTRY_SIZE = 48 /*O2 RAA.ArchiveEntry*/
    val HENTRY_SIZE = 48 /*O2 Hashtable.Entry*/

    val htCapacity = O2HT_capacity(r.entriesCount)
    val inMemSize = r.entriesCount * (XSTRING_SIZE + AENTRY_SIZE + HENTRY_SIZE) + r.nameCharsSize + htCapacity * 8

    println("PDB[" + path.name + "]: { fileSize: " + sz(r.fileSize) + ", dataSize: " + sz(r.dataSize) + ", indexSize: " + sz(r.indexSize) + ", " + "entriesCount: " + sz(r.entriesCount) + ", nameCharsCount: " + sz(r.nameCharsCount) + " }")
    println("nameCharsSize: " + sz(r.nameCharsSize) + ", inMemSize: " + sz(inMemSize))
    println()

    sumsz(0) += r.entriesCount
    sumsz(1) += inMemSize

    val suffix = if (r.entriesCount > 0) {
      val name = r.entries.head._1
      val pos = name.lastIndexOf('.')
      assert(pos > 0)
      name.substring(pos)
    } else XString("")

    val idx = Index.withSuffixW(new MemIndex, suffix)
    for ((k, v) <- r.entries) {
      idx.add(k, toIntExact(v.pos), false) shouldBe 0
    }
    idx.dir.stats.print("\nNEW inMemSize: ")
    println()

    val sidx = SerialIndex.from2(idx)
    sidx.dir.stats.print("Serialized Size: ")
    println()

    val nidx = SerialIndex.from2(idx, normalized = true)
    nidx.dir.stats.print("Normalized Size: ")
    println()

    for ((k, v) <- r.entries) {
      allidx.add(k, toIntExact(v.pos), false) shouldBe 0
    }
  }

  private def pdbStats2(dir: String, files: Seq[String]): Unit = {
    println(s"PDB files from directory \"$dir\"")
    val allidx = new MemIndex
    val sumsz = new Array[Int](2)
    for (fname <- files) {
      pdbStats(dir + fname, allidx, sumsz)
    }
    println("SUM entriesCount: " + sz(sumsz(0)) + ", inMemSize: " + sz(sumsz(1)))
    println()
    allidx.stats.print("\nNEW inMemSize: ")
    println()

    val allsidx = SerialIndex.from(allidx)
    allsidx.stats.print("Serialized Size: ")
    println()
  }

  ignore("PDBStats") {
    val testDataDir = "D:/JETdev/develop/pdb2-archive"

    val pdir = s"$testDataDir/JavaLP-profile/"
    val pfiles = Seq("irb.pdb", "irei.pdb", "mbi.pdb", "sym.pdb")
    pdbStats2(pdir, pfiles)

    val cjlibdir = s"$testDataDir/CangJieStdLib/"
    val cjlibfiles = Seq("irb.pdb", "irei.pdb", "sym.pdb")
    pdbStats2(cjlibdir, cjlibfiles)

    val eclipsedir = "C:/Projects/!tmp/eclipse47pdb/"
    val appfiles = Seq("irb.pdb", "irei.pdb", "sym.pdb", "mbi.pdb", "cho.pdb", "mod.pdb")
    //pdbStats2(eclipsedir, appfiles);
  }
}
