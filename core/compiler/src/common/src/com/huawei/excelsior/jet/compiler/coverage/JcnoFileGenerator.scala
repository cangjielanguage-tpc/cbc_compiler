/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.coverage

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.compiler.adler32.Adler32
import xscala.io.TextOutput
import xscala.text.Utf8Encoding

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object JcnoFileGenerator {

  private var prevFile: String = null

  private case class Block(id: Int, locs: Array[(String, Array[Int])]) {
    def write(jcno: TextOutput): Unit = {
      assert(locs.nonEmpty)
      for ((file, lines) <- locs) {
        if (!file.equals(prevFile)) {
          jcno.println(file)
          prevFile = file
        }
        val arr = new ArrayBuffer[(Int, Int)]
        var start = lines.head
        var prev = start
        for (line <- lines.tail) {
          if (line != prev + 1) {
            arr += ((start, prev))
            start = line
          }
          prev = line
        }
        arr += ((start, prev))
        for ((start, end) <- arr) {
          if (start == end) {
            jcno.println(s":$start")
          } else {
            jcno.println(s":$start..$end")
          }
        }
      }
    }
  }

  private val blocks = new ArrayBuffer[Block]
  val checksum = new Adler32

  // all the valid source lines
  private val sourceLines = mutable.LinkedHashMap.empty[String, mutable.LinkedHashSet[Int]]

  def sendSourceLines(filename: String, line: Int): Unit = {
    sourceLines.getOrElseUpdate(filename, mutable.LinkedHashSet.empty) += line
  }

  def send(id: Int, locs: Array[(String, Array[Int])]): Unit = {
    blocks += Block(id, locs)

    val b = new Array[Byte](4)
    def update(v: Int): Unit = {
      b(0) = (v >>> 24).toByte
      b(1) = (v >>> 16).toByte
      b(2) = (v >>> 8).toByte
      b(3) = v.toByte
      checksum.update(b, 4)
    }

    update(id)
    for ((file, lines) <- locs) {
      val bytes = file.getBytes
      checksum.update(bytes, bytes.length)
      for (line <- lines) {
        update(line)
      }
    }
  }

  def completeValidLines(): Unit = {
    for {
      block <- blocks
      (file, lines) <- block.locs
    } {
        sourceLines(file) --= lines
    }
    val lines = sourceLines.filter((k, v) => v.nonEmpty).map((k, v) => (k, v.toArray.sorted)).toArray
    if (lines.nonEmpty) {
      send(-1, lines)
    }
  }

  def generate(outputName: String): Unit = {
    val jcno = TextOutput.fromFile(outputName + ".jcno", encoding = Utf8Encoding)
    try {
      jcno.println("Checksum")
      jcno.println("0x" + checksum.getValue.toHexString)
      var prev = blocks.head
      jcno.println("!" + prev.id)
      prev.write(jcno)
      for (block <- blocks.tail) {
        if (block.id == prev.id + 1) {
          jcno.println("!")
        } else {
          jcno.println("!" + block.id)
        }
        block.write(jcno)
        prev = block
      }
    } finally {
      jcno.close()
    }
  }
}
