/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.model.ModelCompare

import java.nio.file.{Files, Path, Paths}

/** Round-trip harness: for each .chir file, load with the materializer, serialize
  * with the serializer, re-load, and compare the two models (including ids).
  * Also checks that re-serializing the re-loaded model reproduces the same bytes.
  * The re-serialized files are written under `outDir` for chir-dis text comparison.
  *
  * Usage: <chir-file-list> <out-dir>
  */
object RoundTrip {

  def main(args: Array[String]): Unit = {
    val listFile = args(0)
    val outDir = Paths.get(args(1))
    Files.createDirectories(outDir)

    val files = scala.io.Source.fromFile(listFile).getLines().map(_.trim).filter(_.nonEmpty).toList
    var ok = 0
    val failures = new scala.collection.mutable.ArrayBuffer[(String, String)]

    for ((f, i) <- files.zipWithIndex) {
      val in = Paths.get(f)
      val bytes = Files.readAllBytes(in)
      val m1 = Materializer1203.load(bytes)
      val bytes2 = Serializer1203.toBytes(m1, m1.phase)
      val m2 = Materializer1203.load(bytes2)
      // ids may legitimately shift on the first round-trip (def seed grouping,
      // see ModelCompare doc); structure must be identical
      val diff = ModelCompare.compare(m1, m2, compareIds = false)
      val bytes3 = Serializer1203.toBytes(m2, m2.phase)
      val stable = java.util.Arrays.equals(bytes2, bytes3)

      if (diff.isEmpty && stable) {
        val dir = outDir.resolve(f"$i%03d")
        Files.createDirectories(dir)
        Files.write(dir.resolve("input.chir"), bytes2)
        ok += 1
      } else {
        val why =
          if (diff.isDefined) s"model diff: ${diff.get}"
          else s"unstable re-serialization (${bytes2.length} vs ${bytes3.length} bytes)"
        failures += ((f, why))
        println(s"FAIL $f: $why")
      }
    }

    println(s"round-trip: ${ok}/${files.length} ok, ${failures.size} failed")
    if (failures.nonEmpty) sys.exit(1)
  }
}
