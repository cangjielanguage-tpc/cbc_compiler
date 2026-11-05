/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import java.nio.file.{Files, Paths}

/** Round-trip stability checker for CHIR serialization/deserialization: for
  * every input .chir file, materializes it, serializes it back, materializes
  * the result again and serializes it again — the second and third byte arrays
  * must be identical (the first pass may reassign ids, so the input bytes are
  * not required to match; what must hold is that serialization is a fixed
  * point from the second pass on). Used by the `chir-stability` mode of
  * test/runner.py. Exits non-zero if any file fails or cannot be read.
  *
  * Usage: [--chir-version <v>] <chir-file>...
  */
object RoundTripStability {

  def roundTripStable(bytes: Array[Byte], version: Int): Either[String, Array[Byte]] = {
    val m1 = CHIR.loadPackage(bytes, version)
    val bytes2 = CHIR.serializePackage(m1, version, m1.phase)
    val m2 = CHIR.loadPackage(bytes2, version)
    val bytes3 = CHIR.serializePackage(m2, version, m2.phase)
    if (bytes2.length == bytes3.length && bytes2.indices.forall(i => bytes2(i) == bytes3(i))) Right(bytes2)
    else Left(s"unstable re-serialization (${bytes2.length} vs ${bytes3.length} bytes)")
  }

  def main(args: Array[String]): Unit = {
    val (version, files) =
      if (args.length >= 2 && args(0) == "--chir-version") (args(1).toInt, args.drop(2).toSeq)
      else (CHIR.defaultVersion, args.toSeq)
    if (files.isEmpty) {
      System.err.println("Usage: [--chir-version <v>] <chir-file>...")
      sys.exit(2)
    }

    var ok = 0
    val failures = new scala.collection.mutable.ArrayBuffer[(String, String)]
    for (f <- files) {
      try {
        val bytes = Files.readAllBytes(Paths.get(f))
        roundTripStable(bytes, version) match {
          case Right(_) => ok += 1
          case Left(why) =>
            failures += ((f, why))
            println(s"FAIL $f: $why")
        }
      } catch {
        case e: Exception =>
          failures += ((f, e.toString))
          println(s"FAIL $f: $e")
      }
    }

    println(s"chir-stability: ${ok}/${files.length} ok, ${failures.size} failed")
    if (failures.nonEmpty) sys.exit(1)
  }
}
