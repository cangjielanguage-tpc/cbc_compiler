/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.patchgen

import java.nio.file.{Files, Paths}
import com.huawei.excelsior.jet.compiler.chir.model.Package
import com.huawei.excelsior.jet.compiler.chir.v1203.{Materializer1203, Serializer1203}
import com.huawei.excelsior.jet.compiler.chir.v13002.{Materializer13002, Serializer13002}
import scala.collection.mutable.ArrayBuffer

/** Port of C++ `main.cpp` (namespace PatchGen).
  *
  * Usage:
  *   patch-gen [--chir-version <v>] <patched_chir_path>
  *   patch-gen [--chir-version <v>] <source_chir_path> <patched_chir_path>
  *
  * With a single argument the patched package is used as its own source (the
  * whole package is treated as a hotfix); with two arguments a diff between
  * the source and the patched versions is generated.
  */
object Main {
  private val DefaultChirVersion = 13002
  private val SupportedChirVersions = Seq(13002, 1203)

  private def usage(): Unit = {
    println("Usage:")
    println("  patch-gen [--chir-version <v>] <patched_chir_path>")
    println("  patch-gen [--chir-version <v>] <source_chir_path> <patched_chir_path>")
    println("Supported versions: " + SupportedChirVersions.mkString(", ") +
      " (default " + DefaultChirVersion + ")")
  }

  private def load(chirPath: String, chirVersion: Int): Package = {
    val bytes = Files.readAllBytes(Paths.get(chirPath))
    chirVersion match {
      case 13002 => Materializer13002.load(bytes)
      case 1203  => Materializer1203.load(bytes)
      case v     => throw new IllegalArgumentException("unsupported CHIR version: " + v)
    }
  }

  private def serialize(pkg: Package, chirVersion: Int, outPath: String): Unit = {
    val bytes = chirVersion match {
      case 13002 => Serializer13002.toBytes(pkg, pkg.phase)
      case 1203  => Serializer1203.toBytes(pkg, pkg.phase)
      case v     => throw new IllegalArgumentException("unsupported CHIR version: " + v)
    }
    Files.write(Paths.get(outPath), bytes)
  }

  /** Port of `PatchGen::createHotfix` (returns false on PatchGenException). */
  private def createHotfix(sourcePath: Option[String], patchedPath: String, chirVersion: Int,
                           diffFilePath: String, patchedDiffFilePath: String): Boolean = {
    try {
      val sourcePackage = sourcePath.map(load(_, chirVersion))
      val patchedPackage = load(patchedPath, chirVersion)
      val patchGen = new PatchGen(patchedPath, sourcePackage, patchedPackage)
      patchGen.createPatch()
      serialize(patchedPackage, chirVersion, diffFilePath)
      serialize(patchedPackage, chirVersion, patchedDiffFilePath)
      true
    } catch {
      case e: PatchGenException =>
        println(e.getMessage)
        false
    }
  }

  def main(args: Array[String]): Unit = {
    var chirVersion = DefaultChirVersion
    val rest = ArrayBuffer[String]()
    var i = 0
    var bad = false
    while (i < args.length) {
      if (args(i) == "--chir-version") {
        if (i + 1 >= args.length) {
          println("missing value for --chir-version")
          bad = true
          i = args.length
        } else {
          chirVersion = args(i + 1).toIntOption.getOrElse {
            println("invalid --chir-version value: " + args(i + 1))
            bad = true
            DefaultChirVersion
          }
          if (!SupportedChirVersions.contains(chirVersion)) {
            println("unsupported CHIR version: " + chirVersion)
            bad = true
          }
          i += 2
        }
      } else {
        rest += args(i)
        i += 1
      }
    }
    if (bad || rest.isEmpty || rest.size > 2) {
      usage()
      sys.exit(1)
    }

    val (sourcePath, patchedPath) = rest.size match {
      case 1 => (None, rest(0))
      case 2 => (Some(rest(0)), rest(1))
    }

    val patchedFileName = Paths.get(patchedPath).getFileName.toString
    val diffFileName = "diff" + patchedFileName
    val patchedDiffFileName = "patcheddiff" + patchedFileName

    val ok = createHotfix(sourcePath, patchedPath, chirVersion, diffFileName, patchedDiffFileName)
    if (!ok) {
      sys.exit(1)
    }
  }
}
