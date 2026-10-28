/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.patchgen

import java.nio.file.{Files, Paths}
import com.huawei.excelsior.jet.compiler.chir.model.{Package, Phase}
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
  * the source and the patched versions is generated. The patch is written to
  * `"diff" + <patched basename>` in the current working directory, phase OPT.
  */
object Main {
  private val DefaultChirVersion = 13002
  private val SupportedChirVersions = Seq(13002, 1203)

  private def usage(): Unit = {
    System.err.println("Usage:")
    System.err.println("  patch-gen [--chir-version <v>] [--test] [--debug] <patched_chir_path>")
    System.err.println("  patch-gen [--chir-version <v>] [--test] [--debug] <source_chir_path> <patched_chir_path>")
    System.err.println("  --test: after generating the patch, re-deserialize it and dump the identifiers of")
    System.err.println("          non-imported global vars, functions with body and custom type defs to stdout")
    System.err.println("          (equivalent of the C++ #if TEST build; compared by the patch-gen test suite)")
    System.err.println("  --debug: print the C++ #if DEBUG phase banners, marking traces and IR dumps")
    System.err.println("Supported versions: " + SupportedChirVersions.mkString(", ") +
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
      case 13002 => Serializer13002.toBytes(pkg, Phase.Opt)
      case 1203  => Serializer1203.toBytes(pkg, Phase.Opt)
      case v     => throw new IllegalArgumentException("unsupported CHIR version: " + v)
    }
    Files.write(Paths.get(outPath), bytes)
  }

  /** Port of `genPatch`; returns the patch file name on success (None mirrors nullopt). */
  private def genPatch(sourcePackage: Option[Package], patchedChirPath: String, chirVersion: Int): Option[String] = {
    if (!Files.isReadable(Paths.get(patchedChirPath))) {
      System.err.println("Error: CHIR file does not exist: " + patchedChirPath)
      return None
    }
    val patchedPackage = try {
      load(patchedChirPath, chirVersion)
    } catch {
      case e: RuntimeException =>
        System.err.println("Error: Couldn't deserialize: " + patchedChirPath)
        System.err.println(e.getMessage)
        return None
    }

    // TODO rename to patch everywhere
    val patchFileName = "diff" + Paths.get(patchedChirPath).getFileName.toString
    try {
      val patchGen = new PatchGen(patchFileName, sourcePackage, patchedPackage)
      patchGen.createPatch()
      // C++ serializes inside PatchGen::genPatch; it always runs last there, so
      // serializing here (only on success) is observably equivalent
      serialize(patchedPackage, chirVersion, patchFileName)
      Some(patchFileName)
    } catch {
      case e: PatchGenException =>
        System.err.println(e.getMessage)
        None
    }
  }

  def main(args: Array[String]): Unit = {
    var chirVersion = DefaultChirVersion
    var testMode = false
    var debugMode = false
    val rest = ArrayBuffer[String]()
    var i = 0
    var bad = false
    while (i < args.length) {
      if (args(i) == "--chir-version") {
        if (i + 1 >= args.length) {
          System.err.println("missing value for --chir-version")
          bad = true
          i = args.length
        } else {
          chirVersion = args(i + 1).toIntOption.getOrElse {
            System.err.println("invalid --chir-version value: " + args(i + 1))
            bad = true
            DefaultChirVersion
          }
          if (!SupportedChirVersions.contains(chirVersion)) {
            System.err.println("unsupported CHIR version: " + chirVersion)
            bad = true
          }
          i += 2
        }
      } else if (args(i) == "--test") {
        testMode = true
        i += 1
      } else if (args(i) == "--debug") {
        debugMode = true
        i += 1
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

    val sourcePackage = sourcePath match {
      case Some(sourceChirPath) =>
        if (!Files.isReadable(Paths.get(sourceChirPath))) {
          System.err.println("Error: Source CHIR file does not exist: " + sourceChirPath)
          sys.exit(1)
        }
        try {
          Some(load(sourceChirPath, chirVersion))
        } catch {
          case e: RuntimeException =>
            System.err.println("Error: Couldn't deserialize: " + sourceChirPath)
            System.err.println(e.getMessage)
            sys.exit(1)
        }
      case None => None
    }

    if (debugMode) {
      Debug.setEnabled(true)
    }

    genPatch(sourcePackage, patchedPath, chirVersion) match {
      case None =>
        sys.exit(1)
      case Some(patchFileName) =>
        // C++ main(): #if DEBUG success line, then #if TEST re-deserialize + dump
        if (debugMode) {
          println("Patch was successfully generated in " + patchFileName)
        }
        if (testMode) {
          try {
            DebugDump.dumpTestInfo(load(patchFileName, chirVersion))
          } catch {
            case e: RuntimeException =>
              System.err.println("Error: Couldn't deserialize: " + patchFileName)
              System.err.println(e.getMessage)
              sys.exit(1)
          }
        }
    }
  }
}
