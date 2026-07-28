/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler
import xscala.io.{DataOutput, stderr, stdout}

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

object AsmParser {
  def main(args: Array[String]): Unit = {
    try {
      sys.exit(run(args))
    } catch {
      case e: Throwable =>
        stderr.println()
        stderr.println("Unexpected internal error:")
        stderr.printStackTrace(e)
        sys.exit(1)
    }
  }

  private def printHelp(): Unit = {
    stdout.println("CBC Assembler Proto")
    stdout.println("Usage: java -jar cbc-asm.jar <asm-file>")
  }

  private def constructOutputPath(str: String): String = {
    var outputPath = str
    if (outputPath.endsWith(".asm")) {
      outputPath = outputPath.stripSuffix(".asm")
    }
    outputPath + ".cbc"
  }
  
  private def run(args: Array[String]): Int = {
    if (args.isEmpty || args.head == "--help") {
      printHelp()
      return if (args.isEmpty) 1 else 0
    }

    val inputPath = Paths.get(args(0)).normalize()

    val builder = CbcFileFormat.newBuilder()
    builder.setBytecodeVersion(Assembler.BYTECODE_VERSION)

    stdout.println(s"Assembling $inputPath")
    val lines = Files.readAllLines(inputPath)
    val successful = NewAsmParser(builder, lines.asScala.toSeq).parse()
    if (!successful) {
      return 2
    }
    Using.resource(DataOutput.fromFile(constructOutputPath(inputPath.toString))) { out =>
      CbcFileEncoder.gen(builder.build(), out)
    }
    0
  }
}
