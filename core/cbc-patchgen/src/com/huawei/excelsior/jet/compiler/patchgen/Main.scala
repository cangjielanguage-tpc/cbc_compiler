package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.patchgen.{PatchGen, PatchGenException}
import java.nio.file.{Files, Paths}

object Main {
  def main(args: Array[String]): Unit = {
    var version = CHIR.defaultVersion
    var test = false
    var debug = false
    val paths = scala.collection.mutable.ArrayBuffer.empty[String]
    var i = 0
    try {
      while (i < args.length) {
        args(i) match {
          case "--test" => test = true
          case "--debug" => debug = true
          case "--chir-version" =>
            i += 1
            require(i < args.length, "missing --chir-version value")
            version = args(i).toInt
          case path => paths += path
        }
        i += 1
      }
      require(paths.size == 1 || paths.size == 2,
        "Usage: patch-gen [--chir-version <version>] [--test] <source.chir> [patched.chir]")
      for (path <- paths) require(Files.isReadable(Paths.get(path)), "CHIR file does not exist: " + path)
      val source = if (paths.size == 2) Some(CHIR.read(Paths.get(paths.head), version)) else None
      val pkg = CHIR.read(Paths.get(paths.last), version)
      new PatchGen(source, pkg).createPatch()
      val output = Paths.get("diff" + Paths.get(paths.last).getFileName.toString)
      pkg.write(output)
      if (test) {
        val saved = CHIR.read(output, version)
        def imported(v: CHIR.HasAttributes): Boolean = v.attributes.contains(CHIR.Attribute.Imported)
        println("global vars")
        for (g <- saved.values.collect { case g: CHIR.GlobalVar if !imported(g) => g }) println(g.identifier.stripPrefix("@"))
        println("global funcs")
        for (f <- saved.values.collect { case f: CHIR.Func if f.body.nonEmpty => f }) {
          val marker = if (f.attributes.contains(CHIR.Attribute.Unreachable)) "[unreachable] " else ""
          println(marker + f.identifier.stripPrefix("@"))
        }
        println("types")
        for (d <- saved.typeDefs if !imported(d)) println(d.identifier.stripPrefix("@"))
      }
    } catch {
      case e: PatchGenException => System.err.println(e.getMessage); System.exit(1)
      case e: Exception =>
        if (debug) e.printStackTrace() else System.err.println(e.getMessage)
        System.exit(1)
    }
  }
}
