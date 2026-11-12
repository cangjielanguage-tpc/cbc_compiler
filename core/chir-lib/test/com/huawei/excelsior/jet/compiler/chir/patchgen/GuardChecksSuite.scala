package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.EditableCHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR
import java.nio.file.{Files, Path, Paths}
import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process.*

class GuardChecksSuite extends AnyFunSuite {
  for (name <- Seq("func1", "class1")) {
    test(s"remove plugin guards and move $name bodies without cloning, then save once") {
      val home = sys.env.getOrElse("CANGJIE_HOME", cancel("Set CANGJIE_HOME for native CHIR integration tests"))
      val repo = Paths.get(sys.props("user.dir")).toAbsolutePath
      val data = repo.resolve("test/patch-gen/test_data")
      val work = Files.createTempDirectory("chir-guards-")
      val tool = repo.resolve("test/tool.sh").toString
      val stdx = Paths.get(home, "stdx/dynamic/stdx").toString
      def run(directory: Path, args: String*): Unit = {
        val output = new StringBuilder
        val command = Process(Seq(tool) ++ args, directory.toFile,
          "TOOLCHAIN" -> home, "CANGJIE_STDX_PATH" -> stdx,
          "LD_LIBRARY_PATH" -> (stdx + ":" + sys.env.getOrElse("LD_LIBRARY_PATH", "")))
        val status = command.!(ProcessLogger(s => output.append(s).append('\n'), s => output.append(s).append('\n')))
        assert(status == 0, output.toString)
      }
      try {
        run(work, "cjc", "-p", data.resolve("lib/hotfix").toString,
          "--output-type=staticlib", "-o", "hotfix.a")
        run(work, "cjc", data.resolve(name + "_patched.cj").toString, "hotfix.a",
          "--import-path", work.toString, "--output-type=staticlib",
          "--plugin", Paths.get(home, "tools/lib/libhotfix-plugin.so").toString,
          "-O0", "--emit-chir", "-o", "input.chir")
        val editable = EditableCHIR.read(work.resolve("input.chir"))
        val pkg = editable
        val functions = pkg.values.collect { case f: CHIR.Func => f }.toVector
        val guarded = functions.filter(f => GuardChecks.hasFlag(f))
        assert(guarded.nonEmpty)
        val sources = guarded.filter(f => f.kind == CHIR.Func.Kind.Default && f.srcCodeIdentifier == "foo")
        assert(sources.nonEmpty)
        for (function <- guarded) {
          GuardChecks.remove(editable.edit(function))
          assert(!GuardChecks.hasFlag(function))
        }
        for (source <- sources) {
          val target = functions.find(f => f.srcCodeIdentifier == PatchableName.guardMethod(source) &&
            f.declaringDef.exists(_.srcCodeIdentifier.endsWith("$PatchImpl$GC"))).get
          editable.edit(source).moveBody(target)
          editable.edit(source).replaceAttributes((source.attributes :+ CHIR.Attribute.Imported).distinct)
          assert(source.body.isEmpty && target.body.nonEmpty)
        }
        editable.write(work.resolve("output.chir"))
        run(work, "chir-dis", "output.chir")
        val saved = CHIR.read(work.resolve("output.chir"), 13002)
        assert(saved.values.collect { case f: CHIR.Func if GuardChecks.hasFlag(f) => f }.isEmpty)
      } finally {
        val paths = Files.walk(work)
        try paths.sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.delete(p))
        finally paths.close()
      }
    }
  }
}
