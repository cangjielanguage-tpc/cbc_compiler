package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.{CHIR, EditableCHIR}
import java.nio.file.{Files, Paths}

/** Invoked by the functional runner; no external tools run inside this checker. */
object GuardChecksCheck {
  def main(args: Array[String]): Unit = {
    require(args.length == 2, "expected input and output CHIR paths")
    val pkg = EditableCHIR.read(Paths.get(args(0)))
    val guarded = pkg.functions.filter(GuardChecks.hasFlag).toVector
    assert(guarded.nonEmpty, "expected plugin guards")
    val sources = guarded.filter(f => f.kind == CHIR.Func.Kind.Default && f.srcCodeIdentifier == "foo")
    assert(sources.nonEmpty, "expected guarded foo")
    for (f <- guarded) {
      GuardChecks.remove(f)
      assert(!GuardChecks.hasFlag(f))
    }
    for (source <- sources) {
      val targets = pkg.functions.filter(f => f.srcCodeIdentifier == PatchableName.guardMethod(source) &&
        f.declaringDef.exists(_.srcCodeIdentifier.endsWith("$PatchImpl$GC"))).toVector
      val target = PatchGen.uniquePatchMethod(targets).get
      source.moveBody(target)
      source.replaceAttributes((source.attributes :+ CHIR.Attribute.Imported).distinct)
      assert(source.body.isEmpty && target.body.nonEmpty)
    }
    val output = Paths.get(args(1))
    pkg.write(output)
    val saved = CHIR.read(output)
    assert(!saved.values.exists { case f: CHIR.Func => GuardChecks.hasFlag(f); case _ => false })

    // Validation failure must leave an existing file intact.
    val invalid = EditableCHIR.empty("invalid")
    val block = invalid.newBlock()
    block.replaceTerminator(invalid.newExit())
    invalid.newBlockGroup(Seq(block), block)
    val sentinel = Files.readAllBytes(output)
    for (bad <- Seq(invalid, EditableCHIR.empty("\ud800"))) {
      var rejected = false
      try bad.write(output)
      catch { case _: IllegalArgumentException => rejected = true }
      assert(rejected, "expected invalid CHIR or an invalid UTF-8 package name to be rejected")
      assert(Files.readAllBytes(output).sameElements(sentinel), "failed serialization replaced the output")
    }
  }
}
