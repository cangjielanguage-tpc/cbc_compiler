package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageImpl
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*

import java.nio.file.{Files, Path, Paths}
import scala.sys.process.*

/** Run from the repository root with chirLib/runMain. */
object MoveFooBodyMain {
  def main(args: Array[String]): Unit = {
    val toolchain = if (args.nonEmpty) Paths.get(args(0)).toAbsolutePath
      else Paths.get(System.getProperty("user.home"), "Projects/software/hotfix-20.10")
    val source = Paths.get("core/chir-lib/examples/foo.cj").toAbsolutePath
    val out = Paths.get("core/chir-lib/target/chir-body-demo").toAbsolutePath
    Files.createDirectories(out)

    println("SOURCE\n" + new String(Files.readAllBytes(source), java.nio.charset.StandardCharsets.UTF_8))
    // Source envsetup in bash: it supplies the toolchain's library search path.
    // Arguments are passed positionally, not interpolated into shell code.
    run(toolchain, out, "cjc", "--emit-chir=raw", "--output-dir", out.toString, source.toString)
    val input = out.resolve("bodymove.chir")
    val output = out.resolve("bodymove-moved.chir")
    val pkg = CHIR.read(input)
    val functions = pkg.values.collect { case f: CHIR.Func => f }.toVector
    val foos = functions.filter(f => f.srcCodeIdentifier == "foo" && f.genericTypeParams.nonEmpty)
    val from: CHIR.Func = foos.find(_.params.size == 1).getOrElse(
      throw new IllegalArgumentException("Missing global foo(x)"))
    val to: CHIR.Func = foos.find(_.params.size == 2).getOrElse(
      throw new IllegalArgumentException("Missing C.foo(this, x)"))
    val raw = CHIRPackageT.deserializeFromBinary(Files.readAllBytes(input))
    println(s"CHIR.Package: implementation=${pkg.getClass.getName}, name=${pkg.name}")
    println(s"tables: types=${raw.getTypes.length}, values=${raw.getValues.length}, " +
      s"expressions=${raw.getExprs.length}, definitions=${raw.getDefs.length}")
    Seq(from, to).foreach { f =>
      println(s"CHIR.Func: id=${f.id}, identifier=${f.identifier}, name=${f.srcCodeIdentifier}")
      println("  parameters=" + f.params.map(p => s"${p.identifier}[${p.srcCodeIdentifier}]:${showType(p.tpe)}").mkString(", "))
      println(s"  returnType=${showType(f.tpe.returnType)}, blocks=${f.body.toSeq.flatMap(_.blocks).size}")
      val table = raw.getValues()((f.id - 1).toInt).asFunction()
      println(s"  parameter IDs=${table.getParams.toSeq.mkString(",")}, " +
        s"body ID=${table.getBody}, return local ID=${table.getRetVal}")
      table.getGenericTypeParams.foreach { id =>
        val t = raw.getTypes()((id - 1).toInt).asGenericType()
        println(s"  generic type ID=$id, identifier=${t.getIdentifier}, " +
          s"bounds=${t.getUpperBounds.toSeq.mkString(",")}")
      }
      f.body.foreach(_.blocks.zipWithIndex.foreach { (b, i) =>
        println(s"  block[$i]: ${b.expressions.map(e => e.getClass.getInterfaces.find(
          _.getName.startsWith("com.huawei.excelsior.jet.compiler.chir.CHIR$")
        ).map(_.getSimpleName).getOrElse(e.getClass.getSimpleName)).mkString(", ")}")
      })
    }

    disassemble(toolchain, out, input, "BEFORE", Seq(from.identifier, to.identifier))
    val heldCast = from.body.get.blocks.flatMap(_.expressions).collectFirst {
      case e: CHIR.CastToConcrete => e
    }.get
    val heldInvoke = from.body.get.blocks.flatMap(_.expressions).collectFirst {
      case e: CHIR.Invoke => e
    }.get
    removeIf(pkg, from)
    require(from.body.get.blocks.size == 3 && to.body.get.blocks.size == 3)
    println("EDIT: global foo has 3 blocks and no Branch; C.foo is still unchanged (in memory)")
    pkg.moveBody(from, to)
    require(from.body.isEmpty && to.body.get.blocks.size == 3)
    require(heldCast.value eq to.params(1), "Held cast must use the destination parameter")
    require(heldInvoke.thisType eq to.genericTypeParams.head,
      "Held Invoke must use the destination generic type")
    println("VERIFIED IN MEMORY: source detached; parameter and generic references updated")
    pkg.write(output)
    println(s"SERIALIZED ONCE: $output")
    val saved = CHIRPackageT.deserializeFromBinary(Files.readAllBytes(output))
    val moved = saved.getValues()((to.id - 1).toInt).asFunction()
    val group = saved.getValues()((moved.getBody - 1).toInt).asBlockGroup()
    require(group.getOwnedFunc == to.id)
    require(moved.getParams()(0) != moved.getParams()(1))
    println(s"MOVE: parameter ${raw.getValues()((from.id - 1).toInt).asFunction().getParams()(0)} " +
      s"-> ${moved.getParams()(1)}; receiver ${moved.getParams()(0)} remains unused")
    val sourceTable = raw.getValues()((from.id - 1).toInt).asFunction()
    println(s"MOVE: generic type ${sourceTable.getGenericTypeParams()(0)} -> ${moved.getGenericTypeParams()(0)}")
    val destinationTable = raw.getValues()((to.id - 1).toInt).asFunction()
    require(moved.getGenericTypeParams()(0) == destinationTable.getGenericTypeParams()(0),
      "The destination generic declaration must stay unchanged")
    val sourceGeneric = saved.getTypes()((sourceTable.getGenericTypeParams()(0) - 1).toInt).asGenericType()
    val targetGeneric = saved.getTypes()((moved.getGenericTypeParams()(0) - 1).toInt).asGenericType()
    require(sourceGeneric.getUpperBounds.sameElements(targetGeneric.getUpperBounds), "ToString bound changed")
    val invokes = group.getBlocks.toSeq.flatMap { id =>
      val block = saved.getValues()((id - 1).toInt).asBlock()
      block.getExprs.toSeq.map(id => saved.getExprs()((id - 1).toInt))
        .filter(_.getType == ExpressionElem.InvokeBase).map(_.asInvokeBase())
    }
    require(invokes.nonEmpty && invokes.forall(_.getBase.getObjType == moved.getGenericTypeParams()(0)),
      "ToString invocation must reference the destination T")
    val reloaded = new PackageImpl(output.toString)
    val finalBody = reloaded.function(to.id.toInt).body.get
    require(finalBody.blocks.size == 3 &&
      !finalBody.blocks.exists(_.expressions.exists(_.isInstanceOf[CHIR.Branch])),
      "The moved body must retain the edited three-block control flow")
    require(reloaded.function(from.id.toInt).body.isEmpty, "Source must become a declaration")
    println("VERIFIED: ToString bound preserved; moved Invoke uses the destination generic type")
    println("VERIFIED: edited body moved to C.foo with 3 blocks and no Branch; global foo is a declaration")
    disassemble(toolchain, out, output, "AFTER", Seq(from.identifier, to.identifier))
    println(s"Saved artifacts: $out")
  }

  private def removeIf(pkg: CHIR.Package, function: CHIR.Func): Unit = {
    require(function.ownerPackage eq pkg, "Function belongs to another package")
    val body = function.body.get
    val before = body.blocks.size
    val block = body.blocks.find(_.expressions.lastOption.exists(_.isInstanceOf[CHIR.Branch]))
      .getOrElse(throw new IllegalArgumentException("Global foo has no if branch"))
    val branch = block.terminator.asInstanceOf[CHIR.Branch]
    block.replaceTerminator(pkg.newGoto(branch.falseBlock, branch.debugLoc))
    require(!body.blocks.exists(_.expressions.exists(_.isInstanceOf[CHIR.Branch])),
      "Global foo must have no Branch after the edit")
    println(s"Edited ${function.identifier}: $before -> ${body.blocks.size} blocks; no Branch remains")
  }

  private def showType(t: CHIR.Type): String = t match {
    case r: CHIR.RefType => showType(r.baseType) + "&"
    case g: CHIR.GenericType => g.identifier
    case c: CHIR.CustomType => c.typeDef.srcCodeIdentifier
    case other => other.toString
  }

  private def disassemble(toolchain: Path, out: Path, file: Path,
                          label: String, names: Seq[String]): Unit = {
    run(toolchain, out, "chir-dis", file.toString)
    // chir-dis writes to its working directory, using the input basename.
    val text = out.resolve(file.getFileName.toString.stripSuffix(".chir") + ".chirtxt")
    val lines = new String(Files.readAllBytes(text), java.nio.charset.StandardCharsets.UTF_8).linesIterator.toVector
    val selected = names.map { name =>
      val start = lines.indexWhere(line =>
        line.contains(s"Func $name(") || line.contains(s"Func $name<"))
      require(start >= 0, s"Disassembly missing $name")
      lines.drop(start).takeWhile(_.nonEmpty).mkString("\n")
    }.mkString("\n\n")
    Files.write(out.resolve(label.toLowerCase + "-foo.chirtxt"), (selected + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8))
    println(s"$label — ONLY foo FUNCTIONS\n$selected")
  }

  private def run(toolchain: Path, cwd: Path, command: String, args: String*): Unit = {
    val invocation = Seq("bash", "-c",
      "source \"$1/envsetup.sh\" && shift && exec \"$@\"", "chir-demo", toolchain.toString, command) ++ args
    val code = Process(invocation, cwd.toFile).!
    require(code == 0, s"$command failed with status $code")
  }
}
