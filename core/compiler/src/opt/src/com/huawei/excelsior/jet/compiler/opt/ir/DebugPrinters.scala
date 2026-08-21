/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.options.BoolOption.{ContinueCompilationAfterIncorrectGlobalOrder, DebugIrLogsAlwaysWithPositions}
import com.huawei.excelsior.jet.compiler.options.StrOption.IrLogsDir
import com.huawei.excelsior.jet.compiler.options.{BoolOption, NumOption}
import com.huawei.excelsior.jet.compiler.util.Names
import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.ordering.TopSort
import xscala.io.{ByteBuffer, Files, Path, TextOutput, stdout}

import java.io.FileNotFoundException
import java.nio.file.Paths
import scala.collection.mutable
import scala.util.Using
import scala.util.control.NonFatal

/**
 * Debug information printer.
 *
 * TODO: refactor, see scala.reflect.api.TreePrinters.
 *
 * @author paul
 * @author conwor
 * @author cypok
 */

enum LogsKind:
  case IRBuild, IRDeser, CodeGen

object DebugPrinters {

  private var _irLogsDir: Path = _

  def irLogsDir(env: Environment) = {
    if (_irLogsDir == null) {
      _irLogsDir = env.getDebugIrLogsDir
      if (env.valueOf(NumOption.Worker) == 0) {
        Files.cleanDir(_irLogsDir)
      }
    }
    _irLogsDir
  }

  val outputProjections = true
}

trait DebugPrinters { self: Universe with DebugPrinters =>

  /** Debug graph info provider. */
  final class DGIProvider private(private[DebugPrinters] val labelsAndColors: Block => (Seq[String], Seq[String])) {
    def and(that: DGIProvider): DGIProvider = {
      new DGIProvider({ b =>
        val (l1, c1) = this.labelsAndColors(b)
        val (l2, c2) = that.labelsAndColors(b)
        (l1 ++ l2, c1 ++ c2)
      })
    }
  }

  object DGIProvider {
    private val emptyPair = (Seq.empty, Seq.empty)

    private def wrapStr(s: String) = if (s == null || s.isEmpty) Seq.empty else Seq(s)

    def apply(action: Block => DGI): DGIProvider = {
      new DGIProvider({ b =>
        action(b) match {
          case null | DGI(null, null) => emptyPair
          case DGI(msg, color) => (wrapStr(msg), wrapStr(color))
        }
      })
    }

    val empty = new DGIProvider(_ => emptyPair)
  }

  /** Debug graph info item. */
  case class DGI(label: String = null, color: String = null)


  /**
   * Abstract debug printer.
   */
  abstract class DebugPrinter {
    protected final def processIncorrectOrder(): Unit = {
      assert(env.enabled(ContinueCompilationAfterIncorrectGlobalOrder),
        "Incorrect global order of nodes (details may be found in the last IR log). For debug purposes you may try " +
          "to continue compilation using option +ContinueCompilationAfterIncorrectGlobalOrder")
    }

    protected def debug[N <: Node](nodes: => IterableOnce[N], message: String, info: N => String, asCFG: Boolean): Unit

    /** Print CFG to IRLog. */
    def debugCFG(message: String, info: Block => String = { _ => null }): Unit = {
      val reachable = cfg.topSort.order
      val unreachable = (all[Block].toSet -- reachable).toSeq.sortBy(_.id)
      debug(reachable ++ unreachable, message, info, asCFG = true)
    }

    private def debugNodesOfScope(message: String, info: Node => String = { _ => null }, scope: Scope): Unit = {
      var orderIsIncorrect = false
      debug({
        val (order, isIncorrect) = LinearNodeOrder.globalOrder(scope)
        orderIsIncorrect = isIncorrect
        order
      }, message, info, asCFG = false)
      if (orderIsIncorrect) processIncorrectOrder()
    }

    /** Print all nodes to IRLog. */
    def debugNodes(message: String, info: Node => String = { _ => null }): Unit = {
      debugNodesOfScope(message, info, currentScope)
    }

    def debugOuterNodes(message: String, info: Node => String = { _ => null }): Unit = {
      debugNodesOfScope(message, info, currentScope.outer)
    }

    /** Print IR graphs to DOT files. */
    def debugGraphs(message: String, printNodesGraph: Boolean = true, printCFG: Boolean = true, info: DGIProvider = DGIProvider.empty): Unit

    def debugDFA(infos: (String, Block => IterableOnce[Node])*): Unit

    def close(): Unit = {}
  }

  abstract class TextDebugPrinter extends DebugPrinter {
    protected def openOut(message: String, extension: String = "log"): TextOutput
    protected def closeOut(out: TextOutput): Unit

    protected def debug[N <: Node](nodes: => IterableOnce[N], message: String, info: N => String, asCFG: Boolean): Unit = {
      val out = openOut(message)
      out.print("============= " + message + "\n\n")
      out.print(nodes.iterator map { n =>
        val rawInfo = info(n)
        val extra =
          (if (rawInfo != null) rawInfo else "") +
          (if (env.enabled(DebugIrLogsAlwaysWithPositions)) "(" + n.pos.toString + ")" else "")
        if (asCFG) {
          toStrCFG(n.asInstanceOf[Block], extra)
        } else {
          toStrNodes(n, extra)
        }
      } filter { _.nonEmpty } mkString("\n"))
      out.println()
      closeOut(out)
    }

    def debugDFA(infos: (String, Block => IterableOnce[Node])*): Unit = {
      val out = openOut("DFA")
      for ((name, info) <- infos.iterator) {
        out.println("============= debugDFA: " + name)
        for (b <- all[Block]) {
          val ids = sortByNodeID(info(b))
          out.println("  "+b.id+": " + ids.mkString("{", ", ", "}"))
        }
        out.println()
      }
      closeOut(out)
    }

    private def makeDotAttrs(action: ((String, String) => Unit) => Unit): String = {
      val attrs = mutable.LinkedHashMap.empty[String, mutable.Buffer[String]]
      action { (k, v) =>
        val values = attrs.getOrElseUpdate(k, mutable.Buffer.empty[String])
        values.append(v)
      }
      val attrsStr = new StringBuilder
      for ((k, vs) <- attrs) {
        assert(vs.nonEmpty)
        attrsStr.append(" [").append(k).append("=\"").append(vs.mkString(",").replace("\"", "\\\"")).append("\"]")
      }
      attrsStr.toString()
    }

    def debugGraphs(message: String, printNodesGraph: Boolean, printCFG: Boolean, info: DGIProvider): Unit = {
      if (printNodesGraph) {
        val outDot = openOut(message + "_nodes", "gv")
        outDot.println("digraph G {")

        var firstBlock = true

        val (order, isIncorrect) = LinearNodeOrder.globalOrder()
        for (n <- order) {
          n match {
            case b: Block =>
              if (firstBlock) {
                firstBlock = false
              } else {
                // finish previous subgraph
                outDot.println(s"\t}")
                outDot.println()
              }
              outDot.println(s"\tsubgraph cluster_${b.id} {")
              outDot.println(s"\t\tcolor = lightgrey;")
              outDot.println(s"\t\tstyle = filled;")
              outDot.println(s"\t\tnode [style = filled, color = white];")

            case _ =>
          }
          val attrs = makeDotAttrs { add =>
            add("label", s"${n.id}: ${n.name}")

            add("style", "filled")
            add("fillcolor", "white")
            add("gradientangle", "270") // gradients from top to bottom

            n match {
              case b: BBlock if b == entryBlock => add("style", "bold")
              case end: BlockEnd if end.exits.isEmpty => add("style", "bold")
              case _: XBlock | _: XPoint => add("shape", "diamond")
              case _ =>
            }
          }
          outDot.println(s"\t\t${n.id} $attrs;")
        }
        if (!firstBlock) {
          // finish previous subgraph
          outDot.println(s"\t}")
          outDot.println()
        }

        for (n <- allNodes) {
          for (e @ Edge(_, use) <- n.outEdges) {
            val attr = e.sourceLabel match {
              case Tag.CONTROL => "[color = black]" +
                (use match { case _: ControlNode => " [style = bold]"; case _ => "" })
              case Tag.XCONTROL => "[style = dashed]"
              case Tag.MEMORY => "[color = blue]"
              case Tag.VALUE => "[color = red]"
            }
            outDot.println(s"\t${n.id} -> ${use.id} $attr;")
          }
        }

        outDot.println("}")
        closeOut(outDot)

        if (isIncorrect) processIncorrectOrder()
      }
      if (printCFG) {
        val reachable = cfg.topSort.order
        val isReachable = reachable.toSet
        val blocks = reachable ++ (all[Block] filterNot isReachable)

        val outDot = openOut(message + "_cfg", "gv")
        outDot.println("digraph G {")

        for (b <- blocks) {
          val attrs = makeDotAttrs { add =>
            val (extraLabels, colors) = info.labelsAndColors(b)

            add("label", s"${b.id}${if (extraLabels.nonEmpty) extraLabels.mkString("\\n", "\\n", "") else ""}")

            add("style", "filled")
            add("fillcolor", colors.distinct match {
              case Seq() => "white"
              case Seq(single) => single
              case _ => "grey" // simulate color mixing :)
            })
            add("gradientangle", "270") // gradients from top to bottom

            b match {
              case _ if b == entryBlock => add("style", "bold")
              case _: XBlock => add("style", "diamond")
              case _ if b.succBlocks.isEmpty => add("style", "bold")
              case _ =>
            }
          }
          outDot.println(s"\t${b.id} $attrs;")
        }
        for (b <- blocks) {
          for (succ <- b.succBlocks) {
            outDot.println(s"\t${b.id} -> ${succ.id};")
          }
          for (xHandler <- b.handledXPoints map (_.handler)) {
            val attr = "[style = dashed]"
            outDot.println(s"\t${b.id} -> ${xHandler.id} $attr;")
          }
        }

        outDot.println("}")
        closeOut(outDot)
      }
    }
  }

  /**
    * Debug printer for IR logs.
    */
  abstract class BaseIRLogsDebugPrinter(kind: LogsKind) extends TextDebugPrinter {
    private val LongMethodNameLength = 75

    protected val logsDirectory = {
      def trimmedName(name: String, uniqNumber: => Int) = {
        if (name.length > LongMethodNameLength) {
          s"${name.substring(0, LongMethodNameLength)}...#$uniqNumber"
        } else {
          name
        }
      }

      val className = trimmedName(
        // Mangle dots to prevent creation of hidden folder on linux (e.g. ".springboot" classloader ID).
        Names.className(hostingClass).replace('.', '_'),
        hostingClass.getUniqueNumber)

      val name = trimmedName(
        Names.mangle(codeUnit.getName) + Names.mangle(codeUnit.method.getSignature.toJETSignature),
        codeUnit.getUniqueNumberInClass)

      val subDir = kind match {
        case LogsKind.IRBuild => "0_irbuilt"
        case LogsKind.IRDeser => "1_irdeser"
        case LogsKind.CodeGen => "2_codegen"
      }

      val dir = DebugPrinters.irLogsDir(env) / className / name / subDir
      Files.cleanDir(dir)
      dir
    }

    protected def prepareMsg(str: String) = {
      str.
        replaceAll("([a-z])([A-Z])", "$1_$2").replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2"). // convert to underscore
        replaceAll("[-\\s:;,<>]", "_").
        replace('\\', '_').
        replace('/', '_').
        toLowerCase
    }

    private var lastNum = -1
    def nextLogNum() = { lastNum += 1; lastNum }
  }

  /**
   * Debug printer for IR logs.
   */
  class IRLogsDebugPrinter(kind: LogsKind) extends BaseIRLogsDebugPrinter(kind) {
    protected def openOut(message: String, extension: String): TextOutput = {
      val num = nextLogNum()
      try {
        val file = ("%s/ir__%04d__%s.%s" format(logsDirectory, num, prepareMsg(message), extension)).replace(':', '_')
        val path = xscala.io.Path(file)
        Files.makeDir(path)
        TextOutput.fromFile(file)
      } catch {
        case _: FileNotFoundException =>
          TextOutput.fromFile(("%s/ir__%04d.%s" format(logsDirectory, num, extension)).replace(':', '_'))
      }
    }

    protected def closeOut(out: TextOutput): Unit = { out.close() }
  }

  /**
    * Debug printer for IR logs into zip archive.
    */
  class ZipIRLogsDebugPrinter(kind: LogsKind) extends BaseIRLogsDebugPrinter(kind) {
    private val zipPath = s"$logsDirectory/logs.zip"
    private val zipFile = Minizip.openWriter(zipPath)

    private val buf = ByteBuffer()
    private var logName: String = _

    protected def openOut(message: String, extension: String): TextOutput = {
      val num = nextLogNum()
      logName = "ir__%04d__%s.%s" format(num, prepareMsg(message), extension)

      buf.reset()
      TextOutput(buf, TextOutput.defaultEncoding)
    }

    protected def closeOut(out: TextOutput): Unit = {
      out.close()
      zipFile.putBytesToArchive(buf.toByteArray, Path(logName))
    }

    override def close(): Unit = {
      zipFile.close()
    }
  }

  /**
   * Debug printer for standard output.
   */
  class StdOutDebugPrinter extends TextDebugPrinter {
    protected def openOut(message: String, extension: String) = stdout
    protected def closeOut(out: TextOutput): Unit = {}
  }

  /**
   * Debug printer that outputs nothing.
   */
  class SilentDebugPrinter extends DebugPrinter {
    override protected def debug[N <: Node](nodes: => IterableOnce[N], message: String, info: N => String, asCFG: Boolean): Unit = { }
    override def debugCFG(message: String, info: Block => String): Unit = { }
    override def debugNodes(message: String, info: Node => String): Unit = { }
    override def debugDFA(infos: (String, Block => IterableOnce[Node])*): Unit = { }
    override def debugGraphs(message: String, printNodesGraph: Boolean, printCFG: Boolean, info: DGIProvider): Unit = { }
  }


  /**
   * Node to text visualization utilities
   */

  private def idStr(n: Node): String = if (n == null) "null" else n.id.toString

  private def idsStr(nodes: IterableOnce[Node]) = nodes.iterator map idStr mkString("(", ",", ")")

  private def toStrCodeGen(n: Node) = {
    // TODO: replace `generated` flag to something else (see comment near this flag)
    if (!n.generated) {
      ""
    } else {
      val additionalInfo = n match {
        case ic: MayHaveImplicitCheck if ic.hasImplicitCheck => s" with implicit check ${ic.implicitCheck}"
        case _ => ""
      }
      if (n.mayHaveResource) {
        s"loc: ${n.resource}$additionalInfo"
      } else {
        s"loc: no resource$additionalInfo"
      }
    }
  }

  private def toStrResource(n: Node) = if (n.allocatedToFrameSlot) "frame slot" else ""

  private def blockToStr(n: Node) = if (n.block == null) "" else "block: " + idStr(n.block)

  private def pointToStr(n: Node) = {
    val point = n match {
      case n: FloatingNode => n.upperPoint
      case n: PinnedNode => n.point
    }
    if (point == null) "" else "point: " + idStr(point)
  }

  private def constraintsInfo(c: Constraints) = {
    "(" + idStr(c.owner) + ", " + (c.inEdges.toArray.tail map { e => idStr(e.source) + "{" + Constraints.shouldBeLiveOn(e) + "}"} mkString("", ",", ")"))
  }

  private def defaultToStrNodes(n: Node): String = {
    s"\t${idStr(n)}:\t${n.name} " +
      (n match {
        case c: Constraints => s"${constraintsInfo(c)}"
        case _ => s"${idsStr(n.args)}"
      }) +
      s" ${blockToStr(n)} ${pointToStr(n)}"
  }

  // TODO: remove implicit output of CodeGen information, use `info` arg of debugNodes methods
  private def defaultToStrNodesWithCodeGenInfo(n: Node): String = defaultToStrNodes(n) + " " + toStrCodeGen(n) + " " + toStrResource(n)

  private def toStrCFG(b: Block, extra: String): String = {
    s"${idStr(b)}: ${b.name}\n\tsuccs: ${idsStr(b.succBlocks)}\n\tpreds: ${idsStr(b.predBlocks)}\n\tpos: ${b.pos}" +
      (if (extra.nonEmpty) ("\n\t" + extra) else "")
  }

  private def toStrNodes(n: Node, extra: String): String = {
    try {
      (n match {
        case b: BBlock =>
          val args = b.args map { x => if (x == null) null else x.block }
          val order = if (CodeOrder contains b) (CodeOrder in b).mkString("order(", ", ", ")") else ""
          s"\n${idStr(b)}:\n\t${b.name} ${idsStr(args)} end:${idStr(b.blockEnd)} $order"

        case b: XBlock =>
          s"\n${idStr(b)}:\n\t${b.name} ${idsStr(b.args)} end:${idStr(b.blockEnd)}"

        case g: Goto =>
          s"${defaultToStrNodes(n)} --> ${idStr(g.target)}"

        case br: If =>
          s"${defaultToStrNodes(n)} --> false:${idStr(br.falseBlock)}, true:${idStr(br.trueBlock)}"

        case sw: AnySwitch[_] =>
          val cases = sw.caseExits map { r => s", ${r.caseValue}:${idStr(r.target)}" }
          s"${defaultToStrNodes(n)} --> default:${idStr(sw.defaultExit.target)}" + cases.mkString

        case ex: Branch.Exit =>
          s"${defaultToStrNodes(n)} --> ${idStr(ex.target)}"

        case tj: TableJump =>
          s"${defaultToStrNodes(n)} --> " + idsStr(tj.exits map (_.target))

        case x: XPoint =>
          s"${defaultToStrNodes(n)} --> " + (if (x.hasHandler) idStr(x.handler) else "(no handler)")

        case _ => defaultToStrNodesWithCodeGenInfo(n)
      }) +
        (if (extra.nonEmpty) (" " + extra) else "")

    } catch {
      case NonFatal(_) => defaultToStrNodes(n)
    }
  }

}
