/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame

import com.huawei.excelsior.jet.classfile.NameAndSigComparable
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.Interactive.Command.commands
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.*
import com.huawei.excelsior.jet.compiler.options.StrOption
import com.huawei.excelsior.jet.compiler.symlevel.{Method => SymMethod}
import com.huawei.excelsior.jet.util.ScalaCollections.mapWith
import xscala.io.{Path, stdin, stdout}
import xscala.util.simpleClassName

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/** gdb-like interactive interface for analysing inline planning results and .jprof files.
  *
  * TODO: add plan/profile editing capabilities?
  *
  * @author ijorch
  */
object Interactive {
  def analysisLoop(): Unit = {
    vars.put("$plan", Blame.inlinePlan)
    vars.put("$pg", Blame.profileGraph)

    var stop = false
    while (!stop) {
      print("> ")
      try stdin.getLine() match {
        case Quit() =>
          stop = true

        case ShowVarTypes() =>
          for ((name, v) <- vars) {
            println(s"\t$name -> ${shortDesc(v)}")
          }

        case ShowVar(v) =>
          println(s"\t$v -> ${vars(v)}")

        case MakeSet(names) =>
          val setName = newVarName("s")
          val set = names.map(vars).toSet
          println(s"\t$setName = ${set map shortDesc}")
          vars.put(setName, set)

        case Print(PlanVar(plan)) =>
          plan.print(stdout)

        case Print(PlanVar(plan), MethodVar(method)) =>
          plan.print(stdout, Seq(method))

        case Print(PlanVar(plan), SetVar(roots)) =>
          plan.print(stdout, roots.toSeq)

        // may take arbitrary big amount of time
        case Print(GraphVar(g), MethodVar(method)) =>
          CallGraphPrinter.printText(stdout, g, Seq(method))()()

        // may take arbitrary big amount of time
        case Print(GraphVar(g), SetVar(roots)) =>
          CallGraphPrinter.printText(stdout, g, roots.toSeq)()()

        case FindEdge(graph, caller, target) =>
          vars ++= graph.edges collect {
            case e @ Edge(c, _, t) if c.toString.contains(caller) && t.toString.contains(target) =>
              val varName = newVarName("e")
              println(s"\t$varName = $e")
              varName -> e
          }

        case FindMethod(graph, method) =>
          vars ++= graph.methods collect {
            case m if m.toString.contains(method) =>
              val varName = newVarName("m")
              println(s"\t$varName = $m")
              varName -> m
          }

        case Subgraph(g, ms) =>
          val sgName = newVarName("sg")
          val sg = g.subgraph(ms)
          println(s"\t$sgName = ${shortDesc(sg)}")
          vars.put(sgName, sg)

        case CroppedSubgraph(g, ms, p) =>
          val sgName = newVarName("sg")
          val sg = g.croppedSubgraph(ms, p.pgoHostSet)
          println(s"\t$sgName = ${shortDesc(sg)}")
          vars.put(sgName, sg)

        case TotalHits(g) =>
          println(s"\t${g.totalHits}")

        case EdgeShares(g) =>
          def percentage(hits: Int) = hits.toDouble / g.totalHits * 100
          for (e <- g.edges.toSeq.sortBy(-_.info.totalHits))  {
            println(f"\t${e.info.inlineList} -> ${e.target} | ${e.info.totalHits}% 5d | ${percentage(e.info.totalHits)}% 2.2f%%")
          }

        case MethodInfo(m) =>
          println(s"\tcurrent: ${m.info}\n\tprofile: ${m.profileInfo}")

        case MethodReason(p, m) =>
          print(s"\tReasons for method ${m.toString}:\n\t\t")
          println(p.reasoning(m))
          for (e <- p.callGraph.inEdges(m) ++ p.callGraph.outEdges(m)) {
            print(s"\tReasons for edge ${e.target} called from ${e.info.inlineList.reverse}:\n\t\t")
            println(p.reasoning(e))
          }

        case EdgeCallSite(e) =>
          val csName = newVarName("cs")
          val cs = e.info.inlineList
          println(s"\t$csName = $cs")
          vars.put(csName, cs)

        case Reverse(rcs) =>
          val csName = newVarName("cs")
          val cs = rcs.reverse
          println(s"\t$csName = $cs")
          vars.put(csName, cs)

        case LoadMReg() => parseMarkedRegionSizes(env.valueOf(StrOption.OutputName) + ".mreg")
        case LoadMReg(name) => parseMarkedRegionSizes(name)

        case RegionalHotness(g, cs) => regHotness(g, Some(cs))
        case RegionalHotnessDigest(g) => regHotness(g, None)

        case ProfileMethodMatch(g) =>
          def stringifyJProf(m: Method) = m.name + m.sig
          def stringifySym(m: SymMethod) = m.getName + m.getSignature.toJETSignature

          val allMethods = mapWith(g.methods)(_.toSymlevel(env))
          val (present, missing) = allMethods partition { case (_, m) => m != null }
          val inconsistent = present filterNot { case (jprof, sym) => jprof.sig == sym.getSignature.toJETSignature }
          println(s"Method count: ${allMethods.size}, missing: ${missing.size}, inconsistent: ${inconsistent.size}")
          for ((m, _) <- missing) {
            print(s"\t$m")
            m.ownerSymlevel(env) match {
              case null => println(s" (no owner `${m.declaringType}` found)")
              case owner =>
                val variants = closest(stringifyJProf, stringifySym)(m, owner.getDeclaredMethods.toSeq)
                println(s" |> variants (${variants.length} present, ${variants.length min 3} shown):")
                for (variant <- variants.take(3)) {
                  println(s"\t    * $variant")
                }
            }
          }
          if (missing.nonEmpty && inconsistent.nonEmpty) {
            println()
          }
          for ((jprof, sym) <- inconsistent) {
            println(s"\t$jprof resolved to $sym")
          }

        case Help() =>
          commands.sortBy(_.name).foreach(_.display(Command.longestNameLen))
          println("All command names are case insensitive, but their arguments (variable names and strings) are not.")

        case Help(command) =>
          commands.find(_.names contains command.toLowerCase) match {
            case Some(c) => c.display(c.name.length)
            case None => println(s"Help for unknown command `$command` requested, try `help`.")
          }

        case s =>
          s.split("\\s+").headOption flatMap { cmd => commands.find(_.names contains cmd)} match {
            case Some(c) =>
              println(s"Invalid arguments, try `help` or `quit`.")
              c.display(c.name.length)
            case None =>
              println(s"Unrecognised command `$s`, try `help` or `quit`.")
          }

      } catch {
        case NonFatal(ex) =>
          println(s"Exception $ex occurred, make sure you entered valid commands")
          stdout.printStackTrace(ex)
      }
    }
  }

  val vars = mutable.SeqMap.empty[String, Any]

  var lastVarIdx = mutable.Map.empty[String, Int]
  def newVarName(prefix: String): String = {
    lastVarIdx.updateWith(prefix){
      case Some(idx) => Some(idx + 1)
      case None => Some(0)
    }
    "$" + prefix + lastVarIdx(prefix)
  }

  object GraphVar {
    def unapply(v: String): Option[CallGraph] = vars.get(v) collectFirst {
      case g: CallGraph => g
      case p: InlinePlan => p.callGraph
    }
  }
  object SetVar {
    def unapply(v: String): Option[Set[Method]] = vars.get(v) collectFirst {
      case s: Set[Method @unchecked] => s
    }
  }
  sealed class SimpleVar[T : ClassTag] {
    def unapply(v: String) = vars.get(v) collectFirst { case x: T => x }
  }
  object MethodVar extends SimpleVar[Method]
  object EdgeVar extends SimpleVar[Edge]
  object PlanVar extends SimpleVar[InlinePlan]
  object CallSiteVar extends SimpleVar[InlineList]

  def shortDesc(v: Any) = s"${simpleClassName(v)}@${v.hashCode}"


  object Command {
    val commands = mutable.Buffer.empty[Command]
    lazy val longestNameLen = commands.iterator.map(_.name.length).max
  }
  sealed trait Command { self: Product =>
    require(!commands.exists(_.names.exists(names)))
    commands += this

    def name = self.productPrefix
    def names = (name :: aliases).map(_.toLowerCase).toSet
    def aliases = List.empty[String]
    def help: String

    def display(spaces: Int): Unit = {
      print(name)
      print(" " * (spaces - name.length))
      print(" : ")
      for (l <- help split "\\n") {
        println(l)
        print(" " * (spaces + 3))
      }
      if (aliases.nonEmpty) {
        print("Aliases: ")
        println(aliases mkString ", ")
      }
      println()
    }
  }
  sealed trait SimpleCommand extends Command { self: Product =>
    def unapply(s: String): Boolean = names.contains(s.toLowerCase)
  }
  sealed trait ArgCommand extends Command { self: Product =>
    protected def unpackArgs[T](s: String)(unpack: PartialFunction[Array[String], T]): Option[T] = {
      val ss = s split "\\s+"
      if (ss.nonEmpty && names.contains(ss.head.toLowerCase)) {
        unpack.lift(ss.tail)
      } else None
    }
  }
  sealed trait MultiArgCommand extends ArgCommand { self: Product =>
    def unapplySeq(s: String): Option[Seq[String]] = unpackArgs(s)(x => x.toIndexedSeq)
  }

  case object Quit extends SimpleCommand {
    override def aliases = List("q", "exit", "stop")
    def help = "stop analysis and continue with compilation"
  }

  case object Help extends MultiArgCommand {
    override def aliases = List("h", "?")
    def help = "show this help message"
  }

  case object ShowVarTypes extends SimpleCommand {
    override def aliases = List("vars", "svt")
    def help = "print types of all known variables"
  }

  case object ShowVar extends ArgCommand {
    override def aliases = List("var", "sv")
    def help = "print value of given var"

    def unapply(s: String): Option[String] = unpackArgs(s) { case Array(varName) if vars.contains(varName) => varName }
  }

  case object MakeSet extends ArgCommand {
    override def aliases = List("toSet", "ms")
    def help = "collect given vars into set"

    def unapply(s: String): Option[Seq[String]] = unpackArgs(s) { case x if x forall vars.contains => x.toIndexedSeq }
  }

  case object Print extends MultiArgCommand {
    def help =
      """print graph.
        |If graph is not an inline plan, second argument describing roots is required.
        |Example: print $pg $m0""".stripMargin
  }

  case object FindEdge extends ArgCommand {
    override def aliases = List("edge", "fe", "e")
    def help =
      """find edges in given graph, for which caller & target method names contain given substrings, case-sensitive.
        |All found edges are printed and assigned to new variables.
        |Example: fe $pg Tuple2.equals NumObject""".stripMargin

    def unapply(s: String): Option[(CallGraph, String, String)] = unpackArgs(s) {
      case Array(GraphVar(g), caller, target) => (g, caller, target)
    }
  }

  case object FindMethod extends ArgCommand {
    override def aliases = List("method", "fm", "meth", "m")
    def help =
      """find methods in given graph, for which name contains given substring, case-sensitive.
        |All found methods are printed and assigned to new variables.
        |Example: fm $plan changeValue""".stripMargin

    def unapply(s: String): Option[(CallGraph, String)] = unpackArgs(s) {
      case Array(GraphVar(g), method) => (g, method)
    }
  }

  case object MethodInfo extends ArgCommand {
    override def aliases = List("mi")
    def help = "prints info about given method"

    def unapply(s: String): Option[Method] = unpackArgs(s) { case Array(MethodVar(m)) => m }
  }

  case object MethodReason extends ArgCommand {
    override def aliases = List("mr")
    def help = "prints inline reasoning of given method and adjacent edges in given plan"

    def unapply(s: String): Option[(InlinePlan, Method)] = unpackArgs(s) { case Array(PlanVar(p), MethodVar(m)) => (p, m) }
  }

  case object Subgraph extends ArgCommand {
    override def aliases = List("sg")
    def help = "creates new subgraph of nodes reachable from given methods in given graph"

    def unapply(s: String): Option[(CallGraph, Set[Method])] = unpackArgs(s) {
      case Array(GraphVar(g), SetVar(s)) => (g, s)
    }
  }

  case object CroppedSubgraph extends ArgCommand {
    override def aliases = List("csg")
    def help = "creates new subgraph of nodes reachable from given methods through non-root methods of given graph"

    def unapply(s: String): Option[(CallGraph, Set[Method], InlinePlan)] = unpackArgs(s) {
      case Array(GraphVar(g), SetVar(s), PlanVar(p)) => (g, s, p)
    }
  }

  case object TotalHits extends ArgCommand {
    override def aliases = List("hits", "th")
    def help = "prints number of hits into methods of given graph"

    def unapply(s: String): Option[CallGraph] = unpackArgs(s) { case Array(GraphVar(g)) => g }
  }

  case object EdgeShares extends ArgCommand {
    override def aliases = List("shares", "es")
    def help = "for all edges in given graph prints ratio of their total hits to total hits into graph"

    def unapply(s: String): Option[CallGraph] = unpackArgs(s) { case Array(GraphVar(g)) => g }
  }

  case object EdgeCallSite extends ArgCommand {
    override def aliases = List("ecs", "callSite", "cs")
    def help = "assigns inline list on given edge (i.e. position of its call site) to a new variable"

    def unapply(s: String): Option[Edge] = unpackArgs(s) { case Array(EdgeVar(e)) => e }
  }

  case object Reverse extends ArgCommand {
    def help = "reverse given inline list"

    def unapply(s: String): Option[InlineList] = unpackArgs(s) { case Array(CallSiteVar(cs)) => cs }
  }

  case object LoadMReg extends MultiArgCommand {
    def help = s"load and parse `$${outputname}.mreg` file or the one with given name"
  }
  var markedRegionSizes: Map[MReg.Region, Long] = _
  def parseMarkedRegionSizes(name: String): Unit = {
    val file = Path(name)
    if (!file.isFile) {
      println(s"File $name not found in current working dir ${file.canonicalPath.parent}")
    } else {
      markedRegionSizes = MReg.parseSizes(name)
    }
  }

  case object RegionalHotness extends ArgCommand {
    override def aliases = List("regHotness", "rh")
    def help = "calculates regional hotness of given call-site in given graph"

    def unapply(s: String): Option[(CallGraph, InlineList)] = unpackArgs(s) {
      case Array(GraphVar(g), CallSiteVar(cs)) => (g, cs)
    }
  }

  case object RegionalHotnessDigest extends ArgCommand {
    override def aliases = List("regHotnessDigest", "rhd")
    def help = "shows regional hotness of all call-sites in given graph"

    def unapply(s: String): Option[CallGraph] = unpackArgs(s) { case Array(GraphVar(g)) => g }
  }

  case object ProfileMethodMatch extends ArgCommand {
    override def aliases = List("ProfileMethods", "pm")

    def help =
      """Attempt to resolve all methods in the call graph and print all failures.
        |Example: pm $pg""".stripMargin

    def unapply(s: String): Option[CallGraph] = unpackArgs(s) { case Array(GraphVar(g)) => g }
  }

  def regHotness(g: CallGraph, ocs: Option[InlineList]): Unit = {
    if (markedRegionSizes != null) {
      val graphCallSites = g.edges.map(_.info.inlineList).distinct.toSeq

      val temperature = mapWith(graphCallSites) { il =>
        MarkedRegions.correspondingMarker(il.reverse) match {
          case Some((marker, hits)) =>
            def fqn(m: Method) = s"${m.declaringType}.${m.name}${m.sig}"
            val reg = MReg.Region(fqn(marker.host), marker.regionID)
            val sizeInBytes = markedRegionSizes.get(reg)
            sizeInBytes map (hits.toDouble / _) getOrElse 0D
          case None => 0D
        }
      }
      val graphTemp = temperature.values.sum
      def hotness(t: Double) = t / graphTemp * 100

      ocs match {
        case Some(cs) => // report only given cs
          if (graphCallSites contains cs) {
            println(hotness(temperature(cs)))
          } else {
            println(s"Call-site $cs doesn't belong to given graph")
          }

        case None => // show digest for all call-sites in g
          for ((cs, t) <- temperature.toSeq.sortBy(-_._2)(Ordering.Double.TotalOrdering)) {
            println(f"\t$cs | ${hotness(t)}% 2.2f%%")
          }
      }
    } else {
      println("Need to `LoadMReg` first!")
    }
  }

  // Based on https://github.com/lampepfl/dotty/blob/ed5b11958c02aa09fd591878129f30c33a2bb6a4/compiler/src/dotty/tools/dotc/reporting/messages.scala#L323-L343
  // Ported from Scala 3 to Scala 2
  // Licensed under Apache License 2.0
  // https://github.com/lampepfl/dotty/blob/ed5b11958c02aa09fd591878129f30c33a2bb6a4/LICENSE
  private def distance(s1: String, s2: String): Int = {
    val dist = Array.tabulate(s2.length + 1)(_ => Array.ofDim[Int](s1.length + 1))
    for {
      j <- 0 to s2.length
      i <- 0 to s1.length
    } {
      dist(j)(i) =
        if (j == 0) i
        else if (i == 0) j
        else if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1)
        else (dist(j - 1)(i) min dist(j)(i - 1) min dist(j - 1)(i - 1)) + 1
    }
    dist(s2.length)(s1.length)
  }

  // Produce a list of possible candidates sorted by their Levenstein distances to the missing member
  private def closest[A, B](stringifyA: A => String, stringifyB: B => String)(missing: A, present: Seq[B]): Seq[B] = {
    val missing0 = stringifyA(missing)

    def measure(m: B) = {
      val m0 = stringifyB(m)
      (distance(m0, missing0), m0, m)
    }

    val variants = present map measure filter { case (d, m0, _) => d < missing0.length && d < m0.length }
    variants sortBy { case (d, m0, _) => (d, m0) } map { case (_, _, m) => m } // Sort by distance, then alphabetical
  }
}
