/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame

import com.huawei.excelsior.jet.compiler.{Stage, Stats}
import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.ir.BytecodeOffset
import com.huawei.excelsior.jet.compiler.jprof.JProfManager
import com.huawei.excelsior.jet.compiler.lambda.LambdaClassNaming
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline.{HotnessAnalysis, InlinePlanner, StaticAnalysis}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation2.InlinePlanChains
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.{CalledMethod, EdgeInfo, Hotspot, MethodInfo, State}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.{StronglyConnectedComponent as SCC, *}
import com.huawei.excelsior.jet.compiler.opt.platforms.PlatformConfig
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.*
import com.huawei.excelsior.jet.compiler.options.StrOption.*
import com.huawei.excelsior.jet.compiler.options.{BoolOption, NumOption}
import com.huawei.excelsior.jet.compiler.symlevel.Method as SymMethod
import com.huawei.excelsior.jet.util.ScalaCollections.{singleElement, sumBy}
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.jprof.JProfData.Section
import com.huawei.excelsior.jet.jprof.JProfFormat.{EntryType, ObjType, SectionType}
import com.huawei.excelsior.jet.jprof.{JProfFormat, JProfReader, JProfWriter}
import xscala.io.{Path, TextOutput}
import xscala.text.ModifiedUtf8Encoding

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Using
import scala.util.control.Breaks.*

/**
  * Interface to results of blame profiling for optimizing compiler and project system.
  * Provides oracle vision after performing all necessary global analysis.
  *
  * @author ijorch
  */
private[jprof] object Blame {

  /** @return Java `Collection` of classes from inline plan. */
  def allClasses: collection.Set[JProfManager.ClassNameAndCLID] = {
    profileGraph.methodSet.map(m =>
      getClassNameAndCLID(m, convertLambdaClasses = true)
    )
  }

  private def getClassNameAndCLID(m: Method, convertLambdaClasses: Boolean) = {
    JProfManager.ClassNameAndCLID(
      m.classLoaderSIDOrNull,
      if (m.isLambda && convertLambdaClasses) LambdaClassNaming.getLambdaClassHostName(m.declaringType) else m.declaringType
    )
  }

  /** Perform global inline planning based on profiling data. */
  def planInline(platformConfig: PlatformConfig, stats: Stats): Unit = env.stage(Stage.InlinePlanning) {
    if (env.enabled(PGO)) {
      assert(_inlinePlan == null, "Inline was already planned.")
      if (env.valueOf(NumOption.Worker) != 0 || env.enabled(BoolOption.ReuseInlinePlan)) {
        val plan = env.pdb.getFile("inline.plan")
        if (plan.exists) {
          deserialize(plan.absolutePath)
          if (env.enabled(BoolOption.DebugInlinePlanSerialization)) {
            serialize(env.pdb.getFile("inline.plan.copy").absolutePath)
          }
        } else {
          env.println("\n Inline Plan doesn't exists")
          initEmpty()
        }
      } else {
        env.print("\n--------------------  Inline Planning Stage  -----------------------------------\n")

        if (profileGraph.isEmpty) {
          // fast-path
          initEmpty()
          return
        }
        genJitGuide()

        val staticAnalysis = new StaticAnalysis(platformConfig, stats)

        if (env.enabled(PGOUseBodySizeApproximation)) {
          profileGraph.methods.foreach(_.approximateBodySize(staticAnalysis))
        }

        val baseGraph = if (env.enabled(VerboseInlinePlanningOnPG)) profileGraph else null // it must only be used for debug output
        var mainPlan = planIteratively(staticAnalysis, baseGraph)
        mainPlan = planForcedEdges(mainPlan)
        mainPlan = planSubgraphLocalHotEdges(mainPlan, staticAnalysis)
        val inlinePlan = deflateInlineListsToEdges(mainPlan)

        profileGraph.methods.foreach(_.restoreJProfInfo())
        env.reportStatus("", "") // clear last reported line

        if (env.enabled(VerboseMarkedRegions)) {
          MarkedRegions.printAsPlainText()
        }

        if (env.enabled(VerboseInlinePlanningGraphs)) {
          mainPlan.printAsDOT("final-plan", baseGraph)
          inlinePlan.printAsDOT("final-plan-deflated", null)
        }

        if (env.enabled(VerboseInlinePlanning)) {
          env.println("\nResulting Inline plan:")
          inlinePlan.printAsPlainText()
          env.println("\nOptimized methods:")
          inlinePlan.pgoHostSet.toArray.sorted foreach { m => env.println("  " + m + " " + inlinePlan.reasoning(m)) }
        }

        if (env.defined(ShowInlinePlanFor)) {
          inlinePlan.printSubgraphAsDOT(env.valueOf(ShowInlinePlanFor) split ',')
        }

        env.print("\n-------------------------------------------------------------------\n")

        if (env.enabled(InteractivePGO)) {
          env.forcePrint("\n----------------  Interactive PGO analysis  -----------------------\n")
          Interactive.analysisLoop()
          env.forcePrint("\n-------------------------------------------------------------------\n")
        }

        if (usePGOChains) {
          env.stage(Stage.ChainsInlinePlanning) {
            env.print("\n--------------------  Chain Inline Planning Stage  -----------------------------------\n")
            val newInlPlan = new InlinePlanChains
            //TODO: to be completed
            newInlPlan.printPlan("FinalPlanChains")
            _inlinePlan = newInlPlan
            env.forcePrint("\n-------------------------------------------------------------------\n")
          }
        } else {
          _inlinePlan = inlinePlan
        }

        if (ProjectLogic.parallelismMayBeEnabled || env.enabled(BoolOption.DebugInlinePlanSerialization)) {
          serialize(env.pdb.getFile("inline.plan").absolutePath)
        }
        if (env.enabled(BoolOption.DebugInlinePlanSerialization)) {
          deserialize(env.pdb.getFile("inline.plan").absolutePath)
        }

        if (env.enabled(TerminateAfterInlinePlanning)) {
          sys.exit(0)
        }
      }
    } else {
      initEmpty()
    }
  }

  /** Iteratively plan inline using info about hits into methods gathered in jprof. */
  private def planIteratively(staticAnalysis: StaticAnalysis, baseGraph: ProfileGraph) = {
    val verbose = env.enabled(VerboseInlinePlanning)
    val verboseOnPG = env.enabled(VerboseInlinePlanningOnPG)
    val verboseIters = env.enabled(VerboseInlinePlanningIterations)
    val printFirstPG = verbose && !verboseOnPG

    val iterations = env.valueOf(PGOIterations)
    val topRootsCount = env.valueOf(PGOIterationTopRootsLimit)

    var mainPlan: InlinePlan = null
    var borderEdges: IterableOnce[Edge] = null


    /* TODO: Correct jprof shouldn't contain invalid bytecode positions. But currently invalid positions can occur.
     *       The edges with invalid positions can lead to multiple paths in the inline plan for the given inline context, which is incorrect.
     *       The current solution is to remove such edges.
     *       JET-12842 is caused by this problem.
     *       The list of the edges to be removed is printed to the output during profile graph reading.
     *       The edges can't be removed before the call graph is built because of the consistency checks for hits into edges and targets.
     *       The hits should be recalculated if some edges were removed.
     */
    var pg: CallGraph = CallGraph(profileGraph.edges filterNot (_.info.callSiteBytecodePos == BytecodeOffset.INVALID))
    if ((profileGraph.edges filter (_.info.callSiteBytecodePos == BytecodeOffset.INVALID)).nonEmpty) {
      pg.methods.foreach(_.recalculateInfo(pg))
    }

    breakable { for (i <- 1 to iterations) {
      val lastIteration = i == iterations

      val planner = new InlinePlanner(pg, staticAnalysis, verbose)
      if (verboseIters || (printFirstPG && i == 1)) planner.printProfileGraph(s"profile-$i", baseGraph)

      val plan = planner.plan()
      if (verboseIters) plan.printAsDOT(s"plan-$i", baseGraph)

      env.stage(Stage.PGOIterationTransition) {
        val (topRoots, otherRoots) = if (lastIteration || topRootsCount == 0 || plan.pgoHostSet.size <= topRootsCount) {
          (plan.pgoHostSet, mutable.LinkedHashSet.empty[Method])
        } else {

          def subgraphHits(m: Method) = m.info.totalHits + plan.callGraph.subgraph(Iterable.single(m)).totalHits

          val sccs = SCC.collect(plan.callGraph)
          val (top, others) = plan.pgoHostSet
            .filter(m => plan.callGraph.isRoot(m) || sccs.exists(_.contains(m)))
            .to(ArrayBuffer).sortInPlaceBy(-subgraphHits(_))
            .iterator.splitAt(topRootsCount)

          (Sets[Method].newQSet(top), Sets[Method].newQSet(others))
        }

        mainPlan = planner.merge(mainPlan, borderEdges, plan.limitTo(topRoots, otherRoots))
        if (verboseIters && !lastIteration) mainPlan.printAsDOT(s"merged-plan-$i", baseGraph)

        if (!lastIteration) {
          val (g, es) = pg.subtractSubgraph(topRoots, otherRoots)
          if (topRoots.isEmpty) {
            assert(otherRoots.isEmpty)
            assert(g.edges sameElements pg.edges)
            break() // plan won't change by further iterations
          }
          pg = g; borderEdges = es
          pg.methods.foreach(_.recalculateInfo(pg))
        }
      }
    }}

    mainPlan.limitTo(mainPlan.pgoHostSet, mutable.LinkedHashSet.empty)
  }

  /** Deflate inline lists of profile edges into seperate edges. */
  private def deflateInlineListsToEdges(mainPlan: InlinePlan): FinalInlinePlan = {
    val deflatedEdgesReasoning = PlanReasoning(mainPlan.reasoning.enabled)

    def deflate(e: Edge) = {
      for (List(c, t) <- e.info.inlineList.entries :+ InlineList.Entry(e.target, BytecodeOffset.INVALID) sliding 2)
        yield {
          val edge = Edge(c.method, e.info.withInlineListEntry(c), t.method)

          import EdgeInfo._
          edge.info.kind =
            if (edge.caller == e.caller && edge.target == e.target) Profile
            else if (edge.target == e.target) Finish
            else if (edge.caller == e.caller) Start
            else Bridge

          if (edge.info.kind == Profile || edge.info.kind == Finish) {
            deflatedEdgesReasoning(edge) ++= mainPlan.reasoning(e)
            edge.info.forced = e.info.forced
          } else {
            deflatedEdgesReasoning(edge) += PlanReasoning.StaticallyInlinedEdge
          }
          edge
        }
    }

    val plan = new FinalInlinePlan(
      MutableCallGraph.sorted(Edge.deduplicate(mainPlan.callGraph.edges flatMap deflate)),
      mainPlan.pgoHostSet,
      mainPlan.totalHits, // actually deflation increased number of hits,
                          // but this value is only used for debug printing and the old one is more suitable for that
      mainPlan.reasoning ++ deflatedEdgesReasoning
    )
    plan.callGraph.methods.foreach(_.recalculateInfo(plan.callGraph))
    plan
  }


  /** Check whether some edges are forced via JCA and add those to plan. */
  private def planForcedEdges(mainPlan: InlinePlan): InlinePlan = {
    if (!env.defined(JCAdvise)) return mainPlan

    val forcedEdges = profileGraph.edgeSet.filter(e => e.info.forced)

    mainPlan.withAdditionalEdges(forcedEdges, PlanReasoning.ForcedRoot, PlanReasoning.ForcedEdge)
  }

  /** Finds edges which are hot within some subgraphs (either by edge hits or by call-site regions). */
  private def planSubgraphLocalHotEdges(mainPlan: InlinePlan, staticAnalysis: StaticAnalysis): InlinePlan = {
    if (!env.enabled(PlanSubgraphLocalHotEdges)) return mainPlan

    val cropBy = mainPlan.truePGOHostSet
    val roots = mainPlan.pgoHostSet union profileGraph.rootSet

    val subgraphs = roots.iterator
      .map(profileGraph.croppedSubgraph(_, cropBy))
      .filter(HotnessAnalysis.hotSubgraph(_, profileGraph))
      .to(mutable.LinkedHashSet)

    val hotByEdges = subgraphs.flatMap { sg =>
      sg.edges
        .filterNot(e => mainPlan.callGraph.contains(e))
        .filter(HotnessAnalysis.hot(_, sg.totalHits))
        .filterNot(InlinePlanner.isTargetPotentialInlineRoot(_, staticAnalysis, mainPlan.reasoning))
    }

    val planWithHotByEdges = mainPlan.withAdditionalEdges(hotByEdges, PlanReasoning.SubgraphLocalHotRoot, PlanReasoning.SubgraphLocalHotEdge)

    val hotByCS = subgraphs.flatMap { sg =>
      sg.edges
        .filterNot(e => planWithHotByEdges.callGraph.contains(e))
        .filter(HotnessAnalysis.hotCallSite(_, Some(sg)))
        .filterNot(InlinePlanner.isTargetPotentialInlineRoot(_, staticAnalysis, planWithHotByEdges.reasoning))
    }

    planWithHotByEdges.withAdditionalEdges(hotByCS, PlanReasoning.SubgraphLocalHotRoot, PlanReasoning.SubgraphLocalHotCS)
  }

  /** @return `Iterator` over all methods that were seen to be called from given `callSitePos`. */
  def calledMethods(callSitePos: Position): Iterator[(SymMethod, Int)] = {
    val il = InlineList(callSitePos)

    profileGraph.edges collect {
      case e if e.info.inlineList.reverse isPrefix il =>
        (e.target.toSymlevel(env), e.info.initialHits)
    } filter (_._1 != null)
  }

  /** @return `true` if given call site of `original` method at `callSitePos` is hot by edge counters. */
  def isHot(callSitePos: Position, original: Option[SymMethod]): Boolean = {
    val plannedEdges = if (_inlinePlan != null) {
      inlinePlan.methods(callSitePos) filter { case (m, _) => original forall (_ sameNameAndSig m) }
    } else {
      Iterator.empty
    }

    plannedEdges.nonEmpty || {
      val o = original map Method.fromSymlevel
      val il = InlineList(callSitePos)
      val profileEdges = profileGraph.edges filter (e =>
        (o forall (_ sameNameAndSig e.target)) && (e.info.inlineList.reverse isPrefix il))

      HotnessAnalysis.aggregatedlyHot(profileEdges, profileGraph.totalHits)
    }
  }

  /** Initialize fields dependent on planning if won't ever plan. */
  def initEmpty(): Unit = {
    assert(_inlinePlan == null, "Inline was already planned.")
    _inlinePlan = new FinalInlinePlan(MutableCallGraph.empty, mutable.LinkedHashSet.empty, 0, PlanReasoning.empty)
  }

  def serialize(fileName: Path): Unit = {
    Using.resource(new JProfWriter(fileName)) { jprofWriter =>

      jprofWriter.printHeader()
      assert(jprofWriter != null)
      jprofWriter.sectionStart(SectionType.BLAME_PROF)
      _inlinePlan.serialize(jprofWriter)

      jprofWriter.sectionEnd()
    }
  }

  def deserialize(fileName: Path): Unit = {
    val section = Using.resource(JProfReader(fileName)) { jprf =>
      singleElement(jprf.parse().getSectionsByType(SectionType.BLAME_PROF))
    }
    _inlinePlan = FinalInlinePlan.deserialize(section)
  }

  /** @return the set of optimized classes (i.e. roots from inline plan and other hot methods). */
  def optimizedClasses: collection.Set[JProfManager.ClassNameAndCLID] = {
    optimizedMethods.map(m => JProfManager.ClassNameAndCLID(m.classLoaderSIDOrNull, m.declaringType))
  }

  /** @return global plan of inlining. */
  def inlinePlan: InlinePlanBase = {
    assert(_inlinePlan != null, "Inline was not planned yet.")
    _inlinePlan
  }

  /** Such methods should be optimized (i.e. roots from inline plan and other hot methods). */
  def requiresBetterOptimizationOf(method: SymMethod): Boolean =
    optimizedMethods contains Method.fromSymlevel(method)

  def usePGOChains = env.enabled(PGOChains) && profileGraph.forest != ProfileForest.empty

  private var _inlinePlan: InlinePlanBase = _
  private var _warmSpectrum: collection.Set[Method] = _
  private def optimizedMethods: collection.Set[Method] = inlinePlan.pgoHostSet

  private var _profileGraph: ProfileGraph = _

  def profileGraph: ProfileGraph = {
    if (_profileGraph == null) env.stage(Stage.ProfileGraphBuilding) {
      val profileGraph = JProf.handleMultipleJProfs {
        buildProfileGraph(JProfManager.main, accumulateHits = false)
      } { (g, jprof) =>
        g + buildProfileGraph(jprof, accumulateHits = true)
      }

      def checkHits(m: Method, methodHits: MethodInfo => Int, edgeHits: EdgeInfo => Int) = {
        sumBy(profileGraph.inEdges(m))(e => edgeHits(e.info)) == methodHits(m.profileInfo)
      }
      def checkInitialHits(m: Method) = checkHits(m, _.initialHits, _.initialHits)
      def checkFollowupHits(m: Method) = checkHits(m, _.followupHits, _.followupHits)
      def validHits(m: Method) = !m.aotAvailable || (checkInitialHits(m) && checkFollowupHits(m))

      assert(
        profileGraph.methods forall validHits,
        {
          val (followupFails, initialFails) =
            profileGraph.methods filterNot validHits partition checkInitialHits
          followupFails.mkString("Followup fails:", "\n\t", "\n") + initialFails.mkString("Initial fails:", "\n\t", "\n")
        }
      )

      _profileGraph = profileGraph
    }
    _profileGraph
  }

  /** Parses profiling data into the `ProfileGraph`. */
  private def buildProfileGraph(jprof: JProfManager, accumulateHits: Boolean): ProfileGraph = {
    val pg = JProf.parseBlameSection(jprof, ProfileGraph.empty, (s, _) =>
      if (s.entries.exists(_.tpe == EntryType.BLAME_HOTSPOT)) {
        ProfileGraph(JProf.hotspots(s, accumulateHits))
      } else {
        ProfileGraph(JProf.parseProfileForest(s))
      }
    )

    if (env.enabled(VerboseProfileGraph)) {
      CallGraphPrinter.printDot(pg, null, pg.rootSet,
        pg.totalHits, PlanReasoning.empty, _ => false, s"raw-$jprof")
      ForestPrinter.printTrees(pg.forest, "Forest")
    }

    pg
  }

  private def genJitGuide(): Unit = {
    // TODO: consider changing .jitguide file reader in runtime to expect UTF-8 instead
    Using(TextOutput.fromFile(env.valueOf(OutputName) + ".jitguide", encoding = ModifiedUtf8Encoding)) { out =>
      for (m <- profileGraph.methods if !m.aotAvailable) {
        out.println(m.declaringType + "." + m.name + m.sig)
      }
    }
  }

  /** There are consistency checks while parsing jprof files and some information is cached into
    * 'Method.internalTable'. If we need to read a standalone jprof file, for example when inline plan is deserialized,
    * we should do it in a clean scope as if it is first to be read. After reading it we need to restore the environment.
    */
  private[blame] def isolatedJProfRead[T](section: Section)(action: Iterable[Hotspot] => T): T = {
    _profileGraph = null
    Method.dropCache()
    val res = action(JProf.hotspots(section, accumulateHits = false))
    Method.dropCache()
    profileGraph
    res
  }
}
