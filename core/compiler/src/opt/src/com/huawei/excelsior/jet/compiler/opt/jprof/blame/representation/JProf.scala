/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.compiler.ir.BytecodeOffset
import com.huawei.excelsior.jet.compiler.jprof.JProfManager
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.MarkedRegions
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.MarkedRegions.Marker
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.options.BoolOption.MultipleJProfs
import com.huawei.excelsior.jet.compiler.options.StrOption.JProfileDir
import com.huawei.excelsior.jet.util.ScalaCollections.{groupBy, singleton, sumBy}
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.jprof.JProfFormat.KeyName.*
import com.huawei.excelsior.jet.jprof.JProfFormat.*
import com.huawei.excelsior.jet.jprof.{JProfData, JProfWriter, JProfFormat as JPF}
import xscala.io.Path

import scala.PartialFunction.cond
import scala.annotation.nowarn
import scala.reflect.ClassTag
import scala.util.Try

/** Low-level parsing and representation of blame profiling data.
  *
  * @author ijorch
  */
object JProf {

  /** In case there are several jprof files provided, use `aggregate` to parse and merge them into single `T`. */
  def handleMultipleJProfs[T](first: T)(aggregate: (T, JProfManager) => T): T = {
    if (env.enabled(MultipleJProfs)) {
      val jprofDir = Path(env.valueOf(JProfileDir))
      assert(jprofDir.exists && jprofDir.isDirectory)

      val (failed, successful) = jprofDir
        .listFiles
        .filter(f => f.name.endsWith(".jprof") && f != JProfManager.main.file)
        .sortBy(_.name)
        .map(f => Try(new JProfManager(f)))
        .partitionMap(_.toEither)

      if (failed.nonEmpty) {
        env.forcePrintln("=== JProf reading errors ===")
        for (err <- failed) {
          env.forcePrintln("----------------------------")
          env.forcePrintln(err)
        }
        env.forcePrintln("============================")
      }

      successful.foldLeft(first) { (acc: T, jprof: JProfManager) =>
        try {
          aggregate(acc, jprof)
        } catch {
          case err: Throwable =>
            // Can't proceed with aggregating other jprofs,
            // as Method cache might have been inconsistently modified prior to the error.
            throw new Error(s"Failed to aggregate $jprof", err)
        }
      }
    } else {
      first
    }
  }

  /** Find the single Blame section of jprof and `parse` it, or evaluate `empty` if there is no Blame section. */
  def parseBlameSection[T](jprof: JProfManager, empty: => T, parse: (JProfData.Section, JProfManager) => T): T = {
    val sections = jprof.getSectionsByType(SectionType.BLAME_PROF)

    if (sections.size > 1) {
      CodeHelpers.notImplemented("Multiple sections of blame profiler")
    } else if (sections.isEmpty) {
      empty
    } else {
      parse(sections.head, jprof)
    }
  }

  /** Raw representation of hotspot profile entries. */
  case class Hotspot(target: Method, callers: Iterable[CallerMethod])
  type CallerMethod = (Method, EdgeInfo)

  case class MethodInfo(bodySize: Int, initialHits: Int, followupHits: Int, isInlineRoot: Boolean = false) {
    require(bodySize >= 0)
    require(initialHits >= 0)
    require(followupHits >= 0)

    def totalHits = initialHits + followupHits
  }
  val unknownBodySize = Int.MaxValue
  val unknownMethodInfo = MethodInfo(unknownBodySize, 0, 0)

  case class EdgeInfo(heuristicHits: Int, initialHits: Int, followupHits: Int, inlineList: InlineList, var forced: Boolean) {
    import EdgeInfo.*
    var kind: Kind = Profile

    require(heuristicHits >= 0)
    require(initialHits >= heuristicHits)
    require(followupHits >= 0)
    if (unknownEdgeInfo != null) {
      // unknownEdgeInfo is already created
      require(inlineList.nonEmpty) // contains at least real (non-inlined) method which received hit
      require(inlineList.reversed) // and that method is first in list
    }

    /** If there is not a single non-heuristic hit, then related edge might be imaginary. */
    def imaginary = heuristicHits == initialHits && followupHits == 0

    def callSiteBytecodePos = inlineList.entries.last.bcPosInMethod
    def totalHits = initialHits + followupHits

    def withInlineListEntry(e: InlineList.Entry) =
      copy(inlineList = new InlineList(e :: Nil, inlineList.reversed))
  }

  object EdgeInfo {

    /**
      * After deflating preinline edges there are different kind of edges.
      * Profile edges - the original edges as they come from the profile a -> b.
      * If m was statically inlined to a and m calls b, a -> m is a start edge,
      * m -> b is a finish edge. If there was a chain of statically inlined methods,
      * the inner edges are bridge edges.
      * When the graph is traversed to find a path for inline or devirtualization,
      * a path can start with either profile or start edges, and a path can finish with
      * either profile or finish edges. 
      *
      * <pre>
      *       Profile
      *        / \
      *  Start     Finish
      *        \ /
      *       Bridge
      * </pre>
      */
    sealed trait Kind {
      @nowarn("msg=match may not be exhaustive") // can't properly analyse the first case
      def merge(that: Kind) = (this, that) match {
        case _ if this == that => this

        case (Profile, _) => Profile
        case (_, Profile) => Profile

        case (Finish, Start) => Profile
        case (Start, Finish) => Profile

        case (Bridge, x) => x
        case (x, Bridge) => x
      }

      def canStart = cond(this) {
        case Profile => true
        case Start => true
      }

      def canFinish = cond(this) {
        case Profile => true
        case Finish => true
      }
    }
    case object Profile extends Kind
    case object Bridge  extends Kind
    case object Start   extends Kind
    case object Finish  extends Kind

    def kindByName(s: String) = s match {
      case "Profile" => Profile
      case "Bridge"  => Bridge
      case "Start"   => Start
      case "Finish"  => Finish
    }
  }

  val unknownEdgeInfo = EdgeInfo(0, 0, 0, InlineList.empty, forced = false)

  /** `start` is the index of most-inlined method and
    * `end-1` is the index of a method inlined directly into an inline root.
    */
  case class InlineContextID(start: Int, end: Int) {
    require(0 <= start && start < end)
  }

  /** Parse hotspots entries from given blame section. */
  def hotspots(section: JProfData.Section, accumulateHits: Boolean): Iterable[Hotspot] = {
    assert(section.tpe == SectionType.BLAME_PROF)

    val icEntries = inlineContexts(section)

    val hotspotEntries = section.entries filter (_.tpe == EntryType.BLAME_HOTSPOT)
    val hotspots = hotspotEntries map { e =>
      val objs = e.objs
      Hotspot(parseTarget(objs.head, accumulateHits), objs.tail map parseCaller(icEntries))
    }

    if (!accumulateHits) {
      assert(hotspots forall (hs => hs.target.profileInfo.initialHits == sumBy(hs.callers)(_._2.initialHits)),
        "PGO Error: sum of callers initial hits differ from number of initial hits into their target")

      assert(hotspots forall (hs => hs.target.profileInfo.followupHits == sumBy(hs.callers)(_._2.followupHits)),
        "PGO Error: sum of callers follow-up hits differ from number of follow-up hits into their target")
    }
    hotspots
  }

  /** Parse marked regions entries from given blame section. */
  def markedRegions(section: JProfData.Section, jprof: JProfManager): Iterable[(Marker, Int)] = {
    assert(section.tpe == SectionType.BLAME_PROF)

    val icEntries = inlineContexts(section)

    val regionEntries = section.entries filter (_.tpe == EntryType.BLAME_MARKED_REGIONS)
    val regions = regionEntries flatMap { e =>
      val objs = e.objs
      assert(objs.head.tpe == JPF.ObjType.BLAME_METHOD)
      val (host, _) = parseMethod(objs.head)
      objs.tail map parseMarker(host, icEntries)
    }

    MarkedRegions.verifyMarkers(regions) // verify only internal invariants of each region

    assert(regions.iterator.map(_._1).toSet.size == regions.toSet.size,
      s"PGO Error: there must be no duplicate markers with different hits counters in $jprof")

    // Note that above we check against `regions.toSet.size` instead of `regions.size` because of the following situation:
    // 1. Compiler might clone some code during optimizations producing several markers with the same bcPos.
    // 2. Those markers might end up in a single marked region (so that they have the same `regionID`).
    // 3. During profiling, a hit into such region will be written for all its markers, including those with the same bcPos.
    //
    // Such duplicates must not be aggregated, as it might lead to breaking the `MarkedRegions.verifyRegions` invariants.
    // Instead, we select only one of these duplicates.
    val (badRegions, goodRegions) = groupBy(regions)(_._1).values partition (b => b.length > 1 && (b exists (_._2 > 0)))
    if (badRegions.nonEmpty) {
      env.reportWarning(s"JProf Warning: there are duplicate markers in $jprof:${badRegions.flatten mkString ("\n\t", "\n\t", "\n")}")
    }
    goodRegions.flatten ++ badRegions.map(_.head ensuring (_._2 > 0))
  }

  def writeMethodAttrs(jprof: JProfWriter, method: Method): Unit = {
    jprof.attrAppendKeyValue(KeyName.RT_ANON, method.isLambda)
    if (method.classLoaderSID.nonEmpty) {
      jprof.attrAppendKeyValue(KeyName.CLASSLOADER_SID, method.classLoaderSID)
    }
    jprof.attrAppendKeyValue(KeyName.CLASS_NAME, method.declaringType)
    jprof.attrAppendKeyValue(KeyName.METHOD, method.name + method.sig)
    if (method.versionedFor.nonEmpty) {
      jprof.attrAppendKeyValue(KeyName.VERSIONED_FOR, method.versionedFor)
    }
    jprof.attrAppendKeyValue(KeyName.EXEC_KIND, method.execKind.value)
    jprof.attrAppendKeyValue(KeyName.CALL_TYPE, method.callType.value)
    jprof.attrAppendKeyValue(KeyName.AOT_AVAILABLE, method.aotAvailable)
  }

  def writeHotspotTarget(jprof: JProfWriter, target: Method, isRoot: Boolean): Unit = {
    jprof.objStart(ObjType.BLAME_TARGET)
    jprof.attrAppendKeyValue(KeyName.METHOD_SIZE, target.info.bodySize)
    writeMethodAttrs(jprof, target)
    jprof.attrAppendKeyValue(KeyName.INITIAL_HITS, target.info.initialHits)
    jprof.attrAppendKeyValue(KeyName.FOLLOWUP_HITS, target.info.followupHits)
    jprof.attrAppendKeyValue(KeyName.INLINE_ROOT, isRoot)
    jprof.objEnd()
  }

  def writeHotspotCaller(jprof: JProfWriter, e: Edge): Unit = {
    jprof.objStart(ObjType.BLAME_CALLER)
    writeMethodAttrs(jprof, e.caller)
    jprof.attrAppendKeyValue(KeyName.INITIAL_HITS, e.info.initialHits)
    jprof.attrAppendKeyValue(KeyName.HEURISTIC_HITS, e.info.heuristicHits)
    jprof.attrAppendKeyValue(KeyName.FOLLOWUP_HITS, e.info.followupHits)
    jprof.attrAppendKeyValue(KeyName.BC, e.info.callSiteBytecodePos)
    jprof.attrAppendKeyValue(KeyName.EDGE_TYPE, e.info.kind.toString)
    jprof.attrAppendKeyValue(KeyName.FORCED_INLINE, e.info.forced)
    jprof.objEnd()
  }

  private def inlineContexts(section: JProfData.Section): Array[InlineList.JProfEntry] = {
    val inlineContexts = singleton(section.entries filter (_.tpe == EntryType.BLAME_INLINE_CONTEXTS))
    inlineContexts map (_.objs.to(Array) map parseInlineContextEntry) getOrElse Array.empty
  }

  private def parseMethod(obj: JProfData.Obj): (Method, AttrsMap) = {
    val attrs = collectAttributes(obj.attributes)

    val nameAndSig = attrs[String](METHOD)
    val Array(name, sigWithoutBracket) = nameAndSig split '('
    val sig = "(" + sigWithoutBracket

    val method = Method(
      attrs.getOrElse(RT_ANON, false),
      attrs.getOrElse(CLASSLOADER_SID, ""),
      attrs[String](CLASS_NAME),
      name,
      sig,
      attrs.getOrElse(VERSIONED_FOR, ""),
      attrs[JPF.ExecutionKind](EXEC_KIND),
      attrs[JPF.MethodCallType](CALL_TYPE),
      attrs.getOrElse(AOT_AVAILABLE, false),
    )

    (method, attrs)
  }

  private def parseInlineContextEntry(obj: JProfData.Obj): InlineList.JProfEntry = {
    assert(obj.tpe == JPF.ObjType.BLAME_METHOD)
    val (m, attrs) = parseMethod(obj)
    InlineList.JProfEntry(m, parseBCPos(attrs))
  }

  private def parseMarker(host: Method, icEntries: Array[InlineList.JProfEntry])(obj: JProfData.Obj): (Marker, Int) = {
    assert(obj.tpe == JPF.ObjType.BLAME_MARKER)
    val attrs = collectAttributes(obj.attributes)

    val inlineList = InlineList(icEntries, parseInlineContextID(attrs), host, parseBCPos(attrs))
    val marker = Marker(host, attrs[Int](REGION_ID), inlineList)

    (marker, attrs.getOrElse(REGION_HITS, 0))
  }

  private def parseTarget(obj: JProfData.Obj, accumulateHits: Boolean): Method = {
    assert(obj.tpe == JPF.ObjType.BLAME_TARGET)

    val (method, attrs) = parseMethod(obj)
    method.withInfo(
      MethodInfo(attrs[Int](METHOD_SIZE), attrs.getOrElse(INITIAL_HITS, 0), attrs.getOrElse(FOLLOWUP_HITS, 0), attrs.getOrElse(INLINE_ROOT, false)),
      accumulateHits
    )
  }

  private def parseCaller(icEntries: Array[InlineList.JProfEntry])(obj: JProfData.Obj): CallerMethod = {
    assert(obj.tpe == JPF.ObjType.BLAME_CALLER)

    val (caller, attrs) = parseMethod(obj)

    val edgeInfo = EdgeInfo(
      attrs.getOrElse(HEURISTIC_HITS, 0),
      attrs.getOrElse(INITIAL_HITS, 0),
      attrs.getOrElse(FOLLOWUP_HITS, 0),
      InlineList.reversed(icEntries, parseInlineContextID(attrs), caller, parseBCPos(attrs)),
      attrs.getOrElse(FORCED_INLINE, false),
    )
    edgeInfo.kind = EdgeInfo.kindByName(attrs.getOrElse(EDGE_TYPE, "Profile"))

    (caller, edgeInfo)
  }

  private[jprof] case class AttrsMap(map: Map[JPF.KeyName, Any]) {
    def apply[T : ClassTag](key: JPF.KeyName) = map.apply(key).asInstanceOf[T]
    def getOrElse[T : ClassTag](key: JPF.KeyName, default: => T) = map.getOrElse(key, default).asInstanceOf[T]

    def get(key: JPF.KeyName) = map.get(key)
    def isEmpty = map.isEmpty
  }

  private def asKeyValue(str: String): (JPF.KeyName, Any) = {
    // Note that value might contain separator, don't use split.
    val sepIndex = str.indexOf(JPF.KEY_VALUE_ASSIGN) ensuring (_ >= 0)
    val rawKey = str.substring(0, sepIndex)
    val rawValue = str.substring(sepIndex + 1, str.length)

    val key = JPF.KeyName.fromString(rawKey) getOrElse {
      env.println(s"PGO Warning: unknown key name `$rawKey`")
      JPF.KeyName.UNKNOWN
    }
    (key, key.deserialize(rawValue))
  }

  private[jprof] def collectAttributes(attrs: String): AttrsMap = {
    AttrsMap((attrs split JPF.ATTRS_DEF_SEPARATOR map asKeyValue).toMap)
  }

  private def parseInlineContextID(attrs: AttrsMap): Option[InlineContextID] = {
    attrs.get(IC) map {
      case IntPair(x, y) => InlineContextID(x, y)
    }
  }

  private def parseBCPos(attrs: AttrsMap) = {
    attrs.getOrElse(BC, BytecodeOffset.INVALID)
  }

  //<editor-fold desc="JProf 3 support">
  def parseProfileForest(section: JProfData.Section): ProfileForest = {
    assert(section.tpe == SectionType.BLAME_PROF)
    val methodMap = codeUnits(section)
    val calledMethods = parseCalledMethods(section, methodMap)
    val mHits = section.entries filter (_.tpe == EntryType.BLAME_METHOD_HITS)
    mHits foreach (x => parseTarget(x.objs.head, methodMap))
    val states = mHits flatMap (_.objs.tail map parseState filter (_.isValid))
    ProfileForest(states, calledMethods)
  }

  case class CalledMethod(method: Method, nodeId: String, callerId: String, callCount: Int, followUpCount: Int, bcInCaller: Int, inlined: Boolean, heuristic: Boolean)

  case class State(scopeId: String, bcInScope: Int, initialHits: Int, followupHits: Int, heuristicHits: Int, markedRegionId: Int) {
    def isValid: Boolean = scopeId.nonEmpty && hasHits
    def hasHits: Boolean = initialHits > 0 || followupHits > 0
    override def toString = s"$productPrefix: ${productElementNames zip productIterator map { case (n, v) => s"$n=$v" } mkString ", "}"
  }

  private def parseCallTreeEntry(methodMap: Map[String, Method])(obj: JProfData.Obj): CalledMethod = {
    assert(obj.tpe == JPF.ObjType.BLAME_CALL_NODE)
    val attrs = collectAttributes(obj.attributes)
    CalledMethod(
      method = methodMap(attrs[String](CUID)),
      nodeId = attrs[String](NODE_ID),
      callerId = attrs.getOrElse(CALLER_ID, ""),
      callCount = attrs.getOrElse(CALL_COUNT, 0),
      followUpCount = attrs.getOrElse(FOLLOWUP_COUNT, 0),
      bcInCaller = attrs.getOrElse(BC_IN_CALLER, BytecodeOffset.INVALID),
      inlined = attrs.getOrElse(INLINED, false),
      heuristic = attrs.getOrElse(HEURISTIC, false)
    )
  }

  private def parseCalledMethods(section: JProfData.Section, methodMap: Map[String, Method]): Seq[CalledMethod] = {
    val callTree = singleton(section.entries filter (_.tpe == EntryType.BLAME_CALL_TREE))
    callTree map (_.objs map parseCallTreeEntry(methodMap)) getOrElse Seq.empty
  }

  private def parseCodeUnitEntry(obj: JProfData.Obj): (String, Method) = {
    assert(obj.tpe == JPF.ObjType.BLAME_CODE_UNIT_DEF)
    val (m, attrs) = parseMethod(obj)
    (attrs[String](CUID), m)
  }

  private def codeUnits(section: JProfData.Section): Map[String, Method] = {
    val codeUnits = singleton(section.entries filter (_.tpe == EntryType.BLAME_CODE_UNIT_IDS))
    val cuids = codeUnits map (_.objs map parseCodeUnitEntry) getOrElse Map.empty
    cuids.toMap
  }

  private def parseTarget(obj: JProfData.Obj, methodMap: Map[String, Method]): Unit = {
    assert(obj.tpe == JPF.ObjType.BLAME_METHOD)
    val attrs = collectAttributes(obj.attributes)
    val method = methodMap(attrs[String](CUID))
    method.withInfo(MethodInfo(attrs[Int](METHOD_SIZE), 0, 0), accumulateHits = true)
  }

  private def parseState(obj: JProfData.Obj): State = {
    assert(obj.tpe == JPF.ObjType.BLAME_STATE)
    val attrs = collectAttributes(obj.attributes)
    State(
      scopeId = attrs.getOrElse(SCOPE_ID, ""),
      bcInScope = attrs.getOrElse(BC_IN_SCOPE, BytecodeOffset.INVALID),
      initialHits = attrs.getOrElse(INITIAL_HITS, 0),
      followupHits = attrs.getOrElse(FOLLOWUP_HITS, 0),
      heuristicHits = attrs.getOrElse(HEURISTIC_HITS, 0),
      markedRegionId = attrs.getOrElse(REGION_ID, -1)
    )
  }
  //</editor-fold>
}

