/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.jet.classfile.NameAndSigComparable
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.{blame, env}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline.StaticAnalysis
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.{MethodInfo, unknownBodySize}
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, JETSignatureParser, MethodSignature, Type, Method as SymMethod}
import com.huawei.excelsior.jet.jprof.JProfFormat as JPF
import com.huawei.excelsior.jet.util.ScalaCollections.sumBy
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}

/** Text representation of a method, doesn't depend on sym-level. Deduplicated upon creation.
  * Might contain additional [[info]] obtained from profiler and optionally refined by compiler.
  *
  * @author ijorch
  */
case class Method private (isLambda: Boolean, classLoaderSID: String, declaringType: String,
                  name: String, sig: String, versionedFor: String, aotAvailable: Boolean) {
  def sameNameAndSig(that: Method) = this.name == that.name && this.sig == that.sig

  def classLoaderSIDOrNull = if (classLoaderSID.isEmpty) null else classLoaderSID
  def versioned = versionedFor.nonEmpty

  /** Info about this method that was either obtained from profiler or calculated during inline planning.
    * Note that it might change during planning iterations, as opposed to [[profileInfo]].
    */
  def info = _info

  /** Info about this method that was obtained from profiler. Guaranteed to remain unchanged. */
  def profileInfo = if (jprofInfo == null) info else jprofInfo

  private[representation] def withInfo(info: MethodInfo, accumulateHits: Boolean): Method = {
    require(info ne JProf.unknownMethodInfo)
    if (_info ne JProf.unknownMethodInfo) {
      if (!aotAvailable) {
        // JIT & interpreter can report duplicates due to dynamic class loading, see JET-13258, JET-13510 & JET-16300
        env.reportWarning(s"\nJProf Warning: ignoring additional info about non-AOT-available method $this")
        return Method.newFakeTarget.withInfo(info, accumulateHits = false)
      }

      assert(accumulateHits, s"trying to reassign MethodInfo: $this::${_info} <- $info")
      assert(_info.bodySize == info.bodySize || _info.bodySize == unknownBodySize || info.bodySize == unknownBodySize,
        s"unexpected different bodySize ${_info.bodySize} != ${info.bodySize} for $this")

      _info = MethodInfo(
        bodySize = _info.bodySize min info.bodySize,
        initialHits = _info.initialHits + info.initialHits,
        followupHits = _info.followupHits + info.followupHits
      )
      
    } else {
      _info = info
    }
    this
  }
  private var _info = JProf.unknownMethodInfo

  private var jprofInfo: MethodInfo = _
  def recalculateInfo(graph: CallGraph): Unit = {
    if (jprofInfo == null) jprofInfo = _info

    _info = MethodInfo(
      bodySize = _info.bodySize,
      initialHits = sumBy(graph.inEdges(this))(_.info.initialHits),
      followupHits = sumBy(graph.inEdges(this))(_.info.followupHits)
    )
  }
  def restoreJProfInfo(): Unit = {
    if (jprofInfo != null) _info = jprofInfo
  }
  def approximateBodySize(sa: StaticAnalysis): Unit = {
    if (_info.bodySize == 0) {
      if (jprofInfo == null) jprofInfo = _info

      _info = MethodInfo(
        bodySize = sa(this).bodySizeApproximation ensuring (_ != 0),
        initialHits = _info.initialHits,
        followupHits = _info.followupHits
      )
    }
  }

  private var _execKind: JPF.ExecutionKind = _
  def execKind = _execKind

  private var _callType: JPF.MethodCallType = _
  def callType = _callType

  override def toString: String = {
    val clid = if (classLoaderSID.nonEmpty) classLoaderSID + "/" else ""
    val vers = if (versioned) "-versioned-for-" + versionedFor else ""
    s"$clid$declaringType.$name$vers$sig"
  }

  def ownerSymlevel(env: Environment): ClassType = env.getTypeProvider.getClassTypeByNameAndClassLoaderSID(declaringType, classLoaderSIDOrNull) match {
    case t: ClassType if t.isDeferred => null
    case t => t
  }

  /** Find symlevel object corresponding to `this` method.
    * @return `null` if method hasn't passed sanity checks, and `symlevel.Method` otherwise. */
  def toSymlevel(env: Environment, absenceIsFatal: Boolean = false): SymMethod = {
    val t = ownerSymlevel(env)
    // TODO: properly handle versioned methods
    val m = if (this.sig == JPF.METHOD_SIG_UNKNOWN || t == null) null else {
      // Ignore malformed signatures and try to find method only by name (without signature).
      // TODO: report PGO failure instead of trying to lookup methods with malformed signatures.
      val sig = try {
        JETSignatureParser.parse(this.sig).asInstanceOf[MethodSignature]
      } catch {
        case _: IllegalArgumentException => null
        case _: JETSignatureParser.Error => null
      }
      t.findDeclaredMethodOrNull(XString(name), sig)
    }

    if ((m != null) && !m.isAbstract) {
      m
    } else {
      if (declaringType != JPF.CLASS_UNKNOWN && name != JPF.METHOD_NAME_UNKNOWN) {
        env.reportPGOFailure(this.toString, absenceIsFatal)
      }
      null
    }
  }
}

/** Helpers to interact with [[SymMethod]] and implicits. */
private[blame] object Method {

  def isAlwaysInlinedRTProc(method: SymMethod) = {
    // AJ RT allocators cannot be inlined directly but we want them to look like "always inlineable",
    // opt may use this information to inline suitable allocator.
    // See PreLowering.inlineNews().isHotByProfile().
    method != null && method.isAJRTAllocator
  }

  def isNeverInlined(method: SymMethod) = {
    method == null || (method.isNeverInline && !isAlwaysInlinedRTProc(method))
  }

  def fromSymlevel(sm: SymMethod): Method = {
    val sig = sm.getSignature.toJETSignature
    Method(sm.getDeclaringClass.isAnonymous, sm.getDeclaringClass.getClassLoaderSID, sm.getDeclaringClass.getName,
      sm.getName, sig, "", JPF.ExecutionKind.UNKNOWN, JPF.MethodCallType.UNKNOWN, aotAvailable = true)
  }

  implicit object SetsAndMaps extends Sets.Default[Method] with Maps.Default[Method]

  implicit val ord: Ordering[Method] = Ordering by { m =>
    (m.isLambda, m.classLoaderSID, m.declaringType, m.name, m.sig, m.versionedFor, m.aotAvailable)
  }

  /** Override case class' default `apply` to deduplicate Method instances. */
  def apply(isLambda: Boolean, classLoaderSID: String, declaringType: String,
            name: String, sig: String, versionedFor: String,
            execKind: JPF.ExecutionKind, callType: JPF.MethodCallType, aotAvailable: Boolean) = {
    val clid = if (classLoaderSID == null) "" else classLoaderSID

    val m = new Method(isLambda, clid, declaringType, name, sig, versionedFor,
      aotAvailable
        || (execKind == JPF.ExecutionKind.AOT_COMPILED)
        || (execKind == JPF.ExecutionKind.JIT_COMPILED && callType == JPF.MethodCallType.CANGJIE)
        || (execKind == JPF.ExecutionKind.INTERPRETED && callType == JPF.MethodCallType.CANGJIE)
    )
    m._execKind = execKind
    m._callType = callType
    internTable.getOrElseUpdate(m, m)
  }
  private val internTable = Maps[Method].newMMap[Method]
  def dropCache(): Unit = {
    internTable.clear()
  }

  val fakeCaller = Method(isLambda = false, null, JPF.CLASS_UNKNOWN, "FAKE_CALLER", JPF.METHOD_SIG_UNKNOWN, "",
    JPF.ExecutionKind.UNKNOWN, JPF.MethodCallType.UNKNOWN, aotAvailable = false)
  
  private var fakeMethodCounter = 0
  private def newFakeTarget = {
    fakeMethodCounter += 1
    Method(isLambda = false, null, JPF.CLASS_UNKNOWN, JPF.METHOD_NAME_UNKNOWN + fakeMethodCounter, JPF.METHOD_SIG_UNKNOWN, "",
      JPF.ExecutionKind.UNKNOWN, JPF.MethodCallType.UNKNOWN, aotAvailable = false)
  }
}
