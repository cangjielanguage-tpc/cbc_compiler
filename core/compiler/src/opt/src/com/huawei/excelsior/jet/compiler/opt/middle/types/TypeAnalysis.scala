/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.jet.compiler.{Domain, StatsKind}
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.compiler.Env.{isStandalone, languagePack}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.serialization.OptExtraInfo
import com.huawei.excelsior.jet.compiler.opt.util.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.compiler.options.BoolOption.{IgnoreNonNullSignatureInfo, OptimizeGetFlatThin, WorkaroundForJET16467}
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.CallTargetInfos
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.CallTargetSearchResults.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.symlevel.{Method, SignatureType, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.Approximation
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.{Closure, ScalaCollections, Worklist}

import scala.annotation.nowarn

trait TypeAnalysis extends OptExtraInfo with CallTargetInfos { self: Universe =>

  private val typeCache = Maps[Node].newMMap[ReferenceApprox]
  private var version = 0

  protected def registerTypeCacheCallbacks(): Unit = {
    onDecommit.addCallback { n =>
      assert(state == NoEval)
      typeCache.remove(n)
    }
  }

  // TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
  @nowarn("msg=The outer reference in this type test cannot be checked at run time")
  private[types] def typeCacheKeyForNode(n: Node): Node = n match {
    case n: EOPOperation => typeCacheKeyForNode(n.obj)
    case n: WriteBarrier => typeCacheKeyForNode(n.value)
    case n: EnrichCBC    => typeCacheKeyForNode(n.obj)
    case _ => n
  }

  private def getCachedType(n: Node): ReferenceApprox = typeCache(typeCacheKeyForNode(n))


  def nodeTypeCacheVersion() = version

  def nodeType(n: Node): ReferenceApprox = {
    val key = typeCacheKeyForNode(n)
    evalType(key)
    typeCache(key)
  }

  def nodeTypeAt(n: Node, point: ControlNode): ReferenceApprox =
    ContextTypesMap.getContextTypeAt(n, point) collect { case x: ReferenceApprox => x } getOrElse nodeType(n)

  def nodeTypeAfter(n: Node, point: UpperPoint): ReferenceApprox =
    ContextTypesMap.getContextTypeAfter(n, point) collect { case x: ReferenceApprox => x } getOrElse nodeType(n)

  def invalidateNodeType(n: Node): Unit = {
    val key = typeCacheKeyForNode(n)
    if (typeCache contains key) deepInvalidate(Some(key))
  }

  def invalidateGlobalDependentNodeTypes(): Unit = {
    deepInvalidate(typeCache.keysIterator filter isGlobalDependent)
  }


  private abstract sealed class EvalState
  private object NoEval extends EvalState
  private object WithinEval extends EvalState

  private var state: EvalState = NoEval
  private val invalidateQueue = Worklist.empty[Node]

  private def isGlobalDependent(node: Node) = node match {
    case AnyDirectCall(_) | AnyVirtualCall() | _: GetMemoryOperation with FieldOperation => true
    case _ => false
  }

  private def incrementVersion(): Unit = {
    assert(version < Int.MaxValue)
    version += 1
  }

  private def deepInvalidate(xs: IterableOnce[Node]): Unit = {
    invalidateQueue ++= xs
    if (state == NoEval && invalidateQueue.nonEmpty) {
      for (n <- invalidateQueue.accumulate) invalidateQueue ++= cachedOutDeps(n)
      for (n <- invalidateQueue.drain) typeCache.remove(n)
      incrementVersion()
    }
  }

  private def inDeps(n: Node): Iterator[Node] = n match {
    case n: ArgDependentTypeNode =>
      n.inEdges collect { case e if n.isTypeDependency(e) => typeCacheKeyForNode(e.source) }
    case _ => Iterator.empty
  }

  private def outDeps(n: Node): Iterator[Node] = {
    for {
      e @ Edge(_, x: ArgDependentTypeNode) <- n.outEdges
      if x.isTypeDependency(e)
      q <- if (typeCacheKeyForNode(x) != x) outDeps(x) else Iterator.single(x)
    } yield q
  }

  private def unknownInDeps(n: Node) = inDeps(n) filterNot typeCache.contains
  private def cachedOutDeps(n: Node) = outDeps(n) filter typeCache.contains

  private def evalType(n: Node): Unit = {
    var iterCnt, iterMax = 0
    while (!(typeCache contains n)) {
      val wl = makeWorklist(n)
      iterCnt += 1
      iterMax += wl.size
      assert(iterCnt <= iterMax, "dead looping in TypeAnalysis.evalType()")

      state = WithinEval // disable invalidate while evaluating types
      try {
        evalList(wl)
      } finally {
        state = NoEval
      }

      deepInvalidate(Iterator.empty)
      // retry evaluation if type(n) was invalidated due to global analysis update
    }
  }

  // postorder traversal of unknownInDeps
  private def makeWorklist(n: Node) = {
    val wl = Worklist.empty[Node]
    Closure.withPostAction(Sets[Node].newMSet, Some(n)) (unknownInDeps) { wl += _ }
    wl
  }

  private def evalList(list: Worklist[Node]): Unit = {
    for (n <- list.drain) {
      if (unknownInDeps(n).isEmpty) {
        assert(!(typeCache contains n))
        typeCache(n) = calculateOneType(n)
      } else {
        // loop detected => do iterative flow analysis starting from `TypeEmpty` up to fixpoint
        val loop = makeWorklist(n)
        list --= loop.iterator
        evalLoop(loop)
        assert(typeCache contains n)
      }
    }
  }

  private def evalLoop(loop: Worklist[Node]): Unit = {
    for (n <- loop.drain) {
      for (d <- unknownInDeps(n)) {
        typeCache(d) = RefEmpty
        loop += d // type(d) must be re-evaluated later
      }
      val t1 = calculateOneType(n)

      typeCache.get(n) match {
        case Some(t0) if t0 == t1 =>

        case Some(t0) =>
          val t2 = if (t1 >= t0) {
            t1
          } else {
            // the following assert guarantees convergence of flow analysis (or enabled workaround)
            assert(env.enabled(WorkaroundForJET16467), s"non-monotonic type flow function found: t0=$t0, t1=$t1")
            t0 union t1
          }
          incrementVersion()
          loop ++= cachedOutDeps(n) // type(n) changed => re-evaluate types of dependent nodes
          typeCache(n) = t2

        case None =>
          typeCache(n) = t1
      }
    }
  }

  // Protected only for unit-tests, should not be used anywhere else directly.
  protected def calculateOneType(n: Node): ReferenceApprox = n match {
    case _: AnyNull => RefNull

    case n: Param =>
      if (isStandalone) {
        formalTypeApproximation(n.formalType)
      } else if (n.isReceiver) {
        OpenCone(ReferenceType(rootReceiverType), mayBeNull = false)
      } else if (rootMethod.isThinConstructor && n.num == 0) {
        assert(n.formalType.isThinClass)
        OpenCone(ReferenceType(asClassType(n.formalType)), mayBeNull = false)
      } else {
        formalTypeApproximation(n.formalType)
      }

    case x: TDBarrier => calculateOneType(x.obj)

    case s: ConstString =>
      OpenCone(ReferenceType(s.strType), mayBeNull = false)

    case s: StrConcat => OpenCone(if (s.isAJ) ReferenceType.ajLangAJString else ReferenceType.javaLangString, mayBeNull = false)
    case _: GetClass | _: AnyClassObject | ComputeAtCompileTime(CompileTimeOp.Kind.GetComponentType) =>
      OpenCone(if (languagePack == LanguagePack.SCALA) ReferenceType.xscalaClass else ReferenceType.javaLangClass, mayBeNull = false)
    case _: Catch =>
      if (isStandalone) {
        OpenCone(ReferenceType.cangjieStdCoreObject, mayBeNull = false)
      } else {
        OpenCone(ReferenceType.ajLangAJObject, mayBeNull = false)
      }

    case n @ (ReinterpretCast(_, _, _) | _: Proxy | _: ReadVar | _: Deferred | _: BitcodeDeferred | _: PublishRef | _: BeginLocalUnmovable) =>
      // we could do better, but it requires motivation
      OpenCone(n.tpe match {
        case ThinType => ReferenceType.ajLangThinType
        case _: EopType => if (isStandalone) ReferenceType.cangjieStdCoreObject else ReferenceType.ajLangAJObject
        case _ => shouldNotReachHere(n.tpe)
      }, mayBeNull = true)

    case _: NoValue => RefEmpty

    case n: NewArrayCopyRT => getCachedType(n.src)

    case _: NewArrayRT => OpenCone(ReferenceType(ReferenceType.javaLangObject, 1), mayBeNull = false)

    case n: AnyNew =>
      if (isStandalone) {
        OpenCone(ReferenceType.cangjieStdCoreObject, mayBeNull = false)
      } else {
        Point(ReferenceType(asClassType(n.allocType)), mayBeNull = false)
      }

    case n: NewArrayFill => Point(ReferenceType(asClassType(n.allocType)), mayBeNull = false)

    case StackAlloc.Local(initType) if initType.isThinClass => ClosedCone.max(ClassType(asClassType(initType)), mayBeNull = false)

    case StackAlloc.DebugVar(t, _) => formalTypeApproximation(t)

    case n: GetMemoryOperation with FieldOperation =>
      if (n.field.isAJFlat) {
        Point(ClassType(asClassType(n.field.getType)), mayBeNull = false)
      } else {
        globallyAnalyzeFieldType(n.field) map (_._2) getOrElse formalTypeApproximation(n.field.getType)
      }

    case n: UniversalGeneric.GetField =>
      formalTypeApproximation(n.instantiatedFieldType)

    case n: FieldSeqOperation =>
      formalTypeApproximation(n.resType)

    case n: GetConstField =>
      formalTypeApproximation(n.field.getType)

    case b: BoxedValue => Point(ReferenceType(b.boxType.symType), mayBeNull = false)

    case AnyDirectCall(target) =>
      getMethodReturnTypeApproximation(target, unionWithFormal = false)

    case call @ AnyVirtualCall() =>
      if (isStandalone) {
        formalTypeApproximation(call.methodType.returnType)
      } else {
        val receiverAppr = getCachedType(call.receiver)
        findTargetMethod(call, call.receiver, receiverAppr, GuardMode.AnyGuards) match {
          case _: NoTarget => RefEmpty
          case OneDirectTarget(t) => getMethodReturnTypeApproximation(t, unionWithFormal = false)
          case OneGuardedTarget(t, _, _, _) => getMethodReturnTypeApproximation(t, unionWithFormal = true)
          case MultipleGuardedTargets(gts, _, _) => getMethodReturnTypeApproximation(gts map (_._1), unionWithFormal = true)
          case UnknownTarget | _: MultipleGuardedTargets => formalTypeApproximation(call.methodType.returnType)
          case ProbableNoTarget => formalTypeApproximation(call.methodType.returnType) withProbableType RefEmpty
        }
      }

    case n: AbstractCall => formalTypeApproximation(n.methodType.returnType)

    case phi: Phi =>
      (phi.inEdges map { e =>
        val cold = phi.controlInput(e).source.block.isCold
        TypeOnEdge(nodeTypeAt(e.source, e.usePoint), cold)
      } reduce (_ union _)).tpe

    case n: ArrayGet if n.enrichedElemType.symType.isCangjieType && !n.enrichedElemType.isDeferred && !n.enrichedElemType.symType.hasDeferredSuper =>
      formalTypeApproximation(n.enrichedElemType)

    case n: ArrayGet =>
      arrayGetTypeApproximation(ReferenceType(asClassType(n.arrayType)), getCachedType(n.array))

    case l: LoadMemory =>
      formalTypeApproximation(l.signature, mayBeNull = true) // TODO: account for nullability

    case gf: GetFlatThin =>
      Point(ClassType(gf.thinType), mayBeNull = false)

    case lea: Lea =>
      // Result of GetField of thin flat lowering (nodeType may be asked from remained ThinNullCheck or any other operation)
      // TODO: think about it
      assert(lea.tpe == ThinType)
      assert(!env.enabled(OptimizeGetFlatThin))
      OpenCone(ReferenceType.ajLangThinType, mayBeNull = true) // TODO: may it be not null by some reason?


    case convertDomain: ConvertDomain =>
      OpenCone(convertDomain.domain match {
        case Domain.AJ => ReferenceType.ajLangAJThrowable
        case Domain.JAVA => ReferenceType.javaLangThrowable
        case Domain.CANGJIE => ReferenceType.ajLangAJObject // TODO Replace with CJ type
        case Domain.SCALA => ReferenceType.xscalaAnyRef
      }, mayBeNull = false) // Caught exception can't be null => converted exception can't be null

    case _: DelayedGet =>
      OpenCone(ReferenceType.ajLangAJObject, mayBeNull = true)

    case Evacuate(obj) => OpenCone(ReferenceType.javaLangObject, mayBeNull = true)

    case SingletonObject(obj) => Point(ReferenceType(asClassType(obj)), mayBeNull = false)

    case _: MutFunc.Host => OpenCone(ReferenceType.ajLangAJObject, mayBeNull = true)

    case _: DerivedPtr.Local | _: DerivedPtr.Global | _: Box | _: Unbox | _: SpawnFuture | _: SpawnClosure |
         _: EnumCast | _: OptionPayloadGeneric | _: NewNoneOptionGeneric | _: NewSomeOptionGeneric | _: NewGeneric =>
      OpenCone(ReferenceType.cangjieStdCoreObject, mayBeNull = true)

    case n: AtomicOps.AtomicNode =>  
      formalTypeApproximation(n.field.fieldType)

    case _ =>
      shouldNotReachHere(n)
  }

  private def mayBeNullType(sigType: SignatureType): Boolean = {
    val mayBeNull = sigType match {
      case sigType: SignatureType.NullableWrapper => true
      case sigType: SignatureType.NonNullableWrapper.Base => true
      case _ => false
    }
    mayBeNull || env.enabled(IgnoreNonNullSignatureInfo)
  }

  private[types] def arrayGetTypeApproximation(arrayType: ReferenceType, arrayTypeAppr: ReferenceApprox): ReferenceApprox = {
    // TODO: rework array types, so that JavaReferenceArrayType has reference arrayElement
    assert(arrayType.isInstanceOf[ArrayType] && arrayType.asInstanceOf[ArrayType].arrayElement.isInstanceOf[ReferenceType])
    val (refinedArrayTypeAppr, strict) = arrayTypeAppr weakIntersect OpenCone(arrayType, mayBeNull = false)
    assert(strict)
    refinedArrayTypeAppr transform {
      case UpperBounded(arrayType: ArrayType, _) => OpenCone(arrayType.arrayElement.asInstanceOf[ReferenceType], mayBeNull = true)
      case RefEmpty | RefNull => RefEmpty
      case t => shouldNotReachHere(t)
    }
  }

  private def getMethodReturnTypeApproximation(target: Method, unionWithFormal: Boolean): ReferenceApprox = {
    getMethodReturnTypeApproximation(Seq(target), unionWithFormal)
  }

  private def getMethodReturnTypeApproximation(targets: Seq[Method], unionWithFormal: Boolean): ReferenceApprox = {
    assert(targets.nonEmpty)
    lazy val formalRetType = {
      val rt = targets.head.getReturnType
      assert(targets forall (_.getReturnType == rt))
      formalTypeApproximation(rt)
    }
    ScalaCollections.sequence(targets map { t => globallyAnalyzeMethod(t) flatMap (_.returnType) }) match {
      case Some(ts) =>
        val t = ts reduce (_ union _)
        if (unionWithFormal) {
          (TypeOnEdge(t, isColdEdge = false) union TypeOnEdge(formalRetType, isColdEdge = true)).tpe
        } else {
          t
        }
      case None => formalRetType
    }
  }

  def formalTypeApproximation(sigType: SignatureType): ReferenceApprox =
    formalTypeApproximation(sigType, mayBeNullType(sigType))

  def formalTypeApproximation(t: SignatureType, mayBeNull: Boolean): ReferenceApprox = {
    lazy val rootType = ReferenceType.typeAnalysisRootBy(t)

    if (isStandalone) {
      // TODO: support proper root type in standalone mode
      if (t.isCangjieArray || t.isInstanceOf[SignatureType.Box] || t.isTypeVariable || t.isInstanceOf[SignatureType.CangjieEnum]) {
        OpenCone(ReferenceType.cangjieStdCoreObject, mayBeNull)
      } else {
        OpenCone(ReferenceType(t), mayBeNull)
      }

    } else if (t.isDeferred || t.hasDeferredSuper || t.symType.isErroneous) {
      // Every (array of) absent type is represented conservatively as (array of) root type.
      val safeType = t match {
        case t: SignatureType.JavaArray => ReferenceType(rootType, t.dimNum)
        case _ => rootType
      }
      OpenCone(safeType, mayBeNull)

    } else if (t.isInterface) {
      // External values with interface formal type may not implement this interface because of bytecode verifier features.
      OpenCone(rootType, mayBeNull)
        .withProbableType(OpenCone(ReferenceType(t), mayBeNull))

    } else {
      OpenCone(ReferenceType(t), mayBeNull)
    }
  }

  /** Type of return value if it is better than formal type of return value. */
  def calcRefinedReturnType(): Option[ReferenceApprox] = {
    val returnType = rootMethod.getReturnType
    if (!returnType.isReference) {
      None
    } else {
      def stat(msg: String): Unit = {
        stats.count(StatsKind.RefinedReturnType, msg, rootMethodPos)
      }

      Return.unique match {
        case None =>
          stat("no-return")
          Some(RefEmpty)

        case Some(ret) =>
          val formal = formalTypeApproximation(returnType)

          val actual = nodeTypeAt(ret.inValue, ret)
          val intersected = (actual weakIntersect formal)._1

          (intersected compareWidened formal) match {
            case CC.Less =>
              stat("refined")
              Some(intersected)

            case CC.Equal | CC.PartiallyEqual =>
              stat("not refined")
              None

            case CC.Greater | CC.Incomparable =>
              shouldNotReachHere((actual, formal, intersected))
          }
      }
    }
  }

  def checkTypeEmptyUses(): Unit = {
    for (n <- all[SpinalNode] if n.tpe.isTraceableRefType && nodeType(n).isEmpty) {
      assert(n.valueUses.isEmpty && n.controlUses.forall(_.isInstanceOf[Halt]))
    }
  }
}
