/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.devirtualization

import com.huawei.excelsior.jet.compiler.symlevel.{FindMethodImplResult, Method, MethodReference, SignatureType, MethodReferenceAccessKind as MAK, Type as SymType}
import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.options.NumOption.{PGIHitsCoverageThreshold, PGIMaxTargets}
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo.PGO
import com.huawei.excelsior.jet.compiler.opt.middle.inline.CallSitesHelper
import com.huawei.excelsior.jet.compiler.options.BoolOption.NoTauTests
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.MethodSearchError
import com.huawei.excelsior.jet.compiler.types.CHA
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.collection.mutable

trait CallTargetInfos extends CallSitesHelper { self: Universe =>

  import CallTargetSearchResults._

  private[devirtualization] def pgiMaxTargetsCount = env.valueOf(PGIMaxTargets)
  private[devirtualization] def pgiHitsCoverageThreshold = env.valueOf(PGIHitsCoverageThreshold)

  enum GuardMode {
    case NoGuards, RealGuards, AnyGuards
  }

  def refineReceiverType(rcvTypeAppr: ReferenceApprox, refClass: SignatureType): ReferenceApprox = {
    // Our type analysis may be imperfect: target's reference class may be more accurate than type of receiver.
    // We try to use bytecode's information about reference class intersected with type analysis results.
    (rcvTypeAppr weakIntersect formalTypeApproximation(refClass, mayBeNull = false))._1
  }

  def findTargetMethod(call: Call, guardMode: GuardMode): CallTargetSearchResult = {
    val rcv = call.receiver
    findTargetMethod(call, rcv, nodeTypeAt(rcv, call), guardMode)
  }

  def findTargetMethod(call: Call, rcv: Node, rcvTypeAppr: ReferenceApprox, guardMode: GuardMode): CallTargetSearchResult = {
    val ref = call.targetRef
    val akind = ref.accessKind
    require(akind == MAK.VIRTUAL || akind == MAK.INTERFACE)
    assert(ref.method.getDeclaringClass isAssignableFrom ref.refClass)

    if (!ref.hasVirtualMethodSlot) {
      OneDirectTarget(ref.method)
    } else {
      findTargetMethod0(call, rcv, refineReceiverType(rcvTypeAppr, SignatureType.fromSymType(ref.refClass)), guardMode)
    }
  }

  private def findTargetMethod0(call: Call, rcv: Node, rcvTypeAppr: ReferenceApprox, guardMode: GuardMode): CallTargetSearchResult = {
    val originalRef = call.targetRef
    val original = originalRef.method

    rcvTypeAppr match {
      case RefEmpty => UnreachableCall

      case _ if original.isFinal || original.isPrivate =>
        OneDirectTarget(original)

      case rcvTypeAppr: UpperBounded =>
        findInPoint(rcvTypeAppr.root, originalRef) match {
          case r @ OneDirectTarget(method) if method.isFinal => r

          case rootImpl => rcvTypeAppr match {
            case _: Point | UpperBounded(_: JavaArrayType, _) =>
              rootImpl

            case rcvTypeAppr: ClosedCone =>
              findInClosedClassCone(rcvTypeAppr, originalRef) orElse
                findGuarded(call, rcv, rcvTypeAppr, guardMode, originalRef)

            case rcvTypeAppr: OpenCone =>
              findGuarded(call, rcv, rcvTypeAppr, guardMode, originalRef)
          }
        }

      case _ => shouldNotReachHere()
    }
  }

  private def findGuarded(call: Call, rcv: Node, rcvTypeAppr: Cone, guardMode: GuardMode, originalRef: MethodReference): CallTargetSearchResult = {
    if (env.enabled(NoTauTests) || guardMode == GuardMode.NoGuards || originalRef.method.getDeclaringClass.isThinClass) {
      return UnknownTarget
    }

    // first some static heuristics...
    findProbableMethodTarget(rcvTypeAppr, guardMode, originalRef) orElse
      findRecursiveTarget(rcv, rcvTypeAppr, guardMode, originalRef) orElse
      // then some guided heuristics...
      findProfileMethodTarget(rcvTypeAppr, call.pos, guardMode, originalRef) orElse
      findJCAMethodTarget(rcvTypeAppr, guardMode, originalRef) orElse
      findJCAInlineInContextWithPointTest(rcv, rcvTypeAppr, guardMode, originalRef)
  }

  /** Smart wrapper over raw search of method implementation in scope which handles non-class types. */
  private def findInPoint(rcvType: ReferenceType, originalRef: MethodReference): CallTargetSearchResult = {
    rcvType match {
      case _: JavaArrayType =>
        ReferenceType(originalRef.method.getDeclaringClass) match {
          case declClass: InterfaceType =>
            assert(!JavaArrayType.isSupertype(declClass)) // superinterfaces of array don't declare methods
            ErroneousCall(MethodSearchError.INCOMPATIBLE_CLASS_CHANGE)

          case declClass =>
            val rootType = ReferenceType.typeAnalysisRootBy(rcvType.sigType)
            if (declClass == rootType) {
              OneDirectTarget(originalRef.method)
            } else {
              shouldNotReachHere(declClass)
            }
        }

      case _: InterfaceType =>
        UnknownTarget

      case x: ClassType =>
        if (ReferenceType(originalRef.refClass) >= rcvType) {
          x.findMethodImplementation(originalRef) match {
            case r: FindMethodImplResult.Found => OneDirectTarget(r.result)
            case r: FindMethodImplResult.Error => ErroneousCall(r.result)
          }
        } else {
          // Note: this should always be interface call and we should always throw ICCE here,
          //       however JET changes interface calls of j.l.Object methods to virtual ones during parsing,
          //       so in such cases we shouldn't throw ICCE, because it becomes virtual call
          //       (which is not correct: see JET-7343).
          if (originalRef.isInterfCall) {
            ErroneousCall(MethodSearchError.INCOMPATIBLE_CLASS_CHANGE)
          } else {
            // TODO: remove this case when JET-7343 is fixed
            UnknownTarget
          }
        }

      case _: AJArrayType | _: CangjieArrayType => shouldNotReachHere("unexpected AJ array receiver type")
    }
  }

  private def findInClosedClassCone(rcvClassAppr: ClosedCone, originalRef: MethodReference): CallTargetSearchResult = {
    val root = rcvClassAppr.root
    val levelLimit = rcvClassAppr.maxLevel
    findCHAImplementation(root, levelLimit, originalRef, None) match {
      case Some(CHAResult(foundMethod, _, maxLevel)) if maxLevel == levelLimit =>
        foundMethod match {
          case x: FindMethodImplResult.Found => OneDirectTarget(x.result)
          case x: FindMethodImplResult.Error => ErroneousCall(x.result)
        }
      case _ =>
        UnknownTarget
    }
  }

  def refineTypeSpeculatively(appr: ReferenceApprox): ReferenceApprox = {
    assert(!appr.mayBeNull)
    val probable = appr.probableType
    appr match {
      case UpperBounded(root, _) if chaInlineAllowed(root) =>
        val refined = probable.filterClosed()._1
        refined compare probable match {
          case CC.Less | CC.Equal => refined // typical case
          case CC.PartiallyEqual =>
            // `probable` is likely to be an interface with multiple class inheritors,
            // in practice interface result is more useful.
            // See JET-11009 and corresponding unit-tests.
            probable
          case res @ (CC.Incomparable | CC.Greater) =>
            shouldNotReachHere(s"unexpected refine($probable) = $refined (which is $res)")
        }

      case _ => probable
    }
  }

  private def findProbableMethodTarget(rcvTypeAppr: Cone, guardMode: GuardMode, originalRef: MethodReference): CallTargetSearchResult = {
    val original = originalRef.method
    val probableType = (refineTypeSpeculatively(rcvTypeAppr) weakIntersect refineTypeSpeculatively(cone(originalRef.refClass)))._1 match {
      case `rcvTypeAppr` =>
        // nothing new here
        return UnknownTarget

      case t @ (_: ClosedUpperBounded | OpenCone(_: JavaArrayType, _)) =>
        // we got refined probable type
        t.asInstanceOf[UpperBounded]

      case RefEmpty =>
        return ProbableNoTarget

      case _: OpenCone =>
        // we cannot analyze it
        return UnknownTarget

      case _ => shouldNotReachHere()
    }

    val info = TauInfo.Static

    probableType.root match {
      case _: JavaArrayType =>
        assert(original.getDeclaringClass.isJavaLangObject)
        guardMode match {
          case GuardMode.AnyGuards =>
            OneGuardedTarget(original, MagicGuard, info, rcvTypeAppr)

          case GuardMode.RealGuards =>
            // TODO: array bit test
            // method test may be used here, but we do not want this
            UnknownTarget

          case GuardMode.NoGuards => shouldNotReachHere()
        }

      case pRoot: ClassType =>
        if (!chaInlineAllowed(pRoot)) {
          // We can do only point test, other tests are not allowed.
          probableType match {
            case _: Point =>
              findInPoint(pRoot, originalRef) match {
                case OneDirectTarget(method) =>
                  OneGuardedTarget(method, PointGuard(pRoot.symType), info, rcvTypeAppr)

                case _: ErroneousCall =>
                  ProbableNoTarget

                case _ => shouldNotReachHere()
              }

            case _: ClosedCone =>
              UnknownTarget

            case _ => shouldNotReachHere()
          }

        } else if (!chaInlineAllowed(rcvTypeAppr.root)) {
          UnknownTarget

        } else {
          val TypeClosedClass(_, _, _, pLevelLimit) = probableType
          findCHAImplementation(pRoot, pLevelLimit, originalRef, None) match {
            case Some(CHAResult(fmi: FindMethodImplResult.Found, minLevel, maxLevel)) if maxLevel == pLevelLimit =>
              guardMode match {
                case GuardMode.AnyGuards => OneGuardedTarget(fmi.result, MagicGuard, info, rcvTypeAppr)

                case GuardMode.RealGuards => selectRealGuardForSubCone(rcvTypeAppr, pRoot, pLevelLimit, originalRef, fmi.result, minLevel, maxLevel, info)

                case GuardMode.NoGuards => shouldNotReachHere()
              }

            case Some(CHAResult(_: FindMethodImplResult.Error, _, maxLevel)) if maxLevel == pLevelLimit =>
              ProbableNoTarget

            case _ =>
              UnknownTarget
          }
        }

      case _ => shouldNotReachHere()
    }
  }

  private def findRecursiveTarget(rcv: Node, rcvTypeAppr: Cone, guardMode: GuardMode, originalRef: MethodReference) = {
    rcv match {
      case ReceiverParam() =>
        findInPoint(rcvTypeAppr.root, originalRef) match {
          case OneDirectTarget(method) if method == rootMethod => chooseGuardForTarget(rcvTypeAppr, originalRef, method, guardMode, TauInfo.Static)
          case _ => UnknownTarget
        }
      case _ => UnknownTarget
    }
  }

  private def findProfileMethodTarget(rcvTypeAppr: Cone, callSitePos: Position,
                                      guardMode: GuardMode, originalRef: MethodReference): CallTargetSearchResult = {
    def isCompatible(targetAndHits: (Method, Int)) = targetAndHits match { case (target, _) =>
      assert(!target.isAbstract)
      // these methods can be called virtually in general
      !target.isStatic && !target.isPrivate &&
        // these methods can be called virtually from original
        originalRef.method.overridesNameAndSig(target) && isCompatibleTarget(originalRef, target) &&
        // these methods can be called from receiver
        receiverHasTarget(rcvTypeAppr, originalRef, target)
    }
    val plannedTargetsIter = profile.devirtTargets(callSitePos) filter isCompatible
    if (plannedTargetsIter.isEmpty) {
      // fast path
      return UnknownTarget
    }
    val plannedTargets = plannedTargetsIter.toSeq
    val allKnownTargets = profile.calledMethods(callSitePos) filter isCompatible

    assert(plannedTargets.size < 2 || (plannedTargets sliding 2 forall { case Seq(mh1, mh2) => mh1._2 >= mh2._2 }),
      "profile targets must be sorted by hits")
    val totalHits = ScalaCollections.sumBy(allKnownTargets)(_._2)
    val hitsThreshold = totalHits * pgiHitsCoverageThreshold / 100

    var gatheredHits = 0
    val gatheredTargets = mutable.Buffer.empty[(Method, PGO)]
    for ((t, h) <- plannedTargets.iterator take pgiMaxTargetsCount) {
      if (gatheredHits < hitsThreshold) {
        gatheredHits += h
        gatheredTargets += ((t, PGO(h, totalHits - gatheredHits)))
      } // else break (but this is scala)
    }

    val guardedTargets = gatheredTargets.iterator
      .map(x => chooseGuardForTarget(rcvTypeAppr, originalRef, x._1, guardMode, x._2))
      .takeWhile(_ != UnknownTarget)
      .map(_.asInstanceOf[OneGuardedTarget])
      .toList
    guardedTargets match {
      case Seq() => UnknownTarget
      case Seq(x) => x
      case xs =>
        assert(xs forall (_.rcvType == rcvTypeAppr))
        val info = xs.iterator map (_.info.asInstanceOf[PGO]) reduce (_ ++ _)
        MultipleGuardedTargets(xs map { gt => (gt.target, gt.guard) }, info, rcvTypeAppr)
    }
  }

  private def findJCAMethodTarget(rcvTypeAppr: Cone, guardMode: GuardMode, originalRef: MethodReference) = {
    findInPoint(rcvTypeAppr.root, originalRef) match {
      case OneDirectTarget(method) if method.isJCAInline => chooseGuardForTarget(rcvTypeAppr, originalRef, method, guardMode, TauInfo.JCA)
      case _ => UnknownTarget
    }
  }

  private def findJCAInlineInContextWithPointTest(rcv: Node, rcvTypeAppr: Cone, guardMode: GuardMode, originalRef: MethodReference): CallTargetSearchResult = {
    assert(guardMode != GuardMode.NoGuards)

    if (!rootMethod.hasReceiverParameter) {
      return UnknownTarget
    }

    val rcvClass = rcvTypeAppr.root
    val rcvClassSym = rcvClass.symType
    if (rcvClassSym != rootReceiverType) {
      // Someone casted receiver to another class, don't mess with this.
      return UnknownTarget
    }

    findInPoint(rcvClass, originalRef) match {
      case OneDirectTarget(method) if
          shouldInlineWithContextPointTest(method, rcv) &&
          pointTestIsPossible(originalRef, rcvClassSym) =>
        OneGuardedTarget(method, PointGuard(rcvClassSym), TauInfo.JCA, rcvTypeAppr)

      case _ => UnknownTarget
    }
  }

  /** Returns whether `target` potentially may be called while calling `original`. */
  private def isCompatibleTarget(originalRef: MethodReference, target: Method): Boolean = {
    val original = originalRef.method
    val refClass = originalRef.refClass
    val targetHost = target.getDeclaringClass
    if (originalRef.isInterfCall) {
      assert(refClass.isInterface)
      assert(original.isPublic)
      // check that there is no IAE, check trivial incompatibility, nothing more can be done
      target.isPublic && (!targetHost.isFinal || (targetHost doesImplement refClass))

    } else {
      if (targetHost.isClass) {
        (ReferenceType(refClass) compare ReferenceType(targetHost)) match {
          case CC.Equal => true
          case CC.Greater => targetHost.findMethodImplementation(originalRef) contains target // check that target overrides original
          case CC.Less => false // original overrides target, target cannot be called
          case CC.Incomparable => false // totally different methods
          case CC.PartiallyEqual => shouldNotReachHere()
        }

      } else {
        assert(targetHost.isInterface)
        assert(target.isPublic)
        // default interface method cannot be called if there is a different implementation in refClass
        refClass.findMethodImplementation(originalRef) contains target
      }
    }
  }

  /** Returns whether `target` potentially may be called from given receiver. */
  private def receiverHasTarget(rcvTypeAppr: Cone, originalRef: MethodReference, target: Method): Boolean = {
    (cone(target.getDeclaringClass) compare rcvTypeAppr) match {
      case CC.Incomparable =>
        false

      case CC.Greater =>
        rcvTypeAppr.root match {
          case root: ClassType =>
            // if we assume that target is our referenced method (aka original)
            // check that it may be called at least at root
            // (i.e. not overriden by somebody between target.getDeclaringClass and root)
            !(ReferenceType(originalRef.refClass) >= root) ||
              (root.findMethodImplementation(originalRef) contains target)

          case _: InterfaceType =>
            // we cannot analyze interface implementation, assume true
            true

          case _: JavaArrayType =>
            assert(target.getDeclaringClass.isJavaLangObject)
            shouldNotReachHere("such call must be devirtualized earlier")

          case _: AJArrayType | _: CangjieArrayType =>
            shouldNotReachHere("unexpected AJ array receiver type")
        }

      case CC.Equal | CC.Less | CC.PartiallyEqual =>
        true
    }
  }

  /** Assumes that `target` potentially may be called while calling `original` from receiver type approximation.
    * Returns some `OneGuardedTarget` if given target is invoked from given receiver type if and only if type approximation satisfies test.
    * Returns `UnknownTarget` if we cannot generate any test for this.
    */
  private def chooseGuardForTarget(rcvTypeAppr: Cone, originalRef: MethodReference, target: Method, guardMode: GuardMode, tauInfo: TauInfo): CallTargetSearchResult = {
    require(!target.getDeclaringClass.isThinClass)
    guardMode match {
      case GuardMode.AnyGuards => return OneGuardedTarget(target, MagicGuard, tauInfo, rcvTypeAppr)
      case GuardMode.RealGuards => // continue and find real guard
      case GuardMode.NoGuards => shouldNotReachHere()
    }

    def backupPath() = {
      val targetHost = target.getDeclaringClass
      if (targetHost.isFinal) {
        assert(pointTestIsPossible(originalRef, targetHost))
        OneGuardedTarget(target, PointGuard(targetHost), tauInfo, rcvTypeAppr)
      } else {
        methodTestIfPossible(rcvTypeAppr, originalRef, target, tauInfo)
      }
    }

    if (!chaInlineAllowed(rcvTypeAppr.root)) {
      return backupPath()
    }

    val refClassAppr = cone(originalRef.refClass)
    val targetHostAppr = cone(target.getDeclaringClass)
    val intersected =
      ReferenceApprox.weakIntersect(refineTypeSpeculatively(rcvTypeAppr), refineTypeSpeculatively(refClassAppr), refineTypeSpeculatively(targetHostAppr))._1 match {
        case RefEmpty => return backupPath() // Cone without non-abstract implementations in CHA.
        case x: UpperBounded => x
        case x => shouldNotReachHere(x)
      }

    val (iRoot, iLevelLimit) = intersected match {
      case TypeClosedClass(root: ClassType, _, _, maxLevel) =>
        (root, maxLevel)

      case OpenCone(ReferenceType.javaLangObject | _: InterfaceType, _) =>
        // It is uselss to analyze Object's hierarchy or such interface (if interface cannot be refined to some class,
        // i.e. more than one implementing class).
        return backupPath()

      case UpperBounded(_: JavaArrayType, _) =>
        assert(originalRef.method.getDeclaringClass.isJavaLangObject && rcvTypeAppr.root.isInstanceOf[JavaArrayType])
        shouldNotReachHere("such calls must be devirtualized unconditionally")

      case _ => shouldNotReachHere(intersected)
    }

    findCHAImplementation(iRoot, iLevelLimit, originalRef, Some(target)) match {
      case Some(CHAResult(result, minLevel, maxLevel)) =>
        assert(result contains target)
        selectRealGuardForSubCone(rcvTypeAppr, iRoot, iLevelLimit, originalRef, target, minLevel, maxLevel, tauInfo)

      case _ =>
        backupPath()
    }
  }


  /** It is known that invocation of `original` method in scope of sub-cone (defined by `subRoot` and `subRootLevelLimit`)
    * results in invocation of `target` method if level is limited in range from `targetMinLevel` to `targetMaxLevel`.
    * This function selects guard which covers the sub-cone limited by level from range and
    * does not clash with other implementations of `original` method from `rcvType` cone.
    */
  private def selectRealGuardForSubCone(rcvType: Cone,
                                        subRoot: ClassType, subRootLevelLimit: Int,
                                        originalRef: MethodReference, target: Method,
                                        targetMinLevel: Int, targetMaxLevel: Int,
                                        tauInfo: TauInfo): CallTargetSearchResult = {
    require(!target.getDeclaringClass.isThinClass)
    assert(chaInlineAllowed(rcvType.root))

    def sameImplIn(root: ReferenceType, levelLimit: Int): Boolean = {
      if (root == subRoot && levelLimit <= targetMaxLevel) {
        // fast-path:
        // we were asked for subcone of subcone with the only implementation
        return true
      }

      findCHAImplementation(root, levelLimit, originalRef, Some(target)) match {
        case Some(CHAResult(_, _, maxLevel)) if maxLevel == levelLimit => true
        case _ => false
      }
    }

    val rcvRoot = rcvType.root
    val rcvLevelLimit = rcvType match {
      case rcvType: ClosedCone => rcvType.maxLevel
      case _: OpenCone => rcvType.root match {
        case c: ClassType => CHA.maxClassLevel(c)
        case i: InterfaceType => CHA.implClasses(i) map CHA.maxClassLevel reduceOption (_ max _) getOrElse (-1)
        case x => shouldNotReachHere(x)
      }
    }

    // Cone of object is practically useless if guard doesn't refine root.
    // It may be treated like a fast-path however it is required for smart bookkeeping.
    def rcvRootIsNotObject = rcvRoot != ReferenceType.javaLangObject

    if (rcvRootIsNotObject &&
        sameImplIn(rcvRoot, rcvLevelLimit)) {
      return OneGuardedTarget(target, CHABitGuard, tauInfo, rcvType)
    }

    if (targetMinLevel == subRoot.cohenLevel) {
      assert(pointTestIsPossible(originalRef, subRoot.symType))
      return OneGuardedTarget(target, PointGuard(subRoot.symType), tauInfo, rcvType)
    }

    if (rcvRootIsNotObject &&
        sameImplIn(rcvRoot, targetMinLevel)) {
      return OneGuardedTarget(target, LevelGuard(targetMinLevel), tauInfo, rcvType)
    }

    if (sameImplIn(subRoot, rcvLevelLimit)) {
      return OneGuardedTarget(target, MaxClosedConeGuard(subRoot.symType), tauInfo, rcvType)
    }

    methodTestIfPossible(rcvType, originalRef, target, tauInfo)
  }

  /** Check if such method test can be generated and wouldn't hide ICCE in case of interface call. */
  private def methodTestIfPossible(rcvTypeAppr: Cone, originalRef: MethodReference, target: Method, tauInfo: TauInfo): CallTargetSearchResult = {
    if (MethodTest.canBeGeneratedFor(target)) OneGuardedTarget(target, MethodGuard(originalRef, target), tauInfo, rcvTypeAppr) else UnknownTarget
  }

  /** Check if such point test can be generated and wouldn't hide ICCE in case of interface call. */
  private def pointTestIsPossible(originalRef: MethodReference, targetHost: SymType) = {
    !targetHost.isAbstractClass && (!originalRef.isInterfCall || (targetHost doesImplement originalRef.method.getDeclaringClass))
  }

  def cone(root: SymType) = OpenCone(ReferenceType(asClassType(root)), mayBeNull = false)


  /** CHA is enabled and it may be used for guarded devirtualization at root method
    * (guarded devirtualization should not be used at runtime reusable classes).
    */
  private def chaInlineAllowed(rcvType: ReferenceType) = CHA.isKnownType(rcvType)

  private case class CHAResult(target: FindMethodImplResult, minLevel: Int, maxLevel: Int)

  /** Finds implemention of `original` method in cone of `rcvType`.
    * It correctly handles sitution when method is not invocable in scope of `rcvType`.
    * Cone is traversed using CHA, abstract classes and classes not satisfying `bitCheck` are ignored
    * (special `noBitCheck` may be used).
    * Cone height is limited by `levelLimit`.
    *
    * There are two modes:
    * <ul>
    *   <li>
    *     if `expectedTarget` is not defined:<br/>
    *     looks for implementation closest to `rcvType` and determines height of subcone
    *     where this implementation is used
    *   </li>
    *   <li>
    *     if `expectedTarget` is defined:<br/>
    *     determines height of subcone where given target is used
    *   </li>
    * </ul>
    *
    * Returns `None` if there is no suitable subcone (some inheritors override found implementation and some not).
    * Otherwise returns `CHAResult` which contains:
    * found implementation (may be absent, i.e. no implementation in given limits) and
    * subcone level limits range (any level from this range may be used as subcone level limit
    * (abstract classes are sources of non-point range)).
    */
  private def findCHAImplementation(rcvType: ReferenceType, levelLimit: Int,
                                    originalRef: MethodReference, expectedTarget: Option[Method]): Option[CHAResult] = {
    require(CHA.isKnownType(rcvType))
    require(0 <= levelLimit) // level limit may exceed max level of rcvType, it's ok

    val refClass = ReferenceType(originalRef.refClass)
    if (!(refClass >= rcvType)) {
      // Note that abstract and bit-less classes may violate this check.
      // But it is quite expensive check, so we conservatively check it only once for the root.
      return None
    }

    val klasses = mutable.Queue.empty[ClassType]
    rcvType match {
      case c: ClassType => klasses += c
      case i: InterfaceType => klasses ++= CHA.implClasses(i)
      case _ => shouldNotReachHere(rcvType)
    }

    // mutable state:
    // Equal to null unless target was given or found during the traversing.
    var target: FindMethodImplResult = (expectedTarget map (new FindMethodImplResult.Found(_))).orNull
    var minLevel = 0
    var maxLevel = levelLimit
    var foundAnyClassForTarget = false

    while (klasses.nonEmpty) {
      val klass = klasses.dequeue()
      val curLevel = klass.cohenLevel

      val analyzeSubClasses =
        if (curLevel > levelLimit) {
          false

        } else if (!klass.isAbstract) {
          // find method only in classes that could be instantiated at run-time

          val curImpl = klass.findMethodImplementation(originalRef)
          if (target == null) {
            // the first found target, even if it is Left
            target = curImpl
          }

          if (target == curImpl) {
            // we have found the same target, advance level to include klass
            foundAnyClassForTarget = true
            minLevel = Math.max(minLevel, curLevel)
          } else {
            // we have found another target, advance level to exclude klass
            maxLevel = Math.min(maxLevel, curLevel - 1)
          }

          if (minLevel > maxLevel) {
            // It means that there is no suitable subcone with the same target, stop everything.
            return None
          }

          // Stop scanning subclasses if they have different implementation and are not interesting anymore.
          (curImpl == target)

        } else {
          true
        }

      if (analyzeSubClasses) {
        klasses ++= CHA.subClasses(klass)
      }
    }

    if (foundAnyClassForTarget) {
      assert(target != null)
      assert(minLevel <= maxLevel)
      Some(CHAResult(target, minLevel, maxLevel))
    } else {
      None
    }
  }

}

