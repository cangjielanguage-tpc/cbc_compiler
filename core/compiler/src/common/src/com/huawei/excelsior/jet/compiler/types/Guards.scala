/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.common.CodeHelpers._
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes._
import com.huawei.excelsior.jet.compiler.types.References._
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType => SymClassType, Method, MethodReference}
import xscala.util.simpleClassName

import scala.PartialFunction.condOpt

object Guards {
  sealed abstract class Guard(private[Guards] val priority: Int) {
    final def name: String = simpleClassName(this)

    final def intersectWith(tpe: ReferenceApprox): (ReferenceApprox, Boolean) =
      processNullable(tpe, intersectGuardWith(this, _))

    final def subtractFrom(tpe: ReferenceApprox): (ReferenceApprox, Boolean) =
      processNullable(tpe, subtractGuardFrom(this, _))

    /** Returns guard `iGuard` such that it filters the same objects as conjunction of `thisGuard` and `thatGuard`.
      * Returns `None` if such guard cannot be found.
      */
    final def intersectWith(that: Guard, inputType: ReferenceApprox): Option[Guard] =
      intersectGuards(this, that, inputType)

    // TODO write better implementation and unit-test when there are uses outside of asserts
    final def disjointWith(that: Guard): Boolean =
      intersectWith(that.intersectWith(OpenCone(ReferenceTypes.ReferenceType.javaLangObject, mayBeNull = false))._1)._1 == References.RefEmpty
  }

  sealed abstract class TypeGuard(priority: Int) extends Guard(priority)

  sealed abstract class UpperBoundedGuard(priority: Int) extends TypeGuard(priority) {
    def root: SymClassType
    override def toString = s"$name(${root.getName})"

    require(root.isClass, root)
  }

  sealed trait StrictTypeApprGuard extends TypeGuard {
    def typeAppr: ReferenceApprox
  }

  sealed abstract class ConeGuard(priority: Int, val closed: Boolean) extends UpperBoundedGuard(priority)

  object ConeGuard {
    def unapply(guard: ConeGuard): Option[(SymClassType, Boolean)] = Some((guard.root, guard.closed))
  }

  case object MagicGuard extends Guard(0)

  case object CHABitGuard extends TypeGuard(1)

  case class PointGuard(root: SymClassType) extends UpperBoundedGuard(2) with StrictTypeApprGuard {
    require(!root.isAbstractClass, root)

    def typeAppr = Point(ReferenceType(root), mayBeNull = false)
  }

  case class LevelGuard(level: Int) extends TypeGuard(3) {
    override def toString = s"$name($level)"

    // level 0 should not be used, it's a PointTest(j.l.Object)
    require(level >= 1)
  }

  case class MaxClosedConeGuard(root: SymClassType) extends ConeGuard(4, true) {
    require(CHA.maxClassHeight(ClassType(root)) > 1) // Otherwise you should use PointGuard.

    // Note that its type approximation is not strict if root has no CC bit.
  }

  case class OpenConeGuard(root: SymClassType) extends ConeGuard(5, false) with StrictTypeApprGuard {
    def typeAppr = OpenCone(ReferenceType(root), mayBeNull = false)
  }

  case class MethodGuard(originalRef: MethodReference, target: Method) extends Guard(6) {
    def original: Method = originalRef.method
    override def toString = s"$name(${original.getFullName} -> ${target.getFullName})"

    // target host should not be final, it's a PointTest(target.getDeclaringClass)
    require(!target.getDeclaringClass.isFinal)
  }


  private def flatMap(tpe: ReferenceApprox, strict: Boolean, func: ReferenceApprox => (ReferenceApprox, Boolean)): (ReferenceApprox, Boolean) = {
    val (resTpe, resStrict) = func(tpe)
    if (resTpe == RefEmpty) {
      (resTpe, true)
    } else {
      (resTpe, strict && resStrict)
    }
  }

  // Suffix "P" (pair) - somehow these overloaded methods conflict with each other.
  private def flatMapP(tpe: (ReferenceApprox, Boolean), func: ReferenceApprox => (ReferenceApprox, Boolean)): (ReferenceApprox, Boolean) =
    flatMap(tpe._1, tpe._2, func)


  /** In run-time all guards have non-null input type.
    * However our type system is conservative so we must be able to handle even nullable input types.
    * We simply ignore it leaving this conservatism because filtering it out may be observed as real null filtering
    * (which is not logically correct).
    */
  // TODO: introduce trustedPreFilterFunc and remove this hack, see ContextTypes.recalculate
  private def processNullable(tpe: ReferenceApprox, processNonNull: ReferenceApprox => (ReferenceApprox, Boolean)): (ReferenceApprox, Boolean) = {
    if (!tpe.mayBeNull) {
      processNonNull(tpe)
    } else {
      // ...
      val (r, s) = processNonNull(tpe.withoutNull)
      (r.withNull, s)
    }
  }

  private def intersectGuardWith(guard: Guard, tpe: ReferenceApprox): (ReferenceApprox, Boolean) = {
    assert(!tpe.mayBeNull)

    if (tpe == RefEmpty) {
      return (tpe, true)
    }

    guard match {
      case guard: StrictTypeApprGuard /* PointGuard | OpenConeGuard */ =>
        flatMap(guard.typeAppr, strict = true, tpe.weakIntersect)

      case CHABitGuard =>
        tpe.filterClosed()

      case LevelGuard(level) =>
        tpe.filterLevel(level)

      case MaxClosedConeGuard(klass) =>
        // Strict intersection followed by filtering CC bit is better in practice
        // compared to intersection with filtered cone.
        flatMapP(flatMap(
          ClosedCone.max(ClassType(klass), mayBeNull = false), strict = true,
          tpe.weakIntersect),
          _.filterClosed()
        )

      case MethodGuard(originalRef, target) =>
        val targetHost = ReferenceType(target.getDeclaringClass)
        val potentialCone = OpenCone(targetHost, mayBeNull = false)
        flatMap(potentialCone, strict = true, tpe.weakIntersect) match {
          case (tpe @ UpperBounded(root, _), true) =>
            if (originalRef.refClass isAssignableFrom root.symType) {
              if (root.symType.findMethodImplementation(originalRef) contains target) {
                tpe match {
                  case _: Point => (tpe, true)
                  case _: OpenCone => (tpe, false)
                  case _: ClosedCone => (tpe, false) // We could do better, but not now.
                }
              } else {
                (RefEmpty, true)
              }

            } else {
              // Guarded type is non-strict because without analyzing methods we cannot be sure
              // that given cone really has implementation target as implementation.
              (tpe, false)
            }

          case res => res
        }

      case _ => shouldNotReachHere(guard)
    }
  }

  private def subtractGuardFrom(guard: Guard, tpe: ReferenceApprox): (ReferenceApprox, Boolean) = {
    assert(!tpe.mayBeNull)

    val (iRes, iStrict) = intersectGuardWith(guard, tpe)
    if (iStrict) {
      // T subtract G == T subtract (T intersect G), if intersection is strict
      tpe subtract iRes
    } else {
      // it is not correct to subtract unstrict result, so we just ignore subtraction :/
      (tpe, false)
    }
  }

  private def intersectGuards(guard1: Guard, guard2: Guard, inputType: ReferenceApprox): Option[Guard] = {

    /** This intersection doesn't create more precise (and more expensive?) guards. */
    def tryOneOfExisting() = {
      val (type1, strict1) = guard1.intersectWith(inputType)
      val (type2, strict2) = guard2.intersectWith(inputType)

      condOpt(type1 compare type2) {
        case CC.Equal if strict1 && strict2 => if (guard1.priority <= guard2.priority) guard1 else guard2
        case CC.Equal | CC.Greater  if strict1 => guard2
        case CC.Equal | CC.Less     if strict2 => guard1
      }
    }

    def tryByTypeIntersection(guard1: Guard, guard2: Guard): Option[Guard] = {
      val (t1, s1) = guard1.intersectWith(inputType)
      if (!s1) return tryOneOfExisting()

      val (t2, s2) = guard2.intersectWith(t1)
      if (!s2) return tryOneOfExisting()

      val tGuard = t2 match {
        case Point(root, _) => PointGuard(root.symType)
        case OpenCone(root, _) => OpenConeGuard(root.symType)
        case TypeClosedClass(root, _, _, maxLevel) if CHA.maxClassLevel(root) == maxLevel => MaxClosedConeGuard(root.symType)
        case RefEmpty => return None
        case _ => return tryOneOfExisting()
      }

      Some(tryOneOfExisting() match {
        case Some(bGuard) if bGuard.priority <= tGuard.priority => bGuard
        case _ => tGuard
      })
    }

    (guard1, guard2) match {
      // fast path
      case (`guard1`, `guard1`) => Some(guard1)

      case (LevelGuard(l1), LevelGuard(l2)) => Some(LevelGuard(math.min(l1, l2)))

      case _ =>
        // Order of guard intersection application matters!
        // E.g. it's better to apply PointGuard first and then MaxCCGuard.
        tryByTypeIntersection(guard1, guard2) orElse
          tryByTypeIntersection(guard2, guard1)
    }
  }

}
