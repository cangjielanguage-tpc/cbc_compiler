/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType.LanguageRoot

import scala.PartialFunction.condOpt
import scala.annotation.nowarn

object References {

  sealed abstract class ReferenceApprox extends Approximation {

    final def union(that: Approximation): ReferenceApprox = {
      that match {
        case that: ReferenceApprox => ReferenceApprox.union(this, that)
        case _ => shouldNotReachHere()
      }
    }

    final override def weakIntersect(that: Approximation): (ReferenceApprox, Boolean) =  {
      that match {
        case that: ReferenceApprox => ReferenceApprox.weakIntersect(this, that)
        case _ => shouldNotReachHere()
      }
    }

    final override def intersect(that: Approximation): ReferenceApprox = weakIntersect(that)._1

    override final def compare(that: Approximation): CC = that match {
      case that: ReferenceApprox => ReferenceApprox.compare(this, that)
      case _ => shouldNotReachHere()
    }

    final def compareWidened(that: ReferenceApprox): CC = ReferenceApprox.compareWidened(this, that)

    final def incomparable(that: ReferenceApprox): Boolean = (this compare that) == CC.Incomparable

    // Note that `equals` ignores probable type.
    final def equalsWidened(that: ReferenceApprox): Boolean = (this == that) && (this.probableType == that.probableType)

    final def subtract(that: ReferenceApprox): (ReferenceApprox, Boolean) = ReferenceApprox.subtract(this, that)

    final def filterClosed(): (ReferenceApprox, Boolean) = ReferenceApprox.filterNonAbstractCHAClasses(this, None)
    final def filterLevel(level: Int): (ReferenceApprox, Boolean) = ReferenceApprox.filterNonAbstractCHAClasses(this, Some(level))

    def mayBeNull: Boolean

    def withNull = if (mayBeNull && probableType.mayBeNull) this else withNullImpl
    protected def withNullImpl: ReferenceApprox

    def withoutNull = if (!mayBeNull) this else withoutNullImpl
    protected def withoutNullImpl: ReferenceApprox

    def hasRefinedProbableType: Boolean = false
    def probableType: ReferenceApprox = this
    def safeType: ReferenceApprox = this
    def withProbableType(probableType: ReferenceApprox) = this

    def transform(f: ReferenceApprox => ReferenceApprox): ReferenceApprox = {
      if (hasRefinedProbableType) f(this).withProbableType(f(probableType))
      else f(this)
    }

    override final def isEmpty = (this eq RefEmpty)
  }

  // TODO: inheritors are de facto final classes, but Scala 2.10.3 has problems with pattern matching
  // of final inner classes and generates warnings:
  //     warning: The outer reference in this type test cannot be checked at run time.
  // Mark them final when this will be fixed (SI-4440).

  object ProbableType {
    def unapply(t: ReferenceApprox) = if (t.hasRefinedProbableType) Some(t.probableType) else None
  }


  case object RefEmpty extends ReferenceApprox {
    def mayBeNull = false

    protected def withNullImpl = RefNull
    protected def withoutNullImpl = this
  }


  case object RefNull extends ReferenceApprox {
    def mayBeNull = true

    protected def withNullImpl = this
    protected def withoutNullImpl = RefEmpty
  }

  private def nullOrEmpty(mayBeNull: Boolean) =
    if (mayBeNull) RefNull else RefEmpty


  sealed abstract class UpperBounded extends ReferenceApprox {
    def root: ReferenceType
  }

  object UpperBounded {
    def unapply(t: UpperBounded) = Some(t.root, t.mayBeNull)
  }


  sealed trait ClosedUpperBounded extends UpperBounded {
    def height: Int
  }

  object TypeClosedClass {
    def unapply(t: ClosedUpperBounded): Option[(ClassType, Boolean, Int, Int)] = condOpt(t) {
      case Point(root: ClassType, mbn) => (root, mbn, root.cohenLevel, root.cohenLevel)
      case t: ClosedCone => (t.root, t.mayBeNull, t.rootLevel, t.maxLevel)
    }
  }


  /** Reference approximations which can really be widened by probable type.
    * Other type approximations just ignore their probable type.
    */
  sealed abstract class WidenedUpperBounded private[References](val _probableType: ReferenceApprox) extends UpperBounded {
    if (hasRefinedProbableType) {
      require(!probableType.hasRefinedProbableType, "reject nested probable types")
      require((this compare probableType) == CC.Greater, (this, probableType))
    }

    override final def toString = if (hasRefinedProbableType) {
      s"<$toStringNoProbable, ${_probableType}>"
    } else {
      toStringNoProbable
    }

    protected def toStringNoProbable: String

    override final def hasRefinedProbableType = _probableType != null
    override final def probableType: ReferenceApprox = if (hasRefinedProbableType) _probableType else this
    override final def safeType: ReferenceApprox = if (hasRefinedProbableType) withProbableType(null) else this

    override final def withProbableType(probableType: ReferenceApprox): ReferenceApprox = {
      require(probableType == null || !probableType.hasRefinedProbableType)
      if (probableType == this) withProbableType(null)
      else if (probableType != _probableType) withProbableTypeImpl(probableType)
      else this
    }

    protected def withProbableTypeImpl(probableType: ReferenceApprox): ReferenceApprox

    protected final def withNullImpl = copyNoProbable(mayBeNull = true).withProbableType(
      if (hasRefinedProbableType) _probableType.withNull else null
    )

    protected final def withoutNullImpl = copyNoProbable(mayBeNull = false).withProbableType(
      if (hasRefinedProbableType) _probableType.withoutNull else null
    )

    protected def copyNoProbable(mayBeNull: Boolean): ReferenceApprox

  }


  sealed abstract class Cone private[References](_probableType: ReferenceApprox) extends WidenedUpperBounded(_probableType) {
    require(!root.isFinal, root)
  }

  object Cone {
    def unapply(t: Cone) = Some(t.root, t.mayBeNull)
  }


  final class OpenCone private(val root: ReferenceType, val mayBeNull: Boolean, _probableType: ReferenceApprox) extends Cone(_probableType) {

    override def equals(that: Any) = that match {
      case that: OpenCone => this.root == that.root && this.mayBeNull == that.mayBeNull
      case _ => false
    }

    override def hashCode = root.hashCode * 2 + (if (mayBeNull) 1 else 0)

    protected def toStringNoProbable = s"TypeOpenCone($root, $mayBeNull)"
    protected def withProbableTypeImpl(probableType: ReferenceApprox) = new OpenCone(root, mayBeNull, probableType)
    protected def copyNoProbable(mayBeNull: Boolean) = new OpenCone(root, mayBeNull, null)
  }

  object OpenCone {
    def apply(root: ReferenceType, mayBeNull: Boolean) = root match {
      case _ if root.isFinal => Point(root, mayBeNull)
      case root: ClassType if CHA.isKnownType(root) && CHA.isClosed(root) => ClosedCone.max(root, mayBeNull)
      case _ => new OpenCone(root, mayBeNull, null)
    }

    def unapply(t: OpenCone) = Some(t.root, t.mayBeNull)
  }


  final class ClosedCone private(val root: ClassType, val mayBeNull: Boolean,
                                 val height: Int,
                                 _probableType: ReferenceApprox)
      extends Cone(_probableType) with ClosedUpperBounded {
    require(CHA.isKnownType(root), s"inconsistency between CHA and type $root")

    require(height >= 2)
    require(height <= CHA.maxClassHeight(root))

    def rootLevel: Int = root.cohenLevel
    def maxLevel: Int = rootLevel + height - 1

    /** Returns common root for all subcones implementing given interface, may be non-strict.
      * Returns `None` if there is no non-abstract classes implementing interface.
      */
    private[types] def subConeImplementingInterface(interf: InterfaceType): Option[ClassType] =
      ReferenceApprox.greatestCommonRootForNonAbstractSubClasses(root, maxLevel, _ implements interf)

    override def equals(that: Any) = that match {
      case that: ClosedCone => this.root == that.root && this.mayBeNull == that.mayBeNull && this.height == that.height
      case _ => false
    }

    override def hashCode = (root.hashCode * 31 + height) * 2 + (if (mayBeNull) 1 else 0)

    protected def toStringNoProbable = s"TypeClosedCone($root, $mayBeNull, $height)"
    protected def withProbableTypeImpl(probableType: ReferenceApprox) = new ClosedCone(root, mayBeNull, height, probableType)
    protected def copyNoProbable(mayBeNull: Boolean) = new ClosedCone(root, mayBeNull, height, null)
  }

  object ClosedCone {

    private [References] val MaxHeight = Int.MaxValue
    private [References] val MaxLevel = Int.MaxValue

    def max(root: ClassType, mayBeNull: Boolean): ReferenceApprox = withHeight(root, mayBeNull, MaxHeight)

    def withMaxLevel(root: ClassType, mayBeNull: Boolean, maxLevel: Int): ReferenceApprox = {
      withHeight(root, mayBeNull,
        if (maxLevel == MaxLevel) MaxHeight else (maxLevel - root.cohenLevel + 1))
    }

    def withHeight(root: ClassType, mayBeNull: Boolean, height: Int): ReferenceApprox = {
      require(CHA.isKnownType(root))
      require(height >= 1)
      val maxLevel = if (height == MaxHeight) MaxLevel else (root.cohenLevel + height - 1)
      ReferenceApprox.greatestCommonRootForNonAbstractSubClasses(root, maxLevel) match {
        case None =>
          nullOrEmpty(mayBeNull)
        case Some(newRoot) =>
          val newHeight = Math.min(CHA.maxClassHeight(newRoot), height - (newRoot.cohenLevel - root.cohenLevel))
          if (newHeight == 1) {
            Point(newRoot, mayBeNull)
          } else {
            new ClosedCone(newRoot, mayBeNull, newHeight, null)
          }
      }
    }
  }

  case class Point(root: ReferenceType, mayBeNull: Boolean) extends ClosedUpperBounded {
    require(root match {
      case root: ClassType => !root.isAbstract
      case _: ArrayType => true
      case _: InterfaceType => false
    }, root)

    def height = 1

    override def toString = s"TypePoint($root, $mayBeNull)"

    protected def withNullImpl = copy(mayBeNull = true)
    protected def withoutNullImpl = copy(mayBeNull = false)
  }


  case class TypeOnEdge(tpe: ReferenceApprox, isColdEdge: Boolean) {
    def union(that: TypeOnEdge): TypeOnEdge = ReferenceApprox.unionOnEdges(this, that)
  }


  object ReferenceApprox {

    private def getProbableTypeOnEdge(typeOnEdge: TypeOnEdge): ReferenceApprox =
      if (typeOnEdge.isColdEdge) RefEmpty else typeOnEdge.tpe.probableType

    def unionOnEdges(typeOnEdge1: TypeOnEdge, typeOnEdge2: TypeOnEdge): TypeOnEdge = {
      val TypeOnEdge(t1, isColdEdge1) = typeOnEdge1
      val TypeOnEdge(t2, isColdEdge2) = typeOnEdge2
      val checkProbableType = isColdEdge1 || isColdEdge2 || t1.hasRefinedProbableType || t2.hasRefinedProbableType
      val isColdResult = isColdEdge1 && isColdEdge2

      val unionResult = if (checkProbableType) {
        unionWithProbableTypes(t1.safeType, getProbableTypeOnEdge(typeOnEdge1), t2.safeType, getProbableTypeOnEdge(typeOnEdge2))
      } else {
        union(t1, t2)
      }
      TypeOnEdge(unionResult, isColdResult)
    }

    private def unionWithProbableTypes(t1: ReferenceApprox, p1: ReferenceApprox, t2: ReferenceApprox, p2: ReferenceApprox): ReferenceApprox = {
      assert(!t1.hasRefinedProbableType)
      assert(!p1.hasRefinedProbableType)
      assert(!t2.hasRefinedProbableType)
      assert(!p2.hasRefinedProbableType)

      val safeUnion = union(t1, t2)
      val probableUnion = union(p1, p2)

      if (safeUnion > probableUnion) {
        safeUnion.withProbableType(probableUnion)
      } else {
        safeUnion
      }
    }

    def union(t1: ReferenceApprox, t2: ReferenceApprox): ReferenceApprox = {
      if (t1.hasRefinedProbableType || t2.hasRefinedProbableType) {
        unionWithProbableTypes(t1.safeType, t1.probableType, t2.safeType, t2.probableType)

      } else {
        compare(t1, t2) match {
          case CC.Equal | CC.Greater => t1
          case CC.Less => t2

          case CC.PartiallyEqual | CC.Incomparable =>
            (t1, t2) match {
              case (RefNull, _) => t2.withNull
              case (_, RefNull) => t1.withNull

              case (UpperBounded(root1, mbn1), UpperBounded(root2, mbn2)) =>
                val mbn = mbn1 || mbn2

                (t1, t2) match {
                  case (TypeClosedClass(root1, _, _, maxLevel1), TypeClosedClass(root2, _, _, maxLevel2))
                    if CHA.isKnownType(root1 commonSuper root2) =>

                    val cs: ClassType = root1 commonSuper root2
                    val maxLevel = Math.max(maxLevel1, maxLevel2)
                    ClosedCone.withMaxLevel(cs, mayBeNull = mbn, maxLevel)

                  case _ =>
                    val cs: ReferenceType = root1 commonSuper root2
                    OpenCone(cs, mayBeNull = mbn)
                }

              case _ => shouldNotReachHere(s"unexpected types: $t1 & $t2")
            }
        }
      }
    }

    def compareWidened(t1: ReferenceApprox, t2: ReferenceApprox): CC = compare(t1, t2) match {
      case CC.Equal if t1.hasRefinedProbableType || t2.hasRefinedProbableType =>
        t1.probableType compare t2.probableType

      case x => x
    }

    def compare(t1: ReferenceApprox, t2: ReferenceApprox): CC = {
      def emptyAndAny = CC.Less
      def nullAndAny(x: ReferenceApprox) = if (x.mayBeNull) CC.Less else CC.Incomparable

      (t1, t2) match {
        case _ if t1 == t2 => CC.Equal

        case (RefEmpty, _) => emptyAndAny
        case (_, RefEmpty) => emptyAndAny.inverse

        case (RefNull, _) => nullAndAny(t2)
        case (_, RefNull) => nullAndAny(t1).inverse

        case _ =>

          def pointAndPoint(x: Point, y: Point) =
            if (x.root == y.root) CC.Equal else CC.Incomparable

          def pointAndCone(x: Point, y: OpenCone) =
            (x.root compare y.root) match {
              case CC.Equal | CC.Less => CC.Less
              case CC.Greater | CC.Incomparable | CC.PartiallyEqual => CC.Incomparable
            }

          def pointAndClosedCone(x: Point, y: ClosedCone) =
            x.root match {
              case xRoot: ClassType if y.rootLevel <= xRoot.cohenLevel &&
                                       xRoot.cohenLevel <= y.maxLevel &&
                                       (xRoot commonSuper y.root) == y.root =>
                CC.Less

              case _ => CC.Incomparable
            }

          def openConeAndOpenCone(x: OpenCone, y: OpenCone) =
            x.root compare y.root

          def openConeAndClosedCone(x: OpenCone, y: ClosedCone) = {
            def interfaceCase(xRoot: InterfaceType) =
              if (y.subConeImplementingInterface(xRoot).nonEmpty) CC.PartiallyEqual
              else CC.Incomparable

            (x.root compare y.root) match {
              case CC.Equal | CC.Greater => CC.Greater
              case CC.Incomparable => CC.Incomparable
              case CC.Less =>
                x.root match {
                  case xRoot: ClassType =>
                    if (xRoot.cohenLevel <= y.maxLevel) CC.PartiallyEqual else CC.Incomparable
                  case xRoot: InterfaceType =>
                    interfaceCase(xRoot)
                  case _: ArrayType =>
                    CC.Incomparable // no arrays in closed cone
                }
              case CC.PartiallyEqual =>
                interfaceCase(x.root.asInstanceOf[InterfaceType])
            }
          }

          def closedConeAndClosedCone(x: ClosedCone, y: ClosedCone) = {
            def cmpLG(lesser: ClosedCone, greater: ClosedCone) = {
              assert(lesser.rootLevel <= greater.rootLevel)
              if (lesser.maxLevel < greater.rootLevel) CC.Incomparable
              else if (lesser.maxLevel >= greater.maxLevel) CC.Greater
              else if (lesser.rootLevel == greater.rootLevel) CC.Less
              else CC.PartiallyEqual
            }

            val cs = x.root commonSuper y.root
            if (cs == x.root) cmpLG(x, y)
            else if (cs == y.root) cmpLG(y, x).inverse
            else CC.Incomparable
          }

          val rwon = (t1.withoutNull, t2.withoutNull) match {
            case (x: Point,      y: Point)      => pointAndPoint(x, y)
            case (x: Point,      y: OpenCone)   => pointAndCone(x, y)
            case (x: Point,      y: ClosedCone) => pointAndClosedCone(x, y)
            case (x: OpenCone,   y: Point)      => pointAndCone(y, x).inverse
            case (x: OpenCone,   y: OpenCone)   => openConeAndOpenCone(x, y)
            case (x: OpenCone,   y: ClosedCone) => openConeAndClosedCone(x, y)
            case (x: ClosedCone, y: Point)      => pointAndClosedCone(y, x).inverse
            case (x: ClosedCone, y: OpenCone)   => openConeAndClosedCone(y, x).inverse
            case (x: ClosedCone, y: ClosedCone) => closedConeAndClosedCone(x, y)

            case (x, y) => shouldNotReachHere(s"unexpected types without null: $x & $y")
          }


          def notNullAndNull(rwon: CC) =
            rwon match {
              case CC.Less | CC.PartiallyEqual | CC.Incomparable => rwon
              case CC.Equal => CC.Less
              case CC.Greater => CC.PartiallyEqual
            }

          def nullAndNull(rwon: CC) =
            rwon match {
              case CC.Greater | CC.Less | CC.Equal | CC.PartiallyEqual => rwon
              case CC.Incomparable => CC.PartiallyEqual
            }

          (t1.mayBeNull, t2.mayBeNull) match {
            case (false, false) => rwon
            case (false, true)  => notNullAndNull(rwon)
            case (true,  false) => notNullAndNull(rwon.inverse).inverse
            case (true,  true)  => nullAndNull(rwon)
          }
      }
    }

    /** Apply `safeOp` to safe and probable parts of `ts`. */
    private def unstrictOpWithProbable(ts: Seq[ReferenceApprox])
                                      (safeOp: Seq[ReferenceApprox] => (ReferenceApprox, Boolean)): (ReferenceApprox, Boolean) = {

      val (rSafe, strict) = safeOp(ts)

      val r =
        if ((ts exists (_.hasRefinedProbableType)) && rSafe.isInstanceOf[WidenedUpperBounded]) {
          val (probable, pStrict) = safeOp(ts map (_.probableType))
          if (rSafe >= probable) {
            rSafe.withProbableType(probable)
          } else {
            assert(!pStrict)
            rSafe
          }
        } else {
          rSafe
        }

      (r, strict)
    }

    private def isInterfaceOrArrayOfInterfaces(x: ReferenceType): Boolean = x match {
      case _: InterfaceType => true
      case x: JavaArrayType if x.base.isInstanceOf[InterfaceType] => true
      case _ => false
    }

    /** Returns pair (r, s), where `r` is the result of intersection, and `s` is true iff result is strict.
      * Not strict result should be >= than theoretical result.
      */
    def weakIntersect(t1: ReferenceApprox, t2: ReferenceApprox): (ReferenceApprox, Boolean) = {
      unstrictOpWithProbable(Seq(t1, t2)) { case Seq(t1: ReferenceApprox, t2: ReferenceApprox) =>
        val t1won = t1.withoutNull
        val t2won = t2.withoutNull

        val (rwon, strict) = t1won compare t2won match {
          case CC.Incomparable => (RefEmpty, true)
          case CC.Less | CC.Equal => (t1won, true)
          case CC.Greater => (t2won, true)
          case CC.PartiallyEqual =>
            def closedConeAndClosedCone(x: ClosedCone, y: ClosedCone) = {
              val root = if (x.rootLevel < y.rootLevel) y.root else x.root
              val maxLevel = Math.min(x.maxLevel, y.maxLevel)
              (ClosedCone.withMaxLevel(root, mayBeNull = false, maxLevel), true)
            }

            def closedConeAndOpenCone(x: ClosedCone, y: OpenCone) = {
              val root = y.root match {
                case yRoot: ClassType => yRoot
                case yRoot: InterfaceType =>
                  x.subConeImplementingInterface(yRoot) match {
                    case Some(subRoot) => subRoot
                    case None => shouldNotReachHere("impossible in case of partial equality")
                  }

                case _: ArrayType => shouldNotReachHere()
              }
              assert(x.root >= root)
              val strict = y.root >= root
              val maxLevel = x.maxLevel
              (ClosedCone.withMaxLevel(root, mayBeNull = false, maxLevel), strict)
            }

            def openConeAndOpenCone(x: OpenCone, y: OpenCone) = {
              // One of them is interface or array of interfaces.
              // Try to choose class or array of classes because they are more useful.
              val r = if (!isInterfaceOrArrayOfInterfaces(y.root)) y else x
              (r, false)
            }

            // TODO: investigate non strict cases, add method with explicit priority of arguments?

            (t1won, t2won) match {
              case (t1won: ClosedCone, t2won: ClosedCone) => closedConeAndClosedCone(t1won, t2won)
              case (t1won: ClosedCone, t2won: OpenCone)   => closedConeAndOpenCone(t1won, t2won)
              case (t1won: OpenCone, t2won: ClosedCone)   => closedConeAndOpenCone(t2won, t1won)
              case (t1won: OpenCone, t2won: OpenCone)     => openConeAndOpenCone(t1won, t2won)
              case _ => shouldNotReachHere(s"unexpected partially equal types: $t1won & $t2won")
            }
        }

        (if (t1.mayBeNull && t2.mayBeNull) rwon.withNull else rwon, strict)
      }
    }

    def weakIntersect(ts: ReferenceApprox*): (ReferenceApprox, Boolean) = {
      require(ts.size > 1, "at least one type should be intersected")

      @nowarn("msg=match may not be exhaustive")
      def shouldBeIntersectedFirst(t: ReferenceApprox) = t match {
        case RefEmpty | RefNull => true
        case UpperBounded(jt, _) => !isInterfaceOrArrayOfInterfaces(jt)
      }

      // First intersect types which are more likely to give strict results (e.g. classes).
      val tsSorted = ts.toList sortWith { (x, y) => shouldBeIntersectedFirst(x) && !shouldBeIntersectedFirst(y) }
      val (i, s) = tsSorted.tail.foldLeft((tsSorted.head, true)) { case ((i1, s1), x) =>
        val (i2, s2) = i1 weakIntersect x
        (i2, s1 && s2)
      }

      val correctedS = s || {
        // Calculated `s` might be false even for absolutely strict result.
        // E.g. there are at least three input types and the last one is TypePoint.
        tsSorted.size >= 3 && (tsSorted forall (_ >= i))
      }
      (i, correctedS)
    }

    /** Returns pair (r, s), where `r` is the result of subtraction, and `s` is true iff result is strict.
      * Not strict result should be >= than theoretical result.
      */
    private def subtract(t1: ReferenceApprox, t2: ReferenceApprox): (ReferenceApprox, Boolean) = {
      unstrictOpWithProbable(Seq(t1, t2)) { case Seq(t1: ReferenceApprox, t2: ReferenceApprox) =>
        val t1won = t1.withoutNull
        val t2won = t2.withoutNull

        val (rwon, strict) =
          t1won compare t2won match {
            case CC.Incomparable => (t1won, true)
            case CC.Less | CC.Equal => (RefEmpty, true)
            case CC.Greater | CC.PartiallyEqual =>
              if (t2won == RefEmpty) (t1won, true) // in this case it's more like incomparable
              else (t1won, false) // oops, we cannot do better
          }

        (if (t1.mayBeNull && !t2.mayBeNull) rwon.withNull else rwon, strict)
      }
    }

    /** Returns pair (r, s), where `r` is the result of filtering
      * only non-abstract classes passing given `classCheck` with level less than `levelLimitOpt`,
      * and `s` is true iff result is strict.
      * Not strict result should be >= than theoretical result.
      */
    private def filterNonAbstractCHAClasses(t: ReferenceApprox, levelLimitOpt: Option[Int]): (ReferenceApprox, Boolean) = {
      require(!t.mayBeNull)
      assert(levelLimitOpt forall (_ >= 1))

      unstrictOpWithProbable(Seq(t)) { case Seq(t: ReferenceApprox) =>
        t match {
          case RefEmpty => (RefEmpty, true)

          case UpperBounded(_: ArrayType, _) =>
            // In case of arrays it's hard to discuss about their bits, so we make conservative assumption.
            (t, false)

          case UpperBounded(root, _) if !CHA.isKnownType(root) =>
            (t, false)

          case UpperBounded(root, _) =>
            val typeLevelLimit = t match {
              case TypeClosedClass(_, _, _, maxLevel) => maxLevel
              case _ => ClosedCone.MaxLevel
            }
            val givenLevelLimit = levelLimitOpt getOrElse ClosedCone.MaxLevel
            val levelLimit = Math.min(typeLevelLimit, givenLevelLimit)

            greatestCommonRootForNonAbstractSubClasses(root, levelLimit) match {
              case None =>
                (RefEmpty, true)

              case Some(subRoot) if !CHA.isKnownType(subRoot) =>
                (t, false)

              case Some(subRoot) =>
                val strict = root >= subRoot
                (ClosedCone.withMaxLevel(subRoot, mayBeNull = false, levelLimit), strict)
            }

          case _ => shouldNotReachHere(t)
        }
      }
    }

    /** Returns common super class of all non-abstract classes from given closed cone passing given `classCheck`.
      * Returns none if there is no such classes.
      * It is assumed that `classCheck` must be always propagated to subclasses.
      */
    private[References] def greatestCommonRootForNonAbstractSubClasses(root: ReferenceType,
                                                                       levelLimit: Int,
                                                                       classCheck: ClassType => Boolean = _ => true): Option[ClassType] = {
      assert(levelLimit >= 0)

      def find(subRoot: ClassType): Option[ClassType] = {
        if (subRoot.cohenLevel > levelLimit) {
          None
        } else if (!subRoot.isAbstract && classCheck(subRoot)) {
          Some(subRoot)
        } else {
          (CHA.subClasses(subRoot).iterator flatMap find take 2).toList match {
            case Seq() => None
            case Seq(x) => Some(x)
            case _ => Some(subRoot)
          }
        }
      }

      root match {
        case root @ LanguageRoot(_) =>
          // Cones of these classes should not be analyzed due to compilation time degradation concerns (see JET-12584).
          Option.when(root.cohenLevel <= levelLimit)(root)
          
        case root: ClassType =>
          if (root == ReferenceType.ajLangAJObject || root == ReferenceType.ajLangLockableAJObject || root == ReferenceType.javaLangObject) {
            // Cones of these classes should not be analyzed due to compilation time degradation concerns (see JET-12584).
            Option.when(root.cohenLevel <= levelLimit)(root)
          } else {
            find(root)
          }

        case root: InterfaceType =>
          (CHA.implClasses(root).iterator flatMap find).toList match {
            case Seq() => None
            case Seq(x) => Some(x)
            case xs => Some(xs reduce (_ commonSuper _))
          }

        case _: ArrayType => shouldNotReachHere(root)
      }
    }
  }

}
