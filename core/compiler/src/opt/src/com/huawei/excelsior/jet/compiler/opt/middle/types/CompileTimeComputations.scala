/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.jet.compiler.bytecode.NoPosition
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString.ascii
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{InstanceDescriptorSymbol, SignatureType, Type as SymType}
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.condOpt
import scala.annotation.nowarn

trait CompileTimeComputations { self: Universe =>

  def classNativesIntrinsicsEnabled = !env.enabled(BoolOption.DisableClassNativesIntrinsification)

  object GetClassApprox {
    def unapply(n: Node) = condOpt(n) {
      case ClassObject(t) => Exact(t)
      case n @ GetClass(o) => TypeSet(nodeTypeAfter(o, n.inCtrl))
      // TODO case Phi => union over all arguments?
    }

    trait TypeSet {
      /** Returns `Some(() => True())` if every type from `that` is definitely assignable to every type from `this`;
        *         `Some(() => False())` if every type from `that` is definitely not assignable to every type from `this`;
        *         `None` if some types are assignable and some are not.
        */
      def isAssignableFrom(that: TypeSet) = (this, that) match {
        case (Empty, _) | (_, Empty) => Some(() => True())

        case (Exact(l), Exact(r)) => Some(() => ConstCondition(l.symType.isAssignableFrom(r)))
        case (Exact(l), Rooted(r)) if l.symType.isAssignableFrom(r) => Some(() => True())
        case (Rooted(l), Exact(r)) if r.symType.isAssignableFrom(l) => Some(() => False())
        case (Rooted(l), Rooted(r)) if haveCommonSubtypes(l, r) => None
        case _ => Some(() => False())
      }

      /** Returns `Some(True())` if `this` type set contains only arrays;
        *         `Some(False())` if `this` type set contains no arrays;
        *         `None` if `this` type set may contains some arrays, but we cant know for sure.
        */
      def containsOnlyArrays = this match {
        case Empty => Some(() => False())
        case Exact(t) if t.isPrimitive => Some(() => False())
        case WithSubtypes(root) if JavaArrayType.isSupertype(root) => None
        case Rooted(root) => Some(() => ConstCondition(root.isJavaArray))
        case _ => shouldNotReachHere(this)
      }
    }

    object TypeSet {
      @nowarn("msg=match may not be exhaustive")
      def apply(ta: ReferenceApprox): TypeSet = ta match {
        case Point(t, _) => Exact(t.sigType)
        case UpperBounded(t, _) => WithSubtypes(t.sigType)
        case RefNull | RefEmpty => Empty
      }
    }


    case object Empty extends TypeSet

    trait Rooted extends TypeSet {
      def root: SignatureType
    }

    object Rooted {
      def unapply(t: Rooted) = Some(t.root)
    }

    case class Exact(root: SignatureType) extends Rooted

    object Exact {
      def apply(root: SymType): Exact = Exact(SignatureType.fromSymType(root))
    }

    case class WithSubtypes(root: SignatureType) extends Rooted {
      require(!root.isPrimitive)
    }

    object WithSubtypes {
      def apply(root: SymType): WithSubtypes = WithSubtypes(SignatureType.fromSymType(root))
    }


    private def haveCommonSubtypes(t1: SignatureType, t2: SignatureType): Boolean = {
      if (t1.isPrimitive || t2.isPrimitive) {
        t1 == t2
      } else {
        !OpenCone(ReferenceType(t1), mayBeNull = false).incomparable(OpenCone(ReferenceType(t2), mayBeNull = false))
      }
    }
  }

  def computeCompileTime(): Boolean = {
    var changed = false
    for {
      n <- all[CompileTimeOp]
      lazyReplacement <- computeAtCompileTime(n)
    } {
      val replacement = n match {
        case _: IsComputableAtCompileTime => True()
        case _: ComputeAtCompileTime => lazyReplacement()
      }
      replaceTransitively(n, replacement)
      changed = true
    }

    changed
  }

  private def computeAtCompileTime(n: CompileTimeOp): Option[() => Node] = {
    import CompileTimeOp.Kind._

    (n.kind, n.argsSeq) match {
      case (IsNull, Seq(inCtrl: UpperPoint, obj)) =>
        condOpt(nodeTypeAt(obj, inCtrl)) {
          case RefNull => () => True()
          case t if !t.mayBeNull => () => False()
        }

      case (HasTraceableFields, Seq(inCtrl: UpperPoint, obj)) =>
        condOpt(nodeTypeAt(obj, inCtrl)) {
          case UpperBounded(root @ (_: ClassType | _: JavaArrayType | _: AJArrayType), _) if root.symType.hasTraceableFields =>
            () => True()

          // TODO: arbitrary closed cones could be supported if required.
          case Point(root, _) =>
            assert(!root.symType.hasTraceableFields, root.symType)
            () => False()
        }

      case (IsArray, Seq(inCtrl: UpperPoint, obj)) =>
        GetClassApprox.TypeSet(nodeTypeAt(obj, inCtrl)).containsOnlyArrays

      case (GetReferenceArrayElementFormalType, Seq(inCtrl: UpperPoint, obj)) =>
        getRefArrElemFormalTypeNonTrivial(nodeTypeAt(obj, inCtrl)) map (t => () => AJString.bstr(ascii(t.getName)))

      case (GetComponentType, Seq(inCtrl, ClassObject(symType))) => getComponentType(n, inCtrl, symType)

      case (GetArrayDimNum, Seq(_, SymbolAddress(t: InstanceDescriptorSymbol))) =>
        val symType = t.tpe
        Option.when(symType.isArray)(() => IConst(symType.getArrayDimnum))

      case (GetArrayElemLog2Size, Seq(_, SymbolAddress(t: InstanceDescriptorSymbol))) =>
        val symType = t.tpe
        Option.when(symType.isArray)(() => IConst(symType.getArrayElemType.symType.log2Size))

      case (HasPrimitiveArrayBaseType, Seq(_, SymbolAddress(t: InstanceDescriptorSymbol))) =>
        val symType = t.tpe
        Option.when(symType.isArray)(() => ConstCondition(symType.getArrayBase.isPrimitive))

      case (IsArrayClass, Seq(_, GetClassApprox(t))) if classNativesIntrinsicsEnabled => t.containsOnlyArrays

      case (IsInstance, Seq(inCtrl: UpperPoint, GetClassApprox(clsTpe), obj)) if classNativesIntrinsicsEnabled =>
        val objTpe = nodeTypeAfter(obj, inCtrl)
        if (objTpe == RefNull) {
          Some(() => False())
        } else if (objTpe.mayBeNull) {
          None
        } else {
          clsTpe.isAssignableFrom(GetClassApprox.TypeSet(objTpe))
        }

      case (IsAssignable, Seq(_, from, to)) if classNativesIntrinsicsEnabled => (from, to) match {
        case (x, y) if x == y => Some(() => True())
        case (GetClassApprox(from), GetClassApprox(to)) => to.isAssignableFrom(from)
        case _ => None
      }

      case _ => None
    }
  }

  private[types] def getRefArrElemFormalTypeNonTrivial(tpe: ReferenceApprox): Option[SymType] = {
    tpe match {
      case UpperBounded(root: JavaArrayType, _) => root.arrayElement match {
        case ReferenceType.javaLangObject => None // it's the most conservative result, postpone our desicion till better type refinement
        case elemType => Some(elemType.symType)
      }
      case _ => None
    }
  }

  private[types] def getComponentType(n: CompileTimeOp, inCtrl: Node, symType: SymType): Option[() => Node] = {
    if (!symType.isJavaArray) {
      Some(() => Null())
    } else {
      Some(() => withPos(n.pos ensuring (_ != NoPosition)) {
          ClassObject(symType.getArrayElemType.symType)(inCtrl)
      })
    }
  }

  def dgiForCompileTimeComputations = DGIProvider { b =>
    val controlledNodes = (b.points flatMap (_.controlUses)).toArray
    val isComp = controlledNodes exists (_.isInstanceOf[IsComputableAtCompileTime])
    val op = controlledNodes exists (_.isInstanceOf[ComputeAtCompileTime])
    (isComp, op) match {
      case (true,  false) => DGI("IT check",      "green")
      case (false, true ) => DGI("IT op",         "red")
      case (true,  true ) => DGI("IT check + op", "green:red")
      case (false, false) => null
    }
  }
}
