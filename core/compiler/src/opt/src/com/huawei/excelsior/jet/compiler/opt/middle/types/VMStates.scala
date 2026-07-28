/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.Approximation

trait VMStates { self: Universe =>

  /** Returns `true` if `klass` has no clinit or its initialization has finished or in progress. */
  def isClassClinitedAt(klass: SymClassType, point: ControlNode) =
    getVMStateApprox(point) exists (_.isClinited(klass))

  private def getVMStateApprox(point: ControlNode): Option[VMStateApprox] =
    ContextTypesMap.getContextTypeAt(JVMState(), point) map (_.asInstanceOf[VMStateApprox])


  object TypeState {
    enum Kind {
      case CLINITED, PREPARED, UNKNOWN
    }

    val empty = TypeState(Kind.UNKNOWN)
  }

  private def lesserKind(x: TypeState.Kind, y: TypeState.Kind): Boolean = x.ordinal < y.ordinal
  private def maxKind(x: TypeState.Kind, y: TypeState.Kind): TypeState.Kind = if (lesserKind(y, x)) x else y
  private def minKind(x: TypeState.Kind, y: TypeState.Kind): TypeState.Kind = if (lesserKind(y, x)) y else x

  case class TypeState(kind: TypeState.Kind) {
    def withPreparation(): TypeState =
      TypeState(minKind(kind, TypeState.Kind.PREPARED))

    def withClinit(): TypeState =
      TypeState(minKind(kind, TypeState.Kind.CLINITED))

    def >=(that: TypeState): Boolean =
      !lesserKind(this.kind, that.kind)

    def union(that: TypeState): TypeState =
      TypeState(maxKind(this.kind, that.kind))

    def intersect(that: TypeState): TypeState =
      TypeState(minKind(this.kind, that.kind))

    def isPrepared: Boolean = !lesserKind(TypeState.Kind.PREPARED, kind)
    def isClinited: Boolean = !lesserKind(TypeState.Kind.CLINITED, kind)
  }


  case class VMStateApprox(states: Map[SymType, TypeState]) extends Approximation {

    private def types: Iterator[SymType] = states.keysIterator

    private def stateOf(tpe: SymType): TypeState = states.getOrElse(tpe, TypeState.empty)

    private def mapState(tpe: SymType, map: TypeState => TypeState): VMStateApprox = {
      val oldState = stateOf(tpe)
      val newState = map(oldState)

      if (oldState == newState) {
        this
      } else if (newState == TypeState.empty) {
        VMStateApprox(states - tpe)
      } else {
        VMStateApprox(states + (tpe -> newState))
      }
    }

    private def foldUpdate(types: IterableOnce[SymType], update: TypeState => TypeState): VMStateApprox = {
      types.iterator.foldLeft(this)((vmt, tpe) => vmt.mapState(tpe, update))
    }


    // Public VMStateApprox API

    def this() = this(Map.empty)

    def withPreparation(tpe: SymType): VMStateApprox = {
      assert(!tpe.isDeferred)
      def dependentTypes(tpe: SymType): Iterator[SymType] = {
        if (tpe.isPrimitive) {
          Iterator.single(typeProvider.getObjectType)

        } else if (tpe.isJBCArray) {
          def arrayInterfaces = if (tpe.isJavaArray) {
            Iterator(
              typeProvider.getSerializableType,
              typeProvider.getCloneableType
            )
          } else Iterator.empty

          // Note: might contain duplicates!
          Iterator.single(tpe) ++
            tpe.getCohenSupertypes ++
            arrayInterfaces ++
            dependentTypes(tpe.getArrayElemType.symType)

        } else if (tpe.isInfectedAJClass) {
          Iterator.single(tpe)

        } else {
          Iterator.single(tpe) ++
            tpe.getCohenSupertypes ++
            asClassType(tpe).allSuperInterfaces
        }
      }

      val toPrepare = dependentTypes(tpe) filter { t =>
        // TODO: better check bootstrap prepared types
        lazy val isBootstrapPreparedType =
          t == typeProvider.getAJObjectType || t == typeProvider.getLockableAJObjectType || t == typeProvider.getObjectType

        lazy val isTurboClinitedInRootMethod =
          t.isClass && t.isTurboClinitedIn(rootMethod)

        t.preparationRequired && !isStandalone && !isBootstrapPreparedType && !isTurboClinitedInRootMethod
      }

      foldUpdate(toPrepare, _.withPreparation())
    }

    def withClinit(tpe: SymClassType): VMStateApprox = {
      assert(!tpe.isDeferred)
      // 1. Fast path for a lot of types
      if (tpe.isPreClinited || tpe.isTurboClinitedIn(rootMethod)) return this

      // 2. Clinit for `tpe` will provoke preparation of it and for all it's super types, no matter if they
      // do not have clinit or if they are interfaces without default methods.
      val withPrepared = withPreparation(tpe)

      // 3. Clinit itself
      val dependentTypes = Iterator.single(tpe) ++
        tpe.getSuperClasses ++
        (tpe.allSuperInterfaces filter (_.getDeclaredMethods exists (m => !m.isAbstract && !m.isStatic)))

      val dependentTypesToClinit = dependentTypes
        .filterNot(t => t.isPreClinited || t.isTurboClinitedIn(rootMethod)) // filter out types without clinit and turbo clinited

      withPrepared.foldUpdate(dependentTypesToClinit, _.withClinit())
    }

    def isPrepared(tpe: SymType): Boolean = tpe.isTurboClinitedIn(rootMethod) || stateOf(tpe).isPrepared
    def isClinited(tpe: SymType): Boolean = tpe.isPreClinited || tpe.isTurboClinitedIn(rootMethod) || stateOf(tpe).isClinited


    // Public ContextableType API

    override final def compare(that: Approximation): CC = that match {
      case _ if that eq this => CC.Equal

      case that: VMStateApprox =>
        val greaterOrEqual = this.types forall { t => this.stateOf(t) >= that.stateOf(t) }
        val lessOrEqual    = that.types forall { t => that.stateOf(t) >= this.stateOf(t) }

        (greaterOrEqual, lessOrEqual) match {
          case (true, true)   => CC.Equal
          case (true, false)  => CC.Greater
          case (false, true)  => CC.Less

          case _ =>
            // Each two non-empty VMStateApprox approximations have at least one element in the intersection - JVM with all types clinited.
            CC.PartiallyEqual
        }

      case _ => shouldNotReachHere()
    }

    override def union(that: Approximation): Approximation = that match {
      case that: VMStateApprox => this.types.foldLeft(this)((vmt, tpe) =>
        vmt.mapState(tpe, _ union that.stateOf(tpe))
      )
      case _ => shouldNotReachHere()
    }

    override def intersect(that: Approximation): Approximation = that match {
      case that: VMStateApprox => that.types.foldLeft(this)((vmt, tpe) =>
        vmt.mapState(tpe, _ intersect that.stateOf(tpe))
      )
      case _ => shouldNotReachHere()
    }

    override def isEmpty = false
  }
}
