/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.{Environment, Stage, symlevel}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CHA {

  private var initialized: Boolean = false
  private var _subClasses: collection.Map[ClassType, collection.Seq[ClassType]] = _
  private var _maxHeight: collection.Map[ClassType, Int] = _
  private var _implClasses: collection.Map[InterfaceType, collection.Seq[ClassType]] = _

  private var env: Environment = _

  /** Returns whether CHA is aware of the given type. */
  def isKnownType(tpe: ReferenceType): Boolean = {
    tpe match {
      case tpe: ClassType => _subClasses contains tpe
      case tpe: InterfaceType => _implClasses contains tpe
      case _ => false
    }
  }

  def isClosed(tpe: ReferenceType): Boolean = {
    tpe match {
      // Note: even in closed world these interface supertypes cannot be closed,
      //       because we don't have interface closed cones.
      // TODO: support interface closed cones and close following classes in closed world.
      case ReferenceType.ajLangAJObject | ReferenceType.ajLangLockableAJObject | ReferenceType.javaLangObject => false

      // Do not close AJ hierarchy root.
      case ReferenceType.LanguageRoot(_) => false

      // Cangjie library should not be treated as closed because its object files are reused as-is.
      case _ if env.enabled(GenLibrary) || env.enabled(GenCbcStdLib) => false

      // Do not close scala standard library
      // TODO: We check interface supertype xscala.AnyRef only when compiling with Scala language pack,
      //       since it is absent under any other LP. However, this check should be presented more clearly.
      case _ if tpe.symType.isXScalaType && (env.enabled(BuildXKRN) || tpe == ReferenceType.xscalaAnyRef || tpe == ReferenceType.scalaRefType) => false

      case _ => env.enabled(ClosedWorld) || tpe.symType.isAJManagedType || tpe.symType.isThinClass || tpe.symType.isCangjieType || tpe.symType.isXScalaType
    }
  }

  /** Returns direct subclasses of the given class. */
  def subClasses[CT <: ClassType](klass: CT): collection.Seq[CT] = {
    assert(initialized)
    _subClasses(klass).asInstanceOf[collection.Seq[CT]]
  }

  /** Returns distance to the deepest inheritor of the given class, i.e. 1 if there is no inheritors. */
  def maxClassHeight(klass: ClassType): Int = {
    assert(initialized)
    _maxHeight(klass)
  }

  /** Returns level of the deepest inheritor of the given class, i.e. `klass.cohenLevel` if there is no inheritors. */
  def maxClassLevel(klass: ClassType): Int = {
    assert(initialized)
    _maxHeight(klass) + klass.cohenLevel - 1
  }

  /** Returns direct implementators of the given interface (classes explicitly re-implementing interface are not included). */
  def implClasses(interface: InterfaceType): collection.Seq[ClassType] = {
    assert(initialized)
    _implClasses(interface)
  }

  /** For unit-tests. */
  private[types] def reset(env: Environment): Unit = {
    initialized = false
    _subClasses = null
    _implClasses = null
    _maxHeight = null
    init(env)
  }

  def init(env: Environment): Unit = {
    assert(!initialized)

    this.env = env

    env.stage(Stage.Cha) {

      assert(_subClasses == null && _implClasses == null && _maxHeight == null)

      // CHA disabled for Cangjie classes outside of compilation set because their subtypes may be loaded to project
      // system after CHA tables built (except for PGO for some reason).
      def workaroundForJET16671(t: symlevel.ClassType): Boolean =
        env.enabled(PGO) || !t.isCangjieType || t.isInCurrentCompilationSet

      def goodForCha (t: symlevel.ClassType): Boolean = {
        if (t.isDeferred || t.isErroneous || t.isSynthetic || t.hasDeferredSuper || t.isCangjiePackage) {
          false
        } else {
          t.isThinClass || //JET-17606
            (ProjectLogic.isCHAEnabled && t.isTraceableReference && env.isCHAEnabledForTraceableReferences && workaroundForJET16671(t))
        }
      }

      // collect all classes and interfaces available for CHA
      val goodClassesAndInterfaces = env.getTypeProvider.getAllClasses
        .filter (goodForCha)
        .collect {
          case t if t.isClass => ClassType(t)
          case t if t.isInterface => InterfaceType(t)
        }

      // these maps will contain immutable empty list for classes/interfaces without subclasses/implementations
      // and mutable buffer if there is at least one subclass/implementation
      val subClasses = new mutable.HashMap[ClassType, collection.Seq[ClassType]]
      val implClasses = new mutable.HashMap[InterfaceType, collection.Seq[ClassType]]

      goodClassesAndInterfaces foreach {
        case t: ClassType => subClasses(t) = Nil
        case t: InterfaceType => implClasses(t) = Nil
      }

      // populate them with all direct subclasses/implementations
      for (klass <- subClasses.keys.toList) {
        val superClass = klass.superclass
        if ((superClass != null) && (subClasses contains superClass)) {
          addToClasses(subClasses, superClass, klass)
        }

        for (superInterface <- klass.symType.allSuperInterfacesSigs) {
          val interfaceType = InterfaceType(superInterface)
          if ((implClasses contains interfaceType) && (superClass == null || !(superClass implements superInterface))) {
            addToClasses(implClasses, interfaceType, klass)
          }
        }
      }

      // calculate max inheritors height for classes
      val maxHeight = new mutable.HashMap[ClassType, Int]

      def calcHeight(superClass: ClassType): Int = {
        maxHeight.getOrElseUpdate(superClass, 1 + subClasses(superClass).map(calcHeight).fold(0)(Math.max))
      }

      for (superClass <- subClasses.keys if !(maxHeight contains superClass)) {
        maxHeight(superClass) = calcHeight(superClass)
      }

      _subClasses = subClasses
      _implClasses = implClasses
      _maxHeight = maxHeight

    }

    initialized = true
  }

  private def addToClasses[RT <: ReferenceType, CT <: ClassType](subsMap: mutable.HashMap[RT, collection.Seq[CT]], superType: RT, klass: CT): Unit = {
    val classesBuffer = (subsMap get superType) match {
      case None =>
        shouldNotReachHere(s"class $klass has supertype $superType which is not in CHA")

      case Some(Nil) =>
        val buffer = new ArrayBuffer[CT](8)
        subsMap(superType) = buffer
        buffer

      case Some(buffer) =>
        buffer.asInstanceOf[mutable.Buffer[CT]]
    }

    classesBuffer += klass
  }

}
