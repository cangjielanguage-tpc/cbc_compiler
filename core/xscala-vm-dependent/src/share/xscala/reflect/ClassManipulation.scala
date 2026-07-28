/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.reflect

import xscala.reflect.ClassManipulation.*
import xscala.util.Feature

import scala.language.implicitConversions

trait ClassManipulation {
  def getClass(name: String): Class
  def getConstructor(klazz: Class)(args: Class*): Constructor
  def newInstance(constr: Constructor)(args: Any*): AnyRef
  def newInstance(klazz: Class): AnyRef
}

object ClassManipulation extends Feature[ClassManipulation] {
  opaque type Class = java.lang.Class[?]
  opaque type Constructor = AnyRef

  def withContext[T](action: ClassManipulation ?=> T): Option[T] = get map (action(using _))
  inline def context(using classManipulation: ClassManipulation): ClassManipulation = classManipulation

  extension (cls: Class) {
    inline def getConstructor(args: Class*)(using classManipulation: ClassManipulation): Constructor =
      classManipulation.getConstructor(cls)(args*)

    inline def newInstance()(using classManipulation: ClassManipulation): AnyRef =
      classManipulation.newInstance(cls)
  }

  extension (constr: Constructor) {
    inline def newInstance(args: Any*)(using classManipulation: ClassManipulation): Constructor =
      classManipulation.newInstance(constr)(args*)
  }

  implicit inline def reflectClassToManipulationClass(cls: java.lang.Class[?]): Class = cls
}
