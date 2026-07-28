/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.reflect

import xscala.reflect.ClassManipulation.{Class, Constructor}

import scala.annotation.nowarn

private[xscala] final class ClassManipulationJDK extends ClassManipulation {
  override def getClass(name: String): Class = java.lang.Class.forName(name)

  override def getConstructor(klazz: Class)(args: Class*): Constructor =
    klazz.asInstanceOf[java.lang.Class[?]].getConstructor(args.map(_.asInstanceOf[java.lang.Class[?]])*).asInstanceOf[Constructor]

  override def newInstance(constr: Constructor)(args: Any*): AnyRef =
    constr.asInstanceOf[java.lang.reflect.Constructor[?]].newInstance(args*)

  // TODO: Following warning happens only when building compiler via newer versions of IDEA
  //       (observed on 2024.1.4 with the Scala plugin of version 2024.1.25), possibly because
  //       the actual module-level JDK is ignored or shadowed by the one used by Scala compiler
  //       server. If possible, investigate, report the issue and remove this workaround.
  @nowarn("msg=method newInstance in class Class is deprecated since")
  override def newInstance(klazz: Class): AnyRef = klazz.asInstanceOf[java.lang.Class[?]].newInstance()
}
