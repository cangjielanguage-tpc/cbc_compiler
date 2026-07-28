/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie

import com.huawei.excelsior.jet.compiler.cangjie.UMLWriter.t2s
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Type}

import scala.collection.mutable

/** Use any UML viewer to view diagram. */
object UMLWriter {
  private def t2s(t: Type): String =
    if (t.isHierarchyRoot) "AJObject" else t.getName.replaceAll("/", ".")
}

class UMLWriter {
  private val written = mutable.HashSet.empty[Type]

  def writeClasses(classes: Iterable[ClassType]): Unit = {
    println("============v= UML DIAGRAM =v============")
    println("@startuml")
    classes foreach writeOne
    println("@enduml")
    println("============^= UML DIAGRAM =^============")
  }

  def writeOne(klass: ClassType): Unit = {
    if (written.contains(klass) || klass.isHierarchyRoot) return
    written.add(klass)

    for (spr <- klass.getDeclaredSuperTypes) {
      writeOne(spr)
    }

    println((if (klass.isInterface) "interface" else "class") + " " + t2s(klass) + " {")

    for (f <- klass.getDeclaredFields) {
      println("  " + f.getType.toJETSignature + " " + f.getName)
    }

    for (m <- klass.getDeclaredMethods) {
      val params = (0 until m.getParamsCount).map(m.getParamType).map(_.toJETSignature).mkString("(", ", ", ")")
      println("  " + m.getReturnType.toJETSignature + " " + m.getName + params)
    }

    println("}")

    for (spr <- klass.getDeclaredSuperTypes if !spr.isHierarchyRoot) {
      println(t2s(klass) + "--|>" + t2s(spr))
    }

    println()
  }
}
