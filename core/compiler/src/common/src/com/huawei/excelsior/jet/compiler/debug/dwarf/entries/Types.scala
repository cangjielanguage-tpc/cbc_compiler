/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.entries

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Label
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.debug.info.{DTArray, DTClass, DTCompound, DTConst, DTCustom, DTEnumeration, DTInterface, DTOption, DTPayloadEnumHeir, DTPayloadEnumeration, DTPointer, DTRecord, DTStaticField, DebugType}
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind
import com.huawei.excelsior.jet.util.Worklist

import scala.annotation.tailrec
import scala.collection.mutable

/** Collection of types, referenced in one compilation unit.
  *
  * @author conwor
  * @author gatimosh
  * @author orangebyte256
  */
object Types {
  @tailrec def isDeref(tpe: DebugType): Boolean = tpe match {
    case dt: DTStaticField => isDeref(dt.baseType)
    case DTConst(baseType) => isDeref(baseType)
    case dt: DTOption => isDeref(dt.payload)
    case _: DTRecord | _: DTCustom | _: DTEnumeration | _: DTPayloadEnumHeir => false
    case _: DTPayloadEnumeration | _: DTArray | _: DTClass | _: DTPointer | _: DTInterface => true
  }

  def pointerWrapper(tpe: DebugType): DebugType = tpe match {
    case _: DTClass | _: DTPayloadEnumeration => DTPointer(tpe)
    case _ => tpe // TODO-DWARF why DTArray is not wrapped here?
  }
}

abstract class Types {
  private val cache = new mutable.LinkedHashMap[DebugType, Label]
  private val toEmit = Worklist.empty[(DebugType, Label)]

  private val staticFieldLabels = new mutable.LinkedHashMap[XString, Label]

  private val typesInsideNamespace = new Dwarf.Entry()
  private val typesOutsideNamespace = new Dwarf.Entry()
  private def entryForType(`type`: DebugType): Dwarf.Entry = `type` match {
    case tpe: DebugType if isOutsideNamespaceType(tpe) => typesOutsideNamespace
    case _ => typesInsideNamespace
  }

  val toPubnames = new mutable.LinkedHashSet[DebugType]

  // some types for some languages may require to be declared inside a compilation unit namespace
  def isOutsideNamespaceType(`type`: DebugType) = true

  def label(`type`: DebugType): Label = `type` match {
    case _ => cache.getOrElseUpdate(`type`, {
      val entry = entryForType(`type`)
      val resLabel = entry.newLabel
      toEmit.append((`type`, resLabel))
      resLabel
    })
  }

  def staticFieldLabel(name: XString, scope: DebugType) = {
    label(scope) // ensure static field scope is emitted inside current compilation unit
    staticFieldLabels.getOrElseUpdate(name, entryForType(scope).newLabel)
  }

  def label(kind: TypeKind): Label = label(DTCustom(kind))

  protected def emitType(`type`: DebugType, entry: Dwarf.Entry): Unit

  def finish(): (Dwarf.Entry, Dwarf.Entry) = {
    for ((tpe, label) <- toEmit.drain) {
      val typesEntry = if (isOutsideNamespaceType(tpe)) typesOutsideNamespace else typesInsideNamespace
      typesEntry.bind(label)
      emitType(tpe, typesEntry)
    }
    (typesInsideNamespace, typesOutsideNamespace)
  }
}