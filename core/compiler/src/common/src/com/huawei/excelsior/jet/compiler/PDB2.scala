/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.compiler.symlevel.ClassType
import xscala.io.{DataInput, DataOutput, Path}

import PDB2.*

object PDB2 {
  sealed abstract class EntryKind private(val ext: String)

  object EntryKind {
    final class Project(_ext: String) extends EntryKind(_ext) { // one entry per project
      def loc(itemID: String) = new Location(this, null, itemID)
    }

    final class Class(_ext: String) extends EntryKind(_ext) { // one entry per class
      def loc(cls: ClassType) = new Location(this, cls, null)
    }

    final class ClassItem(_ext: String) extends EntryKind(_ext) { // several local entries for a class
      def loc(contextClass: ClassType, itemID: String) = new Location(this, contextClass, itemID)
    }

    val IR               = new ClassItem("irb")
    val ExtraInfo        = new ClassItem("irei")
    val ModuleInfo       = new Project("mod")
    val DelayedUsage     = new Project("set")
    val Repl             = new Project("pdb")
  }


  final class Location private[PDB2](val kind: EntryKind, val contextClass: ClassType, val itemID: String) {
      kind match {
        case _: EntryKind.Project   => assert(contextClass == null && itemID != null)
        case _: EntryKind.Class     => assert(contextClass != null && itemID == null)
        case _: EntryKind.ClassItem => assert(contextClass != null && itemID != null)
      }

      def name: String = {
        if (contextClass == null) { // EntryKind.Project
          return itemID
        }
        assert(!contextClass.isDeferred);
        val name = contextClass.getMangledName
        if (itemID != null) { // EntryKind.ClassItem
          /* Project system has an internal assumption that entry names for .irb files
           * are class names + File.separator + some suffix (to reclaim old entries from PDB).
           * So if it is changed the changes must be reflected in xcMain.ResourceCleanupAdviser.
           */
          s"$name/$itemID"
        } else name // EntryKind.Class
      }

      def fullName: String = s"$name.${kind.ext}"

      override def toString() = fullName
  }
}

abstract class PDB2 {
  /**
   * Returns resource from PDB.
   * @return `DataInput` for specified resource location or null if such resource does not exist
   */
  def getDataInputOrNull(loc: Location): DataInput

  /**
   * Returns `DataOutput` to write into current PDB for specified resource location.
   * @throws UnsupportedOperationException if opening previously opened stream
   */
  def getDataOutput(loc: Location): DataOutput

  /** Returns `true` if specified resource location exists. */
  def exists(loc: Location): Boolean

  def getFile(name: String): Path
}
