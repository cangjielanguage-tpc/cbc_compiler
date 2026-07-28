/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.tools

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.isWorkMode
import com.huawei.excelsior.jet.compiler.o2lib.fe.{NumerateModule, pc, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.{ClassID, AttrAPIModule as AttrAPI, CacheAPIModule as CacheAPI, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.SymLevelObject

object ExportIds {
  private lazy val optReexport = env.config.option("reexport")

  private[tools] val NONE_ID: Int = -2
  val INVALID_ID: Int = -1
  private val TYPEHANDLE_ID: Int = 0 // export ID of any TypeHandle is always = 0
  private val RTTI_ID: Int = 1
  private val THINTYPEHANDLE_ID: Int = 2
  private val INSTDESC_ID: Int = 3
  private val SINGLOBJECT_ID: Int = 4

  def getExportID(o: pc.Symbol): Int = {
    o match {
      case o: pcO.Member =>
        getExportIDForMember(o)
      case _: pc.DataSymbol.InstanceDescriptor =>
        INSTDESC_ID
      case _: pc.DataSymbol.RunTimeTypeInfo =>
        RTTI_ID
      case _: pc.DataSymbol.SingletonObject =>
        SINGLOBJECT_ID
      case _: pc.DataSymbol.ThinTypeHandle =>
        THINTYPEHANDLE_ID
      case _: pc.DataSymbol.TypeHandle =>
        TYPEHANDLE_ID
      case _ =>
        throw new AssertionError
    }
  }

  def memberExportID(o: symlevel.Member): Int = o match {
    case o: SymLevelObject => getExportIDForMember(o.o2object.asInstanceOf[pcO.Member])
  }

  def getExportIDForMember(o: pcO.Member): Int = {
    var id = getExportID0(o)
    if (id == INVALID_ID) {
      calculateExportIDs(o.getDeclaringClass)
      id = getExportID0(o)
      assert(id != INVALID_ID)
    } else if (isWorkMode) {
      val tbl = o.getDeclaringClass.getStringTable
      if (tbl != null) {
        assert(getExportID0(tbl) != INVALID_ID)
      }
    }
    id
  }

  private def getExportID0(m: pcO.Member): Int = {
    // TODO: consider using ordinary intercomponent Java import/export for RT procs
    if (CacheAPI.isThisClass(m.getDeclaringClass, ClassID.CompilerInterface)) {
      return NONE_ID
    }
    m.exportID
  }

  private def setExportID(m: pcO.Member, id: Int): Unit = {
    if (isWorkMode) {
      assert(getExportID0(m) == INVALID_ID)
    }
    m.exportID = id
  }

  // prerequisites: methods & fields should be sorted by alphanumeric order
  private def calculateExportIDs(c: pcO.Class): Unit = {
    if (!c.isVerifiable) {
      return
    }

    val descID = if (!c.hasTypeHandle) {
      INVALID_ID
    } else if (!c.hasInstanceDescriptor) {
      // If type has TypeHandle it also has RTTI. So in this case we need to add additional +1 to descID.
      if (c.hasThinTD) THINTYPEHANDLE_ID else RTTI_ID
    } else if (!c.isSingletonObject) {
      INSTDESC_ID
    } else {
      SINGLOBJECT_ID
    }

    var ID = descID + 1

    NumerateModule.checkMethodOrder(c)
    NumerateModule.checkFieldOrder(c)

    for (m <- c.declaredMethods) {
      if (m.shouldBeGenerated && !m.isClinit && (!m.isExternal || optReexport || m.isExported && m.getExportedName == null)) {
        setExportID(m, ID)
        ID += 1
      } else {
        setExportID(m, NONE_ID)
      }
    }

    for (case f: pcO.StaticField <- c.declaredFields) {
      if (f.shouldBeGenerated && (!f.isExternal || optReexport || f.isExported)) {
        setExportID(f, ID)
        ID += 1
      } else {
        setExportID(f, NONE_ID)
      }
    }

    val tbl = c.getStringTable
    if (tbl != null) {
      if (!c.hasMetaInformation) {
        ID = NONE_ID
      }
      val oldID = getExportID0(tbl)
      if (oldID == INVALID_ID) {
        setExportID(tbl, ID)
      } else {
        assert(oldID == ID)
      }
    }
  }

}
