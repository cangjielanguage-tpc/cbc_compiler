/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule as pcO
import com.huawei.excelsior.jet.compiler.o2lib.u.{ManifestModule as man, xiFilesModule as xfs, xmZipModule as xmZip}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS

import scala.collection.mutable

object GetPackageSupportModule {

  class PackageInfo {
    var name: XString = _
    var jar: XString = _
    var specTitle: XString = _
    var specVersion: XString = _
    var specVendor: XString = _
    var implTitle: XString = _
    var implVersion: XString = _
    var implVendor: XString = _
    var sealed0: Boolean = _
  }

  private var lastKey: (XString, XString) = _
  private var lastPackInfo: PackageInfo = _
  private val packInfos = mutable.HashMap.empty[(XString, XString), PackageInfo]

  private def getPackInfo(jar: XString, pkg: XString): PackageInfo = {
    val pi = new PackageInfo()
    pi.name = pkg
    if (pcO.isIdea) {
      // com.intellij.util.lang.UrlClassLoader does not read package info from jars and
      // defines packages containing only their name for all classes.
      return pi
    }
    val m = man.getManifest(jar)
    if (m == null) {
      return null
    }
    var attr = m.getAttributes(pkg)
    var sealed0: XString = null
    if (attr != null) {
      pi.specTitle = attr.getValue(man.SPECIFICATION_TITLE)
      pi.specVersion = attr.getValue(man.SPECIFICATION_VERSION)
      pi.specVendor = attr.getValue(man.SPECIFICATION_VENDOR)
      pi.implTitle = attr.getValue(man.IMPLEMENTATION_TITLE)
      pi.implVersion = attr.getValue(man.IMPLEMENTATION_VERSION)
      pi.implVendor = attr.getValue(man.IMPLEMENTATION_VENDOR)
      sealed0 = attr.getValue(man.SEALED)
    }
    attr = m.getMainAttributes
    if (attr != null) {
      if (pi.specTitle == null) {
        pi.specTitle = attr.getValue(man.SPECIFICATION_TITLE)
      }
      if (pi.specVersion == null) {
        pi.specVersion = attr.getValue(man.SPECIFICATION_VERSION)
      }
      if (pi.specVendor == null) {
        pi.specVendor = attr.getValue(man.SPECIFICATION_VENDOR)
      }
      if (pi.implTitle == null) {
        pi.implTitle = attr.getValue(man.IMPLEMENTATION_TITLE)
      }
      if (pi.implVersion == null) {
        pi.implVersion = attr.getValue(man.IMPLEMENTATION_VERSION)
      }
      if (pi.implVendor == null) {
        pi.implVendor = attr.getValue(man.IMPLEMENTATION_VENDOR)
      }
      if (sealed0 == null) {
        sealed0 = attr.getValue(man.SEALED)
      }
    }
    if (sealed0 != null) {
      sealed0 = sealed0.toUpperCase
    }
    pi.sealed0 = sealed0 != null && sealed0.equals2("TRUE")

    pi.jar = FS.cutPath(FS.HOST.fromPlatform(jar))
    pi
  }

  def getJarName(cls: pcO.Class): XString = cls.fileDescriptor match {
    case fd: xmZip.FileDescriptor => fd.zname
    case _ => null
  }

  def getPackageInfo(cls: pcO.Class): PackageInfo = {
    assert(!cls.isSystemClass)
    val name = cls.name
    val pos = name.lastIndexOf('/')
    val jar = getJarName(cls)
    if (jar == null || pos == -1) {
      return null
    }
    val pkg = name.substring(0, pos + 1)
    val key = (jar, pkg)
    if (key != lastKey) {
      lastKey = key
      lastPackInfo = packInfos.getOrElseUpdate(key, getPackInfo(jar, pkg))
    }
    lastPackInfo
  }
}
