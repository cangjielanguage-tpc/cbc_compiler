/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

/** Class represents version of JET.
  *
  * It contains major version and minor version.
  * Note that minor version should be in range `[0, 99]`.
  *
  * Two main representations exist:
  *
  *   - '''printable version''' - version as string which contains dot-separated major and minor version
  *     (e.g. "1.25", "7.2", "10.5", "11.0").
  *   - '''version code''' - version as integer which contains major version (as number of hundreds)
  *     and minor version (in two lower digits) (e.g. 1125, 720, 1050, 1100).
  */
class JetVersion(val printableVersion: String, val versionCode: Int) {

  override def toString = printableVersion

  override def equals(obj: Any) = obj match {
    case that: AnyRef if this eq that => true
    case that: JetVersion => this.versionCode == that.versionCode
    case _ => false
  }

  override def hashCode = versionCode
}

object JetVersion {

  /** Parses JET version from the printable version.
    *
    * @param version the printable version
    * @return parsed JET version
    * @throws IllegalArgumentException if cannot parse JET version
    */
  def fromPrintableVersion(version: String) = {
    // Converts the version string to integer in the following way:
    //      XX.Y  --> XXY0
    //      XX.YZ --> XXYZ

    val versionParts = version.split("\\.")

    if (versionParts.length != 2) {
      throw new IllegalArgumentException(s"Cannot parse Excelsior JET version: '$version'")
    }

    val majorVer = versionParts(0)
    val minorVer = versionParts(1)

    if (minorVer.length > 2) {
      throw new IllegalArgumentException(s"Cannot parse Excelsior JET version: '$version'")
    }

    val versionCode = new java.lang.StringBuilder
    versionCode.append(majorVer).append(minorVer)
    if (minorVer.length == 1) {
      versionCode.append('0')
    }

    val versionCodeAsInt = versionCode.toString.toInt
    if (versionCodeAsInt < 0) {
      throw new IllegalArgumentException(s"Cannot parse Excelsior JET version: '$version'")
    }
    new JetVersion(version, versionCodeAsInt)
  }

  /** Parses JET version from the version code.
    *
    * @param version the version code
    * @return parsed JET version
    * @throws IllegalArgumentException if cannot parse JET version
    */
  def fromVersionCode(version: Int) = {
    // Converts the version integer to string in the following way:
    //      XXY0 --> XX.Y
    //      XXYZ --> XX.YZ

    if (version < 0) {
      throw new IllegalArgumentException(s"Cannot parse Excelsior JET version code: '$version'")
    }

    val majorVer = version / 100
    val minorVer = version % 100

    if (majorVer == 0) {
      throw new IllegalArgumentException(s"Cannot parse Excelsior JET version code: '$version'")
    }

    val printableVersion = new java.lang.StringBuilder
    printableVersion.append(majorVer).append('.')
    printableVersion.append(minorVer / 10)
    if (minorVer % 10 != 0) {
      printableVersion.append(minorVer % 10)
    }

    new JetVersion(printableVersion.toString, version)
  }
}
