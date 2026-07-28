/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.properties

private[xscala] final class PropertiesJDK extends Properties {
  def userDir(): String   = System.getProperty("user.dir")
  def osName(): String    = System.getProperty("os.name")
  def arch(): String      = System.getProperty("os.arch")
  def tmpDir(): String    = System.getProperty("java.io.tmpdir")
  def jetExeDir(): String = System.getProperty("jet.exe.dir")
  def userHome(): String  = System.getProperty("user.home")
}
