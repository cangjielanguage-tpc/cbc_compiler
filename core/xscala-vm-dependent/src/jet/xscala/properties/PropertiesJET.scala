/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.properties

import xscala.properties.PropertiesJET.*

import scala.annotation.static

private[xscala] final class PropertiesJET extends Properties {
  def userDir(): String = userDir0()
  def osName(): String = osName0()
  def arch(): String = arch0()
  def tmpDir(): String = tmpDir0()
  def jetExeDir(): String = jetExeDir0()
  def userHome(): String = userHome0()
}

private object PropertiesJET {
  @native @static private def userDir0(): String
  @native @static private def osName0(): String
  @native @static private def arch0(): String
  @native @static private def tmpDir0(): String
  @native @static private def jetExeDir0(): String
  @native @static private def userHome0(): String
}
