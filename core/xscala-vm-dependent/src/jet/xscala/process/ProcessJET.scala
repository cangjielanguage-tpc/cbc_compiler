/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.process

import xscala.internal.*
import xscala.process.ProcessJET.*

import scala.annotation.static

private[xscala] final class ProcessJET extends ProcessVMDependent {
  override def start(command: Array[String]): Object = wrapForeign(start0(command))
  override def getOutputStream(process: Object): Object = wrapForeign(getOutputStream0(unwrapForeign(process)))
  override def getInputStream(process: Object): Object = wrapForeign(getInputStream0(unwrapForeign(process)))
  override def getErrorStream(process: Object): Object = wrapForeign(getErrorStream0(unwrapForeign(process)))
  override def waitFor(process: Object): Int = waitFor0(unwrapForeign(process))
}

private object ProcessJET {
  @native @static private def start0(command: Array[String]): ForeignRef0
  @native @static private def getOutputStream0(process: ForeignRef0): ForeignRef0
  @native @static private def getInputStream0(process: ForeignRef0): ForeignRef0
  @native @static private def getErrorStream0(process: ForeignRef0): ForeignRef0
  @native @static private def waitFor0(process: ForeignRef0): Int
}
