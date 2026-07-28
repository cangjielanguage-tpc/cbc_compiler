/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.internal.*
import xscala.io.InputStreamJET.*

import scala.annotation.static

private[xscala] final class InputStreamJET extends InputStreamVMDependent {
  override def getStdin(): Object = wrapForeign(getStdin0())
  override def readByte(stream: Object): Int = readByte0(unwrapForeign(stream))
  override def bytesAvailable(stream: Object): Int = bytesAvailable0(unwrapForeign(stream))
  override def close(stream: Object): Unit = close0(unwrapForeign(stream))
}

private object InputStreamJET {
  @native @static private def getStdin0(): ForeignRef0
  @native @static private def readByte0(stream: ForeignRef0): Int
  @native @static private def bytesAvailable0(stream: ForeignRef0): Int
  @native @static private def close0(stream: ForeignRef0): Unit
}
