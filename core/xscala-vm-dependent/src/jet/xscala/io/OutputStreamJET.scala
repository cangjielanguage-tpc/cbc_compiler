/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.internal.*
import xscala.io.OutputStreamJET.*

import scala.annotation.static

private[xscala] final class OutputStreamJET extends OutputStreamVMDependent {
  override def getStdout(): Object = wrapForeign(getStdout0())
  override def getStderr(): Object = wrapForeign(getStderr0())
  override def writeByte(stream: Object, b: Int): Unit = writeByte0(unwrapForeign(stream), b)
  override def close(stream: Object): Unit = close0(unwrapForeign(stream))
  override def flush(stream: Object): Unit = {} // TODO: for now our out-stream writes directly, fix it when it changes
}

private object OutputStreamJET {
  @native @static private def getStdout0(): ForeignRef0
  @native @static private def getStderr0(): ForeignRef0
  @native @static private def writeByte0(stream: ForeignRef0, b: Int): Unit
  @native @static private def close0(stream: ForeignRef0): Unit
}
