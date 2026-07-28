/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

private[xscala] final class OutputStreamJDK extends OutputStreamVMDependent {
  override def getStdout(): Object = java.lang.System.out
  override def getStderr(): Object = java.lang.System.err
  override def writeByte(stream: Object, b: Int): Unit = { asJava(stream).write(b); asJava(stream).flush() }
  override def close(stream: Object): Unit = asJava(stream).close()
  override def flush(stream: Object): Unit = asJava(stream).flush()

  private inline def asJava(o: Object): java.io.OutputStream = o.asInstanceOf[java.io.OutputStream]
}
