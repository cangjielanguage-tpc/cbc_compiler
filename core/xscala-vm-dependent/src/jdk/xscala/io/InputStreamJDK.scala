/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

private[xscala] final class InputStreamJDK extends InputStreamVMDependent {
  override def getStdin(): Object = java.lang.System.in

  override def readByte(stream: Object): Int = asJava(stream).read()
  override def bytesAvailable(stream: Object): Int = asJava(stream).available()
  override def close(stream: Object): Unit = asJava(stream).close()

  private inline def asJava(o: Object): java.io.InputStream = o.asInstanceOf[java.io.InputStream]
}
