/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.process

private[xscala] final class ProcessJDK extends ProcessVMDependent {
  override def start(command: Array[String]): Object = {
    val pb = new java.lang.ProcessBuilder(java.util.Arrays.asList(command:_*))
    pb.start()
  }

  override def getOutputStream(process: Object): Object = asJava(process).getOutputStream
  override def getInputStream(process: Object): Object = asJava(process).getInputStream
  override def getErrorStream(process: Object): Object = asJava(process).getErrorStream
  override def waitFor(process: Object): Int = asJava(process).waitFor()

  private inline def asJava(o: Object): java.lang.Process = o.asInstanceOf[java.lang.Process]
}
