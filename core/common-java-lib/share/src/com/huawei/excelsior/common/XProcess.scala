/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import xscala.io.*
import xscala.process.ProcessVMDependent
import xscala.text.{Encoding, Utf8Encoding}

// TODO: specify proper PlatformEncoding as Encoding for each stream
private val defaultEncoding: Encoding = Utf8Encoding

object XProcess {
  def start(command: scala.collection.Seq[String]): XProcess = new XProcess(ProcessVMDependent.get.start(command.toArray))
}

class XProcess(process: Object) {
  val stdin: TextOutput = TextOutput.wrapHandle(ProcessVMDependent.get.getOutputStream(process), defaultEncoding)
  val stdout: TextInput = TextInput.wrapHandle(ProcessVMDependent.get.getInputStream(process), defaultEncoding)
  val stderr: TextInput = TextInput.wrapHandle(ProcessVMDependent.get.getErrorStream(process), defaultEncoding)
  def waitFor(): Int = ProcessVMDependent.get.waitFor(process)
}
