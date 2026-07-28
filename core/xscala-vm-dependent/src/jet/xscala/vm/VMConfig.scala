/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.vm

import xscala.adler32.{Adler32JET, Adler32VMDependent}
import xscala.collection.{IdentityHashMapJET, IdentityHashMapVMDependent}
import xscala.io.{FileSystemJET, FileSystemVMDependent, IOJET, IOVMDependent, InputStreamJET, InputStreamVMDependent, OutputStreamJET, OutputStreamVMDependent}
import xscala.sync.{LockableJET, LockableVMDependent}
import xscala.management.{Management, ManagementJET}
import xscala.matching.{RegexCompiler, RegexCompilerJET}
import xscala.process.{ProcessJET, ProcessVMDependent}
import xscala.properties.{Properties, PropertiesJET}
import xscala.time.{TimeJET, TimeVMDependent}
import xscala.xminizip.{Xmz, XmzJET}
import xscala.text.{TextJET, TextVMDependent}

import scala.annotation.static

class VMConfig
object VMConfig {
  @static def init(): Unit = {
    Management := new ManagementJET
    Properties := new PropertiesJET
    RegexCompiler := new RegexCompilerJET
    ProcessVMDependent := new ProcessJET
    InputStreamVMDependent := new InputStreamJET
    OutputStreamVMDependent := new OutputStreamJET
    FileSystemVMDependent := new FileSystemJET
    IOVMDependent := new IOJET
    Adler32VMDependent := new Adler32JET
    Xmz := new XmzJET
    TimeVMDependent := new TimeJET
    TextVMDependent := new TextJET
    IdentityHashMapVMDependent := new IdentityHashMapJET
    LockableVMDependent := new LockableJET
  }
}
