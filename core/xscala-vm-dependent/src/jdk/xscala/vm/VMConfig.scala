/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.vm

import xscala.adler32.{Adler32JDK, Adler32VMDependent}
import xscala.collection.{IdentityHashMapJDK, IdentityHashMapVMDependent}
import xscala.io.{FileSystemJDK, FileSystemVMDependent, IOJDK, IOVMDependent, InputStreamJDK, InputStreamVMDependent, OutputStreamJDK, OutputStreamVMDependent}
import xscala.sync.{LockableJDK, LockableVMDependent}
import xscala.management.{Management, ManagementJDK}
import xscala.matching.{RegexCompiler, RegexCompilerJDK}
import xscala.process.{ProcessJDK, ProcessVMDependent}
import xscala.properties.{Properties, PropertiesJDK}
import xscala.reflect.{ClassManipulation, ClassManipulationJDK}
import xscala.time.{TimeJDK, TimeVMDependent}
import xscala.xminizip.{Xmz, XmzJDK}
import xscala.text.{TextJDK, TextVMDependent}

import scala.annotation.static

class VMConfig
object VMConfig {
  @static def init(): Unit = {
    Management := new ManagementJDK
    Properties := new PropertiesJDK
    RegexCompiler := new RegexCompilerJDK
    ProcessVMDependent := new ProcessJDK
    InputStreamVMDependent := new InputStreamJDK
    OutputStreamVMDependent := new OutputStreamJDK
    FileSystemVMDependent := new FileSystemJDK
    IOVMDependent := new IOJDK
    Adler32VMDependent := new Adler32JDK
    Xmz := new XmzJDK
    TimeVMDependent := new TimeJDK
    TextVMDependent := new TextJDK
    IdentityHashMapVMDependent := new IdentityHashMapJDK
    ClassManipulation := new ClassManipulationJDK
    LockableVMDependent := new LockableJDK
  }
}
