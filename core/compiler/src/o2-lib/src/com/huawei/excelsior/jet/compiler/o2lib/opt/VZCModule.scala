/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.opt

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Pass
import com.huawei.excelsior.jet.compiler.o2lib.opt.OptEnvModule as BEnv
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env.env
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule as pcO
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.jbcFrontModule as jbcFront
import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule as xiEnv
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.VersionedMethod

object VZCModule {

  /////////////////////////////////////////////////////////////////////////////
  // Interface between project system and new compiler environment

  def dropSymCache(): Unit = env.dropSymCache()


  /////////////////////////////////////////////////////////////////////////////
  // Options subsystem. TODO: find better place for it

  import com.huawei.excelsior.jet.compiler.options.Option as COption

  def getDefaultOption(name: XString): xiEnv.OptionSpecifiedType = {
    assert(env != null)
    COption.byName(name.toString) match {
      case option: BoolOption => option.defaultValueOrNull(env) match {
        case null => xiEnv.UNSPECIFIED
        case java.lang.Boolean.TRUE => xiEnv.YES
        case java.lang.Boolean.FALSE => xiEnv.NO
      }
      case _ => xiEnv.UNSPECIFIED
    }
  }

  def getDefaultEquation(name: XString): XString = {
    assert(env != null)
    COption.byName(name.toString) match {
      case null | _: BoolOption => null
      case option => option.defaultValueOrNull(env) match {
        case null => null
        case any => XString.ascii(any.toString)
      }
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Interface between project system and new compiler.

  abstract class CompilerInterface {
    def enterClass(`class`: pcO.Class, stage: Pass): Unit
    def exitClass(`class`: pcO.Class): Unit
    def compileMethod(method: pcO.Method, versioned: VersionedMethod): Unit
    def printFinalStatistics(): Unit
  }

  abstract class CompilerProvider {
    def provideCompiler(): CompilerInterface
  }

  private var _compiler: CompilerInterface = _
  private var _compilerProvider: CompilerProvider = _

  def compilerProvider = _compilerProvider

  def compilerProvider_=(compilerProvider: CompilerProvider): Unit = {
    assert(_compilerProvider == null)
    _compilerProvider = compilerProvider
  }

  def compiler: CompilerInterface = {
    if (_compiler == null) {
      _compiler = _compilerProvider.provideCompiler()
      _compilerProvider = null
    }
    _compiler ensuring { _ != null }
  }
}