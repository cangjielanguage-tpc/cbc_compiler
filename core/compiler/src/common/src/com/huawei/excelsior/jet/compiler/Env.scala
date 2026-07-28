/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg}
import com.huawei.excelsior.jet.compiler.abi.{ABI, Platform}

/** Global compiler environment with static access. Provides access to stable properties of
  * compilation process - platform, mode, options, resolved symlevel etc.
  *
  * Implemented for three modes: AOT, JIT and unit-tests.
  *
  * NOTE: be careful and think twice before you add anything here. Avoid mutable and
  * context-dependent elements.
  *
  * @author conwor
  */
object Env {

  ///////////////////////////////////////////////////////////////////////////////////////////////
  // Fundamental constants

  val bitsInByte = 8


  ///////////////////////////////////////////////////////////////////////////////////////////////
  // Platform

  def targetPlatform = { ensureInitialized(); _targetPlatform }

  def targetArch = targetPlatform.arch
  def targetOS   = targetPlatform.os

  def addressSize     = targetPlatform.arch.addressSize
  def addressLog2Size = targetPlatform.arch.addressLog2Size
  def stackSlotSize   = targetPlatform.arch.stackSlotSize

  def stackPointer        = targetPlatform.stackPointer
  def framePointer        = targetPlatform.framePointer
  def linkRegister        = targetPlatform.linkRegister
  def execEnvRegister     = targetPlatform.execEnvRegister
  def tailRegister        = targetPlatform.tailRegister
  def frameMiddleRegister = targetPlatform.frameMiddleRegister

  def frameAlignment      = targetPlatform.frameAlignment
  def forceFrameAlignment = targetPlatform.forceFrameAlignment


  ///////////////////////////////////////////////////////////////////////////////////////////////
  // Mode

  def isJIT           = { ensureInitialized(); _isJIT           }
  def isWorkMode      = { ensureInitialized(); _isWorkMode      }
  def isDynamicBundle = { ensureInitialized(); _isDynamicBundle }
  def languagePack    = { ensureInitialized(); _languagePack    }
  def isStandalone    = { ensureInitialized(); _isStandalone    }


  ///////////////////////////////////////////////////////////////////////////////////////////////
  // Implementation

  private var _targetPlatform: Platform[_ <: IReg, _ <: FReg, _ <: ABI[_ <: IReg, _ <: FReg]] = _
  private var _isJIT = false
  private var _isWorkMode = false
  private var _isDynamicBundle = false
  private var _languagePack: LanguagePack = _
  private var _isUnitTestsEnv = false
  private var _isStandalone = false

  def setUnitTestsEnv(): Unit = _isUnitTestsEnv = true
  def isUnitTestsEnv = _isUnitTestsEnv

  private def ensureInitialized(): Unit = assert(_targetPlatform != null)

  def init(targetPlatform: Platform[_ <: IReg, _ <: FReg, _ <: ABI[_ <: IReg, _ <: FReg]], isJIT: Boolean, isWorkMode: Boolean, isDynamicBundle: Boolean, languagePack: LanguagePack, isStandalone: Boolean): Unit = {
    assert(_targetPlatform == null || _isUnitTestsEnv)
    _targetPlatform = targetPlatform
    _isJIT = isJIT
    _isWorkMode = isWorkMode
    _isDynamicBundle = isDynamicBundle
    _languagePack = languagePack
    _isStandalone = isStandalone
  }
}
