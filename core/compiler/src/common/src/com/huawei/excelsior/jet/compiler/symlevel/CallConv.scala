/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

/** Source code calling convention.
  *
  * Note: the order of enumeration elements should match the com.huawei.excelsior.aj.lang.CallType 
  * 
  * @author cypok
  */
enum CallConv(val isManaged: Boolean, val hasManagedExecEnv: Boolean, val isJET: Boolean, val ecoFriendly: Boolean) {
  case STDCALL   extends CallConv(isManaged = false, hasManagedExecEnv = false, isJET = false, ecoFriendly = false)
  case CCALL     extends CallConv(isManaged = false, hasManagedExecEnv = false, isJET = false, ecoFriendly = false)
  case MANAGED   extends CallConv(isManaged = true,  hasManagedExecEnv = true,  isJET = true,  ecoFriendly = false)
  case RTCALL    extends CallConv(isManaged = true,  hasManagedExecEnv = true,  isJET = true,  ecoFriendly = true)
  case VMCALL    extends CallConv(isManaged = false, hasManagedExecEnv = false, isJET = true,  ecoFriendly = true)
  case UNMANAGED extends CallConv(isManaged = false, hasManagedExecEnv = false, isJET = true,  ecoFriendly = false)
  case GCAWARE   extends CallConv(isManaged = false, hasManagedExecEnv = true,  isJET = true,  ecoFriendly = false)
  case MANUAL    extends CallConv(isManaged = false, hasManagedExecEnv = true,  isJET = true,  ecoFriendly = false)
}
