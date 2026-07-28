/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.symlevel.{MethodType, SignatureType}

/** [[FrameProperties]] should be used to create instance of [[Frame]]. Do not use this interface in any other context. */
trait FrameProperties {

  /////////////////////////////////////////////////////////////////////////////
  // Frame descriptor

  /** Returns true iff this frame corresponds to code which has frame descriptor. */
  def hasFrameDescriptor: Boolean

  /** Returns frame descriptor of code corresponds to this frame. */
  def getFrameDescriptor: Symbol


  /////////////////////////////////////////////////////////////////////////////
  // Stack checks

  /** Returns true iff this frame prologue should not contain stack checks. */
  def isStackCheckDisabled: Boolean

  /** Returns true iff stack check for this frame should be done by caller. */
  def shouldStackCheckByCaller: Boolean

  /** Returns count of additional bytes that should be touched in caller's prologue. */
  def getStackCheckByCallerBytes: Int


  /////////////////////////////////////////////////////////////////////////////
  // GC points

  /** Returns true iff GC points should be used in code corresponds to this frame. */
  def shouldContainGCPoints: Boolean

  /** Returns true iff GC points should be inserted in epilogue. */
  def shouldContainGCPointInEpilogue: Boolean

  /** Returns true iff code corresponds to this frame should contain GC points in epilogue before saved registers was
    * restored. That is the case only if the method is @RTCall and so we don't have any free register to use for more
    * lightweight GCPoint in epilogue after frame drop. See [[shouldContainGCPointInEpilogueAfterFrameDrop]].
    */
  def shouldContainGCPointInEpilogueBeforeFrameDrop: Boolean

  /** Returns true iff code corresponds to this frame should contain lightweight GCPoint in the very end of epilogue.
    * For such GCPoints no GCMap needed.
    */
  def shouldContainGCPointInEpilogueAfterFrameDrop: Boolean


  /////////////////////////////////////////////////////////////////////////////
  // Other

  def getFullName: String

  /** Returns method type with real parameters (explicit varargs and no special wrapper params). */
  def getRealMethodType(varArgs: Iterable[SignatureType]): MethodType

  /** Returns true iff code corresponds to this frame uses varargs. */
  def isVarArgs: Boolean

  /** Returns true iff frame has managed ExecEnv or corresponds to @CallToManaged method. */
  def isManagedFrame: Boolean

  /** Returns true iff frame corresponds to code with managed calling convention. */
  def isManaged: Boolean

  /** Returns true iff frame corresponds to method annotated with @Hook.Invoker. */
  def isHookInvoker: Boolean
}
