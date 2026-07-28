/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, MethodType, Type}
import com.huawei.excelsior.jet.compiler.{Environment, RTSProc}

abstract class GenerationContext {
  def rootMethodType: MethodType
  def hostingClass: ClassType

  def fromClass: ClassType // Should be used for absent access only.

  def isRootInlineLevel: Boolean

  def fullName: String

  def isDirtyForClassGC(env: Environment): Boolean

  def inlineContext: InlineContext

  def shouldAddXSite: Boolean

  def isClinited(refClass: Type): Boolean

  def isManaged: Boolean

  def rootHasFrameDescriptor: Boolean
  def rootHasManagedExecEnv: Boolean
  def rootManual: Boolean
}

object GenerationContext {
  def forMethod(inlineContext: InlineContext): GenerationContext = new NormalContext(inlineContext ensuring (_ != null))
  def forThunk(methodType: MethodType): GenerationContext = new ThunkContext(methodType)

  private class NormalContext(override val inlineContext: InlineContext) extends GenerationContext {
    override def rootMethodType = inlineContext.rootMethod.getMethodType

    override def hostingClass = {
      // Note: hosting class is always the root declaring class,
      //       because baseline doesn't generate versioned methods.
      inlineContext.rootMethod.getDeclaringClass
    }

    override def fromClass = inlineContext.method.getDeclaringClass

    override def isRootInlineLevel = inlineContext.method == inlineContext.rootMethod

    override def fullName = s"method${inlineContext.rootMethod.getFullName}"

    override def isDirtyForClassGC(env: Environment): Boolean =
      inlineContext.toRoot exists (_.method.isDirtyForClassGC)

    override def shouldAddXSite = rootHasFrameDescriptor

    override def isClinited(refClass: Type) =
      refClass.isPreClinited || refClass.isTurboClinitedIn(inlineContext.rootMethod)

    override def isManaged = inlineContext.method.isManaged

    override def rootHasFrameDescriptor = inlineContext.rootMethod.hasFrameDescriptor

    override def rootHasManagedExecEnv = inlineContext.rootMethod.hasManagedExecEnv

    override def rootManual = inlineContext.rootMethod.isManual
  }

  private class ThunkContext(val methodType: MethodType) extends GenerationContext {
    override def rootMethodType = methodType

    override def hostingClass = shouldNotCallThis()

    override def fromClass = shouldNotCallThis()

    override def isRootInlineLevel = true

    override def fullName = s"thunk${methodType.toMethodDescriptor.toJETSignature}"

    override def isDirtyForClassGC(env: Environment) = shouldNotCallThis()

    override def inlineContext = shouldNotCallThis()

    override def shouldAddXSite = false // implicit null check handling in thunk code is implemented in runtime

    override def isClinited(refClass: Type) = refClass.isPreClinited

    override def isManaged = true

    override def rootHasFrameDescriptor = {
      assert(isRootInlineLevel)
      isManaged
    }

    override def rootHasManagedExecEnv = {
      assert(isRootInlineLevel)
      isManaged
    }

    override def rootManual = {
      assert(isRootInlineLevel)
      false
    }

  }
}
