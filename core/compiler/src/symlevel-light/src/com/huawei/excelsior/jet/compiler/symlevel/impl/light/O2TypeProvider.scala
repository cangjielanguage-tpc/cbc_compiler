/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.o2lib.fe.pc
import com.huawei.excelsior.jet.compiler.o2lib.u.{CacheAPIModule, ClassID}

object O2TypeProvider {
  def isJavaLangObject(_class: pc.SymType)      = CacheAPIModule.isThisClass(_class, ClassID.Object)
  def isJavaIoSerializable(_class: pc.SymType)  = CacheAPIModule.isThisClass(_class, ClassID.Serializable)
  def isJavaLangCloneable(_class: pc.SymType)   = CacheAPIModule.isThisClass(_class, ClassID.Cloneable)
  def isJavaLangThrowable(_class: pc.SymType)   = CacheAPIModule.isThisClass(_class, ClassID.JavaThrowable)
  def isJavaLangSystem(_class: pc.SymType)      = CacheAPIModule.isThisClass(_class, ClassID.System)
  def isJavaLangClassLoader(_class: pc.SymType) = CacheAPIModule.isThisClass(_class, ClassID.ClassLoader)
  def isSunMiscUnsafe(_class: pc.SymType)       = CacheAPIModule.isThisClass(_class, ClassID.Unsafe)
  def isCompilerInterface(_class: pc.SymType)   = CacheAPIModule.isThisClass(_class, ClassID.CompilerInterface)
  def isAJObject(_class: pc.SymType)            = CacheAPIModule.isThisClass(_class, ClassID.AJObject)
  def isLockableAJObject(_class: pc.SymType)    = CacheAPIModule.isThisClass(_class, ClassID.LockableAJObject)
  def isThinType(_class: pc.SymType)            = CacheAPIModule.isThisClass(_class, ClassID.ThinType)
  def isAJCompoundType(_class: pc.SymType)      = CacheAPIModule.isThisClass(_class, ClassID.CompoundType)
  def isAJWeakRef(_class: pc.SymType)           = CacheAPIModule.isThisClass(_class, ClassID.AJWeakRef)
  def isXScalaAnyRef(_class: pc.SymType)        = CacheAPIModule.isThisClass(_class, ClassID.XScalaAnyRef)
  def isJavaType(_class: pc.SymType)            = CacheAPIModule.isThisClass(_class, ClassID.JavaRefType)
  def isScalaType(_class: pc.SymType)           = CacheAPIModule.isThisClass(_class, ClassID.ScalaRefType)
  def isCangjieType(_class: pc.SymType)         = CacheAPIModule.isThisClass(_class, ClassID.CangjieRefType)

  def isAJArray(_class: pc.SymType) =
    CacheAPIModule.isThisClass(_class, ClassID.AJRefArray) ||
      CacheAPIModule.isThisClass(_class, ClassID.AJByteArray) ||
      CacheAPIModule.isThisClass(_class, ClassID.AJBooleanArray) ||
      CacheAPIModule.isThisClass(_class, ClassID.AJCharArray) ||
      CacheAPIModule.isThisClass(_class, ClassID.AJShortArray) ||
      CacheAPIModule.isThisClass(_class, ClassID.AJIntArray) ||
      CacheAPIModule.isThisClass(_class, ClassID.AJLongArray) ||
      CacheAPIModule.isThisClass(_class, ClassID.AJFloatArray) ||
      CacheAPIModule.isThisClass(_class, ClassID.AJDoubleArray)
}
