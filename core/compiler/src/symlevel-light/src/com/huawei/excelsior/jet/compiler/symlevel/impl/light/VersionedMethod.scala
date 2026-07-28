/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.{CodeUnit, TypeProvider}
import com.huawei.excelsior.jet.compiler.abi.FrameProperties
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcNamesModule, pcOModule}
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Method, SignatureType, Type}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*

/** @param versionedIdx index of this version of the [[original]] method in versioned methods array of [[versionHostingClass]] */
case class VersionedMethod(m: Method, versionHostingClass: pcOModule.Class, versionedIdx: Int) extends CodeUnit(m) { self =>

  val original: pcOModule.Method = getO2Method(m)

  val bodyObj: pc.Symbol = {
    val mName = this.method.getXName
    val cName = this.method.getDeclaringClass.getXName
    val name = JStringsModule.format("%S-versioned-from-%S", mName, cName)
    val body = new pcOModule.VersionedMethodBody(versionHostingClass.mno, pcNamesModule.RawName(name))
    if (this.original.isOverloaded) body.markAsOverloaded()
    body
  }

  private var frameDesc: pc.Symbol = _

  def hasFrameDescriptor: Boolean = m.hasFrameDescriptor

  def getFrameDescriptor: pc.Symbol = {
    assert(hasFrameDescriptor)
    if (frameDesc == null) {
      frameDesc = opAttrsModule.newFrameDescriptor(this)
    }
    frameDesc
  }

  def getXName = o2name(bodyObj)

  override def isVersionedMethod = true

  override def getReceiverType(implicit tp: TypeProvider): ClassType = classByO2Object(versionHostingClass)

  override def getHostingClass: ClassType = classByO2Object(versionHostingClass)

  override lazy val getHostedIndex = getHostingClass.getGeneratedMethods.size + versionedIdx

  override def getSymbol = symbolByO2Object(bodyObj)

  override val getFrameProperties: FrameProperties = new FrameProperties {
    override def getFullName                                          = self.toString
    override def getFrameDescriptor                                   = symbolByO2Object(self.getFrameDescriptor)

    override def getRealMethodType(varArgs: Iterable[SignatureType])  = method.getRealMethodType(varArgs)
    override def hasFrameDescriptor                                   = method.hasFrameDescriptor
    override def shouldStackCheckByCaller                             = method.shouldStackCheckByCaller
    override def shouldContainGCPoints                                = method.shouldContainGCPoints
    override def shouldContainGCPointInEpilogue                       = method.shouldContainGCPointInEpilogue
    override def shouldContainGCPointInEpilogueBeforeFrameDrop        = method.shouldContainGCPointInEpilogueBeforeFrameDrop
    override def shouldContainGCPointInEpilogueAfterFrameDrop         = method.shouldContainGCPointInEpilogueAfterFrameDrop
    override def isStackCheckDisabled                                 = method.isStackCheckDisabled
    override def isVarArgs                                            = method.isVarArgs
    override def isManagedFrame                                       = method.isManagedFrame
    override def isManaged                                            = method.isManaged
    override def isHookInvoker                                        = method.isHookInvoker
    override def getStackCheckByCallerBytes                           = method.getStackCheckByCallerBytes
  }

  override def getUniqueNumberInClass = versionHostingClass.declaredMethodsCount + 1 + versionedIdx

  override def getName = getXName.toString
}
