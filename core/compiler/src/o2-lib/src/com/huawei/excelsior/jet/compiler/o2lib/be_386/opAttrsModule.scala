/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.{isStandalone, isWorkMode}
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.be_386.CodeDefModule.Segment
import com.huawei.excelsior.jet.compiler.o2lib.be_386.formOMFModule.EXTDEF_Invalid
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.tools.ExportNames
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule as js
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenStackTrace
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.methodByO2Object
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{ExteriorMethodsVersioning, VersionedMethod}
import com.huawei.excelsior.jet.compiler.{CodeMetadata, Env, RTConst}
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.collection.IdentityHashMap
import xscala.util.UByte

import scala.collection.mutable

object opAttrsModule {

  /////////////////////////////////////////////////////////////////////////////
  // Back-end internal nameless objects

  class BOBJECT(_mno: Int) extends pc.Symbol(_mno, null) {
    override def getReadableName(need_class_name: Boolean, need_full_sign: Boolean = true): XString =
      shouldNotCallThis("BE objects do not have names")
  }

  case class StringHolder(str: XString) extends BOBJECT(pcO.x2cClass.mno) {
    assert(str != null)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Ready segments attached to objects

  private final case class ReadySegmentAttr(seg: Segment, metadata: CodeMetadata)
  private var readySegmentsAttrs: IdentityHashMap[pc.Symbol, ReadySegmentAttr] = _

  def setSegment(obj: pc.Symbol, seg: Segment, metadata: CodeMetadata = null): pc.Symbol = {
    readySegmentsAttrs(obj) = ReadySegmentAttr(seg, metadata)
    obj
  }

  def hasSegment(obj: pc.Symbol): Boolean = readySegmentsAttrs contains obj

  def getSegment(obj: pc.Symbol): Segment = readySegmentsAttrs(obj).seg

  def getMetadata(obj: pc.Symbol): CodeMetadata = readySegmentsAttrs(obj).metadata


  /////////////////////////////////////////////////////////////////////////////
  // Static variables offsets

  case class BaseOffsAttr(base: pc.Symbol, offs: Int)
  private var baseOffsAttrs: IdentityHashMap[pc.Symbol, BaseOffsAttr] = _

  def setBaseOffsAttr(obj: pc.Symbol, base: pc.Symbol, offs: Int): Unit = {
    baseOffsAttrs(obj) = BaseOffsAttr(base, offs)
  }

  def hasBaseOffsAttr(obj: pc.Symbol): Boolean = baseOffsAttrs contains obj

  def getBaseOffsAttr(obj: pc.Symbol): BaseOffsAttr = baseOffsAttrs(obj)


  /** -------------- S P E C I A L   O B J E C T s ------------------- */
  // Data objects which need to be handled in special way:
  // Type Descriptor (1st layer, positive offsets)
  // Type Descriptor (1st layer, negative offsets)
  // Type Descriptor (2nd layer - Serial type info)
  // Type Descriptor (2nd layer - Reflection info)
  // Static Fields Bundle
  // Local String Pool (per-class)
  // see also formOMF.ob2
  type SpecObjKind = UByte
  val BootstrapRequirements: SpecObjKind = UByte(0)
  val SerialTypeInfo: SpecObjKind = UByte(1)
  val VMTEncoding: SpecObjKind = UByte(2)
  val PreparationInfo: SpecObjKind = UByte(3)
  val TDReflection: SpecObjKind = UByte(4)
  val TDReflectionNegative: SpecObjKind = UByte(5)
  val StaticBundle: SpecObjKind = UByte(6)
  private def SpecObjKinds: IndexedSeq[SpecObjKind] = BootstrapRequirements to StaticBundle

  class BEConstData(_mno: Int, _name: XString, _size: Option[Int]) extends pc.DataSymbol.Const(_mno, pcNames.RawName(_name), _size)

  class BERawData(_mno: Int, _name: XString, _size: Option[Int]) extends pc.DataSymbol.RW(_mno, _name, _size)

  class ModuleVisitor {
    def otherObject(obj: pc.Symbol): Unit = {}

    def method(x: pcO.Method)                                   : Unit = otherObject(x)
    def staticField(x: pcO.StaticField)                         : Unit = otherObject(x)
    def stringTable(x: pcO.StringTable)                         : Unit = otherObject(x)
    def instanceDescriptor(x: pc.DataSymbol.InstanceDescriptor) : Unit = otherObject(x)
    def singletonObject(x: pc.DataSymbol.SingletonObject)       : Unit = otherObject(x)
    def runTimeTypeInfo(x: pc.DataSymbol.RunTimeTypeInfo)       : Unit = otherObject(x)
    def typeHandleBase(x: pc.DataSymbol.TypeHandle)             : Unit = otherObject(x)

    def thinTypeHandle(x: pc.DataSymbol.ThinTypeInfo, isInfected: Boolean): Unit = {
      otherObject(x.headerTypeHandle); otherObject(x.thinTypeHandle)
    }

    def absentContainer(x: pc.DataSymbol.TypeHandle)         : Unit = otherObject(x)
    def versionedMethod(x: VersionedMethod)                     : Unit = otherObject(x.bodyObj)
  }

  private val INVPROCNUM: Int = -1
  var currProc: pcO.Method = _
  var currProcno: Int = _
  var currUserProcno: Int = _
  var currClass: pcO.Class = _
  var genStackTrace: Boolean = _
  private val workObjects = mutable.LinkedHashMap.empty[XString, List[pc.Symbol]]
  private val SpecNames: Array[String] = Array[String](
    "BootstrapRequirements",
    "serial",
    "vmtenc",
    "prepare",
    "reflection",
    "reflection_negative",
    "static_bundle",
  )
  private val specialObjects: Array[pc.DataSymbol.Sized] = new Array[pc.DataSymbol.Sized](7)
  assert(SpecNames.length == SpecObjKinds.length)
  assert(specialObjects.length == SpecObjKinds.length)
  private val absentClasses = mutable.LinkedHashSet.empty[pcO.Class] // TODO: consider to use ImmSet, MSet & QSet in common part

  def nativeParamsLen(p: pcO.Method): Int = {
    assert(p.isDeclaredNative)

    var n = 2 // ee + this/clazz
    for (type0 <- p.getSignature.parameterTypes) {
      n += type0.jbcKindErased.nslots
    }
    n
  }

  def addToWorkObjects(o: pc.Symbol): Unit = workObjects.updateWith(o.name) {
    case Some(list) => Some(o :: list)
    case None => Some(List(o))
  }

  def findWorkObject(nm: XString): pc.Symbol =
    workObjects.get(nm).map(_.head).orNull

  def createSpecialObject(kind: SpecObjKind): pc.DataSymbol.Sized = {
    assert(SpecObjKinds contains kind)
    val nm = js.format("%s\'%S", SpecNames(kind.toInt), currClass.name)
    assert(findWorkObject(nm) == null)
    assert(specialObjects(kind.toInt) == null)
    val o = if (kind == StaticBundle) {
      newRawData(nm)
    } else if (kind == SerialTypeInfo || kind == VMTEncoding || kind == PreparationInfo || kind == TDReflection) {
      newRawConst(nm)
    } else {
      newUnsizedConst(nm)
    }
    specialObjects(kind.toInt) = o
    o
  }

  def findSpecialObject(kind: SpecObjKind): pc.DataSymbol.Sized = {
    assert(SpecObjKinds contains kind)
    specialObjects(kind.toInt) ensuring (_ != null || kind != TDReflection) // TDReflection is always present
  }

  private def newConst(name: XString, size: Option[Int], hostPar: pcO.Class = null): BEConstData = {
    assert(name != null && name.nonEmpty)

    val host = if (hostPar == null) currClass else hostPar
    val data = new BEConstData(host.mno, name, size)
    addToWorkObjects(data)
    data
  }

  def newSizedConst(name: XString, size: Int, host: pcO.Class = null): BEConstData = newConst(name, Some(size), host)

  def newUnsizedConst(name: XString, host: pcO.Class = null): BEConstData = newConst(name, None, host)

  def newRawConst(name: XString): BEConstData = newSizedConst(name, 0)

  def newSizedData(name: XString, size: Int, addToWork: Boolean = true): BERawData = {
    assert(name != null && name.nonEmpty)

    val data = new BERawData(currClass.mno, name, Some(size))
    if (addToWork) {
      // Data without add to work required to create objects just to reference them
      addToWorkObjects(data)
    }
    data
  }

  def newRawData(name: XString): BERawData = newSizedData(name, 0)

  // reserves data in BSS if omark_gen_ready is not set
  def newUninitializedData(size: Int): BERawData = newSizedData(js.format("$$uninitializedData"), size)

  private def newFrameDescriptorData(fdName: XString): pc.DataSymbol.Sized = newSizedData(js.format("%S$$frameDescriptor", fdName), RTConst.MethodAndTypeInfoFrameDescriptor.size, addToWork = false)

  def newFrameDescriptor(m: pcO.Method): pc.DataSymbol.Sized = {
    newFrameDescriptorData(ExportNames.methodLinkageName(methodByO2Object(m), needClassName = false))
  }

  def newFrameDescriptor(m: VersionedMethod): pc.DataSymbol.Sized = {
    newFrameDescriptorData(ExportNames.versionedMethodLinkageName(m, needClassName = false))
  }

  def addAbsentClass(aclass: pcO.Class): Unit = {
    assert(aclass.isShielded)
    absentClasses += aclass
  }

  def iterateModule(v: ModuleVisitor): Unit = {
    val c = currClass

    val strTable = c.getStringTable

    if (c.hasMetaInformation) {
      if (!c.hasThinTD || c.isInfectedAJClass) {
        v.typeHandleBase(c.typeHandle)
      }
      if (c.hasThinTD) {
        v.thinTypeHandle(c.thinTypeInfo, c.isInfectedAJClass)
      }

      if (c.hasInstanceDescriptor) {
        v.instanceDescriptor(c.instanceDescriptor)
      }

      if (c.isSingletonObject) {
        v.singletonObject(c.singletonObject)
      }

      if (strTable != null) {
        v.stringTable(strTable)
      }

      for (absent <- absentClasses) {
        v.absentContainer(absent.typeHandle)
      }
    } else {
      // ASSERT((strTable = NIL) OR (strTable.getLength() = 0));
      assert(absentClasses.isEmpty)
    }

    for (f <- c.declaredStaticFields if f.shouldBeGenerated) {
      v.staticField(f)
    }

    for (m <- c.declaredMethods if m.shouldBeGenerated) {
      v.method(m)
      if (m.hasFrameDescriptor && !isStandalone) {
        v.otherObject(m.getFrameDescriptor)
      }
    }

    ExteriorMethodsVersioning.getIteratorOverVersionedMethods(c) foreach { m =>
      v.versionedMethod(m)
      if (m.hasFrameDescriptor && !isStandalone) {
        v.otherObject(m.getFrameDescriptor)
      }
    }

    workObjects.valuesIterator.flatMap(_.iterator) foreach v.otherObject
  }

  //----------------------------
  def ini(): Unit = {
    currProc = null
    currProcno = INVPROCNUM
    currUserProcno = INVPROCNUM
    currClass = null

    workObjects.clear()
    for (i <- SpecObjKinds) {
      specialObjects(i.toInt) = null
    }

    readySegmentsAttrs = IdentityHashMap.empty[pc.Symbol, ReadySegmentAttr]
    baseOffsAttrs = IdentityHashMap.empty[pc.Symbol, BaseOffsAttr]

    genStackTrace = O2Env.env.enabled(GenStackTrace)
  }

  def exi(): Unit = {
    ini()
    absentClasses.clear()

    readySegmentsAttrs = null
    baseOffsAttrs = null
  }
}
