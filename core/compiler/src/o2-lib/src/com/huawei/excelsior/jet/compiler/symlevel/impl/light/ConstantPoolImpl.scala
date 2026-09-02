/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.bytecode.*
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool.ErrorAccessInfo
import com.huawei.excelsior.jet.compiler.util.Maps.Defaults.default
import com.huawei.excelsior.jet.compiler.o2lib.opt.OptEnvModule
import com.huawei.excelsior.jet.compiler.o2lib.opt.OptEnvModule.AccessResult
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{opAttrsModule, opStdModule}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule.TagInvokeDynamic
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.{JBCPreprocessor, JavaClassParserModule}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{IgnoreResolveErrors, LogUnresolvedErrors}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.verifier.VerificationError
import com.huawei.excelsior.jet.util.Numbering
import xscala.util.UInt.*

object ConstantPoolImpl {
  private[ConstantPoolImpl] class MethodAccess(private[ConstantPoolImpl] val o2m: pcOModule.Method) extends ConstantPool.Access[Method] {
    override def getResult = if (o2m.getDeclaringClass.isShielded) ConstantPoolAccessResult.DEFERRED else ConstantPoolAccessResult.OK
    override def getObject = methodByO2Object(o2m)
    override def getError = shouldNotCallThis()
    override def getDeferredInfo = shouldNotCallThis()
  }
}

class ConstantPoolImpl private[light](private val host: TypeImpl) extends ConstantPool {
  private[light] val o2cp = new O2CPImpl()

  private def getCPEntry(cpEntry: Int) = {
    assert(cpEntry >= 0)
    o2cp.classInfo.constantPool(cpEntry)
  }

  override def getHost = host

  override def getTypeProvider = env.getTypeProvider

  override def getCount = o2cp.classInfo.constantPoolCount.toInt

  override def getTag(cpEntry: Int) = tags(getCPEntry(cpEntry).constantType)
  override def getInt(cpEntry: Int)     = o2cp.getInt(cpEntry)
  override def getFloat(cpEntry: Int)   = o2cp.getFloat(cpEntry)
  override def getLong(cpEntry: Int)    = o2cp.getLong(cpEntry)
  override def getDouble(cpEntry: Int)  = o2cp.getDouble(cpEntry)
  override def getUtf8(cpEntry: Int)    = getCPEntry(cpEntry).bufferPtr

  override def getMethodCodeAttribute(method: Method) = {
    assert(method.getDeclaringClass == host)
    new CodeAttributeImpl(this, method.asInstanceOf[MethodImpl])
  }

  override def getClassIndex(cpEntry: Int)                = getCPEntry(cpEntry).index.toInt
  override def getClassNameIndex(cpEntry: Int)            = getCPEntry(cpEntry).indexName.toInt
  override def getNameAndTypeIndex(cpEntry: Int)          = getCPEntry(cpEntry).indexName.toInt
  override def getNameIndex(cpEntry: Int)                 = getCPEntry(cpEntry).indexName.toInt
  override def getDescriptorIndex(cpEntry: Int)           = getCPEntry(cpEntry).index.toInt
  override def getStringIndex(cpEntry: Int)               = getCPEntry(cpEntry).index.toInt
  override def getMemberIndex(cpEntry: Int)               = getCPEntry(cpEntry).index.toInt
  override def getMethodTypeDescriptorIndex(cpEntry: Int) = getCPEntry(cpEntry).index.toInt

  override def getMethodHandleRefKind(cpMethodHandleEntry: Int) = o2cp.getMethodHandleRefKind(cpMethodHandleEntry)
  override def getBootstrapMethodIndex(cpInvokeDynamicEntry: Int) = o2cp.getBootstrapMethodIndex(cpInvokeDynamicEntry)

  override def getBootstrapMethodArgsIndexes(cpInvokeDynamicEntry: Int): Array[Short] = {
    val bootstrapMethodArgsIndexes = o2cp.getBootstrapMethodArgsIndexes(cpInvokeDynamicEntry)
    if (bootstrapMethodArgsIndexes == null) {
      return null
    }
    val result = new Array[Short](bootstrapMethodArgsIndexes.length)
    for (i <- result.indices) {
      result(i) = bootstrapMethodArgsIndexes(i).toShort
    }
    result
  }

  private[light] class O2CPImpl extends OptEnvModule.CachedConstantPool {
    initConstantPool(o2env, host.asClass)

    override def makeErrorAccess(cpEntry: Int, errCode: VerificationError.ExceptionKind, errmsg: XString) = {
      checkIgnoreResolveErrors(errCode, errmsg)
      val rtsProc = opStdModule.stdExceptionProc(errCode)
      new ConstantPool.ErrorAccess(cpEntry, rtsProc, errmsg)
    }

    override def makeErrorAccess2(cpEntry: Int, cause: Object): Object = {
      // makeErrorAccess2 is called for field/method resolution when the declaring class has some resolution problems.
      // "cause" object is created by makeErrorAccess in this cases so there is no need to call checkIgnoreResolveErrors again.
      new ConstantPool.ErrorAccess(cpEntry, cause.asInstanceOf[ErrorAccessInfo])
    }

    override def makeDeferredAccess(cpEntry: Int, obj: pc.SymLevelObject) = {
      val name = obj match {
        case o2type: pc.SymType => o2type.symType.getName
        case field: pcOModule.Field => fieldByO2Object(field).getFullName
        case _ => methodByO2Object(obj.asInstanceOf[pcOModule.Method]).getFullName
      }

      if (!JBCPreprocessor.ignoreDeferred(name, host.asClass)) {
        if (LightweightEnvironment.env.enabled(LogUnresolvedErrors)) {
          println(s"UNRESOLVED: deferred $name at ${LightweightEnvironment.env.currentDebugPosition}")
        }
        assert(LightweightEnvironment.env.enabled(IgnoreResolveErrors), s"deferred $name")
      }

      if (obj.isInstanceOf[pc.SymType]) {
        val access = makeNormalAccess(cpEntry, obj).asInstanceOf[ConstantPool.Access[? <: ConstantPoolObject]]
        new ConstantPool.DeferredAccess(cpEntry, access.getObject)
      } else {
        makeNormalAccess(cpEntry, obj) // TODO: rewise
      }
    }

    override def makeNormalAccess(cpEntry: Int, obj: pc.SymLevelObject) = obj match {
      case obj: pc.SymType => typeByO2Object(obj)
      case obj: pcOModule.Field => fieldByO2Object(obj)
      case obj: pcOModule.Method =>
        // Create explicit MethodAccess, because MethodImpl does not inherit it.
        // TODO: consider inheriting Method from ConstantPool.Access
        new ConstantPoolImpl.MethodAccess(obj)

    }

    override def getAccessResult(access: Object): AccessResult =
      access.asInstanceOf[ConstantPool.Access[_]].getResult.ordinal.toUByte

    override def getTypeFromAccess(access0: Object) = {
      val access = access0.asInstanceOf[ConstantPool.Access[Type]]
      assert(access.getResult ne ConstantPoolAccessResult.ERROR)
      access.getObject.asInstanceOf[TypeImpl].o2object
    }

    override def getMethodFromAccess(access0: Object) = {
      val access = access0.asInstanceOf[ConstantPoolImpl.MethodAccess]
      assert(access.getResult eq ConstantPoolAccessResult.OK)
      access.o2m
    }

    override def getFieldFromAccess(access0: Object) = {
      val access = access0.asInstanceOf[ConstantPool.Access[Field]]
      assert(access.getResult eq ConstantPoolAccessResult.OK)
      val member = access.getObject
      member.asInstanceOf[FieldImpl].o2f
    }
  }

  override def getType(cpClassEntry: Int) =
    o2cp.getClassAndCheck(cpClassEntry).asInstanceOf[ConstantPool.Access[Type]] // TODO: caching

  override def getClassType(cpClassEntry: Int) =
    o2cp.getClassAndCheck(cpClassEntry).asInstanceOf[ConstantPool.Access[ClassType]] // TODO: caching

  override def getField(cpFieldEntry: Int, akind: FieldAccessKind) =
    o2cp.fieldAccess(cpFieldEntry, akind.isStatic, akind.isWrite).asInstanceOf[ConstantPool.Access[Field]] // TODO: caching

  override protected def getMethod(cpMethodEntry: Int, akind: MethodAccessKind) =
    o2cp.methodAccess(cpMethodEntry, akind).asInstanceOf[ConstantPool.Access[Method]] // TODO: caching

  override def getSignaturePolymorphicMethodID(cpMethodEntry: Int) =
    o2cp.getSignaturePolymorphicMethodID(cpMethodEntry)


  private lazy val invokeDynamicNumbering: Numbering[Int] = Numbering(0 until getCount filter { i => getCPEntry(i).constantType == TagInvokeDynamic })

  def getInvokeDynamicEntryNumber(cpEntryIdx: Int): Int = invokeDynamicNumbering.number(cpEntryIdx)

  // ===========================================
  //             T O D O
  // ===========================================

  override def getConstString(index: Int) =
    constStringByHostAndStringNumber(o2cp.klass, o2cp.getConstStringNumber(index))

  override def getFieldTypeKind(index: Int) =
    o2cp.getFieldTypeKind(index)

  override def getFieldRefClass(index: Int) =
    typeByO2Object(o2cp.getFieldRefClass(index))

  override def getRuntimeIndex(another: ConstantPool, index: Int) =
    TypeMetaInfoGenerator.AOTConstantPool.addAOTConstantPoolEntryFromClass(host.asClass, another.asInstanceOf[ConstantPoolImpl].host.asClass, index).toInt

  override def getAOTClassRefIndex(importIndex: Int) =
    TypeMetaInfoGenerator.AOTConstantPool.addAOTConstantPoolClassRefEntry(importIndex.toUShort).toInt
}
