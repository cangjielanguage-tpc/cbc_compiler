/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.o2lib.opt.{OptEnvModule, O2Env}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{JUtilModule, pc, pcOModule}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*

class SymlevelReaderImpl(private val reader: SymlevelReader.StreamReader, _contextClass: Type) extends OptEnvModule.O2SymlevelReader with SymlevelReader {
  private val contextClass = typeToO2Class(_contextClass)

  override def nextInt() = reader.nextInt()

  override def nextXString() = reader.nextXString()

  private def o2Class = readClassSymRef(contextClass, allowAbsenceOfExternalRefs = false)

  private def o2Member(isField: Boolean): pcOModule.Member = {
    val host = o2Class
    if (!host.isShielded) {
      host.getMemberByLRef(nextInt())
    } else {
      val name = nextXString()
      val xsig = nextXString()
      val statik = nextInt() != 0
      val sig = JETSignatureParser.parse(xsig.toString)
      val member = if (isField) {
        JUtilModule.insertAbsentField(contextClass, host, name, sig.asInstanceOf[SignatureType], statik)
      } else {
        JUtilModule.insertAbsentMethod(contextClass, host, name, sig.asInstanceOf[MethodSignature], statik)
      }
      assert(member != null)
      member
    }
  }

  private def o2ArrayType(dimNum: Int, base: Type) = typeToO2Type(base).array(dimNum)

  override def tkind() = typeKinds(reader.nextInt())

  override def tpe(allowAbsenceOfExternalRefs: Boolean) = {
    val kind = tkind()
    if (kind.isPrimitive) {
      typeByO2Object(getO2PrimType(kind))
    } else if (kind eq TypeKind.ARRAY) {
      val dimNum = reader.nextInt()
      if (dimNum == 0) {
        val o2class = readClassSymRef(contextClass, allowAbsenceOfExternalRefs = false)
        typeByO2Object(o2class)
      } else {
        typeByO2Object(o2ArrayType(dimNum, tpe()))
      }
    } else {
      assert((kind eq TypeKind.CLASS) || (kind eq TypeKind.INTERFACE) || (kind eq TypeKind.THIN) || (kind eq TypeKind.RECORD))
      val o2class = readClassSymRef(contextClass, allowAbsenceOfExternalRefs)
      if (o2class != null) typeByO2Object(o2class) else null
    }
  }

  override def field() = fieldByO2Object(o2Member(isField = true).asInstanceOf[pcOModule.Field])

  override def method() = methodByO2Object(o2Member(isField = false).asInstanceOf[pcOModule.Method])

  override def constString() = CPConstString.deserialize(this, this.reader, this.contextClass)

  override def frameDesc() = new FrameDescSymbolImpl(o2Member(isField = false).asInstanceOf[pcOModule.Method])
}
