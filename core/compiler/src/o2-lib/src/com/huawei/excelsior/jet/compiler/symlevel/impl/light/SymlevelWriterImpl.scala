/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.opt.OptEnvModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.symlevel._
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment._

class SymlevelWriterImpl(writer: SymlevelWriter.StreamWriter, contextClassType: Type) extends OptEnvModule.O2SymlevelWriter with SymlevelWriter {
  private val contextClass = typeToO2Class(contextClassType)

  override def putInt(x: Int): Unit = writer.putInt(x)

  override def putXString(x: XString): Unit = writer.putXString(x)

  private def o2Class(klass: pcOModule.Class): Unit = writeClassSymRef(klass, contextClass)

  private def o2Member(member: pcOModule.Member): Unit = {
    val host = member.getDeclaringClass
    o2Class(host)
    if (!host.isShielded) {
      putInt(member.lref)
    } else {
      putXString(member.name)
      putXString(XString(member.getSignature.toJETSignature))
      putInt(if (member.isStatic) 1 else 0)
    }
  }

  override def tkind(tkind: TypeKind): Unit = writer.putInt(tkind.ordinal)

  override def tpe(`type`: Type): Unit = {
    tkind(`type`.getKind)
    if (`type`.isPrimitive) {
      // no extra information needed for primitive types
    } else if (`type`.isJBCArray) {
      writer.putInt(`type`.getArrayDimnum)
      tpe(`type`.getArrayBase)
    } else if (`type`.isAJArray || `type`.isCangjieArray) {
      writer.putInt(0)
      o2Class(typeToO2Class(`type`))
    } else {
      o2Class(typeToO2Class(`type`))
    }
  }

  override def field(field: Field): Unit = o2Member(field.asInstanceOf[FieldImpl].o2f)

  override def method(method: Method): Unit = o2Member(method.asInstanceOf[MethodImpl].o2m)

  override def constString(str: ConstString): Unit = str.asInstanceOf[CPConstString].serialize(this, this.writer)

  override def frameDesc(fd: FrameDescSymbol): Unit = method(fd.getMethod)
}
