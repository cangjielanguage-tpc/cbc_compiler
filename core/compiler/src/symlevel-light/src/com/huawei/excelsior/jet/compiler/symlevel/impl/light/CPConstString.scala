/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.symlevel._
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment._

object CPConstString {
  private[light] def deserialize(sr: SymlevelReader, reader: SymlevelReader.StreamReader, contextClass: pcOModule.Class) = {
    var strnum = reader.nextInt()
    if (strnum != -1) {
      val host = typeToO2Class(sr.tpe())
      val stringTable = host.getStringTable
      assert(strnum < stringTable.getSymFileTimeLength,
        s"string index $strnum out of string table range ${stringTable.getSymFileTimeLength} in${host.name}")
      CPConstString(host, strnum)
    } else {
      val value = reader.nextXString()
      var curClass = opAttrsModule.currClass
      if (curClass == null) {
        curClass = contextClass
      }
      val stringTable = curClass.getStringTable
      strnum = stringTable.getIndexByStringIfPresent(value)
      if (strnum == -1) {
        strnum = stringTable.addString(value)
      }
      CPConstString(curClass, strnum)
    }
  }
}

case class CPConstString(host: pcOModule.Class, strnum: Int) extends ConstString {
  override def value = host.getStringTable.getStringByIndex(strnum)

  override def getHost = typeByO2Object(host)

  override def getStringTable = stringTableSymbolByO2Object(host.getStringTable, "string table of " + host.name)

  override def getStringNumber = strnum

  def serialize(sw: SymlevelWriter, writer: SymlevelWriter.StreamWriter) = {
    if (strnum < host.getStringTable.getSymFileTimeLength) {
      writer.putInt(strnum)
      sw.tpe(getHost)
    } else {
      writer.putInt(-1)
      writer.putXString(value)
    }
  }

  override def toString = "\"" + value + "\" (" + host.name + "#" + strnum + ")"
}
