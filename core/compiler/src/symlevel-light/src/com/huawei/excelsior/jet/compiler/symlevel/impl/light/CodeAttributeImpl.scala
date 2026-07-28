/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute.{ExceptionTableTraverser, ExceptionTableTraverserArrayImpl}
import com.huawei.excelsior.jet.compiler.ir.LineNumber
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule.LocalVariable
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.{JBCPreprocessor, JavaClassParserModule as jcp}
import com.huawei.excelsior.jet.compiler.symlevel.{Method, SignatureType, Type}
import xscala.io.{BigEndian, ByteBuffer}

import java.io.IOException
import scala.collection.mutable.ArrayBuffer

/** Implementation of Code attribute of method. */
final class CodeAttributeImpl private[light](private val cp: ConstantPoolImpl, method: MethodImpl) extends Method.CodeAttribute {
  private val jcpCode = cp.o2cp.getCodeAttribute(method.o2m)
  assert(jcpCode != null)
  assert(jcpCode.codePtr.length == jcpCode.codeLength)
  assert((jcpCode.excepTable == null && jcpCode.excepTableLength.toInt == 0) || (jcpCode.excepTable.length == jcpCode.excepTableLength.toInt))

  private var lineNumberAttributes: ArrayBuffer[Array[jcp.LineNumber]] = _

  override def maxStack = jcpCode.stackSize.toInt

  override def maxLocals = jcpCode.localSize.toInt

  override def bytecodeLength = jcpCode.codePtr.length

  override def bytecodeArray = jcpCode.codePtr

  override def bytecodeStart = 0

  override def hasExceptionTable = jcpCode.excepTableLength.toInt > 0

  override def getExceptionTableTraverser: ExceptionTableTraverser = new ExceptionTableTraverserArrayImpl[jcp.ExcepInfo](jcpCode.excepTable) {
    override def startPC(x: jcp.ExcepInfo): Int = x.startPC.toInt

    override def endPC(x: jcp.ExcepInfo): Int = x.endPC.toInt

    override def handlerPC(x: jcp.ExcepInfo): Int = x.handlerPC.toInt

    override def catchTypeIndex(x: jcp.ExcepInfo): Int = x.catchType.toInt

    override def catchTypeName(x: jcp.ExcepInfo) = {
      val catchTypeCPIndex = catchTypeIndex(x)
      if (catchTypeCPIndex == 0) null else JBCPreprocessor.preprocessClassName(cp.getClassNameValue(catchTypeCPIndex), cp.o2cp.klass)
    }
  }

  override def exceptionTableLength = jcpCode.excepTableLength.toInt

  private def ensureLineNumberAttributesExists(): Unit = if (lineNumberAttributes == null) {
    val attrs = ArrayBuffer.empty[Array[jcp.LineNumber]]
    val attrCount = jcpCode.attributeCount.toInt
    for (i <- 0 until attrCount) {
      val attr = jcpCode.attribute(i)
      if (cp.getUtf8(attr.nameIndex.toInt) == jcp.jstrLineNumber) {
        val _table = attr.lineNumberTable
        if (_table != null) {
          // sort line number table for binary search
          val table = _table.sortWith { (x, y) => x.startPC < y.startPC }
          // reset duplicate entries (only leading entry should be taken into account)
          for (j <- 1 until table.length) {
            if (table(j - 1).startPC == table(j).startPC) {
              jcp.setLineNumber(table(j), table(j - 1).lineNumber)
            } else {
              assert(table(j - 1).startPC.toInt < table(j).startPC.toInt)
            }
          }
          attrs += table
        }
      }
    }
    lineNumberAttributes = attrs
  }

  /** Looks for line number information by given bytecode offset.
    *
    * @return best matching line number, or {@link LineNumber# UNKNOWN} if line number information is not available.
    */
  override def findLineNumber(bytecodeOffset: Int): Int = {
    assert((bytecodeStart <= bytecodeOffset) && (bytecodeOffset < bytecodeLength))

    ensureLineNumberAttributesExists()

    var bestMatchPC = -1
    var bestMatchLineNum = LineNumber.UNKNOWN

    for (attr <- lineNumberAttributes) { // binary search
      var lo = 0
      var hi = attr.length - 1
      var m = 0
      while (lo <= hi) {
        m = (lo + hi) / 2
        val ln = attr(m)
        val pc = ln.startPC.toInt
        if (pc == bytecodeOffset) { // Exact match
          return ln.lineNumber.toInt
        }
        if (pc < bytecodeOffset) {
          if (bestMatchPC < pc) {
            bestMatchPC = pc
            bestMatchLineNum = ln.lineNumber.toInt
          }
          lo = m + 1
        } else {
          hi = m - 1
        }
      }
    }

    bestMatchLineNum
  }

  override def firstLineNumber = {
    ensureLineNumberAttributesExists()

    var result = Integer.MAX_VALUE
    for (attr <- lineNumberAttributes) {
      for (ln <- attr) {
        val line = ln.lineNumber.toInt
        if (line < result) {
          result = line
        }
      }
    }

    result
  }

  private def getAttribute(name: XString) = {
    val classInfo = cp.getHost.asInstanceOf[TypeImpl].asClass.classInfo
    jcp.getAttribute(classInfo, jcpCode.attribute, jcpCode.attributeCount.toInt, name)
  }

  final class LVTImpl(private val lvt: Array[LocalVariable], private val cp: ConstantPoolImpl) extends Method.LocalVariablesTable {

    override def localCount(): Int = lvt.length

    override def localName(i: Int): XString = cp.getUtf8(lvt(i).nameIndex.toInt)

    override def localSignature(i: Int): XString = cp.getUtf8(lvt(i).signatureIndex.toInt)
  }

  override def getLocalVariablesTable(): Option[Method.LocalVariablesTable] =
    getAttribute(jcp.jstrLocVarName).map(_.localVariableTable).map(LVTImpl(_, cp))

  override def localVariableTable: Array[Byte] = {
    for (a <- getAttribute(jcp.jstrLocVarName)) {
      val table = a.localVariableTable
      if (table != null) {
        val length = table.length
        val buf = new ByteBuffer(2 + length * 10)
        val be = BigEndian(buf)
        try {
          be.putW16(length)
          for (i <- 0 until length) {
            be.putW16(table(i).startPC.toInt)
            be.putW16(table(i).length.toInt)
            be.putW16(table(i).nameIndex.toInt)
            be.putW16(table(i).signatureIndex.toInt)
            be.putW16(table(i).slot.toInt)
          }
        } catch {
          case _: IOException => shouldNotReachHere()
        }
        return buf.toByteArray
      }
    }
    null
  }

  override def stackMapTable: Array[Byte] =
    getAttribute(jcp.jstrStackMapTable).map(_.info).orNull
}
