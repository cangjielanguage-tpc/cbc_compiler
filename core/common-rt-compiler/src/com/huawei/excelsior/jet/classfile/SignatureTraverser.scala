/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.classfile

import com.huawei.excelsior.jet.common.MyPredef.*
import com.huawei.excelsior.jet.common.XString

/** Special signature traverser.
  *
  * Note that unlike [[java.util.Iterator]] this traverser assumes that any signature has at least one entry
  * which can be obtained just after traverser creation (without calls to [[SignatureTraverser.hasNext]]/[[queryNext()]]).
  *
  * Following entries could be obtained after call to [[queryNext()]] if [[hasNext]] returns `true`.
  *
  * Such API simplifies access to
  *
  *  - one-entry signatures (i.e. field's signature):
  *    {{{
  *      val traverser = getTraverser()
  *      processFieldType(traverser)
  *      assert(!traverser.hasNext())
  *    }}}
  *
  *  - the last element of signatures (i.e. return type of method's signature):
  *    {{{
  *      val traverser = getTraverser()
  *      while (traverser.hasNext()) {
  *        processParamType(traverser)
  *        traverser.queryNext()
  *      }
  *      processReturnType(traverser)
  *    }}}
  */
abstract class SignatureTraverser {
  def hasNext: Boolean
  def queryNext(): Unit
  def isClass: Boolean
  def getClassName: XString
  def getPrimitiveSigChar: Byte
  def getArrayDim: Int
  final def getSlotsNum = {
    if (isClass || (getArrayDim > 0)) {
      1
    } else {
      val ch = getPrimitiveSigChar
      if ((ch == 'J') || (ch == 'D')) 2 else  1
    }
  }
}

object SignatureTraverser {
  def fromString(sig: XString): SignatureTraverser = new FromStringImpl(sig)

  private class FromStringImpl(sig: XString) extends SignatureTraverser {
    private val bytes = XString.unsafeGetValue(sig)
    private val offset = XString.unsafeGetOffset(sig)
    private val len = sig.length
    private val end = offset + len
    private var pos = offset
    private var arrayDim = 0
    private var nameStart = 0
    private var nameLen = 0

    queryNext()

    override def hasNext = pos < end

    override def queryNext(): Unit = {
      arrayDim = 0
      nameStart = -1
      while (true) {
        if (pos >= end) {
          invalidSignature("unexpected end")
          return
        }
        val entryChar = bytes(pos)
        pos += 1
        entryChar match {
          case '(' | ')' =>
            if (arrayDim > 0) {
              invalidSignature(s"unexpected '${entryChar.toChar}' after '['")
              return
            } else {
              // expected parenthesis (skip)
            }

          case 'L' =>
            nameStart = pos
            nameLen = 0
            while (pos < end) {
              val ch = bytes(pos)
              pos += 1
              if (ch == ';') {
                if (nameLen == 0) {
                  invalidSignature(s"'${entryChar.toChar};' without class name")
                  return
                }
                return
              }
              nameLen += 1
            }
            invalidSignature(s"no ';' after '${entryChar.toChar}'")
            return

          case '[' =>
            arrayDim += 1

          case 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' | 'V' =>
            return

          case _ =>
            invalidSignature(s"invalid char '${entryChar.toChar}'")
        }
      }
    }

    override def isClass = nameStart > 0

    override def getPrimitiveSigChar = {
      assert(!isClass && (pos > 0))
      bytes(pos - 1)
    }

    override def getClassName = {
      assert(isClass)
      XString.unsafeWrap(bytes, nameStart, nameLen)
    }

    override def getArrayDim = arrayDim

    private def invalidSignature(message: String): Unit = {
      throw new IllegalArgumentException(s"Signature '$getSig' at index ${pos-offset}: $message")
    }

    private def getSig = XString.unsafeWrap(bytes, offset, len)
  }
}
