/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import xscala.util.StringOps.{asciiAsDecimal, asciiIsDecimal}

import scala.collection.mutable.ArrayBuffer

object JETSignatureParser {
  def parse(sig: String): Signature =
    new JETSignatureParser(sig).parse()

  class Error(message: String) extends Exception(message)
}

private class JETSignatureParser(sig: String) {
  private var idx = 0

  def parse(): Signature = {
    val res = nextSig()
    check(!hasNext, "expected end of signature", idx)
    res
  }

  private def nextSig(): Signature = {
    import SignatureType.*
    withNext {
      case 'V' => Void
      case 'U' => Unit
      case 'N' => Nothing

      case 'b' => Boolean

      case sign @ ('i' | 'u') =>
        val signed = sign == 'i'
        withNext {
          case 'a' =>              if signed then AddrInt else AddrUInt
          case '8' =>              if signed then Int8    else UInt8
          case '1' => expect('6'); if signed then Int16   else UInt16
          case '3' => expect('2'); if signed then Int32   else UInt32
          case '6' => expect('4'); if signed then Int64   else UInt64
        }

      case 'c' => expect("32"); UnicodeChar32

      case 'f' =>
        withNext {
          case '1' => expect('6'); Float16
          case '3' => expect('2'); Float32
          case '6' => expect('4'); Float64
        }

      case 'B' => expect('S'); BString

      case 'P' => CPointer(nextSig())

      case 'S' => Record(nextStringUntil(';'))

      case 'R' =>
        val name = nextStringUntil(';')
        CangjieReference(name)

      case 'L' =>
        val name = nextStringUntil(';')
        JBCReference(name)

      case '!' => withNextSig { case t: NonNullableWrapper.Base => NonNullableWrapper(t) }

      case '?' => withNextSig { case t: NullableWrapper.Base => NullableWrapper(t) }

      case 'I' =>
        def nextTypeParameters(): Seq[SignatureType] = {
          expect('<')
          check(peek != '>', s"unexpected empty type parameters", idx)
          withNextSigsUntil('>', Some('_')) { case t: SignatureType => t }
        }
        withNext {
          case 'S' => InstantiatedRecord(nextStringUntil(';'), nextTypeParameters())
          case 'R' => InstantiatedReference(nextStringUntil(';'), nextTypeParameters())
        }

      case 'A' =>
        withNext {
          case 'S' => withNextSig { case elemType: SignatureType => ArraySlice(elemType) }

          case 'R' => withNextSig { case elemType: SignatureType => CangjieArray(elemType) }

          case 'J' =>
            val curIdx = idx
            val dimNum = nextNumber()
            check(dimNum.isValidInt, s"unexpected dimNum $dimNum", curIdx)
            withNextSig { case baseType: SignatureType => JavaArray(baseType, dimNum.toInt) }

          case 'V' =>
            val length = nextNumber()
            withNextSig { case elemType: SignatureType => VArray(elemType, length) }
        }

      case 'E' =>
        expect('W')
        val base = withNextSig { case t: CangjieEnumWrapper.Base => t }
        val name = nextStringUntil(';')
        CangjieEnumWrapper(base, name)

      case 'T' =>
        withNext {
          case 'T' =>
            ThisTypeInfo

          case 'L' =>
            val curIdx = idx
            val index = nextNumber()
            check(index.isValidInt, s"unexpected index $index", curIdx)
            LocalTypeVariable(index.toInt)

          case 'C' =>
            val curIdx = idx
            val index = nextNumber()
            check(index.isValidInt, s"unexpected index $index", curIdx)
            ClassTypeVariable(index.toInt)
        }

      case '(' =>
        val paramTypes = withNextSigsUntil(')', Some('_')) { case t: SignatureType => t }
        val returnType = withNextSig { case t: SignatureType => t }
        MethodSignature(returnType, paramTypes)
    }
  }

  ///////////////////
  // Utilities

  private def withNext[T](action: PartialFunction[Char, T]): T = {
    action.applyOrElse(next(), ch => throw error(s"unexpected symbol $ch", idx - 1))
  }

  private def withNextSig[T](action: PartialFunction[Signature, T]): T = {
    val start = idx
    action.applyOrElse(nextSig(), t => throw error(s"unexpected signature ${t.toJETSignature}", start))
  }

  private def withNextSigsUntil[T](end: Char, separator: Option[Char])(action: PartialFunction[Signature, T]): Seq[T] = {
    val sigs = ArrayBuffer.empty[T]

    var sigsEnded = peek == end

    while (!sigsEnded) {
      sigs += withNextSig(action)

      if (peek == end) {
        sigsEnded = true
      } else {
        separator foreach expect
      }
    }
    expect(end)

    sigs.toSeq
  }

  private def hasNext: Boolean = idx < sig.length

  private def peek: Char = {
    check(hasNext, "unexpected end of signature", idx)
    sig.charAt(idx)
  }

  private def next(): Char = {
    val ch = peek
    idx += 1
    ch
  }

  // Note: consumes `ch` but does not include it in result.
  private def nextStringUntil(ch: Char): String = {
    val start = idx
    while (next() != ch) {}
    sig.substring(start, idx - 1) // do not include `ch` in the result
  }

  private def nextNumber(): Long = {
    def nextDigit(): Int = withNext { case ch if ch.asciiIsDecimal => ch.asciiAsDecimal }

    var res: Long = nextDigit()
    while (peek.asciiIsDecimal) {
      res = res * 10 + nextDigit()
    }
    res
  }

  ///////////////////
  // Error reporting

  private def error(what: String, where: Int) = {
    new JETSignatureParser.Error(
      s"""$what in signature '$sig' at position $where
         |  $sig
         |  ${" " * where}^""".stripMargin)
  }

  private def check(condition: Boolean, msg: String, pos: Int): Unit = {
    if (!condition) throw error(msg, pos)
  }

  private def expect(ch: Char): Unit = {
    check(next() == ch, s"expected $ch", idx - 1)
  }

  private def expect(str: String): Unit = {
    str foreach expect
  }
}
