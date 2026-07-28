/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.classfile


import com.huawei.excelsior.jet.common.XString

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import com.huawei.excelsior.jet.classfile
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.common.MyPredef.*

/** Convenient interface for parsing a single [[XString]] with field or method signature.
  *
  * A signature is treated as a sequence of type entries:
  *
  *  - Primitive entry or array of them ('I', '[J' etc.);
  *  - Class entry or array of them ('LFoo;', '[LBaz ; ' etc.).
  *
  * These entries are parsed into an [[Iterator]] of concrete types
  * with conversion rules defined by implementations of this interface.
  *
  * The iteration continues as long as the next type entry in sequence can be correctly parsed.
  * The parser omits all parentheses, which do not prevent correct parsing of entries
  * (e.g. signature ')[V' is valid for parsing, while '[)V' is not).
  * If a type cannot be parsed, [[IllegalArgumentException]] is thrown.
  *
  * ''Note:'' this parser does not distinguish between field and method signatures
  * and can iterate over malformed signatures without raising an error.
  * However, for valid signatures (in terms of JVMS) it will always produce the following type sequences:
  *
  *  - Field signature:   iterator with a single element -- field type;
  *  - Method signature:  non-empty iterator with parameter types, followed by return type.
  *
  * @author liontiger
  */

abstract class SignatureParser[T](private var traverser: SignatureTraverser) extends Iterator[T] {
  require(traverser != null)

/** Creates a parser for the signature wrapped by the given [[SignatureTraverser]]. */

  def this(sig: XString) = this(SignatureTraverser.fromString(sig))

  protected def parsePrimitive(arrayDim: Int, sigChar: Byte): T

  protected def parseClass(arrayDim: Int, name: XString): T

  override final def hasNext = traverser != null

  override final def next() = {
    if (!hasNext) throw new NoSuchElementException

    val arrayDim = traverser.getArrayDim
    val result = if (traverser.isClass) {
      parseClass(arrayDim, traverser.getClassName)
    } else {
      parsePrimitive(arrayDim, traverser.getPrimitiveSigChar)
    }

    if (traverser.hasNext) {
      traverser.queryNext()
    } else {
      traverser = null
    }

    result
  }

  /** Convenience method for signatures representing field types.
    *
    * Ensures that there is exactly one element in parsed signature and returns it.
    */
  final def singleElement = {
    val res = next()
    if (hasNext) throw new IllegalArgumentException("signature has more than one entry")
    res
  }

  /** Convenience method for signatures representing method types.
    *
    * Returns pair of (parameter types sequence, return type).
    */
  final def asMethodSig: (Seq[T], T) = {
    val buf = ListBuffer.empty[T]
    var t = next()
    while (hasNext) {
      buf += t
      t = next()
    }
    (buf.toList, t)
  }
}
