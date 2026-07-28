/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.fixups.Relocation
import xscala.util.simpleClassName

import scala.annotation.varargs

/** Fixup is an unfinished piece of code.
  *
  * It can be `resolved` when all required data is ready
  * or `converted` to some external environment for future resolving.
  *
  * @author paul
  * @author cypok
  * @author conwor
  */
object Fixup {
  @varargs def seq(args: Any*): Array[Any] = args.toArray
}

abstract class Fixup protected(
  private var _isVariable: Boolean,

  /** Current evaluated size. Can be changed in the future. */
  private var _size: Int
) extends Label {

  /** Returns size of fixup. It can be changed in future, iff Fixup [[isVariable]]. */
  protected[assembler] def size = _size

  /** Updates size of this variable-size fixup and returns subtract of new size and old one.
    *
    * Current [[expectedSize]] may be bigger, equal or lesser than previously calculated [[size]].
    *
    * If current size is bigger, we must extend fixup and return positive difference between
    * old and new sizes. If current size is equal, we can do nothing and return zero.
    *
    * The most complicated case is when current size is lesser than previously calculated. It means
    * that we possibly can shrink fixup. But in fixup expansion process [[Segment.expandFixups]] summary
    * delta of fixups resize should not be negative. So we check if we can resize out fixup to new size
    * without breaking resolving process. Negative difference between new size and old one should not be
    * lesser than `lowerResizeLimit`.
    */
  private[assembler] def resize(lowerResizeLimit: Int) = {
    assert(isVariable)
    var diff = expectedSize - size
    if (diff < lowerResizeLimit) diff = 0 // Avoid shrinking fixup when it can provoke endless looping in segment's layout calculation algorithm
    _size += diff
    diff
  }

  def isVariable = _isVariable

  private[assembler] def freeze(): Unit = _isVariable = false

  /** Size that this fixup would have in current context. */
  protected[assembler] def expectedSize: Int

  /** Resolves this fixup.
    *
    * Fixup resolving consists of two steps:
    *  1. Changing segment bytes associated with fixup
    *  1. Optional sending some simple external fixup (like [[Relocation]]) or several fixups outside
    *     (to AOT or JIT environment) through `converter`
    *
    * NOTE: these two step MUST be done in described order, because JIT environment patches segment bytes
    * immediately in [[com.huawei.excelsior.jet.assembler.fixups.Relocation.Converter.send]] method. So, if some
    * fixup resolving will call `send` and after that patch bytes it may produce strange bugs.
    */
  def resolve(converter: Relocation.Converter): Unit

  /** Returns array of fixup fields. Used to auto-implement equals/toString/hashCode.
    *
    * These methods used only in unit-tests, so performance is not critical here. Also, if some fixups are not used
    * in unit-tests it is not necessary to implement this method.
    */
  protected def guts: Array[Any] = shouldNotCallThis() // TODO-DECAF: use case classes.

  override def equals(that: Any) =
    (this.getClass eq that.getClass) && (guts sameElements that.asInstanceOf[Fixup].guts)

  override def hashCode = (getClass +: guts).toSeq.hashCode

  override def toString = guts.mkString(simpleClassName(this) + ": [", ", ", "]")
}
