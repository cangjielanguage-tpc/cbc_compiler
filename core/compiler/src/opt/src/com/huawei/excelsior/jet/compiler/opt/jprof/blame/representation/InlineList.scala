/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, Position}
import com.huawei.excelsior.jet.compiler.ir.BytecodeOffset
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.InlineList.Entry
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.InlineContextID
import com.huawei.excelsior.jet.jprof.{JProfFormat => JPF}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** `List` of `Method`s which either
  * - starts from deepest inlined method and ends with root method of inline, if `!reversed`;
  * - starts from root method of inline and ends with the deepest inlined method, if `reversed`.
  *
  * @author ijorch
  */
class InlineList(val entries: List[Entry], val reversed: Boolean) {

  def nonEmpty = entries.nonEmpty
  def length = entries.length
  def drop(n: Int) = new InlineList(entries drop n, reversed)

  private var _reversed: InlineList = _
  def reverse = if (_reversed == null) {
    val rev = new InlineList(entries.reverse, !reversed)
    rev._reversed = this
    _reversed = rev
    rev
  } else _reversed

  def isPrefix(that: InlineList): Boolean = {
    require(this.reversed == that.reversed)
    InlineList.isPrefix(this.entries, that.entries)
  }

  override def equals(obj: Any) = obj match {
    case ref: AnyRef if this eq ref => true
    case that: InlineList => this.reversed == that.reversed && this.entries == that.entries
    case _ => false
  }
  override def hashCode = if (reversed) entries.## else -entries.##
  override def toString = if (reversed) entries mkString " -> " else entries mkString " inlined to "
}

/** Transformations from compiler and raw jprof representations. */
object InlineList {

  case class JProfEntry(method: Method, bcPosInCaller: Int) {
    require(BytecodeOffset.isValid(bcPosInCaller))
  }
  case class Entry(method: Method, bcPosInMethod: Int) {
    override def toString = s"$method:$bcPosInMethod"
  }

  val empty = new InlineList(Nil, reversed = true)

  def unapply(arg: InlineList): Option[(Entry, InlineList)] = arg.entries match {
    case head :: tail => Some((head, new InlineList(tail, arg.reversed)))
    case _ => None
  }

  /** Transforms given bytecode position into the [[InlineList]] and reverses it. */
  def reversed(pos: Position) = {
    val (topBCPos, inlineContext) = Position.offsetAndInlineContext(pos) getOrElse shouldNotReachHere(pos)

    val top = Entry(
      Method.fromSymlevel(inlineContext.method),
      topBCPos ensuring (inlineContext.bytecodePos == BytecodeOffset.INVALID)
    )
    var chain = top :: Nil
    var ctx = inlineContext.caller

    while (ctx != null) {
      chain = Entry(Method.fromSymlevel(ctx.method), ctx.bytecodePos) :: chain
      ctx = ctx.caller
    }

    new InlineList(chain, reversed = true)
  }

  /** Transforms slice of `icMethods` indicated by `id` into the [[InlineList]] and reverses it.
    * As `id` does not include root method of inline, `inlineRoot` must be specified separately.
    * Also, `id` does not include bytecode position in the last inlined method, so it should be provided via `topBCPos`.
    */
  def reversed(icEntries: Array[JProfEntry], id: Option[InlineContextID], inlineRoot: Method, topBCPos: Int) = id match {
    case None =>
      assert(inlineRoot != null)
      new InlineList(Entry(inlineRoot, topBCPos) :: Nil, reversed = true)

    case Some(InlineContextID(start, end)) =>
      var chain = List.empty[Entry]
      var bcPos = topBCPos
      var i = start
      while (i < end) {
        val JProfEntry(method, bcPosInCaller) = icEntries(i)
        chain = Entry(method, bcPos) :: chain
        bcPos = bcPosInCaller
        i += 1
      }
      assert (inlineRoot != null)
      chain = Entry(inlineRoot, bcPos) :: chain
      new InlineList(chain, reversed = true)
  }

  /** Transforms given bytecode position into the [[InlineList]]. */
  def apply(pos: Position) = {
    val (topBCPos, inlineContext) = Position.offsetAndInlineContext(pos) getOrElse shouldNotReachHere(pos)

    val top = Entry(
      Method.fromSymlevel(inlineContext.method),
      topBCPos ensuring (inlineContext.bytecodePos == BytecodeOffset.INVALID)
    )
    val list = top :: List.unfold(inlineContext.caller) {
      case null => None
      case ic => Some((Entry(Method.fromSymlevel(ic.method), ic.bytecodePos), ic.caller))
    }
    new InlineList(list, reversed = false)
  }

  /** Transforms slice of `icMethods` indicated by `id` into the [[InlineList]].
    * As `id` does not include root method of inline, `inlineRoot` must be specified separately.
    * Also, `id` does not include bytecode position in the last inlined method, so it should be provided via `topBCPos`.
    */
  def apply(icEntries: Array[JProfEntry], id: Option[InlineContextID], inlineRoot: Method, topBCPos: Int) = id match {
    case None =>
      assert(inlineRoot != null)
      new InlineList(Entry(inlineRoot, topBCPos) :: Nil, reversed = false)

    case Some(InlineContextID(start, end)) =>
      val buf = ListBuffer.empty[Entry]
      var bcPos = topBCPos
      for (i <- start until end) {
        val JProfEntry(method, bcPosInCaller) = icEntries(i)
        buf += Entry(method, bcPos)
        bcPos = bcPosInCaller
      }
      assert (inlineRoot != null)
      buf += Entry(inlineRoot, bcPos)
      new InlineList(buf.toList, reversed = false)
  }

  /** Returns `true` iff inline list `xxs` is prefix of `yys`. */
  @tailrec private def isPrefix(xxs: List[Entry], yys: List[Entry]): Boolean = (xxs, yys) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (x :: xs, y :: ys) =>
      assert(x.method.name != JPF.METHOD_NAME_UNKNOWN || x.method.declaringType == JPF.CLASS_UNKNOWN,
        "PGO Error: <unknown> method in inline list, update your .jprof")

      equalEntries(x, y) && isPrefix(xs, ys)
  }

  def equalEntries(x: Entry, y: Entry): Boolean = {
    // TODO: investigate invalid position reasons and remove this method
    x.method == y.method &&
      (x.bcPosInMethod == BytecodeOffset.INVALID || x.bcPosInMethod == y.bcPosInMethod)
  }

  import Ordering.Implicits._
  implicit val ord: Ordering[InlineList] = Ordering by { (il: InlineList) =>
    (il.entries map (_.method), il.entries map (_.bcPosInMethod), il.reversed)
  }
}
