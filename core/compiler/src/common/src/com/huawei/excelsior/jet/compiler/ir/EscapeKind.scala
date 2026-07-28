/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.ir.CallEscapeKind.Empty
import com.huawei.excelsior.jet.compiler.ir.NewEscapeKind.Empty

import scala.language.implicitConversions

sealed trait EscapeKind {
  def /\(escapeKind: EscapeKind): EscapeKind

  def containsReceiverEscape: Boolean

  def containsRetEscape: Boolean

  def transformReceiverEscapeTo(escapeKind: EscapeKind): EscapeKind

  def transformRetEscapeTo(escapeKind: EscapeKind): EscapeKind

  def containsEscape: Boolean

  def containsPotentialEscape: Boolean
}

case class EscapeKindTuple(callEscapeKind: CallEscapeKind, newEscapeKind: NewEscapeKind) extends EscapeKind {
  import CallEscapeKind.*
  import NewEscapeKind.*

  def /\(that: EscapeKind): EscapeKindTuple = that match {
    case x: EscapeKindTuple => EscapeKindTuple(callEscapeKind meet x.callEscapeKind, newEscapeKind meet x.newEscapeKind)
    case x: NewEscapeKind => EscapeKindTuple(callEscapeKind, newEscapeKind meet x)
    case x: CallEscapeKind => EscapeKindTuple(callEscapeKind meet x, newEscapeKind)
  }

  def containsRetEscape: Boolean = newEscapeKind == NoEscape && callEscapeKind.containsRetEscape

  def containsReceiverEscape: Boolean = newEscapeKind == NoEscape && callEscapeKind.containsReceiverEscape

  /** Replace ret-escape part of `this` escape kind by `retEscReplacement`. */
  def transformRetEscapeTo(retEscReplacement: EscapeKind): EscapeKind = this.callEscapeKind match {
    case RetEscape => retEscReplacement
    case RetReceiverEscape => EscapeKindTuple(ReceiverEscape, NewEscapeKind.Empty) /\ retEscReplacement
    case _ => shouldNotReachHere("escape kind does not contain ret-escape")
  }

  /** Replace rcv-escape part of `this` escape kind by `rcvEscReplacement`. */
  override def transformReceiverEscapeTo(rcvEscReplacement: EscapeKind): EscapeKind = this.callEscapeKind match {
    case ReceiverEscape => rcvEscReplacement
    case RetReceiverEscape => EscapeKindTuple(RetEscape, NewEscapeKind.Empty) /\ rcvEscReplacement
    case _ => shouldNotReachHere("escape kind does not contain rcv-escape")
  }

  override def containsEscape: Boolean = newEscapeKind.containsEscape

  override def containsPotentialEscape: Boolean = newEscapeKind.containsPotentialEscape
}

/**
  * <pre>
  *           Empty
  *        /         \
  *   RetEscape ReceiverEscape
  *        \         /
  *     RetReceiverEscape
  * </pre>
  */
enum CallEscapeKind extends EscapeKind {
  case Empty
  case RetEscape
  case ReceiverEscape
  case RetReceiverEscape

  def meet(that: CallEscapeKind): CallEscapeKind = (this, that) match {
    case _ if this == that => this
    case (Empty, x) => x
    case (RetEscape, ReceiverEscape) => RetReceiverEscape
    case (RetReceiverEscape, _) => RetReceiverEscape
    case _ => that meet this
  }

  def /\(that: EscapeKind) = that match {
    case x: CallEscapeKind => this meet x
    case x: EscapeKindTuple => x /\ this
    case x: NewEscapeKind => EscapeKindTuple(this, x)
  }

  override def containsReceiverEscape: Boolean = this match {
    case ReceiverEscape | RetReceiverEscape => true
    case _ => false
  }

  override def containsRetEscape: Boolean = this match {
    case RetEscape | RetReceiverEscape => true
    case _ => false
  }

  override def transformReceiverEscapeTo(escapeKind: EscapeKind): EscapeKind = this match {
    case CallEscapeKind.ReceiverEscape => escapeKind
    case CallEscapeKind.RetReceiverEscape => RetEscape /\ escapeKind
    case _ => shouldNotReachHere("escape kind does not contain rcv-escape")
  }

  override def containsEscape: Boolean = false

  override def transformRetEscapeTo(escapeKind: EscapeKind): EscapeKind = this match {
    case CallEscapeKind.RetEscape => escapeKind
    case CallEscapeKind.RetReceiverEscape => ReceiverEscape /\ escapeKind
    case _ => shouldNotReachHere("escape kind does not contain ret-escape")
  }

  override def containsPotentialEscape: Boolean = false
}

/**
  * <pre>
  *           Empty
  *        /         \
  *   NoEscape GuaranteeEscape
  *        \         /
  *      PotentialEscape
  * </pre>
  */
enum NewEscapeKind extends EscapeKind {
  case Empty
  case NoEscape
  case GuaranteeEscape
  case PotentialEscape

  def meet(that: NewEscapeKind): NewEscapeKind = (this, that) match {
    case _ if this == that => this
    case (Empty, x) => x
    case (GuaranteeEscape, NoEscape | PotentialEscape) => PotentialEscape
    case (NoEscape, PotentialEscape) => PotentialEscape
    case _ => that meet this
  }

  def /\(that: EscapeKind) = that match {
    case x: CallEscapeKind => EscapeKindTuple(x, this)
    case x: EscapeKindTuple => x /\ this
    case x: NewEscapeKind => this meet x
  }

  override def containsReceiverEscape: Boolean = false

  override def containsRetEscape: Boolean = false

  override def transformReceiverEscapeTo(escapeKind: EscapeKind): EscapeKind = shouldNotReachHere("escape kind does not contain rcv-escape")

  override def transformRetEscapeTo(escapeKind: EscapeKind): EscapeKind = shouldNotReachHere("escape kind does not contain ret-escape")

  override def containsPotentialEscape: Boolean = this == PotentialEscape

  override def containsEscape: Boolean = this match {
    case GuaranteeEscape | PotentialEscape => true
    case _ => false
  }
}
