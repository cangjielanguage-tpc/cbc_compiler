/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

/** Condition code: used in jcc, setcc, cmovcc instructions
  *
  * @author paul
  * @author cypok
  */
enum CC(_code: Int) {
  case O  extends CC(0x0)
  case NO extends CC(0x1)
  
  case B  extends CC(0x2)
  case AE extends CC(0x3)
  case E  extends CC(0x4)
  case NE extends CC(0x5)
  case BE extends CC(0x6)
  case A  extends CC(0x7)
  
  case S  extends CC(0x8)
  case NS extends CC(0x9)
  case P  extends CC(0xA)
  case NP extends CC(0xB)
  
  case L  extends CC(0xC)
  case GE extends CC(0xD)
  case LE extends CC(0xE)
  case G  extends CC(0xF)
  
  case C  extends CC(B)
  case NC extends CC(AE)
  case Z  extends CC(E)
  case NZ extends CC(NE)
  
  case PE extends CC(P)
  case PO extends CC(NP)
  
  case NA  extends CC(BE)
  case NAE extends CC(B)
  case NB  extends CC(AE)
  case NBE extends CC(A)
  
  case NG  extends CC(LE)
  case NGE extends CC(L)
  case NL  extends CC(GE)
  case NLE extends CC(G)

  def this(another: CC) = this(another.code)

  val code = _code.toByte

  def opposite() = {
    val oppositeCode = code ^ 1
    CC.VALUES find (_.code == oppositeCode) getOrElse {
      throw new AssertionError("Unable to calculate opposite CC for " + this)
    }
  }
}

object CC {
  private val VALUES = values
}
