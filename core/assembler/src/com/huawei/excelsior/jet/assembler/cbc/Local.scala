/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.Register.*

import scala.collection.mutable

/** Enumeration of all local slots (aka virtual registers or CBC variables) of a CBC method.
  * Used by old-style [[Assembler]] instructions which operate with a local value.
  *
  * @deprecated To be replaced with direct use of [[Register.IR]], [[Register.FR]] and enumeration of stack slots.
  */
trait Local

object Local {

  object LocX {
    private val instances = mutable.HashMap.empty[Integer, LocX]

    // Map all registers to CBC locals: iregs in first order and fregs in second.
    def apply(r: IR): LocX = apply(r.idx)
    def apply(r: FR): LocX = apply(r.idx + IR.count)

    def apply(encoding: Int): LocX = {
      assert(0 <= encoding && encoding <= 0xffff)
      instances.getOrElseUpdate(encoding, {
        if (encoding <= 0xf) new Loc4(encoding)
        else if (encoding <= 0xff) new Loc8(encoding)
        else new Loc16(encoding)
      })
    }
  }

  sealed abstract class LocX(val encoding: Int) extends Local {
    override def toString = "v" + encoding
  }

  class Loc16 private[Local](_encoding: Int) extends LocX(_encoding)
  class Loc8  private[Local](_encoding: Int) extends Loc16(_encoding)
  class Loc4  private[Local](_encoding: Int) extends Loc8(_encoding)

  object Loc16 {
    def apply(encoding: Int): Loc16 = LocX(encoding).asInstanceOf[Loc16]
    def apply(r: IR): Loc16 = LocX(r).asInstanceOf[Loc16]
    def apply(r: FR): Loc16 = LocX(r).asInstanceOf[Loc16]
  }

  object Loc8 {
    def apply(encoding: Int): Loc8  = LocX(encoding).asInstanceOf[Loc8]
    def apply(r: IR): Loc8 = LocX(r).asInstanceOf[Loc8]
    def apply(r: FR): Loc8 = LocX(r).asInstanceOf[Loc8]
  }

  object Loc4 {
    def apply(encoding: Int): Loc4  = LocX(encoding).asInstanceOf[Loc4]
    def apply(r: IR): Loc4 = LocX(r).asInstanceOf[Loc4]
    def apply(r: FR): Loc4 = LocX(r).asInstanceOf[Loc4]
  }

}
