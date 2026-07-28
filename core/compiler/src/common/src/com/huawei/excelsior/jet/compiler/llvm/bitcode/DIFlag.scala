/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.llvm.bitcode

enum DIFlag(val mask: Int):
  case FlagPrivate              extends DIFlag(1)
  case FlagProtected            extends DIFlag(2)
  case FlagPublic               extends DIFlag(3)
  case FlagFwdDecl              extends DIFlag(1 << 2)
  case FlagAppleBlock           extends DIFlag(1 << 3)
  case FlagBlockByrefStruct     extends DIFlag(1 << 4)
  case FlagVirtual              extends DIFlag(1 << 5)
  case FlagArtificial           extends DIFlag(1 << 6)
  case FlagExplicit             extends DIFlag(1 << 7)
  case FlagPrototyped           extends DIFlag(1 << 8)
  case FlagObjcClassComplete    extends DIFlag(1 << 9)
  case FlagObjectPointer        extends DIFlag(1 << 10)
  case FlagVector               extends DIFlag(1 << 11)
  case FlagStaticMember         extends DIFlag(1 << 12)
  case FlagLValueReference      extends DIFlag(1 << 13)
  case FlagRValueReference      extends DIFlag(1 << 14)
  case FlagReserved             extends DIFlag(1 << 15)
  case FlagSingleInheritance    extends DIFlag(1 << 16)
  case FlagMultipleInheritance  extends DIFlag(2 << 16)
  case FlagVirtualInheritance   extends DIFlag(3 << 16)
  case FlagIntroducedVirtual    extends DIFlag(1 << 18)
  case FlagBitField             extends DIFlag(1 << 19)
  case FlagNoReturn             extends DIFlag(1 << 20)
  case FlagMainSubprogram       extends DIFlag(1 << 21)
  case FlagTypePassByValue      extends DIFlag(1 << 22)
  case FlagTypePassByReference  extends DIFlag(1 << 23)
  case FlagEnumClass            extends DIFlag(1 << 24)
  case FlagThunk                extends DIFlag(1 << 25)
  case FlagTrivial              extends DIFlag(1 << 26)
  case FlagBigEndian            extends DIFlag(1 << 27)
  case FlagLittleEndian         extends DIFlag(1 << 28)
  case FlagIndirectVirtualBase  extends DIFlag((1 << 2) | (1 << 5));

object DIFlags {
  def apply(raw: Int) = new DIFlags(raw)

  def apply(flags: DIFlag*) = new DIFlags(flags.map(_.mask) reduce (_ | _))
}

final class DIFlags (val raw: Int) extends AnyVal {
  def contains(v: DIFlag): Boolean = (v.mask & raw) == v.mask

  def containsAll(that: DIFlags): Boolean = (that.raw & raw) == that.raw

  override def toString: String = String.format("0x%x", raw)
}
