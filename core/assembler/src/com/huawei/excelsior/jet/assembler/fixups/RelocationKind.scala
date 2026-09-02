/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.fixups

import com.huawei.excelsior.common.Arch.ARM64
import com.huawei.excelsior.jet.assembler.Width.W16
import com.huawei.excelsior.jet.assembler.Width.W32
import com.huawei.excelsior.jet.assembler.Width.W64
import com.huawei.excelsior.jet.assembler.Width.WNONE
import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.jet.assembler.Width

// TODO: separate fixups to internal and external when CODE_SEGM will be dead
/** Enumerates supported relocation kinds. */
enum RelocationKind(val width: Width = WNONE, private val supportedArch: Arch = null) {
  case ADDR32          extends RelocationKind(W32)
  case ADDR64          extends RelocationKind(W64)
  case OFFS32          extends RelocationKind(W32)
  case CODE_OFFS32     extends RelocationKind(W32) // Like OFFS32, but guaranteed that target is code and source is call/jump
  case TD_INDEX_16     extends RelocationKind(W16)
  case TD_REL_16       extends RelocationKind(W16)
  case TD_REL_32       extends RelocationKind(W32)
  case TD_REL_32_DEL   extends RelocationKind(W32)
  case BYTE_STR_32     extends RelocationKind(W32)
  case RVA_32          extends RelocationKind(W32)
  case OFFS32_LOCAL    extends RelocationKind(W32) // Must be resolved before writing .obj file TODO: try to remove
  case OFFS32_IN_SEG   extends RelocationKind(W32) // Position of target label in it's segment

  // CBC specific fixups
  case CBC_ID16        extends RelocationKind(W16)
  case CBC_ID32        extends RelocationKind(W32)

  // ARM64-specific
  case ARM64_B_BL_IMM      extends RelocationKind(supportedArch = ARM64)
  case ARM64_ADRP_IMM      extends RelocationKind(supportedArch = ARM64)
  case ARM64_ADD_IMM_LO12  extends RelocationKind(supportedArch = ARM64)

  // DWARF format fixups
  case DWARF_SECTION       extends RelocationKind(W32)

  /** Checks whether this relocation kind is supported on the specified architecture. */
  def supportedOn(arch: Arch) = supportedArch == null || supportedArch == arch
}
