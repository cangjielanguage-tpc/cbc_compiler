/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.internal

/** Traceable reference to the entity of foreign language (AJ, first of all). */
opaque type ForeignRef0 = ForeignRefType0

private abstract class ForeignRefType0

private class Wrapper(val ref: ForeignRef0)

def wrapForeign(x: ForeignRef0): AnyRef =
  if (x == null) null else Wrapper(x)

def unwrapForeign(x: AnyRef): ForeignRef0 = x match {
  case null => null
  case x: Wrapper => x.ref
}
