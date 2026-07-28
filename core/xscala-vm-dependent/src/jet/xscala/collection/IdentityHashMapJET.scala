/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.collection

import collection.mutable

final class IdentityHashMapImpl[K <: AnyRef, V] extends mutable.HashMap[K, V] with IdentityHashMap[K, V] {
  override def basicHash(o: K) = System.identityHashCode(o)
}

private [xscala] class IdentityHashMapJET extends IdentityHashMapVMDependent {
  override def empty[K <: AnyRef, V]: IdentityHashMap[K, V] = new IdentityHashMapImpl[K, V]
}
