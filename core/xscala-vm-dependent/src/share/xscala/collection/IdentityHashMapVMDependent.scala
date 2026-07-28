/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.collection

import xscala.vm.VMDependent

import scala.collection.mutable

/** Implementation of mutable hashtable map with the keys compared as references, not objects.  */
trait IdentityHashMap[K <: AnyRef, V] extends mutable.Map[K, V]

object IdentityHashMap {
  def empty[K <: AnyRef, V]: IdentityHashMap[K, V] = IdentityHashMapVMDependent.get.empty[K, V]
}

trait IdentityHashMapVMDependent {
  def empty[K <: AnyRef, V]: IdentityHashMap[K, V]
}

object IdentityHashMapVMDependent extends VMDependent[IdentityHashMapVMDependent]
