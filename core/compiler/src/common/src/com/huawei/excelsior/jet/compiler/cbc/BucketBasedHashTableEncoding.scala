/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import scala.collection.mutable

/** Bucket-based hash table of file entities which can be accessed by entity name.
  * Buckets are stored flat in a single array. Each bucket is addressing by `start` and `end` indices.
  */
object BucketBasedHashTableEncoding {

  private type bucketId = Int

  private val loadFactor = 0.75

  /** @return hash of string encoded in UTF-8
    *
    * Note: result must be the same as [[com.huawei.excelsior.jet.runtime.jit.cbc.file.registry.BucketBasedHashTable#hash(AJString)]]
    */
  private def hash(name: String) = {
    val bytes = name.getBytes("UTF-8")

    var h = 0
    for (i <- bytes) {
      h = (h << 5) - h + (i & 0xff)
    }
    h
  }

  def encode[T](entities: Iterable[T], name: T => String): (Seq[bucketId], Seq[T]) = {
    val N = entities.knownSize ensuring (_ >= 0)
    val d = ((1 / loadFactor) - 1) ensuring (_ > 0)
    val bucketCount = math.max(((1 + d) * N).toInt, N)

    val buckets = Array.fill(bucketCount)(mutable.Seq.empty[T])
    for (entity <- entities) {
      val idx = (hash(name(entity)) % bucketCount).abs
      buckets(idx) = buckets(idx) :+ entity
    }

    val bucketTable = new Array[bucketId](bucketCount + 1)
    var pointer = 0
    for (i <- buckets.indices) {
      bucketTable(i) = pointer
      pointer += buckets(i).size
    }

    assert(pointer == N)
    bucketTable(bucketCount) = N
    
    (bucketTable.toSeq, buckets.toSeq.flatten)
  }

  def write[T](entities: Iterable[T], name: T => String, offset: T => Int, writeInt: Int => Unit): Unit = {
    val (bucketTable, buckets) = encode(entities, name)
    writeInt(bucketTable.size)
    writeInt(buckets.size)
    bucketTable.foreach(writeInt)
    buckets.foreach(e => writeInt(offset(e)))
  }
}
