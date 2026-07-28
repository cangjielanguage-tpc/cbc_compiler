/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.parsing.HandlersTreeMap.reachableHandlers
import com.huawei.excelsior.jet.compiler.Domain
import com.huawei.excelsior.jet.util.SuffixTree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Map from abstract blocks `B` to suffix tree elements representing their exception handlers.
  *
  * @author conwor
  */
class HandlersTreeMap[B](map: mutable.LinkedHashMap[B, SuffixTree[XHInfo[B]]]) {
  def get(b: B): Option[SuffixTree[XHInfo[B]]] = map.get(b)
  def getOrNull(b: B) = get(b).orNull
  def apply(b: B) = map.apply(b)

  // TODO: ensure _.isEmpty?
  def put(b: B, tree: SuffixTree[XHInfo[B]]) = map.put(b, tree)

  def iterator: Iterator[(B, SuffixTree[XHInfo[B]])] = map.iterator

  def isEmpty = map.isEmpty

  def root = map.values.head.getRoot

  /** Optimizes handlers tree by removing unreachable handlers. */
  def optimized(): HandlersTreeMap[B] = {
    val newRoot = SuffixTree.newRoot[XHInfo[B]]()
    new HandlersTreeMap(map.map { case (block, handlers) =>
      (block, newRoot prepend reachableHandlers(handlers))
    })
  }

  def keySet = map.keySet
}

object HandlersTreeMap {
  private val AJTHROWABLE_NAME = XString("com/huawei/excelsior/aj/lang/AJThrowable")
  private val JAVA_LANG_THROWABLE_NAME = XString("java/lang/Throwable")

  def wouldCatchAnyException[B](info: XHInfo[B]): Boolean = {
    if (info.isCatchAll) {
      return true
    }

    if (info.catchTypeName == AJTHROWABLE_NAME) {
      assert(info.domain == Domain.AJ)
      return true
    }

    if (info.catchTypeName == JAVA_LANG_THROWABLE_NAME) {
      assert(info.domain == Domain.JAVA)
      return true
    }

    false
  }

  def reachableHandlers[B](handlers: SuffixTree[XHInfo[B]]): collection.IndexedSeq[XHInfo[B]] = {
    val alreadyCaught = mutable.HashSet.empty[XString]
    val newHandlers = ArrayBuffer.empty[XHInfo[B]]

    for (xhInfo <- handlers.toRoot) {
      if (wouldCatchAnyException(xhInfo)) {
        newHandlers += xhInfo
        return newHandlers
      } else {
        val catchTypeName = xhInfo.catchTypeName
        if (!alreadyCaught.contains(catchTypeName)) {
          newHandlers += xhInfo
          alreadyCaught += catchTypeName
        }
      }
    }
    newHandlers
  }
}