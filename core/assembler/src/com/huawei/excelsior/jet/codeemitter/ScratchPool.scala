/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter

import com.huawei.excelsior.jet.assembler.Location.IReg
import com.huawei.excelsior.jet.codeemitter.ScratchPool.MAX_REGS

/** Basic scratch provider implementation.
  *
  * @author paul
  */
object ScratchPool {
  private val MAX_REGS = 8 // should be enough for everyone

  def empty() = new ScratchPool
  def _apply(regs: IReg*) = empty().appendScratches(regs)

  // TODO: remove after translation to Scala
  def apply(r1: IReg) = _apply(r1)
  def apply(r1: IReg, r2: IReg) = _apply(r1, r2)
  def apply(rs: Array[_ <: IReg]) = _apply(rs.toIndexedSeq: _*)
}

class ScratchPool private(onAcquire: IReg => Unit = _ => {}) extends ScratchProvider {
  private val regs = new Array[IReg](MAX_REGS)
  private val busy = new Array[Boolean](MAX_REGS)
  private var nregs = 0
  private var nfree = 0
  private var ifree = 0 // index of first !busy reg or `nregs` if `nfree == 0`

  //                                             nregs
  //                                               |
  //                                               V
  // regs: [s0,   s1,   s2,    ..., sN-1, sN,    null,  null,  ...,  null]
  // busy: [true, true, false, ..., true, false, false, false, ..., false],   nfree = count of false in range [0, nregs)
  //                      ^
  //                      |
  //     all busy <---  ifree  ---> may be busy or free
  //                 (first free)

  def appendScratches(regs: Iterable[IReg]) = { regs foreach appendScratch; this }

  def withOnAcquire(onAcquire: IReg => Unit) = new ScratchPool(onAcquire).appendScratches(allScratches)

  override def available = nfree

  override def contains(r: IReg): Boolean = regs.contains(r)

  override def allScratches = regs.filter(_ != null)

  override def acquireScratch(): IReg = {
    if (nfree == 0) {
      throw new ScratchProvider.NoAvailableScratchError
    }
    val i = ifree
    assert(i < nregs && !busy(i))
    busy(i) = true
    nfree -= 1
    while (ifree < nregs && busy(ifree)) ifree += 1
    onAcquire(regs(i))
    regs(i)
  }

  override def releaseScratch(scratch: IReg): Unit = {
    for (i <- ifree - 1 to 0 by -1) {
      assert(busy(i))
      if (regs(i) == scratch) {
        busy(i) = false
        nfree += 1
        ifree = i
        return
      }
    }

    for (i <- ifree + 1 until nregs) {
      if (busy(i) && (regs(i) == scratch)) {
        busy(i) = false
        nfree += 1
        return
      }
    }

    assert(false)
  }

  override def appendScratch(scratch: IReg): Unit = {
    assert(nregs < MAX_REGS, "Why u want so many scratches? R u crazy or what?")
    assert(!contains(scratch))
    busy(nregs) = false
    regs(nregs) = scratch
    nregs += 1
    nfree += 1
  }

  override def removeScratch(scratch: IReg): Unit = {
    assert(nregs > 0 && nfree > 0)
    var i = nregs - 1

    // Search from highest indices to improve LIFO-like usages
    while (i >= 0 && (regs(i) ne scratch)) i -= 1

    assert(i >= 0 && !busy(i))
    nregs -= 1
    nfree -= 1
    if (i == nregs) {
      regs(i) = null
    } else {
      if (ifree >= i) {
        ifree = -1
      }
      while (i < nregs) {
        regs(i) = regs(i + 1)
        busy(i) = busy(i + 1)
        if (ifree < 0 && !busy(i)) {
          ifree = i
        }
        i += 1
      }
      if (ifree < 0) {
        ifree = nregs
      }
      regs(nregs) = null
    }
  }
}