/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter

import com.huawei.excelsior.jet.assembler.amd64.GPR
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class ScratchPoolSuite extends AnyFunSuite with BeforeAndAfter {

  var sp: ScratchPool = _

  before {
    sp = ScratchPool.empty()
  }

  private def now(count: Int): Unit = assertResult(count) { sp.available }
  private def acquire(reg: GPR): Unit = assertResult(reg) { sp.acquireScratch() }
  private def release(reg: GPR): Unit = sp.releaseScratch(reg)
  private def append(regs: GPR*): Unit = regs foreach sp.appendScratch
  private def remove(regs: GPR*): Unit = regs foreach sp.removeScratch

  test("empty") {
    now(0)
  }

  test("acquire from empty") {
    assertThrows[ScratchProvider.NoAvailableScratchError] {
      acquire(RAX)
    }
  }

  test("release from empty") {
    assertThrows[AssertionError] {
      release(RAX)
    }
  }

  test("append and remove") {
    now(0)
    append(RAX)
    now(1)
    append(RBX)
    now(2)
    append(RCX)
    now(3)
    append(RDX)
    now(4)
    remove(RCX)
    now(3)
    remove(RAX)
    now(2)
    remove(RDX)
    now(1)
    append(RAX)
    now(2)
    remove(RBX)
    now(1)
  }

  test("simple acquire release in loop") {
    append(RAX)
    for (_ <- 0 until 10) {
      now(1)
      acquire(RAX)
      now(0)
      release(RAX)
      now(1)
    }
  }

  test("simple acquire release two registers in loop") {
    append(RAX, RBX)
    for (i <- 0 until 10) {
      now(2)
      acquire(RAX)
      now(1)
      if (i % 2 == 0) {
        acquire(RBX)
        now(0)
        release(RBX)
        now(1)
      }
      release(RAX)
      now(2)
    }
  }

  test("cross acquire release two registers in loop") {
    append(RAX, RBX)
    for (i <- 0 until 10) {
      now(2)
      acquire(RAX)
      now(1)
      if (i % 2 == 0) {
        acquire(RBX)
        now(0)
      }
      release(RAX)
      if (i % 2 == 0) {
        release(RBX)
      }
      now(2)
    }
  }

  test("cross acquire release remove") {
    append(RAX, RBX, RCX, RDX)
    for (i <- 0 until 10) {
      now(4)
      acquire(RAX)
      now(3)
      if (i % 2 == 0) {
        remove(RBX)
        now(2)
        acquire(RCX)
      } else {
        remove(RCX)
        now(2)
        acquire(RBX)
      }
      now(1)
      release(RAX)
      now(2)
      if (i % 3 == 0) {
        remove(RAX)
        now(1)
        acquire(RDX)
      } else {
        remove(RDX)
        now(1)
        acquire(RAX)
      }
      now(0)
      if (i % 3 == 0) {
        append(RAX)
      } else {
        release(RAX)
      }
      now(1)
      if (i % 2 == 0) {
        append(RBX)
        now(2)
        release(RCX)
      } else {
        release(RBX)
        now(2)
        append(RCX)
      }
      now(3)
      if (i % 3 == 0) {
        release(RDX)
      } else {
        append(RDX)
      }
      now(4)
      remove(RCX, RAX, RDX, RBX)
      now(0)
      append(RAX, RBX, RCX, RDX)
      now(4)
    }
  }
}
