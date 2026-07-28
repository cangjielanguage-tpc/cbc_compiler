/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.preparation

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.I32
import com.huawei.excelsior.jet.compiler.{CompilerSuite, CompilerEnvironment}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.IRBuilderDSLBase
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import org.junit.Ignore

/**
  * Tests for Preparation.
  */
@Ignore
trait PreparationSuite extends CompilerSuite with BackEnd with Preparation with CompilerEnvironment { self: IRBuilderDSLBase =>

  private def recursiveStructureEqual(x: Node, y: Node): Boolean =
    (x.proto == y.proto) && (x.tpe == y.tpe) && (x.argsSeq.zip(y.argsSeq) forall {p => recursiveStructureEqual(p._1, p._2)})

  /**
    * Check that after createLea() invocation, `before` node equal to `after` node.
    * `after` node should be passed by name, to avoid createLea() effects on it.
    */
  protected def testLea(before: => Node, after: => Node, loadTpe: AsmType = I32): Unit = {
    resetUniverse()

    val use = currentScope.inState(entryBlock, entryMemory) {
      LoadMemory(loadTpe, SignatureType.Primitive(loadTpe), atomic = false)(before)
    }

    disableTypeChecks()
    createLeaForRMA()

    disableIdentity()
    recombineRematerializeAndGroupRMAAndLea()
    recursiveStructureEqual(use.asInstanceOf[LoadMemory].addr, after) should be (true) // VN does not work in BackEnd
    enableIdentity()
  }

  protected def keyType = AddrType

  protected def foo = Param(keyType, 0)
  protected def bar = Param(keyType, 1)
  protected def baz = Param(keyType, 2)
  protected def const(x: Int) = IntegralConst(keyType)(x)
  protected def iConst(x: Int) = IConst(x)
  protected def lea(base: Node, disp: Int) = Lea.Base(base, disp)
  protected def lea(base: Node, index: Node, scale: Int) = Lea.Scaled(base, index, scale)
  protected def lea(base: Node, index: Node, scale: Int, disp: Int) = Lea.Scaled(base, index, scale, disp)

  def archIndependent(): Unit = {
    // base + disp
    testLea(
      Add(foo, const(928)),
      lea(foo, 928))

    // base + index
    testLea(
      Add(foo, bar),
      lea(foo, bar, 1))

    // base + index*scale
    testLea(
      Add(foo, lsl(bar, 3)),
      lea(foo, bar, 8))

    // index*scale + base
    testLea(
      Add(lsl(foo, 1), bar),
      lea(bar, foo, 2))

    // index*scale1 + base*scale2
    testLea(
      Add(lsl(foo, 2), lsl(bar, 3)),
      lea(lsl(foo, 2), bar, 8))

    // index*scale1 + base*scale2
    // base*scale2 has other uses
    testLea(
      {
        val r = lsl(bar, 3)
        Add(r, r)
        Add(lsl(foo, 2), r)
      },
      lea(lsl(bar, 3), foo, 4))
  }
}
