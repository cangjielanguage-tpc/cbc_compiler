/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter

import com.huawei.excelsior.jet.assembler.AssemblerToolbox.TestResult
import com.huawei.excelsior.jet.assembler.{AssemblerToolbox, Emitter, FakeSymbol, Label, Symbol}
import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind
import org.scalatest.funsuite.AnyFunSuite

trait CodeEmitterToolbox[T >: Null <: Emitter] extends AnyFunSuite with AssemblerToolbox[T] {

  val symbol = new FakeSymbol
  val directSymbol = FakeSymbol("directSymbol")
  val farSymbol = FakeSymbol("farSymbol")

  final class FakeSymbolInfo extends SymbolInfo {
    override def accessKind(symbol: Symbol) = if (symbol == farSymbol) AccessKind.FAR else AccessKind.DIRECT
  }

  def scratchesNumber: Int

  def scratchProvider: ScratchProvider

  def getResult: TestResult = {
    assert(scratchProvider.has(scratchesNumber))
    getFinalSegmentResult(freezeAndTearDown())
  }

  protected def testSame(name: String)(actual: => Unit)(expected: => Unit): Unit = {
    test(name) {
      beforeEach()
      actual
      val result = getResult
      beforeEach()
      expected
      getResult.assertEqualsResults(result)
    }
  }

  protected def testBranchSame(name: String)(actual: Label => Unit)(expected: Label => Unit): Unit = {
    test(name) {
      beforeEach()
      actual(emit.newBoundLabel)
      val actualResult = getResult
      beforeEach()
      expected(emit.newBoundLabel)
      getResult.assertEqualsResults(actualResult)
    }
  }

  protected def testBranchThrows(name: String)(actual: Label => Unit)(expected: Label => Unit): Unit = {
    test(name) {
      assertThrows[AssertionError] {
        beforeEach()
        actual(emit.newBoundLabel)
        val actualResult = getResult
        beforeEach()
        expected(emit.newBoundLabel)
        getResult.assertEqualsResults(actualResult)
      }
    }
  }
}
