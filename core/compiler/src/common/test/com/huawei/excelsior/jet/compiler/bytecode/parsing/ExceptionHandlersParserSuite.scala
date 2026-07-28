/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.{CompilerSuite, Domain}
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute.ExceptionTableTraverserArrayImpl
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.ExceptionInfo
import com.huawei.excelsior.jet.util.SuffixTree

import scala.collection.mutable

/**
 * Tests for ExceptionHandlersBuilder.
 */
class ExceptionHandlersParserSuite extends CompilerSuite {

  class Block
  case class BytecodeBlock(startPC: Int, endPC: Int) extends Block

  private var allBytecodeBlocks: mutable.Set[Block] = _
  private var eiSeq: mutable.ArrayBuffer[ExceptionInfo] = _
  private var hSeq: mutable.ArrayBuffer[Block] = _

  override def beforeEach(): Unit = {
    super.beforeEach()

    allBytecodeBlocks = mutable.Set.empty
    eiSeq = mutable.ArrayBuffer.empty
    hSeq = mutable.ArrayBuffer.empty
  }

  val codeAttr: MethodCodeAttribute = new MethodCodeAttribute {
    override def maxStack = shouldNotCallThis()
    override def maxLocals = shouldNotCallThis()
    override def bytecodeLength = shouldNotCallThis()
    override def bytecodeArray = shouldNotCallThis()
    override def bytecodeStart = shouldNotCallThis()

    override def hasExceptionTable = eiSeq.nonEmpty
    override def getExceptionTableTraverser = new ExceptionTableTraverserArrayImpl[ExceptionInfo](eiSeq.toArray) {
      override def startPC(x: ExceptionInfo): Int = x.startPC
      override def endPC(x: ExceptionInfo): Int = x.endPC
      override def handlerPC(x: ExceptionInfo): Int = x.handlerPC
      override def catchTypeIndex(x: ExceptionInfo): Int = x.catchTypeIndex
      override def catchTypeName(x: ExceptionInfo): XString = x.catchTypeName
    }

    override def stackMapTable: Array[Byte] = shouldNotCallThis()
  }

  private var _catchTypeIndex = 1
  private def nextCatchType(): (Int, String) = {
    val idx = _catchTypeIndex
    _catchTypeIndex += 1
    (idx, s"Exception$idx")
  }

  private def nextCatchTypeIndexFor(catchTypeName: String): Int = {
    if (catchTypeName == null) {
      0
    } else {
      nextCatchType()._1
    }
  }

  def exception(startPC: Int, endPC: Int, handlerPC: Int, catchTypeIndex: Int, catchTypeName: String, handler: Block, domain: Domain): XHInfo[Block] = {
    val catchTypeNameX = if (catchTypeName != null) XString.ascii(catchTypeName) else null
    eiSeq += ExceptionInfo(startPC, endPC, handlerPC, catchTypeIndex, catchTypeNameX)
    hSeq += handler
    new XHInfo(catchTypeIndex, catchTypeNameX, handler, domain)
  }

  def exception(startPC: Int, endPC: Int, handlerPC: Int, catchTypeName: String, handler: Block, domain: Domain): XHInfo[Block] =
    exception(startPC, endPC, handlerPC, nextCatchTypeIndexFor(catchTypeName), catchTypeName, handler, domain)

  def exception(startPC: Int, endPC: Int, handlerPC: Int, catchTypeName: String, domain: Domain): XHInfo[Block] =
    exception(startPC, endPC, handlerPC, nextCatchTypeIndexFor(catchTypeName), catchTypeName, block(), domain)

  def exception(startPC: Int, endPC: Int, handlerPC: Int, handler: Block, domain: Domain): XHInfo[Block] = {
    val catchType = nextCatchType()
    exception(startPC, endPC, handlerPC, catchType._1, catchType._2, handler, domain)
  }

  def exception(startPC: Int, endPC: Int, handlerPC: Int, domain: Domain = Domain.JAVA): XHInfo[Block] =
    exception(startPC, endPC, handlerPC, block(), domain)

  def block(): Block = new Block

  def block(startPC: Int, endPC: Int): Block = {
    val block = BytecodeBlock(startPC, endPC)
    allBytecodeBlocks += block
    block
  }

  def makeTree(domain: Domain = Domain.JAVA): HandlersTreeMap[Block] = {
    val (raw, optim) = makeTreeOptimized(domain)
    raw.keySet should be (optim.keySet)
    for (b <- raw.keySet) {
      raw(b).toRoot.toSeq should be (optim(b).toRoot.toSeq)
    }
    raw
  }

  def makeTreeOptimized(domain: Domain = Domain.JAVA): (HandlersTreeMap[Block], HandlersTreeMap[Block]) = {
    val parser = new ExceptionHandlersParser[Block](codeAttr) {
      protected override def exceptionHandlerBlocks = hSeq.iterator

      override def blockStartPC(block: Block) = block match {
        case BytecodeBlock(startPC, _) => startPC
      }

      override def blockEndPC(block: Block) = block match {
        case BytecodeBlock(_, endPC) => endPC
      }
    }
    val tree = parser.makeHandlersTree(allBytecodeBlocks, domain)
    (tree, tree.optimized())
  }

  def checkTreePath(actualExs: SuffixTree[XHInfo[Block]], expectedExs: XHInfo[Block]*): Unit = {
    actualExs.toRoot.toSeq should be(expectedExs)
  }

  test("build tree 0") {
    val b1 = block(0, 5)

    val map = makeTree(Domain.JAVA)

    map.get(b1) should be (None)
  }

  test("build tree 1") {
    val ex1 = exception(0, 5, 100)
    val b1 = block(0, 5)

    val map = makeTree()

    checkTreePath(map(b1), ex1)
  }

  test("build tree 2") {
    val ex1 = exception( 0,  5, 100, Domain.AJ)
    val ex2 = exception(10, 15, 110, Domain.AJ)
    val ex3 = exception(20, 25, 120, Domain.AJ)
    val b1 = block( 0,  5)
    val b2 = block(10, 15)
    val b3 = block(20, 25)

    val map = makeTree(Domain.AJ)

    checkTreePath(map(b1), ex1)
    checkTreePath(map(b2), ex2)
    checkTreePath(map(b3), ex3)
  }

  test("build tree 3") {
    val ex1 = exception( 0,  5, 100)
    val ex2 = exception(10, 15, 110)
    val ex3 = exception(20, 25, 120)
    val b1 = block( 0,  2)
    val b2 = block(10, 12)
    val b3 = block(20, 22)
    val b4 = block( 2,  5)
    val b5 = block(12, 15)
    val b6 = block(22, 25)

    val map = makeTree()

    checkTreePath(map(b1), ex1)
    checkTreePath(map(b2), ex2)
    checkTreePath(map(b3), ex3)
    checkTreePath(map(b4), ex1)
    checkTreePath(map(b5), ex2)
    checkTreePath(map(b6), ex3)
  }

  test("build tree 4") {
    val exs = for (x <- 0 until 5) yield for (y <- 0 until 5-x) yield exception(y, y+x+1, x*10 + y)
    val bs = for (x <- 0 until 5) yield block(x, x+1)

    val map = makeTree()

    checkTreePath(map(bs(0)), exs(0)(0), exs(1)(0),            exs(2)(0),                       exs(3)(0),            exs(4)(0))
    checkTreePath(map(bs(1)), exs(0)(1), exs(1)(0), exs(1)(1), exs(2)(0), exs(2)(1),            exs(3)(0), exs(3)(1), exs(4)(0))
    checkTreePath(map(bs(2)), exs(0)(2), exs(1)(1), exs(1)(2), exs(2)(0), exs(2)(1), exs(2)(2), exs(3)(0), exs(3)(1), exs(4)(0))
    checkTreePath(map(bs(3)), exs(0)(3), exs(1)(2), exs(1)(3), exs(2)(1), exs(2)(2),            exs(3)(0), exs(3)(1), exs(4)(0))
    checkTreePath(map(bs(4)), exs(0)(4), exs(1)(3),            exs(2)(2),                       exs(3)(1),            exs(4)(0))
  }

  test("build tree 5 (equal types and handlers)") {
    val type1 = "ExFoo"
    val handler1 = block()
    val ex1 = exception(0, 5, 100, catchTypeName = type1, handler = handler1, Domain.JAVA)
    val _ = exception(10, 15, 100, catchTypeName = type1, handler = handler1, Domain.JAVA)
    val b1 = block(0, 5)
    val b2 = block(10, 15)

    val map = makeTree()

    checkTreePath(map(b1), ex1)
    checkTreePath(map(b2), ex1)
  }

  test("build tree 6 (equal names with diffirent indices and different handlers") {
    val type1 = "ExFoo"
    val handler1 = block()
    val handler2 = block()
    val ex1 = exception(5,  10, 100, catchTypeIndex = 1, catchTypeName = type1, handler = handler1, Domain.JAVA)
    val ex2 = exception(0,  10, 105, catchTypeIndex = 2, catchTypeName = type1, handler = handler2, Domain.JAVA)
    val _   = exception(10, 15, 110, catchTypeIndex = 3, catchTypeName = type1, handler = handler2, Domain.JAVA)
    val ex3 = exception(0,  15, 115)
    val b1 = block(0, 5)
    val b2 = block(5, 10)
    val b3 = block(10, 15)

    val (map, mapOptim) = makeTreeOptimized()

    checkTreePath(map(b1),      ex2, ex3)
    checkTreePath(map(b2),      ex1, ex2, ex3)
    checkTreePath(map(b3),      ex2, ex3)

    checkTreePath(mapOptim(b1), ex2, ex3)
    checkTreePath(mapOptim(b2), ex1, ex3)
    checkTreePath(mapOptim(b3), ex2, ex3)
  }

  test("build tree with catch any") {
    val ex1 = exception(5, 10, 100, catchTypeName = null, Domain.JAVA)
    val ex2 = exception(0, 15, 105)
    val b = block(5, 10)

    val (map, mapOptim) = makeTreeOptimized()

    checkTreePath(map(b),      ex1, ex2)

    checkTreePath(mapOptim(b), ex1)
  }

  test("build tree with catch throwable aj") {
    val ex1 = exception(5, 10, 100, catchTypeName = "com/huawei/excelsior/aj/lang/AJThrowable", Domain.AJ)
    val ex2 = exception(0, 15, 105, Domain.AJ)
    val b = block(5, 10)

    val (map, mapOptim) = makeTreeOptimized(Domain.AJ)

    checkTreePath(map(b),      ex1, ex2)

    checkTreePath(mapOptim(b), ex1)
  }

  test("build tree with catch throwable java") {
    val ex1 = exception(5, 10, 100, catchTypeName = "java/lang/Throwable", Domain.JAVA)
    val ex2 = exception(0, 15, 105, Domain.JAVA)
    val b = block(5, 10)

    val (map, mapOptim) = makeTreeOptimized(Domain.JAVA)

    checkTreePath(map(b),      ex1, ex2)

    checkTreePath(mapOptim(b), ex1)
  }

  test("get root") {
    val ex1 = exception(5, 10, 110)
    val ex2 = exception(0, 15, 100)
    val ex3 = exception(20, 25, 120)
    val b1 = block(0, 5)
    val b2 = block(5, 10)
    val b3 = block(20, 25)

    val map = makeTree()

    checkTreePath(map(b1), ex2)
    checkTreePath(map(b2), ex1, ex2)
    checkTreePath(map(b3), ex3)

    map.root.getChildren.map(_.elem).toSet should be (Set(ex2, ex3))
  }
}
