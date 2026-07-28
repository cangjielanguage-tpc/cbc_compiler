/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.pdb.archive

import com.huawei.excelsior.jet.common.XString
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe
import xscala.io.{Path, TextInput, stdout}

import scala.collection.mutable
import scala.util.Using

/** Simple unit tests for `MemIndex`.
  *
  * @author paul
  */
class MemIndexSimpleSuite extends AnyFunSuite with BeforeAndAfter {
  val sampleName = XString("TestName")
  val sampleNames = Seq("TestName", "Some regular string", "a", "abc", "abcde", "abcxy", "axy").map(XString(_))

  var idx: MemIndex = null

  before { // set up
    idx = new MemIndex
  }


  test("Empty") {
    idx.find(sampleName) shouldBe 0
    val ser1 = SerialIndex.from(idx)
    ser1.find(sampleName) shouldBe 0
    val ser2 = SerialIndex.from(ser1)
    ser2.find(sampleName) shouldBe 0
  }

  test("Serial.Empty") {
    val ser1 = SerialIndex.from(idx)
    ser1.iterate { (name, id) => assert(false) }
    val ser2 = SerialIndex.from(ser1)
    ser2.iterate { (name, id) => assert(false) }
  }

  test("AddSameName") {
    idx.find(sampleName) shouldBe 0
    idx.add(sampleName, 123, false) shouldBe 0
    idx.find(sampleName) shouldBe 123
    idx.add(sampleName, 456, false) shouldBe 123
    idx.find(sampleName) shouldBe 123
    idx.add(sampleName, 789, true) shouldBe 123
    idx.find(sampleName) shouldBe 789
    idx.find(XString(s"${sampleName}123")) shouldBe 0
  }

  private def testManyEntries(names: Seq[XString]): Unit = {
    for ((n, i) <- names.zipWithIndex) {
      idx.add(n, i + 1, false)
    }
    val len = names.length
    idx.printStructure(stdout)
    val map = mutable.HashMap.empty[XString, Int]
    idx.iterate((s, id) => map(s) = id)
    println(map)
    map.size shouldBe names.size
    for ((n, i) <- names.zipWithIndex) {
      map(n) shouldBe (i + 1)
    }

    def checkSerial(ser: SerialIndex): Unit = {
      ser.printStructure(stdout)
      var serCnt = 0
      ser.iterate { (s, id) =>
        serCnt += 1
        map(s) shouldBe id
      }
      serCnt shouldBe names.size
    }

    val ser1 = SerialIndex.from(idx)
    checkSerial(ser1)
    checkSerial(SerialIndex.from(ser1))

    val ser2 = SerialIndex.from(idx, normalized = true)
    checkSerial(ser2)
    checkSerial(SerialIndex.from(ser2))
  }

  test("AddManyEntries") {
    testManyEntries(sampleNames)
  }

  test("AddManyEntries2") {
    testManyEntries(sampleNames.reverse)
  }
}
