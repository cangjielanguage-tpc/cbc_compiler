/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.assembler.AssemblerToolbox.*
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind
import org.scalatest.Assertions.*
import org.scalatest.{BeforeAndAfterEach, Suite}
import xscala.util.MathUtils.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Base class for all assembler tests.
  *
  * @author cypok
  * @author conwor
  */
object AssemblerToolbox {
  private def hex(x: Int, width: Int, add0x: Boolean): String = {
    val sb = new StringBuilder
    if (add0x) {
      sb ++= "0x"
    }
    var s: String = Integer.toHexString(x)
    if (width > 0) {
      for (_ <- s.length until width) {
        sb += '0'
      }
      if (width < s.length) {
        s = s.substring(s.length - width)
      }
    }
    sb ++= s
    sb.toString
  }

  private def dumpBytes(bytes: IndexedSeq[Byte], from: Int, to: Int): String = {
    val lineLength = 16
    val from0 = alignDown(from, lineLength)
    val to0 = alignUp(to, lineLength) min bytes.size
    val sb = new StringBuilder
    for (i <- from0 until to0) {
      if (i % lineLength == 0) {
        if (i != from0) {
          sb += '\n'
        }
        sb ++= (hex(i, 4, add0x = false) + ": ")
      } else {
        sb ++= (if (i % lineLength == lineLength / 2) "  " else " ")
      }
      val n = bytes(i)
      sb ++= hex(n & 0xff, 2, add0x = false)
    }
    sb.toString
  }

  private def dumpBytes(bytes: IndexedSeq[Byte], from: Int): String = dumpBytes(bytes, from, bytes.size)

  private def assertBytesEqual(expected: IndexedSeq[Byte])(actual: IndexedSeq[Byte]): Unit = {
    val minSize = expected.size min actual.size
    for (i <- 0 until minSize) {
      if (expected(i) != actual(i)) {
        assertResult(dumpBytes(expected, i), s"bytes[${hex(i, 4, add0x = false)}]") {
          dumpBytes(actual, i)
        }
        fail()
      }
    }
    if (expected.size != actual.size) {
      val i: Int = minSize - 1
      assertResult(dumpBytes(expected, i), s"bytes[$i]") {
        dumpBytes(actual, i)
      }
      fail()
    }
  }

  final case class Zeroes(number: Int)

  final case class TestResult(bytes: IndexedSeq[Byte], fixups: collection.Map[Int, Fixup]) {
    def assertEqualsResults(actual: TestResult): Unit = {
      assertBytesEqual(this.bytes)(actual.bytes)
      assertResult(this.fixups)(actual.fixups)
    }

    def withExtraFixups(extra: Map[Int, Fixup]): TestResult = {
      val newFixups = mutable.HashMap.from(fixups)
      for (position <- extra.keys) {
        assert(!newFixups.contains(position))
        newFixups += (position -> extra(position))
      }
      TestResult(bytes, newFixups.toMap)
    }
  }

  private def segmentToByteList(seg: Segment): IndexedSeq[Byte] = seg.toByteArray.toIndexedSeq

  enum ResultParseFormat {
    case INTEL, ARM64, CBC
  }
}

trait AssemblerToolbox[T >: Null <: Emitter] extends Suite with BeforeAndAfterEach {
  def newSymbol(name: String) = FakeSymbol(name)
  def newSymbol = new FakeSymbol

  def resultParseFormat: ResultParseFormat = fail("result parse format is not specified")

  def relocation(kind: RelocationKind, target: Symbol): Fixup = FakeRelocation(kind, target, 0)

  def zeroes(number: Int) = Zeroes(number)

  def getIntermediateSegmentResult(seg: Segment): TestResult = {
    val fixups = mutable.HashMap.empty[Int, Fixup]
    for (f <- seg.getFixups) {
      assert(!fixups.contains(f.position))
      fixups(f.position) = f
    }
    TestResult(segmentToByteList(seg), fixups)
  }

  def getFinalSegmentResult(seg: Segment): TestResult = {
    val fixups = mutable.HashMap.empty[Int, Fixup]
    seg.finish((position: Int, kind: RelocationKind, target: Symbol) => {
      assert(!fixups.contains(position))
      fixups(position) = relocation(kind, target)
    })
    TestResult(segmentToByteList(seg), fixups)
  }

  def parseTestResult(expectedLines: Any*): TestResult = {
    val bytes = new ArrayBuffer[Byte]
    val fixups = mutable.HashMap.empty[Int, Fixup]

    for (element <- expectedLines) {
      element match {
        case value: Integer =>
          assert(isNBits(value, 8))
          bytes += value.toByte

        case line: String =>
          resultParseFormat match {
            case ResultParseFormat.INTEL => notImplemented("feel free to implement")

            case ResultParseFormat.CBC => notImplemented("feel free to implement")

            case ResultParseFormat.ARM64 =>
              for (j <- 0 until line.length / 2) {
                val value: Int = Integer.parseInt(line.substring(j * 2, (j + 1) * 2), 16)
                assert(isNBits(value, 8))
                bytes += value.toByte
              }
          }

        case fixup: Fixup =>
          assert(!fixups.contains(bytes.size))
          fixups(bytes.size) = fixup

        case zeroes: Zeroes =>
          for (_ <- 0 until zeroes.number) {
            bytes += 0
          }
      }
    }

    TestResult(bytes.toIndexedSeq, fixups)
  }

  final var emit: T = _

  def createEmitter(): T = null

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    emit = createEmitter()
  }

  def freezeAndTearDown(): Segment = emit.freeze().tearDown()

  // TODO: do we really need unit-tests for segment/assembler internal organization (number of fixups, their kind, ...)?
  def checkIntermediate(seg: Segment, expectedLines: Any*): Unit =
    parseTestResult(expectedLines*).assertEqualsResults(getIntermediateSegmentResult(seg))

  def checkFinal(seg: Segment, expectedLines: Any*): Unit = {
    parseTestResult(expectedLines*).assertEqualsResults(getFinalSegmentResult(seg))
  }

  def checkFinal(expectedLines: Any*): Unit = checkFinal(freezeAndTearDown(), expectedLines*)

  def checkCrashOnFinish(): Unit = getFinalSegmentResult(freezeAndTearDown())

  def emitZeroes(number: Int): Unit = emit.emitData(_.putZeroes(number))

  def bini(value: String): Int = {
    val valueCleaned = value.replace("_", "")
    java.lang.Integer.parseUnsignedInt(valueCleaned, 2)
  }

  def binl(value: String): Long = {
    val valueCleaned = value.replace("_", "")
    java.lang.Long.parseUnsignedLong(valueCleaned, 2)
  }
}
