/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.pdb.archive

import com.huawei.excelsior.jet.common.XString
import xscala.io.{ByteBuffer, LEB128Encoder}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import SerialIndex.*
import Index.*

object SerialIndex {
  // the data stream contains 5 types of records: FWD record & 4 special record types
  // general record format: {
  //   byte op = { B: 1 bit, L: 1 bit; nameLen: 6 bits }
  //   if (nameLen != 0) { // FWD record
  //     if (B) branchDisp: uleb128
  //     nameChars: byte[nameLen]
  //     branchBase:
  //     if (L) leafData: byte[...]
  //   } else (B,L) match { // nameLen == 0  =>  special record
  //     case (0,0) => // END record
  //       // no more data, the whole record is one zero byte
  //     case (1,x) => // JUMP | LCONT record
  //       branchDisp: uleb128
  //       branchBase:
  //       if (L) leafData: byte[...]
  //     case (0,1) => // BISECT record
  //       pivot: byte
  //       branchDisp: uleb128
  //       branchBase:
  //   }

  //   leafData = leafID: uleb128 //TODO: generalise
  //   branchTarget = branchBase + branchDisp

  // semantics:
  // val testCh = if (searchName.length > 0) searchName.head else 0
  // if (nameLen != 0) { // FWD record:
  //   if (B && testCh > nameChars.head) { // unsigned compare
  //     setPos(branchTarget)
  //   } else if (!searchName.startsWith(nameChars)) {
  //     searchFailed()
  //   } else if (L) {
  //     if (searchName.length == nameLen) searchSuccess(leafData) else searchFailed()
  //   } else {
  //     searchName = searchName.substring(nameLen)
  //     continue with next record
  //   }
  // } else op match {
  //   case OP_END =>
  //     searchFailed()
  //   case OP_JUMP =>
  //     setPos(branchTarget)
  //   case OP_LCONT =>
  //     if (searchName.length == 0) searchSuccess(leafData) else setPos(branchTarget)
  //   case OP_BISECT =>
  //     if (testCh > pivot) setPos(branchTarget)
  //     else continue with next record
  // }

  private val TAG_B = 1 << 7
  private val TAG_L = 1 << 6
  private val TAG_MASK = TAG_B | TAG_L
  private val LEN_MASK = ~TAG_MASK & 0xff
  private val LEN_MAX = LEN_MASK

  private val OP_END = 0
  private val OP_JUMP = TAG_B
  private val OP_LCONT = TAG_B | TAG_L
  private val OP_BISECT = TAG_L

  class Stats {
    var bytesCount = 0
    var leafCount = 0
    var recordCount = 0
    var charsCount = 0
    var longNameCount = 0
    var leafContCount = 0

    def print(header: String): Unit = {
      println(s"SerialIndex: bytes: $bytesCount, chars: $charsCount, leafs: $leafCount [LC: $leafContCount], records: $recordCount [LN: $longNameCount]")
      if (header != null) {
        val size = Utils.sizeStr(bytesCount)
        println(s"$header$size")
      }
    }
  }

  /** Convert `Index` to serialized form. */
  def from(idx: Index, normalized: Boolean = false) = {
    val b = new Builder(idx, normalized)
    new SerialIndex(b.buf.toByteArray, b.stats)
  }

  def from2[I <: Index](idx: Index.WithSuffix[I], normalized: Boolean = false) = {
    Index.withSuffix(from(idx.dir, normalized), idx.suffix)
  }

  private def ulebSize(x: Int) = LEB128Encoder.calcSizeULEB128(x)

  private object Builder {
    class Record (var id: NodeID, var chars: Array[Byte], var charsStart: Int, var charsCnt: Int) {
      var leafID = -1
      val children = ArrayBuffer.empty[Record]
      var treeSize = -1
      var wholeSize = -1

      def isLeaf = leafID >= 0

      def leafData = { assert(isLeaf); leafID }

      def branchDisp = {
        assert(treeSize >= 0)
        if (isLeaf) { assert(treeSize == 0); ulebSize(leafData) } else treeSize
      }

      def op(hasBranch: Boolean) = {
        var op = charsCnt & LEN_MASK
        assert(op == charsCnt)
        if (hasBranch) op |= TAG_B
        if (isLeaf) op |= TAG_L
        op.toByte
      }

      def setChars(chars: Array[Byte], charsStart: Int, charsCnt: Int): Unit = {
        this.chars = chars
        this.charsStart = charsStart
        this.charsCnt = charsCnt
      }

      def addPrefixChars(p: Record): Unit = {
        val newChars = new Array[Byte](p.charsCnt + charsCnt)
        Array.copy(p.chars, p.charsStart, newChars, 0, p.charsCnt)
        Array.copy(chars, charsStart, newChars, p.charsCnt, charsCnt)
        setChars(newChars, 0, newChars.length)
      }
    }
  }

  private class Builder(source: Index, normalized: Boolean) extends Processor {
    import Builder.*

    val buf = ByteBuffer()
    val stats = new Stats

    private val ctx = ArrayBuffer.empty[Record]
    private def push(r: Record): Unit = { ctx += r }
    private def peek(id: NodeID): Record = ctx.last ensuring (_.id == id)
    private def pop(id: NodeID): Record = ctx.remove(ctx.length - 1) ensuring (_.id == id)

    def build(): Unit = {
      source.traverse(this)

      assert(ctx.length == 1)
      var root = ctx.last
      assert(root.charsCnt == 0)
      if (root.children.length == 1 && root.children.head.charsCnt == 0) {
        root = root.children.head
        assert(!root.isLeaf)
      }

      root = normalize(root, true, normalized)
      computeSizes(root)
      if (root.children.nonEmpty) { // skip on empty directory
        writeSubTree(root)
      }
      buf.putByte(OP_END)

      stats.bytesCount = buf.length
    }

    { build() }

    override def root(rootID: NodeID): Unit = {
      assert(ctx.isEmpty)
      push(new Record(rootID, null, 0, 0))
    }

    override def forward(dstID: NodeID, chars: Array[Byte], start: Int, charsCnt: Int): Unit = {
      push(new Record(dstID, chars, start, charsCnt))
    }

    override def leaf(nodeID: NodeID, entryID: EntryID): Unit = {
      assert(entryID >= 0)
      peek(nodeID).leafID = entryID
    }

    override def backward(srcID: NodeID, dstID: NodeID, charsCnt: Int): Unit = {
      val n = pop(srcID)
      val p = peek(dstID)
      p.children += n
    }

    def normalize(p0: Record, isRoot: Boolean, mergeLongNames: Boolean): Record = {
      var p = p0
      p.children.mapInPlace(normalize(_, false, mergeLongNames))

      p.children.sortInPlaceWith { (x, y) =>
        assert(x.charsCnt > 0 && y.charsCnt > 0)
        val (cx, cy) = (x.chars(x.charsStart), y.chars(y.charsStart))
        assert(cx != cy)
        cx < cy
      }

      if (!isRoot && !p.isLeaf && p.children.length == 1) {
        val c = p.children.head
        if (mergeLongNames && p.charsCnt + c.charsCnt <= LEN_MAX) {
          c.addPrefixChars(p)
          c.id = p.id * 1000 + c.id
          p = c
        } else {
          // TODO: move some chars from `c` to `p`
          stats.longNameCount += 1
        }
      }

      if (p.isLeaf && p.children.nonEmpty) {
        val lc = new Record(-p.id, null, 0, 0)
        lc.leafID = p.leafID
        p.leafID = -1
        p.children.insert(0, lc)
        stats.leafContCount += 1
      }

      return p
    }

    def computeSizes(p: Record): Unit = {
      assert(p.treeSize == -1)
      var treeSize = 0
      for (r <- p.children) {
        assert(r.wholeSize == -1)
        computeSizes(r)
        var s = r.branchDisp
        if (r != p.children.last) {
          s += ulebSize(s)
        }
        r.wholeSize = s + r.charsCnt + 1 /*op*/
        treeSize += r.wholeSize
      }
      p.treeSize = treeSize
    }

    def writeSubTree(p: Record): Unit = {
      for (r <- p.children) {
        stats.recordCount += 1
        assert(r.wholeSize > 0)
        assert(r.isLeaf || r.charsCnt > 0)

        val isLast = r == p.children.last
        val startPos = buf.length
        buf.putByte(r.op(!isLast))
        if (!isLast) {
          buf.putULEB(r.branchDisp)
        }
        if (r.charsCnt > 0) {
          buf.putBytes(r.chars, r.charsStart, r.charsCnt)
        }
        stats.charsCount += r.charsCnt
        if (r.isLeaf) {
          assert(r.children.isEmpty)
          stats.leafCount += 1
          buf.putULEB(r.leafData)
        } else {
          writeSubTree(r)
        }
        assert(buf.length == startPos + r.wholeSize)
      }
    }
  }

  private class Walker(var data: Array[Byte], var dataPos: Int = 0) {
    private def peekByte() = data(dataPos) & 0xFF

    private def fetchByte() = { dataPos += 1; data(dataPos - 1) & 0xFF }

    private def fetchULEB(): Int = LEB128Encoder.decodeULEB128(() => fetchByte())

    def decodeLeaf(): Int = {
      val leafData = fetchULEB() // TODO: unpack leafData elsewhere
      leafData
    }

    def find(name: XString): Int = {
      dataPos = 0
      val found = doFind(XString.unsafeGetValue(name), XString.unsafeGetOffset(name), name.length)
      if (found) decodeLeaf() else 0
    }

    private def doFind(query: Array[Byte], queryStart: Int, queryLength: Int): Boolean = {
      assert(queryLength > 0)
      var queryPos = queryStart
      val queryEnd = queryStart + queryLength

      def queryDone = queryPos == queryEnd
      def queryHead = if (queryDone) 0 else query(queryPos) & 0xFF

      def matchName(length: Int): Boolean = {
        val stop = queryPos + length
        (stop <= queryEnd) && {
          while (queryPos < stop) {
            if (fetchByte() != queryHead) return false
            queryPos += 1
          }
          true
        }
      }

      @tailrec def loop(): Boolean = {
        val op = fetchByte()
        val nameLen = op & LEN_MASK

        val branchTarget = if ((op & TAG_B) != 0) {
          val branchDisp = fetchULEB()
          dataPos + nameLen + branchDisp
        } else -1

        if (nameLen != 0) {
          // FWD record
          if ((op & TAG_B) != 0 && peekByte() < queryHead) {
            dataPos = branchTarget
            return loop() // continue
          }
          if (!matchName(nameLen)) {
            return false
          }
          if ((op & TAG_L) != 0) {
            return queryDone
          }

        } else op match {
          case OP_BISECT =>
            val pivot = fetchByte()
            val branchDisp = fetchULEB()
            if (pivot < queryHead) {
              dataPos += branchDisp
            }

          case OP_END =>
            return false

          case OP_LCONT if queryDone =>
            return true

          case _ => // JUMP | LCONT
            dataPos = branchTarget
        }

        loop()
      }

      loop()
    }

    def traverse(processor: Processor): Unit = {
      val rootID = -1
      processor.root(rootID)
      dataPos = 0
      doTraverse(prev = rootID, processor)
    }

    private def doTraverse(prev: Int, processor: Processor): Unit = {
      var n = dataPos
      while (n >= 0) {
        dataPos = n
        val op = fetchByte()
        val nameLen = op & LEN_MASK

        var branchTarget = if ((op & TAG_B) != 0) {
          val branchDisp = fetchULEB()
          dataPos + nameLen + branchDisp
        } else -1

        if (nameLen != 0) {
          // FWD record
          processor.forward(n, data, dataPos, nameLen)
          dataPos += nameLen
          if ((op & TAG_L) != 0) {
            processor.leaf(n, decodeLeaf())
          } else {
            doTraverse(n, processor)
          }
          processor.backward(n, prev, nameLen)

        } else if (op == OP_LCONT) {
          processor.leaf(prev, decodeLeaf())

        } else if (op == OP_BISECT) {
          dataPos += 1 // skip pivot
          val branchDisp = fetchULEB()
          branchTarget = dataPos + branchDisp
          doTraverse(prev, processor)
        }
        n = branchTarget
      }
    }
  }
}

/** Very compact read-only representation of `Index` in serial form
  * (simple byte array, ready to use, no need for unpacking).
  * Can be stored to/loaded from external storage.
  *
  * @author paul
  */
class SerialIndex private(data: Array[Byte], val stats: Stats) extends Index {
  private val johnnie: Walker = new Walker(data)

  override def traverse(processor: Processor): Unit = {
    johnnie.traverse(processor)
  }

  override def find(name: XString): Int = {
    johnnie.find(name)
  }
}