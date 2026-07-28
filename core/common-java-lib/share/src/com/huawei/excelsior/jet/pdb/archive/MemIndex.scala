/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.pdb.archive

import com.huawei.excelsior.jet.common.XString

object MemIndex {
  // node entry format: {
  //   nodeData[id*2]   = { nameLen: 6 high bits, nameStart: 26 low bits }
  //   nodeData[id*2+1] = { tag: 3 high bits, refID: 29 low bits }
  // }
  // tags: 3bit `LXI` field. Interpretation:
  //   XI == 10 => "xbranch node": refID = xbranch ID; L is "linear bit"; L=0 <=> xbranch entries sorted; may use binary search
  //   XI == x1 => "ibranch node": node has XI (1 or 3) continuation entries (each occupies two ints in `nodeData` just after the node)
  //   XI != 10 => L is "leaf bit"; L=1 => refID is entryID of tree leaf; L=0 => refID is child nodeID
  // ibranch/xbranch slots seq may have some unused/empty slots at the end, all such unused slots are zeroed:
  // tag bits on ibranch/xbranch slots:
  //   X=0 always
  //   I=0 => this is last valid (or unused) slot in ibranch/xbranch list
  //   L=0 on all but first slots
  //   L bit on first slot is "leaf bit"

  private val NODE_SIZE = 2
  private val NAME_LEN_BITS = 6
  private val NAME_LEN_SHIFT = 32 - NAME_LEN_BITS
  private val NAME_LEN_MASK = (-1) << NAME_LEN_SHIFT
  private val NAME_LEN_MAX = (1 << NAME_LEN_BITS) - 1
  private val NAME_START_MASK = ~NAME_LEN_MASK
  private val TAG_BITS = 3
  private val TAG_SHIFT = 32 - TAG_BITS
  private val TAG_MASK = (-1) << TAG_SHIFT
  private val REF_ID_MASK = ~TAG_MASK
  private val TAG_L = 1 << 31
  private val TAG_X = 1 << 30
  private val TAG_I = 1 << 29

  private val BSEARCH_THRESHOLD = 17 // minimal number of branch slots where binary search becomes profitable

  private val LEAF_CHAR_MARK: Int = 256 // Differ from any 8-bit char value, either signed or unsigned

  private def growCapacity(minCapacity: Int) = { // TODO: use maxPowerOfTwo
    var pow = 1
    while ((1 << pow) < minCapacity) pow += 1
    1 << pow
  }

  class Stats(val nodeDataLength: Int, val nodeCount: Int, val deadNodeCount: Int,
              val charsLength: Int, val charCount: Int,
              val xbranchLength: Int, val xbranchCount: Int,
              val xbranchSlotsLength: Int, val xbranchSlotsMax: Int, val xbranchSlotsSum: Int) {

    def print(header: String): Unit = {
      val xbavg = (xbranchSlotsSum * 100 / xbranchCount / 100.0)
      val xbStat = s"xbranch[$xbranchLength] { used: $xbranchCount, slots(len: $xbranchSlotsLength, max: $xbranchSlotsMax, sum: $xbranchSlotsSum, avg: $xbavg) }"
      println(s"MemIndex: nodeData[$nodeDataLength] { used: $nodeCount, dead: $deadNodeCount }, chars[$charsLength] { used: $charCount }, $xbStat")
      val ARRHDR = 32
      val sumSize = nodeDataLength * 4 + charsLength + xbranchLength * 8 + xbranchCount * ARRHDR + xbranchSlotsLength * 4
      println(s"sumSize: $sumSize")
      if (header != null) {
        val sz = Utils.sizeStr(sumSize)
        println(s"$header$sz")
      }
    }

  }
}

/** Memory-efficient implementation of `RWIndex` stored in memory.
  *
  * @author paul
  */
class MemIndex extends RWIndex {
  import MemIndex.*
  import Index.*

  private var nodeData = new Array[Int](1 * NODE_SIZE)
  private var nodeCount = 1

  private var chars = new Array[Byte](1)
  private var charCount = 1

  private var xbranch = new Array[Array[Int]](1)
  private var xbranchCount = 1

  private val deadNodes = new Array[NodeID](5) // free lists for width={1,2,4}
  private var deadNodeCount = 0

  private def nameStart(id: NodeID) = nodeData(id << 1) & NAME_START_MASK
  private def nameLen(id: NodeID) = nodeData(id << 1) >>> NAME_LEN_SHIFT

  private def refTagsPos(id: NodeID) = (id << 1) + 1

  private def refID(id: NodeID) = nodeData(refTagsPos(id)) & REF_ID_MASK
  private def tags(id: NodeID) = nodeData(refTagsPos(id)) & TAG_MASK

  private def setName(id: NodeID, nameStart: Int, nameLen: Int): Unit = {
    assert((nameStart & ~NAME_START_MASK) == 0)
    assert(nameLen > 0 && nameLen <= NAME_LEN_MAX)
    nodeData(id << 1) = nameStart | (nameLen << NAME_LEN_SHIFT)
  }

  private def setRefTags(id: NodeID, refID: Int, tags: Int): Unit = {
    assert((refID & ~REF_ID_MASK) == 0)
    assert((tags & ~TAG_MASK) == 0)
    nodeData(refTagsPos(id)) = refID | tags
  }

  private def nodeWidth(id: NodeID) = {
    val xi = tags(id) & (TAG_X | TAG_I)
    if (xi == TAG_X) 1 else 1 + (xi >>> TAG_SHIFT)
  }

  private def isXBranch(id: NodeID) = (tags(id) & (TAG_X | TAG_I)) == TAG_X
  private def useBSearch(id: NodeID) = (tags(id) & (TAG_L | TAG_X | TAG_I)) == TAG_X

  private def branchStart(id: NodeID) = if (isXBranch(id)) 0 else refTagsPos(id)

  private def branchSlots(id: NodeID) = if (isXBranch(id)) xbranch(refID(id)) else nodeData

  private def branchSlotCount(id: NodeID) =
    if (isXBranch(id)) xbranch(refID(id)).length else nodeWidth(id) * NODE_SIZE - 1

  override def traverse(processor: Processor): Unit = {
    processor.root(rootID = NO_ENTRY)
    val root = refID(0)
    if (root != NO_ENTRY) {
      doTraverse(NO_ENTRY, root, processor)
    }
  }

  private def doTraverse(p: NodeID, n: NodeID, processor: Index.Processor): Unit = {
    processor.forward(n, chars, nameStart(n), nameLen(n))

    val slots = branchSlots(n)
    var pos = branchStart(n)
    var done = false
    while (!done) {
      val v = slots(pos)
      pos += 1
      val id = v & REF_ID_MASK
      if ((v & TAG_L) != 0) {
        processor.leaf(n, id)
      } else {
        doTraverse(n, id, processor)
      }
      if ((v & TAG_I) == 0) done = true
    }
    processor.backward(n, p, nameLen(n))
  }

  private def calcWeights = {
    val ws = new Array[NodeID](nodeCount)
    traverse(new Processor() {
      override def forward(dstID: NodeID, chars: Array[Byte], start: Int, charsCnt: Int): Unit = {
        assert(ws(dstID) == 0)
      }

      override def leaf(nodeID: NodeID, entryID: EntryID): Unit = {
        ws(nodeID) += 1
      }

      override def backward(srcID: NodeID, dstID: NodeID, charsCnt: Int): Unit = {
        ws(dstID) += ws(srcID)
      }
    })
    ws
  }

  override def find(name: XString): EntryID = {
    doSearch(name, NO_ENTRY, modify = false, allowReplace = false)
  }

  override def add(name: XString, id: EntryID, allowReplace: Boolean): EntryID = {
    assert(id != NO_ENTRY)
    doSearch(name, id, modify = true, allowReplace)
  }

  private type TaggedID = Int // NodeID or (EntryID & TAG_L)
  private type AnyID = Int // NodeID or EntryID without tags; distinguish by context

  private def doSearch(name: XString, entryID: EntryID, modify: Boolean, allowReplace: Boolean): EntryID = {
    var p = NO_ENTRY
    var c = refID(0) //root
    var namePos = 0
    assert(name.length > 0)
    assert((entryID & ~REF_ID_MASK) == 0)

    if (c == NO_ENTRY) { // empty tree
      if (modify) setRefTags(p, appendLeaf(name, namePos, entryID), tags = 0)
      return NO_ENTRY
    }

    while (true) {
      val i = indexOfDiff(c, name, namePos)
      namePos += i

      if (i < nameLen(c)) {
        // split c at i'th char
        if (modify) split(p, c, i, appendLeaf(name, namePos, entryID))
        return NO_ENTRY
      }

      val searchForLeaf = namePos == name.length
      val found = succ(c, if (searchForLeaf) LEAF_CHAR_MARK else name.charAt(namePos))
      if (found == NO_ENTRY) {
        if (modify) {
          val c1 = expand(c)
          appendSucc(c1, appendLeaf(name, namePos, entryID))
          if (c1 != c) replaceSucc(p, c, c1)
        }
        return NO_ENTRY
      }

      if (searchForLeaf) { // leafID is found at c
        if (modify && allowReplace) replaceLeafID(c, entryID)
        return found
      } else {
        p = c
        c = found
      }
    }
    ???
  }

  private def indexOfDiff(node: NodeID, name: XString, pos: Int): Int = {
    val len = name.length
    val ns = nameStart(node)
    val nl = nameLen(node)
    for (i <- 0 until nl) {
      if (pos + i == len || name.charAt(pos + i) != chars(ns + i)) return i
    }
    nl
  }

  private def startsWith(node: NodeID, ch: Int) = {
    assert(nameLen(node) > 0)
    chars(nameStart(node)) == ch
  }

  private def linearSearch(slots: Array[Int], pos: Int, ch: Int): AnyID = {
    var p = pos
    while (true) {
      val v = slots(p)
      p += 1
      val id = v & REF_ID_MASK
      if (ch == LEAF_CHAR_MARK) {
        return if ((v & TAG_L) != 0) id else NO_ENTRY
      }
      if ((v & TAG_L) == 0 && startsWith(id, ch)) return id
      if ((v & TAG_I) == 0) return NO_ENTRY
    }
    ???
  }

  private def binarySearch(node: NodeID, ch: Int): AnyID = {
    val slots = xbranch(refID(node))
    ??? //TODO
  }

  private def succ(node: NodeID, ch: Int): AnyID = {
    if (useBSearch(node)) {
      binarySearch(node, ch)
    } else {
      linearSearch(branchSlots(node), branchStart(node), ch)
    }
  }

  private def replaceLeafID(node: NodeID, newID: EntryID): Unit = {
    val slots = branchSlots(node)
    val i = branchStart(node)
    val v = slots(i)
    assert((v & TAG_L) != 0)
    assert((newID & ~REF_ID_MASK) == 0)
    slots(i) = newID | (v & TAG_MASK)
  }

  // replace `p -> n` => `p -> x`
  private def replaceSucc(p: NodeID, n: NodeID, x: NodeID): Unit = {
    val slots = branchSlots(p)
    var pos = branchStart(p)
    while (true) {
      val v = slots(pos)
      pos += 1
      val id = v & REF_ID_MASK
      if ((v & TAG_L) == 0 && id == n) {
        slots(pos - 1) = x | (v & TAG_MASK)
        return
      }
      assert((v & TAG_I) != 0)
    }
  }

  // replace `c -> ...` =>  `c -> ..., n`
  private def appendSucc(c: NodeID, n: TaggedID): Unit = {
    if (useBSearch(c)) {
      appendSuccSorted(c, n)
      return
    }
    val slots = branchSlots(c)
    val start = branchStart(c)
    var pos = start + branchSlotCount(c) - 1
    assert(slots(pos) == 0)
    while (slots(pos - 1) == 0) {
      pos -= 1
    }
    slots(pos - 1) |= TAG_I
    if ((n & TAG_L) == 0) {
      slots(pos) = n
    } else { // `n` is leafID, it should come first
      val first = slots(start)
      assert((first & TAG_L) == 0)
      slots(pos) = first & ~TAG_MASK
      slots(start) = n | (first & TAG_MASK)
    }
    if (pos - start >= BSEARCH_THRESHOLD) {
      assert(isXBranch(c))
      // TODO: sort branch slots
      // setRefTags(c, refID(c), TAG_X) // clear TAG_L
    }
  }

  private def appendSuccSorted(c: NodeID, n: TaggedID): Unit = {
    ??? //TODO
  }

  // expand `n` so it has at least one free slot to use
  private def expand(n: NodeID): NodeID = {
    if (isXBranch(n)) {
      val i = refID(n)
      val xb = xbranch(i)
      if (xb(xb.length - 1) != 0) {
        xbranch(i) = Array.copyOf(xb, xb.length * 2)
      }
      return n
    }

    val w = nodeWidth(n)
    if (nodeData((n << 1) + w * NODE_SIZE - 1) == 0) {
      return n // last slot is free
    }

    // realloc & expand `n`
    if (w < 4) {
      val w1 = w * 2
      val n1 = allocateNode(w1)
      Array.copy(nodeData, n << 1, nodeData, n1 << 1, w * NODE_SIZE)
      setRefTags(n1, refID(n), (tags(n) & TAG_L) | ((w1 - 1) << TAG_SHIFT))
      freeNode(n, w)
      return n1
    } else {
      // create xbranch node
      val xi = xbranchCount
      xbranchCount += 1
      if (xbranchCount > xbranch.length) {
        xbranch = Array.copyOf(xbranch, xbranch.length * 2)
      }
      xbranch(xi) = new Array[Int](w * NODE_SIZE * 2)
      Array.copy(nodeData, branchStart(n), xbranch(xi), 0, branchSlotCount(n))
      xbranch(xi)(0) &= ~TAG_X

      val nameInfo = nodeData(n << 1)
      freeNode(n, w)

      val n1 = allocateNode(1)
      nodeData(n1 << 1) = nameInfo
      setRefTags(n1, xi, TAG_L | TAG_X)
      return n1
    }
  }

  private def freeNode(n: NodeID, width: Int): Unit = {
    for (i <- 0 until width * NODE_SIZE) {
      nodeData((n << 1) + i) = 0
    }
    if (n + width == nodeCount) {
      nodeCount -= width
    } else {
      nodeData(n << 1) = deadNodes(width)
      deadNodes(width) = n
      deadNodeCount += width
    }
  }

  private def allocateNode(width: Int): NodeID = {
    assert(width == 1 || width == 2 || width == 4)
    var n = deadNodes(width)
    if (n != NO_ENTRY) {
      deadNodes(width) = nodeData(n << 1)
      deadNodeCount -= width
    } else if (width < 4 && deadNodes(width * 2) != NO_ENTRY) {
      n = allocateNode(width * 2)
      freeNode(n + width, width)
    } else {
      n = nodeCount
      nodeCount += width
      if (nodeCount * NODE_SIZE > nodeData.length) {
        nodeData = Array.copyOf(nodeData, growCapacity(nodeCount * NODE_SIZE))
      }
    }
    n
  }

  private def copyChars(name: XString, pos: Int) = {
    val len = name.length
    assert(pos < len)

    val start = charCount
    charCount += len - pos
    if (charCount > chars.length) {
      chars = Array.copyOf(chars, growCapacity(charCount))
    }
    name.getChars(pos, len, chars, start)
    start
  }

  private def makeLeaf(nameStart: Int, nameLen: Int, entryID: EntryID): NodeID = {
    val n = allocateNode(width = 1)
    if (nameLen > NAME_LEN_MAX) {
      val tail = makeLeaf(nameStart + NAME_LEN_MAX, nameLen - NAME_LEN_MAX, entryID)
      setName(n, nameStart, NAME_LEN_MAX)
      setRefTags(n, tail, tags = 0)
    } else {
      setName(n, nameStart, nameLen)
      setRefTags(n, entryID, TAG_L)
    }
    n
  }

  private def appendLeaf(name: XString, namePos: Int, entryID: EntryID): TaggedID = {
    val len = name.length - namePos
    if (len == 0) {
      entryID | TAG_L
    } else {
      makeLeaf(copyChars(name, namePos), len, entryID)
    }
  }

  /** Having tree pattern `p -> c{"xy", slots}` we insert branch to `n` (which may be leaf or nodeRef)
    * at c.name[splitPos] by splitting node `c`.
    * This results in `p -> q{"x", [n, c']}; c'{"y", slots}` where width(c) = width(c'); width(q) = 2.
    */
  private def split(p: NodeID, c: NodeID, splitPos: Int, n: TaggedID): Unit = {
    assert(splitPos > 0 || p == NO_ENTRY)
    val ns = nameStart(c)
    val nl = nameLen(c)
    val q = allocateNode(width = 2)
    if (splitPos == 0) {
      nodeData(q << 1) = 0 // special case; don't want to weaken assert in `setName`
    } else {
      setName(q, ns, splitPos)
    }
    val iq = refTagsPos(q)
    nodeData(iq) = n | TAG_I
    nodeData(iq + 1) = c
    setName(c, ns + splitPos, nl - splitPos)
    replaceSucc(p, c, q)
  }

  /** For use in unit-tests. */
  private[pdb] def stats = {
    var xbranchSlotsLength = 0
    var xbranchSlotsMax = 0
    var xbranchSlotsSum = 0
    for (i <- 1 until xbranchCount) {
      xbranchSlotsLength += xbranch(i).length
      val cnt = xbranch(i).lastIndexWhere(_ != 0)
      xbranchSlotsMax = xbranchSlotsMax max cnt
      xbranchSlotsSum += cnt
    }

    new Stats(nodeData.length, nodeCount, deadNodeCount,
      chars.length, charCount,
      xbranch.length, xbranchCount, xbranchSlotsLength, xbranchSlotsMax, xbranchSlotsSum)
  }
}