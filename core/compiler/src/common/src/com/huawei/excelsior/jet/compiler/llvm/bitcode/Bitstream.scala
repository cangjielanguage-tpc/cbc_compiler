/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.llvm.bitcode

import com.huawei.excelsior.jet.compiler.Env.bitsInByte
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitstream.Context
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Errors.{error, hopeThat, require}
import xscala.io.{DataInput, Path}
import xscala.text.{USAsciiEncoding, Utf8Encoding}
import xscala.util.MathUtils
import xscala.util.MathUtils.{alignUp, isNBits, nextPowerOf2, rightNBits32}

import java.lang.Long.numberOfLeadingZeros
import scala.math.toIntExact
import scala.util.Using

object Bitstream {

  private object AbbrevIds {
    val END_BLOCK = 0
    val ENTER_SUBBLOCK = 1
    val DEFINE_ABBREV = 2
    val UNABBREV_RECORD = 3

    val FIRST_NON_BUILTIN = 4
  }

  private[bitcode] object BlockIds {
    val TOP_LEVEL = -1
    val BLOCKINFO = 0

    val FIRST_NON_BUILTIN = 8
  }

  private object BlockInfoCodes {
    val SETBID = 1 // [blockid]
    val BLOCKNAME = 2 // [name]
    val SETRECORDNAME = 3 // [id, name]
  }

  private val NEXT_BITS_MAX_SIZE = java.lang.Long.SIZE

  private val DEBUG = false

  private object BlockInfo {
    def copy(x: BlockInfo) = new BlockInfo(deepCopyOf(x.abbrevsMap), x.depth, x.exclusive, x.outOfScope)

    private def deepCopyOf(abbrevsMap: Array[AbbrevList]) =
      abbrevsMap map (al => if (al == null) null else AbbrevList.deepCopy(al))

    def share(src: BlockInfo): Unit = {
      if (src != null && src.exclusive) {
        src.exclusive = false
      }
    }

    def endOfBlock(src: BlockInfo, depth: Int) = {
      if (src == null || depth != src.depth) {
        src
      } else if (src.exclusive) {
        src.outOfScope = true
        src
      } else {
        new BlockInfo(src.abbrevsMap, src.depth, src.exclusive, outOfScope = true)
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  // BLOCKINFO's content has essential influence on further bitstream parsing.
  // So we need to carefully follow the scoping semantics of BLOCKEND definitions.
  // Unfortunately, such semantics is not well specified neither in docs nor in LLVM source code.
  // We need to have definite answers for the following three questions:
  // 1. For BLOCKINFO located in block B1, what exactly is affected by its abbrev definitions:
  //    a. body of B1 itself,
  //    b. bodies of blocks nested in B1,
  //    c. tail of the bitstream after B1's body?
  // 2. What if more than one BLOCKINFOs occur in the same block?
  // 3. What if BLOCKINFOs occur in B1 & B2, where B2 is directly or indirectly nested in B1?
  //
  // As stated in LLVM docs:
  //   > Block definitions allow the reader to efficiently skip blocks in constant time if the reader
  //   > wants a summary of blocks, or if it wants to efficiently skip data it does not understand.
  //   > The LLVM IR reader uses this mechanism to skip function bodies, lazily reading them on demand.
  // So, to be able to correctly skip blocks while parsing, BLOCKINFOs must have strict lexical scope
  // of influence, possibly with reasonable scope nesting rules.
  // However, reference implementation does not care about BLOCKINFO scoping at all, and for repeated
  // BLOCKINFOs in bitstream it just replaces previous `BlockInfo` object by newly created one
  // (we call this `linear scoping` below).
  // This is unfortunate, as it leaves us with a choice:
  //   - do as reference implementation does and lose ability for partial bitstream parsing, or
  //   - implement reasonable (i.e. lexical) BLOCKINFO scoping rules and have a chance to misparse
  //     some fuzzy bitstream files which are successfully parsed by reference implementation.
  //
  // To solve this dilemma we implement restricted scoping semantics for BLOCKINFO, which allows
  // most cases where linear and lexical scoping rules have the same effect on bitstream decoding
  // and rejects most[*] bitstreams where these rules differ.
  // [*] It is not possible to reject all such bitstreams without full parsing, however in practice
  //     for LLVM IR bitcodes this situation is very unlikely to appear as LLVM IR has well-known
  //     structure where BLOCKINFOs should not appear in arbitrary blocks.
  // Restricted scoping semantics is as follows (see three questions above):
  // 1. For which blocks BLOCKINFO located in block B1 take effect:
  //    a. body of B1 itself - No. BLOCKINFO takes effect only at the point of block enter,
  //    b. bodies of blocks nested in B1 - Yes, iff these blocks occur after BLOCKINFO in the bitstream,
  //    c. tail of the bitstream after B1's body - No. As it contradicts with linear scoping, parser
  //       checks such situation and rejects the bitstream.
  // 2. What if more than one BLOCKINFOs occur in the same block?
  //    It is ok, previous `BlockInfo` object replaced by new one.
  // 3. What if BLOCKINFOs occur in B1 & B2, where B2 is directly or indirectly nested in B1?
  //    Bitstream will be rejected. BLOCKINFO nesting is not allowed as linear and lexical scoping may
  //    produce different results after B2's exit.
  ///////////////////////////////////////////////////////////////////////////////////////////////////
  private class BlockInfo private(
    private var abbrevsMap: Array[AbbrevList],
    private[Bitstream] val depth: Int,
    private var exclusive: Boolean,
    private[Bitstream] var outOfScope: Boolean
  ) {

    def this(depth: Int) = {
      this(new Array[AbbrevList](16), depth, true, false)
    }

    def modifyAbbrevsForBlock(bid: Int) = {
      assert(exclusive)
      if (bid >= abbrevsMap.length) {
        abbrevsMap = Array.copyOf(abbrevsMap, nextPowerOf2(bid + 1))
      }
      var abbrevs = abbrevsMap(bid)
      if (abbrevs == null) {
        abbrevs = AbbrevList.empty
        abbrevsMap(bid) = abbrevs
      }
      abbrevs
    }

    def getAbbrevsForBlock(bid: Int) = {
      val predefined = if (bid < abbrevsMap.length) abbrevsMap(bid) else null
      if (predefined == null || predefined.isEmpty) {
        AbbrevList.empty
      } else {
        // this is check for case 1c (see text above)
        hopeThat(!outOfScope, "potential out-of-scope usage of BLOCKINFO-defined abbreviations")
        AbbrevList.copy(predefined)
      }
    }
  }

  private object AbbrevList {
    private val TAGS_MASK = 7 << 29
    private val TAG_FIXED = 4 << 29
    private val TAG_VBR   = 5 << 29
    private val TAG_CHAR6 = 6 << 29
    private val HAS_BLOB  = 1 << 29
    private val HAS_ARRAY = 2 << 29

    def empty = new AbbrevList

    def copy(x: AbbrevList): AbbrevList = {
      val result = empty
      result.index = x.index
      result.data = x.data
      result.count = x.count
      result.dataSize = x.dataSize
      result.exclusive = false
      result
    }

    def deepCopy(x: AbbrevList): AbbrevList = {
      val result = copy(x)
      result.index = x.index.clone()
      result.data = x.data.clone()
      result
    }
  }

  private class AbbrevList {
    import AbbrevList.*

    private var index: Array[Int] = _
    private var data: Array[Int] = _
    private var count = 0
    private var dataSize = 0
    private var exclusive = true

    def isEmpty = count == 0

    private def ensureCapacity(arr: Array[Int], cap: Int, forceCopy: Boolean) = {
      assert(cap > 0)
      val newCap = nextPowerOf2(cap max 16)
      if (arr == null) {
        new Array[Int](newCap)
      } else if (newCap <= arr.length && !forceCopy) {
        arr
      } else {
        Array.copyOf(arr, newCap)
      }
    }

    private def reserve(c: Int, n: Int): Unit = {
      index = ensureCapacity(index, count + c, !exclusive)
      data = ensureCapacity(data, dataSize + n, !exclusive)
      exclusive = true
    }

    private[bitcode] def parseAbbrevDefinition(bs: Bitstream): Unit = {
      val opsNum = bs.readVBR(5)
      reserve(1, 1 + opsNum)
      val startPos = dataSize
      index(count) = startPos
      count += 1
      dataSize += 1 // skip place for header (opsCount & flags)

      var hasArray = false
      var hasBlob = false

      def parseValue(i: Int): Option[Int] = {
        val isLiteral = bs.readBit() == 1

        val value = if (isLiteral) {
          bs.readVBR(8) ensuring (_ >= 0)

        } else {
          val encoding = bs.readFixed(3)
          val tag: Int = encoding match {
            case 1 => TAG_FIXED
            case 2 => TAG_VBR
            case 4 => TAG_CHAR6
            case 3 =>
              require(i == opsNum - 2, "Array operand must be penultimate in abbrev definition")
              hasArray = true
              return None

            case 5 =>
              require(i == opsNum - 1, "Blob operand must be last in abbrev definition")
              hasBlob = true
              return None

            case _ =>
              return error("unexpected abbreviation operand encoding %d", encoding)
          }
          if (tag == TAG_FIXED || tag == TAG_VBR) {
            val width = bs.readVBR(5)
            hopeThat(0 <= width && width <= 32, "unexpected fixed/VBR width %d", width)
            if (width == 0) {
              0 // replace zero-width fixed/VBR by literal zero, as reference implementation does
            } else {
              tag | width
            }
          } else {
            tag
          }
        }
        Some(value)
      }

      for (i <- 0 until opsNum; value <- parseValue(i)) {
        data(dataSize) = value
        dataSize += 1
      }

      require(!(hasBlob && hasArray), "there could be no array and blob operands simultaneously")

      val opsCount = opsNum - (if (hasBlob) 1 else 0) - (if (hasArray) 2 else 0)
      hopeThat((opsCount & ~TAGS_MASK) == opsCount, "unexpected number of abbrev operands %d", opsNum)
      val header = opsCount | (if (hasBlob) HAS_BLOB else 0) | (if (hasArray) HAS_ARRAY else 0)
      data(startPos) = header

      val id = AbbrevIds.FIRST_NON_BUILTIN + (count - 1)
      bs.log("define abbrev %d", id)
    }

    private def parseOperand(bs: Bitstream, encoding: Int, isCode: Boolean): Unit = {
      val tag = encoding & TAGS_MASK
      val op: Long = if (encoding >= 0) {
        encoding // literal
      } else if (tag == TAG_FIXED) {
        bs.readFixed(encoding & ~TAGS_MASK)
      } else if (tag == TAG_VBR) {
        bs.readVBR64(encoding & ~TAGS_MASK)
      } else {
        assert(tag == TAG_CHAR6)
        bs.readChar6()
      }
      bs.ctx.append(op)
      bs.log(if (isCode) "* %d" else "- %d", op)
    }

    private[bitcode] def parseRecord(bs: Bitstream, abbrevId: Int): Unit = {
      require(abbrevId < AbbrevIds.FIRST_NON_BUILTIN + count, "unexpected abbreviation id %s", abbrevId)
      bs.log("abbrev %d", abbrevId)

      var pos = index(abbrevId - AbbrevIds.FIRST_NON_BUILTIN)
      val header = data(pos)
      pos += 1
      val opsCount = header & ~TAGS_MASK
      bs.ctx.clearData().reserve(opsCount)

      // Read normal operands.
      for (i <- 0 until opsCount) {
        parseOperand(bs, data(pos), i == 0)
        pos += 1
      }

      if ((header & HAS_ARRAY) != 0) {
        val len = bs.readVBR(6)
        bs.ctx.reserve(len)
        for (_ <- 0 until len) {
          parseOperand(bs, data(pos), isCode = false)
        }

      } else if ((header & HAS_BLOB) != 0) {
        val len = bs.readVBR(6)
        bs.ctx.blob = bs.readBytesAligned32(len)
        bs.align32()
        bs.log("- blob (%d)", len)
      }
    }
  }

  object Context {
    private[Bitstream] val DEFAULT_CAPACITY = 32
    private[Bitstream] val START_BLOCK = -1
    private[Bitstream] val SINGLE_BLOCK = -2

    def copy(x: Context) = {
      val res = new Context(x.depth, x.blockId,
        if (x.blockInfo == null) null else BlockInfo.copy(x.blockInfo),
        if (x.data == null) null else x.data.clone(),
        x.dataSize
      )
      res.blob = if (x.blob == null) null else x.blob.clone()
      res
    }
  }

  class Context private(
    private[Bitstream] var _depth: Int,
    private[Bitstream] var _blockId: Int,
    private[Bitstream] var blockInfo: BlockInfo,
    private[Bitstream] var data: Array[Long],
    private[Bitstream] var dataSize: Int
  ) {

    private[Bitstream] var blob: Array[Byte] = _

    def this() = {
      this(0, BlockIds.TOP_LEVEL, null, new Array[Long](Context.DEFAULT_CAPACITY), 0)
    }

    private[Bitstream] def reserve(size: Int): Unit = {
      if (dataSize + size > data.length) data = Array.copyOf(data, nextPowerOf2(dataSize + size))
    }

    private[Bitstream] def append(num: Long): Context = {
      reserve(1)
      data(dataSize) = num
      dataSize += 1
      this
    }

    private[Bitstream] def clearData(): Context = {
      dataSize = 0
      blob = null
      this
    }

    def depth = _depth

    def blockId = _blockId

    def code = toIntExact(data(0)) ensuring (_ > 0)

    def operandsCount = dataSize - 1

    def operand(i: Int) = {
      assert(0 <= i && i < dataSize - 1)
      data(i + 1)
    }

    def hasBlob = blob != null

    def getBlob = {
      assert(hasBlob)
      blob
    }

    def operandsAsArray = data.slice(1, dataSize)

    def operandsAsName(startIdx: Int): String = {
      val bytes = Array.tabulate[Byte](operandsCount - startIdx) { i =>
        val op = operand(i + startIdx)
        require(isNBits(op, 8), "unexpected non-byte operand [0x%s] in UTF-8 name", java.lang.Long.toHexString(op))
        op.toByte
      }
      decodeString(bytes, 0, bytes.length)
    }

    def operandsAsName: String = operandsAsName(0)

    def makeSingleBlockContext() = {
      import Context.*
      assert(dataSize > 0 && data(0) == START_BLOCK && blob == null)
      val newCapacity = nextPowerOf2(dataSize max DEFAULT_CAPACITY)
      BlockInfo.share(blockInfo)
      val ctx = new Context(depth, blockId, blockInfo, Array.copyOf(data, newCapacity), dataSize)
      ctx.data(0) = SINGLE_BLOCK
      ctx
    }
  }

  private val MAX_GET_BITS_COUNT = Integer.SIZE
  private val GET_BITS_MASKS = Array.tabulate[Int](MAX_GET_BITS_COUNT + 1)(i => rightNBits32(i))

  /** Character codes indexed by their Char6 encoding. */
  private val CHAR6_DECODER = USAsciiEncoding.encodeStringThrowing("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._")
    .ensuring(_.length == (1 << 6))

  private[bitcode] def decodeString(utf8Blob: Array[Byte], offset: Int, length: Int) =
    Utf8Encoding.decodeStringReplacing(utf8Blob, offset, length)

  def raw(buf: Array[Byte]): Bitstream = raw(buf, 0, buf.length)

  def raw(buf: Array[Byte], offset: Int, length: Int): Bitstream =
    new Bitstream(DataInput.from(buf, offset, length), null)

  def parseWhole(in: DataInput, consumer: BitstreamConsumer): Unit = {
    val ctx = new Context
    consumer.setContext(ctx)

    val bs = new Bitstream(in, ctx)
    bs.parseBitstream(consumer)

    consumer.endOfStream()
    consumer.setContext(null)
  }

  def parseWhole(fileName: String, consumer: BitstreamConsumer): Unit = { // TODO: String => Path
    Using.resource(DataInput.from(Path(fileName), buffered = true)) { in =>
      Bitstream.parseWhole(in, consumer)
    }
  }

  def parseSingleBlock(in: DataInput, ctx: Context, consumer: BitstreamConsumer): Unit = {
    assert(ctx.data(0) == Context.SINGLE_BLOCK)
    ctx.data(0) = Context.START_BLOCK
    consumer.setContext(ctx)

    val bs = new Bitstream(in, ctx)
    bs.skip(ctx.data(1))
    bs.parseBlock(consumer, ctx.data(2).toInt, ctx.data(3).toInt)
    ctx.clearData()

    consumer.endOfStream()
    consumer.setContext(null)
  }

  def parseSingleBlock(fileName: String, ctx: Context, consumer: BitstreamConsumer): Unit = { // TODO: String => Path
    Using.resource(DataInput.from(Path(fileName), buffered = true)) { in =>
      Bitstream.parseSingleBlock(in, ctx, consumer)
    }
  }
}

class Bitstream private(in: DataInput, private val ctx: Context) {
  import Bitstream.*

  private var nextBits = 0L
  private var nextBitsSize = 0
  private var position = 0L

  private def log(format: String, args: Any*): Unit = {
    if (DEBUG) println("[bitstream] " + (" " * ctx.depth) + format.format(args))
  }

  private def fetchBits(count: Int): Boolean = {
    assert(count <= NEXT_BITS_MAX_SIZE - bitsInByte)
    while (nextBitsSize < count) {
      val b = in.getByte()
      if (b == -1) {
        // end of stream
        return false
      }

      nextBits |= b.toLong << nextBitsSize
      nextBitsSize += bitsInByte
    }
    true
  }

  private def hasMore = fetchBits(1)

  private def errorEndOfBitstream(): Unit = error("unexpected end of bitstream")

  private def incrementPosition(delta: Long): Unit = {
    assert(delta >= 0)
    position += delta
    // overflow is ok, position is used for alignment and for debugging
  }

  /** Extract single bit. */
  private[bitcode] def readBit(): Int = readBits(1)

  /** Extract `count` bits as little-endian integer with zero-extension to 32 bits.
    * Should be treated as unsigned value (`int` value could be negative).
    */
  private[bitcode] def readBits(count: Int): Int = {
    assert(0 <= count && count <= MAX_GET_BITS_COUNT)
    if (!fetchBits(count)) {
      errorEndOfBitstream()
    }
    val res = nextBits.toInt & GET_BITS_MASKS(count)
    nextBits >>>= count
    nextBitsSize -= count
    incrementPosition(count)
    res
  }

  /** Extract `bits`-bit fixed-width unsigned integer (maximum of 31 significant bits). */
  private[bitcode] def readFixed(bits: Int): Int = {
    hopeThat(0 <= bits && bits <= 32, "unexpected fixed width %d", bits)
    val value = readBits(bits)
    hopeThat(value >= 0, "fixed value overflowed 32-bit signed integer")
    value
  }

  /** Extract `bits`-bit variable-width integer (non-negative, maximum of 31 significant bits). */
  private[bitcode] def readVBR(bits: Int): Int = {
    val value = readVBR64(bits)
    val truncated = value.toInt
    hopeThat(value >= 0 && truncated == value, "VBR overflowed 32-bit signed integer")
    truncated
  }

  /** Extract `bits`-bit variable-width integer (might be negative, maximum of 64 significant bits). */
  private[bitcode] def readVBR64(bits: Int): Long = {
    hopeThat(2 <= bits && bits <= 32, "unexpected VBR width %d", bits)
    var res = 0L
    var shift = 0
    var hasMore = true
    while (hasMore) {
      // Note: must be long to avoid overflow!
      val value: Long = readBits(bits - 1).toLong
      hopeThat(numberOfLeadingZeros(value) >= shift, "VBR overflowed 64-bit signed integer")
      res |= (value << shift)
      shift += bits - 1
      hasMore = readBit() != 0
    }
    res
  }

  private[bitcode] def readChar6() = {
    val encoding = readBits(6)
    require(0 <= encoding && encoding < CHAR6_DECODER.length, "invalid char6 encoding %s", encoding)
    CHAR6_DECODER(encoding)
  }

  private[bitcode] def skip(count: Long): Unit = {
    assert(count >= 0)
    // Take some bits from cached nextBits.
    val nextBitsCount = (count min nextBitsSize).toInt
    readBits(nextBitsCount)

    // Skip some complete bytes.
    var remaining = count - nextBitsCount
    while (remaining >= bitsInByte) {
      assert(nextBitsSize == 0) // we don't overcache next bits
      val chunkSize = in.skip(toIntExact(remaining / bitsInByte))
      // Note that skip always returns non-negative value so it's hard to check end of stream.
      // We hope that zero skipped bytes mean something bad.
      hopeThat(chunkSize > 0, "unexpected state of bitstream")
      remaining -= chunkSize * bitsInByte
      incrementPosition(chunkSize * bitsInByte)
    }

    // Cache at most one byte and skip some bits.
    readBits(remaining.toInt)
  }

  private[bitcode] def align32(): Unit = {
    val newPos = alignUp(position, 32)
    hopeThat(newPos >= 0, "position overflow")
    skip(newPos - position)
    // We don't overcache next bits, thus nextBitsSize is always less than or equal to BITS_IN_BYTE.
    assert(nextBitsSize == 0)
  }

  private[bitcode] def readBytesAligned32(count: Int) = {
    align32()

    val res = new Array[Byte](count)
    var readBytes = 0
    while (readBytes < count) {
      val chunkSize = in.getBytes(res, readBytes, count - readBytes)
      if (chunkSize == -1) {
        errorEndOfBitstream()
      }
      assert(chunkSize >= 0)
      readBytes += chunkSize
    }
    assert(readBytes == count)

    incrementPosition(count.toLong * bitsInByte)
    res
  }

  private def parseBitstream(consumer: BitstreamConsumer): Unit = {
    val magic = readBits(32)
    log("magic 0x%8X", magic)
    if (!consumer.magic(magic)) {
      return
    }

    val topLevelAbbrevIdWidth = 2 // note that all builtin ids fit into 2 bits
    while (hasMore) {
      val topLevelAbbrevId = readFixed(topLevelAbbrevIdWidth)
      require(topLevelAbbrevId == AbbrevIds.ENTER_SUBBLOCK, "top-level entry should be a block")
      parseBlock(consumer)
    }
  }

  private[bitcode] def parseBlock(consumer: BitstreamConsumer): Unit = {
    val blockId = readVBR(8)
    val abbrevIdWidth = readVBR(4)
    align32()
    ctx.clearData().append(Context.START_BLOCK).append(position).append(blockId).append(abbrevIdWidth)
    parseBlock(consumer, blockId, abbrevIdWidth)
    ctx.clearData()
  }

  private def parseBlock(consumer: BitstreamConsumer, blockId: Int, abbrevIdWidth: Int): Unit = {
    val length32 = readFixed(32)
    log("block: id %d, len32 %d, abbrev id width %d", blockId, length32, abbrevIdWidth)

    if (blockId == BlockIds.BLOCKINFO) {
      require(consumer.blockInfoAllowed, "BlockInfo is not allowed in this context: block id %d", ctx.blockId)
      parseBlockInfo(abbrevIdWidth)
      return
    }
    hopeThat(blockId >= BlockIds.FIRST_NON_BUILTIN, "unknown builtin block id %d", blockId)

    val shouldSkip = !consumer.enterBlock(blockId)
    if (shouldSkip) {
      ctx._depth += 1
      skip(length32 * 32L)
      log("skip")
      ctx._depth -= 1
      return
    }
    val parentBlockId = ctx.blockId
    ctx._blockId = blockId
    ctx._depth += 1

    val abbrevs = if (ctx.blockInfo != null) ctx.blockInfo.getAbbrevsForBlock(blockId) else AbbrevList.empty

    while (true) {
      readFixed(abbrevIdWidth) match {
        case AbbrevIds.END_BLOCK =>
          align32()
          ctx.blockInfo = BlockInfo.endOfBlock(ctx.blockInfo, ctx.depth)
          val id = ctx.blockId
          ctx._blockId = parentBlockId
          ctx._depth -= 1
          log("end block %d", id)
          consumer.endBlock(id)
          return

        case AbbrevIds.ENTER_SUBBLOCK =>
          parseBlock(consumer)

        case AbbrevIds.UNABBREV_RECORD =>
          parseUnabbreviatedRecord(readVBR(6), skip = false)
          consumer.record(ctx.code, ctx.operandsCount, ctx.hasBlob)
          ctx.clearData()

        case AbbrevIds.DEFINE_ABBREV =>
          abbrevs.parseAbbrevDefinition(this)

        case abbrevId =>
          abbrevs.parseRecord(this, abbrevId)
          consumer.record(ctx.code, ctx.operandsCount, ctx.hasBlob)
          ctx.clearData()
      }
    }
  }

  private[bitcode] def parseBlockInfo(abbrevIdWidth: Int): Unit = {
    if (ctx.blockInfo != null && !ctx.blockInfo.outOfScope) {
      val relativeDepth = ctx.depth - ctx.blockInfo.depth
      assert(relativeDepth >= 0)
      // this is check for case 3 (see text in class BlockInfo body)
      hopeThat(relativeDepth == 0, "more than one BLOCKINFO in nested blocks")
    }
    ctx.blockInfo = new BlockInfo(ctx.depth)
    var abbrevs: AbbrevList = null // null unless SETBID record

    ctx._depth += 1
    log("(blockinfo block)")
    val skipNames = true

    while (true) {
      readFixed(abbrevIdWidth) match {
        case AbbrevIds.END_BLOCK =>
          align32()
          ctx._depth -= 1
          log("end block 0")
          return

        case AbbrevIds.UNABBREV_RECORD =>
          val code = readVBR(6)
          if (code == BlockInfoCodes.SETBID) {
            parseUnabbreviatedRecord(code, skip = false)
            require(ctx.operandsCount == 1, "SETBID record must have one operand")
            val bid = ctx.operand(0).toInt
            abbrevs = ctx.blockInfo.modifyAbbrevsForBlock(bid)
            log("setbid %d", bid)
          } else {
            // Note that two other BLOCKINFO block's records (BLOCKNAME and SETRECORDNAME)
            // are ignored as they are optional.
            parseUnabbreviatedRecord(code, skipNames)
            if (!skipNames) {
              if (code == BlockInfoCodes.BLOCKNAME) {
                log("blockname %s", ctx.operandsAsName)
              } else if (code == BlockInfoCodes.SETRECORDNAME) {
                log("setrecordname %d %s", ctx.operand(0), ctx.operandsAsName(1))
              }
            }
          }
          ctx.clearData()

        case AbbrevIds.DEFINE_ABBREV =>
          require(abbrevs != null, "missing SETBID before DEFINE_ABBREV in BLOCKINFO block")
          abbrevs.parseAbbrevDefinition(this)

        case abbrevId @ (AbbrevIds.ENTER_SUBBLOCK | _) =>
          error("unexpected abbreviation in BLOCKINFO block %d", abbrevId)
      }
    }
  }

  private def parseUnabbreviatedRecord(code: Int, skip: Boolean): Unit = {
    val opsCount = readVBR(6)
    if (!skip) {
      log("unabbrev")
      log("* %d", code)
      ctx.clearData().append(code).reserve(opsCount)
    }

    for (_ <- 0 until opsCount) {
      val op = readVBR64(6)
      if (!skip) {
        ctx.append(op)
        log("- %d", op)
      }
    }
  }
}