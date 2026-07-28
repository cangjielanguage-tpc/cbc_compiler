/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.xiFilesModule.SymFile.*
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.*
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.MathUtils.{high32Bits, low32Bits, makeLong}
import xscala.util.{Set32, Set64, UInt, ULong, UShort}

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.collection.mutable.ArrayBuffer

/** Abstract files
  *
  * TODO: use xscala.io (at least start to synchronize interfaces)
  * */
object xiFilesModule {

  def withBufferOfSize[A](size: Int)(action: Array[Byte] => A): A = bufferCache.withBuffer(size)(action)

  private abstract class BufferCache {
    def withBuffer[A](minCapacity: Int = 0)(action: Array[Byte] => A): A
  }

  private val bufferCache: BufferCache = new BufferCache {
    // TODO: add AtomicReference to xscala library
    //private val ref = new AtomicReference[Array[Byte]](null)
    private var cachedBuf = new Array[Byte](1024)
    @volatile private var busy = 0

    private def nextPowerOfTwo(cap: Int) = {
      Integer.highestOneBit(cap - 1) << 1
    }

    override def withBuffer[A](minCapacity: Int)(action: Array[Byte] => A) = {
      //var buf = ref.getAndSet(null)
      var buf = {
        busy += 1
        require(busy == 1)
        cachedBuf
      }
      try {
        if (buf == null || (minCapacity > 0 && minCapacity > buf.length)) {
          buf = new Array[Byte](nextPowerOfTwo(minCapacity max 1024))
        }
        action(buf)
      } finally {
        //ref.compareAndSet(null, buf)
        cachedBuf = buf
        busy -= 1
      }
    }
  }

  abstract class File {
    enum Status {
      case INVALID, READONLY, WDIRTY, WCLEAN
    }
    protected var status = Status.INVALID

    private[xiFilesModule] var name: XString = _
    var rewriteAll: Boolean = _
    var readRes: Int = _ /** result of the last Read operation */
    var readLen: Int = _ /** lenght of the last Read operation */

    def setReadLen(readLen: Int): Unit = {
      this.readLen = readLen
    }

    def setReadRes(readRes: Short): Unit = {
      this.readRes = readRes.toInt
    }

    def getName: XString = this.name

    def init3(fname: XString, writeable: Boolean, rewriteAll: Boolean): Unit = {
      this.name = fname
      this.rewriteAll = rewriteAll
      this.status = if(!writeable) Status.READONLY else if (rewriteAll) Status.WDIRTY else Status.WCLEAN
      assert(rewriteAll <= writeable)
    }

    /** Closes old file. */
    def close(): Unit
  }

  abstract class TextFile extends File {
    /** Closes created file. */
    def closeNew(): Unit

    def readLine(): XString

    def print(format: String, args: Any*): Unit
  }

  def posToInt(pos: Long): Int = {
    assert(pos <= Int.MaxValue)
    pos.toInt
  }

  abstract class RawFile extends File {
    def closeNew(): Unit

    def flush(): Unit = {} /* default implementation is empty */

    def setPos(pos: Long): Unit

    def getPos: Long

    def getPosAsInt = posToInt(getPos)

    def writeBlock(buf: Array[Byte], pos: Int, len: Int): Unit

    def writeFile(in: RawFile): Unit = {
      val MB = 1 << 20
      var toCopy = in.length - in.getPos
      withBufferOfSize((toCopy min MB).toInt) { buf =>
        while (toCopy > 0) {
          val n = in.readBlock(buf)
          if (n < 0) {
            throw new Error("EOFException")
          } else {
            writeBlock(buf, 0, n)
            toCopy -= n
          }
        }
      }
    }

    def readBlock(buf: Array[Byte], pos: Int, len: Int): Int

    def readBlock(buf: Array[Byte]): Int = readBlock(buf, 0, buf.length)

    def readFully(len: Int): Array[Byte] = {
      val buf = new Array[Byte](len)
      val n = if (len > 0) readBlock(buf) else 0
      if (n != len) {
        throw new Error("EOFException")
      }
      buf
    }

    def readFully(): Array[Byte] = {
      val toRead = length - getPos
      assert(toRead.toInt == toRead)
      readFully(toRead.toInt)
    }

    def length: Long

    /** returns length for files less then 2GB.  */
    def lengthAsInt = posToInt(length)
  }

  class SymFile extends File {
    var tag: Int = _
    private[xiFilesModule] var raw: RawFile = _
    private[xiFilesModule] var bpos: Int = _     /* file position of the block */
    private[xiFilesModule] var pos: Int = _      /* position in buf */
    private[xiFilesModule] var len: Int = _      /* buf length (used for read only) */
    private[xiFilesModule] var buf: Array[Byte] = new Array[Byte](1024)
    private[xiFilesModule] var coordName: XString = _
    private[xiFilesModule] var writeable: Boolean = _

    def closeNew(): Unit = {
      assert(rewriteAll)
      sync()
      raw.closeNew()
    }

    /** Closes old file. */
    override def close(): Unit = {
      assert(!rewriteAll)
      sync()
      raw.close()
    }

    def length = raw.length

    def lengthAsInt = raw.lengthAsInt

    def getPos: Long = bpos + pos

    def getPosAsInt: Int = posToInt(getPos)

    def setPos(p: Long): Unit = {
      sync()
      len = 0
      raw.setPos(p)
      bpos = p.toInt
      pos = 0
    }

    def flush(): Unit = {
      sync()
      raw.flush()
    }

    private def sync(): Unit = {
      if (writeable && pos > 0) {
        raw.writeBlock(buf, 0, pos)
        bpos += pos
        pos = 0
      }
    }

    def setCoordName(file: XString): Unit = {
      assert(this.coordName == null)
      this.coordName = file
    }

    /** Writes string: {character} LF */
    def writeJString(s: XString): Unit = {
      for (i <- 0 until s.length) {
        this.writeChar(s.charAtAsChar(i))
      }
      this.writeChar(EOS)
    }

    /** Writes string: {character} LF */
    def writeString(s: String): Unit = {
      var i = 0
      while (i < s.length && s(i) != '\u0000') {
        this.writeChar(s(i))
        i += 1
      }
      this.writeChar(EOS)
    }

    def writeSet(x: Set32): Unit = {
      this.writeCard(x.toUInt)
    }

    def writeSet64(x: Set64): Unit = {
      this.writeLong(x.toLong)
    }

    def writeInt(x: Int): Unit = {
      _write4(x)
    }

    def writeLong(x: Long): Unit = {
      write8(x)
    }

    def writeReal(x: Double): Unit = {
      write8(doubleToRawLongBits(x))
    }

    def writeFloat(x: Float): Unit = {
      _write4(floatToRawIntBits(x))
    }

    def write8(x: Long): Unit = {
      _write4(high32Bits(x))
      _write4(low32Bits(x))
    }

    private def _write4(x: Int): Unit = {
      _write2((x >>> 16) & 0xFFFF)
      _write2(x & 0xFFFF)
    }

    def write2(x: UShort): Unit = _write2(x.toInt)

    private def _write2(x: Int): Unit = {
      write((x >>> 8) & 0xFF)
      write(x & 0xFF)
    }

    def writeChar(ch: Char): Unit = {
      assert(ch >= 0 && ch <= 255)
      write(ch)
    }

    def writeTag(x: Int): Unit = {
      this.writePackedInt(x)
    }

    /** Writes packed integer */
    def writePackedInt(xPar: Int): Unit = {
      var x = xPar
      while (x < -64 || x > 63) {
        write(x & 127)
        x >>= 7
      }
      write(x + 192)
    }

    def writeUInt(x: Int): Unit = {
      assert(x >= 0)
      writeCard(x.toUInt)
    }

    def writeCard(xPar: UInt): Unit = {
      var x = xPar
      while (x > UInt(127)) {
        write((x & UInt(127)).toByte)
        x >>>= 7
      }
      write((x + UInt(128)).toByte)
    }

    /** Write one byte to the file. */
    def write(x: Int): Unit = {
      if (pos >= buf.length) {
        sync()
      }
      buf(pos) = x.toByte
      pos += 1
    }

    /** Reads string: {character} LF */
    def readJString(): XString = {
      val buf = new js.StringBuffer()
      this.readStringToBuf(buf)
      if (this.readRes != allRight) {
        return null
      }
      buf.intern()
    }

    /** Reads string: {character} LF, appends it to the buffer */
    def readStringToBuf(result: js.StringBuffer): Unit = {
      var i = 0
      var ch = read()
      while (ch != EOS) {
        i += 1
        result.appendChar(ch.toChar)
        ch = read()
      }
      readLen = i
    }

    /** Reads packed set. */
    def readSet(): Set32 = this.readCard().toSet32

    /** Reads packed set64. */
    def readSet64(): Set64 = this.readCardLong().toSet64

    def readInt(): Int = this.read4().toInt

    def readLong(): Long = this.read8()

    def readReal(): Double = java.lang.Double.longBitsToDouble(this.read8())

    def readFloat(): Float = java.lang.Float.intBitsToFloat(this.read4().toInt)

    def read8(): Long = {
      val high = read4().toInt
      val low = read4().toInt
      makeLong(low, high)
    }

    def read4(): UInt = {
      val high = read2().toUInt
      val low = read2().toUInt
      (high << 16) + low
    }

    def read2(): UShort = {
      val high = read()
      val low = read()
      ((high << 8) + low).toUShort
    }

    /** Reads packed signed integer */
    def readPackedInt(): Int = {
      var x = read()
      if (x >= 128) {
        return x - 192
      }
      var shift = 7
      var n = x
      x = read()
      while (x < 128) {
        n += x << shift
        shift += 7
        x = read()
      }
      n + ((x - 192) << shift)
    }

    def readUInt(): Int = this.readCard().toInt

    /** Reads packed unsigned integer */
    def readCard(): UInt = {
      var x = read()
      if (x >= 128) {
        return (x - 128).toUInt
      }
      var shift = 7
      var n = x
      x = read()
      while (x < 128) {
        n += x << shift
        shift += 7
        x = read()
      }
      (n + ((x - 128) << shift)).toUInt
    }

    def readCardLong(): ULong = readLong().toULong

    /** Read one byte from the file. Throws exception on read error. */
    def read(): Int = {
      val b = tryRead()
      assert((b < 0) == (readRes == endOfInput))
      if (b < 0) {
        env.errors.fault(ErrMsg445, getName)
      }
      b & 0xFF
    }

    def tryRead(): Int = {
      if (pos >= len) {
        bpos += pos
        pos = 0
        val n = raw.readBlock(buf)
        if (n >= 0) {
          len = n
        } else {
          readRes = endOfInput
          readLen = 0
          return -1
        }
      }
      val b = buf(pos)
      pos += 1
      readLen = 1
      readRes = allRight
      b & 0xFF
    }

    def writeConstValue(v: ConstValue): Unit = v match {
      case v: IntValue    => write(TAG_INT);    writeInt(v.value)
      case v: LongValue   => write(TAG_LONG);   writeLong(v.value)
      case v: FloatValue  => write(TAG_FLOAT);  writeFloat(v.value)
      case v: DoubleValue => write(TAG_DOUBLE); writeReal(v.value)
      case v: StringValue => write(TAG_STR);    writeJString(v.value)
    }

    def readConstValue(): ConstValue = read() match {
      case TAG_INT    => IntValue     (readInt())
      case TAG_LONG   => LongValue    (readLong())
      case TAG_FLOAT  => FloatValue   (readFloat())
      case TAG_DOUBLE => DoubleValue  (readReal())
      case TAG_STR    => StringValue  (readJString())
    }
  }

  object SymFile {
    private val TAG_INT    : Byte = 0
    private val TAG_LONG   : Byte = 1
    private val TAG_FLOAT  : Byte = 2
    private val TAG_DOUBLE : Byte = 3
    private val TAG_STR    : Byte = 4
  }

  abstract class Manager[N <: File] {
    var errmsg: XString = _

    def open0(name: XString, writeable: Boolean, append: Boolean = false): N = ???

    def openToRead  (name: XString): N = open0(name, writeable = false)
    def openToWrite (name: XString): N = open0(name, writeable = true)
    def openToAppend(name: XString): N = open0(name, writeable = true, append = true)
  }

  class SymManager {
    var errmsg: XString = _

    def wrapForRead  (fraw: RawFile): SymFile = wrap0(fraw, writeable = false, rewriteAll = false)
    def wrapForWrite (fraw: RawFile): SymFile = wrap0(fraw, writeable = true, rewriteAll = true)
    def wrapForAppend(fraw: RawFile): SymFile = wrap0(fraw, writeable = true, rewriteAll = false)

    def wrap0(fraw: RawFile, writeable: Boolean, rewriteAll: Boolean): SymFile = {
      assert(fraw != null)
      val f = getSymFromPool
      f.init3(fraw.getName, writeable, rewriteAll)
      f.raw = fraw
      f.bpos = 0
      f.pos = 0
      f.len = 0
      f.coordName = null
      f.writeable = writeable
      f
    }

    def openToRead(name: XString): SymFile = {
      val f = raw.openToRead(name)
      if (f != null) {
        wrapForRead(f)
      } else {
        errmsg = raw.errmsg
        null
      }
    }

    def openToWrite(name: XString): SymFile = {
      val f = raw.openToWrite(name)
      if (f != null) {
        wrapForWrite(f)
      } else {
        errmsg = raw.errmsg
        null
      }
    }
  }

  abstract class FileSys {
    def compareExtSys(ext1: XString, ext2: XString): Boolean
    def createFileDescriptor(fname: XString): FileDescriptor
    def makeExecutable(name: XString): Boolean
    def remove(name: XString): Boolean
    def rename(name: XString, newname: XString): Boolean

    /** Returns path in canonical form.
      *
      * Note: XDS lib has very week heruistics for path canonicalization,
      * so it may  not work in some situations.
      */
    def getCanonicalPath(name: XString): XString

    def modifyTime(name: XString): Int
    def restoreRedAtLevel(level: Int): Unit
    def restoreRed(): Unit
    def saveRed(): Unit

    /** retval < 0 -- ok, else position of error in line */
    def parseRed(line: XString): Int

    /** retval < 0 -- ok, else position of error in line */
    def parseRedAtLevel(s: XString, level: Int): Int

    def checkRedirections(): Unit
    def useFirstDir(name: XString): XString
    def useFirst(name: XString): XString
    def sysLookup(ext: String): XString
    def exists(name: XString): Boolean
    def lookupDir(name: XString, pattern: XString): FileDescriptor
    def lookup(name: XString, lookInCurrentDir: Boolean = true): FileDescriptor
    def existLookups(pat: XString): Boolean
    def listFiles(path: XString): ArrayBuffer[DirEntry]
    def iterateDir(name: XString, /*VAR*/ i: DirIterator): Boolean
    def createDir(name: XString): Boolean
  }

  abstract class DirIterator {
    /** Result `true` means stop iteration now. */
    def entry(name: XString, dir: Boolean): Boolean
  }

  abstract class FileDescriptor {
    var next: FileDescriptor = _ // file descriptor with the same name

    def getLength: Int = {
      val file = openRawFile()
      val length = file.lengthAsInt
      file.close()
      length
    }

    def getFileContents: Array[Byte] = {
      val file = openRawFile()
      val buf = file.readFully()
      file.close()
      buf
    }

    def openRawFile(): RawFile
    def openSymFile(): SymFile
    def openTextFile(): TextFile

    def getIterator: Iterator
    def iterateDir(i: DirIterator): Boolean
    def getEntry(name: XString, ext: XString): FileDescriptor
    def getDir(name: XString): FileDescriptor
    def isDirectory: Boolean
    def exists: Boolean
    def modifyTime(): Int
    def getName: XString
  }

  abstract class Iterator {
    def getRelativeName: XString
    def getFileDescriptor: FileDescriptor
    def next(): Boolean
  }

  class DirEntry(val name: XString, val isDir: Boolean)

  private class PooledSymFile extends SymFile {
    private[xiFilesModule] var closed: Boolean = _

    override def closeNew(): Unit = {
      super.closeNew()
      if (!this.closed) {
        this.closed = true
        releaseSymToPool(this)
      }
    }

    override def close(): Unit = {
      super.close()
      if (!this.closed) {
        this.closed = true
        releaseSymToPool(this)
      }
    }
  }

  val MSG_FILE_OPEN_ERROR = ErrMsg425 /* %S */
  val MSG_FILE_CREATE_ERROR = ErrMsg424 /* %S */
  /** Read results */
  val allRight: Int = 0
  val endOfInput: Int = 1
  val endOfLine: Int = 2 /** for TextFile only */
  /* Redirection levels */
  val RED_LEVEL_REDFILE: Int = 1 // jc.red parsing level
  private val EOS: Char = '\u0000' /* SymFile: end of string */
  var sys: FileSys = _
  var text: Manager[TextFile] = _
  var raw: Manager[RawFile] = _
  /*RO*/ var sym: SymManager = new SymManager()
  private var pool = new Array[PooledSymFile](1)
  private var top: Int = -1

  def setRawManager(m: Manager[RawFile]): Unit = {
    raw = m
  }

  def setFileSys(fs: FileSys): Unit = {
    sys = fs
  }

  private def getSymFromPool: SymFile = {
    val file = if (top == -1) { // pool is empty
      new PooledSymFile()
    } else {
      top -= 1
      pool(top + 1)
    }
    file.closed = false
    file
  }

  private def releaseSymToPool(sym: PooledSymFile): Unit = {
    top += 1
    if (top == pool.length) {
      // realloc
      val newpool = new Array[PooledSymFile](pool.length * 2)
      for (i <- pool.indices) {
        newpool(i) = pool(i)
      }
      pool = newpool
    }
    pool(top) = sym
  }

  def copy(fromF: RawFile, toF: RawFile, closeTo: Boolean = true): Unit = {
    toF.writeFile(fromF)
    fromF.close()
    if (closeTo) {
      toF.closeNew()
    }
  }
}
