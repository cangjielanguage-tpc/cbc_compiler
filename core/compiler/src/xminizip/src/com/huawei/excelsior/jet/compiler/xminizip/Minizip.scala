/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xminizip

import xscala.io.{DataInput, Path}
import xscala.sync.Sync.{Lock, newLock}
import xscala.xminizip.Xmz

import java.io.{Closeable, IOException}
import scala.collection.{Map, mutable}
import scala.util.{Failure, Success, Using}

object Minizip {
  private[xminizip] type ZrHandle = Long
  private[xminizip] type ZwHandle = Long
  type ZwInMemHandle = Long
  val ZIP_IN_MEM_INVALID: ZwInMemHandle = 0

  val xminizipLibName = System.mapLibraryName("xminizip")

  // seconds between 1980-01-01 00:00:00 and Epoch time (unixtime base) that is 1970-01-01 00:00:00
  val DOSTIME_BASE = 315532800

  val COMPRESS_METHOD_STORE: Char = 0
  val COMPRESS_METHOD_DEFLATE: Char = 8

  val COMPRESS_LEVEL_DEFAULT: Short = -1

  @throws[IOException]
  def unzipArchive(zipArchive: String, targetDir: String): Unit = Xmz.get.unzip(zipArchive, targetDir)

  @throws[IOException]
  def unzipEntry(zipArchive: String, name: String, dstPath: String): Boolean = Xmz.get.unzipEntry(zipArchive, name,  dstPath)

  @throws[IOException]
  def openZipIterator(zipArchive: String): ZipIterator = new ZipIterator(Xmz.get.openZipReader(zipArchive))

  @throws[IOException]
  def openReader(zipArchive: String): ZipReader = new ZipReader(Xmz.get.openZipReader(zipArchive))

  @throws[IOException]
  def openWriter(zipArchive: String, append: Boolean = false): Writer = new Writer(Xmz.get.openZipWriter(zipArchive, append))

  /**
    * Created zip-in-mem will be deleted when it gets written to a Writer by putZipInMemToArchive 
    */
  @throws[IOException]
  def createZipInMem(): ZwInMemHandle = Xmz.get.createZipInMemWriter()

  /**
    * Passed zip-archive is filtered and entry by entry copied to a new zip-in-memory archive.
    * Resulted zip-in-memory handle is supposed to be passed to [[Writer.putZipInMemToArchive()]]
    * 
    * @param zip    source archive to be filtered into zip-in-memory
    * @param filter the filter to be applied for source archive entries before copying to zip-in-memory
    * @return       resulted zip-in-memory handle
    */
  def filterZipToMem(zip: Path, filter: String => Boolean): ZwInMemHandle = {
    val zipInMemWithFilteredContent = Minizip.createZipInMem()
    val zipInMem = Using(new ZipReaderBase(Xmz.get.openZipReader(zip.absolutePath.toString))) { reader =>
      var curEntry: String = Xmz.get.zrGotoFirstEntry(reader.handle)
      while (curEntry != null) {
        if (filter(curEntry)) {
          Xmz.get.zrCopyEntryToZW(reader.handle, zipInMemWithFilteredContent)
        }
        curEntry = Xmz.get.zrGotoNextEntry(reader.handle)
      }
      zipInMemWithFilteredContent
    }
    zipInMem match {
      case Failure(e) => Xmz.get.closeZipWriter(zipInMemWithFilteredContent); throw e
      case Success(zipInMemWithFilteredContent) => zipInMemWithFilteredContent
    }
  }

  def copyEntriesToWriter(srcZip: String, filter: String => Boolean, zw: Writer): Unit =
    Using.resource(new ZipReader(Xmz.get.openZipReader(srcZip))) { srcReader =>
      var curEntry: String = Xmz.get.zrGotoFirstEntry(srcReader.handle)
      while (curEntry != null) {
        if (filter(curEntry)) {
          Xmz.get.zrCopyEntryToZW(srcReader.handle, zw.handle)
        }
        curEntry = Xmz.get.zrGotoNextEntry(srcReader.handle)
      }
    }

  private[Minizip] class ZipReaderBase(val handle: ZrHandle) extends Closeable {
    @throws[IOException]
    override def close(): Unit = Xmz.get.closeZipReader(handle)
  }

  /** Allows to read non-dir zip entry names one by one. */
  class ZipIterator(handle: ZrHandle) extends ZipReaderBase(handle) with Iterator[String] {
    private var curEntry: String = Xmz.get.zrGotoFirstEntry(handle)

    override def hasNext = curEntry != null

    override def next(): String = {
      assert(hasNext)
      val res = curEntry
      curEntry = Xmz.get.zrGotoNextEntry(handle)
      res
    }
  }

  /** Allows to read a zip entry by its name or all the entries at once */
  class ZipReader(handle: ZrHandle) extends ZipReaderBase(handle) {

    private val lock = newLock()

    @throws[IOException]
    def lookupEntry(name: String, loadBytes: Boolean = false): Option[ZipEntry] = lock.sync {
      Option(Xmz.get.zrLookupEntry(handle, name, loadBytes)).map(ZipEntry(_))
    }

    @throws[IOException]
    def getEntry(pos: Long, loadBytes: Boolean = true): Option[ZipEntry] = lock.sync {
      Option(Xmz.get.zrGetEntry(handle, pos, loadBytes)).map(ZipEntry(_))
    }

    @throws[IOException]
    def allEntries(loadBytes: Boolean = false): Map[String, ZipEntry] = lock.sync {
      // not immutable.ListMap as it is not in xscala stdlib
      val builder = mutable.LinkedHashMap.newBuilder[String, Minizip.ZipEntry]
      var ze = Xmz.get.zrLookupFirstEntry(handle, loadBytes)
      while (ze != null) {
        builder.addOne(Xmz.get.zeName(ze), ZipEntry(ze))
        ze = Xmz.get.zrLookupNextEntry(handle, loadBytes)
      }
      builder.result()
    }

    @throws[IOException]
    override def close(): Unit = lock.sync {
      Xmz.get.closeZipReader(handle)
    }

    @throws[IOException]
    def copyEntryAtPosToWriter(pos: Long, zw: Writer): Unit = {
      Xmz.get.zrCopyEntryAtPosToZW(handle, pos, zw.handle)
    }
  }

  class Writer (private[Minizip] val handle: ZwHandle) extends Closeable {
    @throws[IOException]
    def putFileToArchive(file: Path, pathInArch: Path): Unit = {
      Xmz.get.zwAddFile(handle, file.absolutePath.toString, prepareFilepath(pathInArch))
    }

    @throws[IOException]
    def putAllFromAnotherZipToArchive(anotherZip: Path): Unit =
      Xmz.get.zwAddAllFromZip(handle, anotherZip.absolutePath.toString)

    @throws[IOException]
    def putZipInMemToArchive(zim: ZwInMemHandle, pathInArch: Path): Unit = {
      Xmz.get.zwAddZipInMem(handle, zim, prepareFilepath(pathInArch))
    }

    @throws[IOException]
    def putBytesToArchive(bytes: Array[Byte], pathInArch: Path): Unit = {
      // currently compress_method is always DEFLATE in minizip.zipWriterAddBytesAsFile
      Xmz.get.zwAddBytesAsFile(handle, bytes, prepareFilepath(pathInArch))
    }

    private def prepareFilepath(pathInArch: Path): String = {
      val filepath = pathInArch.toString.replace(pathInArch.slash, '/')
      // On Windows, if path contains dots, xminizip will add to archive
      // directories named `.` or `..`, thus malformed archive will be generated.
      //
      // [[xscala.io.Path]] resolves inner dots when created,
      // however, relative paths still contain at least one leading dot.
      // Here we handle this case by manually dropping leading dot prefix.
      // TODO: remove when Path implementation is reworked.
      if (filepath startsWith "./") {
        filepath.substring(2)
      } else {
        filepath
      }
    }

    @throws[IOException]
    def putBytesAndExtraToArchive(bytes: Array[Byte], extra: Array[Byte], pathInArch: Path, mtime: Long, isDir: Boolean = false,
                                  method: Char = COMPRESS_METHOD_STORE, level: Short = COMPRESS_LEVEL_DEFAULT): Unit =
      Xmz.get.zwAddBytesAndExtraAsFile(handle, bytes, extra, prepareFilepath(pathInArch), mtime, isDir, method, level)

    @throws[IOException]
    override def close(): Unit = Xmz.get.closeZipWriter(handle)
  }

  class ZipEntry(ze: Object) {
    def name: String = Xmz.get.zeName(ze)
    def pos: Long = Xmz.get.zePos(ze)
    def size: Int = Xmz.get.zeSize(ze)
    def time: Long = Xmz.get.zeTime(ze)
    def isDir: Boolean = Xmz.get.zeIsDir(ze)

    def getBytesStream: DataInput =  {
      val availableBytesStream = Xmz.get.zeBytesStream(ze)
      // the stream can be null as zip-entry in JET-based VM can be loaded partially - without content
      if (availableBytesStream != null) {
        DataInput.wrapHandle(availableBytesStream)
      } else {
        null
      }
    }
  }
}
