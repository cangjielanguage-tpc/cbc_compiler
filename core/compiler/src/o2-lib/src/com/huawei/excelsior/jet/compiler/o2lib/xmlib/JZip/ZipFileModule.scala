/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.xmlib.JZip

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import xscala.io.DataInput

import java.io.{EOFException, IOException}
import java.lang.ref.SoftReference
import scala.collection.{Map, mutable}
import scala.util.Using

/** This class provides interface to zip file functionality (both in-memory and ordinary file) required by the compiler.
  * Note: the class is pure Java (native-less) implementation of ZipFile.ob2 lib module which is not translated intentionally.
  *
  * @author kit
  * @author vitvit
  */
object ZipFileModule {
  abstract class ZipFile(val name: String) {
    def close(): Unit

    def entries: Iterator[ZipEntry]

    def entryNames: Iterator[XString]

    def getInputStream(entry: ZipEntry) = new ZipFileInputStream(this, entry)

    def getEntry(name: XString): ZipEntry

    @throws[IOException]
    private[JZip] def getInputStream(entry: XString): DataInput
  }

  private class FileZipFile(name: String, private val allEntries: Map[String, Minizip.ZipEntry]) extends ZipFile(name) {
    private[JZip] var numOpened = 1

    override def close(): Unit = {
      numOpened -= 1
      if (numOpened == 0) {
        try {
          fileZipFileCache.remove(name)
        } catch {
          case e: IOException => throw new AssertionError(e)
        }
      }
    }

    override def entries: Iterator[ZipEntry] = allEntries.valuesIterator.map(new ZipEntry(_))

    override def entryNames: Iterator[XString] = allEntries.keysIterator.map(XString.apply)

    override def getEntry(name: XString): ZipEntry = allEntries.get(name.utf8ToString).map(new ZipEntry(_)).orNull

    @throws[IOException]
    override private[JZip] def getInputStream(entry: XString) =
      allEntries.get(entry.utf8ToString).map(ze => {
        val bytesStream = ze.getBytesStream
        if (bytesStream != null) {
          // ze content is loaded already
          bytesStream
        } else {
          // open zip and load this particular entry along with its content, so bytes-stream gets available
          Using.resource(Minizip.openReader(name)) { zr => zr.getEntry(ze.pos).map(_.getBytesStream).orNull }
        }
      }).orNull
  }

  class ZipFileInputStream(file: ZipFile, entry: ZipEntry) {

    private[JZip] val in: DataInput = try {
      file.getInputStream(entry.getName)
    } catch {
      case _: IOException => null
    }

    /** Reads bytes, blocking until all bytes are read.*/
    @throws[IOException]
    private def readFully(b: Array[Byte], offset: Int, length: Int): Unit = {
      val bytesRead = in.getBytes(b, offset, length)
      if (bytesRead != length) {
        throw new EOFException
      }
    }

    def read(buf: Array[Byte]): Boolean = {
      if (in == null) return false
      try {
        readFully(buf, 0, buf.length)
        in.close()
        true
      } catch {
        case _: IOException => false
      }
    }
  }

  /** Map with weak values */
  // TODO: WeakReference(null) instances may remain in the table but currently this is not the case
  private val fileZipFileCache = mutable.HashMap.empty[String, SoftReference[FileZipFile]]

  def newZipFile(name: XString): ZipFile = {
    val keyName = name.toString
    var res = fileZipFileCache.get(keyName) match {
      case Some(ref) => ref.get()
      case None => null
    }
    if (res == null) {
      try {
        res = Using.resource(Minizip.openReader(keyName)) { zr => new FileZipFile(keyName, zr.allEntries()) }
      } catch {
        case _: IOException =>
          return null
      }
      fileZipFileCache.put(keyName, new SoftReference[FileZipFile](res))
    } else {
      res.numOpened += 1
    }
    res
  }
}
