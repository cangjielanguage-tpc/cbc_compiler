/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.xminizip

import xscala.xminizip.Xmz.{ZrHandle, ZwHandle, ZwInMemHandle}

object XmzJDK {
  // When moving this class or changing the signature - take care of it's usage in libxminizip
  class JZipEntry(val name: String, val pos: Long, val size: Int, val time: Long, val isDir: Boolean, val bytes: Array[Byte])

  private lazy val ensureInited = doInit()
  private def doInit(): Boolean = { System.loadLibrary("xminizip"); true }
}

final class XmzJDK extends Xmz {

  override private[xminizip] def init(): Unit = XmzJDK.ensureInited

  @native
  def unzip(zipArchive: String, targetDir: String): Unit

  @native
  def unzipEntry(zipArchive: String, name: String, dstPath: String): Boolean

  // region ZipReader

  @native
  def openZipReader(zipArchive: String): ZrHandle

  @native
  def zrLookupFirstEntry(zr: ZrHandle, loadBytes: Boolean): Object

  @native
  def zrLookupNextEntry(zr: ZrHandle, loadBytes: Boolean): Object

  @native
  def zrLookupEntry(zr: ZrHandle, name: String, loadBytes: Boolean): Object

  @native
  def zrGetEntry(zr: ZrHandle, pos: Long, loadBytes: Boolean): Object

  // navigates zr to the first non-dir entry, returns NULL if no such entries in the reader
  @native
  def zrGotoFirstEntry(zr: ZrHandle): String

  // navigates zr to the next non-dir entry, returns NULL if no more such entries
  @native
  def zrGotoNextEntry(zr: ZrHandle): String

  @native
  def zrCopyEntryToZW(zr: ZrHandle, zw: ZwInMemHandle): Unit

  @native
  def zrCopyEntryAtPosToZW(zr: ZrHandle, pos: Long, zw: ZwInMemHandle): Unit

  @native
  def closeZipReader(zr: ZrHandle): Unit

  // endregion

  // region ZipWriter

  @native
  def openZipWriter(zipArchive: String, append: Boolean): ZwHandle

  @native
  def createZipInMemWriter(): ZwInMemHandle

  @native
  def zwAddAllFromZip(zw: ZwHandle, srcZip: String): Unit

  @native
  def zwAddFile(zw: ZwHandle, filepath: String, pathInArch: String): Unit

  @native
  def zwAddBytesAsFile(zw: ZwHandle, bytes: Array[Byte], pathInArch: String): Unit

  @native
  def zwAddBytesAndExtraAsFile(zw: ZwHandle, bytes: Array[Byte], extra: Array[Byte], pathInArch: String, mtime: Long, isDir: Boolean, method: Char, level: Short): Unit

  @native
  def zwAddZipInMem(zw: ZwHandle, zim: ZwInMemHandle, pathInArch: String): Unit

  @native
  def closeZipWriter(zw: ZwHandle): Unit

  // endregion

  // region ZipEntry

  private inline def asJava(o: Object): XmzJDK.JZipEntry = o.asInstanceOf[XmzJDK.JZipEntry]
  def zeName(ze: Object): String = asJava(ze).name
  def zePos(ze: Object): Long = asJava(ze).pos
  def zeSize(ze: Object): Int = asJava(ze).size
  def zeTime(ze: Object): Long = asJava(ze).time
  def zeIsDir(ze: Object): Boolean = asJava(ze).isDir
  def zeBytesStream(ze: Object): Object = {
    val bytes = asJava(ze).bytes
    if (bytes == null) null else new java.io.ByteArrayInputStream(bytes)
  }

  // endregion
}
