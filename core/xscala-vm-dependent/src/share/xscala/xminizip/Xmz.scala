/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.xminizip

import xscala.xminizip.Xmz.{ZrHandle, ZwHandle, ZwInMemHandle}
import xscala.vm.VMDependent

object Xmz extends VMDependent[Xmz] {
  private[xminizip] type ZrHandle = Long
  private[xminizip] type ZwHandle = Long
  private[xminizip] type ZwInMemHandle = Long

  override def get: Xmz = {
    val instance = super.get
    instance.init()
    instance
  }
}

protected trait Xmz {

  private[xminizip] def init(): Unit

  def unzip(zipArchive: String, targetDir: String): Unit

  // returns false when respective entry was not found in zipArchive
  def unzipEntry(zipArchive: String, name: String, dstPath: String): Boolean

  // region ZipReader

  def openZipReader(zipArchive: String): ZrHandle

  def zrLookupFirstEntry(zr: ZrHandle, loadBytes: Boolean): Object

  def zrLookupNextEntry(zr: ZrHandle, loadBytes: Boolean): Object

  def zrLookupEntry(zr: ZrHandle, name: String, loadBytes: Boolean): Object

  def zrGetEntry(zr: ZrHandle, pos: Long, loadBytes: Boolean): Object

  // navigates zr to the first non-dir entry, returns NULL if no such entries in the reader
  def zrGotoFirstEntry(zr: ZrHandle): String

  // navigates zr to the next non-dir entry, returns NULL if no more entries in zr
  def zrGotoNextEntry(zr: ZrHandle): String

  def zrCopyEntryToZW(zr: ZrHandle, zw: ZwInMemHandle): Unit

  def zrCopyEntryAtPosToZW(zr: ZrHandle, pos: Long, zw: ZwInMemHandle): Unit

  def closeZipReader(zr: ZrHandle): Unit

  // endregion

  // region ZipWriter

  def openZipWriter(zipArchive: String, append: Boolean): ZwHandle

  def createZipInMemWriter(): ZwInMemHandle

  def zwAddAllFromZip(zw: ZwHandle, srcZip: String): Unit

  def zwAddFile(zw: ZwHandle, filepath: String, pathInArch: String): Unit

  def zwAddBytesAsFile(zw: ZwHandle, bytes: Array[Byte], pathInArch: String): Unit

  def zwAddBytesAndExtraAsFile(zw: ZwHandle, bytes: Array[Byte], extra: Array[Byte], pathInArch: String, mtime: Long, isDir: Boolean, method: Char, level: Short): Unit

  def zwAddZipInMem(zw: ZwHandle, zim: ZwInMemHandle, pathInArch: String): Unit

  def closeZipWriter(zw: ZwHandle): Unit

  // endregion

  // region ZipEntry

  def zeName(ze: Object): String
  def zePos(ze: Object): Long
  def zeSize(ze: Object): Int
  def zeTime(ze: Object): Long
  def zeIsDir(ze: Object): Boolean
  def zeBytesStream(ze: Object): Object

  // endregion
}
