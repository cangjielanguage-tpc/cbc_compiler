/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.xminizip

import xscala.internal.*
import xscala.xminizip.Xmz.{ZrHandle, ZwHandle, ZwInMemHandle}
import xscala.xminizip.XmzJET.*

import scala.annotation.static

final class XmzJET extends Xmz {
  private[xminizip] def init(): Unit = {}

  def unzip(zipArchive: String, targetDir: String): Unit = unzip0(zipArchive, targetDir)
  def unzipEntry(zipArchive: String, name: String, dstPath: String): Boolean = unzipEntry0(zipArchive, name, dstPath)

  // region ZipReader

  def openZipReader(zipArchive: String): ZrHandle = openZipReader0(zipArchive)

  def zrLookupFirstEntry(zr: ZrHandle, loadBytes: Boolean): Object = wrapForeign(zrLookupFirstEntry0(zr, loadBytes))

  def zrLookupNextEntry(zr: ZrHandle, loadBytes: Boolean): AnyRef = wrapForeign(zrLookupNextEntry0(zr, loadBytes))

  def zrLookupEntry(zr: ZrHandle, name: String, loadBytes: Boolean): AnyRef = wrapForeign(zrLookupEntry0(zr, name, loadBytes))

  def zrGetEntry(zr: ZrHandle, pos: Long, loadBytes: Boolean): AnyRef = wrapForeign(zrGetEntry0(zr, pos, loadBytes))

  // navigates zr to the first non-dir entry, returns NULL if no such entries in the reader
  def zrGotoFirstEntry(zr: ZrHandle): String = zrGotoFirstEntry0(zr)

  // navigates zr to the next non-dir entry, returns NULL if no more such entries
  def zrGotoNextEntry(zr: ZrHandle): String = zrGotoNextEntry0(zr)

  def zrCopyEntryToZW(zr: ZrHandle, zw: ZwInMemHandle): Unit = zrCopyEntryToZW0(zr, zw)

  def zrCopyEntryAtPosToZW(zr: ZrHandle, pos: Long, zw: ZwInMemHandle): Unit = zrCopyEntryAtPosToZW0(zr, pos, zw)

  def closeZipReader(zr: ZrHandle): Unit = closeZipReader0(zr)

  // endregion

  // region ZipWriter

  def openZipWriter(zipArchive: String, append: Boolean): ZwHandle = openZipWriter0(zipArchive, append)

  def createZipInMemWriter(): ZwInMemHandle = createZipInMemWriter0()

  def zwAddAllFromZip(zw: ZwHandle, srcZip: String): Unit = zwAddAllFromZip0(zw, srcZip)

  def zwAddFile(zw: ZwHandle, filepath: String, pathInArch: String): Unit = zwAddFile0(zw, filepath, pathInArch)

  def zwAddBytesAsFile(zw: ZwHandle, bytes: Array[Byte], pathInArch: String): Unit = zwAddBytesAsFile0(zw, bytes, pathInArch)

  def zwAddBytesAndExtraAsFile(zw: ZwHandle, bytes: Array[Byte], extra: Array[Byte], pathInArch: String, mtime: Long, isDir: Boolean, method: Char, level: Short): Unit =
    zwAddBytesAndExtraAsFile0(zw, bytes, extra, pathInArch, mtime, isDir, method, level)

  def zwAddZipInMem(zw: ZwHandle, zim: ZwInMemHandle, pathInArch: String): Unit = zwAddZipInMem0(zw, zim, pathInArch)

  def closeZipWriter(zw: ZwHandle): Unit = closeZipWriter0(zw)

  // endregion

  // region ZipEntry

  def zeName(ze: AnyRef): String = zeName0(unwrapForeign(ze))
  def zePos(ze: AnyRef): Long = zePos0(unwrapForeign(ze))
  def zeSize(ze: AnyRef): Int = zeSize0(unwrapForeign(ze))
  def zeTime(ze: AnyRef): Long = zeTime0(unwrapForeign(ze))
  def zeIsDir(ze: AnyRef): Boolean = zeIsDir0(unwrapForeign(ze))
  def zeBytesStream(ze: AnyRef): Object = wrapForeign(zeBytesStream0(unwrapForeign(ze)))

  // endregion
}

private object XmzJET {

  @native @static def unzip0(zipArchive: String, targetDir: String): Unit
  @native @static def unzipEntry0(zipArchive: String, name: String, dstPath: String): Boolean

  // region ZipReaderNatives

  @native @static def openZipReader0(zipArchive: String): ZrHandle

  @native @static def zrLookupFirstEntry0(zr: ZrHandle, loadBytes: Boolean): ForeignRef0

  @native @static def zrLookupNextEntry0(zrHandle: ZrHandle, loadBytes: Boolean): ForeignRef0

  @native @static def zrLookupEntry0(zrHandle: ZrHandle, name: String, loadBytes: Boolean): ForeignRef0

  @native @static def zrGetEntry0(zrHandle: ZrHandle, pos: Long, loadBytes: Boolean): ForeignRef0

  // navigates zr to the first non-dir entry, returns NULL if no such entries in the reader
  @native @static def zrGotoFirstEntry0(zrHandle: ZrHandle): String

  // navigates zr to the next non-dir entry, returns NULL if no more such entries
  @native @static def zrGotoNextEntry0(zrHandle: ZrHandle): String

  @native @static def zrCopyEntryToZW0(zrHandle: ZrHandle, zw: ZwInMemHandle): Unit

  @native @static def zrCopyEntryAtPosToZW0(zrHandle: ZrHandle, pos: Long, zw: ZwInMemHandle): Unit

  @native @static def closeZipReader0(zr: ZrHandle): Unit

  // endregion

  // region ZipWriterNatives

  @native @static def openZipWriter0(zipArchive: String, append: Boolean): ZwHandle

  @native @static def createZipInMemWriter0(): ZwInMemHandle

  @native @static def zwAddAllFromZip0(zwHandle: ZwHandle, srcZip: String): Unit

  @native @static def zwAddFile0(zwHandle: ZwHandle, filepath: String, pathInArch: String): Unit

  @native @static def zwAddBytesAsFile0(zwHandle: ZwHandle, bytes: Array[Byte], pathInArch: String): Unit

  @native @static def zwAddBytesAndExtraAsFile0(zwHandle: ZwHandle, bytes: Array[Byte], extra: Array[Byte], pathInArch: String, mtime: Long, isDir: Boolean, method: Char, level: Short): Unit

  @native @static def zwAddZipInMem0(zwHandle: ZwHandle, zim: ZwInMemHandle, pathInArch: String): Unit

  @native @static def closeZipWriter0(zwHandle: ZwHandle): Unit

  // endregion

  // region ZipEntryNatives

  @native @static def zeName0(ze: ForeignRef0): String

  @native @static def zePos0(ze: ForeignRef0): Long

  @native @static def zeSize0(ze: ForeignRef0): Int

  @native @static def zeTime0(ze: ForeignRef0): Long

  @native @static def zeIsDir0(ze: ForeignRef0): Boolean

  @native @static def zeBytesStream0(ze: ForeignRef0): ForeignRef0

  // endregion
} 