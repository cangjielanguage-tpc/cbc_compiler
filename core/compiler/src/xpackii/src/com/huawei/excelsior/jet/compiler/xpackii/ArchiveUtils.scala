/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii

import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import com.huawei.excelsior.jet.compiler.xminizip.Minizip.{COMPRESS_METHOD_DEFLATE, DOSTIME_BASE}
import xscala.io.{DataInput, Files, Path}
import xscala.properties.OS

import java.io.IOException
import scala.collection.mutable

/** Various utilities for processing of ZIP/JAR archives. */
object ArchiveUtils {
  private val ZIP_MAGIC = Array[Byte](0x50, 0x4b, 0x03, 0x04)
  private val COPY_BUFFER_SIZE = 1024 * 1024
  private val SEP = OS.host.fileSeparator

  /** A global cache used for implementation of [[ArchiveUtils.unzipOnce(String)]]. */
  private val unzipCache = mutable.Map.empty[Path, Path]

  /** Checks whether the given path represents an existing ZIP file.
    *
    * @param path the path to check
    * @return `true` if the path represents an existing ZIP file
    * @throws IOException if an I/O error occurred
    */
  def isZipArchive(path: String): Boolean = {
    val p = Path(path)
    if (!p.exists || !p.isRegularFile) {
      false
    } else {
      val in = DataInput.from(p)
      try {
        val magicBytes = new Array[Byte](ZIP_MAGIC.length)
        val bytesRead = in.getBytes(magicBytes)
        if (bytesRead != ZIP_MAGIC.length) {
          false
        } else {
          magicBytes.sameElements(ZIP_MAGIC)
        }
      } finally {
        if (in != null) in.close()
      }
    }
  }

  /** Puts filtered directory `sourceDir` to the new archive to a related path accordingly `rootDir`
    * with `prefixPath` prefix.
    *
    * @param prefixPath    the prefix to use, or empty string
    * @param rootDir       canonical path to the root source directory, used as base for relative paths in the archive
    * @param sourceDir     canonical path to the source directory to put into the archive, shall be subdirectory of
    *                      `rootDirectory`
    * @param targetZip     the path to the target ZIP archive
    * @param filter        the file and contents filter to use
    * @throws IOException if an I/O error occurred
    */
  def putDirectoryToArchive(prefixPath: Path, rootDir: Path, sourceDir: Path, targetZip: Path, filter: Filter): Unit = {
    val zipFile = Minizip.openWriter(targetZip.toString)
    try {
      putDirectoryToArchive(prefixPath, rootDir, sourceDir, zipFile, filter)
    } finally {
      zipFile.close()
    }
  }

  /** Puts filtered directory `sourceDir` to the existing archive to a related path accordingly `rootDir`
    * with `prefixPath` prefix.
    *
    * @param prefixPath the prefix to use, or [[Path.dot]]
    * @param rootDir    canonical path to the root source directory, used as base for relative paths in the archive
    * @param sourceDir  canonical path to the source directory to put into the archive, shall be subdirectory of
    *                   `rootDirectory`
    * @param zw         the output archive zip writer to use
    * @param filter     the file and contents filter to use
    * @throws IOException if an I/O error occurred
    */
  private[xpackii] def putDirectoryToArchive(prefixPath: Path, rootDir: Path, sourceDir: Path, zw: Minizip.Writer, filter: Filter): Unit = {
    val files = sourceDir.listFiles.sortBy(_.name)
    for (file <- files) {
      if (filter.accept(file)) {
        if (file.isDirectory) {
          putDirectoryToArchive(prefixPath, rootDir, sourceDir / file.name, zw, filter)
        } else {
          val pathInZip = prefixPath / file.relativeTo(rootDir)
          val bytes = filter.filterToBytes(file)
          if (bytes != null) {
            if (filter.resetMtime(file)) {
              zw.putBytesAndExtraToArchive(bytes, extra = null, pathInZip, mtime = DOSTIME_BASE, method = COMPRESS_METHOD_DEFLATE)
            } else {
              zw.putBytesToArchive(bytes, pathInZip)
            }
          } else {
            val zipInMem = filter.filterToZipInMem(file)
            if (zipInMem != Minizip.ZIP_IN_MEM_INVALID) {
              zw.putZipInMemToArchive(zipInMem, pathInZip)
            } else {
              zw.putFileToArchive(file, pathInZip)
            }
          }
        }
      }
    }
  }

  private[xpackii] def putDirectoryToArchive(prefixPath: Path, rootDir: Path, sourceDir: Path, zw: Minizip.Writer): Unit =
    putDirectoryToArchive(prefixPath, rootDir, sourceDir, zw, Filter.ACCEPT_ALL)

  private def isEmptyDir(f: Path): Boolean = {
    if (!f.isDirectory) {
      false
    } else {
      f.listFiles.isEmpty
    }
  }

  /** Ensures that `targetDir` points to an empty directory.
    * Creates a directory if it doesn't exist or cleans an existing file/directory if `cleanTarget` is set.
    *
    * @param targetDir   a path to check.
    * @param cleanTarget should remove `targetDir` if it already exists
    * @return `true` if `targetDir` points to an empty directory,
    *         or `false` if `targetDir` is already exists but not a directory or is not empty
    *         and `cleanTarget` is not set.
    * @throws IOException if an I/O error occurred
    */
  def ensureIsEmptyDir(targetDir: String, cleanTarget: Boolean): Boolean = {
    val dir = Path(targetDir)
    if (!dir.exists) {
      Files.makeDir(dir)
      true
    } else if (cleanTarget) {
      Files.deleteRecursively(dir)
      Files.makeDir(dir)
      true
    } else {
      isEmptyDir(dir)
    }
  }

  /** Unzips `zipArchive` zip to `targetDir` directory.
    *
    * @param zipArchive zip to extract
    * @param targetDir  a directory path where `zipArchive`'s content is unpacked
    * @throws IOException if an I/O error occurred
    */
  def unzip(zipArchive: String, targetDir: String): Unit = {
    val target = Path(targetDir)
    assert(isEmptyDir(target))
    Minizip.unzipArchive(zipArchive, targetDir)
  }

  /** Unzips `zipArchive` content to a temporary directory.
    * If `zipArchive` was already unzipped this method returns the unzip target directory.
    *
    * @param zipArchive zip to extract
    * @return the temporary directory path
    * @throws IOException if an I/O error occurred
    */
  def unzipOnce(zipArchive: String): String = {
    val zipArchivePath = Path(zipArchive).canonicalPath
    unzipCache.get(zipArchivePath) match {
      case Some(tempDirPath) => tempDirPath.toString
      case None =>
        val tempDir = Files.makeTempDir("jc-unpack-").get.toString
        unzip(zipArchive, tempDir)
        unzipCache.put(zipArchivePath, Path(tempDir))
        tempDir
    }
  }
}