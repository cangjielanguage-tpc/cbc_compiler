/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.properties.Properties
import xscala.util.Random

import scala.collection.mutable.ArrayBuffer
import scala.util.Using

/** Utilities for files and directories operations.
  *
  * @author conwor
  */
object Files {
  def delete(x: Path): Boolean = {
    FileSystem.delete(x)
  }

  def deleteRecursively(x: Path): Unit = {
    if (x.isDirectory) {
      x.listFiles foreach deleteRecursively
    }
    delete(x)
  }

  def cleanDir(dir: Path): Unit = {
    if (dir.exists) {
      deleteRecursively(dir)
    }
    makeDir(dir)
  }

  def createNewFile(path: Path): Boolean = FileSystem.makeFile(path)

  def ensureDirExists(dir: Path): Unit = {
    if (dir.exists) {
      assert(dir.isDirectory)
    } else {
      makeDir(dir)
    }
  }

  def makeDir(dir: Path, withParents: Boolean = true): Boolean = {
    if (withParents) {
      FileSystem.makeDirs(dir)
    } else {
      FileSystem.makeDir(dir)
    }
  }

  def makeTempDir(prefix: String): Option[Path] = {
    val possiblePathIterator: Iterator[Path] = new Iterator[Path] {
      val prng = Random.PRNG()
      val tmpDir = Path(Properties.get.tmpDir())

      override def hasNext = true
      override def next() = tmpDir / s"$prefix${prng.next}" // TODO: format suffix as unsigned long
    }

    for {
      nonExistentPath <- possiblePathIterator.find(p => !p.exists)
      if makeDir(nonExistentPath, withParents = false)
    } yield nonExistentPath
  }

  def rename(source: Path, target: Path): Boolean = {
    FileSystem.rename(source, target)
  }

  def readAllLines(path: Path): collection.Seq[String] = {
    FileSystem.readAllLines(path)
  }

  def write(path: Path, bytes: Array[Byte]): Unit = {
    FileSystem.write(path, bytes)
  }

  def copy(source: Path, target: Path, replaceExisting: Boolean = false): Unit = {
    FileSystem.copy(source, target, replaceExisting)
  }

  def getLastModifiedTime(path: Path): Long = {
    FileSystem.lastModified(path)
  }

  def setExecutable(path: Path, executable: Boolean, ownerOnly: Boolean): Boolean = {
    FileSystem.setExecutable(path, executable, ownerOnly)
  }

  def size(path: Path): Long = {
    FileSystem.size(path)
  }

  def readAllBytes(path: Path): ByteBuffer = {
    Using.resource(DataInput.from(path)) { in =>
      // TODO: create a more performant utility method to read DataInput until the end
      val buf = new ByteBuffer()
      var byte = in.getByte()
      while (byte != -1) {
        buf.putByte(byte)
        byte = in.getByte()
      }
      buf
    }
  }
}
