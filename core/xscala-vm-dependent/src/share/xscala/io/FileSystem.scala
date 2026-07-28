/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.properties.Properties

import java.io.IOException

object FileSystem {
  def isDirectory(path: Path)         = verify(path).exists(FileSystemVMDependent.get.isDirectory)
  def canExecute(path: Path): Boolean = verify(path).exists(FileSystemVMDependent.get.canExecute)
  def isFile(path: Path): Boolean     = verify(path).exists(FileSystemVMDependent.get.isFile)
  def exists(path: Path): Boolean     = verify(path).exists(FileSystemVMDependent.get.exists)
  def delete(path: Path): Boolean     = verify(path).exists(FileSystemVMDependent.get.delete)

  def setExecutable(path: Path, executable: Boolean, ownerOnly: Boolean): Boolean =
    verify(path).exists(FileSystemVMDependent.get.setExecutable(_, executable, ownerOnly))

  def rename(from: Path, to: Path): Boolean =
    (verify(from), verify(to)) match {
      case (Some(f), Some(t)) => FileSystemVMDependent.get.rename(f, t)
      case _ => false
    }

  def makeDir(path: Path): Boolean    = verify(path).exists(FileSystemVMDependent.get.makeDir)
  def makeDirs(path: Path): Boolean   = verify(path).exists(FileSystemVMDependent.get.makeDirs)
  def makeFile(path: Path): Boolean   = verify(path).exists(FileSystemVMDependent.get.makeFile)
  def lastModified(path: Path): Long  = verify(path).map(FileSystemVMDependent.get.lastModified).getOrElse(0)
  def size(path: Path): Long          = verify(path).map(FileSystemVMDependent.get.size).getOrElse(0)

  def abs(path: Path): Path = {
    if (path.isAbsolute) {
      path
    } else {
      Path(Properties.get.userDir()) / path.asInstanceOf[Path.Rel]
    }
  }

  def canonical(path: Path): Path = {
    def errMsg: String = s"Path \"$path\" is invalid"

    if (verify(path).isEmpty) {
      throw new IOException(errMsg)
    }
    validate(abs(path)).map(FileSystemVMDependent.get.canonical).map(Path.apply).getOrElse {
      throw new IOException(errMsg)
    }
  }

  def list(path: Path): Seq[Path] = {
    val names = verify(path).map(FileSystemVMDependent.get.list).orNull
    if (names == null) {
      Seq.empty
    } else {
      Seq.from(names.iterator.map(path.withName))
    }
  }

  def copy(source: Path, target: Path, replaceTarget: Boolean): Unit =
    (verify(source), verify(target)) match {
      case (Some(s), Some(t)) => FileSystemVMDependent.get.copy(s, t, replaceTarget)
      case _ => throw new IOException(s"One of \"$source\", \"$target\" is invalid")
    }

  def validate(path: Path): Option[String] = {
    Option.when(!FileSystemVMDependent.get.isInvalid(path.asString))(path.asString)
  }

  // TODO: merge this with xscala.io.InputStream
  def newFileInputStream(path: Path): DataInput = {
    val input = verify(path).map(FileSystemVMDependent.get.newFileInputStream(_)).get
    new DataInput {
      def available: Int = input.available()
      def getByte(): Int = input.read()
      override def getBytes(data: Array[Byte]): Int = input.read(data)
      override def getBytes(data: Array[Byte], offset: Int, size: Int): Int = input.read(data, offset, size)
      override def skip(n: Int) = input.skip(n)
      override def close(): Unit = input.close()
    }
  }

  // TODO: merge this with xscala.io.OutputStream
  def newFileOutputStream(path: Path, append: Boolean): DataOutput = {
    val output = verify(path).map(FileSystemVMDependent.get.newFileOutputStream(_, append)).get
    new DataOutput {
      override def putByte(b: Int): Unit = output.write(b)
      override def putBytes(data: Array[Byte]): Unit = output.write(data)
      override def putBytes(data: Array[Byte], offset: Int, size: Int): Unit = output.write(data, offset, size)
      override def flush(): Unit = output.flush()
      override def close(): Unit = output.close()
    }
  }

  def readAllLines(path: Path): Seq[String] =
    verify(path).map(FileSystemVMDependent.get.readAllLines(_)).get

  def write(path: Path, bytes: Array[Byte]): Unit = {
    verify(path) match {
      case Some(s) => FileSystemVMDependent.get.write(s, bytes)
      case _ => throw new IOException(s"$path is invalid")
    }
  }

  private def verify(path: Path): Option[String] = validate(path)
}
