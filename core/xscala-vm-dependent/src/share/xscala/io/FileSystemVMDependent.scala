/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.vm.VMDependent

trait FileSystemVMDependent {
  def isInvalid(path: String): Boolean
  def isDirectory(path: String): Boolean
  def canExecute(path: String): Boolean
  def isFile(path: String): Boolean
  def exists(path: String): Boolean
  def delete(path: String): Boolean
  def setExecutable(path: String, executable: Boolean, ownerOnly: Boolean): Boolean
  def rename(from: String, to: String): Boolean
  def makeDir(path: String): Boolean
  def makeDirs(path: String): Boolean
  def makeFile(path: String): Boolean
  def lastModified(path: String): Long
  def size(path: String): Long
  def canonical(path: String): String
  def list(path: String): Array[String]
  def copy(source: String, target: String, replaceTarget: Boolean): Unit

  def newFileInputStream(path: String): InputStream
  def newFileOutputStream(path: String, append: Boolean): OutputStream
  
  def readAllLines(path: String): Seq[String]
  def write(path: String, bytes: Array[Byte]): Unit
}

object FileSystemVMDependent extends VMDependent[FileSystemVMDependent]