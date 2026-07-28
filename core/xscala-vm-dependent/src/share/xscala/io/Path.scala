/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.io.Path.{Rel, rel}
import xscala.properties.OS

import java.io.IOException

object Path {

  private def startsWithWindowsRootDir(s: String) = s.length >= 3 && {
    val c1 = s(0)
    (('a' <= c1 && c1 <= 'z') || ('A' <= c1 && c1 <= 'Z')) && (s(1) == ':') && ((s(2) == '\\') || (s(2) == '/'))
  }

  private def rootPrefix(path: String): Int = {
    if (path startsWith raw"\\") { // Windows UNC root
      2
    } else if (startsWithWindowsRootDir(path)) { // Windows Drive root
      3
    } else if (path startsWith "/") { // Unix root
      1
    } else {
      0
    }
  }

  /** Split path string into sequence of names in order from the last name to the root
    * and count of leading steps to parent directory ("`..`"), if any.
    */
  private def split(s: String): (Int, Array[String]) = {
    var skip = 0
    val names = s.split('\\').flatMap(_.split('/')).reverse.flatMap {
      case "." | "" =>
        Seq.empty
      case ".." =>
        skip += 1
        Seq.empty
      case _ if skip > 0 =>
        skip -= 1
        Seq.empty
      case s =>
        Seq(s)
    }
    (skip, names)
  }

  def apply(s: String): Path = {
    val prefix = rootPrefix(s)
    val (skip, names) = split(s.substring(prefix))
    if (prefix > 0) {
      require(skip == 0, s"Invalid absolute path: $s")
      names.foldRight(root(s))((name, path) => path.withName(name))
    } else {
      names.foldRight(up(skip))((name, path) => path.withName(name))
    }
  }

  def abs(s: String): Abs = apply(s).asInstanceOf[Abs]
  def rel(s: String): Rel = apply(s).asInstanceOf[Rel]

  val dot: Rel = Dot(0)
  def up(count: Int): Rel = Dot(count)
  def root(s: String): Abs = Root(s.substring(0, rootPrefix(s).ensuring(_ > 0)))

  private[Path] abstract case class Node[P <: Path](val name: String, override val parent: P) extends Path {
    def upImpl(count: Int) = parent.up(count - 1)

    override def exe: Path = parent withName os.mangleExeName(name)
    override def script: Path = parent withName os.mangleExeLikeScriptName(name)

    override def toString = parent match {
      case _: Root => s"$parent$name" // root already contains fileSeparator
      case _ => s"$parent$slash$name"
    }
  }

  private[Path] case class Dot(up: Int) extends Rel {
    def upImpl(count: Int) = Dot(up + count)
    def appendTo(path: Path) = path.up(up)

    def name: String = if (up > 0) ".." else "."
    override def toString = "." + s"$slash.." * up
  }

  private[Path] case class Root(val name: String) extends Abs {
    def upImpl(count: Int) = throw new IOException("Can not go upper than root")

    override def toString = name
  }

  trait Rel extends Path {
    private[Path] def appendTo(path: Path): Path
    private[io] def withName(name: String): Rel = new Node[Rel](name, this) with Rel { n =>
      def appendTo(path: Path): Path = (path / n.parent) withName n.name
    }
  }

  trait Abs extends Path {
    private[io] def withName(name: String): Abs = new Node[Abs](name, this) with Abs
  }
}

abstract class Path {
  val os = OS.host
  def slash = os.fileSeparator

  // guaranteed to be normalized
  private[io] lazy val asString = toString

  def exists = FileSystem.exists(this)
  def isFile = FileSystem.isFile(this)

  def isDirectory = FileSystem.isDirectory(this)
  def canExecute = FileSystem.canExecute(this)
  def isRegularFile = isFile

  def absolutePath: Path = FileSystem.abs(this)
  def canonicalPath: Path = FileSystem.canonical(this)

  def listFiles: Seq[Path] = FileSystem.list(this)

  def name: String

  def / (suffix: String): Path = this / rel(suffix)
  def / (suffix: Rel): Path = suffix.appendTo(this)

  private[io] def withName(name: String): Path

  def parent: Path = up(1)
  def up(count: Int): Path = if (count == 0) this else upImpl(count.ensuring(_ > 0))

  protected def upImpl(count: Int): Path

  final def isAbsolute: Boolean = this.isInstanceOf[Path.Abs]
  final def isRelative: Boolean = this.isInstanceOf[Path.Rel]

  def exe: Path = throw new IOException(s"Can not create path to executable for $this")
  def script: Path = throw new IOException(s"Can not create path to script for $this")

  def relativeTo(base: Path): Path.Rel = {
    if (this == base) {
      Path.dot
    } else {
      val thisCanonical = this.canonicalPath.asString
      val baseCanonical = base.canonicalPath.asString

      assert(thisCanonical.startsWith(baseCanonical), s"path $this is not in $base")
      val relativeName = thisCanonical.substring(baseCanonical.length + 1)
      Path.rel(relativeName)
    }
  }
}
