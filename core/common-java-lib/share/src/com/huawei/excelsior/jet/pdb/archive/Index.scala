/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.pdb.archive

import com.huawei.excelsior.jet.common.XString
import xscala.io.TextOutput

import Index.*

object Index {
  /** Entry ID is local to `Index` instance. All valid IDs are positive and non-zero. */
  type EntryID = Int
  final val NO_ENTRY: EntryID = 0

  /** Internal node ID, has no meaning outside single `traverse` call. */
  type NodeID = Int

  trait Processor {
    def root(rootID: NodeID): Unit = {}
    def forward(dstID: NodeID, chars: Array[Byte], start: Int, charsCnt: Int): Unit
    def backward(srcID: NodeID, dstID: NodeID, charsCnt: Int): Unit
    def leaf(nodeID: NodeID, entryID: EntryID): Unit
  }

  class WithSuffix[I <: Index](val dir: I, val suffix: XString) extends Index {
    protected def stripSuffix(name: XString) =
      if (name.endsWith(suffix)) {
        name.substring(0, name.length - suffix.length)
      } else null

    override def find(name: XString): Int = {
      val baseName = stripSuffix(name)
      if (baseName != null) dir.find(baseName) else 0
    }

    override def traverse(processor: Processor): Unit = dir.traverse(processor)
    override def iterate(f: (XString, Int) => Unit): Unit = {
      dir.iterate { (s, i) => f(s.concat(suffix), i) }
    }
  }

  class WritableWithSuffix[I <: RWIndex](_dir: I, _suffix: XString) extends WithSuffix(_dir, _suffix) with RWIndex {
    override def add(name: XString, recID: Int, allowReplace: Boolean): Int = {
      val baseName = stripSuffix(name)
      assert(baseName != null)
      dir.add(baseName, recID, allowReplace)
    }
  }

  def withSuffix[I <: Index](dir: I, suffix: XString) = new WithSuffix(dir, suffix)

  def withSuffixW[I <: RWIndex](dir: I, suffix: XString) = new WritableWithSuffix(dir, suffix)
}

/** `Index` is collection of names (strings).
  * Implementations expected to be very memory-efficient
  * for set of strings with common prefixes, such as set of paths
  * for files in some (sub-)directory, or list of entries of a zip archive.
  *
  * @author paul
  */
trait Index {
  /** Returns ID of named entry in this `Index` or NO_ENTRY. */
  def find(name: XString): EntryID

  /** Low-level traversal of this `Index` instance. */
  def traverse(processor: Processor): Unit

  /** Iterate all entries of this `Index`. */
  def iterate(f: (XString, EntryID) => Unit): Unit = {
    traverse(new Processor() {
      var buf = new Array[Byte](64)
      var size = 0

      def ensureCapacity(cap: Int): Unit = {
        if (cap > buf.length) buf = Array.copyOf(buf, cap max (buf.length * 2))
      }

      override def forward(dstID: NodeID, chars: Array[Byte], start: Int, charsCnt: Int): Unit = {
        ensureCapacity(size + charsCnt)
        Array.copy(chars, start, buf, size, charsCnt)
        size += charsCnt
      }

      override def backward(srcID: NodeID, dstID: NodeID, charsCnt: Int): Unit = {
        size -= charsCnt
      }

      override def leaf(nodeID: NodeID, entryID: EntryID): Unit = {
        f(XString.slice(buf, 0, size), entryID)
      }
    })
  }

  def printStructure(out: TextOutput): Unit = {
    out.println("{")
    traverse(new Processor() {
      enum PrevState { case START, FWD, BACK, LEAF }
      import PrevState.*

      var last = START
      var nesting = 0

      def indent(): Unit = {
        (0 until nesting) foreach { _ => out.print("  ") }
      }

      override def forward(dstID: NodeID, chars: Array[Byte], start: Int, charsCnt: Int): Unit = {
        last match {
          case START | BACK => //no-op
          case FWD | LEAF => out.println(" {")
        }
        nesting += 1
        indent()
        val name = XString.slice(chars, start, charsCnt)
        out.print(s"$dstID:\"$name\"")
        last = FWD
      }

      override def backward(srcID: NodeID, dstID: NodeID, charsCnt: Int): Unit = {
        last match {
          case LEAF => out.println()
          case BACK => indent(); out.println("}")
          case _ => assert(false)
        }
        nesting -= 1
        last = BACK
      }

      override def leaf(nodeID: NodeID, entryID: EntryID): Unit = {
        out.print(s" $entryID")
        last = LEAF
      }
    })
    out.println("}")
  }
}