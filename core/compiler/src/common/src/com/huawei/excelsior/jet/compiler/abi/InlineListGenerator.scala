/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.ir.{BytecodeOffset, InlineContext, LineNumber, XSiteKind}
import com.huawei.excelsior.jet.compiler.symlevel.Method
import xscala.io.ByteBuffer
import xscala.io.LEB128Encoder.{calcSizeSLEB128, calcSizeULEB128}

import scala.collection.mutable.ArrayBuffer

trait InlineListGenerator { self: XTableGenerator =>

  /** @return buffer with generated inline list. */
  def genInlineList(unsortedXSitesInfo: ArrayBuffer[XSiteInfo]): ByteBuffer = {
    val xSitesInfo = unsortedXSitesInfo filter (_.inlineList.byteSize != 0) sortBy (-_.inlineList.byteSize)
    if (xSitesInfo.isEmpty) return null

    assert(xSitesInfo.forall(_.inlineList != null))

    var format = 0
    if (writeMethods) {
      format |= (1 << RTConst.InlineList.Format.HAS_METHOD_BIT.intValue)
    }
    if (writeBCPos) {
      format |= (1 << RTConst.InlineList.Format.HAS_BCPOS_BIT.intValue)
    }
    if (writeLines) {
      format |= (1 << RTConst.InlineList.Format.HAS_LINES_BIT.intValue)
    }

    val buf = new ByteBuffer()
    buf.putByte(format)
    val dataStart = buf.length
    assert(RTConst.InlineList.data.offset == dataStart)

    for (current <- xSitesInfo) {
      val foundOuter = xSitesInfo find { x => (x eq current) || (x.inlineList endsWith current.inlineList) }
      foundOuter match {
        case Some(`current`) => // no sequence containing `current` was found, so write out this one.
          current.inlineListHead = buf.length - dataStart
          current.inlineList.writeTo(buf)

        case Some(outer) => // `outer` sequence was already written, so just calculate start offset of `current`.
          val start = outer.inlineListHead
          assert(start != noInlinedMethods) // `outer` indeed was written

          val pos = outer.inlineList.byteSize - current.inlineList.byteSize
          assert(pos >= 0) // previously calculated as `outer` is larger than `current`.

          current.inlineListHead = start + pos

        case None => shouldNotReachHere()
      }
    }
    buf
  }

  object InlineList {
    // define Element in companion object so that it does not depend on the instance of outer InlineList,
    // otherwise its equals is useless
    private case class Element(typeIndex: Int, methodIndex: Int, bcPos: Int, lineNumber: Int, markers: Int)
    private object Element {
      val NotWritten = Int.MinValue
      assert(!BytecodeOffset.isValid(NotWritten))
      assert(!LineNumber.isValid(NotWritten))

      def apply(method: Method, bcPos: Int, lineNumber: Int, markers: Int): Element = {
        Element(
          env.getImportedClassIdx(method.getDeclaringClass, rootMethod),
          if (writeMethods) method.getHostedIndex ensuring (_ != NotWritten) else NotWritten,
          if (writeBCPos) bcPos ensuring (_ != NotWritten && writeMethods) else NotWritten,
          if (writeLines) lineNumber ensuring (_ != NotWritten && writeMethods) else NotWritten,
          markers // always written if there are at least one marker present in given bit set
        )
      }
    }
  }
  class InlineList(context: InlineContext, xSiteKind: XSiteKind) {
    import InlineList.*

    private val elements = context match {
      case null =>
        assert(xSiteKind == XSiteKind.GCPOINT || xSiteKind == XSiteKind.CALL || xSiteKind.isPreCall ||
          xSiteKind == XSiteKind.SOFT_EXCEPTION, xSiteKind)
        // * GCPoints are created with NoPosition.
        // * Soft exceptions never have InlineContext (see `XSitesGenerator.addXSite`).
        // * For some reason we might add XSite for call inside inlined unmanaged method.
        //   * This affects Call nodes, and also nodes inserted at the same position: PreCall nodes.
        // TODO: try to get rid of such xSites & replace this case with `shouldNotReachHere`
        null

      case _ if context.caller == null => // fast-path for empty context
        assert(context.method == rootMethod)
        Nil

      case _ =>
        val elements = ArrayBuffer.empty[Element]
        var ctx = context
        assert(ctx.bytecodePos == BytecodeOffset.INVALID && ctx.lineNumber == LineNumber.INVALID) // see NOTE(2) below
        while (ctx.caller != null) { // don't include class of current method, see NOTE(1) below
          val method = ctx.method
          val host = method.getDeclaringClass

          assert(host.hasRunTimeTypeInfo,
            s"internal error: $method from unmanaged $host (inlined into $rootMethod) contains exception tables")
          assert(!host.isDeferred)

          // Omit methods that will not be generated into final binary artifact
          if (method.getHostedIndex >= 0) {
            var markers = 0
            if (method.getInlineMarker != null) {
              markers |= RTConst.InlineList.Element.Markers.REFLECT_METHOD_INVOKE.intValue
            }
            elements += Element(method, ctx.caller.bytecodePos, ctx.caller.lineNumber, markers)
            // NOTE that inline list consists not of entries like `method:bcPos/lineNo-in-it`
            //      but rather `method:bcPos/lineNo-of-callsite-in-caller`.
            //      There are three reasons for that:
            //      1. We don't want to store `rootMethod` in each inline context of that method, as it is always the same.
            //         Moreover, it always can be determined directly from physical stack frame, as it is not inlined.
            //      2. First entry of `InlineContext` doesn't contain meaningful position, so it may be used for several
            //         call-sites in inlined method.
            //      3. Thus, from (1) we have only `bcPos`/`lineNo` for `rootMethod` and from (2) only `method` at the top
            //         of inline context. So to avoid waisting two entries of inline list we shift positions in callers to
            //         the entries describing callees.
          }

          ctx = ctx.caller
        }
        assert(ctx.method == rootMethod)
        elements
    }

    override def equals(obj: Any) = obj match {
      case that : InlineList => this.elements == that.elements
      case _ => false
    }
    override def hashCode = elements.##
    override def toString = if (elements == null) "null" else elements.toString

    def endsWith(that: InlineList) = this.elements endsWith that.elements

    val byteSize = elements match {
      case null | Nil => 0
      case seq =>
        var sum = 0
        def countULEB(v: Int): Unit = {
          require(v >= 0)
          sum += calcSizeULEB128(v)
        }
        def countSLEB(v: Int): Unit = {
          sum += calcSizeSLEB128(v)
        }
        seq foreach process(countULEB, countSLEB)
        sum
    }

    private def process(unsigned: Int => Unit, signed: Int => Unit): Element => Unit = {
      case Element(typeIndex, methodIndex, bcPos, lineNumber, markers) =>
        if (markers != 0) {
          unsigned(RTConst.InlineList.Iterator.INLINE_ENTRY_MARKERS.intValue)
          unsigned(markers)
        }
        unsigned(typeIndex + RTConst.InlineList.Iterator.INLINE_INDEX_ADDEND.intValue)
        if (writeMethods) {
          unsigned(methodIndex ensuring (_ != Element.NotWritten))
          if (writeBCPos) signed(bcPos ensuring (_ != Element.NotWritten))
          if (writeLines) signed(lineNumber ensuring (_ != Element.NotWritten))
        }
    }

    /** Write `this` inline sequence to given `Segment` in `uleb128` encoding. */
    def writeTo(buf: ByteBuffer): Unit = {
      val start = buf.length

      elements foreach process(buf.putULEB, buf.putSLEB)
      assert(byteSize == buf.length - start)
      buf.putULEB(RTConst.InlineList.Iterator.INLINE_END.intValue)
    }
  }
}
