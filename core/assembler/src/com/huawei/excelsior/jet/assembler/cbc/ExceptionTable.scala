/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.{Label, Segment}
import com.huawei.excelsior.jet.assembler.cbc.CbcFileEncoder.Offset
import com.huawei.excelsior.jet.assembler.cbc.ExceptionTable.Region

import scala.collection.mutable

case class ExceptionTable(regionRefs: Seq[ExceptionTable.RegionRef]) {
  def resolve(segment: Segment): Seq[ExceptionTable.Region] = {
    val regions = regionRefs.map(_.resolve(segment)).sortBy(_.start)
    ExceptionTable.validateRegions(regions)
    regions
  }
}

object ExceptionTable {

  class Builder {

    private val regionsRefs = mutable.ArrayBuffer.empty[RegionRef]

    def addRegionRef(start: Label, end: Label, target: Label): Unit = {
      regionsRefs += RegionRef(start, end, target)
    }

    def build: ExceptionTable = {
      ExceptionTable(regionsRefs.toSeq)
    }
  }
  
  private def validateRegions(regions: Seq[Region]): Unit = {
    for (outerIdx <- regions.indices; innerIdx <- (outerIdx + 1) until regions.size) {
      val curr = regions(outerIdx)
      val that = regions(innerIdx)
      if (curr.target != that.target && curr.start < that.end && curr.end > that.start) {
        val currInThat = that.start <= curr.start && curr.end <= that.end
        val thatInCurr = curr.start <= that.start && that.end <= curr.end
        if (!currInThat && !thatInCurr) {
          throw new IllegalArgumentException(
            s"Unable to define exact target for overlapped regions ($curr, $that)"
          )
        }
      }
    }
  }

  case class RegionRef(start: Label, end: Label, target: Label) {
    assert(start != end)
    assert(start != target)
    assert(end != target)

    def resolve(s: Segment): Region = {
      Region(s.getLabelPosition(start), s.getLabelPosition(end), s.getLabelPosition(target))
    }
  }
  
  case class Region(start: Offset, end: Offset, target: Offset) {
    assert(start <= end)
    assert(start != target)
    //assert(end != target)
  }
}
