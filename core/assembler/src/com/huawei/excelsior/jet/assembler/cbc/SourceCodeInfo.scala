package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.SourceCodeInfo.Item
import com.huawei.excelsior.jet.assembler.{Label, Segment}

import scala.collection.mutable

case class SourceCodeInfo private[cbc](items: Seq[Item]) {
  def resolve(segment: Segment): Seq[(Int, Int)] = {
    items.map(i => (segment.getLabelPosition(i.bcPos), i.sourceLine))
  }
}

object SourceCodeInfo {
  
  class Builder {

    private val lines = mutable.ArrayBuffer.empty[Item]

    def add(bcPos: Label, sourceLine: Int): Unit = {
      lines += Item(bcPos, sourceLine)
    }
    
    def build: SourceCodeInfo = {
      new SourceCodeInfo(lines.toSeq)
    }
  }
  
  case class Item(bcPos: Label, sourceLine: Int)
}
